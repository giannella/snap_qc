# ──────────────────────────────────────────────────────────────────────────────
# Optimize ONE inclusion rule's thresholds by grid search
#
# Parallel to 5_gridsearch_optimize_exclusion_rules.R, but for FLAGGING (inclusion).
# Takes one multi-condition rule from script 6, e.g.
#   earned_by_hh_size >= 250 & shelter_to_gross_ratio < 0.70 & gross_by_hh_size < 1000
# and searches round thresholds (operators fixed), scoring each combination as a
# FLAGGING criterion. Returns the thresholds that MAXIMIZE PRECISION (share of
# flagged cases that are true errors) while holding RECALL at/above a floor.
#
# The search runs separately within each household-size stratum (1, 2, 3, 4, 5+);
# every output row is tagged with its hh_size.
#
# Search space: dollar-style variables step by multiples of 50; a ratio gets a
# decimal grid. Ranges bound to the 2nd-98th percentile, snapped to the step.
# ──────────────────────────────────────────────────────────────────────────────

library(dplyr)

## ── 0. Config ─────────────────────────────────────────────────────────────────

# `flagged_cases` expected in the environment.

TARGET_IS_ERROR <- quote(!is.na(over_threshold) & over_threshold != 0)
ERR_AMT_COL     <- "total_error_amount"
OBJECTIVE       <- "dollars"     # recall basis: "dollars" or "counts"
RECALL_FLOOR    <- 0.02          # capture at least this share of (dollars | error cases)

# THE INCLUSION RULE TO OPTIMIZE -------------------------------------------------
# Paste an INCLUDE rule from script 6_rulefit_find_inclusion_rules.R. 
# One entry per condition: variable, operator # (kept fixed), 
# rounding STEP for the grid, and the ORIGINAL threshold (baseline).
# Dollar-style variables: step 50 (or 100). Ratio: step 0.05.

#gross_by_hh_size < 498.4 & unc_rawben_rel_max < 0.9609 & shelter_expenses >= 978.5

rule_terms <- list(
  list(var = "gross_by_hh_size",      op = "<", step = 50,   original = 498.4),
  list(var = "unc_rawben_rel_max", op = "<",  step = 0.05, original = 0.96),
  list(var = "shelter_expenses",       op = "<",  step = 50,   original = 978.5)
)

# Household-size stratification: cert_HH_size_FS_n collapsed to 1, 2, 3, 4, 5+.
HH_SIZE_COL <- "cert_HH_size_FS_n"
HH_LEVELS   <- c("1", "2", "3", "4", "5+")
hh_group_of <- function(n) { g <- pmin(n, 5); ifelse(g == 5, "5+", as.character(g)) }

GRID_LO_Q <- 0.02
GRID_HI_Q <- 0.98

out_dir <- "single_rule_gridsearch_inclusion"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
stopifnot(OBJECTIVE %in% c("dollars", "counts"))

recall_col <- if (OBJECTIVE == "dollars") "dollar_recall" else "recall"

## ── 1. Helpers ────────────────────────────────────────────────────────────────

apply_op <- function(x, op, t) switch(op,
  ">=" = x >= t, "<=" = x <= t, ">" = x > t, "<" = x < t, "==" = x == t,
  stop("unsupported operator: ", op))

# Candidate thresholds: multiples of `step` over the data's central range, built
# from finite values only (ratios can be Inf/NaN when a denominator is 0).
snapped_grid <- function(x, step, lo_q = GRID_LO_Q, hi_q = GRID_HI_Q) {
  x <- x[is.finite(x)]
  if (length(x) == 0) stop("no finite values to build a grid from")
  lo <- floor(quantile(x, lo_q) / step) * step
  hi <- ceiling(quantile(x, hi_q) / step) * step
  if (!is.finite(lo) || !is.finite(hi))
    stop("non-finite grid bounds; check the variable's distribution")
  if (hi <= lo) hi <- lo + step
  seq(lo, hi, by = step)
}

# Performance of an INCLUSION flag (TRUE = case flagged for review).
inclusion_perf <- function(flag, is_error, err_dollars) {
  N <- length(flag); n_flag <- sum(flag); total_err <- sum(is_error)
  tp <- sum(flag & is_error); base_rate <- total_err / N
  ed <- err_dollars; ed[is.na(ed)] <- 0
  dollars_total <- sum(ed); dollars_caught <- sum(ed[flag])
  tibble(
    n_flagged = n_flag, workload_pct = 100 * n_flag / N,
    errors_caught = tp, clean_flagged = n_flag - tp,
    precision = if (n_flag > 0) tp / n_flag else NA_real_,
    recall = if (total_err > 0) tp / total_err else NA_real_,
    dollar_recall = if (dollars_total > 0) dollars_caught / dollars_total else NA_real_,
    lift = if (n_flag > 0 && base_rate > 0) (tp / n_flag) / base_rate else NA_real_,
    err_dollars_caught = dollars_caught
  )
}

## ── 2. Per-stratum grid search ────────────────────────────────────────────────
# Builds the grid on the stratum's own data, scores every combination, and returns
# the full results, the original-threshold baseline, and the best feasible point
# (max precision with recall >= floor), all tagged with hh_size.

run_for_hh <- function(flagged_cases, hh_label) {

  is_error <- eval(TARGET_IS_ERROR, envir = flagged_cases)
  is_error[is.na(is_error)] <- FALSE
  ed <- flagged_cases[[ERR_AMT_COL]]; ed[is.na(ed)] <- 0
  err_dollars <- ifelse(is_error, abs(ed), 0)

  cat(sprintf("\n\n#################### HOUSEHOLD SIZE %s ####################\n", hh_label))
  cat(sprintf("  N = %d | errors = %d | error $ = $%s\n",
              nrow(flagged_cases), sum(is_error),
              format(round(sum(err_dollars)), big.mark = ",")))

  terms <- rule_terms
  for (i in seq_along(terms)) {
    v <- terms[[i]]$var
    if (!v %in% names(flagged_cases)) stop("variable not found: ", v)
    terms[[i]]$grid <- snapped_grid(flagged_cases[[v]], terms[[i]]$step)
    cat(sprintf("  %-24s %s  grid: %d values [%s ... %s] step %s\n",
                v, terms[[i]]$op, length(terms[[i]]$grid),
                format(min(terms[[i]]$grid)), format(max(terms[[i]]$grid)),
                format(terms[[i]]$step)))
  }

  grid <- do.call(expand.grid,
                  c(lapply(terms, `[[`, "grid"), list(KEEP.OUT.ATTRS = FALSE)))
  names(grid) <- vapply(terms, `[[`, character(1), "var")

  xs  <- lapply(terms, function(tm) flagged_cases[[tm$var]])
  ops <- vapply(terms, `[[`, character(1), "op")
  eval_combo <- function(thresholds) {
    flag <- Reduce(`&`, Map(function(x, op, t) {
      f <- apply_op(x, op, t); f[is.na(f)] <- FALSE; f
    }, xs, ops, thresholds))
    inclusion_perf(flag, is_error, err_dollars)
  }

  results <- bind_cols(
    grid,
    bind_rows(lapply(seq_len(nrow(grid)), function(r) eval_combo(as.numeric(grid[r, ]))))
  ) %>%
    mutate(hh_size = hh_label, .before = 1) %>%
    mutate(across(c(workload_pct, precision, recall, dollar_recall, lift), ~ round(.x, 4)),
           err_dollars_caught = round(err_dollars_caught, 0))

  baseline <- eval_combo(vapply(terms, `[[`, numeric(1), "original")) %>%
    mutate(hh_size = hh_label, .before = 1)

  feasible <- results %>% filter(.data[[recall_col]] >= RECALL_FLOOR)
  best <- if (nrow(feasible) == 0) NULL else
    feasible %>% slice_max(precision, n = 1, with_ties = FALSE)
  if (is.null(best))
    cat(sprintf("  no combination reaches %s >= %.2f in this stratum\n", recall_col, RECALL_FLOOR))

  list(results = results, baseline = baseline, best = best, feasible = feasible)
}

## ── 3. Run every household-size stratum and combine ───────────────────────────

groups       <- hh_group_of(flagged_cases[[HH_SIZE_COL]])
results_list <- lapply(HH_LEVELS, function(lab)
  run_for_hh(flagged_cases[!is.na(groups) & groups == lab, , drop = FALSE], lab))

results_all  <- bind_rows(lapply(results_list, `[[`, "results"))
baseline_all <- bind_rows(lapply(results_list, `[[`, "baseline"))
best_all     <- bind_rows(lapply(results_list, `[[`, "best"))
feasible_all <- bind_rows(lapply(results_list, `[[`, "feasible"))

write.csv(results_all, file.path(out_dir, "inclusion_gridsearch_full.csv"), row.names = FALSE)
write.csv(feasible_all %>% arrange(hh_size, desc(precision)),
          file.path(out_dir, "inclusion_gridsearch_feasible.csv"), row.names = FALSE)
write.csv(best_all, file.path(out_dir, "inclusion_gridsearch_best_by_hh.csv"), row.names = FALSE)

var_names <- vapply(rule_terms, `[[`, character(1), "var")

cat("\n\n========== ORIGINAL thresholds per household size ==========\n")
print(as.data.frame(baseline_all %>%
  select(hh_size, precision, !!recall_col, n_flagged, errors_caught)))

cat(sprintf("\n========== BEST thresholds per household size (max precision with %s >= %.2f) ==========\n",
            recall_col, RECALL_FLOOR))
print(as.data.frame(best_all %>%
  select(hh_size, all_of(var_names), precision, !!recall_col, n_flagged, errors_caught)))

for (i in seq_len(nrow(best_all))) {
  b <- best_all[i, ]
  rule_str <- paste(mapply(function(tm) sprintf("%s %s %s", tm$var, tm$op, b[[tm$var]]),
                           rule_terms), collapse = " & ")
  cat(sprintf("\n  [HH %s]  %s\n    precision %.3f | %s %.3f | flagged %d | errors caught %d\n",
              b$hh_size, rule_str, b$precision, recall_col, b[[recall_col]],
              b$n_flagged, b$errors_caught))
}

# flagged_cases$rule_1 <- flagged_cases$gross_by_hh_size < 350 & flagged_cases$unc_rawben_rel_max < 0.6 & flagged_cases$shelter_expenses < 1200
# table(flagged_cases$rule_1)
# sum(flagged_cases$total_error_amount[flagged_cases$rule_1], na.rm = TRUE)

## ── 4. Notes ──────────────────────────────────────────────────────────────────
# - Operators fixed; only thresholds move. Maximize precision subject to recall.
# - There is a precision/recall tradeoff: a higher RECALL_FLOOR forces more cases
#   flagged and usually lowers the achievable precision. Re-run at a few floors to
#   see the curve, or sort the full CSV by precision within recall bands.
# - For a local tune near the original, swap snapped_grid() for an explicit seq().
# - Thresholds are tuned in-sample; re-run on a holdout to confirm.
