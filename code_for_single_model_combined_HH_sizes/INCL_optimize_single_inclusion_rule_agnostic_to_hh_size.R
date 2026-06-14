# ──────────────────────────────────────────────────────────────────────────────
# Optimize ONE inclusion rule's thresholds by grid search
#
# Parallel to EXCL_exclusion_rules_single_model_across_hh_sizes_find_state_specific_params_for_single_rule.R, but for FLAGGING (inclusion).
# Takes one multi-condition rule from script 6, e.g.
#   earned_by_hh_size >= 250 & shelter_to_gross_ratio < 0.70 & gross_by_hh_size < 1000
# and searches round thresholds (operators fixed), scoring each combination as a
# FLAGGING criterion. Returns the thresholds that MAXIMIZE PRECISION (share of
# flagged cases that are true errors) while holding RECALL at/above a floor.
#
# Search space: dollar-style variables step by multiples of 50; a ratio gets a
# decimal grid. Ranges bound to the 2nd-98th percentile, snapped to the step.
# ──────────────────────────────────────────────────────────────────────────────

library(dplyr)

## ── 0. Config ─────────────────────────────────────────────────────────────────

# `flagged_cases` expected in the environment.
#or use:
#flagged_cases <- reg_model_data %>% filter(fiscal_year>2019 & state=="")

TARGET_IS_ERROR <- quote(!is.na(over_threshold) & over_threshold != 0)
ERR_AMT_COL     <- "total_error_amount"
OBJECTIVE       <- "dollars"     # recall basis: "dollars" or "counts"
RECALL_FLOOR    <- 0.02          # capture at least this share of (dollars | error cases)

# THE INCLUSION RULE TO OPTIMIZE -------------------------------------------------
# Paste an INCLUDE rule from INCL_find_inclusion_rules_single_model_combines_all_hh_sizes.R.
# One entry per condition: variable, operator # (kept fixed), 
# rounding STEP for the grid, and the ORIGINAL threshold (baseline).
# Dollar-style variables: step 50 (or 100). Ratio: step 0.05.

#gross_by_hh_size < 498.4 & unc_rawben_rel_max < 0.9609 & shelter_expenses >= 978.5

rule_terms <- list(
  list(var = "gross_by_hh_size",      op = "<", step = 50,   original = 498.4),
  list(var = "unc_rawben_rel_max", op = "<",  step = 0.05, original = 0.96),
  list(var = "shelter_expenses",       op = "<",  step = 50,   original = 978.5)
)

GRID_LO_Q <- 0.02
GRID_HI_Q <- 0.98

out_dir <- "../inclusion_rules_combined_hh_sizes"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
stopifnot(OBJECTIVE %in% c("dollars", "counts"))

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

## ── 2. Prepare the pile ───────────────────────────────────────────────────────

is_error <- eval(TARGET_IS_ERROR, envir = flagged_cases)
is_error[is.na(is_error)] <- FALSE
ed <- flagged_cases[[ERR_AMT_COL]]; ed[is.na(ed)] <- 0
err_dollars <- ifelse(is_error, abs(ed), 0)

recall_col <- if (OBJECTIVE == "dollars") "dollar_recall" else "recall"

cat(sprintf("\n=== Inclusion rule grid search (recall basis: %s, floor: %.2f) ===\n",
            recall_col, RECALL_FLOOR))
cat(sprintf("  N = %d | errors = %d | error $ = $%s\n",
            nrow(flagged_cases), sum(is_error),
            format(round(sum(err_dollars)), big.mark = ",")))

## ── 3. Build the search grid ──────────────────────────────────────────────────

for (i in seq_along(rule_terms)) {
  v <- rule_terms[[i]]$var
  if (!v %in% names(flagged_cases)) stop("variable not found: ", v)
  rule_terms[[i]]$grid <- snapped_grid(flagged_cases[[v]], rule_terms[[i]]$step)
  cat(sprintf("  %-24s %s  grid: %d values [%s ... %s] step %s\n",
              v, rule_terms[[i]]$op, length(rule_terms[[i]]$grid),
              format(min(rule_terms[[i]]$grid)), format(max(rule_terms[[i]]$grid)),
              format(rule_terms[[i]]$step)))
}

grid <- do.call(expand.grid,
                c(lapply(rule_terms, `[[`, "grid"), list(KEEP.OUT.ATTRS = FALSE)))
names(grid) <- vapply(rule_terms, `[[`, character(1), "var")
cat(sprintf("  total combinations: %d\n", nrow(grid)))

## ── 4. Evaluate every combination ─────────────────────────────────────────────

xs  <- lapply(rule_terms, function(tm) flagged_cases[[tm$var]])
ops <- vapply(rule_terms, `[[`, character(1), "op")

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
  mutate(across(c(workload_pct, precision, recall, dollar_recall, lift), ~ round(.x, 4)),
         err_dollars_caught = round(err_dollars_caught, 0))

write.csv(results, file.path(out_dir, "inclusion_gridsearch_full.csv"), row.names = FALSE)

## ── 5. Baseline (original thresholds) and best under the recall floor ─────────

baseline <- eval_combo(vapply(rule_terms, `[[`, numeric(1), "original"))
cat("\n-- ORIGINAL thresholds --\n")
cat(sprintf("  %s\n", paste(mapply(function(tm) sprintf("%s %s %s", tm$var, tm$op, tm$original),
                                   rule_terms), collapse = " & ")))
cat(sprintf("  precision %.3f | %s %.3f | flagged %d | errors caught %d\n",
            baseline$precision, recall_col, baseline[[recall_col]],
            baseline$n_flagged, baseline$errors_caught))

feasible <- results %>% filter(.data[[recall_col]] >= RECALL_FLOOR)

if (nrow(feasible) == 0) {
  cat(sprintf("\nNo combination reaches %s >= %.2f. Lower RECALL_FLOOR or widen grids.\n",
              recall_col, RECALL_FLOOR))
} else {
  best <- feasible %>% slice_max(precision, n = 1, with_ties = FALSE)
  tvals <- vapply(rule_terms, function(tm) best[[tm$var]], numeric(1))
  cat(sprintf("\n-- BEST thresholds (max precision with %s >= %.2f) --\n",
              recall_col, RECALL_FLOOR))
  cat(sprintf("  %s\n",
              paste(mapply(function(tm, tv) sprintf("%s %s %s", tm$var, tm$op, tv),
                           rule_terms, tvals), collapse = " & ")))
  cat(sprintf("  precision %.3f | %s %.3f | flagged %d | errors caught %d\n",
              best$precision, recall_col, best[[recall_col]],
              best$n_flagged, best$errors_caught))

  cat("\n-- top 15 feasible combinations by precision --\n")
  print(as.data.frame(
    feasible %>% arrange(desc(precision)) %>% head(15) %>%
      select(all_of(names(grid)), precision, !!recall_col, n_flagged, errors_caught)))
  write.csv(feasible %>% arrange(desc(precision)),
            file.path(out_dir, "inclusion_gridsearch_feasible.csv"), row.names = FALSE)
}

# flagged_cases$rule_1 <- flagged_cases$gross_by_hh_size < 350 & flagged_cases$unc_rawben_rel_max < 0.6 & flagged_cases$shelter_expenses < 1200
# table(flagged_cases$rule_1)
# sum(flagged_cases$total_error_amount[flagged_cases$rule_1], na.rm = TRUE)

## ── 6. Notes ──────────────────────────────────────────────────────────────────
# - Operators fixed; only thresholds move. Maximize precision subject to recall.
# - There is a precision/recall tradeoff: a higher RECALL_FLOOR forces more cases
#   flagged and usually lowers the achievable precision. Re-run at a few floors to
#   see the curve, or sort the full CSV by precision within recall bands.
# - For a local tune near the original, swap snapped_grid() for an explicit seq().
# - Thresholds are tuned in-sample; re-run eval_combo() on a holdout to confirm.
