# ──────────────────────────────────────────────────────────────────────────────
# Optimize ONE exclusion rule's thresholds by grid search
#
# Takes a single multi-condition rule, e.g.
#   earned_by_hh_size >= 227.3 & shelter_to_gross_ratio < 0.7111 & gross_by_hh_size < 991.5
# and searches round thresholds for each condition (keeping the operators fixed),
# scoring every combination as an EXCLUSION criterion. Returns the best thresholds
# that maximize workload cut while holding dollar recall at/above a floor, plus the
# full grid so you can see the tradeoff and pick your own point.
#
# The search runs separately within each household-size stratum (1, 2, 3, 4, 5+);
# every output row is tagged with its hh_size.
#
# Search space: dollar-style variables step by multiples of 50 (a superset of
# multiples of 100); a ratio/proportion gets a decimal grid. Ranges are bounded to
# where the data actually lives (2nd-98th percentile, snapped to the step) so the
# search stays reasonable.
# ──────────────────────────────────────────────────────────────────────────────

library(dplyr)

## ── 0. Config ─────────────────────────────────────────────────────────────────

# `flagged_cases` expected in the environment. 

TARGET_IS_ERROR <- quote(!is.na(over_threshold) & over_threshold != 0)
ERR_AMT_COL     <- "total_error_amount"
OBJECTIVE       <- "dollars"     # "dollars" or "counts"
RECALL_FLOOR    <- 0.95          # keep at least this share of (dollars | error cases)

# THE RULE TO OPTIMIZE -----------------------------------------------------------
# One entry per condition: the variable, the operator (kept fixed), the rounding
# STEP for its candidate grid, and the ORIGINAL threshold (for a baseline compare).
# For dollar-style variables use step 50 (or 100); for a ratio use e.g. 0.05.
rule_terms <- list(
  list(var = "n_deduction_types",      op = ">=", step = 1,   original = 4),
  list(var = "rawben_rel_max", op = "<",  step = .05, original = 0.8377),
  list(var = "earned_by_hh_size",       op = "<",  step = 50,   original = 650.5)
)

# Household-size stratification: cert_HH_size_FS_n collapsed to 1, 2, 3, 4, 5+.
HH_SIZE_COL <- "cert_HH_size_FS_n"
HH_LEVELS   <- c("1", "2", "3", "4", "5+")
hh_group_of <- function(n) { g <- pmin(n, 5); ifelse(g == 5, "5+", as.character(g)) }

# Bound each variable's grid to this quantile range of the data, snapped to step.
GRID_LO_Q <- 0.02
GRID_HI_Q <- 0.98

out_dir <- "single_rule_gridsearch"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
stopifnot(OBJECTIVE %in% c("dollars", "counts"))

recall_col <- if (OBJECTIVE == "dollars") "dollar_recall_retained" else "recall_retained"

## ── 1. Helpers ────────────────────────────────────────────────────────────────

apply_op <- function(x, op, t) switch(op,
  ">=" = x >= t, "<=" = x <= t, ">" = x > t, "<" = x < t, "==" = x == t,
  stop("unsupported operator: ", op))

# Candidate thresholds: multiples of `step` spanning the data's central range.
# Build from finite values only (ratios can be Inf/NaN when a denominator is 0).
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

# Exclusion performance of a flag (TRUE = case dropped). Both count and dollar views.
exclusion_perf <- function(excl, is_error, err_dollars) {
  N <- length(excl); n_excl <- sum(excl); n_ret <- N - n_excl
  total_err <- sum(is_error)
  ed <- err_dollars; ed[is.na(ed)] <- 0
  dollars_total <- sum(ed); dollars_lost <- sum(ed[excl])
  errors_lost   <- sum(excl & is_error)
  clean_excl    <- sum(excl & !is_error)
  errors_ret    <- total_err - errors_lost
  tibble(
    n_excluded             = n_excl,
    workload_cut_pct       = 100 * n_excl / N,
    clean_excluded         = clean_excl,
    errors_lost            = errors_lost,
    exclusion_purity       = if (n_excl > 0) clean_excl / n_excl else NA_real_,
    err_dollars_lost       = dollars_lost,
    dollar_recall_retained = if (dollars_total > 0) (dollars_total - dollars_lost) / dollars_total else NA_real_,
    recall_retained        = if (total_err > 0) errors_ret / total_err else NA_real_,
    retained_precision     = if (n_ret > 0) errors_ret / n_ret else NA_real_
  )
}

## ── 2. Per-stratum grid search ────────────────────────────────────────────────
# Builds the grid on the stratum's own data, scores every combination, and returns
# the full results, the original-threshold baseline, and the best feasible point,
# all tagged with hh_size.

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
    exclusion_perf(flag, is_error, err_dollars)
  }

  results <- bind_cols(
    grid,
    bind_rows(lapply(seq_len(nrow(grid)), function(r) eval_combo(as.numeric(grid[r, ]))))
  ) %>%
    mutate(hh_size = hh_label, .before = 1) %>%
    mutate(across(c(workload_cut_pct, exclusion_purity, dollar_recall_retained,
                    recall_retained, retained_precision), ~ round(.x, 4)),
           err_dollars_lost = round(err_dollars_lost, 0))

  baseline <- eval_combo(vapply(terms, `[[`, numeric(1), "original")) %>%
    mutate(hh_size = hh_label, .before = 1)

  feasible <- results %>% filter(.data[[recall_col]] >= RECALL_FLOOR)
  best <- if (nrow(feasible) == 0) NULL else
    feasible %>% slice_max(workload_cut_pct, n = 1, with_ties = FALSE)
  if (is.null(best))
    cat(sprintf("  no combination holds %s >= %.2f in this stratum\n", recall_col, RECALL_FLOOR))

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

write.csv(results_all, file.path(out_dir, "rule_gridsearch_full.csv"), row.names = FALSE)
write.csv(feasible_all %>% arrange(hh_size, desc(workload_cut_pct)),
          file.path(out_dir, "rule_gridsearch_feasible.csv"), row.names = FALSE)
write.csv(best_all, file.path(out_dir, "rule_gridsearch_best_by_hh.csv"), row.names = FALSE)

var_names <- vapply(rule_terms, `[[`, character(1), "var")

cat("\n\n========== ORIGINAL thresholds per household size ==========\n")
print(as.data.frame(baseline_all %>%
  select(hh_size, workload_cut_pct, !!recall_col, errors_lost, err_dollars_lost)))

cat(sprintf("\n========== BEST thresholds per household size (max workload cut with %s >= %.2f) ==========\n",
            recall_col, RECALL_FLOOR))
print(as.data.frame(best_all %>%
  select(hh_size, all_of(var_names), workload_cut_pct, !!recall_col,
         errors_lost, err_dollars_lost)))

for (i in seq_len(nrow(best_all))) {
  b <- best_all[i, ]
  rule_str <- paste(mapply(function(tm) sprintf("%s %s %s", tm$var, tm$op, b[[tm$var]]),
                           rule_terms), collapse = " & ")
  cat(sprintf("\n  [HH %s]  %s\n    workload cut %.1f%% | %s %.3f | errors lost %d\n",
              b$hh_size, rule_str, b$workload_cut_pct, recall_col, b[[recall_col]], b$errors_lost))
}

## ── 4. Notes ──────────────────────────────────────────────────────────────────
# - Operators are held fixed; only thresholds move. To flip a direction, edit `op`.
# - To search nearer the original (a local tune rather than global), replace the
#   grid for a term with, e.g., seq(150, 350, by = 50) instead of snapped_grid().
# - For a ratio variable, step is a decimal (0.05); it is not a 50/100 grid.
# - These thresholds are tuned in-sample. Re-run on a holdout period to confirm.
