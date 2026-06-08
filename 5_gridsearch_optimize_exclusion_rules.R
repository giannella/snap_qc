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

# Bound each variable's grid to this quantile range of the data, snapped to step.
GRID_LO_Q <- 0.02
GRID_HI_Q <- 0.98

out_dir <- "single_rule_gridsearch"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
stopifnot(OBJECTIVE %in% c("dollars", "counts"))

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

## ── 2. Prepare the pile ───────────────────────────────────────────────────────

is_error <- eval(TARGET_IS_ERROR, envir = flagged_cases)
is_error[is.na(is_error)] <- FALSE
ed <- flagged_cases[[ERR_AMT_COL]]; ed[is.na(ed)] <- 0
err_dollars <- ifelse(is_error, abs(ed), 0)

recall_col <- if (OBJECTIVE == "dollars") "dollar_recall_retained" else "recall_retained"

cat(sprintf("\n=== Rule grid search (objective: %s, floor: %.2f) ===\n",
            toupper(OBJECTIVE), RECALL_FLOOR))
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
                c(lapply(rule_terms, `[[`, "grid"),
                  list(KEEP.OUT.ATTRS = FALSE)))
names(grid) <- vapply(rule_terms, `[[`, character(1), "var")
cat(sprintf("  total combinations: %d\n", nrow(grid)))

## ── 4. Evaluate every combination ─────────────────────────────────────────────

xs  <- lapply(rule_terms, function(tm) flagged_cases[[tm$var]])
ops <- vapply(rule_terms, `[[`, character(1), "op")

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
  mutate(across(c(workload_cut_pct, exclusion_purity, dollar_recall_retained,
                  recall_retained, retained_precision), ~ round(.x, 4)),
         err_dollars_lost = round(err_dollars_lost, 0))

write.csv(results, file.path(out_dir, "rule_gridsearch_full.csv"), row.names = FALSE)

## ── 5. Baseline (original thresholds) and best under the recall floor ─────────

baseline <- eval_combo(vapply(rule_terms, `[[`, numeric(1), "original"))
cat("\n-- ORIGINAL thresholds --\n")
cat(sprintf("  %s\n", paste(mapply(function(tm) sprintf("%s %s %s", tm$var, tm$op, tm$original),
                                   rule_terms), collapse = " & ")))
cat(sprintf("  workload cut %.1f%% | %s %.3f | errors lost %d | $ lost $%s\n",
            baseline$workload_cut_pct, recall_col, baseline[[recall_col]],
            baseline$errors_lost, format(round(baseline$err_dollars_lost), big.mark = ",")))

feasible <- results %>% filter(.data[[recall_col]] >= RECALL_FLOOR)

if (nrow(feasible) == 0) {
  cat(sprintf("\nNo combination holds %s >= %.2f. Lower RECALL_FLOOR or widen the grids.\n",
              recall_col, RECALL_FLOOR))
} else {
  best <- feasible %>% slice_max(workload_cut_pct, n = 1, with_ties = FALSE)
  tvals <- vapply(rule_terms, function(tm) best[[tm$var]], numeric(1))
  cat(sprintf("\n-- BEST thresholds (max workload cut with %s >= %.2f) --\n",
              recall_col, RECALL_FLOOR))
  cat(sprintf("  %s\n",
              paste(mapply(function(tm, tv) sprintf("%s %s %s", tm$var, tm$op, tv),
                           rule_terms, tvals), collapse = " & ")))
  cat(sprintf("  workload cut %.1f%% | %s %.3f | errors lost %d | $ lost $%s\n",
              best$workload_cut_pct, recall_col, best[[recall_col]],
              best$errors_lost, format(round(best$err_dollars_lost), big.mark = ",")))
  
  cat("\n-- top 15 feasible combinations by workload cut --\n")
  print(as.data.frame(
    feasible %>% arrange(desc(workload_cut_pct)) %>% head(15) %>%
      select(all_of(names(grid)), workload_cut_pct, !!recall_col,
             errors_lost, err_dollars_lost)))
  write.csv(feasible %>% arrange(desc(workload_cut_pct)),
            file.path(out_dir, "rule_gridsearch_feasible.csv"), row.names = FALSE)
}

## ── 6. Notes ──────────────────────────────────────────────────────────────────
# - Operators are held fixed; only thresholds move. To flip a direction, edit `op`.
# - To search nearer the original (a local tune rather than global), replace the
#   grid for a term with, e.g., seq(150, 350, by = 50) instead of snapped_grid().
# - For a ratio variable, step is a decimal (0.05); it is not a 50/100 grid.
# - These thresholds are tuned in-sample. Re-run eval_combo() on a holdout period
#   to confirm the chosen thresholds hold before adopting them.

