# Read the typed-delivery scorecard and print it, one block per review budget.
#
# Safe to run mid-build: holdout_metrics.json is rewritten after every state, so
# it always reflects the states finished so far.
#
#   Rscript methods/summarize_typed_holdout_v2.R
#
# Written by methods/build_typed_blended_delivery_v2.R. See
# methods/compare_typed_vs_anyerror_v2.R for the head-to-head against the
# any-error vocabulary.
#
# `fill_ratio` is the column to watch: the share of the HOLDOUT caseload the
# list actually flags, divided by the budget it was filled to on the training
# years. 1.0 means the list carries its intended workload into the next year;
# well below 1.0 means the rules are narrower than the fill assumed and fire on
# fewer cases than budgeted, which costs recall even when precision holds.
suppressMessages(library(jsonlite))

f <- "methods/typed_blended_holdout_2024/holdout_metrics.json"
if (!file.exists(f)) stop("no scorecard yet: ", f)
j <- fromJSON(f)
r <- j$records

cat(sprintf("holdout %s | trained %s | delivered rules %s\n",
            j$holdout_fiscal_year, j$train_years, j$delivered_rule_years))
cat(sprintf("vocabulary: %s\nadmission : %s\nranking   : %s\nPER basis : %s\n\n",
            j$vocabulary, j$admission, j$ranking, j$per_basis))

for (b in sort(unique(r$budget_pct))) {
  s <- r[r$budget_pct == b, ]
  s <- s[order(-s$per_reduction_pts_weighted), ]
  cat(sprintf("=== review budget %.0f%% of caseload (%d states) ===\n", b, nrow(s)))
  out <- data.frame(
    state       = s$state,
    rules       = s$n_rules_core,
    flagged     = s$n_cases_flagged_holdout,
    pct_load    = round(100 * s$flagged_share_of_caseload, 1),
    fill_ratio  = round(100 * s$flagged_share_of_caseload / b, 2),
    err_found   = s$n_errors_flagged_holdout,
    err_total   = s$n_errors_holdout,
    precision   = round(s$precision_holdout, 3),
    base_rate   = round(s$base_rate_holdout, 3),
    lift        = round(s$precision_holdout / s$base_rate_holdout, 2),
    PER_unwtd   = round(s$per_reduction_pts_unweighted, 2),
    PER_wtd     = round(s$per_reduction_pts_weighted, 2),
    err_dollars = format(round(s$per_total_weighted_error_dollars), big.mark = ","),
    stringsAsFactors = FALSE)
  print(out, row.names = FALSE)
  cat(sprintf(
    "\nmedian: precision %.3f | lift %.2fx | fill ratio %.2f | PER reduction %.2f%% unwtd, %.2f%% wtd\n\n",
    median(s$precision_holdout, na.rm = TRUE),
    median(s$precision_holdout / s$base_rate_holdout, na.rm = TRUE),
    median(100 * s$flagged_share_of_caseload / b, na.rm = TRUE),
    median(s$per_reduction_pts_unweighted, na.rm = TRUE),
    median(s$per_reduction_pts_weighted, na.rm = TRUE)))
}
