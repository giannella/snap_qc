# Head-to-head: the typed five-frame vocabulary against the any-error
# vocabulary, both trained 2022-23 and tested on 2024.
#
#   Rscript methods/compare_typed_vs_anyerror_v2.R
#
# THE BASELINE. Use the `fdr10f` arm of
#   methods/state_similarity_v2/transfer_benchmark_train2223_test24/fdr_admission_audition.csv
# It matches this run on every axis except the mining vocabulary: same BH FDR
# 10% admission, same n >= 30 floor, same 99% Wilson LCB ranking, same
# core/buffer budget fill, same era. Its targets_of() is byte-identical to the
# builder's, so its `dollar_recall` IS our unweighted PER reduction / 100 and
# its `precision` and `workload` are ours directly.
#
# DO NOT compare against blended_frozen_results.csv or
# blended_frozen_results_5frames.csv in that same folder. Both were built
# before v2.3.0 on pools that used the legacy raw-precision admission, so a
# comparison against them confounds the vocabulary change with the admission
# change. Those two ARE a valid comparison with EACH OTHER -- that pair is what
# modeling_findings.md section 17 rests on -- but this run cannot be dropped
# into it.
#
# Only the states present in BOTH the scorecard and the benchmark's 18 are
# compared; the rest are reported as not comparable.
suppressMessages(library(jsonlite))

BASE <- "methods/state_similarity_v2/transfer_benchmark_train2223_test24/fdr_admission_audition.csv"
MINE <- "methods/typed_blended_holdout_2024/holdout_metrics.json"
for (f in c(BASE, MINE)) if (!file.exists(f)) stop("missing: ", f)

base <- subset(read.csv(BASE), arm == "fdr10f")
base$budget_pct <- 100 * base$budget

j <- fromJSON(MINE)
mine <- j$records
mine$dollar_recall_typed <- mine$per_reduction_pts_unweighted / 100

m <- merge(mine, base, by.x = c("state", "budget_pct"),
           by.y = c("target", "budget_pct"))
if (!nrow(m)) stop("no overlap yet with the 18 benchmark states")

skipped <- setdiff(unique(mine$state), unique(m$state))
if (length(skipped))
  cat(sprintf("not in the benchmark 18, no comparison: %s\n\n",
              paste(sort(skipped), collapse = ", ")))

m$d_prec <- m$precision_holdout - m$precision
m$d_doll <- m$dollar_recall_typed - m$dollar_recall
# does the list carry its budgeted workload into the holdout year?
m$fill_typed  <- m$flagged_share_of_caseload / (m$budget_pct / 100)
m$fill_anyerr <- m$workload / (m$budget_pct / 100)

for (b in sort(unique(m$budget_pct))) {
  s <- m[m$budget_pct == b, ]
  cat(sprintf("\n=== %.0f%% review budget -- %d comparable states ===\n", b, nrow(s)))
  out <- data.frame(
    state          = s$state,
    prec_typed     = round(s$precision_holdout, 4),
    prec_anyerr    = round(s$precision, 4),
    d_precision    = round(s$d_prec, 4),
    dollars_typed  = round(s$dollar_recall_typed, 4),
    dollars_anyerr = round(s$dollar_recall, 4),
    d_dollars      = round(s$d_doll, 4),
    fill_typed     = round(s$fill_typed, 2),
    fill_anyerr    = round(s$fill_anyerr, 2),
    stringsAsFactors = FALSE)
  print(out[order(-out$d_dollars), ], row.names = FALSE)
  # medians of the PER-STATE differences -- not the difference of the medians,
  # which is a different (and with few states, misleading) quantity
  cat(sprintf("\n  median per-state difference: precision %+.4f | dollars %+.4f\n",
              median(s$d_prec), median(s$d_doll)))
  cat(sprintf("  levels: precision typed %.4f vs any-error %.4f | dollars %.4f vs %.4f\n",
              median(s$precision_holdout), median(s$precision),
              median(s$dollar_recall_typed), median(s$dollar_recall)))
  cat(sprintf("  median fill ratio: typed %.2f vs any-error %.2f\n",
              median(s$fill_typed), median(s$fill_anyerr)))
  cat(sprintf("  typed wins: precision %d/%d, dollars %d/%d\n",
              sum(s$d_prec > 0), nrow(s), sum(s$d_doll > 0), nrow(s)))
}
cat("\nWorkload is held at the budget by construction on the TRAINING years, so\n")
cat("precision and dollar recall are the comparable outcomes; fill ratio shows\n")
cat("how much of the budgeted workload each vocabulary actually carries into 2024.\n")
