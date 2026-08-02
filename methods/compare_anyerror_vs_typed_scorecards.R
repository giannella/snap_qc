# Any-error vs typed five-frame vocabulary, 49 states, one bench design.
#
# Both scorecards come from the same frozen-list design: mine 2022-23, fill the
# state's list against its 2022-23 caseload, freeze it, score the CORE list on
# the state's 2024 cases. Admission (BH FDR 10% + n >= 30), ranking (99% Wilson
# LCB) and fill are identical, so the vocabulary is the only difference.
#
# This is NOT the same measurement as the fdr10f arm of
# methods/state_similarity_v2/transfer_benchmark_train2223_test24/fdr_admission_audition.csv.
# That arm re-walks core AND buffer against the 2024 caseload and stops at the
# 2024 cap, so its workload equals the budget by construction (0.048-0.100 in
# all 36 cells). Fill ratios from the two studies are not comparable.
#
# Inputs : methods/{anyerror,typed}_blended_holdout_2024/holdout_metrics.jsonl
# Output : methods/anyerror_blended_holdout_2024/anyerror_vs_typed_2024.csv
suppressMessages({ library(dplyr); library(jsonlite) })

rd <- function(p) {
  recs <- lapply(readLines(p, warn = FALSE), fromJSON)
  bind_rows(lapply(recs, as.data.frame, stringsAsFactors = FALSE))
}
a <- rd("methods/anyerror_blended_holdout_2024/holdout_metrics.jsonl")
t <- rd("methods/typed_blended_holdout_2024/holdout_metrics.jsonl")

k <- c("state", "budget_pct")
keep <- c("n_rules_core", "precision_holdout", "flagged_share_of_caseload",
          "per_reduction_pts_unweighted", "per_reduction_pts_weighted")
m <- inner_join(a %>% select(all_of(c(k, keep, "base_rate_holdout",
                                      "n_cases_holdout", "n_errors_holdout"))),
                t %>% select(all_of(c(k, keep))),
                by = k, suffix = c("_any", "_typed"))

# fill ratio  = share of the review budget the frozen core list actually used on
#               the holdout year. Below 1 means the list under-fired and the
#               state would activate buffer rules to reach its budget.
# yield       = dollar recall per unit of caseload actually reviewed. Raw dollar
#               recall is not workload-matched, so a list that over-fires buys
#               dollars with extra review capacity; this normalises that away.
m <- m %>% mutate(
  fill_any    = flagged_share_of_caseload_any   / (budget_pct / 100),
  fill_typed  = flagged_share_of_caseload_typed / (budget_pct / 100),
  yield_any   = per_reduction_pts_unweighted_any   / (100 * flagged_share_of_caseload_any),
  yield_typed = per_reduction_pts_unweighted_typed / (100 * flagged_share_of_caseload_typed),
  lift_any    = precision_holdout_any   / base_rate_holdout,
  lift_typed  = precision_holdout_typed / base_rate_holdout)

cat(sprintf("states in both runs: %d\n\n", n_distinct(m$state)))
med <- function(x) sprintf("%.3f", median(x, na.rm = TRUE))
for (b in sort(unique(m$budget_pct))) {
  s <- m %>% filter(budget_pct == b)
  wins <- function(x, y) sprintf("%d/%d", sum(x > y, na.rm = TRUE), nrow(s))
  cat(sprintf("=== budget %2.0f%% (n = %d states) ===\n", b, nrow(s)))
  cat(sprintf("  precision       any %s  typed %s   any wins %s\n",
      med(s$precision_holdout_any), med(s$precision_holdout_typed),
      wins(s$precision_holdout_any, s$precision_holdout_typed)))
  cat(sprintf("  lift x base     any %s  typed %s\n", med(s$lift_any), med(s$lift_typed)))
  cat(sprintf("  PER reduction %% any %s  typed %s   any wins %s\n",
      med(s$per_reduction_pts_unweighted_any), med(s$per_reduction_pts_unweighted_typed),
      wins(s$per_reduction_pts_unweighted_any, s$per_reduction_pts_unweighted_typed)))
  cat(sprintf("  PER weighted  %% any %s  typed %s\n",
      med(s$per_reduction_pts_weighted_any), med(s$per_reduction_pts_weighted_typed)))
  cat(sprintf("  fill ratio      any %s  typed %s   (below 0.9: any %d, typed %d)\n",
      med(s$fill_any), med(s$fill_typed),
      sum(s$fill_any < 0.9, na.rm = TRUE), sum(s$fill_typed < 0.9, na.rm = TRUE)))
  cat(sprintf("  $ per reviewed  any %s  typed %s   any wins %s\n",
      med(s$yield_any), med(s$yield_typed), wins(s$yield_any, s$yield_typed)))
  cat(sprintf("  core rules      any %s  typed %s\n",
      med(s$n_rules_core_any), med(s$n_rules_core_typed)))
  cat(sprintf("  at or below base rate: any %d, typed %d\n\n",
      sum(s$precision_holdout_any <= s$base_rate_holdout, na.rm = TRUE),
      sum(s$precision_holdout_typed <= s$base_rate_holdout, na.rm = TRUE)))
}
write.csv(m, "methods/anyerror_blended_holdout_2024/anyerror_vs_typed_2024.csv",
          row.names = FALSE)
cat("wrote methods/anyerror_blended_holdout_2024/anyerror_vs_typed_2024.csv\n")
