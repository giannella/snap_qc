# Add workload-matched "refill" metrics to the any-error 2024 scorecard.
#
# The scorecard as built scores the FROZEN CORE list: the list is filled against
# the state's FY2022-23 caseload, frozen, and scored on FY2024 with no refilling.
# That answers "what does last year's list do next year", and it is why the
# median state carries only 0.855 of its budgeted workload at the 5% budget.
#
# This script adds the second reading, the one a state actually experiences:
# walk the delivered list (core THEN buffer, in rank order) against the FY2024
# caseload and activate a rule when it adds unflagged cases and the running
# total stays within that year's review cap. Workload then equals the budget by
# construction, so precision is comparable across states and across arms. It is
# also the basis the admission auditions use (modeling_findings_detailed.md
# section 25), which is what makes those numbers comparable to these.
#
# The refill decides which rules to ACTIVATE from the state's current caseload.
# It never looks at FY2024 outcomes, so it is outcome-free; what it assumes is
# that the state re-walks the delivered list against its current pile, which is
# what the buffer rules exist for.
#
# Reads the frozen lists the run already wrote, so it needs no pools and no
# mining: a condition index over a few hundred delivered rules per state, not
# over the 32,633-rule pool.
#
# Expects `reg_model_data`. Rewrites holdout_metrics.{jsonl,json,csv} in place,
# adding fields and changing none of the existing ones.

suppressMessages({ library(dplyr); library(jsonlite) })
source("rule_mining_helpers.R")

BENCH_DIR    <- "methods/anyerror_blended_holdout_2024"
HOLDOUT_YEAR <- "2024"
BENCH_YEARS  <- c("2022", "2023")
ALL_YEARS    <- c("2022", "2023", "2024")
BUDGETS      <- c(0.05, 0.10)
LCB_Z        <- 2.326
FDR_ALPHA    <- 0.10
MIN_TRAIN_FLAGGED <- 30

HH_LEVELS <- c("1", "2-3", "4+")
hh_group_of <- function(n) {
  n <- suppressWarnings(as.numeric(as.character(n)))
  ifelse(is.na(n), NA_character_, ifelse(n <= 1, "1", ifelse(n <= 3, "2-3", "4+")))
}
features <- c(
  "HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size",
  "expedited_i", "cat_elig", "rawben_rel_max", "medical_deductions",
  "shelter_expenses_by_hh_size", "utilities", "married", "homeless",
  "rawearn_by_hh_size", "rawunearn_by_hh_size", "rawgross_by_hh_size",
  "percent_abawd", "unc_rawben_rel_max",
  "months_since_cert_n", "count_divisible_by_100"
)
targets_of <- function(df) {
  ie  <- !is.na(df$over_threshold) & df$over_threshold != 0
  amt <- df$total_error_amount; amt[is.na(amt)] <- 0
  list(ie = ie, ed = ifelse(ie, abs(amt), 0))
}
stamp <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"),
                                   sprintf(...)))

pf  <- prep_features(reg_model_data %>% filter(fiscal_year %in% ALL_YEARS), features)
adf <- pf$data
st  <- as.character(adf$state)
fy  <- as.character(adf$fiscal_year)

jsonl <- file.path(BENCH_DIR, "holdout_metrics.jsonl")
records <- lapply(readLines(jsonl, warn = FALSE), fromJSON)
stamp("read %d records", length(records))
states <- unique(vapply(records, function(r) as.character(r$state), character(1)))

# Walk core then buffer in rank order, taking a rule when it adds cases and the
# running total still fits this year's cap. Mirrors the audition's walk exactly.
refill <- function(idx, n_rows, budget) {
  cap <- floor(budget * n_rows)
  un <- rep(FALSE, n_rows); used <- 0L
  for (i in seq_along(idx)) {
    add <- sum(!un[idx[[i]]])
    if (add > 0 && sum(un) + add <= cap) { un[idx[[i]]] <- TRUE; used <- used + 1L }
  }
  list(hit = un, n_rules = used)
}

for (S in states) {
  ho_rows <- which(st == S & fy == HOLDOUT_YEAR)
  ho <- adf[ho_rows, , drop = FALSE]
  strata_ho <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(ho$cert_HH_size_FS_n) %in% h))
  tg <- targets_of(ho)
  w  <- ho$fywgt; w[is.na(w)] <- 0
  tot_unw <- sum(tg$ed); tot_wtd <- sum(w * tg$ed)

  for (b in BUDGETS) {
    f <- file.path(BENCH_DIR, sprintf("bench_list_%s_budget%02.0f.csv",
                                      gsub(" ", "_", S), 100 * b))
    lst <- read.csv(f, stringsAsFactors = FALSE)
    lst <- lst[order(lst$rank), , drop = FALSE]        # core then buffer, rank order
    ci  <- build_condition_index(lst$rule)
    cnd <- eval_conditions(ci$conditions, ho)
    idx <- vector("list", nrow(lst))
    for (h in unique(lst$hh)) {
      rows <- which(lst$hh == h)
      idx[rows] <- rule_flag_idx(ci$rule_conds[rows], cnd, base_idx = strata_ho[[h]])
    }
    r <- refill(idx, nrow(ho), b)
    hit <- r$hit
    fl_unw <- sum(tg$ed[hit]); fl_wtd <- sum((w * tg$ed)[hit])

    j <- which(vapply(records, function(x)
      identical(as.character(x$state), S) && isTRUE(all.equal(as.numeric(x$budget_pct), 100 * b)),
      logical(1)))
    if (length(j) != 1) stop(sprintf("no unique record for %s at budget %g", S, b))
    records[[j]]$refill_basis                    <- "core+buffer walked against the holdout caseload to the budget cap"
    records[[j]]$n_rules_deployed_refill         <- r$n_rules
    records[[j]]$n_rules_available_refill        <- nrow(lst)
    records[[j]]$n_cases_flagged_refill          <- sum(hit)
    records[[j]]$flagged_share_of_caseload_refill<- sum(hit) / nrow(ho)
    records[[j]]$n_errors_flagged_refill         <- sum(tg$ie & hit)
    records[[j]]$precision_refill                <- if (sum(hit) > 0) sum(tg$ie & hit) / sum(hit) else NA_real_
    records[[j]]$per_reduction_pts_unweighted_refill <- if (tot_unw > 0) 100 * fl_unw / tot_unw else NA_real_
    records[[j]]$per_reduction_pts_weighted_refill   <- if (tot_wtd > 0) 100 * fl_wtd / tot_wtd else NA_real_
    records[[j]]$error_dollars_flagged_holdout_unweighted_refill <- fl_unw
    records[[j]]$error_dollars_flagged_holdout_weighted_refill   <- fl_wtd
    stamp("%-22s %2.0f%%: frozen core %d rules / %.3f workload / prec %.3f | refill %d rules / %.3f / prec %.3f",
          S, 100 * b, records[[j]]$n_rules_core,
          records[[j]]$flagged_share_of_caseload, records[[j]]$precision_holdout,
          r$n_rules, sum(hit) / nrow(ho),
          if (sum(hit) > 0) sum(tg$ie & hit) / sum(hit) else NA_real_)
  }
}

con <- file(jsonl, open = "w")
for (r in records) writeLines(toJSON(r, auto_unbox = TRUE, digits = 6), con)
close(con)
df <- bind_rows(lapply(records, as.data.frame, stringsAsFactors = FALSE))
write.csv(df, file.path(BENCH_DIR, "holdout_metrics.csv"), row.names = FALSE)
write_json(list(
  holdout_fiscal_year = HOLDOUT_YEAR,
  train_years         = paste(BENCH_YEARS, collapse = "-"),
  delivered_rule_years= "2022-2023-2024",
  vocabulary          = "any_error (the shipped delivery vocabulary)",
  admission           = sprintf("BH FDR %.0f%% vs stratum base rate AND n >= %d",
                                100 * FDR_ALPHA, MIN_TRAIN_FLAGGED),
  ranking             = sprintf("one-sided %.1f%% Wilson LCB of training precision",
                                100 * (1 - pnorm(-LCB_Z))),
  per_basis           = "dollar recall: flagged error dollars / total error dollars",
  scoring_bases       = paste(
    "two readings per state and budget.",
    "FROZEN CORE (fields without a _refill suffix): the core list filled on the",
    "training caseload, frozen, scored on the holdout year with no refilling.",
    "REFILL (fields suffixed _refill): core and buffer walked against the holdout",
    "caseload to that year's budget cap, so workload equals the budget by",
    "construction and precision is workload-matched. The refill uses the holdout",
    "caseload but never holdout outcomes."),
  n_records           = length(records),
  records             = df),
  file.path(BENCH_DIR, "holdout_metrics.json"),
  auto_unbox = TRUE, digits = 6, pretty = TRUE)
stamp("=== rewrote scorecard with refill metrics: %d records ===", length(records))
