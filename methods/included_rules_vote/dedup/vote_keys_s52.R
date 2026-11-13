# §52 part 3, counting correction (2026-11-12), step 1. The scoring of "included rules vote" (each_core10) added
# every 10% core list rule to the 300 national votes. In the workbook a list rule that duplicates a vote rule is
# merged into one row that votes once (rule_selection.py: "A vote rule that duplicates a list rule marks that rule
# Vote? = TRUE"); in §52 the vote rules and the lists came from the same pool, so those rules were counted twice
# (found by the fresh statistician review of the vote-table refit, 2026-11-12). This script reproduces each setting's
# 300 vote rules and saves their keys (hh, rule) for the corrected list counts.
#   map: the seed-117 FY2022-23 pool (methods/v270_seed_replicates/pools/admitted_f1109_seed117.rds), allotted as
#        wb_ref_f1109.R; check: the votes on FY2024 equal out_f1109_wb/wb/votes_W3.rds
#   y2y: the seed-117 FY2017-19 + FY2022 pool (methods/stability_fy2324/pools/admitted_f1109_seed117.rds), allotted as
#        votes_stab.R; check: the votes on FY2024 equal methods/included_rules_vote/votes_cases_y2y.rds (FY2024 rows)
#   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" methods/included_rules_vote/dedup/vote_keys_s52.R <map | y2y>
# Writes methods/included_rules_vote/dedup/votekeys_<map | y2y>.rds.
ROOT <- "C:/Users/ericg/snap_qc_v27"; setwd(ROOT)
SET <- commandArgs(TRUE)[1]; stopifnot(SET %in% c("map", "y2y"))
Sys.setenv(SR_ROOT = ROOT, SR_WINDOW = if (SET == "map") "fy2024" else "stab24")
source("methods/v270_seed_replicates/common.R")
source("methods/vote_scoring/vote_groups.R"); source("methods/vote_scoring/vote_fit.R")
OUTD <- "methods/included_rules_vote/dedup"; dir.create(OUTD, showWarnings = FALSE)
representable <- function(pool) pool[!grepl("count_divisible_by_100", pool$rule, fixed = TRUE), , drop = FALSE]
F <- load_frame("f1109"); tr <- which(F$is_tr); te <- which(F$yr == "2024"); NT <- length(te)
S_tr <- strata_of(F$hh[tr]); ie_tr <- F$ie[tr]
fn <- pool_file("f1109", 117L); rules <- top_rules(representable(read_pool(fn, TRAIN_YEARS)))
fl_tr <- flags_for_rules(rules, F$adf[tr, , drop = FALSE], S_tr)
stopifnot(all(lengths(fl_tr) == rules$n), all(vapply(fl_tr, function(ix) sum(ie_tr[ix]), numeric(1)) == rules$k))
al <- allot(build_groups(rules, fl_tr), 300L, "rep"); rm(fl_tr)
v <- votes_of(al, flags_for_rules(al, F$adf[te, , drop = FALSE], strata_of(F$hh[te])), NT)$vote
ref <- if (SET == "map") readRDS("methods/case_predictions/s46/out_f1109_wb/wb/votes_W3.rds") else {
  C <- readRDS("methods/included_rules_vote/votes_cases_y2y.rds"); C[C$year == "2024", ] }
stopifnot(nrow(ref) == NT, all(ref$st == F$st[te]), all(ref$ie == F$ie[te]), identical(as.integer(ref$vote), as.integer(v)))
stamp("%s: %d rules allotted from %s (FY%s); FY2024 votes equal the stored votes", SET, nrow(al), fn, paste(TRAIN_YEARS, collapse = "+"))
fo <- file.path(OUTD, sprintf("votekeys_%s.rds", SET))
saveRDS(data.frame(hh = al$hh, rule = al$rule, key = paste(al$hh, al$rule)), fo); stamp("wrote %s", fo)
