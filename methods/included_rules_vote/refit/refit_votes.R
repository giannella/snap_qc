# Vote-table refit, step 1 (2026-11-12; design note ../design_note_table_refit_2026-11-12.md). For one seed: the
# 300 national vote rules allotted from that seed's FY2017-19 pool (methods/v270_seed_replicates/fy2022/pools/
# admitted_f1109_seed<SEED>.rds), exactly as wb_ref_f1109.R and deploy_table.R allot them, and each FY2022,
# FY2023 and FY2024 case's vote count. Also keeps each case's kept flag (s46 keep rule, as votes_cases_y2y.R), the
# recreated benefit (raw_benefit_amount, the evaluation basis) and the issued benefit (rawben, the workbook's).
# Check: seed 117's FY2022 votes equal methods/case_predictions/s46/out_f1109_wb/wb/votes_W2.rds.
#   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" methods/included_rules_vote/refit/refit_votes.R 117
# Writes methods/included_rules_vote/refit/votes_seed<SEED>.rds: one row per FY2022-24 case, frame order by year.
ROOT <- "C:/Users/ericg/snap_qc_v27"; setwd(ROOT)
SEED <- as.integer(commandArgs(TRUE)[1]); stopifnot(SEED %in% 117:119)
Sys.setenv(SR_ROOT = ROOT, SR_WINDOW = "fy2022")
source("methods/v270_seed_replicates/common.R")
source("methods/vote_scoring/vote_groups.R"); source("methods/vote_scoring/vote_fit.R")
stopifnot(identical(TRAIN_YEARS, c("2017", "2018", "2019")))
OUTD <- "methods/included_rules_vote/refit"; dir.create(OUTD, showWarnings = FALSE)
representable <- function(pool) pool[!grepl("count_divisible_by_100", pool$rule, fixed = TRUE), , drop = FALSE]
kept_of <- function(a) {   # s46 keep_of(filter = "gap"), as votes_cases_y2y.R
  gap <- abs(a$rawben - a$raw_benefit_amount); gap[is.na(gap)] <- 0
  mm <- a$rawben >= a$benmax & a$rawben_uncapped < a$benmax; mm[is.na(mm)] <- FALSE
  pile <- income_pile_rows(a); pile[is.na(pile)] <- FALSE
  notes <- as.character(a$correctednotes); notes[is.na(notes) | notes == ""] <- "no_change"
  !mm & !pile & !grepl("prior_year_tables", notes) & gap <= 5
}
# training rows are FY2017-19 in every pass; the test-year counts are the frame's (EXPECT_F1109 fy2022, stab23, stab24)
EXPECT_F1109$a2022 <- EXPECT_F1109$fy2022
EXPECT_F1109$a2023 <- c(EXPECT_F1109$fy2022[1:2], EXPECT_F1109$stab23[3:4])
EXPECT_F1109$a2024 <- c(EXPECT_F1109$fy2022[1:2], EXPECT_F1109$stab24[3:4])

al <- NULL; CASES <- list()
for (y in c("2022", "2023", "2024")) {
  WINDOW <- paste0("a", y); TEST_YEAR <- y
  F <- load_frame("f1109"); tr <- which(F$is_tr); te <- which(F$yr == y); NT <- length(te)
  stopifnot(!anyNA(F$adf$raw_benefit_amount[te]))
  rows <- data.frame(year = y, st = F$st[te], ie = F$ie[te], ed = F$ed[te], hh = F$hh[te],
                     ben = num(F$adf$raw_benefit_amount[te]), ben_issued = num(F$adf$rawben[te]),
                     hsz = num(F$adf$cert_HH_size_FS_n[te]), stringsAsFactors = FALSE)
  keep <- kept_of(F$adf)[te]
  if (is.null(al)) {
    S_tr <- strata_of(F$hh[tr]); ie_tr <- F$ie[tr]; ie_tr0 <- ie_tr
    fn <- pool_file("f1109", SEED); rules <- top_rules(representable(read_pool(fn, TRAIN_YEARS)))
    fl_tr <- flags_for_rules(rules, F$adf[tr, , drop = FALSE], S_tr)
    stopifnot(all(lengths(fl_tr) == rules$n), all(vapply(fl_tr, function(ix) sum(ie_tr[ix]), numeric(1)) == rules$k))
    al <- allot(build_groups(rules, fl_tr), 300L, "rep")
    rm(fl_tr); invisible(gc())
  } else stopifnot(identical(F$ie[tr], ie_tr0))
  v <- votes_of(al, flags_for_rules(al, F$adf[te, , drop = FALSE], strata_of(F$hh[te])), NT)$vote
  stamp("seed %d FY%s: %d cases, %d errors, %d kept; %d rules allotted from %s; %.1f%% of cases have a vote (max %d)",
        SEED, y, NT, sum(rows$ie), sum(keep), nrow(al), sub(paste0(ROOT, "/"), "", fn), 100 * mean(v > 0), max(v))
  if (y == "2022" && SEED == 117L) {
    w2 <- readRDS("methods/case_predictions/s46/out_f1109_wb/wb/votes_W2.rds")
    stopifnot(nrow(w2) == NT, all(w2$st == rows$st), all(w2$ie == rows$ie), identical(as.integer(w2$vote), as.integer(v)))
    stamp("check: seed 117 FY2022 votes equal votes_W2.rds")
  }
  CASES[[y]] <- cbind(rows, vote = as.integer(v), keep = keep)
  rm(F); invisible(gc())
}
fo <- file.path(OUTD, sprintf("votes_seed%d.rds", SEED))
saveRDS(do.call(rbind, CASES), fo); stamp("wrote %s", fo)
# the allotted rules' keys: refit_lists.R leaves these out of the core-list count, because the workbook merges a list
# rule that duplicates a vote rule into one row that votes once (rule_selection.py, "A vote rule that duplicates a
# list rule marks that rule Vote? = TRUE")
fk <- file.path(OUTD, sprintf("votekeys_seed%d.rds", SEED))
saveRDS(data.frame(hh = al$hh, rule = al$rule, key = paste(al$hh, al$rule)), fk); stamp("wrote %s (%d keys)", fk, nrow(al))
