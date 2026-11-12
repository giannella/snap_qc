# Year-to-year stability check, the votes (2026-11-11). Eric: "We're just trying to see if there's consistency in
# what works for states over time." See run_stability.cmd beside this file.
# The workbook vote score (WB), built as wb_ref_f1109.R builds test 3's, from rules mined on FY2017-19 + FY2022:
# 300 rules allotted from the seed-117 national pool (pools/admitted_f1109_seed117.rds, mine_national_seed.R under
# SR_WINDOW=stab23), votes counted on FY2023 and on FY2024, each case scored with test 3's scorer (curve and size
# model fit on FY2022 votes from FY2017-19 rules; votes capped at 7): out_f1109_wb/wb/scorer_W3.rds, unchanged.
# By state, year and budget, as s46_f1109_wb_states.R does for FY2024: flags = max(1, round(budget x the state's
# cases that year)), filled with the top-scored kept cases (s46 keep_of(filter = "gap")); errors and error dollars
# over kept cases.
# Check first: the cached FY2024 test-3 scores (wb_ed_W3.rds) run through this file's keep rule and selection must
# reproduce WB_f1109_by_state_5_10pct.csv cell for cell, so the code path is the one the map's vote figures used.
#   [SMOKE=1] "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" methods/stability_fy2324/votes_stab.R
# Writes methods/stability_fy2324[/smoke]/votes_by_state.csv.
ROOT <- "C:/Users/ericg/snap_qc_v27"; setwd(ROOT)
Sys.setenv(SR_ROOT = ROOT, SR_WINDOW = "stab23")
source("methods/v270_seed_replicates/common.R")
source("methods/vote_scoring/vote_groups.R"); source("methods/vote_scoring/vote_fit.R")
VOTE_CAP <- 7L; BUDGETS_V <- c(0.05, 0.10)
representable <- function(pool) pool[!grepl("count_divisible_by_100", pool$rule, fixed = TRUE), , drop = FALSE]
WB <- "methods/case_predictions/s46/out_f1109_wb"
s <- readRDS(file.path(WB, "wb", "scorer_W3.rds"))
stopifnot(identical(s$rule_years, c("2017", "2018", "2019")), s$fit_year == "2022", s$vote_cap == VOTE_CAP)

kept_of <- function(a) {   # s46 keep_of(filter = "gap") on prep_features rows; piles are defined within fiscal year
  gap <- abs(a$rawben - a$raw_benefit_amount); gap[is.na(gap)] <- 0
  mm <- a$rawben >= a$benmax & a$rawben_uncapped < a$benmax; mm[is.na(mm)] <- FALSE
  pile <- income_pile_rows(a); pile[is.na(pile)] <- FALSE
  notes <- as.character(a$correctednotes); notes[is.na(notes) | notes == ""] <- "no_change"
  !mm & !pile & !grepl("prior_year_tables", notes) & gap <= 5
}
by_state <- function(sc, rows, keep, year) do.call(rbind, lapply(BUDGETS_V, function(b) do.call(rbind, lapply(
  split(seq_len(nrow(rows)), rows$st), function(ix) {
    kk <- max(1L, round(b * length(ix))); ix2 <- ix[keep[ix]]; sel <- ix2[order(-sc[ix2], ix2)[seq_len(min(kk, length(ix2)))]]
    data.frame(state = rows$st[ix[1]], year = year, budget = b, cases = length(ix), kept = length(ix2), flags = kk,
               errors_caught = sum(rows$ie[sel]), dollars_caught = sum(rows$ed[sel]),
               errors_all = sum(rows$ie[ix2]), dollars_all = sum(rows$ed[ix2]))
  }))))
score_of <- function(rows, vote) {
  p <- pred_curve(s$curve, vote, rows$hh) / s$national
  as.numeric(p * predict(s$size, size_data(rows$ed, rows$hh, rows$ben, rows$hsz, pmin(vote, VOTE_CAP)), type = "response"))
}

al <- NULL; out <- list()
for (wn in c("stab24", "stab23")) {   # FY2024 first: it carries the reproduction check
  WINDOW <- wn; TEST_YEAR <- if (wn == "stab23") "2023" else "2024"
  F <- load_frame("f1109"); tr <- which(F$is_tr); te <- which(F$yr == TEST_YEAR); NT <- length(te)
  stopifnot(!anyNA(F$adf$raw_benefit_amount[te]))
  rows <- data.frame(st = F$st[te], ie = F$ie[te], ed = F$ed[te], hh = F$hh[te],
                     ben = num(F$adf$raw_benefit_amount[te]), hsz = num(F$adf$cert_HH_size_FS_n[te]), stringsAsFactors = FALSE)
  keep <- kept_of(F$adf)[te]
  stamp("FY%s: %d cases, %d errors, %d kept", TEST_YEAR, NT, sum(rows$ie), sum(keep))
  if (wn == "stab24") {   # reproduce the map's FY2024 vote figures from the cached test-3 scores (pools not involved)
    w <- readRDS(file.path(WB, "wb", "wb_ed_W3.rds")); stopifnot(length(w$score) == NT, all(w$st == rows$st), all(w$ie == rows$ie))
    ref <- read.csv(file.path(WB, "WB_f1109_by_state_5_10pct.csv"))
    got <- merge(by_state(w$score, rows, keep, "2024"), ref, by = c("state", "budget"))
    stopifnot(nrow(got) == nrow(ref))
    for (k in c("cases", "kept", "flags", "errors_caught", "dollars_caught", "errors_all", "dollars_all"))
      if (!isTRUE(all.equal(got[[paste0(k, ".x")]], got[[paste0(k, ".y")]]))) stop("does not reproduce the FY2024 vote file: ", k)
    stamp("reproduces WB_f1109_by_state_5_10pct.csv in all %d state-budget cells", nrow(got))
  }
  if (is.null(al)) {   # the allotment depends on the training rows only, which both windows share
    S_tr <- strata_of(F$hh[tr]); ie_tr <- F$ie[tr]; ie_tr0 <- ie_tr
    fn <- pool_file("f1109", 117L); rules <- top_rules(representable(read_pool(fn, TRAIN_YEARS)))
    fl_tr <- flags_for_rules(rules, F$adf[tr, , drop = FALSE], S_tr)
    stopifnot(all(lengths(fl_tr) == rules$n), all(vapply(fl_tr, function(ix) sum(ie_tr[ix]), numeric(1)) == rules$k))
    al <- allot(build_groups(rules, fl_tr), 300L, "rep")
    rm(fl_tr); invisible(gc())
  } else stopifnot(identical(F$ie[tr], ie_tr0))
  S_te <- strata_of(F$hh[te])
  v <- votes_of(al, flags_for_rules(al, F$adf[te, , drop = FALSE], S_te), NT)$vote
  stamp("FY%s: %d rules allotted from %s (FY%s); %.1f%% of cases have a vote (max %d)", TEST_YEAR, nrow(al),
        sub(paste0(ROOT, "/"), "", fn), paste(TRAIN_YEARS, collapse = "+"), 100 * mean(v > 0), max(v))
  out[[wn]] <- by_state(score_of(rows, v), rows, keep, TEST_YEAR)
  rm(F); invisible(gc())
}
res <- do.call(rbind, out)
fo <- file.path(OUT_DIR, "votes_by_state.csv"); write.csv(res, fo, row.names = FALSE)
stamp("wrote %s (%d rows)", fo, nrow(res))
