# wb_ref.R on frame f1109, test 3 only (2026-11-11, for the results map; Eric asked for the workbook vote scores on f1109).
# Copied from wb_ref.R (the §46 Amendment 2 file, left unchanged). Differences from it:
#   - frame f1109 (wb_ref.R is fixed to f1030); output folder out_f1109_wb/wb.
#   - votes capped at 7 in the size model, fit and prediction, as deploy_table.R and the shipped workbook now do
#     (MIN(Votes, 7); Eric, 2026-11-07/09, after wb_ref.R was written). The uncapped score is kept beside it.
#   - the score phase scores test 3 (W3) only. The fy2019 window (test 2's scorer) is not run.
#   SR_FRAME=f1109 SR_WINDOW=<fy2022 | fy2024> S46_OUT=out_f1109_wb Rscript wb_ref_f1109.R votes
#   SR_FRAME=f1109 SR_WINDOW=fy2022 S46_OUT=out_f1109_wb Rscript wb_ref_f1109.R score
# The original header follows.
# §46 Amendment 2, change 3: the voted-workbook reference (WB). Comparison only; it cannot be chosen.
#   SR_FRAME=f1030 SR_WINDOW=<fy2019 | fy2022 | fy2024> S46_OUT=out_a2 Rscript wb_ref.R votes
#   SR_FRAME=f1030 SR_WINDOW=fy2022 S46_OUT=out_a2 Rscript wb_ref.R score
# votes, by window (each window's own rule pools and frame rows, through the vote study's code):
#   fy2019: FY2017-18 rules (seed 117) -> votes on FY2019 -> curve and size model fit there: test 2's scorer
#   fy2022: FY2017-19 rules (seeds 117-119, stacked) -> votes on FY2022 -> curve and size model as deploy_table.R
#           fits them: test 3's scorer. The seed-117 votes on FY2022 are test 2's scored votes.
#   fy2024: FY2022-23 rules (seed 117) -> votes on FY2024: test 3's scored votes
# score: expected $ per case on FY2022 (test 2) and FY2024 (test 3); s46_a2.R extra scores them.
# The size model uses the recreated benefit (raw_benefit_amount), by the same rule as lben; the shipped workbook's
# issued benefit would read the recreation gap on the QC frame. The FNS level is one constant per state and is left out.
# Writes methods/case_predictions/s46/<S46_OUT>/wb/.
ROOT <- "C:/Users/ericg/snap_qc_v27"; setwd(ROOT); Sys.setenv(SR_ROOT = ROOT)
PHASE <- commandArgs(TRUE)[1]; stopifnot(PHASE %in% c("votes", "score"), Sys.getenv("SR_FRAME") == "f1109", Sys.getenv("SMOKE") == "",
                                          Sys.getenv("S46_OUT") == "out_f1109_wb")
source("methods/v270_seed_replicates/common.R")
source("methods/vote_scoring/vote_groups.R"); source("methods/vote_scoring/vote_fit.R")
VOTE_CAP <- 7L   # deploy_table.R's cap
OUT <- file.path(ROOT, "methods/case_predictions/s46", Sys.getenv("S46_OUT"), "wb")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
logf <- file.path(dirname(OUT), "s46_log.txt")
say <- function(...) { m <- sprintf("[%s] wb %s", format(Sys.time(), "%m-%d %H:%M:%S"), sprintf(...)); cat(m, "\n"); cat(m, "\n", file = logf, append = TRUE) }
representable <- function(pool) pool[!grepl("count_divisible_by_100", pool$rule, fixed = TRUE), , drop = FALSE]

if (PHASE == "votes") {
  stopifnot(WINDOW %in% c("fy2022", "fy2024"))
  F <- load_frame("f1109"); tr <- which(F$is_tr); te <- which(F$yr == TEST_YEAR); NT <- length(te)
  S_tr <- strata_of(F$hh[tr]); S_te <- strata_of(F$hh[te]); ie_tr <- F$ie[tr]
  stopifnot(!anyNA(F$adf$raw_benefit_amount[te]))
  rows <- data.frame(st = F$st[te], ie = F$ie[te], ed = F$ed[te], hh = F$hh[te],
                     ben = num(F$adf$raw_benefit_amount[te]), hsz = num(F$adf$cert_HH_size_FS_n[te]), stringsAsFactors = FALSE)
  votes_from <- function(seed) {
    fn <- pool_file("f1109", seed); rules <- top_rules(representable(read_pool(fn, TRAIN_YEARS)))
    fl_tr <- flags_for_rules(rules, F$adf[tr, , drop = FALSE], S_tr)
    stopifnot(all(lengths(fl_tr) == rules$n), all(vapply(fl_tr, function(ix) sum(ie_tr[ix]), numeric(1)) == rules$k))
    al <- allot(build_groups(rules, fl_tr), 300L, "rep")
    v <- votes_of(al, flags_for_rules(al, F$adf[te, , drop = FALSE], S_te), NT)$vote
    say("%s seed %d: %d rules allotted from %s; %.1f%% of FY%s cases have a vote (max %d)", WINDOW, seed, nrow(al),
        sub(paste0(ROOT, "/"), "", fn), 100 * mean(v > 0), TEST_YEAR, max(v))
    v }
  # curve (relative error rate by size group and vote bin) and gamma size model, stacked over seeds as deploy_table.R does
  scorer_of <- function(V, min_bin) {
    k <- length(V); x <- unlist(V); y <- rep(rows$ie, k); h <- rep(rows$hh, k)
    MIN_BIN_ERRS <<- min_bin; fc <- fit_curve(x, y, h); MIN_BIN_ERRS <<- 50L
    d <- size_data(rep(rows$ed, k), h, rep(rows$ben, k), rep(rows$hsz, k), pmin(x, VOTE_CAP))
    d0 <- size_data(rep(rows$ed, k), h, rep(rows$ben, k), rep(rows$hsz, k), x)   # uncapped, as wb_ref.R; comparison only
    list(curve = fc, size = fit_size(SIZE_FORMS$benefit_size_vote, d, seq_len(nrow(d))),
         size_uncapped = fit_size(SIZE_FORMS$benefit_size_vote, d0, seq_len(nrow(d0))), national = mean(rows$ie),
         fit_year = TEST_YEAR, rule_years = TRAIN_YEARS, seeds = names(V), min_bin = min_bin, vote_cap = VOTE_CAP) }
  if (WINDOW == "fy2022") {
    V <- lapply(setNames(nm = 117:119), votes_from); s <- scorer_of(V, 150L)
    shipped <- read.csv("methods/vote_scoring/deploy/vote_table.csv")
    rel <- unlist(lapply(HH_LEVELS, function(l) s$curve[[l]]$rate / s$national))
    stopifnot(length(rel) == nrow(shipped), isTRUE(all.equal(unname(rel), shipped$rel_rate, tolerance = 1e-9)))
    saveRDS(s, file.path(OUT, "scorer_W3.rds")); saveRDS(cbind(rows, vote = V[["117"]]), file.path(OUT, "votes_W2.rds"))
    say("test 3 scorer: FY2022 votes from FY2017-19 rules, seeds 117-119; curve matches the shipped vote_table.csv; size coefs (votes capped at %d) %s; uncapped %s",
        VOTE_CAP, paste(sprintf("%s %.4f", names(coef(s$size)), coef(s$size)), collapse = ", "),
        paste(sprintf("%s %.4f", names(coef(s$size_uncapped)), coef(s$size_uncapped)), collapse = ", ")) }
  if (WINDOW == "fy2024") saveRDS(cbind(rows, vote = votes_from(117L)), file.path(OUT, "votes_W3.rds"))
}

if (PHASE == "score") for (wn in "W3") {
  fs <- file.path(OUT, sprintf("scorer_%s.rds", wn)); fv <- file.path(OUT, sprintf("votes_%s.rds", wn))
  if (!all(file.exists(c(fs, fv)))) stop(wn, ": scorer or votes missing")
  s <- readRDS(fs); r <- readRDS(fv)
  p <- pred_curve(s$curve, r$vote, r$hh) / s$national
  sc <- p * predict(s$size, size_data(r$ed, r$hh, r$ben, r$hsz, pmin(r$vote, VOTE_CAP)), type = "response")
  sc0 <- p * predict(s$size_uncapped, size_data(r$ed, r$hh, r$ben, r$hsz, r$vote), type = "response")
  saveRDS(list(score = as.numeric(sc), score_uncapped = as.numeric(sc0), st = r$st, ie = r$ie), file.path(OUT, sprintf("wb_ed_%s.rds", wn)))
  say("%s: %d cases scored with the FY%s scorer (rules FY%s); score range %.1f-%.1f", wn, nrow(r), s$fit_year,
      paste(s$rule_years, collapse = "-"), min(sc), max(sc))
}
