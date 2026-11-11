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
PHASE <- commandArgs(TRUE)[1]; stopifnot(PHASE %in% c("votes", "score"), Sys.getenv("SR_FRAME") == "f1030", Sys.getenv("SMOKE") == "")
source("methods/v270_seed_replicates/common.R")
source("methods/vote_scoring/vote_groups.R"); source("methods/vote_scoring/vote_fit.R")
OUT <- file.path(ROOT, "methods/case_predictions/s46", Sys.getenv("S46_OUT", "out_a2"), "wb")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
logf <- file.path(dirname(OUT), "s46_log.txt")
say <- function(...) { m <- sprintf("[%s] wb %s", format(Sys.time(), "%m-%d %H:%M:%S"), sprintf(...)); cat(m, "\n"); cat(m, "\n", file = logf, append = TRUE) }
representable <- function(pool) pool[!grepl("count_divisible_by_100", pool$rule, fixed = TRUE), , drop = FALSE]

if (PHASE == "votes") {
  F <- load_frame("f1030"); tr <- which(F$is_tr); te <- which(F$yr == TEST_YEAR); NT <- length(te)
  S_tr <- strata_of(F$hh[tr]); S_te <- strata_of(F$hh[te]); ie_tr <- F$ie[tr]
  stopifnot(!anyNA(F$adf$raw_benefit_amount[te]))
  rows <- data.frame(st = F$st[te], ie = F$ie[te], ed = F$ed[te], hh = F$hh[te],
                     ben = num(F$adf$raw_benefit_amount[te]), hsz = num(F$adf$cert_HH_size_FS_n[te]), stringsAsFactors = FALSE)
  votes_from <- function(seed) {
    fn <- pool_file("f1030", seed); rules <- top_rules(representable(read_pool(fn, TRAIN_YEARS)))
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
    d <- size_data(rep(rows$ed, k), h, rep(rows$ben, k), rep(rows$hsz, k), x)
    list(curve = fc, size = fit_size(SIZE_FORMS$benefit_size_vote, d, seq_len(nrow(d))), national = mean(rows$ie),
         fit_year = TEST_YEAR, rule_years = TRAIN_YEARS, seeds = names(V), min_bin = min_bin) }
  if (WINDOW == "fy2019") { s <- scorer_of(list(`117` = votes_from(117L)), 50L); saveRDS(s, file.path(OUT, "scorer_W2.rds"))
    say("test 2 scorer: FY2019 votes from FY2017-18 rules; size coefs %s", paste(sprintf("%s %.4f", names(coef(s$size)), coef(s$size)), collapse = ", ")) }
  if (WINDOW == "fy2022") {
    V <- lapply(setNames(nm = 117:119), votes_from); s <- scorer_of(V, 150L)
    shipped <- read.csv("methods/vote_scoring/deploy/vote_table.csv")
    rel <- unlist(lapply(HH_LEVELS, function(l) s$curve[[l]]$rate / s$national))
    stopifnot(length(rel) == nrow(shipped), isTRUE(all.equal(unname(rel), shipped$rel_rate, tolerance = 1e-9)))
    saveRDS(s, file.path(OUT, "scorer_W3.rds")); saveRDS(cbind(rows, vote = V[["117"]]), file.path(OUT, "votes_W2.rds"))
    say("test 3 scorer: FY2022 votes from FY2017-19 rules, seeds 117-119; curve matches the shipped vote_table.csv; size coefs %s",
        paste(sprintf("%s %.4f", names(coef(s$size)), coef(s$size)), collapse = ", ")) }
  if (WINDOW == "fy2024") saveRDS(cbind(rows, vote = votes_from(117L)), file.path(OUT, "votes_W3.rds"))
}

if (PHASE == "score") for (wn in c("W2", "W3")) {
  fs <- file.path(OUT, sprintf("scorer_%s.rds", wn)); fv <- file.path(OUT, sprintf("votes_%s.rds", wn))
  if (!all(file.exists(c(fs, fv)))) { say("%s: scorer or votes missing, not scored", wn); next }
  s <- readRDS(fs); r <- readRDS(fv)
  p <- pred_curve(s$curve, r$vote, r$hh) / s$national
  sc <- p * predict(s$size, size_data(r$ed, r$hh, r$ben, r$hsz, r$vote), type = "response")
  saveRDS(list(score = as.numeric(sc), st = r$st, ie = r$ie), file.path(OUT, sprintf("wb_ed_%s.rds", wn)))
  say("%s: %d cases scored with the FY%s scorer (rules FY%s); score range %.1f-%.1f", wn, nrow(r), s$fit_year,
      paste(s$rule_years, collapse = "-"), min(sc), max(sc))
}
