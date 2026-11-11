# The vote-score table the workbooks ship (2026-10-29; design note
# "Confirmation result and decision", deployment plan agreed 2026-10-28).
#   SR_WINDOW=fy2022 Rscript methods/vote_scoring/deploy_table.R
# The workbook applies a curve to rules about three years old (FY2022-24 rules
# on FY2027 cases), so the curve is fitted at that age: rules mined on
# FY2017-19 (seeds 117-119, stacked), votes counted on FY2022, and checked on
# FY2023, a year this study never opened. The shipped form is RELATIVE: each
# bin's error rate over the national FY2022 error rate. The workbook sets the
# level from the state's FNS-published payment error rate.
# Rules using count_divisible_by_100 are dropped before grouping: the workbook
# cannot compute that field, so the shipped vote never uses it either.
# Writes methods/vote_scoring/deploy/: vote_table.csv, size_coefs.csv,
# check_fy2023_bins.csv, check_fy2023_gain.csv.
# Votes are capped at 7 in the error-size model (Eric, 2026-11-07/09), to match the rate table's "7 or more"
# display: the uncapped slope extrapolated past the data (in MI, a 3-vote case outranked a 23-vote case on size
# alone). The workbook applies the same cap (make_vote.py, MIN(Votes, 7)). The FY2023 check also reports the
# uncapped size model's gain, for comparison (columns *_uncapped).
stopifnot(Sys.getenv("SR_WINDOW") == "fy2022")
source("methods/v270_seed_replicates/common.R")
source("methods/vote_scoring/vote_groups.R"); source("methods/vote_scoring/vote_fit.R")
OUT <- "methods/vote_scoring/deploy"
if (SMOKE) OUT <- file.path(OUT, "smoke")
dir.create(OUT, showWarnings = FALSE)
# SR_FRAME (2026-10-29): final = the v2.7 frame 1dbe8733 (the table first shipped);
# f1027 = the staging frame eb989fda, with all three seeds mined fresh on it
FR <- Sys.getenv("SR_FRAME", "final"); stopifnot(FR %in% names(FRAMES))
POOLS <- if (FR == "final") c(`117` = "methods/v270_cpi_benchmark/fy2022_cpi/cache/bench_national_117.rds",
           `118` = "methods/v270_seed_replicates/fy2022/pools/admitted_final_seed118.rds",
           `119` = "methods/v270_seed_replicates/fy2022/pools/admitted_final_seed119.rds") else
  setNames(vapply(117:119, function(sd) pool_file(FR, sd), ""), 117:119)   # POOL_DIR (smoke-aware)
representable <- function(pool) pool[!grepl("count_divisible_by_100", pool$rule, fixed = TRUE), , drop = FALSE]
set.seed(20261031L)
VOTE_CAP <- 7L

F <- load_frame(FR)
tr <- which(F$is_tr); te <- which(F$yr == TEST_YEAR); NT <- length(te)
S_tr <- strata_of(F$hh[tr]); S_te <- strata_of(F$hh[te])
ie <- F$ie[te]; ed <- F$ed[te]; hh <- F$hh[te]; st <- F$st[te]
ben <- num(F$adf$rawben[te]); hsz <- num(F$adf$cert_HH_size_FS_n[te])
ie_tr <- F$ie[tr]; hh_tr <- F$hh[tr]
base_mine <- vapply(HH_LEVELS, function(h) mean(ie_tr[hh_tr == h]), numeric(1))

reg <- readRDS(FRAMES[[FR]]); a23 <- reg[as.character(reg$fiscal_year) == "2023", , drop = FALSE]; rm(reg)
a23 <- prep_features(a23, VOCAB19)$data
ie23 <- !is.na(a23$over_threshold) & a23$over_threshold != 0
ed23 <- ifelse(ie23, abs(ifelse(is.na(a23$total_error_amount), 0, a23$total_error_amount)), 0)
hh23 <- hh_group_of(a23$cert_HH_size_FS_n); st23 <- as.character(a23$state)
ben23 <- num(a23$rawben); hsz23 <- num(a23$cert_HH_size_FS_n)
stamp("FY2022: %d cases, %d errors; FY2023: %d cases, %d errors", NT, sum(ie), nrow(a23), sum(ie23))

V <- lapply(names(POOLS), function(s) {
  pool <- representable(read_pool(POOLS[[s]], TRAIN_YEARS))
  rules <- top_rules(pool)
  fl_tr <- flags_for_rules(rules, F$adf[tr, , drop = FALSE], S_tr)
  stopifnot(all(lengths(fl_tr) == rules$n), all(vapply(fl_tr, function(ix) sum(ie_tr[ix]), numeric(1)) == rules$k))
  al <- allot(build_groups(rules, fl_tr), 300L, "rep")
  v22 <- votes_of(al, flags_for_rules(al, F$adf[te, , drop = FALSE], S_te), NT)$vote
  v23 <- votes_of(al, flags_for_rules(al, a23, strata_of(hh23)), nrow(a23))$vote
  stamp("seed %s: %d rules allotted; flag %.1f%% of FY2022 and %.1f%% of FY2023 cases", s, nrow(al),
        100 * mean(v22 > 0), 100 * mean(v23 > 0))
  list(v22 = v22, v23 = v23)
})
names(V) <- names(POOLS)

## ---- the table: three seeds stacked, bins pooled to 3 x 50 errors ----------
x <- unlist(lapply(V, `[[`, "v22")); y <- rep(ie, 3); h <- rep(hh, 3)
MIN_BIN_ERRS <- 150L
fc <- fit_curve(x, y, h)
MIN_BIN_ERRS <- 50L
national <- mean(ie)
tab <- bind_rows(lapply(HH_LEVELS, function(l) {
  f <- fc[[l]]
  data.frame(hh = l, votes_from = f$lo, votes_to = c(f$lo[-1] - 1, NA), cases_per_seed = f$n / 3,
             errors_per_seed = f$e / 3, raw_rate = f$raw, rate = f$rate, rel_rate = f$rate / national,
             rr_vs_vote0 = f$rate / f$rate[1])
}))
write.csv(tab, file.path(OUT, "vote_table.csv"), row.names = FALSE)
print(tab, digits = 3)

d <- size_data(rep(ed, 3), h, rep(ben, 3), rep(hsz, 3), pmin(x, VOTE_CAP))
sz <- fit_size(SIZE_FORMS$benefit_size_vote, d, seq_len(nrow(d)))
sz0 <- fit_size(SIZE_FORMS$benefit_size_vote, size_data(rep(ed, 3), h, rep(ben, 3), rep(hsz, 3), x), seq_len(nrow(d)))   # uncapped, comparison only
co <- coef(sz)
names(co) <- sub("^hsz", "hsz", names(co))
write.csv(data.frame(term = names(co), estimate = unname(co)), file.path(OUT, "size_coefs.csv"), row.names = FALSE)
print(co)

## ---- check on FY2023 ---------------------------------------------------------
score_of <- function(v, hh_, ben_, hsz_, ed_, cap = TRUE) {
  p <- pred_curve(fc, v, hh_) / national
  if (cap) p * predict(sz, size_data(ed_, hh_, ben_, hsz_, pmin(v, VOTE_CAP)), type = "response") else
    p * predict(sz0, size_data(ed_, hh_, ben_, hsz_, v), type = "response")
}
top_k <- function(score, tieb, st_, budget) unlist(lapply(split(seq_along(score), st_), function(ix) {
  k <- max(1L, round(budget * length(ix))); ix[order(-score[ix], -tieb[ix], runif(length(ix)))][seq_len(k)]
}), use.names = FALSE)
simple23 <- ben23 * base_mine[hh23]
bins <- list(); gains <- list()
for (s in names(V)) {
  v <- V[[s]]$v23
  sc <- score_of(v, hh23, ben23, hsz23, ed23); sc0 <- score_of(v, hh23, ben23, hsz23, ed23, cap = FALSE)
  for (l in HH_LEVELS) {
    f <- fc[[l]]; i <- hh23 == l; b <- bin_of(f, v[i])
    bins[[length(bins) + 1]] <- data.frame(seed = s, hh = l, votes_from = f$lo, table_rel = f$rate / national,
      cases = tabulate(b, length(f$lo)), errors = tabulate(b[ie23[i]], length(f$lo))) %>%
      mutate(obs_rel = (errors / pmax(cases, 1)) / mean(ie23))
  }
  for (b in c(0.05, 0.10)) {
    fs <- top_k(sc, ben23, st23, b); fm <- top_k(simple23, rep(0, length(simple23)), st23, b); f0 <- top_k(sc0, ben23, st23, b)
    gains[[length(gains) + 1]] <- data.frame(seed = s, budget = b, score_per_case = mean(ed23[fs]),
      simple_per_case = mean(ed23[fm]), gain = mean(ed23[fs]) / mean(ed23[fm]) - 1,
      score_precision = mean(ie23[fs]), simple_precision = mean(ie23[fm]),
      score_per_case_uncapped = mean(ed23[f0]), gain_uncapped = mean(ed23[f0]) / mean(ed23[fm]) - 1,
      score_precision_uncapped = mean(ie23[f0]))
  }
}
bins <- bind_rows(bins); gains <- bind_rows(gains)
write.csv(bins, file.path(OUT, "check_fy2023_bins.csv"), row.names = FALSE)
write.csv(gains, file.path(OUT, "check_fy2023_gain.csv"), row.names = FALSE)
print(bins %>% group_by(hh, votes_from) %>% summarise(table_rel = first(table_rel), obs_rel = mean(obs_rel),
                                                       cases = mean(cases), .groups = "drop"), digits = 3)
print(gains, digits = 3)
stamp("national FY2022 error rate %.4f; FY2023 %.4f", national, mean(ie23))
