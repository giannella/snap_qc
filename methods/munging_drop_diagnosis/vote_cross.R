# Step 1 of plan.md: vote rule sets crossed with frames (2026-10-31).
#   SR_WINDOW=fy2022 SR_ROOT=C:/Users/ericg/snap_qc_v27 Rscript methods/munging_drop_diagnosis/vote_cross.R
# Same procedure as methods/vote_scoring/deploy_table.R's FY2023 check, with the
# rule set and the data taken from different frames. The rule set is the 300
# rules allotted from a frame's own FY2017-19 pool (grouped on that frame's
# training rows); the vote curve and the size model are fitted on the data
# frame's FY2022 and scored on its FY2023. Writes only to this folder.
stopifnot(Sys.getenv("SR_WINDOW") == "fy2022")
source("methods/v270_seed_replicates/common.R")
source("methods/vote_scoring/vote_groups.R"); source("methods/vote_scoring/vote_fit.R")
OUT <- "methods/munging_drop_diagnosis"
FRS <- c("f1027", "f1030"); SEEDS <- 117:119
representable <- function(pool) pool[!grepl("count_divisible_by_100", pool$rule, fixed = TRUE), , drop = FALSE]

D <- lapply(setNames(nm = FRS), function(fr) {
  F <- load_frame(fr)
  tr <- which(F$is_tr); te <- which(F$yr == TEST_YEAR)
  reg <- readRDS(FRAMES[[fr]]); a23 <- reg[as.character(reg$fiscal_year) == "2023", , drop = FALSE]; rm(reg)
  a23 <- prep_features(a23, VOCAB19)$data
  ie23 <- !is.na(a23$over_threshold) & a23$over_threshold != 0
  list(F = F, tr = tr, te = te, a23 = a23, ie23 = ie23,
       ed23 = ifelse(ie23, abs(ifelse(is.na(a23$total_error_amount), 0, a23$total_error_amount)), 0),
       hh23 = hh_group_of(a23$cert_HH_size_FS_n))
})

# the allotted rule sets, each built on its own frame exactly as deploy_table.R does
AL <- lapply(setNames(nm = FRS), function(fr) {
  d <- D[[fr]]; F <- d$F; tr <- d$tr
  lapply(setNames(nm = SEEDS), function(s) {
    rules <- top_rules(representable(read_pool(pool_file(fr, s), TRAIN_YEARS)))
    fl_tr <- flags_for_rules(rules, F$adf[tr, , drop = FALSE], strata_of(F$hh[tr]))
    stopifnot(all(lengths(fl_tr) == rules$n))
    allot(build_groups(rules, fl_tr), 300L, "rep")
  })
})

top_k <- function(score, tieb, st_, budget) unlist(lapply(split(seq_along(score), st_), function(ix) {
  k <- max(1L, round(budget * length(ix))); ix[order(-score[ix], -tieb[ix], runif(length(ix)))][seq_len(k)]
}), use.names = FALSE)

res <- list()
for (rf in FRS) for (df in FRS) {
  d <- D[[df]]; F <- d$F; te <- d$te; NT <- length(te)
  ie <- F$ie[te]; ed <- F$ed[te]; hh <- F$hh[te]
  ben <- num(F$adf$rawben[te]); hsz <- num(F$adf$cert_HH_size_FS_n[te])
  ie_tr <- F$ie[d$tr]; hh_tr <- F$hh[d$tr]
  base_mine <- vapply(HH_LEVELS, function(h) mean(ie_tr[hh_tr == h]), numeric(1))
  a23 <- d$a23; ie23 <- d$ie23; ed23 <- d$ed23; hh23 <- d$hh23
  st23 <- as.character(a23$state); ben23 <- num(a23$rawben); hsz23 <- num(a23$cert_HH_size_FS_n)
  V <- lapply(AL[[rf]], function(al) list(
    v22 = votes_of(al, flags_for_rules(al, F$adf[te, , drop = FALSE], strata_of(hh)), NT)$vote,
    v23 = votes_of(al, flags_for_rules(al, a23, strata_of(hh23)), nrow(a23))$vote))
  x <- unlist(lapply(V, `[[`, "v22")); h <- rep(hh, length(SEEDS))
  MIN_BIN_ERRS <- 150L; fc <- fit_curve(x, rep(ie, length(SEEDS)), h); MIN_BIN_ERRS <- 50L
  national <- mean(ie)
  sz <- fit_size(SIZE_FORMS$benefit_size_vote,
                 size_data(rep(ed, length(SEEDS)), h, rep(ben, length(SEEDS)), rep(hsz, length(SEEDS)), x),
                 seq_along(x))
  simple23 <- ben23 * base_mine[hh23]
  set.seed(20261031L)
  for (s in names(V)) {
    v <- V[[s]]$v23
    sc <- pred_curve(fc, v, hh23) / national *
      predict(sz, size_data(ed23, hh23, ben23, hsz23, v), type = "response")
    for (b in c(0.05, 0.10)) {
      fs <- top_k(sc, ben23, st23, b); fm <- top_k(simple23, rep(0, length(simple23)), st23, b)
      res[[length(res) + 1]] <- data.frame(rules = rf, data = df, seed = s, budget = b,
        voted_share = mean(v > 0), score_per_case = mean(ed23[fs]), simple_per_case = mean(ed23[fm]),
        gain = mean(ed23[fs]) / mean(ed23[fm]) - 1,
        score_precision = mean(ie23[fs]), simple_precision = mean(ie23[fm]))
    }
  }
  stamp("rules %s x data %s done", rf, df)
}
res <- bind_rows(res)
write.csv(res, file.path(OUT, "vote_cross.csv"), row.names = FALSE)
print(res, digits = 3)
print(res %>% group_by(rules, data, budget) %>%
        summarise(across(c(voted_share, score_per_case, simple_per_case, gain, score_precision), mean),
                  .groups = "drop"), digits = 3)
