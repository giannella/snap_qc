# Confirmation window of the case-scoring-by-rule-votes study
# (methods/vote_scoring/design_note.md, "Arms at confirmation" and "Readout and
# bars"; specification fixed in "Exploration result", 2026-10-28).
#   SR_WINDOW=fy2022 Rscript methods/vote_scoring/confirm_window.R   (FY2017-19 rules -> FY2022; decides)
#   SR_WINDOW=fy2024 Rscript methods/vote_scoring/confirm_window.R   (FY2022-23 rules -> FY2024; must agree)
# For each seed (117-119): the window's own pool is grouped on its mining years,
# the representative vote over 300 rules is counted on the test year, and the
# FROZEN FY2019 curve and error-size model of the same seed score the cases.
# Nothing is fitted on the test year except the secondary tests and the
# descriptive references' absence of fitting (both references fit on the
# mining years). VS_SMOKE=1 runs every step at token sizes into a smoke folder.
# Writes methods/vote_scoring/confirm/<window>/*.csv; confirm_readout.R reads them.
WINDOW_ARG <- Sys.getenv("SR_WINDOW"); stopifnot(WINDOW_ARG %in% c("fy2022", "fy2024"))
source("methods/v270_seed_replicates/common.R")
source("methods/vote_scoring/vote_groups.R"); source("methods/vote_scoring/vote_fit.R")
suppressPackageStartupMessages({ library(glmnet); library(xgboost); library(Matrix) })
VS_SMOKE <- identical(Sys.getenv("VS_SMOKE"), "1")
B <- if (VS_SMOKE) 10L else 2000L; NPERM <- if (VS_SMOKE) 4L else 200L
BUDGETS_ALL <- c(0.01, 0.02, 0.05, 0.10); HARM_GAP <- 0.05
OUT <- file.path("methods/vote_scoring/confirm", WINDOW, if (VS_SMOKE) "smoke" else "")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
POOLS <- list(
  fy2022 = c(`117` = "methods/v270_cpi_benchmark/fy2022_cpi/cache/bench_national_117.rds",
             `118` = "methods/v270_seed_replicates/fy2022/pools/admitted_final_seed118.rds",
             `119` = "methods/v270_seed_replicates/fy2022/pools/admitted_final_seed119.rds"),
  fy2024 = c(`117` = "methods/v270_cpi_benchmark/fy2024_cpi/cache/bench_national_117.rds",
             `118` = "methods/v270_seed_replicates/pools/admitted_final_seed118.rds",
             `119` = "methods/v270_seed_replicates/pools/admitted_final_seed119.rds"))[[WINDOW]]
set.seed(if (WINDOW == "fy2022") 20261029L else 20261030L)

## ---- data ---------------------------------------------------------------------
F <- load_frame("final")
tr <- which(F$is_tr); te <- which(F$yr == TEST_YEAR); NT <- length(te)
S_tr <- strata_of(F$hh[tr]); S_te <- strata_of(F$hh[te])
ie <- F$ie[te]; ed <- F$ed[te]; hh <- F$hh[te]; st <- F$st[te]; mm <- F$mm[te]
ben <- num(F$adf$rawben[te]); hsz <- num(F$adf$cert_HH_size_FS_n[te])
ie_tr <- F$ie[tr]; ed_tr <- F$ed[tr]; hh_tr <- F$hh[tr]
base_mine <- vapply(HH_LEVELS, function(h) mean(ie_tr[hh_tr == h]), numeric(1))
simple <- ben * base_mine[hh]
zero <- numeric(NT); S_idx <- split(seq_len(NT), st)
stamp("%s: rules mined on FY%s (%d cases), scored on FY%s: %d cases, %d errors, %d states%s", WINDOW,
      paste(TRAIN_YEARS, collapse = "+"), length(tr), TEST_YEAR, NT, sum(ie), length(S_idx), if (VS_SMOKE) " [SMOKE]" else "")

# FY2019, for the bootstrap refits of the frozen curve and error-size model
reg <- readRDS(FRAMES[["final"]]); a19 <- reg[as.character(reg$fiscal_year) == "2019", , drop = FALSE]; rm(reg)
a19 <- prep_features(a19, VOCAB19)$data
ie19 <- !is.na(a19$over_threshold) & a19$over_threshold != 0
ed19 <- ifelse(ie19, abs(ifelse(is.na(a19$total_error_amount), 0, a19$total_error_amount)), 0)
hh19 <- hh_group_of(a19$cert_HH_size_FS_n); st19 <- as.character(a19$state)
ben19 <- num(a19$rawben); hsz19 <- num(a19$cert_HH_size_FS_n)
stopifnot(nrow(a19) == 38155L, sum(ie19) == 3872L)
S19 <- split(seq_len(nrow(a19)), st19)
G19 <- readRDS("methods/vote_scoring/fy2019/explore_groups.rds")

## ---- ranking helpers --------------------------------------------------------------
top_cases <- function(score, tieb, idx = seq_len(NT), budget = 0.05) {
  unlist(lapply(split(idx, st[idx]), function(ix) {
    k <- max(1L, round(budget * length(ix)))
    ix[order(-score[ix], -tieb[ix], runif(length(ix)))][seq_len(k)]
  }), use.names = FALSE)
}
per_case <- function(f) sum(ed[f]) / length(f)
state_recall <- function(score, tieb, budget) vapply(S_idx, function(ix) {
  k <- max(1L, round(budget * length(ix))); f <- ix[order(-score[ix], -tieb[ix], runif(length(ix)))][seq_len(k)]
  tot <- sum(ed[ix]); if (tot > 0) sum(ed[f]) / tot else NA_real_ }, numeric(1))
ca_z <- function(bin, y) {        # Cochran-Armitage trend z, scores = bin index
  n <- tabulate(bin); e <- tabulate(bin[y], length(n)); x <- seq_along(n); N <- sum(n); p <- sum(e) / N
  T <- sum(x * (e - n * p)); V <- p * (1 - p) * (sum(n * x^2) - sum(n * x)^2 / N)
  if (V > 0) T / sqrt(V) else NA_real_
}

## ---- one seed -------------------------------------------------------------------
run_seed <- function(s) {
  fz <- readRDS(sprintf("methods/vote_scoring/fy2019/frozen_seed%s.rds", s))
  stopifnot(fz$def == "rep", fz$R == 300L, fz$p_form == "vote", fz$size_model == "benefit_size_vote",
            identical(fz$train_years, c("2017", "2018")), fz$min_bin_errs == MIN_BIN_ERRS)
  # FY2019 votes from the same seed's FY2017-18 groups; refit on all of FY2019 must reproduce the frozen fits
  g19 <- G19[[s]]; al19 <- allot(g19, 300L, "rep")
  v19 <- votes_of(al19, flags_for_rules(al19, a19, strata_of(hh19)), nrow(a19))$vote
  d19 <- size_data(ed19, hh19, ben19, hsz19, v19)
  fc_chk <- fit_curve(v19, ie19, hh19); fs_chk <- fit_size(SIZE_FORMS$benefit_size_vote, d19, seq_len(nrow(a19)))
  stopifnot(isTRUE(all.equal(lapply(fc_chk, `[[`, "rate"), lapply(fz$curve, `[[`, "rate"))),
            isTRUE(all.equal(coef(fs_chk), coef(fz$size_fit))))

  # the window's pool, grouped on its own mining years
  pool <- read_pool(POOLS[[s]], TRAIN_YEARS)
  rules <- top_rules(pool)
  fl_tr <- flags_for_rules(rules, F$adf[tr, , drop = FALSE], S_tr)
  stopifnot(all(lengths(fl_tr) == rules$n), all(vapply(fl_tr, function(ix) sum(ie_tr[ix]), numeric(1)) == rules$k))
  g <- build_groups(rules, fl_tr); al <- allot(g, 300L, "rep")
  ral <- match(paste(al$hh, al$rule), paste(rules$hh, rules$rule))
  fl_te <- flags_for_rules(al, F$adf[te, , drop = FALSE], S_te)
  VV <- votes_of(al, fl_te, NT); vote <- VV$vote; maxlcb <- VV$maxlcb
  dte <- size_data(ed, hh, ben, hsz, vote)
  p_full <- pred_curve(fz$curve, vote, hh); sz_full <- predict(fz$size_fit, dte, type = "response")
  full <- p_full * sz_full
  stamp("seed %s: %d groups from %d rules; 300 representative rules flag %.1f%% of test cases (max vote %d)",
        s, sum(g$lead), nrow(rules), 100 * mean(vote > 0), max(vote))

  # descriptive references, fitted on the mining years only
  Xg_tr <- sparseMatrix(i = unlist(fl_tr[ral]), j = rep(seq_along(ral), lengths(fl_tr[ral])), x = 1, dims = c(length(tr), length(ral)))
  Xg_te <- sparseMatrix(i = unlist(fl_te), j = rep(seq_along(fl_te), lengths(fl_te)), x = 1, dims = c(NT, length(ral)))
  hhm <- function(h) Matrix(sapply(HH_LEVELS[-1], function(l) as.numeric(h == l)), sparse = TRUE)
  cvr <- cv.glmnet(cbind(Xg_tr, hhm(hh_tr)), as.numeric(ie_tr), family = "binomial", alpha = 0, nfolds = 5,
                   penalty.factor = c(rep(1, length(ral)), 0, 0))
  ridge <- as.numeric(predict(cvr, cbind(Xg_te, hhm(hh)), s = "lambda.min", type = "response")) * sz_full
  xgbs <- numeric(NT)
  Xtr_all <- data.matrix(F$adf[tr, VOCAB19]); Xte_all <- data.matrix(F$adf[te, VOCAB19])
  PC <- list(objective = "binary:logistic", eta = 0.05, max_depth = 4, min_child_weight = 20, subsample = 0.8, colsample_bytree = 0.8, nthread = 8)
  PR <- list(objective = "reg:squarederror", eta = 0.05, max_depth = 4, min_child_weight = 20, subsample = 0.8, colsample_bytree = 0.8, nthread = 8)
  for (l in HH_LEVELS) {
    itr <- which(hh_tr == l); ite <- which(hh == l); pos <- itr[ie_tr[itr]]
    set.seed(as.integer(s))
    clf <- xgb.train(params = c(PC, seed = as.integer(s)), data = xgb.DMatrix(Xtr_all[itr, ], label = as.numeric(ie_tr[itr])),
                     nrounds = if (VS_SMOKE) 20 else 400, verbose = 0)
    rgr <- xgb.train(params = c(PR, seed = as.integer(s)), data = xgb.DMatrix(Xtr_all[pos, ], label = log1p(ed_tr[pos])),
                     nrounds = if (VS_SMOKE) 20 else 300, verbose = 0)
    sm <- mean(exp(log1p(ed_tr[pos]) - predict(rgr, xgb.DMatrix(Xtr_all[pos, ]))))
    xgbs[ite] <- predict(clf, xgb.DMatrix(Xte_all[ite, , drop = FALSE])) * expm1(predict(rgr, xgb.DMatrix(Xte_all[ite, , drop = FALSE]))) * sm
  }

  ARMS <- list(random = list(runif(NT), zero), simple = list(simple, zero), vote = list(vote, ben),
               lcb = list(maxlcb, ben), full = list(full, ben), ref_ridge = list(ridge, ben),
               ref_xgb = list(xgbs, ben), perfect = list(ed, zero))
  arms <- bind_rows(lapply(names(ARMS), function(a) bind_rows(lapply(BUDGETS_ALL, function(b) {
    f <- top_cases(ARMS[[a]][[1]], ARMS[[a]][[2]], budget = b)
    data.frame(seed = s, arm = a, reading = "all rows", budget = b, reviewed = length(f), errors = sum(ie[f]),
               precision = mean(ie[f]), dollars = sum(ed[f]), per_case = per_case(f), dollar_recall = sum(ed[f]) / sum(ed),
               mm_dollar_share = sum(ed[f][mm[f]]) / sum(ed[f]))
  }))))
  keep <- which(!mm)
  arms <- bind_rows(arms, bind_rows(lapply(c("simple", "full"), function(a) {
    f <- top_cases(ARMS[[a]][[1]], ARMS[[a]][[2]], keep, 0.05)
    data.frame(seed = s, arm = a, reading = "artifact rows dropped", budget = 0.05, reviewed = length(f), errors = sum(ie[f]),
               precision = mean(ie[f]), dollars = sum(ed[f]), per_case = per_case(f), dollar_recall = sum(ed[f]) / sum(ed[keep]),
               mm_dollar_share = 0)
  })))
  write.csv(arms, file.path(OUT, sprintf("arms_seed%s.csv", s)), row.names = FALSE)

  # states: dollar recall by state at 5% and 10%, score vs simple ordering
  states <- bind_rows(lapply(c(0.05, 0.10), function(b) data.frame(seed = s, state = names(S_idx), cases = lengths(S_idx), budget = b,
    recall_full = state_recall(full, ben, b), recall_simple = state_recall(simple, zero, b))))
  write.csv(states, file.path(OUT, sprintf("states_seed%s.csv", s)), row.names = FALSE)
  harm_obs <- vapply(c(0.05, 0.10), function(b) with(states[states$budget == b, ], sum(recall_full < recall_simple - HARM_GAP, na.rm = TRUE)), numeric(1))

  # harm null: the vote permuted within size group x issued-benefit decile, 200 times
  dec <- dplyr::ntile(ben, 10); cells <- split(seq_len(NT), paste(hh, dec))
  rs <- list(`0.05` = state_recall(simple, zero, 0.05), `0.1` = state_recall(simple, zero, 0.10))
  null <- t(replicate(NPERM, {
    vp <- vote; for (ix in cells) vp[ix] <- vote[ix][sample.int(length(ix))]
    dp <- dte; dp$vote <- vp
    sc <- pred_curve(fz$curve, vp, hh) * predict(fz$size_fit, dp, type = "response")
    c(sum(state_recall(sc, ben, 0.05) < rs$`0.05` - HARM_GAP, na.rm = TRUE), sum(state_recall(sc, ben, 0.10) < rs$`0.1` - HARM_GAP, na.rm = TRUE))
  }))
  harm <- data.frame(seed = s, budget = c(0.05, 0.10), harm_observed = harm_obs,
                     null_median = apply(null, 2, median), null_p95 = apply(null, 2, quantile, 0.95))
  write.csv(harm, file.path(OUT, sprintf("harm_seed%s.csv", s)), row.names = FALSE)
  write.csv(data.frame(seed = s, perm = seq_len(NPERM), harm_05 = null[, 1], harm_10 = null[, 2]),
            file.path(OUT, sprintf("harm_null_seed%s.csv", s)), row.names = FALSE)
  stamp("seed %s: arms, states and harm null done", s)

  # bootstrap: cases resampled within state in the test year AND FY2019; curve and
  # error-size model refit on each FY2019 draw; rule pools fixed
  boot <- t(replicate(B, {
    d19i <- unlist(lapply(S19, function(ix) ix[sample.int(length(ix), replace = TRUE)]), use.names = FALSE)
    fc <- fit_curve(v19[d19i], ie19[d19i], hh19[d19i]); fs <- fit_size(SIZE_FORMS$benefit_size_vote, d19, d19i)
    sc <- pred_curve(fc, vote, hh) * predict(fs, dte, type = "response")
    di <- unlist(lapply(S_idx, function(ix) ix[sample.int(length(ix), replace = TRUE)]), use.names = FALSE)
    dk <- di[!mm[di]]
    c(per_case(top_cases(sc, ben, di)) / per_case(top_cases(simple, zero, di)) - 1,
      per_case(top_cases(sc, ben, dk)) / per_case(top_cases(simple, zero, dk)) - 1)
  }))
  write.csv(data.frame(seed = s, draw = seq_len(B), gain = boot[, 1], gain_no_mm = boot[, 2]),
            file.path(OUT, sprintf("boot_seed%s.csv", s)), row.names = FALSE)
  stamp("seed %s: %d bootstrap draws done", s, B)

  # secondaries: trend in error rate across the frozen curve's vote bins (per size
  # group); the vote's coefficient in a gamma model of test-year error size within
  # exact household size with state fixed effects
  sec <- bind_rows(lapply(HH_LEVELS, function(l) { i <- hh == l
    data.frame(seed = s, test = "trend", hh = l, z = ca_z(bin_of(fz$curve[[l]], vote[i]), ie[i])) }))
  e <- ie & ed > 0
  gm <- glm(ed ~ vote + factor(pmin(hsz, 6)) + factor(st), family = Gamma(link = "log"), data = data.frame(ed, vote, hsz, st)[e, ],
            control = glm.control(maxit = 100))
  cf <- summary(gm)$coefficients["vote", ]
  sec <- bind_rows(sec, data.frame(seed = s, test = "size_vote", hh = "all", z = cf[["t value"]], coef = cf[["Estimate"]]))
  write.csv(sec, file.path(OUT, sprintf("secondary_seed%s.csv", s)), row.names = FALSE)

  # descriptive: frozen curve vs observed test-year rates by bin; calibration of
  # expected dollars by score decile; between-state spread of the rate per bin
  curve <- bind_rows(lapply(HH_LEVELS, function(l) { i <- which(hh == l); b <- bin_of(fz$curve[[l]], vote[i])
    data.frame(seed = s, hh = l, bin = seq_along(fz$curve[[l]]$lo), from = fz$curve[[l]]$lo, fy2019_rate = fz$curve[[l]]$rate,
               test_cases = tabulate(b, length(fz$curve[[l]]$lo)), test_errors = tabulate(b[ie[i]], length(fz$curve[[l]]$lo))) %>%
      mutate(test_rate = test_errors / test_cases) }))
  write.csv(curve, file.path(OUT, sprintf("curve_seed%s.csv", s)), row.names = FALSE)
  cal <- data.frame(dec = dplyr::ntile(full, 10), full, ed) %>% group_by(dec) %>%
    summarise(cases = n(), expected_per_case = mean(full), observed_per_case = mean(ed)) %>% mutate(seed = s)
  write.csv(cal, file.path(OUT, sprintf("calibration_seed%s.csv", s)), row.names = FALSE)
  sv <- bind_rows(lapply(HH_LEVELS, function(l) { i <- which(hh == l); b <- bin_of(fz$curve[[l]], vote[i])
    bind_rows(lapply(sort(unique(b)), function(k) { j <- i[b == k]; t <- table(st[j]); r <- tapply(ie[j], st[j], mean)
      ok <- t >= 10; if (sum(ok) < 3) return(NULL); p <- mean(ie[j])
      data.frame(seed = s, hh = l, bin = k, states = sum(ok), var_between = var(r[ok]), var_binomial = mean(p * (1 - p) / t[ok])) })) }))
  write.csv(sv, file.path(OUT, sprintf("statevar_seed%s.csv", s)), row.names = FALSE)
  invisible(NULL)
}
for (s in names(POOLS)) run_seed(s)
stamp("%s confirmation done -> %s", WINDOW, OUT)
