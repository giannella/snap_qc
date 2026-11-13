# Vote-table refit, step 3 (2026-11-12; design note ../design_note_table_refit_2026-11-12.md): the fits, the checks
# in the note's section 5, scoring, the paired bootstrap and the readout against the note's bars (section 7).
# Revised after the fresh statistician review (2026-11-12): votes count each rule once (lv_core10_nv), window B's
# baseline is a national-only table refit on FY2023, the harmed-tail limit applies at both budgets, the median
# joins the mean at the rising budget, results print by test year and by seed, and the shipped pair is scored
# against the shipping candidate on the issued benefit.
#   window A: curve and size model fit on FY2022 combined votes (national + core list, each rule once), tested on
#             FY2023 and FY2024; baseline scorer_W3.rds (the shipped curve; §52's size model, recreated benefit)
#   window B: fit on FY2023 combined votes, tested on FY2022 and FY2024; baseline fit on FY2023 national votes
# Fitting follows wb_ref_f1109.R's scorer_of: three seeds stacked, bins pooled from the top to 150 errors, gamma size
# model ed ~ lben + hsz + vote on error cases with votes capped at 7, relative rate over the fit year's error rate.
# The bootstrap resamples kept cases within state and test year; it is conditional on the fitted tables (no fit-year
# or mining variance).
#   [SMOKE=1] [NBOOT=2000] "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" methods/included_rules_vote/refit/refit_fit_score.R
# Writes methods/included_rules_vote/refit/: refit_by_state.csv, refit_summary.csv, refit_by_year_seed.csv,
# scorer_A.rds, scorer_B.rds, scorer_A_issued.rds (window A on the issued benefit: the shipping candidate).
ROOT <- "C:/Users/ericg/snap_qc_v27"; setwd(ROOT)
Sys.setenv(SR_ROOT = ROOT, SR_WINDOW = "fy2022")
suppressMessages({source("methods/v270_seed_replicates/common.R")
  source("methods/vote_scoring/vote_groups.R"); source("methods/vote_scoring/vote_fit.R")})
SMOKE <- identical(Sys.getenv("SMOKE"), "1")
VOTE_CAP <- 7L; BUD <- c(0.05, 0.10); NB <- as.integer(Sys.getenv("NBOOT", "2000"))
SEEDS <- if (SMOKE) 117L else 117:119
D <- "methods/included_rules_vote/refit"; say <- function(...) cat(sprintf(...), "\n")
sfx <- if (SMOKE) "_smoke" else ""
s0 <- readRDS("methods/case_predictions/s46/out_f1109_wb/wb/scorer_W3.rds")
stopifnot(identical(s0$rule_years, c("2017", "2018", "2019")), s0$fit_year == "2022", s0$min_bin == 150L,
          identical(s0$seeds, c("117", "118", "119")), s0$vote_cap == VOTE_CAP)

## ---- load and align (check 4)
X <- list(); base <- NULL
for (sd in SEEDS) {
  v <- readRDS(file.path(D, sprintf("votes_seed%d.rds", sd)))
  L <- readRDS(file.path(D, sprintf("lists_seed%d%s.rds", sd, sfx))); l <- L$cases
  stopifnot(nrow(v) == nrow(l), all(v$year == l$year), all(v$st == l$st), all(v$ie == l$ie), all(abs(v$ed - l$ed) < 1e-6),
            all(v$keep == l$kept), "lv_core10_nv" %in% names(l))
  if (is.null(base)) base <- v[, c("year", "st", "ie", "ed", "hh", "ben", "ben_issued", "hsz", "keep")] else
    stopifnot(identical(base$st, v$st), identical(base$ie, v$ie), identical(base$keep, v$keep))
  X[[as.character(sd)]] <- list(vote = v$vote, lv = l$lv_core10_nv, lv_dbl = l$lv_core10, rulelist = L$rulelist, walk = L$walk)
}
say("check 4: %d seed(s) hold the same %d cases in the same order; kept flags agree", length(SEEDS), nrow(base))
wk <- do.call(rbind, lapply(X, `[[`, "walk")); w10 <- wk[wk$budget == 0.1, ]
say("check 3: 10%% lists walked on FY2017-19: core rules median %g (of them also vote rules: median %g, %.0f%%); core shortfall median %g, max %g; total shortfall median %g, max %g",
    median(w10$n_core), median(w10$n_core_vote), 100 * sum(w10$n_core_vote) / sum(w10$n_core),
    median(w10$gap_core), max(w10$gap_core), median(w10$gap_total), max(w10$gap_total))
for (sd in names(X)) for (y in c("2022", "2023", "2024")) { i <- base$year == y
  say("  seed %s FY%s: cases with a national vote %.1f%%, with a list vote not already a vote rule %.1f%%, with either %.1f%%; list votes added per case %.3f (double-counted %.3f)",
      sd, y, 100 * mean(X[[sd]]$vote[i] > 0), 100 * mean(X[[sd]]$lv[i] > 0), 100 * mean(X[[sd]]$vote[i] + X[[sd]]$lv[i] > 0),
      mean(X[[sd]]$lv[i]), mean(X[[sd]]$lv_dbl[i])) }

## ---- fits
fit_scorer <- function(year, add_list = TRUE, ben = "ben") {
  i <- base$year == year; r <- base[i, ]
  V <- lapply(X, function(x) x$vote[i] + if (add_list) x$lv[i] else 0L)
  k <- length(V); x <- unlist(V); y <- rep(r$ie, k); h <- rep(r$hh, k)
  MIN_BIN_ERRS <<- 150L; fc <- fit_curve(x, y, h); MIN_BIN_ERRS <<- 50L
  d <- size_data(rep(r$ed, k), h, rep(r[[ben]], k), rep(r$hsz, k), pmin(x, VOTE_CAP))
  list(curve = fc, size = fit_size(SIZE_FORMS$benefit_size_vote, d, seq_len(nrow(d))), national = mean(r$ie),
       fit_year = year, add_list = add_list, ben = ben, seeds = names(X))
}
rel_of <- function(s) unlist(lapply(HH_LEVELS, function(l) s$curve[[l]]$rate / s$national))
a0i <- fit_scorer("2022", add_list = FALSE, ben = "ben_issued")   # the shipped pair, refit (check 1 below)
if (!SMOKE) {   # check 1: national votes alone reproduce the shipped curve and both size models
  shipped <- read.csv("methods/vote_scoring/deploy/vote_table.csv"); sc_ship <- read.csv("methods/vote_scoring/deploy/size_coefs.csv")
  a0 <- fit_scorer("2022", add_list = FALSE)
  stopifnot(length(rel_of(a0)) == nrow(shipped), isTRUE(all.equal(unname(rel_of(a0)), shipped$rel_rate, tolerance = 1e-9)),
            isTRUE(all.equal(unname(rel_of(a0i)), shipped$rel_rate, tolerance = 1e-9)),
            isTRUE(all.equal(unname(coef(a0$size)), unname(coef(s0$size)), tolerance = 1e-6)),
            isTRUE(all.equal(unname(coef(a0i$size)), sc_ship$estimate[match(names(coef(a0i$size)), sc_ship$term)], tolerance = 1e-6)))
  say("check 1: national votes alone reproduce vote_table.csv, scorer_W3's size model and size_coefs.csv")
}
sA <- fit_scorer("2022"); sB <- fit_scorer("2023"); bB <- fit_scorer("2023", add_list = FALSE); sAi <- fit_scorer("2022", ben = "ben_issued")
if (!SMOKE) { saveRDS(sA, file.path(D, "scorer_A.rds")); saveRDS(sB, file.path(D, "scorer_B.rds")); saveRDS(sAi, file.path(D, "scorer_A_issued.rds")) }
show_curve <- function(s, lab) for (l in HH_LEVELS) say("  %s %-3s votes from %s: rel %s", lab, l,
  paste(s$curve[[l]]$lo, collapse = "/"), paste(sprintf("%.2f", s$curve[[l]]$rate / s$national), collapse = "/"))
show_curve(s0, "shipped     "); show_curve(sA, "refit A     "); show_curve(bB, "base B (natl)"); show_curve(sB, "refit B     ")
say("size coefficients (lben, vote): shipped-eval %.3f, %.4f | A %.3f, %.4f | base B %.3f, %.4f | B %.3f, %.4f | shipped issued %.3f, %.4f | A issued %.3f, %.4f",
    coef(s0$size)["lben"], coef(s0$size)["vote"], coef(sA$size)["lben"], coef(sA$size)["vote"], coef(bB$size)["lben"], coef(bB$size)["vote"],
    coef(sB$size)["lben"], coef(sB$size)["vote"], coef(a0i$size)["lben"], coef(a0i$size)["vote"], coef(sAi$size)["lben"], coef(sAi$size)["vote"])

## ---- scoring: per test year, seed and arm
score_with <- function(s, i, vote, ben = "ben") { r <- base[i, ]; p <- pred_curve(s$curve, vote, r$hh) / s$national
  as.numeric(p * predict(s$size, size_data(r$ed, r$hh, r[[ben]], r$hsz, pmin(vote, VOTE_CAP)), type = "response")) }
WIN <- list(A = list(arm = "A", base = "baseA", test = c("2023", "2024"), bars = TRUE),
            B = list(arm = "B", base = "baseB", test = c("2022", "2024"), bars = TRUE),
            ship = list(arm = "candA", base = "shipA", test = c("2023", "2024"), bars = FALSE))
G <- list()
for (y in c("2022", "2023", "2024")) {
  i <- which(base$year == y); SC <- list()
  for (sd in names(X)) {
    cv <- X[[sd]]$vote[i] + X[[sd]]$lv[i]
    SC[[sd]] <- list(baseA = score_with(s0, i, cv), A = score_with(sA, i, cv), baseB = score_with(bB, i, cv), B = score_with(sB, i, cv),
                     natl = score_with(s0, i, X[[sd]]$vote[i]), shipA = score_with(a0i, i, cv, "ben_issued"), candA = score_with(sAi, i, cv, "ben_issued"))
  }
  for (ix in split(seq_along(i), base$st[i])) {
    k <- ix[base$keep[i][ix]]
    G[[length(G) + 1L]] <- list(year = y, st = base$st[i][ix[1]], n = length(ix), ie = base$ie[i][k], ed = base$ed[i][k],
                               ord = lapply(SC, function(z) lapply(z, function(sc) order(-sc[k], k))))
  }
}
# errors and dollars caught by the top kk of one ranking, given case weights (1 = the data; bootstrap counts otherwise)
take <- function(g, o, kk, w) { cw <- w[o]; cu <- cumsum(cw); t <- pmin(cw, pmax(0, kk - (cu - cw)))
  c(e = sum(t * g$ie[o]), d = sum(t * g$ed[o])) }
eval_groups <- function(years, arm, b, W = NULL, seeds = names(X)) {   # per group: seed-averaged e and d, plus totals
  gs <- Filter(function(g) g$year %in% years, G)
  t(vapply(gs, function(g) { w <- if (is.null(W)) rep(1, length(g$ie)) else W[[g$year]][[g$st]]
    kk <- max(1L, round(b * g$n))
    ed <- rowMeans(vapply(seeds, function(sd) take(g, g$ord[[sd]][[arm]], kk, w), numeric(2)))
    c(ed, flags = kk, e_all = sum(w * g$ie), d_all = sum(w * g$ed), kept = sum(w)) }, numeric(6)))
}
tr <- function(m, b) c(errors = (sum(m[, "e"]) / sum(m[, "flags"])) / (sum(m[, "e_all"]) / sum(m[, "kept"])),
                      dollars = sum(m[, "d"]) / sum(m[, "d_all"]) / b)

## ---- readout
SUM <- list(); BYST <- list(); BYYS <- list()
for (win in names(WIN)) for (b in BUD) {
  wn <- WIN[[win]]; gs <- Filter(function(g) g$year %in% wn$test, G)
  mb <- eval_groups(wn$test, wn$base, b); mr <- eval_groups(wn$test, wn$arm, b); mn <- eval_groups(wn$test, "natl", b)
  st_ <- vapply(gs, `[[`, "", "st"); yr_ <- vapply(gs, `[[`, "", "year")
  BYST[[length(BYST) + 1L]] <- data.frame(window = win, budget = b, year = yr_, state = st_, flags = mb[, "flags"],
    kept = mb[, "kept"], errors_all = mb[, "e_all"], dollars_all = mb[, "d_all"], base_e = mb[, "e"], base_d = mb[, "d"],
    refit_e = mr[, "e"], refit_d = mr[, "d"], natl_e = mn[, "e"], natl_d = mn[, "d"])
  dd <- tapply(mr[, "d"] - mb[, "d"], st_, sum) / tapply(mb[, "d_all"], st_, sum)   # the two test years pooled
  rl <- do.call(rbind, lapply(X, `[[`, "rulelist")); rl <- rl[rl$budget == b & rl$year %in% wn$test, ]
  rl_d <- sum(rl$dollars_caught) / sum(rl$dollars_all) / b
  rl_e <- (sum(rl$errors_caught) / sum(rl$flags)) / (sum(rl$errors_all) / sum(rl$kept))
  f2 <- function(m, s) sprintf("%.1f/%.0f", sum(m[st_ == s, "e"]), sum(m[st_ == s, "d"]))
  SUM[[length(SUM) + 1L]] <- data.frame(window = win, budget = b,
    base_e = tr(mb, b)[1], base_d = tr(mb, b)[2], refit_e = tr(mr, b)[1], refit_d = tr(mr, b)[2],
    natl_e = tr(mn, b)[1], natl_d = tr(mn, b)[2], rules_e = rl_e, rules_d = rl_d,
    mean_drec = mean(dd), median_drec = median(dd), harmed = sum(dd < -0.05), up_d = sum(dd > 0), down_d = sum(dd < 0),
    CA = paste(f2(mb, "California"), "->", f2(mr, "California")), TX = paste(f2(mb, "Texas"), "->", f2(mr, "Texas")))
  for (y in wn$test) { xb <- tr(eval_groups(y, wn$base, b), b); xr <- tr(eval_groups(y, wn$arm, b), b)
    BYYS[[length(BYYS) + 1L]] <- data.frame(window = win, budget = b, year = y, seed = "mean", base_e = xb[1], refit_e = xr[1], base_d = xb[2], refit_d = xr[2]) }
  for (sd in names(X)) { xb <- tr(eval_groups(wn$test, wn$base, b, seeds = sd), b); xr <- tr(eval_groups(wn$test, wn$arm, b, seeds = sd), b)
    BYYS[[length(BYYS) + 1L]] <- data.frame(window = win, budget = b, year = "both", seed = sd, base_e = xb[1], refit_e = xr[1], base_d = xb[2], refit_d = xr[2]) }
}
S <- do.call(rbind, SUM); rownames(S) <- NULL; YS <- do.call(rbind, BYYS); rownames(YS) <- NULL

## ---- paired bootstrap: kept cases resampled within each state and test year; one draw for every seed and arm
set.seed(20261112L)
kg <- lapply(split(G, vapply(G, `[[`, "", "year")), function(gy) setNames(lapply(gy, function(g) length(g$ie)), vapply(gy, `[[`, "", "st")))
BW <- names(WIN)[vapply(WIN, `[[`, TRUE, "bars")]
BOOT <- array(NA_real_, c(NB, length(BW), length(BUD), 2), dimnames = list(NULL, BW, BUD, c("errors", "dollars")))
for (r in seq_len(NB)) {
  Wt <- lapply(kg, function(gy) lapply(gy, function(n) tabulate(sample.int(n, n, TRUE), n)))
  for (win in BW) for (b in BUD)
    BOOT[r, win, as.character(b), ] <- tr(eval_groups(WIN[[win]]$test, WIN[[win]]$arm, b, Wt), b) - tr(eval_groups(WIN[[win]]$test, WIN[[win]]$base, b, Wt), b)
}
ci <- function(win, b, m) if (win %in% BW) quantile(BOOT[, win, as.character(b), m], c(0.025, 0.975), na.rm = TRUE) else c(NA, NA)
S$d_lo <- mapply(function(w, b) ci(w, b, "dollars")[1], S$window, S$budget); S$d_hi <- mapply(function(w, b) ci(w, b, "dollars")[2], S$window, S$budget)
S$e_lo <- mapply(function(w, b) ci(w, b, "errors")[1], S$window, S$budget); S$e_hi <- mapply(function(w, b) ci(w, b, "errors")[2], S$window, S$budget)
write.csv(do.call(rbind, BYST), file.path(D, sprintf("refit_by_state%s.csv", sfx)), row.names = FALSE)
write.csv(S, file.path(D, sprintf("refit_summary%s.csv", sfx)), row.names = FALSE)
write.csv(YS, file.path(D, sprintf("refit_by_year_seed%s.csv", sfx)), row.names = FALSE)

say("\n== Times random on kept cases, the window's two test years pooled, %d seed(s) averaged; refit minus baseline with paired 95%% interval (%d draws, conditional on the fitted tables)", length(X), NB)
say("   (ship = the shipped pair against the window A refit, both on the issued benefit; reported, not judged)")
for (k in seq_len(nrow(S))) with(S[k, ], say(
  "%-4s %2d%%: dollars %.3f -> %.3f (%+.3f, %+.3f to %+.3f) | errors %.3f -> %.3f (%+.3f, %+.3f to %+.3f) | rule lists %.3f / %.3f, national votes %.3f / %.3f (errors / dollars)\n      within-state dollar recall change: mean %+.4f, median %+.4f; harmed %d; up %d, down %d | CA %s | TX %s",
  window, round(100 * budget), base_d, refit_d, refit_d - base_d, d_lo, d_hi, base_e, refit_e, refit_e - base_e, e_lo, e_hi,
  rules_e, rules_d, natl_e, natl_d, mean_drec, median_drec, harmed, up_d, down_d, CA, TX))
say("\n== By test year (seeds averaged) and by seed (test years pooled): baseline -> refit, errors / dollars")
for (k in seq_len(nrow(YS))) with(YS[k, ], say("%-4s %2d%% year %-4s seed %-4s: %.3f -> %.3f / %.3f -> %.3f (%+.3f)", window, round(100 * budget), year, seed,
  base_e, refit_e, base_d, refit_d, refit_d - base_d))

## ---- bars (design note section 7)
for (win in BW) {
  x <- S[S$window == win, ]; dD <- x$refit_d - x$base_d; dE <- x$refit_e - x$base_e
  b1 <- all(dD >= -0.05); rise <- which(dD >= 0.05 & x$d_lo > 0); b2 <- length(rise) > 0; b3 <- all(dE >= -0.05)
  b4 <- b2 && any(x$mean_drec[rise] >= 0 & x$median_drec[rise] >= 0); b5 <- all(x$harmed <= 3)
  say("\nwindow %s bars: 1 no dollar loss worse than -0.05 %s | 2 a dollar gain >= +0.05 with interval above 0 %s%s | 3 errors no worse than -0.05 %s | 4 within-state mean and median >= 0 at the rising budget %s | 5 harmed tail <= 3 at both budgets %s => %s",
      win, if (b1) "PASS" else "FAIL", if (b2) "PASS" else "FAIL", if (b2) sprintf(" (at %s)", paste0(round(100 * x$budget[rise]), "%", collapse = ", ")) else "",
      if (b3) "PASS" else "FAIL", if (b4) "PASS" else "FAIL", if (b5) "PASS" else "FAIL", if (b1 && b2 && b3 && b4 && b5) "PASSES" else "FAILS")
}
