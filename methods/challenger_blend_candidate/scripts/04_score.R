# Challenger benchmark, step 3: score every learner's test-year ranking against model B (2026-11-12). See design_note.md.
#   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" 04_score.R
# Scoring is the evaluation benchmark's: methods/eval_benchmark/score_ranking.R on eval_benchmark_f1109.rds. B is
# read from its stored scores (out_f1109_eval), never refit.
# The three top metrics (project lead, 2026-11-12, set before any challenger was scored), each pooled over the 49
# states on kept cases:
#   dr5    national share of error dollars caught at each state's top 5% of cases
#   band   share of error dollars added by the cases between 5% and 10% (dr10 - dr5), so the 10% figure is not the
#          5% figure counted again
#   dr10   share of error dollars caught at each state's top 10%
# Findings 46's L and Q are reported as companions only. The five workloads L averages are nested lists, so half of
# its weight sits on a state's top 2% of cases; it is kept for the link to how B was chosen.
HERE <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE))))
REPO <- "C:/Users/ericg/snap_qc_v27"
source(file.path(REPO, "methods/eval_benchmark/score_ranking.R"))
bench <- read_benchmark(file.path(REPO, "additional_data/eval_benchmark_f1109.rds"))
stopifnot(startsWith(attr(bench, "frame_sha256"), "08e0e825"))
RES <- file.path(HERE, "results"); dir.create(RES, showWarnings = FALSE)
BUD <- c(0.02, 0.03, 0.05, 0.075, 0.10); WEIGHTS <- c(W1 = 0.20, W2 = 0.40, W3 = 0.40); YEAR <- c(W1 = "2019", W2 = "2022", W3 = "2024")
NB <- 2000L; WN <- names(YEAR); STATS <- c("dr5", "band", "dr10", "L", "pooled5", "pooled10")
EXPECT <- c(xgb_ref = 10, xgb_tuned = 10, lgbm = 10, catboost = 5, ebm_main = 3, ebm_pairs = 3, bart = 4, tabnet = 3)   # fits per bag, design note
BSUB <- c(B = 10, B_5seeds = 5, B_3seeds = 3, B_1seed = 1)
# State weights for the national figures, as the results map computes its nationwide dollar line (pooledDollarLift
# and FY24_ERROR_DOLLARS in snap-qc-results-map/map.js): each state's share of error dollars caught is weighted by
# the state's official error dollars, so a state counts by its actual caseload and not by its QC sample size
# (project lead, 2026-11-12: "they should contribute based on their actual cases and not be reweighted for
# balance"). Official error dollars = FNS payment error rate of the scored year x FY2024 issuance (the only issuance
# year on file; for FY2024 this is the map's table exactly). The District of Columbia has no weight in the map's
# table, so the national figures cover 48 states, as the map's do.
AD <- file.path(REPO, "additional_data"); iss <- read.csv(file.path(AD, "fns_state_issuance_fy2024.csv"), stringsAsFactors = FALSE)
WSTATE <- lapply(YEAR, function(fy) { per <- read.csv(file.path(AD, sprintf("fns_payment_error_rates_fy%s.csv", fy)), stringsAsFactors = FALSE)
  w <- setNames(per$payment_error_rate / 100 * iss$issuance_dollars[match(per$state, iss$state)], per$state)
  w[!(names(w) %in% c("District of Columbia", "United States")) & is.finite(w)] })
# One pass per ranking: each state flags max(1, round(b x its cases)) from the top of its kept cases (score_ranking's
# rule; fastL in s46_run.R). Returns dr5, band, dr10 (each state's share of error dollars caught, weighted by ws),
# L (findings 46: the mean state's $/flag over the five workloads, over mean $ per kept case; states weighted
# equally) and the sample-pooled shares at 5% and 10% (summarise_ranking's dollar_recall, the model card's basis).
stats_of <- function(sc, dv, keep, SS, ws) { perflag <- numeric(length(BUD)); caught <- numeric(length(BUD)); num <- c(0, 0); den <- 0
  for (s in names(SS)) { ix <- SS[[s]]; n <- length(ix); kk <- pmax(1L, round(BUD * n)); i2 <- ix[keep[ix]]; if (!length(i2)) next
    cs <- cumsum(dv[i2][order(-sc[i2], i2)]); got <- cs[pmin(kk, length(i2))]; perflag <- perflag + got / kk; caught <- caught + got
    w <- ws[s]; tot <- cs[length(cs)]; if (!is.na(w) && tot > 0) { num <- num + w * got[c(3, 5)] / tot; den <- den + w } }
  dr <- num / den; pooled <- caught / sum(dv[keep])
  c(dr5 = dr[1], band = dr[2] - dr[1], dr10 = dr[2], L = mean(perflag / length(SS)) / mean(dv[keep]), pooled5 = pooled[3], pooled10 = pooled[5]) }

fl <- list.files(file.path(HERE, "scores"), "^.+_W[123]_[0-9]+\\.csv$")
info <- data.frame(file = fl, learner = sub("_W[123]_[0-9]+\\.csv$", "", fl), window = sub("^.+_(W[123])_[0-9]+\\.csv$", "\\1", fl), stringsAsFactors = FALSE)
only <- Sys.getenv("SCORE_ONLY")   # e.g. SCORE_ONLY=xgb_ref,xgb_tuned scores the reference arms without reading a challenger's file
if (nzchar(only)) info <- info[info$learner %in% strsplit(only, ",")[[1]], ]
learners <- unique(info$learner)
by_window <- list(); by_state <- list(); BOOT <- list(); VAL <- list(); SEEDVAR <- list(); NSEED <- list()
for (w in WN) {
  b <- bench[bench$fiscal_year == YEAR[[w]], ]; b <- b[order(b$frame_row), ]
  dv <- b$error_dollars; k <- b$kept; sv <- b$state; SS <- split(seq_along(sv), sv)
  Bm <- as.matrix(read.csv(file.path(HERE, "data", paste0(w, "_B_scores.csv")))); stopifnot(nrow(Bm) == nrow(b), ncol(Bm) == 10)
  ED <- lapply(BSUB, function(n) Bm[, seq_len(n), drop = FALSE])
  for (l in learners) { f <- info$file[info$learner == l & info$window == w]; if (!length(f)) next
    M <- sapply(f, function(x) { z <- read.csv(file.path(HERE, "scores", x), stringsAsFactors = FALSE)
      stopifnot(nrow(z) == nrow(b), all(z$st == sv)); z$ed })       # each score file carries the state: rows must be the benchmark's
    stopifnot(all(is.finite(M))); ED[[l]] <- M }
  ws <- WSTATE[[w]]; stopifnot(length(setdiff(names(SS), c(names(ws), "District of Columbia"))) == 0, sum(names(SS) %in% names(ws)) == 48)
  bag <- lapply(ED, rowMeans); st1 <- function(sc) stats_of(sc, dv, k, SS, ws)
  singleB <- apply(Bm, 2, st1)                                         # B's ten single-seed values, one row per statistic
  tabB <- score_ranking(bench, bag$B, YEAR[[w]])
  for (l in names(ED)) { sc <- bag[[l]]; tab <- score_ranking(bench, sc, YEAR[[w]]); s <- summarise_ranking(tab); v <- st1(sc)
    stopifnot(abs(v[["pooled5"]] - s$dollar_recall[s$budget == 0.05]) < 1e-12, abs(v[["pooled10"]] - s$dollar_recall[s$budget == 0.10]) < 1e-12)   # same figures as score_ranking()
    okw <- tab$state %in% names(ws) & tab$dollars_all > 0                 # and the weighted figure from score_ranking()'s own state table
    for (bu in c(0.05, 0.10)) { x <- tab[okw & tab$budget == bu, ]
      stopifnot(abs(sum(ws[x$state] * x$dollar_recall) / sum(ws[x$state]) - v[[if (bu == 0.05) "dr5" else "dr10"]]) < 1e-12) }
    single <- apply(ED[[l]], 2, st1); NSEED[[l]][w] <- ncol(ED[[l]]); VAL[[l]] <- cbind(VAL[[l]], v); colnames(VAL[[l]])[ncol(VAL[[l]])] <- w
    # variance of a single fit's statistic across seeds. For B's smaller bags it is taken from all ten seeds.
    sv1 <- if (l %in% names(BSUB)) apply(singleB, 1, var) else if (ncol(ED[[l]]) > 1) apply(single, 1, var) else rep(NA_real_, length(STATS))
    SEEDVAR[[l]] <- cbind(SEEDVAR[[l]], sv1); colnames(SEEDVAR[[l]])[ncol(SEEDVAR[[l]])] <- w
    stopifnot(all(tab$state == tabB$state), all(tab$budget == tabB$budget))
    dtr <- tab$times_random - tabB$times_random                           # state by state, error dollars, in times random
    ddr <- tab$dollar_recall - tabB$dollar_recall                         # the same in share of error dollars caught
    # harmed tail, the findings 53 definition: a state whose share of error dollars caught falls by more than 0.05
    pair <- do.call(rbind, lapply(c(0.05, 0.10), function(bu) { x <- dtr[tab$budget == bu]
      data.frame(budget = bu, state_median_change = median(x), state_mean_change = mean(x), states_ahead = sum(x > 0), states_behind = sum(x < 0),
                 states_harmed = sum(ddr[tab$budget == bu] < -0.05)) }))
    by_window[[paste(w, l)]] <- cbind(window = w, score_year = YEAR[[w]], learner = l, seeds = ncol(ED[[l]]),
      complete = if (l %in% names(EXPECT)) ncol(ED[[l]]) == EXPECT[[l]] else TRUE, national_share = c(v[["dr5"]], v[["dr10"]]), national_band_5_10 = v[["band"]], L = v[["L"]],
      rank_cor_with_B = cor(sc, bag$B, method = "spearman"),
      s[, c("budget", "flags", "precision", "base_rate", "dollar_recall", "times_random", "median_state_times_random", "states_below_1x")],
      errors_caught = as.numeric(tapply(tab$errors_caught, tab$budget, sum)), dollars_caught = as.numeric(tapply(tab$dollars_caught, tab$budget, sum)), pair[, -1])
    by_state[[paste(w, l)]] <- cbind(window = w, learner = l, tab[, c("budget", "state", "flags", "errors_caught", "dollars_caught", "precision", "dollar_recall", "times_random")],
                                     times_random_minus_B = dtr, dollar_recall_minus_B = ddr) }
  # paired bootstrap: cases resampled within state, the same resamples for every learner (score_window in s46_run.R)
  set.seed(4646); cn <- names(bag)
  BOOT[[w]] <- replicate(NB, { ii <- unlist(lapply(SS, function(ix) ix[sample.int(length(ix), replace = TRUE)]), use.names = FALSE)
    S2 <- split(seq_along(ii), sv[ii]); k2 <- k[ii]; d2 <- dv[ii]
    vapply(cn, function(l) stats_of(bag[[l]][ii], d2, k2, S2, ws), numeric(length(STATS))) })   # statistic x learner x draw
  dimnames(BOOT[[w]]) <- list(STATS, cn, NULL)
  cat(sprintf("%s (FY%s) scored: %s\n", w, YEAR[[w]], paste(cn, collapse = ", ")))
}
bw <- do.call(rbind, by_window); rownames(bw) <- NULL
write.csv(bw, file.path(RES, "by_window.csv"), row.names = FALSE)
write.csv(do.call(rbind, by_state), file.path(RES, "by_state.csv"), row.names = FALSE)

# Across the three tests: each statistic weighted 0.2 / 0.4 / 0.4 (FY2019 / FY2022 / FY2024), and its difference from
# a reference with a paired standard error: the bootstrap variance of the difference plus each bag's seed term
# (single-fit seed variance over the fits in the bag).
full <- names(which(sapply(names(VAL), function(l) all(WN %in% colnames(VAL[[l]])))))
val <- function(l, s) VAL[[l]][s, WN]
sterm <- function(l, s) { v <- SEEDVAR[[l]][s, WN] / NSEED[[l]][WN]; if (anyNA(v)) stop("no seed variance for ", l); v }
ref_of <- function(l) { n <- min(NSEED[[l]][WN]); if (l %in% names(BSUB)) "B" else if (n >= 10) "B" else if (n >= 5) "B_5seeds" else if (n >= 3) "B_3seeds" else "B_1seed" }
cmp <- function(l, ref, s) { d <- sum(WEIGHTS * (val(l, s) - val(ref, s)))
  se <- sqrt(sum(WEIGHTS^2 * (sapply(WN, function(w) var(BOOT[[w]][s, l, ] - BOOT[[w]][s, ref, ])) + sterm(l, s) + sterm(ref, s)))); c(d = d, se = se, z = d / se) }
isB <- function(l) l %in% names(BSUB)
p5B <- sum(WEIGHTS * sapply(WN, function(w) bw$precision[bw$learner == "B" & bw$window == w & bw$budget == 0.05]))
TOP <- do.call(rbind, lapply(full, function(l) { out <- data.frame(learner = l, seeds = min(NSEED[[l]][WN]), complete = all(bw$complete[bw$learner == l]), matched_ref = ref_of(l))
  for (s in STATS) { a <- if (l == "B") c(d = 0, se = NA, z = NA) else cmp(l, "B", s)
    m <- if (isB(l)) c(z = NA) else cmp(l, ref_of(l), s)
    t <- if ("xgb_tuned" %in% full && !isB(l) && l != "xgb_tuned") cmp(l, "xgb_tuned", s) else c(d = NA, z = NA)
    r <- if ("xgb_ref" %in% full && !isB(l) && l != "xgb_ref") cmp(l, "xgb_ref", s) else c(d = NA, z = NA)
    x <- data.frame(sum(WEIGHTS * val(l, s)), val(l, s)[["W1"]], val(l, s)[["W2"]], val(l, s)[["W3"]], a[["d"]], a[["se"]], a[["z"]], m[["z"]], t[["d"]], t[["z"]], r[["d"]], r[["z"]])
    names(x) <- paste0(s, c("", "_FY2019", "_FY2022", "_FY2024", "_minus_B", "_se", "_z", "_z_matched", "_minus_xgb_tuned", "_z_xgb_tuned", "_minus_xgb_ref", "_z_xgb_ref")); out <- cbind(out, x) }
  out$precision_5pct_pooled <- sum(WEIGHTS * sapply(WN, function(w) bw$precision[bw$learner == l & bw$window == w & bw$budget == 0.05])); out }))
# The reading, per metric: ahead when at least 2 standard errors above BOTH the 10-seed B and the B bag of matching
# size; behind when 2 below both; otherwise not distinguishable.
lvl <- function(s) { zlo <- pmin(TOP[[paste0(s, "_z")]], TOP[[paste0(s, "_z_matched")]]); zhi <- pmax(TOP[[paste0(s, "_z")]], TOP[[paste0(s, "_z_matched")]])
  ifelse(is.na(zlo), "", ifelse(zlo >= 2, "ahead", ifelse(zhi <= -2, "behind", "same"))) }
for (s in c("dr5", "band", "dr10")) TOP[[paste0(s, "_vs_B")]] <- lvl(s)
# The overall call rests on the 10% share. "Ahead of B" also needs: ahead of xgb_tuned at 10% by 2 standard errors
# (the same learner as B, tuned on this frame like the challengers), pooled precision at 5% no more than 1.5 points
# below B's (the findings 46 guard), and not behind B at 5%.
isRef <- TOP$learner %in% c("xgb_ref", "xgb_tuned"); haveT <- "xgb_tuned" %in% TOP$learner
okT <- isRef | (!is.na(TOP$dr10_z_xgb_tuned) & TOP$dr10_z_xgb_tuned >= 2); okP <- TOP$precision_5pct_pooled >= p5B - 0.015
TOP$reading <- ifelse(TOP$dr10_vs_B == "", "", ifelse(TOP$dr10_vs_B == "behind", "behind B at 10%, at the stated settings",
  ifelse(TOP$dr10_vs_B == "same", ifelse(TOP$dr5_vs_B == "same", "not distinguishable from B", paste0("not distinguishable from B at 10%; ", TOP$dr5_vs_B, " at 5%")),
  ifelse(!okP, "ahead of B at 10% but fails the precision guard", ifelse(TOP$dr5_vs_B == "behind", "ahead of B at 10% but behind at 5%",
  ifelse(!haveT, "ahead of B at 10% (xgb_tuned not run: not yet a learner-only reading)",
  ifelse(okT, "ahead of B", "ahead of B at 10% but not of xgb_tuned: not assignable to the learner")))))))
rownames(TOP) <- NULL; write.csv(TOP, file.path(RES, "top_metrics.csv"), row.names = FALSE)

pc <- function(x) sprintf("%.2f", 100 * x); sg <- function(x) ifelse(is.na(x), "", sprintf("%+.2f", 100 * x)); zf <- function(x) ifelse(is.na(x), "", sprintf("%+.1f", x))
options(width = 280)
cat("\nTOP METRICS: national share of error dollars caught, %, as the map computes it: each state's share weighted by its official error dollars, 48 states.\n")
cat("Three tests weighted 0.2 / 0.4 / 0.4 (FY2019 / FY2022 / FY2024). precision_5 pools flag counts over the 49 states, as the map's precision line does.\n")
cat("diff = points against the 10-seed B; z = diff over its paired standard error; zm = against the B bag of matching size; zt = against xgb_tuned.\n")
show <- data.frame(learner = TOP$learner, fits = TOP$seeds,
  at_5 = pc(TOP$dr5), diff = sg(TOP$dr5_minus_B), z = zf(TOP$dr5_z), zm = zf(TOP$dr5_z_matched), zt = zf(TOP$dr5_z_xgb_tuned),
  band_5_10 = pc(TOP$band), diff = sg(TOP$band_minus_B), z = zf(TOP$band_z), zm = zf(TOP$band_z_matched), zt = zf(TOP$band_z_xgb_tuned),
  at_10 = pc(TOP$dr10), diff = sg(TOP$dr10_minus_B), z = zf(TOP$dr10_z), zm = zf(TOP$dr10_z_matched), zt = zf(TOP$dr10_z_xgb_tuned),
  precision_5 = pc(TOP$precision_5pct_pooled), reading = TOP$reading, check.names = FALSE)
print(show, row.names = FALSE)
cat("With six challengers and three metrics read one-sided, a 2 standard error reading is weak evidence on its own (Bonferroni over six at 0.025: z = 2.64).\n")
cat("\nBy test, national share of error dollars caught, % (FY2024 is the map's own figure):\n")
print(data.frame(learner = TOP$learner, at5_FY2019 = pc(TOP$dr5_FY2019), at5_FY2022 = pc(TOP$dr5_FY2022), at5_FY2024 = pc(TOP$dr5_FY2024),
  band_FY2019 = pc(TOP$band_FY2019), band_FY2022 = pc(TOP$band_FY2022), band_FY2024 = pc(TOP$band_FY2024),
  at10_FY2019 = pc(TOP$dr10_FY2019), at10_FY2022 = pc(TOP$dr10_FY2022), at10_FY2024 = pc(TOP$dr10_FY2024)), row.names = FALSE)
cat("\nCompanion: share of error dollars caught with the QC sample cases pooled over 49 states, % (the model card's basis; states count by sample size).\n")
print(data.frame(learner = TOP$learner, at5 = pc(TOP$pooled5), at5_FY2019 = pc(TOP$pooled5_FY2019), at5_FY2022 = pc(TOP$pooled5_FY2022), at5_FY2024 = pc(TOP$pooled5_FY2024),
  at10 = pc(TOP$pooled10), at10_FY2019 = pc(TOP$pooled10_FY2019), at10_FY2022 = pc(TOP$pooled10_FY2022), at10_FY2024 = pc(TOP$pooled10_FY2024),
  at10_diff = sg(TOP$pooled10_minus_B), at10_z = zf(TOP$pooled10_z)), row.names = FALSE)
cat("\nCompanion only (findings 46): L by test and Q, with Q's difference from B. States weighted equally.\n")
print(data.frame(learner = TOP$learner, L_FY2019 = round(TOP$L_FY2019, 3), L_FY2022 = round(TOP$L_FY2022, 3), L_FY2024 = round(TOP$L_FY2024, 3), Q = round(TOP$L, 3),
                 Q_minus_B = round(TOP$L_minus_B, 3), se = round(TOP$L_se, 3), z = zf(TOP$L_z)), row.names = FALSE)
if ("xgb_ref" %in% TOP$learner) { x <- TOP[TOP$learner == "xgb_ref", ]; ok <- abs(x$dr10_z) <= 2 && abs(x$dr5_z) <= 2
  cat(sprintf("\nHARNESS CHECK (xgb_ref, B's recipe refit here): against B, z = %+.2f at 5%% and %+.2f at 10%% -> %s. Rank correlation with B's scores: %s.\n", x$dr5_z, x$dr10_z,
              if (ok) "PASS" else "FAIL: DO NOT READ THE CHALLENGERS",
              paste(sprintf("%s %.4f", WN, sapply(WN, function(w) bw$rank_cor_with_B[bw$learner == "xgb_ref" & bw$window == w][1])), collapse = ", "))) }
if (any(!TOP$complete)) cat("NOTE: incomplete bags (fewer fits than the design note states):", paste(TOP$learner[!TOP$complete], collapse = ", "), "\n")
cat("\nBy test year and workload (state columns are changes against the 10-seed B; harmed = states whose share of error dollars caught falls by more than 0.05):\n")
print(bw[, c("window", "learner", "seeds", "budget", "precision", "dollar_recall", "times_random", "median_state_times_random", "states_below_1x",
             "state_median_change", "state_mean_change", "states_ahead", "states_behind", "states_harmed", "rank_cor_with_B")], digits = 4)
cat("SCORING DONE\n")
