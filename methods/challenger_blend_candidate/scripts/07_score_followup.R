# Follow-up run: the one scoring read (2026-11-13). See design_note_followup.md.
#   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" 07_score_followup.R [--smoke]
# Same scoring as 04_score.R: score_ranking() on eval_benchmark_f1109.rds, map state weights, three tests weighted
# 0.2 / 0.4 / 0.4, paired bootstrap of cases within state (2,000 draws, seed 4646). Differences from 04_score.R:
#   - a bag's seed variance is estimated from its own half-bags (the senior statistician's note: single-fit variance
#     over the number of fits understates it), and used as an upper bound for the full bag;
#   - each arm is also read on FY2024 alone, against its xgboost twin (xgb_es) and against the primary arm (xgb_rev);
#   - a smoother top-of-list figure, the mean of the map-weighted shares at 5%, 7.5% and 10% (drm), is read for the
#     "supported" level; the share at 10% without the three largest states is printed beside the lead figure;
#   - the Poisson deviance of error dollars against P x size (no exponent) and the within-state log loss of P are
#     printed as diagnostics and enter no reading;
#   - the reading has three levels: ahead, supported but not confirmed, not distinguishable (and behind).
SMOKE <- "--smoke" %in% commandArgs(TRUE)
HERE <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE))))
REPO <- "C:/Users/ericg/snap_qc_v27"
source(file.path(REPO, "methods/eval_benchmark/score_ranking.R"))
bench <- read_benchmark(file.path(REPO, "additional_data/eval_benchmark_f1109.rds"))
stopifnot(startsWith(attr(bench, "frame_sha256"), "08e0e825"))
ROOT <- file.path(HERE, "followup")      # --smoke: fewer draws, results to smoke/, for a dry run on last night's arms before any new arm exists
RES <- if (SMOKE) file.path(HERE, "smoke", "followup", "results") else file.path(ROOT, "results"); dir.create(RES, showWarnings = FALSE, recursive = TRUE)
BUD <- c(0.02, 0.03, 0.05, 0.075, 0.10); WEIGHTS <- c(W1 = 0.20, W2 = 0.40, W3 = 0.40); YEAR <- c(W1 = "2019", W2 = "2022", W3 = "2024")
NB <- if (SMOKE) 60L else 2000L; NSPLIT <- if (SMOKE) 6L else 40L; WN <- names(YEAR)
SHARES <- c("dr5", "band", "dr10", "drm", "dr10x3", "pooled5", "pooled10"); STATS <- c(SHARES, "pdev", "wll")
EXPECT <- c(xgb_es = 10, xgb_rev = 10, xgb_rev_b1 = 10, xgb_exp = 10, cb_rev = 5, blend = 5, bart_long = 8, bart_rev = 8,
            xgb_ref = 10, xgb_tuned = 10, catboost = 5, bart = 4)
LAST <- c("xgb_ref", "xgb_tuned", "catboost", "bart")                 # last night's arms, read again for context only
AD <- file.path(REPO, "additional_data"); iss <- read.csv(file.path(AD, "fns_state_issuance_fy2024.csv"), stringsAsFactors = FALSE)
WSTATE <- lapply(YEAR, function(fy) { per <- read.csv(file.path(AD, sprintf("fns_payment_error_rates_fy%s.csv", fy)), stringsAsFactors = FALSE)
  w <- setNames(per$payment_error_rate / 100 * iss$issuance_dollars[match(per$state, iss$state)], per$state)
  w[!(names(w) %in% c("District of Columbia", "United States")) & is.finite(w)] })
# shares, as stats_of() in 04_score.R: each state flags max(1, round(b x its cases)) from the top of its kept cases.
# dr5, band, dr10: each state's share of error dollars caught, weighted by ws. drm: the mean of the weighted shares at
# 5%, 7.5% and 10%. dr10x3: dr10 without the three states of largest weight (big3). pooled5, pooled10: sample cases pooled.
shares_of <- function(sc, dv, keep, SS, ws, big3) { caught <- numeric(length(BUD)); num <- c(0, 0, 0); den <- 0; numx <- 0; denx <- 0
  for (s in names(SS)) { ix <- SS[[s]]; n <- length(ix); kk <- pmax(1L, round(BUD * n)); i2 <- ix[keep[ix]]; if (!length(i2)) next
    cs <- cumsum(dv[i2][order(-sc[i2], i2)]); got <- cs[pmin(kk, length(i2))]; caught <- caught + got
    w <- unname(ws[s]); tot <- cs[length(cs)]
    if (!is.na(w) && tot > 0) { num <- num + w * got[3:5] / tot; den <- den + w; if (!(s %in% big3)) { numx <- numx + w * got[5] / tot; denx <- denx + w } } }
  dr <- num / den; pooled <- caught / sum(dv[keep])
  c(dr5 = dr[1], band = dr[3] - dr[1], dr10 = dr[3], drm = mean(dr), dr10x3 = numx / denx, pooled5 = pooled[3], pooled10 = pooled[5]) }
# Diagnostics. gi is the state as an integer 1..G with every state present, so rowsum() rows are in index order.
# Poisson deviance of error dollars against the score, the score rescaled to each state's dollars (pois_dev in s46_lib.R); kept cases
pdev_of <- function(sc, dv, keep, gi) { mu <- sc[keep]; y <- dv[keep]; g <- gi[keep]
  f <- pmax(rowsum(y, g)[, 1], 1) / pmax(rowsum(mu, g)[, 1], 1e-9); mu <- pmax(mu * f[g], 1e-9)
  2 * mean(ifelse(y > 0, y * log(pmax(y, 1e-300) / mu), 0) - (y - mu)) }
# within-state log loss of P (wll in s46_lib.R: one intercept shift per state fitted on the scored rows); all cases
wll_of <- function(p, y, gi) { z <- qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6)); a <- numeric(max(gi))
  for (it in 1:40) { pr <- plogis(z + a[gi]); st <- rowsum(y - pr, gi)[, 1] / pmax(rowsum(pr * (1 - pr), gi)[, 1], 1e-12); a <- a + st; if (max(abs(st)) < 1e-8) break }
  zz <- z + a[gi]; -mean(y * plogis(zz, log.p = TRUE) + (1 - y) * plogis(-zz, log.p = TRUE)) }
# diag = FALSE skips the two diagnostics (they are slow and enter no reading): the bootstrap computes them on its first NBD draws only
all_stats <- function(sc, ps, p, dv, y, keep, SS, ws, gi, big3, diag = TRUE) c(shares_of(sc, dv, keep, SS, ws, big3),
  pdev = if (diag) pdev_of(ps, dv, keep, gi) else NA_real_, wll = if (!diag || is.null(p)) NA_real_ else wll_of(p, y, gi))
NBD <- 300L

found <- function(dir, pat) { f <- list.files(dir, pat); data.frame(dir = rep(dir, length(f)), file = f, stringsAsFactors = FALSE) }
src <- rbind(found(file.path(ROOT, "scores"), "^.+_W[123]_[0-9]+\\.csv$"), found(file.path(HERE, "scores"), sprintf("^(%s)_W[123]_[0-9]+\\.csv$", paste(LAST, collapse = "|"))))
src$learner <- sub("_W[123]_[0-9]+\\.csv$", "", src$file); src$window <- sub("^.+_(W[123])_[0-9]+\\.csv$", "\\1", src$file)
learners <- unique(src$learner); by_window <- list(); by_state <- list(); BOOT <- list(); VAL <- list(); SEEDVAR <- list(); NSEED <- list(); t00 <- Sys.time()
for (w in WN) {
  b <- bench[bench$fiscal_year == YEAR[[w]], ]; b <- b[order(b$frame_row), ]
  dv <- b$error_dollars; k <- b$kept; sv <- b$state; SS <- split(seq_along(sv), sv); gi <- as.integer(factor(sv)); y <- as.numeric(dv > 0)
  D <- readRDS(file.path(HERE, "data", paste0(w, ".rds"))); D <- D[D$role == "test", ]     # the over-tolerance label, to check it is dollars > 0
  stopifnot(nrow(D) == nrow(b), all(D$st == sv), all((D$y == 1) == (dv > 0)), length(unique(gi)) == max(gi))
  Bm <- as.matrix(read.csv(file.path(HERE, "data", paste0(w, "_B_scores.csv")))); stopifnot(nrow(Bm) == nrow(b), ncol(Bm) == 10)
  ED <- list(B = Bm); PP <- list(); PS <- list(B = Bm)        # scores; P where the arm carries it; P x size (the score without an exponent)
  for (l in learners) { f <- src[src$learner == l & src$window == w, ]; if (!nrow(f)) next
    Z <- lapply(seq_len(nrow(f)), function(i) { z <- read.csv(file.path(f$dir[i], f$file[i]), stringsAsFactors = FALSE); stopifnot(nrow(z) == nrow(b), all(z$st == sv)); z })
    ED[[l]] <- sapply(Z, function(z) z$ed); stopifnot(all(is.finite(ED[[l]])))
    P <- sapply(Z, function(z) z$p); S <- sapply(Z, function(z) z$size); ok <- all(is.finite(P)) && all(is.finite(S)); if (ok) PP[[l]] <- P; PS[[l]] <- if (ok) P * S else ED[[l]] }
  ws <- WSTATE[[w]]; stopifnot(length(setdiff(names(SS), c(names(ws), "District of Columbia"))) == 0, sum(names(SS) %in% names(ws)) == 48)
  big3 <- names(sort(ws[names(ws) %in% names(SS)], decreasing = TRUE))[1:3]; cat(sprintf("%s: three largest weights: %s\n", w, paste(big3, collapse = ", ")))
  bag <- lapply(ED, rowMeans); psbag <- lapply(PS, rowMeans); pbag <- lapply(PP, rowMeans)
  st1 <- function(sc, ps, p = NULL) all_stats(sc, ps, p, dv, y, k, SS, ws, gi, big3)
  tabB <- score_ranking(bench, bag$B, YEAR[[w]])
  for (l in names(ED)) { sc <- bag[[l]]; tab <- score_ranking(bench, sc, YEAR[[w]]); s <- summarise_ranking(tab); v <- st1(sc, psbag[[l]], pbag[[l]])
    stopifnot(abs(v[["pooled5"]] - s$dollar_recall[s$budget == 0.05]) < 1e-12, abs(v[["pooled10"]] - s$dollar_recall[s$budget == 0.10]) < 1e-12)   # same figures as score_ranking()
    okw <- tab$state %in% names(ws) & tab$dollars_all > 0                 # and the weighted figures from score_ranking()'s own state table
    for (bu in c(0.05, 0.10)) { x <- tab[okw & tab$budget == bu, ]
      stopifnot(abs(sum(ws[x$state] * x$dollar_recall) / sum(ws[x$state]) - v[[if (bu == 0.05) "dr5" else "dr10"]]) < 1e-12) }
    n <- ncol(ED[[l]]); NSEED[[l]][w] <- n; VAL[[l]] <- cbind(VAL[[l]], v); colnames(VAL[[l]])[ncol(VAL[[l]])] <- w
    # seed variance of the bag, from random splits of its fits into two half-bags: E[(half A - half B)^2] / 2 is the
    # variance of a half-bag, an upper bound for the full bag's
    set.seed(990 + n); sv1 <- if (n < 2) rep(NA_real_, length(STATS)) else rowMeans(replicate(NSPLIT, { a <- sample.int(n, n %/% 2); bb <- setdiff(seq_len(n), a)
      h <- function(ix) st1(rowMeans(ED[[l]][, ix, drop = FALSE]), rowMeans(PS[[l]][, ix, drop = FALSE]), if (is.null(PP[[l]])) NULL else rowMeans(PP[[l]][, ix, drop = FALSE])); (h(a) - h(bb))^2 / 2 }))
    SEEDVAR[[l]] <- cbind(SEEDVAR[[l]], sv1); colnames(SEEDVAR[[l]])[ncol(SEEDVAR[[l]])] <- w
    stopifnot(all(tab$state == tabB$state), all(tab$budget == tabB$budget)); ddr <- tab$dollar_recall - tabB$dollar_recall; dtr <- tab$times_random - tabB$times_random
    pair <- do.call(rbind, lapply(c(0.05, 0.10), function(bu) { x <- dtr[tab$budget == bu]
      data.frame(budget = bu, state_median_change = median(x), state_mean_change = mean(x), states_ahead = sum(x > 0), states_behind = sum(x < 0), states_harmed = sum(ddr[tab$budget == bu] < -0.05)) }))
    by_window[[paste(w, l)]] <- cbind(window = w, score_year = YEAR[[w]], learner = l, seeds = n, complete = if (l %in% names(EXPECT)) n == EXPECT[[l]] else TRUE,
      national_share = c(v[["dr5"]], v[["dr10"]]), national_band_5_10 = v[["band"]], dollar_deviance = v[["pdev"]], P_log_loss = v[["wll"]], rank_cor_with_B = cor(sc, bag$B, method = "spearman"),
      s[, c("budget", "flags", "precision", "base_rate", "dollar_recall", "times_random", "median_state_times_random", "states_below_1x")], pair[, -1])
    by_state[[paste(w, l)]] <- cbind(window = w, learner = l, tab[, c("budget", "state", "flags", "errors_caught", "dollars_caught", "precision", "dollar_recall", "times_random")],
                                     times_random_minus_B = dtr, dollar_recall_minus_B = ddr) }
  set.seed(4646); cn <- names(bag)
  BOOT[[w]] <- sapply(seq_len(NB), function(r) { ii <- unlist(lapply(SS, function(ix) ix[sample.int(length(ix), replace = TRUE)]), use.names = FALSE)
    S2 <- split(seq_along(ii), sv[ii]); k2 <- k[ii]; d2 <- dv[ii]; y2 <- y[ii]; g2 <- gi[ii]
    vapply(cn, function(l) all_stats(bag[[l]][ii], psbag[[l]][ii], if (is.null(pbag[[l]])) NULL else pbag[[l]][ii], d2, y2, k2, S2, ws, g2, big3, diag = r <= NBD), numeric(length(STATS))) },
    simplify = "array")
  dimnames(BOOT[[w]]) <- list(STATS, cn, NULL)
  cat(sprintf("%s (FY%s) scored: %s  [%.1f min so far]\n", w, YEAR[[w]], paste(cn, collapse = ", "), as.numeric(difftime(Sys.time(), t00, units = "mins"))))
}
bw <- do.call(rbind, by_window); rownames(bw) <- NULL
write.csv(bw, file.path(RES, "by_window.csv"), row.names = FALSE); write.csv(do.call(rbind, by_state), file.path(RES, "by_state.csv"), row.names = FALSE)

# arms read: all three tests present, at least two fits in each (a one-fit bag has no seed variance)
full <- names(which(sapply(names(VAL), function(l) all(WN %in% colnames(VAL[[l]])) && all(NSEED[[l]][WN] >= 2))))
val <- function(l, s) VAL[[l]][s, WN]
sterm <- function(l, s) { v <- SEEDVAR[[l]][s, WN]; if (anyNA(v)) stop("no seed variance for ", l); v }
# difference of arm l from ref on statistic s, over the tests with weights wt, with its paired standard error:
# the bootstrap variance of the difference plus each bag's seed term
cmp <- function(l, ref, s, wt = WEIGHTS) { if (is.na(ref) || !(ref %in% full) || l == ref || anyNA(val(l, s)) || anyNA(val(ref, s))) return(c(d = NA, se = NA, z = NA))
  d <- sum(wt * (val(l, s) - val(ref, s))); se <- sqrt(sum(wt^2 * (sapply(WN, function(w) var(BOOT[[w]][s, l, ] - BOOT[[w]][s, ref, ], na.rm = TRUE)) + sterm(l, s) + sterm(ref, s)))); c(d = d, se = se, z = d / se) }
FY24 <- c(W1 = 0, W2 = 0, W3 = 1); TWIN <- "xgb_es"; PRIM <- "xgb_rev"
prec5 <- function(l) sum(WEIGHTS * sapply(WN, function(w) bw$precision[bw$learner == l & bw$window == w & bw$budget == 0.05]))
harm <- function(l) { x <- bw$states_harmed[bw$learner == l]; sprintf("%d-%d", min(x), max(x)) }
updown <- function(l) paste(sapply(WN, function(w) { x <- bw[bw$learner == l & bw$window == w & bw$budget == 0.10, ]; sprintf("%d/%d", x$states_ahead, x$states_behind) }), collapse = " ")
TOP <- do.call(rbind, lapply(setdiff(full, "B"), function(l) { tw <- if (l %in% c(TWIN, LAST)) NA else TWIN; pr <- if (l %in% c(TWIN, PRIM, LAST)) NA else PRIM
  out <- data.frame(learner = l, fits = min(NSEED[[l]][WN]), complete = all(bw$complete[bw$learner == l]), stringsAsFactors = FALSE)
  for (s in STATS) { a <- cmp(l, "B", s); t <- cmp(l, tw, s); r <- cmp(l, pr, s); f <- cmp(l, "B", s, FY24)
    x <- data.frame(sum(WEIGHTS * val(l, s)), a[["d"]], a[["se"]], a[["z"]], t[["d"]], t[["z"]], r[["d"]], r[["z"]], val(l, s)[["W3"]], f[["d"]], f[["z"]])
    names(x) <- paste0(s, c("", "_minus_B", "_se", "_z", "_minus_twin", "_z_twin", "_minus_rev", "_z_rev", "_FY2024", "_FY2024_minus_B", "_FY2024_z")); out <- cbind(out, x) }
  out$precision_5 <- prec5(l); out$states_harmed <- harm(l); out$states_up_down_10 <- updown(l); out }))
valB <- sapply(STATS, function(s) sum(WEIGHTS * val("B", s))); p5B <- prec5("B")
# ---- the reading (design_note_followup.md, section 6)
has <- function(l) l %in% TOP$learner
SEC <- c(if (has("cb_rev")) "cb_rev", if (has("bart_rev")) "bart_rev" else if (has("bart_long")) "bart_long", if (has("blend")) "blend")
TOP$role <- ifelse(TOP$learner == PRIM, "primary", ifelse(TOP$learner %in% SEC, "secondary", ifelse(TOP$learner == "xgb_rev_b1", "fallback",
            ifelse(TOP$learner %in% LAST, "last night", "component"))))
m <- max(1L, length(SEC)); zsec <- qnorm(1 - 0.025 / m); zsup_sec <- qnorm(1 - 0.05 / m)      # Bonferroni over the secondary arms fitted
bar <- ifelse(TOP$role == "secondary", zsec, 1.96); sup <- ifelse(TOP$role == "secondary", zsup_sec, 1.645)
# Guards: not more than 1.645 standard errors below B at 5%; pooled precision at 5% no more than 1.5 points below
# B's; sample-pooled share at 10% not below B's.
guards <- TOP$dr5_z >= -1.645 & TOP$precision_5 >= p5B - 0.015 & TOP$pooled10_minus_B >= 0
TOP$guards <- ifelse(guards, "met", paste0("not met:", ifelse(TOP$dr5_z < -1.645, " 5%", ""), ifelse(TOP$precision_5 < p5B - 0.015, " precision", ""), ifelse(TOP$pooled10_minus_B < 0, " pooled", "")))
TOP$reading <- ifelse(TOP$dr10_z >= bar & guards, "ahead of B",
  ifelse(TOP$dr10_z >= bar, "ahead of B at 10% but a guard is not met",
  ifelse(TOP$dr10_z <= -1.96, "behind B at 10%",
  ifelse(TOP$dr10_minus_B > 0 & guards & TOP$drm_z >= sup, "supported, not confirmed: ahead of B on the mean share at 5%, 7.5% and 10%; the 10% gain alone is unresolved",
         "not distinguishable from B"))))
claim <- TOP$role %in% c("primary", "secondary"); TOP$reading[!claim] <- paste0("(", TOP$role[!claim], ifelse(TOP$role[!claim] == "fallback", ", read only if xgb_rev misses a guard", ", no claim"), ") ", TOP$reading[!claim])
rownames(TOP) <- NULL; write.csv(TOP, file.path(RES, "top_metrics.csv"), row.names = FALSE)

pc <- function(x) sprintf("%.2f", 100 * x); sg <- function(x) ifelse(is.na(x), "", sprintf("%+.2f", 100 * x)); zf <- function(x) ifelse(is.na(x), "", sprintf("%+.1f", x)); options(width = 320)
cat(sprintf("\nB (stored, 10 seeds): at 5%% %s, 5-10%% band %s, at 10%% %s; mean of 5/7.5/10%% %s; at 10%% without the three largest states %s; sample-pooled at 10%% %s; precision at 5%% %s\n",
            pc(valB[["dr5"]]), pc(valB[["band"]]), pc(valB[["dr10"]]), pc(valB[["drm"]]), pc(valB[["dr10x3"]]), pc(valB[["pooled10"]]), pc(p5B)))
cat(sprintf("Bars: ahead z >= 1.96 (primary, fallback), %.2f (secondary, %d fitted); supported z >= 1.645 / %.2f on the mean share; 5%% guard z >= -1.645.\n", zsec, length(SEC), zsup_sec))
cat("\nTOP METRICS: national share of error dollars caught, %, map state weights, three tests weighted 0.2 / 0.4 / 0.4. diff = points against the stored 10-seed B;\n")
cat("se = paired standard error (bootstrap over cases plus half-bag seed terms); z = diff / se; zt = z against the xgboost twin (xgb_es); zr = z against the primary arm (xgb_rev).\n")
print(data.frame(arm = TOP$learner, role = TOP$role, fits = TOP$fits, at_5 = pc(TOP$dr5), diff = sg(TOP$dr5_minus_B), z = zf(TOP$dr5_z), band = pc(TOP$band), diff = sg(TOP$band_minus_B), z = zf(TOP$band_z),
  at_10 = pc(TOP$dr10), diff = sg(TOP$dr10_minus_B), se = pc(TOP$dr10_se), z = zf(TOP$dr10_z), zt = zf(TOP$dr10_z_twin), zr = zf(TOP$dr10_z_rev),
  mean_5_10 = pc(TOP$drm), diff = sg(TOP$drm_minus_B), se = pc(TOP$drm_se), z = zf(TOP$drm_z), check.names = FALSE), row.names = FALSE)
cat("\nRobustness beside the lead figure: the share at 10% without the three largest states, the sample-pooled share, states up/down against B at 10% by test (FY2019 FY2022 FY2024), precision, harmed states.\n")
print(data.frame(arm = TOP$learner, at_10_without_big3 = pc(TOP$dr10x3), diff = sg(TOP$dr10x3_minus_B), z = zf(TOP$dr10x3_z), pooled_10 = pc(TOP$pooled10), diff = sg(TOP$pooled10_minus_B), z = zf(TOP$pooled10_z),
  states_up_down = TOP$states_up_down_10, precision_5 = pc(TOP$precision_5), harmed = TOP$states_harmed, guards = TOP$guards, check.names = FALSE), row.names = FALSE)
cat("\nFY2024 alone (the only test the era pieces change), points against B:\n")
print(data.frame(arm = TOP$learner, at_5 = pc(TOP$dr5_FY2024), diff = sg(TOP$dr5_FY2024_minus_B), z = zf(TOP$dr5_FY2024_z), at_10 = pc(TOP$dr10_FY2024), diff = sg(TOP$dr10_FY2024_minus_B), z = zf(TOP$dr10_FY2024_z),
  mean_5_10 = pc(TOP$drm_FY2024), diff = sg(TOP$drm_FY2024_minus_B), z = zf(TOP$drm_FY2024_z), pooled_10 = pc(TOP$pooled10_FY2024), diff = sg(TOP$pooled10_FY2024_minus_B), z = zf(TOP$pooled10_FY2024_z), check.names = FALSE), row.names = FALSE)
cat("\nDiagnostics on the test years, no reading rests on them (lower is better): Poisson deviance of error dollars against P x size, against B; log loss of P, against the xgboost twin.\n")
print(data.frame(arm = TOP$learner, dollar_deviance = round(TOP$pdev, 4), minus_B = round(TOP$pdev_minus_B, 4), z = zf(TOP$pdev_z), z_twin = zf(TOP$pdev_z_twin),
  P_log_loss = round(TOP$wll, 5), minus_twin = round(TOP$wll_minus_twin, 5), z_twin = zf(TOP$wll_z_twin), check.names = FALSE), row.names = FALSE)
cat("\nREADING\n"); for (i in seq_len(nrow(TOP))) cat(sprintf("  %-11s %s\n", TOP$learner[i], TOP$reading[i]))
if (!has(PRIM)) cat("  No primary arm: no xgboost piece was promoted on the held-back years.\n")
if (any(!TOP$complete)) cat("NOTE: incomplete bags (fewer fits than the design note states):", paste(TOP$learner[!TOP$complete], collapse = ", "), "\n")
miss <- setdiff(unique(src$learner), full); if (length(miss)) cat("NOTE: arms without all three tests or with a single fit, not read:", paste(miss, collapse = ", "), "\n")
if (has("xgb_ref")) { x <- TOP[TOP$learner == "xgb_ref", ]; cat(sprintf("\nHarness check (xgb_ref, last night's refit of B's recipe): z = %+.2f at 5%%, %+.2f at 10%% against B.\n", x$dr5_z, x$dr10_z)) }
cat("\nBy test year and workload (state columns are changes against the 10-seed B; harmed = states whose share falls by more than 0.05):\n")
print(bw[, c("window", "learner", "seeds", "budget", "precision", "national_share", "dollar_recall", "times_random", "states_below_1x", "state_mean_change", "states_ahead", "states_behind", "states_harmed", "rank_cor_with_B")], digits = 4)
cat("SCORING DONE\n")
