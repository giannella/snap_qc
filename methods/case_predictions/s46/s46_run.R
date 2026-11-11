# §46 Amendment 1 driver. Usage: Rscript s46_run.R <phase> [candidates]
#   phases: choices | tuneB | freeze | methodB | stage1 | freeze2 | fit <A,B,G,...> | score
# S46_SMOKE=1 runs the whole chain on FY2017 -> FY2018 (allowed pair) with small grids and two seeds.
here <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE))))
source(file.path(here, "s46_lib.R")); source(file.path(here, "s46_models.R"))
args <- commandArgs(TRUE); PHASE <- args[1]; CANDS <- if (length(args) > 1) strsplit(args[2], ",")[[1]] else character(0)
say("=== phase %s %s (smoke %s)", PHASE, paste(CANDS, collapse = ","), SMOKE)
WN <- names(WINDOWS); if (SMOKE) WEIGHTS <- c(S0 = 1)

win_data <- function(wn) { W <- WINDOWS[[wn]]; D <- load_s46(unique(c(W$old, W$train, W$score))); D }
f_rds <- function(...) file.path(OUT, paste0(paste(..., sep = "_"), ".rds"))
choices_of <- function(wn) readRDS(f_rds("choices", wn))
train_rows <- function(D, wn) { W <- WINDOWS[[wn]]; ch <- choices_of(wn); which(D$yr %in% c(W$train, if (ch$old) W$old)) }
app_rows <- function(D, wn) which(D$yr == WINDOWS[[wn]]$score)
assert_frozen <- function(f = "manifest_freeze.txt") if (!file.exists(file.path(OUT, f))) stop("scoring before ", f, " is written")
method_of <- function() readRDS(f_rds("method"))$method

# B with default settings, early-stopped on the held-back rows: used only for the percentile and training-year choices
default_B_lift <- function(D, fit, val, pct) {
  P <- x_es(xpar(DEF_P, "binary:logistic", "logloss", TUNE_SEED), dm(D, fit, pct, TRUE), as.numeric(D$y[fit]), dm(D, val, pct, TRUE), as.numeric(D$y[val]))
  ef <- rows_err(D, fit); ev <- rows_err(D, val)
  m <- xgb.train(xpar(DEF_SIZE, "reg:gamma", "gamma-deviance", TUNE_SEED), dmx(dm(D, ef, pct, TRUE), D$dd[ef]), MAXR,
                 evals = list(v = dmx(dm(D, ev, pct, TRUE), D$dd[ev])), early_stopping_rounds = PAT, verbose = 0)
  sz <- predict(m, dmx(dm(D, val, pct, TRUE)), iterationrange = c(1, as.integer(xgb.attr(m, "best_iteration")) + 1L))
  val_lift(D, val, P$pv * sz) }

if (PHASE == "choices") for (wn in WN) {
  W <- WINDOWS[[wn]]; D <- win_data(wn); last <- tail(W$train, 1); old <- FALSE
  if (length(W$old)) { val <- which(D$yr == last)
    lN <- default_B_lift(D, which(D$yr %in% setdiff(W$train, last)), val, FALSE)
    lO <- default_B_lift(D, which(D$yr %in% c(W$old, setdiff(W$train, last))), val, FALSE)
    old <- lO >= 1.01 * lN; say("%s training years: FY%s alone %.4f, with FY%s %.4f -> old years %s", wn, setdiff(W$train, last), lN,
                                paste(W$old, collapse = "/"), lO, if (old) "IN" else "OUT") }
  tr <- which(D$yr %in% c(W$train, if (old) W$old)); sp <- make_splits(D, tr, "holdout", last)
  l0 <- default_B_lift(D, sp[[1]]$fit, sp[[1]]$val, FALSE); l1 <- default_B_lift(D, sp[[1]]$fit, sp[[1]]$val, TRUE)
  pct <- l1 >= 1.01 * l0; say("%s percentiles: out %.4f, in %.4f -> %s", wn, l0, l1, if (pct) "IN" else "OUT")
  saveRDS(list(old = old, pct = pct, lifts = c(new = if (length(W$old)) lN else NA, old = if (length(W$old)) lO else NA, pct_out = l0, pct_in = l1)), f_rds("choices", wn))
}

if (PHASE == "tuneB") for (wn in WN) for (meth in c("holdout", "cv")) {
  fn <- f_rds("S1B", wn, meth); if (file.exists(fn)) next
  D <- win_data(wn); ch <- choices_of(wn); tr <- train_rows(D, wn); sp <- make_splits(D, tr, meth, tail(WINDOWS[[wn]]$train, 1))
  say("%s tuneB %s: %d training rows, %d split(s)", wn, meth, length(tr), length(sp))
  saveRDS(list(P = tune_P(D, sp, tr, ch$pct, TRUE, label = paste("B P", meth)), S = tune_size(D, sp, tr, ch$pct, TRUE, label = paste("B size", meth))), fn)
}

if (PHASE %in% c("freeze", "freeze2")) {
  files <- c(file.path(here, c("s46_lib.R", "s46_models.R", "s46_run.R")), file.path(ROOT, "methods/modeling_findings_detailed.md"))
  if (PHASE == "freeze2") files <- c(files, list.files(OUT, "^(S1|S1B|choices|method)_?.*\\.rds$", full.names = TRUE), logf)
  h <- vapply(files, function(f) digest(file = f, algo = "sha256"), "")
  txt <- c(sprintf("%s written %s", PHASE, format(Sys.time())), sprintf("%s  %s", h, sub(paste0(ROOT, "/"), "", files)),
           sprintf("seeds %s; grids P %d, size %d, tweedie %d x p %s, ranger %d", paste(range(SEEDS), collapse = "-"), nrow(G_P), nrow(G_SIZE), nrow(G_TW),
                   paste(TW_P, collapse = "/"), nrow(G_RF)))
  writeLines(txt, file.path(OUT, paste0("manifest_", PHASE, ".txt"))); say("%s: %d files hashed", PHASE, length(files))
}

# ---------------- stage-2 fits ----------------
fit_cand <- function(D, wn, cand, seed, S1, meth) {
  tr <- train_rows(D, wn); app <- app_rows(D, wn); pct <- choices_of(wn)$pct
  sizeB <- function() cached(cpath(wn, meth, "Bsize", seed), fit_size(D, tr, app, S1$B$S, pct, TRUE, seed))
  metaC <- function() cached(cpath(wn, meth, "Cmeta", seed), fit_C(D, tr, app, S1$B$P, S1$C, pct, seed))
  switch(cand,
    A = fit_P(D, tr, app, S1$A$P, pct, FALSE, seed)$p * fit_size(D, tr, app, S1$A$S, pct, FALSE, seed),
    B = fit_P(D, tr, app, S1$B$P, pct, TRUE, seed)$p * sizeB(),
    G = fit_tw(D, tr, app, S1$G, pct, seed)$g,
    C = metaC()$p_app * sizeB(),
    GC = { g <- fit_tw(D, tr, app, S1$G, pct, seed, oof = TRUE); r <- rf_reg(D, tr, app, S1$GC$cfg, pct, RF_TREES, seed); meta_GC(D, tr, app, g$oof, r$oob, g$g, r$app) },
    E = { e <- fit_E(D, tr, app, S1$E, pct, seed, WINDOWS[[wn]]$prior_fy); saveRDS(e[c("offsets", "ratio")], cpath(wn, meth, "Einfo", seed)); e$ed },
    D = { ed <- numeric(length(app))
      for (h in c("1", "2-3", "4+")) { th <- tr[D$hh[tr] == h]; ah <- which(D$hh[app] == h); if (!length(ah)) next
        mc <- fit_C(D, th, app[ah], S1$D[[h]]$P, S1$D[[h]]$R, pct, seed); ed[ah] <- mc$p_app * fit_size(D, th, app[ah], S1$D[[h]]$S, pct, TRUE, seed) }; ed },
    F = { f <- fit_F_adjust(D, tr, app, metaC(), pct, seed); saveRDS(f$ntrees, cpath(wn, meth, "Fntrees", seed)); f$p * sizeB() }) }
run_fits <- function(cands, meth, s1_of) for (wn in WN) { D <- NULL
  for (cand in cands) for (seed in SEEDS) { fn <- cpath(wn, meth, "ed", cand, seed); if (file.exists(fn)) next
    if (is.null(D)) D <- win_data(wn); t0 <- Sys.time(); ed <- fit_cand(D, wn, cand, seed, s1_of(wn), meth); saveRDS(ed, fn)
    say("%s %s %s seed %d: %.1f min", wn, meth, cand, seed, as.numeric(difftime(Sys.time(), t0, units = "mins"))) } }

# ---------------- scoring ----------------
fastL <- function(sc, dv, keep, SS, buds = BUD) { tot <- numeric(length(buds))
  for (ix in SS) { n <- length(ix); kk <- pmax(1L, round(buds * n)); i2 <- ix[keep[ix]]; if (!length(i2)) next
    cs <- cumsum(dv[i2][order(-sc[i2], i2)]); tot <- tot + cs[pmin(kk, length(i2))] / kk }
  tot / length(SS) }
score_window <- function(wn, cands, meth) {
  assert_frozen(); D <- win_data(wn); app <- app_rows(D, wn); dv <- D$d[app]; yv <- D$y[app]; sv <- D$st[app]; SS <- split(seq_along(sv), sv)
  ED <- lapply(setNames(nm = cands), function(cn) sapply(SEEDS, function(s) readRDS(cpath(wn, meth, "ed", cn, s))))
  bag <- lapply(ED, rowMeans); k <- keep_of(D, app); m0 <- mean(dv[k])
  Lb <- function(sc, keep = k, d = dv, buds = BUD) mean(fastL(sc, d, keep, SS, buds)) / mean(dv[keep])
  pmcells <- local({ cont <- INPUTS[sapply(INPUTS, function(f) length(unique(D$X[app, f])) > 20)]; flag <- rep(FALSE, length(app))
    for (f in cont) { key <- paste(D$yr[app], pmin(D$hhn[app], 6), D$X[app, f]); n <- ave(rep(1, length(app)), key, FUN = sum)
      r <- ave(as.numeric(D$recreated[app]), key, FUN = mean); flag <- flag | (n >= 10 & r >= 0.8) }; flag })
  rows <- lapply(cands, function(cn) { sc <- bag[[cn]]
    # calibration: per-state level set to actual, then slope with state fixed effects; shape ratio within state
    mu <- rescale_by(sc[k], dv[k], sv[k]); slope <- coef(lm(dv[k] ~ mu + factor(sv[k])))[2]
    shp <- mean(sapply(SS, function(ix) { i2 <- ix[k[ix]]; if (sum(dv[i2]) <= 0) return(NA); top <- i2[order(-sc[i2])[seq_len(max(1, round(0.05 * length(i2))))]]
      (mean(dv[top]) / mean(dv[i2])) / (mean(sc[top]) / mean(sc[i2])) }), na.rm = TRUE)
    top5 <- unlist(lapply(SS, function(ix) { i2 <- ix[k[ix]]; i2[order(-sc[i2])[seq_len(min(max(1, round(0.05 * length(ix))), length(i2)))]] }))
    path <- sapply(c("income", "expense", "hhsize", "no_change", "other"), function(g) mean(fastL(sc, dv * (D$grp[app] == g), k, SS)) / m0)
    data.frame(window = wn, cand = cn, L = Lb(sc), L_seed_var = var(apply(ED[[cn]], 2, Lb)), prec5 = prec5(sc, yv, k, SS),
      L_5pct = mean(fastL(sc, dv, k, SS, 0.05)) / m0, L_full = Lb(sc, keep_of(D, app, filter = "full")), L_clean = Lb(sc, keep_of(D, app, filter = "clean")),
      L_pathrobust = Lb(sc, keep_of(D, app, filter = "pathrobust")), L_gap1 = Lb(sc, keep_of(D, app, 1)), L_gap20 = Lb(sc, keep_of(D, app, 20)),
      slope = slope, shape = shp, pointmass_top5 = mean(pmcells[top5]), t(setNames(path, paste0("path_", names(path)))), row.names = NULL) })
  tab <- do.call(rbind, rows)
  NB <- if (SMOKE) 100 else 2000; set.seed(4646)
  BS <- t(replicate(NB, { ii <- unlist(lapply(SS, function(ix) ix[sample.int(length(ix), replace = TRUE)]), use.names = FALSE)
    S2 <- split(seq_along(ii), sv[ii]); k2 <- k[ii]; d2 <- dv[ii]; m2 <- mean(d2[k2])
    vapply(cands, function(cn) mean(fastL(bag[[cn]][ii], d2, k2, S2)) / m2, 0) }))
  list(tab = tab, boot = BS) }

select_rule <- function(tab, boot, wins, w) {   # tab: rows per window x cand; returns the chosen candidate and its trace
  w <- w[wins] / sum(w[wins]); cands <- unique(tab$cand)
  L <- sapply(wins, function(x) setNames(tab$L[tab$window == x], tab$cand[tab$window == x])[cands]); if (!is.matrix(L)) L <- matrix(L, ncol = length(wins), dimnames = list(cands, wins))
  Q <- as.numeric(L %*% w); names(Q) <- cands
  pr <- sapply(wins, function(x) setNames(tab$prec5[tab$window == x], tab$cand[tab$window == x])[cands]); pr <- matrix(pr, ncol = length(wins), dimnames = list(cands, wins))
  sl <- sapply(wins, function(x) setNames(tab$slope[tab$window == x], tab$cand[tab$window == x])[cands]); sl <- matrix(sl, ncol = length(wins), dimnames = list(cands, wins))
  sh <- sapply(wins, function(x) setNames(tab$shape[tab$window == x], tab$cand[tab$window == x])[cands]); sh <- matrix(sh, ncol = length(wins), dimnames = list(cands, wins))
  prec_pool <- as.numeric(pr %*% w); names(prec_pool) <- cands
  elig <- prec_pool >= prec_pool["A"] - 0.015 & apply(sl >= 0.8 & sl <= 1.25, 1, all) & apply(sh >= 0.8 & sh <= 1.2, 1, all)
  t <- names(which.max(Q[elig])); ok <- names(Q)[elig & Q >= 0.99 * Q[t]]
  ch <- ok[order(RANK[ok], -Q[ok])][1]
  if (ch != "A" && all(L[ch, ] - L["A", ] < 0)) ch <- "A"
  list(choice = ch, top = t, Q = Q, eligible = elig, prec_pool = prec_pool) }

if (PHASE == "methodB") { assert_frozen()
  for (meth in c("holdout", "cv")) run_fits("B", meth, function(wn) list(B = readRDS(f_rds("S1B", wn, meth))))
  Q <- sapply(c("holdout", "cv"), function(meth) sum(sapply(WN, function(wn) { s <- score_window(wn, "B", meth); saveRDS(s, f_rds("scoreB", wn, meth)); s$tab$L }) * WEIGHTS[WN]))
  m <- if (Q["cv"] > Q["holdout"]) "cv" else "holdout"
  say("tuning method on B: Q holdout %.4f, Q cv %.4f -> %s", Q["holdout"], Q["cv"], m); saveRDS(list(method = m, Q = Q), f_rds("method")) }

if (PHASE == "stage1") { meth <- method_of()
  for (wn in WN) { fn <- f_rds("S1", wn); S1 <- if (file.exists(fn)) readRDS(fn) else list(B = readRDS(f_rds("S1B", wn, meth)))
    D <- win_data(wn); ch <- choices_of(wn); tr <- train_rows(D, wn); sp <- make_splits(D, tr, meth, tail(WINDOWS[[wn]]$train, 1)); pct <- ch$pct
    step <- function(nm, expr) if (is.null(S1[[nm]])) { t0 <- Sys.time(); S1[[nm]] <<- force(expr); saveRDS(S1, fn)
      say("%s stage1 %s done in %.1f min", wn, nm, as.numeric(difftime(Sys.time(), t0, units = "mins"))) }
    step("A", list(P = tune_P(D, sp, tr, pct, FALSE, label = "A P"), S = tune_size(D, sp, tr, pct, FALSE, label = "A size")))
    step("G", tune_tw(D, sp, tr, pct))
    step("C", tune_rf_C(D, sp, pct, S1$B$P))
    step("GC", tune_rf_GC(D, sp, pct, S1$G))
    step("E", tune_E(D, sp, tr, pct))
    step("D", lapply(setNames(nm = c("1", "2-3", "4+")), function(h) { kp <- D$hh == h
      P <- tune_P(D, sp, tr, pct, TRUE, keep = kp, label = paste("D P", h))
      list(P = P, S = tune_size(D, sp, tr, pct, TRUE, keep = kp, label = paste("D size", h)), R = tune_rf_C(D, sp, pct, P, keep = kp, label = paste("D ranger", h))) }))
  } }

if (PHASE == "fit") { assert_frozen("manifest_freeze2.txt"); meth <- method_of(); run_fits(CANDS, meth, function(wn) readRDS(f_rds("S1", wn))) }

if (PHASE == "score") { assert_frozen("manifest_freeze2.txt"); meth <- method_of()
  have <- function(wn, cn) all(file.exists(sapply(SEEDS, function(s) cpath(wn, meth, "ed", cn, s))))
  cands <- names(RANK)[sapply(names(RANK), function(cn) all(sapply(WN, have, cn = cn)))]; say("scoring %s", paste(cands, collapse = ","))
  S <- lapply(setNames(nm = WN), function(wn) score_window(wn, cands, meth)); tab <- do.call(rbind, lapply(S, `[[`, "tab"))
  write.csv(tab, file.path(OUT, "s46_window_scores.csv"), row.names = FALSE)
  sel <- select_rule(tab, NULL, WN, WEIGHTS)
  # SE of Q and of Q - Q_A: paired bootstrap per window + seed term
  vb <- function(f) sapply(WN, function(wn) var(f(S[[wn]]$boot)))
  se_Q <- sapply(cands, function(cn) sqrt(sum(WEIGHTS[WN]^2 * (vb(function(b) b[, cn]) + tab$L_seed_var[tab$cand == cn] / length(SEEDS)))))
  se_dA <- sapply(cands, function(cn) sqrt(sum(WEIGHTS[WN]^2 * (vb(function(b) b[, cn] - b[, "A"]) + (tab$L_seed_var[tab$cand == cn] + tab$L_seed_var[tab$cand == "A"]) / length(SEEDS)))))
  Qeq <- sapply(cands, function(cn) mean(tab$L[tab$cand == cn]))
  lowo <- if (length(WN) > 1) do.call(rbind, lapply(WN, function(out) { s <- select_rule(tab, NULL, setdiff(WN, out), WEIGHTS)
    LA <- tab$L[tab$window == out & tab$cand == "A"]; Lw <- tab$L[tab$window == out & tab$cand == s$choice]
    data.frame(left_out = out, chosen = s$choice, gain_vs_A = Lw / LA - 1) })) else NULL
  summ <- data.frame(cand = cands, rank = RANK[cands], Q = sel$Q[cands], se_Q = se_Q, Q_minus_A = sel$Q[cands] - sel$Q["A"], se_vs_A = se_dA,
                     Q_equal_weights = Qeq, prec5_pooled = sel$prec_pool[cands], eligible = sel$eligible[cands])
  write.csv(summ, file.path(OUT, "s46_selection.csv"), row.names = FALSE); if (!is.null(lowo)) write.csv(lowo, file.path(OUT, "s46_lowo.csv"), row.names = FALSE)
  say("choice %s (top %s)", sel$choice, sel$top); print(summ); print(lowo) }
