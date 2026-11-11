# §46 after selection. Usage: Rscript s46_post.R <reference | deploy>
#   reference: scores the option-D reference (predict_cases.R held-out files, SR_WINDOW fy2019 / fy2022 / fy2024)
#              with the frozen scoring code, beside A and B. Comparison only; it cannot be chosen.
#   deploy:    fits the chosen model B on FY2017-19 + FY2022-24 (test 3 chose the older years), settings re-chosen
#              by the stage-1 method (holdout) with FY2024 held back, bagged over the ten seeds, levels from the
#              FY2025 FNS payment error rates. Writes out/deploy_B_bundle.rds and out/deploy_B_levels.csv.
# The frozen driver s46_run.R is not run: only its function definitions are read from it.
here <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE))))
source(file.path(here, "s46_lib.R")); source(file.path(here, "s46_models.R"))
PHASE <- commandArgs(TRUE)[1]; stopifnot(PHASE %in% c("reference", "deploy"), !SMOKE)
WN <- names(WINDOWS)
for (e in parse(file.path(here, "s46_run.R")))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))) eval(e)
say("=== post phase %s", PHASE)
meth <- method_of()

if (PHASE == "reference") {
  tag <- c(W1 = "fy2019", W2 = "fy2022", W3 = "fy2024")
  for (wn in WN) {
    h <- readRDS(file.path(ROOT, "methods/case_predictions", sprintf("heldout_f1030_%s.rds", tag[[wn]])))
    D <- win_data(wn); app <- app_rows(D, wn)
    stopifnot(length(h$ie) == length(app), all(h$ie == D$y[app]), all(h$st == D$st[app]))
    for (s in SEEDS) saveRDS(h$p$expected_dollars, cpath(wn, meth, "ed", "OD", s))   # one fit; seed variance 0
  }
  cands <- c("A", "B", "OD")
  S <- lapply(setNames(nm = WN), function(wn) score_window(wn, cands, meth)); tab <- do.call(rbind, lapply(S, `[[`, "tab"))
  Q <- sapply(cands, function(cn) sum(tab$L[tab$cand == cn] * WEIGHTS[tab$window[tab$cand == cn]]))
  vb <- function(f) sapply(WN, function(wn) var(f(S[[wn]]$boot)))
  se_vs <- function(cn, ref) sqrt(sum(WEIGHTS[WN]^2 * (vb(function(b) b[, cn] - b[, ref]) +
    (tab$L_seed_var[tab$cand == cn] + tab$L_seed_var[tab$cand == ref]) / length(SEEDS))))
  pr <- sapply(cands, function(cn) sum(tab$prec5[tab$cand == cn] * WEIGHTS[tab$window[tab$cand == cn]]))
  summ <- data.frame(cand = cands, Q = Q, B_minus_cand = Q["B"] - Q, se = c(se_vs("B", "A"), NA, se_vs("B", "OD")), prec5_pooled = pr)
  write.csv(tab, file.path(OUT, "s46_reference_window_scores.csv"), row.names = FALSE)
  write.csv(summ, file.path(OUT, "s46_reference.csv"), row.names = FALSE)
  print(tab[, c("window", "cand", "L", "prec5", "L_full", "L_clean", "slope", "shape")], digits = 3); print(summ, digits = 4)
}

if (PHASE == "deploy") {
  ch <- choices_of("W3"); pct <- ch$pct; W <- WINDOWS$W3
  yrs <- c(if (ch$old) W$old, W$train, W$score)                          # FY2017-19 (if chosen) + FY2022-24
  D <- load_s46(yrs); tr <- seq_along(D$y)
  sp <- make_splits(D, tr, meth, "2024"); stopifnot(meth == "holdout")
  say("deploy: %d rows (%s), percentiles %s, method %s, FY2024 held back for settings", length(tr), paste(yrs, collapse = ","), pct, meth)
  T1 <- cached(file.path(OUT, "deploy_B_settings.rds"), list(P = tune_P(D, sp, tr, pct, TRUE, label = "deploy P"),
                                                             S = tune_size(D, sp, tr, pct, TRUE, label = "deploy size")))
  say("deploy settings: P %s, %d rounds; size %s, %d rounds", paste(names(T1$P$cfg), unlist(T1$P$cfg), collapse = " "), T1$P$nr,
      paste(names(T1$S$cfg), unlist(T1$S$cfg), collapse = " "), T1$S$nr)
  M <- dm(D, tr, pct, TRUE); b <- build_size(D, pct, TRUE, "any")(tr, T1$S$cfg, tr)
  off <- if (isTRUE(T1$S$cfg$off)) { e <- rows_err(D, tr); coef(glm(dd ~ lben, family = Gamma("log"), data = data.frame(dd = D$dd[e], lben = D$lben[e]),
                                                                  control = glm.control(maxit = 100))) } else NULL
  models <- lapply(SEEDS, function(s) { t0 <- Sys.time()
    mp <- x_fit(xpar(T1$P$cfg, "binary:logistic", "logloss", s), M, as.numeric(D$y[tr]), T1$P$nr)
    ms <- x_fit(xpar(T1$S$cfg, "reg:gamma", "gamma-deviance", s), b$M, b$lab, T1$S$nr, b$bm)
    say("deploy seed %d: %.1f min", s, as.numeric(difftime(Sys.time(), t0, units = "mins")))
    list(seed = s, p = xgb.save.raw(mp), size = xgb.save.raw(ms)) })
  # bagged predictions on FY2024 (each state's latest year) to set the levels
  a24 <- which(D$yr == "2024"); M24 <- dm(D, a24, pct, TRUE); bm24 <- if (!is.null(off)) as.numeric(off[1] + off[2] * D$lben[a24]) else NULL
  pp <- rowMeans(sapply(models, function(m) x_pred(xgb.load.raw(m$p), M24)))
  sz <- rowMeans(sapply(models, function(m) x_pred(xgb.load.raw(m$size), M24, bm24)))
  per_fn <- file.path(ROOT, "methods/vote_scoring/fns_payment_error_rates_fy2025.csv")
  per <- read.csv(per_fn, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM"); names(per) <- tolower(names(per))
  per <- setNames(per$payment_error_rate / 100, trimws(per$state)); stopifnot(all(unique(D$st) %in% names(per)))
  st24 <- D$st[a24]; iss <- D$issued[a24]
  lv <- do.call(rbind, lapply(sort(unique(st24)), function(s) { w <- which(st24 == s); r <- unname(per[s])
    mr <- sum(pp[w] * sz[w]) / sum(iss[w])
    po <- if (is.na(r)) NA else uniroot(function(d) sum(plogis(qlogis(pp[w]) + d) * sz[w]) / sum(iss[w]) - r, c(-15, 15), tol = 1e-8)$root
    data.frame(state = s, n_fy2024 = length(w), model_rate = mr, fns_rate_fy2025 = r, dollar_factor = r / mr, p_logit_offset = po,
               model_mean_p = mean(pp[w]), anchored_mean_p = if (is.na(po)) NA else mean(plogis(qlogis(pp[w]) + po))) }))
  git <- system2("git", c("-C", ROOT, "rev-parse", "HEAD"), stdout = TRUE)
  bundle <- list(model = paste("B (tuned xgboost P with state one-hot x boosted gamma size with state), §46",
                               if (basename(OUT) == "out_a2") "Amendment 2 (lben from the recreated benefit)" else "Amendment 1"),
                 inputs = colnames(M), base_inputs = INPUTS, states = D$ALLST, pct = pct, train_years = yrs, settings = T1[c("P", "S")],
                 size_offset_glm = off, cpi_by_year = D$cpi, models = models, levels = lv, fns_file = per_fn,
                 fns_sha256 = digest::digest(file = per_fn, algo = "sha256"),
                 frame_sha256 = digest::digest(file = FRAMES[["f1030"]], algo = "sha256"), staging_commit = git, built = Sys.time(),
                 note = paste("Rank within a state by expected $ = P x size. Expected $ on the FNS scale = model expected $ x dollar_factor",
                              "(order unchanged). Reported P = plogis(logit(P) + p_logit_offset). Levels were set on each state's FY2024 frame",
                              "rows; a state applying the model should re-solve both on its own caseload."))
  fn <- file.path(OUT, "deploy_B_bundle.rds"); saveRDS(bundle, fn)
  write.csv(lv, file.path(OUT, "deploy_B_levels.csv"), row.names = FALSE)
  say("deploy bundle -> %s (sha256 %s); frame %s; commit %s", fn, digest::digest(file = fn, algo = "sha256"), substr(bundle$frame_sha256, 1, 8), substr(git, 1, 7))
  print(summary(lv[, c("model_rate", "fns_rate_fy2025", "dollar_factor", "p_logit_offset")]))
  cat("states without an FY2025 rate:", paste(lv$state[is.na(lv$fns_rate_fy2025)], collapse = ", "), "\n")
}
