# §46 Amendment 1: stage-1 tuning and stage-2 fitting for every candidate. Sourced after s46_lib.R.
# Training targets: P learners use the over-tolerance label; size and dollar learners use deflated error $ (D$dd).
# lben never enters a tree: only as a GLM offset (base_margin) or as a term in GC's meta-model (E1).

grp_sy <- function(D, rows) paste(D$st[rows], D$yr[rows])
rows_err <- function(D, rows, dir = "any") rows[D$y[rows] & D$dd[rows] > 0 & switch(dir, any = TRUE, over = D$over[rows], under = D$under[rows])]
glm_gamma_off <- function(D, ref) { e <- rows_err(D, ref); g <- glm(dd ~ lben, family = Gamma("log"), data = data.frame(dd = D$dd[e], lben = D$lben[e]),
  control = glm.control(maxit = 100)); function(rows) as.numeric(coef(g)[1] + coef(g)[2] * D$lben[rows]) }
glm_qp_off <- function(D, ref) { g <- glm(dd ~ lben, family = quasipoisson, data = data.frame(dd = D$dd[ref], lben = D$lben[ref]))
  function(rows) as.numeric(coef(g)[1] + coef(g)[2] * D$lben[rows]) }
filt_sp <- function(sp, keep) lapply(sp, function(s) list(fit = s$fit[keep[s$fit]], val = s$val[keep[s$val]]))
n_fit_of <- function(sp) if (length(sp) == 1) length(sp[[1]]$fit) else NA

# ---------- generic xgboost grid search over a split ----------
# build(rows, cfg, ref) -> list(M, lab, bm, rows); crit(pv, b) -> number (lower is better)
tune_grid <- function(grid, sp, build, obj, metric, crit, extra = list(), label = "") {
  res <- lapply(seq_len(nrow(grid)), function(i) { cfg <- as.list(grid[i, , drop = FALSE])
    per <- lapply(sp, function(s) { f <- build(s$fit, cfg, s$fit); v <- build(s$val, cfg, s$fit)
      r <- x_es(xpar(cfg, obj, metric, TUNE_SEED, extra), f$M, f$lab, v$M, v$lab, f$bm, v$bm)
      list(best = r$best, pv = r$pv, b = v, nfit = length(f$rows)) })
    cr <- mean(sapply(per, function(o) crit(o$pv, o$b)))
    list(cfg = cfg, crit = cr, best = sapply(per, `[[`, "best"), nfit = sapply(per, `[[`, "nfit"), per = per) })
  tab <- do.call(rbind, lapply(res, function(r) data.frame(r$cfg, crit = r$crit, best = paste(r$best, collapse = "/"))))
  say("  tune %s: %s", label, paste(capture.output(print(tab[order(tab$crit), ][1:min(3, nrow(tab)), ], row.names = FALSE)), collapse = " | "))
  list(res = res, tab = tab, i = which.min(tab$crit)) }
rounds_of <- function(r, n_all) if (length(r$best) == 1) as.integer(ceiling(r$best * n_all / r$nfit)) else as.integer(ceiling(mean(r$best) * 5 / 4))

# ---------- P (binary xgboost) ----------
build_P <- function(D, pct, state) function(rows, cfg, ref) list(M = dm(D, rows, pct, state), lab = as.numeric(D$y[rows]), bm = NULL, rows = rows)
tune_P <- function(D, sp, tr, pct, state, keep = NULL, label = "P") {
  if (!is.null(keep)) { sp <- filt_sp(sp, keep); tr <- tr[keep[tr]] }
  t <- tune_grid(G_P, sp, build_P(D, pct, state), "binary:logistic", "logloss", function(pv, b) wll(pv, D$y[b$rows], grp_sy(D, b$rows)), label = label)
  r <- t$res[[t$i]]; list(cfg = r$cfg, nr = rounds_of(r, length(tr)), per = r$per, tab = t$tab) }

# ---------- size (boosted gamma on error rows, optional lben offset) ----------
build_size <- function(D, pct, state, dir) function(rows, cfg, ref) { e <- rows_err(D, rows, dir)
  bm <- if (isTRUE(cfg$off)) glm_gamma_off_dir(D, ref, dir)(e) else NULL
  list(M = dm(D, e, pct, state), lab = D$dd[e], bm = bm, rows = e) }
glm_gamma_off_dir <- function(D, ref, dir) { e <- rows_err(D, ref, dir); g <- glm(dd ~ lben, family = Gamma("log"),
  data = data.frame(dd = D$dd[e], lben = D$lben[e]), control = glm.control(maxit = 100)); function(rows) as.numeric(coef(g)[1] + coef(g)[2] * D$lben[rows]) }
tune_size <- function(D, sp, tr, pct, state, dir = "any", keep = NULL, label = "size") {
  if (!is.null(keep)) { sp <- filt_sp(sp, keep); tr <- tr[keep[tr]] }
  t <- tune_grid(G_SIZE, sp, build_size(D, pct, state, dir), "reg:gamma", "gamma-deviance", function(pv, b) gamma_dev(pv, b$lab), label = label)
  r <- t$res[[t$i]]; list(cfg = r$cfg, nr = rounds_of(r, length(rows_err(D, tr, dir))), tab = t$tab) }

# ---------- Tweedie (G) ----------
build_tw <- function(D, pct) function(rows, cfg, ref) list(M = dm(D, rows, pct, TRUE), lab = D$dd[rows],
  bm = if (isTRUE(cfg$off)) glm_qp_off(D, ref)(rows) else NULL, rows = rows)
tune_tw <- function(D, sp, tr, pct) {
  per_p <- lapply(TW_P, function(p) { t <- tune_grid(G_TW, sp, build_tw(D, pct), "reg:tweedie", sprintf("tweedie-nloglik@%s", p),
      function(pv, b) tw_dev(pv, b$lab, grp_sy(D, b$rows), p), extra = list(tweedie_variance_power = p), label = sprintf("tweedie p=%s", p))
    r <- t$res[[t$i]]; lift <- mean(sapply(r$per, function(o) val_lift(D, o$b$rows, o$pv)))
    list(p = p, cfg = r$cfg, nr = rounds_of(r, length(tr)), per = r$per, lift = lift) })
  lifts <- sapply(per_p, `[[`, "lift"); say("  tweedie p by held-back lift: %s", paste(sprintf("%s:%.4f", TW_P, lifts), collapse = " "))
  per_p[[which.max(lifts)]] }

# ---------- ranger + meta stacks (C, D, GC) ----------
rf_prob <- function(D, fit, app, cfg, pct, trees, seed) {
  m <- ranger(x = rf_df(D, fit, pct), y = factor(D$y[fit], levels = c(FALSE, TRUE)), probability = TRUE, num.trees = trees,
    mtry = cfg$mtry, min.node.size = cfg$min.node.size, sample.fraction = cfg$sample.fraction, max.depth = cfg$max.depth,
    respect.unordered.factors = "order", num.threads = NTH, seed = seed)
  list(oob = m$predictions[, "TRUE"], app = predict(m, rf_df(D, app, pct), num.threads = NTH)$predictions[, "TRUE"]) }
rf_reg <- function(D, fit, app, cfg, pct, trees, seed) {
  m <- ranger(x = rf_df(D, fit, pct), y = D$dd[fit], num.trees = trees, mtry = cfg$mtry, min.node.size = cfg$min.node.size,
    sample.fraction = cfg$sample.fraction, max.depth = cfg$max.depth, respect.unordered.factors = "order", num.threads = NTH, seed = seed)
  list(oob = m$predictions, app = predict(m, rf_df(D, app, pct), num.threads = NTH)$predictions) }
oof_x <- function(par, M, lab, nr, seed, bm = NULL) { set.seed(seed); fo <- sample(rep_len(1:5, nrow(M))); o <- numeric(nrow(M))
  for (k in 1:5) { i <- fo != k; m <- x_fit(par, M[i, , drop = FALSE], lab[i], nr, if (is.null(bm)) NULL else bm[i])
    o[!i] <- x_pred(m, M[!i, , drop = FALSE], if (is.null(bm)) NULL else bm[!i]) }; o }
# logistic meta with state fixed effects (sum contrasts); returns slopes-only P and the FE-included margin
meta_C <- function(D, tr, app, lx_tr, lr_tr, lx_app, lr_app) {
  lev <- sort(unique(D$st[tr])); dat <- data.frame(y = as.numeric(D$y[tr]), lx = lx_tr, lr = lr_tr, stf = factor(D$st[tr], levels = lev))
  m <- if (length(lev) > 1) glm(y ~ lx + lr + stf, family = binomial, data = dat, contrasts = list(stf = "contr.sum")) else glm(y ~ lx + lr, family = binomial, data = dat)
  b <- coef(m); fe <- setNames(rep(0, length(lev)), lev)
  if (length(lev) > 1) { cf <- b[grepl("^stf", names(b))]; cf[is.na(cf)] <- 0; fe[] <- c(cf, -sum(cf)) }
  lp_app <- b[1] + b["lx"] * lx_app + b["lr"] * lr_app; fa <- fe[D$st[app]]; fa[is.na(fa)] <- 0
  list(p_app = plogis(lp_app), lp_app_fe = as.numeric(lp_app + fa), lp_tr_fe = as.numeric(predict(m)), coef = b[c("lx", "lr")]) }
meta_GC <- function(D, tr, app, og, orr, g_app, r_app) {
  lg <- function(z) log(pmax(z, 1e-3))
  m <- glm(dd ~ a + b + lb, family = quasipoisson, data = data.frame(dd = D$dd[tr], a = lg(og), b = lg(orr), lb = D$lben[tr]))
  as.numeric(predict(m, data.frame(a = lg(g_app), b = lg(r_app), lb = D$lben[app]), type = "response")) }

tune_rf_C <- function(D, sp, pct, Pt, state = TRUE, keep = NULL, label = "ranger C") {
  if (!is.null(keep)) sp <- filt_sp(sp, keep)
  par <- xpar(Pt$cfg, "binary:logistic", "logloss", TUNE_SEED)
  base <- lapply(seq_along(sp), function(j) { s <- sp[[j]]; M <- dm(D, s$fit, pct, state)
    list(lx_fit = lgt(oof_x(par, M, as.numeric(D$y[s$fit]), Pt$per[[j]]$best, TUNE_SEED)), lx_val = lgt(Pt$per[[j]]$pv)) })
  crit <- sapply(seq_len(nrow(G_RF)), function(i) { cfg <- as.list(G_RF[i, ])
    mean(sapply(seq_along(sp), function(j) { s <- sp[[j]]; r <- rf_prob(D, s$fit, s$val, cfg, pct, RF_TUNE_TREES, TUNE_SEED)
      mc <- meta_C(D, s$fit, s$val, base[[j]]$lx_fit, lgt(r$oob), base[[j]]$lx_val, lgt(r$app))
      wll(mc$p_app, D$y[s$val], grp_sy(D, s$val)) })) })
  tab <- cbind(G_RF, crit = crit); say("  tune %s: best %s", label, paste(capture.output(print(tab[order(crit), ][1:min(3, nrow(tab)), ], row.names = FALSE)), collapse = " | "))
  list(cfg = as.list(G_RF[which.min(crit), ]), tab = tab) }

tune_rf_GC <- function(D, sp, pct, Gt) {
  par <- xpar(Gt$cfg, "reg:tweedie", sprintf("tweedie-nloglik@%s", Gt$p), TUNE_SEED, list(tweedie_variance_power = Gt$p))
  bt <- build_tw(D, pct)
  base <- lapply(seq_along(sp), function(j) { s <- sp[[j]]; f <- bt(s$fit, Gt$cfg, s$fit)
    list(og = oof_x(par, f$M, f$lab, Gt$per[[j]]$best, TUNE_SEED, f$bm), gv = Gt$per[[j]]$pv) })
  crit <- sapply(seq_len(nrow(G_RF)), function(i) { cfg <- as.list(G_RF[i, ])
    mean(sapply(seq_along(sp), function(j) { s <- sp[[j]]; r <- rf_reg(D, s$fit, s$val, cfg, pct, RF_TUNE_TREES, TUNE_SEED)
      mu <- meta_GC(D, s$fit, s$val, base[[j]]$og, r$oob, base[[j]]$gv, r$app); pois_dev(mu, D$dd[s$val], grp_sy(D, s$val)) })) })
  tab <- cbind(G_RF, crit = crit); say("  tune ranger GC: best %s", paste(capture.output(print(tab[order(crit), ][1:min(3, nrow(tab)), ], row.names = FALSE)), collapse = " | "))
  list(cfg = as.list(G_RF[which.min(crit), ]), tab = tab) }

# ---------- E: multinomial ----------
cls_of <- function(D, rows) ifelse(D$over[rows], 2L, ifelse(D$under[rows], 3L, 1L))
as_mat3 <- function(p) if (is.matrix(p)) p else matrix(p, ncol = 3, byrow = TRUE)
tune_E <- function(D, sp, tr, pct) {
  build <- function(rows, cfg, ref) list(M = dm(D, rows, pct, TRUE), lab = cls_of(D, rows) - 1L, bm = NULL, rows = rows)
  t <- tune_grid(G_P, sp, build, "multi:softprob", "mlogloss", function(pv, b) wmll(log(pmax(as_mat3(pv), 1e-9)), cls_of(D, b$rows), grp_sy(D, b$rows)),
                 extra = list(num_class = 3), label = "E multinomial")
  r <- t$res[[t$i]]
  list(P = list(cfg = r$cfg, nr = rounds_of(r, length(tr))),
       so = tune_size(D, sp, tr, pct, TRUE, "over", label = "E size over"), su = tune_size(D, sp, tr, pct, TRUE, "under", label = "E size under")) }

# ====================== stage-2 fits (one seed) ======================
fit_P <- function(D, tr, app, Pt, pct, state, seed, oof = FALSE) { par <- xpar(Pt$cfg, "binary:logistic", "logloss", seed)
  M <- dm(D, tr, pct, state); lab <- as.numeric(D$y[tr]); m <- x_fit(par, M, lab, Pt$nr)
  list(p = x_pred(m, dm(D, app, pct, state)), oof = if (oof) oof_x(par, M, lab, Pt$nr, seed) else NULL) }
fit_size <- function(D, tr, app, St, pct, state, seed, dir = "any") { b <- build_size(D, pct, state, dir)(tr, St$cfg, tr)
  m <- x_fit(xpar(St$cfg, "reg:gamma", "gamma-deviance", seed), b$M, b$lab, St$nr, b$bm)
  bm <- if (isTRUE(St$cfg$off)) glm_gamma_off_dir(D, tr, dir)(app) else NULL
  x_pred(m, dm(D, app, pct, state), bm) }
fit_tw <- function(D, tr, app, Gt, pct, seed, oof = FALSE) { bt <- build_tw(D, pct); f <- bt(tr, Gt$cfg, tr); a <- bt(app, Gt$cfg, tr)
  par <- xpar(Gt$cfg, "reg:tweedie", sprintf("tweedie-nloglik@%s", Gt$p), seed, list(tweedie_variance_power = Gt$p))
  m <- x_fit(par, f$M, f$lab, Gt$nr, f$bm)
  list(g = x_pred(m, a$M, a$bm), oof = if (oof) oof_x(par, f$M, f$lab, Gt$nr, seed, f$bm) else NULL) }
fit_C <- function(D, tr, app, Pt, Rt, pct, seed) { P <- fit_P(D, tr, app, Pt, pct, TRUE, seed, oof = TRUE)
  r <- rf_prob(D, tr, app, Rt$cfg, pct, RF_TREES, seed)
  meta_C(D, tr, app, lgt(P$oof), lgt(r$oob), lgt(P$p), lgt(r$app)) }
# F: per-state boosted adjustment on C's FE-included margin; rounds by within-state 5-fold CV, one-SE, at most 50
fit_F_adjust <- function(D, tr, app, mc, pct, seed) {
  par <- list(objective = "binary:logistic", eval_metric = "logloss", nthread = min(NTH, 4), seed = seed, max_depth = 2, eta = 0.05,
              min_child_weight = 10, lambda = 10, subsample = 0.8)
  out <- mc$lp_app_fe; ntrees <- c()
  for (u in unique(D$st[app])) { it <- which(D$st[tr] == u); ia <- which(D$st[app] == u)
    if (length(it) < 100 || sum(D$y[tr][it]) < 10) { ntrees[u] <- 0; next }
    M <- dm(D, tr[it], pct, FALSE); yv <- as.numeric(D$y[tr][it]); bm <- mc$lp_tr_fe[it]
    set.seed(seed); fo <- sample(rep_len(1:5, length(it)))
    L <- sapply(1:5, function(k) { i <- fo != k
      m <- xgb.train(par, dmx(M[i, , drop = FALSE], yv[i], bm[i]), nrounds = 50, evals = list(v = dmx(M[!i, , drop = FALSE], yv[!i], bm[!i])), verbose = 0)
      el <- attributes(m)$evaluation_log; if (is.null(el)) el <- m$evaluation_log
      c(ll(plogis(bm[!i]), yv[!i]), el$v_logloss) })            # row 1 = base margin, rows 2..51 = rounds 1..50
    mu <- rowMeans(L); se <- apply(L, 1, sd) / sqrt(5); b <- which.min(mu)
    r <- if (b == 1) 0 else min(which(mu <= mu[b] + se[b])) - 1
    ntrees[u] <- r; if (r == 0) next
    m <- x_fit(par, M, yv, r, bm); out[ia] <- x_pred(m, dm(D, app[ia], pct, FALSE), mc$lp_app_fe[ia], margin = TRUE) }
  list(p = plogis(out), ntrees = ntrees) }
ll <- function(p, y) { p <- clampp(p); -mean(y * log(p) + (1 - y) * log(1 - p)) }

# E: multinomial + per-direction size + FNS direction priors (rescaled, offsets capped at +/-1 logit)
fns_tab <- function(fy) read.csv(file.path(ROOT, sprintf("additional_data/fns_payment_error_rates_fy%s.csv", fy)), stringsAsFactors = FALSE)
fit_E <- function(D, tr, app, Et, pct, seed, prior_fy) {
  m <- x_fit(xpar(Et$P$cfg, "multi:softprob", "mlogloss", seed, list(num_class = 3)), dm(D, tr, pct, TRUE), cls_of(D, tr) - 1L, Et$P$nr)
  Z <- as_mat3(x_pred(m, dm(D, app, pct, TRUE), margin = TRUE))
  so <- fit_size(D, tr, app, Et$so, pct, TRUE, seed, "over"); su <- fit_size(D, tr, app, Et$su, pct, TRUE, seed, "under")
  # rescale: national sample $ rate / national FNS rate, per direction, over the training years
  tys <- sort(unique(D$yr[tr])); nat <- do.call(rbind, lapply(tys, function(fy) { f <- fns_tab(fy); f[f$state == "United States", c("overpayment_rate", "underpayment_rate")] }))
  samp <- c(sum(D$d[tr][D$over[tr]]), sum(D$d[tr][D$under[tr]])) / sum(D$issued[tr])
  ratio <- samp / (colMeans(nat) / 100)
  pr <- fns_tab(prior_fy); off <- matrix(0, length(app), 2); stats <- list()
  for (u in unique(D$st[app])) { ia <- which(D$st[app] == u); w <- match(u, pr$state); if (is.na(w)) next
    tgt <- c(pr$overpayment_rate[w], pr$underpayment_rate[w]) / 100 * ratio; a <- c(0, 0); iss <- sum(D$issued[app][ia])
    for (it in 1:30) { e <- exp(cbind(Z[ia, 1], Z[ia, 2] + a[1], Z[ia, 3] + a[2])); p <- e / rowSums(e)
      cur <- c(sum(p[, 2] * so[ia]), sum(p[, 3] * su[ia])) / iss; a <- a + log(pmax(tgt, 1e-6) / pmax(cur, 1e-9)) }
    a <- pmin(pmax(a, -1), 1); off[ia, ] <- matrix(a, length(ia), 2, byrow = TRUE); stats[[u]] <- a }
  e <- exp(cbind(Z[, 1], Z[, 2] + off[, 1], Z[, 3] + off[, 2])); p <- e / rowSums(e)
  list(ed = p[, 2] * so + p[, 3] * su, p_any = p[, 2] + p[, 3], offsets = do.call(rbind, stats), ratio = ratio) }
