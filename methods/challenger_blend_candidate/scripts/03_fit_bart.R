# Challenger benchmark, step 2b: Bayesian additive regression trees (dbarts), the one learner fitted in R (2026-11-12).
#   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" 03_fit_bart.R [W1,W2,W3] [--smoke]
# Same structure as 02_fit.py: P (probit BART on all training rows) x size (BART on log error dollars of the
# training error rows, lognormal mean per draw). BART is run at its defaults (200 trees, k = 2); the only choice
# made on the held-back last training year is whether the size model takes the lben offset. Cut points are input
# quantiles (usequants) because the dollar inputs are skewed. Blank inputs are set to 0 (dbarts 0.9 takes no NA).
# Each chain's score is written as its own file (seeds 117, 118, ...), so the spread between chains is the
# learner's seed term at scoring. The test year's labels are blanked at load; only its inputs are used.
suppressMessages(library(dbarts))
HERE <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE))))
args <- commandArgs(TRUE); SMOKE <- "--smoke" %in% args; args <- setdiff(args, "--smoke")
WINS <- if (length(args)) strsplit(args[1], ",")[[1]] else c("W1", "W2", "W3")
OUT <- file.path(HERE, if (SMOKE) "smoke/scores" else "scores"); TUNE <- file.path(HERE, if (SMOKE) "smoke/tuning" else "tuning")
for (p in c(OUT, TUNE)) dir.create(p, showWarnings = FALSE, recursive = TRUE)
logf <- file.path(HERE, "logs", if (SMOKE) "03_fit_bart_smoke.log" else "03_fit_bart.log")
say <- function(...) { m <- sprintf("[%s] %s", format(Sys.time(), "%m-%d %H:%M:%S"), sprintf(...)); cat(m, "\n"); cat(m, "\n", file = logf, append = TRUE) }
META <- c("role", "yr", "st", "y", "d", "dd", "lben", "keep"); BUD <- c(0.02, 0.03, 0.05, 0.075, 0.10); SEED <- 117L
NTREE <- if (SMOKE) 50L else 200L; NCHAIN <- if (SMOKE) 2L else 4L; NSKIP <- if (SMOKE) 60L else 1000L; NDPOST <- if (SMOKE) 40L else 500L

# criteria, as in s46_lib.R
clampp <- function(p) pmin(pmax(p, 1e-6), 1 - 1e-6)
wll <- function(p, y, g) { z <- qlogis(clampp(p)); tot <- 0
  for (ix in split(seq_along(g), g)) { a <- tryCatch(suppressWarnings(coef(glm(y[ix] ~ 1, offset = z[ix], family = binomial))[1]), error = function(e) 0)
    if (!is.finite(a)) a <- 0; zz <- z[ix] + a; tot <- tot - sum(y[ix] * plogis(zz, log.p = TRUE) + (1 - y[ix]) * plogis(-zz, log.p = TRUE)) }
  tot / length(y) }
gamma_dev <- function(mu, yv) { mu <- pmax(mu, 1e-6); 2 * mean(-log(yv / mu) + (yv - mu) / mu) }
lift_of <- function(sc, dv, keep, st) { SS <- split(seq_along(st), st)
  mean(sapply(BUD, function(b) mean(vapply(SS, function(ix) { kk <- max(1L, round(b * length(ix))); ix2 <- ix[keep[ix]]
    sel <- ix2[order(-sc[ix2], ix2)[seq_len(min(kk, length(ix2)))]]; sum(dv[sel]) / kk }, 0)))) / mean(dv[keep]) }

load_win <- function(w) {
  d <- readRDS(file.path(HERE, "data", paste0(w, ".rds")))      # the exported frame itself, no text round trip
  if (SMOKE) {   # training years only, every 4th row; the last training year stands in for the test year
    d <- d[d$role == "train", ]; d <- d[seq(1, nrow(d), by = 4), ]; d$role <- ifelse(d$yr == max(d$yr), "test", "train")
    d <- rbind(d[d$role == "train", ], d[d$role == "test", ]) } else d[d$role == "test", c("y", "d", "dd")] <- NA   # test labels blanked at load
  feats <- setdiff(names(d), META); Xn <- as.matrix(d[feats]); Xn[is.na(Xn)] <- 0
  states <- sort(unique(d$st)); S <- sapply(states, function(u) as.numeric(d$st == u)); colnames(S) <- paste0("st_", make.names(states))
  tr <- which(d$role == "train"); te <- which(d$role == "test"); last <- max(d$yr[tr])
  fit <- tr[d$yr[tr] != last]; val <- tr[d$yr[tr] == last]
  if (!length(fit)) { stopifnot(SMOKE); k <- seq_along(tr) %% 3 == 0; fit <- tr[!k]; val <- tr[k] }
  list(w = w, d = d, X = cbind(Xn, S), tr = tr, te = te, fit = fit, val = val, y = d$y == 1, dd = d$dd, dn = d$d, lben = d$lben,
       keep = d$keep == 1, st = d$st, grp = paste(d$st, d$yr))
}
err_rows <- function(W, rows) rows[W$y[rows] & W$dd[rows] > 0]
gamma_off <- function(W, ref) { e <- err_rows(W, ref); g <- glm(dd ~ lben, family = Gamma("log"), data = data.frame(dd = W$dd[e], lben = W$lben[e]),
  control = glm.control(maxit = 100)); as.numeric(coef(g)[1] + coef(g)[2] * W$lben) }

# both return a matrix: one row per case in `app`, one column per chain
bart_P <- function(W, rows, app) {
  base <- mean(W$y[rows])
  m <- bart(W$X[rows, ], as.integer(W$y[rows]), W$X[app, ], ntree = NTREE, ndpost = NDPOST, nskip = NSKIP, nchain = NCHAIN, nthread = NCHAIN,
            usequants = TRUE, keeptrainfits = FALSE, combinechains = FALSE, verbose = FALSE, binaryOffset = qnorm(base), seed = SEED)
  stopifnot(identical(dim(m$yhat.test), c(NCHAIN, NDPOST, length(app))))
  p <- sapply(seq_len(NCHAIN), function(ch) colMeans(pnorm(m$yhat.test[ch, , ])))   # yhat.test is f(x) + binaryOffset on the probit scale
  if (mean(p) < base / 3 || mean(p) > base * 3) stop(sprintf("BART P mean %.4f is far from the training rate %.4f: check the offset", mean(p), base))
  p }
bart_S <- function(W, rows, app, off) {
  e <- err_rows(W, rows); o <- if (is.null(off)) numeric(nrow(W$X)) else off
  m <- bart(W$X[e, ], log(W$dd[e]) - o[e], W$X[app, ], ntree = NTREE, ndpost = NDPOST, nskip = NSKIP, nchain = NCHAIN, nthread = NCHAIN,
            usequants = TRUE, keeptrainfits = FALSE, combinechains = FALSE, verbose = FALSE, seed = SEED)
  stopifnot(identical(dim(m$yhat.test), c(NCHAIN, NDPOST, length(app))), identical(dim(m$sigma), c(NCHAIN, NDPOST)))
  sapply(seq_len(NCHAIN), function(ch) colMeans(exp(m$yhat.test[ch, , ] + m$sigma[ch, ]^2 / 2)) * exp(o[app])) }   # lognormal mean, draw by draw

say("=== bart on %s%s: %d trees, %d chains, %d burn-in + %d kept sweeps per chain", paste(WINS, collapse = ","), if (SMOKE) " (smoke)" else "", NTREE, NCHAIN, NSKIP, NDPOST)
for (w in WINS) {
  tf <- file.path(TUNE, sprintf("bart_%s.rds", w)); sf <- file.path(OUT, sprintf("bart_%s_%d.csv", w, SEED + seq_len(NCHAIN) - 1L))
  if (file.exists(tf) && all(file.exists(sf))) next
  W <- load_win(w)
  if (file.exists(tf)) S <- readRDS(tf) else {
    t0 <- Sys.time(); v <- W$val; ev <- err_rows(W, v); pv <- rowMeans(bart_P(W, W$fit, v))
    cand <- lapply(c(FALSE, TRUE), function(use) { sz <- rowMeans(bart_S(W, W$fit, v, if (use) gamma_off(W, W$fit) else NULL))
      list(off = use, dev = gamma_dev(sz[match(ev, v)], W$dd[ev]), sz = sz) })
    b <- cand[[which.min(sapply(cand, `[[`, "dev"))]]
    S <- list(S_off = b$off, val_wll = wll(pv, as.numeric(W$y[v]), W$grp[v]), val_gamma_dev = b$dev, val_gamma_dev_other = max(sapply(cand, `[[`, "dev")),
              val_lift = lift_of(pv * b$sz, W$dn[v], W$keep[v], W$st[v]), tune_minutes = as.numeric(difftime(Sys.time(), t0, units = "mins")))
    saveRDS(S, tf); write.csv(as.data.frame(S), sub("\\.rds$", ".csv", tf), row.names = FALSE)
    say("%s bart settings: size offset %s; held-back year: wll %.5f, gamma deviance %.4f, lift %.3f; %.1f min", w, S$S_off, S$val_wll, S$val_gamma_dev, S$val_lift, S$tune_minutes) }
  if (!all(file.exists(sf))) { t0 <- Sys.time()
    p <- bart_P(W, W$tr, W$te); size <- bart_S(W, W$tr, W$te, if (S$S_off) gamma_off(W, W$tr) else NULL)
    stopifnot(nrow(p) == length(W$te), all(is.finite(p)), all(is.finite(size)), all(size > 0))
    for (ch in seq_len(NCHAIN)) write.csv(data.frame(st = W$st[W$te], p = sprintf("%.17g", p[, ch]), size = sprintf("%.17g", size[, ch]),
                                                     ed = sprintf("%.17g", p[, ch] * size[, ch])), sf[ch], row.names = FALSE, quote = FALSE)
    ed <- rowMeans(p * size)
    say("%s bart: %.1f min, mean p %.4f, median size %.0f, chain-to-chain score correlation %.4f%s", w, as.numeric(difftime(Sys.time(), t0, units = "mins")),
        mean(p), median(size), cor(p[, 1] * size[, 1], p[, 2] * size[, 2]),
        if (SMOKE) sprintf(", stand-in lift %.3f", lift_of(ed, W$dn[W$te], W$keep[W$te], W$st[W$te])) else "") }
  rm(W); invisible(gc())
}
say("=== bart done")
