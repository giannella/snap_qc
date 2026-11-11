# Fitting pieces of the vote score, shared by the confirmation scripts. They
# are the exploration script's own definitions (explore_fy2019.R, run
# 2026-10-28), moved here unchanged so the confirmation refits exactly what the
# exploration froze.
MIN_BIN_ERRS <- 50L
HSZ_LEVELS <- c("1", "2", "3", "4", "5", "6+")
SIZE_FORMS <- list(means = ed ~ hh, benefit_size = ed ~ lben + hsz, benefit_size_vote = ed ~ lben + hsz + vote)
num <- function(x) { x <- suppressWarnings(as.numeric(as.character(x))); x[is.na(x)] <- 0; x }

pava <- function(y, w) {          # weighted isotonic (non-decreasing) regression
  vv <- numeric(0); wv <- numeric(0); lv <- integer(0)
  for (i in seq_along(y)) {
    vv <- c(vv, y[i]); wv <- c(wv, w[i]); lv <- c(lv, 1L)
    while (length(vv) > 1 && vv[length(vv) - 1] > vv[length(vv)]) {
      k <- length(vv); nw <- wv[k - 1] + wv[k]
      vv[k - 1] <- (vv[k - 1] * wv[k - 1] + vv[k] * wv[k]) / nw; wv[k - 1] <- nw; lv[k - 1] <- lv[k - 1] + lv[k]
      vv <- vv[-k]; wv <- wv[-k]; lv <- lv[-k]
    }
  }
  rep(vv, lv)
}
# bins over x pooled from the top until each holds MIN_BIN_ERRS errors (a bin
# short of that joins the next lower one), then isotonic in x
fit_iso1 <- function(x, y) {
  ux <- sort(unique(x)); j <- match(x, ux)
  n <- tabulate(j, length(ux)); e <- tabulate(j[y], length(ux))
  lo <- numeric(0); N <- numeric(0); E <- numeric(0); an <- 0; ae <- 0
  for (k in rev(seq_along(ux))) {
    an <- an + n[k]; ae <- ae + e[k]
    if (ae >= MIN_BIN_ERRS || k == 1L) { lo <- c(ux[k], lo); N <- c(an, N); E <- c(ae, E); an <- 0; ae <- 0 }
  }
  list(lo = lo, n = N, e = E, raw = E / N, rate = pava(E / N, N))
}
bin_of <- function(f, x) pmax(1L, findInterval(x, f$lo))
pred_iso1 <- function(f, x) f$rate[bin_of(f, x)]
fit_curve <- function(x, y, h) lapply(setNames(nm = HH_LEVELS), function(l) fit_iso1(x[h == l], y[h == l]))
pred_curve <- function(fc, x, h) { p <- numeric(length(x)); for (l in HH_LEVELS) { i <- h == l; p[i] <- pred_iso1(fc[[l]], x[i]) }; p }

size_data <- function(ed, hh, ben, hsz, vote)
  data.frame(ed = ed, hh = factor(hh, levels = HH_LEVELS), lben = log(pmax(ben, 1)),
             hsz = factor(ifelse(hsz >= 6, "6+", as.character(pmax(1, hsz))), levels = HSZ_LEVELS), vote = vote)
fit_size <- function(form, d, rows) {
  m <- glm(form, family = Gamma(link = "log"), data = d[rows[d$ed[rows] > 0], ], control = glm.control(maxit = 100))
  m[c("data", "model")] <- NULL
  m
}
