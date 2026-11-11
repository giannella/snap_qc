# §46 selection study, Amendment 1 (modeling_findings_detailed.md, approved 2026-11-01).
# Shared library: data, windows, learners, stage-1 criteria and the stage-2 reading.
# Nothing here writes to state_delivery_lists/. Outputs go to methods/case_predictions/s46/out[_smoke]/.
ROOT <- "C:/Users/ericg/snap_qc_v27"
Sys.setenv(SR_ROOT = ROOT, SR_WINDOW = "fy2024")
source(file.path(ROOT, "methods/v270_seed_replicates/common.R"))   # VOCAB19, prep_features, income_pile_rows; setwd(ROOT)
suppressMessages({ library(xgboost); library(ranger); library(digest) })

SMOKE <- identical(Sys.getenv("S46_SMOKE"), "1")
OUT <- file.path(ROOT, "methods/case_predictions/s46", Sys.getenv("S46_OUT", if (SMOKE) "out_smoke" else "out"))   # Amendment 2 runs into out_a2
dir.create(file.path(OUT, "cache"), showWarnings = FALSE, recursive = TRUE)
NTH <- as.integer(Sys.getenv("S46_THREADS", "16"))
logf <- file.path(OUT, "s46_log.txt")
say <- function(...) { m <- sprintf("[%s] %s", format(Sys.time(), "%m-%d %H:%M:%S"), sprintf(...)); cat(m, "\n"); cat(m, "\n", file = logf, append = TRUE) }

INPUTS <- setdiff(VOCAB19, "count_divisible_by_100"); stopifnot(length(INPUTS) == 18)
PCT_FIELDS <- c("gross_by_hh_size", "shelter_expenses_by_hh_size", "total_deductions_by_hh_size")  # no issued-benefit percentile (E1)
SEEDS <- if (SMOKE) 117:118 else 117:126
TUNE_SEED <- 117L
BUD <- c(0.02, 0.03, 0.05, 0.075, 0.10)
GAP_CUT <- as.numeric(Sys.getenv("S46_GAP", "5"))
WEIGHTS <- c(W1 = 0.20, W2 = 0.40, W3 = 0.40)
RANK <- c(A = 1, B = 2, G = 2, C = 3, GC = 3, E = 3, D = 4, F = 5)

WINDOWS <- if (SMOKE) list(
  S0 = list(train = "2017", old = character(0), score = "2018", prior_fy = "2017")) else list(
  W1 = list(train = c("2017", "2018"), old = character(0), score = "2019", prior_fy = "2017"),
  W2 = list(train = c("2017", "2018", "2019"), old = character(0), score = "2022", prior_fy = "2019"),
  W3 = list(train = c("2022", "2023"), old = c("2017", "2018", "2019"), score = "2024", prior_fy = "2022"))

# ---------------- grids (Amendment 1, stage 1) ----------------
G_P <- expand.grid(max_depth = c(3, 4, 5, 6), min_child_weight = c(5, 25), colsample_bytree = c(0.5, 0.8))
G_SIZE <- expand.grid(max_depth = c(2, 3, 4), min_child_weight = c(5, 25), off = c(FALSE, TRUE))
G_TW <- expand.grid(max_depth = c(3, 4, 5, 6), min_child_weight = c(5, 25), colsample_bytree = c(0.5, 0.8), off = c(FALSE, TRUE))
TW_P <- c(1.1, 1.2, 1.35, 1.5)
G_RF <- expand.grid(mtry = c(4, 8, 14), min.node.size = c(25, 100), sample.fraction = c(0.5, 0.8), max.depth = c(0, 12))
FIX <- list(eta = 0.03, subsample = 0.8, lambda = 5)
MAXR <- 4000L; PAT <- 100L; RF_TUNE_TREES <- 500L; RF_TREES <- 1000L
if (SMOKE) { G_P <- G_P[c(3, 13), ]; G_SIZE <- G_SIZE[c(2, 8), ]; G_TW <- G_TW[c(3, 29), ]; TW_P <- c(1.2, 1.5)
  G_RF <- G_RF[c(2, 20), ]; MAXR <- 400L; PAT <- 30L; RF_TUNE_TREES <- 60L; RF_TREES <- 80L }
DEF_P <- list(max_depth = 5, min_child_weight = 5, colsample_bytree = 0.8)       # used only for the percentile and training-year choices
DEF_SIZE <- list(max_depth = 3, min_child_weight = 5, off = FALSE)

# ---------------- data ----------------
load_s46 <- function(years) {
  if (SMOKE) stopifnot(all(years %in% c("2017", "2018")))
  reg <- readRDS(FRAMES[["f1030"]]); stopifnot(nrow(reg) == 231602L)
  reg <- reg[as.character(reg$fiscal_year) %in% years, ]
  pf <- prep_features(reg, VOCAB19); a <- pf$data; stopifnot(nrow(a) == nrow(reg)); rm(reg)
  st <- as.character(a$state); yr <- as.character(a$fiscal_year)
  y <- !is.na(a$over_threshold) & a$over_threshold != 0
  d <- ifelse(y, abs(ifelse(is.na(a$total_error_amount), 0, a$total_error_amount)), 0)    # nominal $, used for scoring
  cpi <- tapply(a$rawearn / a$rawearn_nominal, yr, median, na.rm = TRUE)                    # one factor per fiscal year
  dd <- d * cpi[yr]                                                                         # deflated $, used for training targets
  over <- y & a$bendiff > 0; under <- y & a$bendiff < 0; stopifnot(!anyNA(over), all(over | under | !y))
  gap <- abs(a$rawben - a$raw_benefit_amount); gap[is.na(gap)] <- 0
  mm <- a$rawben >= a$benmax & a$rawben_uncapped < a$benmax; mm[is.na(mm)] <- FALSE
  pile <- income_pile_rows(a); pile[is.na(pile)] <- FALSE
  notes <- as.character(a$correctednotes); notes[is.na(notes) | notes == ""] <- "no_change"
  step7 <- grepl("prior_year_tables", notes)
  grp <- ifelse(notes == "no_change", "no_change", ifelse(grepl("hhsize", notes), "hhsize", ifelse(grepl("earn", notes), "income",
         ifelse(grepl("rent|util|med|shelter", notes), "expense", "other"))))
  X <- as.matrix(a[INPUTS]); storage.mode(X) <- "double"
  P <- sapply(PCT_FIELDS, function(f) ave(a[[f]], st, yr, FUN = function(v) rank(v, na.last = "keep", ties.method = "average") / sum(!is.na(v))))
  colnames(P) <- paste0("pct_", PCT_FIELDS)
  ALLST <- sort(unique(st))
  S <- sapply(ALLST, function(u) as.numeric(st == u)); colnames(S) <- paste0("st_", make.names(ALLST))
  hh <- ifelse(a$HH_size_n <= 1, "1", ifelse(a$HH_size_n <= 3, "2-3", "4+"))     # D's strata on the input, not the certified size
  list(st = st, yr = yr, y = y, d = d, dd = as.numeric(dd), over = over, under = under, gap = gap, mm = mm, pile = pile,
       step7 = step7, grp = grp, recreated = notes != "no_change", X = X, P = P, S = S, ALLST = ALLST, hh = hh,
       # lben is an input (size offset, combiner term), so it uses the recreated benefit like the other inputs;
       # RAWBEN beside recreated inputs reads the recreation gap (Amendment 2). issued stays RAWBEN: a denominator only.
       lben = log1p(pmax(a$raw_benefit_amount, 0)), issued = pmax(a$rawben, 0), hhn = a$HH_size_n, cpi = cpi)
}

# design matrix for a learner: base inputs, optional percentiles, optional state one-hot
dm <- function(D, rows, pct, state) { M <- D$X[rows, , drop = FALSE]
  if (pct) M <- cbind(M, D$P[rows, , drop = FALSE]); if (state) M <- cbind(M, D$S[rows, , drop = FALSE]); M }
rf_df <- function(D, rows, pct) { f <- as.data.frame(dm(D, rows, pct, FALSE)); f$state <- factor(D$st[rows], levels = D$ALLST); f }

# ---------------- splits ----------------
# split = list of (fit, val) row pairs within a window's training rows. Holdout: last training year held back
# (smoke: 20% of states held back). CV: 5 folds of state-years.
make_splits <- function(D, tr_rows, method, last_year) {
  if (method == "holdout") {
    if (SMOKE) { set.seed(46); hs <- sample(unique(D$st[tr_rows]), round(0.2 * length(unique(D$st[tr_rows]))))
      return(list(list(fit = tr_rows[!D$st[tr_rows] %in% hs], val = tr_rows[D$st[tr_rows] %in% hs]))) }
    return(list(list(fit = tr_rows[D$yr[tr_rows] != last_year], val = tr_rows[D$yr[tr_rows] == last_year]))) }
  g <- paste(D$st[tr_rows], D$yr[tr_rows]); ug <- sort(unique(g)); set.seed(46); fo <- sample(rep_len(1:5, length(ug)))[match(g, ug)]
  lapply(1:5, function(k) list(fit = tr_rows[fo != k], val = tr_rows[fo == k]))
}
round_scale <- function(best, sp, n_all) if (length(sp) == 1) as.integer(ceiling(best[1] * n_all / length(sp[[1]]$fit))) else
  as.integer(ceiling(mean(best) * 5 / 4))

# ---------------- xgboost wrapper ----------------
xpar <- function(cfg, obj, metric, seed, extra = list()) c(list(objective = obj, eval_metric = metric, nthread = NTH, seed = seed),
  FIX, cfg[intersect(names(cfg), c("max_depth", "min_child_weight", "colsample_bytree"))], extra)
dmx <- function(M, lab = NULL, bm = NULL) { if (is.null(bm)) xgb.DMatrix(M, label = lab) else xgb.DMatrix(M, label = lab, base_margin = bm) }
x_es <- function(par, Mf, yf, Mv, yv, bmf = NULL, bmv = NULL) {        # early-stopped fit; returns best rounds and val preds at best
  m <- xgb.train(par, dmx(Mf, yf, bmf), nrounds = MAXR, evals = list(v = dmx(Mv, yv, bmv)), early_stopping_rounds = PAT, verbose = 0)
  best <- as.integer(xgb.attr(m, "best_iteration")) + 1L
  list(best = best, pv = predict(m, dmx(Mv, NULL, bmv), iterationrange = c(1, best))) }
x_fit <- function(par, M, yv, nr, bm = NULL) xgb.train(par, dmx(M, yv, bm), nrounds = nr, verbose = 0)
x_pred <- function(m, M, bm = NULL, margin = FALSE) predict(m, dmx(M, NULL, bm), outputmargin = margin)

# ---------------- criteria ----------------
clampp <- function(p) pmin(pmax(p, 1e-6), 1 - 1e-6)
lgt <- function(p) qlogis(clampp(p))
# within-state(-year) log loss: one intercept shift per group fitted on the validation rows
wll <- function(p, y, g) { z <- lgt(p); tot <- 0
  for (ix in split(seq_along(g), g)) { a <- tryCatch(suppressWarnings(coef(glm(y[ix] ~ 1, offset = z[ix], family = binomial))[1]), error = function(e) 0)
    if (!is.finite(a)) a <- 0; zz <- z[ix] + a; tot <- tot - sum(y[ix] * plogis(zz, log.p = TRUE) + (1 - y[ix]) * plogis(-zz, log.p = TRUE)) }
  tot / length(y) }
# within-state multinomial log loss for E: per-group class offsets by fixed-point iteration
wmll <- function(Z, cls, g) { tot <- 0   # Z: n x 3 margins (none, over, under); cls in 1..3
  for (ix in split(seq_along(g), g)) { z <- Z[ix, , drop = FALSE]; a <- c(0, 0, 0); obs <- tabulate(cls[ix], 3) + 0.5
    for (it in 1:25) { e <- exp(sweep(z, 2, a, "+")); p <- e / rowSums(e); a[2:3] <- a[2:3] + log(obs[2:3] / (colSums(p)[2:3] + 0.5)) - log(obs[1] / (colSums(p)[1] + 0.5)) }
    e <- exp(sweep(z, 2, a, "+")); p <- e / rowSums(e); tot <- tot - sum(log(pmax(p[cbind(seq_along(ix), cls[ix])], 1e-9))) }
  tot / nrow(Z) }
rescale_by <- function(mu, yv, g) { for (ix in split(seq_along(g), g)) mu[ix] <- mu[ix] * max(sum(yv[ix]), 1) / max(sum(mu[ix]), 1e-9); pmax(mu, 1e-9) }
gamma_dev <- function(mu, yv) { mu <- pmax(mu, 1e-6); 2 * mean(-log(yv / mu) + (yv - mu) / mu) }
tw_dev <- function(mu, yv, g, p) { mu <- rescale_by(mu, yv, g)
  2 * mean(ifelse(yv > 0, yv^(2 - p) / ((1 - p) * (2 - p)), 0) - yv * mu^(1 - p) / (1 - p) + mu^(2 - p) / (2 - p)) }
pois_dev <- function(mu, yv, g) { mu <- rescale_by(mu, yv, g); 2 * mean(ifelse(yv > 0, yv * log(yv / mu), 0) - (yv - mu)) }

# ---------------- the reading (stage 2, and validation lift in stage 1) ----------------
keep_of <- function(D, rows, gap_cut = GAP_CUT, filter = "gap") {
  k <- rep(TRUE, length(rows)); if (filter == "full") return(k)
  k <- !D$mm[rows] & !D$pile[rows] & !D$step7[rows]; if (filter == "clean") return(k)
  k <- k & D$gap[rows] <= gap_cut; if (filter == "gap") return(k)
  if (filter == "pathrobust") return(!D$mm[rows] & !D$pile[rows] & !D$step7[rows] & !(D$y[rows] & D$grp[rows] %in% c("hhsize", "no_change"))) }
# per-state $/flag at budget b: k from the state's full n, top k among kept rows
dpf_state <- function(sc, dv, keep, SS, b) vapply(SS, function(ix) { kk <- max(1L, round(b * length(ix))); ix2 <- ix[keep[ix]]
  sel <- ix2[order(-sc[ix2], ix2)[seq_len(min(kk, length(ix2)))]]; sum(dv[sel]) / kk }, 0)
lift_of <- function(sc, dv, keep, SS, buds = BUD) { m <- mean(dv[keep]); mean(sapply(buds, function(b) mean(dpf_state(sc, dv, keep, SS, b)))) / m }
prec5 <- function(sc, yv, keep, SS) { r <- sapply(SS, function(ix) { kk <- max(1L, round(0.05 * length(ix))); ix2 <- ix[keep[ix]]
  sel <- ix2[order(-sc[ix2], ix2)[seq_len(min(kk, length(ix2)))]]; c(sum(yv[sel]), kk) }); sum(r[1, ]) / sum(r[2, ]) }
val_lift <- function(D, rows, sc) { k <- keep_of(D, rows); lift_of(sc, D$d[rows], k, split(seq_along(rows), D$st[rows])) }

# ---------------- caching ----------------
cpath <- function(...) file.path(OUT, "cache", paste0(paste(..., sep = "_"), ".rds"))
cached <- function(fn, expr) { if (file.exists(fn)) return(readRDS(fn)); v <- force(expr); saveRDS(v, fn); v }
