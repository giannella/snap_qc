# Shared probe for steps 3-5 of plan.md (2026-10-31): the miners' xgboost
# settings on VOCAB19, one model per household-size group, FY2022-23 -> FY2024.
source("methods/v270_seed_replicates/common.R")
suppressMessages(library(xgboost))
PROBE_THREADS <- 4L

# a frame file -> FY2017-24 rows with VOCAB19, label, error $, keys
probe_frame <- function(path) {
  r <- readRDS(path); r <- r[r$fiscal_year %in% 2017:2024, ]
  r <- prep_features(r, VOCAB19)$data
  r$ie <- !is.na(r$over_threshold) & r$over_threshold != 0
  r$ed <- ifelse(r$ie, abs(ifelse(is.na(r$total_error_amount), 0, r$total_error_amount)), 0)
  r$hh <- hh_group_of(r$cert_HH_size_FS_n)
  r$key <- paste(r$fiscal_year, r$state, r$hhldno)
  stopifnot(!anyDuplicated(r$key))
  r
}

probe_auc <- function(p, y) { r <- rank(p); n1 <- sum(y); n0 <- length(y) - n1; (sum(r[y]) - n1 * (n1 + 1) / 2) / (n1 * n0) }

# fit on the train years, score the test year; returns test-row scores
probe_scores <- function(d, train = 2022:2023, test = 2024, seed = 117L) {
  X <- as.matrix(d[VOCAB19]); storage.mode(X) <- "double"
  tr <- which(d$fiscal_year %in% train); te <- which(d$fiscal_year == test)
  p <- numeric(length(te))
  for (h in HH_LEVELS) {
    w <- tr[d$hh[tr] == h]; i <- d$hh[te] == h; set.seed(seed)
    m <- xgb.train(params = list(objective = "binary:logistic", max_depth = XGB$max_depth, eta = XGB$eta,
                                 subsample = XGB$subsample, nthread = PROBE_THREADS),
                   data = xgb.DMatrix(X[w, , drop = FALSE], label = as.numeric(d$ie[w])),
                   nrounds = XGB$nrounds, verbose = 0)
    p[i] <- predict(m, xgb.DMatrix(X[te[i], , drop = FALSE]))
  }
  list(p = p, te = te)
}

probe <- function(d, ...) {
  s <- probe_scores(d, ...); te <- s$te; p <- s$p; ie <- d$ie[te]; ed <- d$ed[te]
  o5 <- order(-p)[seq_len(round(0.05 * length(p)))]; o10 <- order(-p)[seq_len(round(0.10 * length(p)))]
  data.frame(auc = probe_auc(p, ie), prec05 = mean(ie[o5]), dpf05 = mean(ed[o5]),
             prec10 = mean(ie[o10]), dpf10 = mean(ed[o10]))
}
