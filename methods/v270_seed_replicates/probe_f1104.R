# Standard frame probe for f1104 (2026-11-04), copied from methods/munging_drop_diagnosis/xgb_probe.R:
#   SR_ROOT=C:/Users/ericg/snap_qc_v27 Rscript methods/v270_seed_replicates/probe_f1104.R
# The miners' xgboost settings (XGB) on VOCAB19, one model per household-size
# group. Two windows: FY2017-19 -> FY2023 (the vote check's mining and scoring
# years) and FY2022-23 -> FY2024. Models trained on each frame are scored on
# each frame's test year, so the 2x2 separates the training data from the
# scored data. Readings: national top 5% / 10% by P(error), and the top 5% /
# 10% within each state (pooled over states, and the median state).
source("methods/v270_seed_replicates/common.R")
suppressMessages(library(xgboost))
OUT <- "methods/f1104_lists/stats"; dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
FRS <- c("f1030", "f1104"); THREADS <- 4L
WINDOWS <- list(fy2023 = list(train = 2017:2019, test = 2023), fy2024 = list(train = 2022:2023, test = 2024))

frame_of <- function(fr) {
  r <- readRDS(FRAMES[[fr]]); r <- r[r$fiscal_year %in% 2017:2024, ]
  r <- prep_features(r, VOCAB19)$data
  ie <- !is.na(r$over_threshold) & r$over_threshold != 0
  list(X = { X <- as.matrix(r[VOCAB19]); storage.mode(X) <- "double"; X },
       yr = r$fiscal_year, ie = ie,
       ed = ifelse(ie, abs(ifelse(is.na(r$total_error_amount), 0, r$total_error_amount)), 0),
       hh = hh_group_of(r$cert_HH_size_FS_n), st = as.character(r$state))
}
D <- lapply(setNames(nm = FRS), frame_of)

fit <- function(d, rows) lapply(setNames(nm = HH_LEVELS), function(h) {
  w <- rows[d$hh[rows] == h]; set.seed(117L)
  xgb.train(params = list(objective = "binary:logistic", max_depth = XGB$max_depth, eta = XGB$eta,
                          subsample = XGB$subsample, nthread = THREADS),
            data = xgb.DMatrix(d$X[w, , drop = FALSE], label = as.numeric(d$ie[w])),
            nrounds = XGB$nrounds, verbose = 0)
})
score <- function(m, d, rows) {
  p <- numeric(length(rows))
  for (h in HH_LEVELS) { i <- d$hh[rows] == h; if (any(i)) p[i] <- predict(m[[h]], xgb.DMatrix(d$X[rows[i], , drop = FALSE])) }
  p
}
auc <- function(p, y) { r <- rank(p); n1 <- sum(y); n0 <- length(y) - n1; (sum(r[y]) - n1 * (n1 + 1) / 2) / (n1 * n0) }
top_nat <- function(p, b) order(-p)[seq_len(round(b * length(p)))]
top_st <- function(p, st, b) unlist(lapply(split(seq_along(p), st), function(ix)
  ix[order(-p[ix])][seq_len(max(1L, round(b * length(ix))))]), use.names = FALSE)

res <- list()
for (wn in names(WINDOWS)) {
  W <- WINDOWS[[wn]]
  M <- lapply(D, function(d) fit(d, which(d$yr %in% W$train)))
  for (tf in FRS) for (sf in FRS) {
    d <- D[[sf]]; te <- which(d$yr == W$test)
    p <- score(M[[tf]], d, te); ie <- d$ie[te]; ed <- d$ed[te]; st <- d$st[te]
    for (b in c(0.05, 0.10)) {
      fn <- top_nat(p, b); fs <- top_st(p, st, b)
      per_st <- vapply(split(fs, st[fs]), function(ix) mean(ie[ix]), numeric(1))
      res[[length(res) + 1]] <- data.frame(window = wn, trained_on = tf, scored_on = sf, budget = b,
        test_errors = sum(ie), base = mean(ie), auc = auc(p, ie),
        nat_precision = mean(ie[fn]), nat_per_flag = mean(ed[fn]), nat_dollar_recall = sum(ed[fn]) / sum(ed),
        state_precision = mean(ie[fs]), state_per_flag = mean(ed[fs]), median_state_precision = median(per_st))
    }
    stamp("%s: trained on %s, scored on %s", wn, tf, sf)
  }
}
res <- do.call(rbind, res)
write.csv(res, file.path(OUT, "xgb_probe_f1030_f1104.csv"), row.names = FALSE)
print(res, digits = 3)
