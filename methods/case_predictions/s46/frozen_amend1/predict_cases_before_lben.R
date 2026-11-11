# Per-case predictions for states that deploy a model rather than a workbook
# (Eric, 2026-10-30: option D). One model per household-size group (1, 2-3,
# 4+), the groups the delivery lists are mined in. In each group the two rule
# generators the lists come from, xgboost and ranger, are fitted with the
# miners' settings (XGB, RF and VOCAB19 read by name from the shipped benchmark
# through common.R) and stacked: a logistic meta-model on their out-of-fold
# probabilities gives P(error). A gamma model of the error amount among errors
# gives E[$ | error], and expected $ = P(error) x E[$ | error].
#
# The delivery lists are NOT model inputs: they were mined on the same FY2022-24
# cases the deploy model trains on, so a list-hit feature would leak. The output
# carries, beside each prediction, the list rules that flag the case (the
# explanation states already know).
#
# Three modes, all read SR_FRAME / SR_ROOT / SR_WINDOW as the miners do:
#   SR_WINDOW=fy2022 PC_MODE=evaluate  fit FY2017-19, score FY2022 (held out)
#   SR_WINDOW=fy2024 PC_MODE=evaluate  fit FY2022-23, score FY2024 (held out)
#   SR_WINDOW=deploy PC_MODE=fit       fit FY2022-24, save the model bundle
#   PC_MODE=predict PC_STATE=Washington [PC_INPUT=cases.csv]
#       score the cases in PC_INPUT (default: the state's FY2022-24 frame rows)
#       with the saved deploy bundle; writes predictions_<State>.csv. The level
#       is set to the state's FNS payment error rate in PC_PER (default the
#       FY2025 rates; PC_PER=none keeps the model's own level). The bundle also
#       holds the over/underpayment stacks (PC_DIRECTIONS=0 leaves them out):
#       the output adds each case's P and expected $ for each direction and a
#       rank for each, with each stack set to the state's FNS over- or
#       underpayment rate
# e.g.  SR_FRAME=f1030 SR_ROOT=C:/Users/ericg/snap_qc_v27 SR_WINDOW=fy2022 PC_MODE=evaluate \
#         Rscript methods/case_predictions/predict_cases.R
# SMOKE=1 runs 40-tree engines and 2 folds.
source("methods/v270_seed_replicates/common.R")
suppressMessages({ library(xgboost); library(ranger) })
MODE <- Sys.getenv("PC_MODE", "evaluate")
stopifnot(MODE %in% c("evaluate", "fit", "predict"))
FR <- Sys.getenv("SR_FRAME", "f1030")
OUT <- file.path("methods/case_predictions", if (SMOKE) "smoke" else "")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
BUNDLE <- file.path(OUT, sprintf("stack_%s_deploy.rds", FR))
K_FOLDS <- if (SMOKE) 2L else 5L
SEED <- 117L
THREADS <- as.integer(Sys.getenv("PC_THREADS", "4"))
EPS <- 1e-4
logit <- function(p) qlogis(pmin(pmax(p, EPS), 1 - EPS))

## ---- level-0 engines (the miners' settings) -----------------------------------
fit_xgb <- function(X, y) {
  set.seed(SEED)
  xgb.train(params = list(objective = "binary:logistic", max_depth = XGB$max_depth,
                          eta = XGB$eta, subsample = XGB$subsample, nthread = THREADS),
            data = xgb.DMatrix(X, label = as.numeric(y)), nrounds = XGB$nrounds, verbose = 0)
}
# the miners drop incomplete rows; here ranger learns a direction for missing
# values instead (under 0.1% of rows) so that every new case gets a score
fit_rf <- function(X, y)
  ranger(x = as.data.frame(X), y = factor(y, levels = c(FALSE, TRUE)), probability = TRUE,
         num.trees = RF$num_trees, max.depth = RF$max_depth, mtry = RF$mtry,
         min.node.size = RF$min_node_size, seed = SEED, num.threads = THREADS,
         na.action = "na.learn")
pred_xgb <- function(m, X) predict(m, xgb.DMatrix(X))
pred_rf  <- function(m, X) predict(m, as.data.frame(X), num.threads = THREADS)$predictions[, "TRUE"]

feature_matrix <- function(df) {
  X <- as.matrix(df[VOCAB19]); storage.mode(X) <- "double"; X
}

## ---- one household-size group ---------------------------------------------------
# Out-of-fold level-0 probabilities -> logistic meta-model; full-data level-0
# fits for new cases; gamma size model on the errors' log issued benefit and
# stacked logit.
fit_group <- function(X, ie, ed, lben) {
  set.seed(SEED)
  fold <- integer(length(ie))   # folds stratified by the error label
  for (v in c(FALSE, TRUE)) { w <- which(ie == v); fold[w] <- sample(rep_len(seq_len(K_FOLDS), length(w))) }
  oof <- matrix(NA_real_, length(ie), 2, dimnames = list(NULL, c("xgb", "rf")))
  for (k in seq_len(K_FOLDS)) {
    tr <- fold != k
    oof[!tr, "xgb"] <- pred_xgb(fit_xgb(X[tr, , drop = FALSE], ie[tr]), X[!tr, , drop = FALSE])
    oof[!tr, "rf"]  <- pred_rf(fit_rf(X[tr, , drop = FALSE], ie[tr]), X[!tr, , drop = FALSE])
  }
  z <- data.frame(y = as.numeric(ie), lx = logit(oof[, "xgb"]), lr = logit(oof[, "rf"]))
  meta <- glm(y ~ lx + lr, family = binomial, data = z)
  ps <- predict(meta, z, type = "response")
  s <- data.frame(ed = ed, lben = lben, lp = logit(ps))[ie & ed > 0, ]
  size <- glm(ed ~ lben + lp, family = Gamma(link = "log"), data = s, control = glm.control(maxit = 100))
  list(xgb = fit_xgb(X, ie), rf = fit_rf(X, ie), meta = meta, size = size,
       oof = oof, oof_stack = ps, y = ie, n = length(ie), k = sum(ie))
}

fit_stack <- function(df, ie, ed, hh) {
  X <- feature_matrix(df); lben <- log1p(pmax(df$rawben, 0))
  groups <- lapply(setNames(nm = HH_LEVELS), function(h) {
    w <- which(hh == h)
    stamp("  group %s: %d rows, %d errors", h, length(w), sum(ie[w]))
    fit_group(X[w, , drop = FALSE], ie[w], ed[w], lben[w])
  })
  list(groups = groups, features = VOCAB19, frame = FR, train_years = TRAIN_YEARS,
       xgb = XGB, rf = RF, k_folds = K_FOLDS, seed = SEED, smoke = SMOKE,
       built = format(Sys.time()))
}

# Over/underpayment arm (Eric, 2026-10-30; evaluate mode only until adopted): the
# same stack fitted twice in each group, overpayments against everything else
# and underpayments against everything else, each with its own size model.
# P(error) = P(over) + P(under); expected $ = the sum of the two expected $.
# Direction is the sign of bendiff (issued minus correct), a label, not an input.
DIRS <- Sys.getenv("PC_DIRECTIONS", "1") == "1"
fit_dirs <- function(df, ie, ed, hh, over) {
  stamp(" overpayment stack"); bo <- fit_stack(df, ie & over, ifelse(over, ed, 0), hh)
  stamp(" underpayment stack"); bu <- fit_stack(df, ie & !over, ifelse(over, 0, ed), hh)
  list(over = bo, under = bu)
}
predict_dirs <- function(bd, df, hh) {
  po <- predict_stack(bd$over, df, hh); pu <- predict_stack(bd$under, df, hh)
  data.frame(p_over = po$p_error, p_under = pu$p_error,
             dollars_over = po$expected_dollars, dollars_under = pu$expected_dollars,
             size_over = po$size_if_error, size_under = pu$size_if_error,
             p_error = pmin(po$p_error + pu$p_error, 1),
             expected_dollars = po$expected_dollars + pu$expected_dollars)
}

# P(error) from each engine and the stack, E[$ | error] and expected $
predict_stack <- function(b, df, hh) {
  stopifnot(all(b$features %in% names(df)))
  X <- feature_matrix(df); lben <- log1p(pmax(df$rawben, 0))
  out <- data.frame(p_xgb = NA_real_, p_rf = NA_real_, p_error = NA_real_,
                    size_if_error = NA_real_)[rep(1, nrow(df)), ]
  for (h in names(b$groups)) {
    w <- which(hh == h); if (!length(w)) next
    g <- b$groups[[h]]
    px <- pred_xgb(g$xgb, X[w, , drop = FALSE]); pr <- pred_rf(g$rf, X[w, , drop = FALSE])
    ps <- predict(g$meta, data.frame(lx = logit(px), lr = logit(pr)), type = "response")
    sz <- predict(g$size, data.frame(lben = lben[w], lp = logit(ps)), type = "response")
    out[w, ] <- cbind(px, pr, ps, sz)
  }
  out$expected_dollars <- out$p_error * out$size_if_error
  rownames(out) <- NULL
  out
}

## ---- state level from the FNS payment error rate ------------------------------
# One logit offset per state, the same in every household-size group, solved so
# the state's expected error dollars over its issued benefits equal its published
# FNS payment error rate (Eric, 2026-10-30). The shift is monotone, so the
# P(error) order within a state does not change; E[$ | error] keeps its fitted
# value, so the expected-$ order can move a little. The cases scored should be the
# state's caseload or a random sample of it: on a pre-screened file the offset
# absorbs the screening and the level is too low.
PER_CSV <- Sys.getenv("PC_PER", "methods/vote_scoring/fns_payment_error_rates_fy2025.csv")
read_per <- function(fn) {
  r <- read.csv(fn, stringsAsFactors = FALSE); names(r) <- tolower(names(r))
  setNames(r$payment_error_rate / 100, r$state)
}
level_offset <- function(p, size, issued, per) {
  f <- function(d) sum(plogis(logit(p) + d) * size) / sum(issued) - per
  uniroot(f, c(-15, 15), tol = 1e-8)$root
}
# applies each state's offset; returns P(error), expected $ and the offsets
set_level <- function(p, st, issued, per) {
  pe <- p$p_error; lv <- data.frame(state = unique(st), offset = NA_real_, model_rate = NA_real_,
                                    fns_rate = NA_real_)
  for (k in seq_len(nrow(lv))) {
    w <- which(st == lv$state[k]); r <- per[lv$state[k]]
    lv$model_rate[k] <- sum(p$expected_dollars[w]) / sum(issued[w])
    if (is.na(r)) next
    lv$offset[k] <- level_offset(p$p_error[w], p$size_if_error[w], issued[w], r)
    lv$fns_rate[k] <- r
    pe[w] <- plogis(logit(p$p_error[w]) + lv$offset[k])
  }
  list(p_error = pe, expected_dollars = pe * p$size_if_error, levels = lv)
}
# The over/underpayment arm gets two offsets per state (Eric, 2026-10-30): the
# overpayment stack is set to the state's FNS overpayment rate and the
# underpayment stack to its underpayment rate, each by set_level().
read_rates <- function(fn) {
  r <- read.csv(fn, stringsAsFactors = FALSE); names(r) <- tolower(names(r))
  col <- function(pre) r[[grep(paste0("^", pre), names(r))[1]]] / 100
  list(over = setNames(col("over"), r$state), under = setNames(col("under"), r$state))
}
level_dirs <- function(pd, st, issued, rates) {
  lvs <- list()
  for (d in c("over", "under")) {
    l <- set_level(list(p_error = pd[[paste0("p_", d)]], expected_dollars = pd[[paste0("dollars_", d)]],
                        size_if_error = pd[[paste0("size_", d)]]), st, issued, rates[[d]])
    pd[[paste0("p_", d)]] <- l$p_error; pd[[paste0("dollars_", d)]] <- l$expected_dollars
    lvs[[d]] <- l$levels
  }
  pd$p_error <- pmin(pd$p_over + pd$p_under, 1); pd$expected_dollars <- pd$dollars_over + pd$dollars_under
  list(pd = pd, levels = merge(lvs$over, lvs$under, by = "state", suffixes = c("_over", "_under")))
}

## ---- held-out scoring ---------------------------------------------------------
# Each state reviews its own top share of cases (ranked within state), pooled
# over states, as the lists are scored.
top_share_by_state <- function(score, st, share) {
  flag <- logical(length(score))
  for (s in unique(st)) {
    w <- which(st == s); m <- max(1L, round(share * length(w)))
    flag[w[order(-score[w], w)[seq_len(m)]]] <- TRUE
  }
  flag
}
auc <- function(score, y) {
  r <- rank(score); n1 <- sum(y); n0 <- sum(!y)
  (sum(r[y]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
score_arms <- function(arms, ie, ed, st, keep, share) {
  do.call(rbind, lapply(names(arms), function(a) {
    f <- top_share_by_state(arms[[a]][keep], st[keep], share)
    y <- ie[keep]; d <- ed[keep]
    data.frame(arm = a, ranked_by = if (grepl("dollars", a)) "expected error $" else "P(error)",
               share = share, flagged = sum(f), errors = sum(y[f]),
               precision = round(mean(y[f]), 4), dollars_per_flag = round(sum(d[f]) / sum(f), 2),
               dollar_recall = round(sum(d[f]) / sum(d), 4), auc = round(auc(arms[[a]][keep], y), 4))
  }))
}
# paired bootstrap over cases of (arm a - arm b) precision and $ per flag at a share
boot_diff <- function(sa, sb, ie, ed, st, keep, share, B = if (SMOKE) 50 else 1000) {
  fa <- top_share_by_state(sa[keep], st[keep], share); fb <- top_share_by_state(sb[keep], st[keep], share)
  y <- ie[keep]; d <- ed[keep]; set.seed(SEED)
  stat <- function(i) c(mean(y[i][fa[i]]) - mean(y[i][fb[i]]), mean(d[i][fa[i]]) - mean(d[i][fb[i]]))
  bs <- replicate(B, stat(sample.int(length(y), replace = TRUE)))
  s0 <- stat(seq_along(y)); q <- function(r, pr) unname(quantile(bs[r, ], pr))
  c(precision_diff = s0[1], lo = q(1, 0.025), hi = q(1, 0.975),
    dollars_per_flag_diff = s0[2], lo_dollars = q(2, 0.025), hi_dollars = q(2, 0.975))
}

## ---- run -----------------------------------------------------------------------
if (MODE %in% c("evaluate", "fit")) {
  stopifnot(MODE == "fit" || length(TEST_YEAR) == 1, MODE == "evaluate" || WINDOW == "deploy")
  F <- load_frame(FR)
  tr <- which(F$is_tr)
  stamp("%s %s: fitting the stack on FY%s (%d rows, %d errors)%s", FR, WINDOW,
        paste(TRAIN_YEARS, collapse = "+"), length(tr), sum(F$ie[tr]), if (SMOKE) " [SMOKE]" else "")
  b <- fit_stack(F$adf[tr, ], F$ie[tr], F$ed[tr], F$hh[tr])
  for (h in names(b$groups)) {
    g <- b$groups[[h]]; cf <- coef(g$meta)
    stamp("  meta %s: intercept %.3f, xgboost %.3f, ranger %.3f; OOF AUC xgboost %.4f, ranger %.4f, stack %.4f",
          h, cf[1], cf[2], cf[3], auc(g$oof[, "xgb"], g$y), auc(g$oof[, "rf"], g$y), auc(g$oof_stack, g$y))
  }
  if (MODE == "fit") {
    if (DIRS) {   # the over/underpayment add-on (Eric, 2026-10-30)
      over <- F$adf$bendiff > 0
      stopifnot(!anyNA(over[F$ie]), all(F$adf$bendiff[F$ie] != 0))
      stamp("over/underpayment stacks: %d overpayment and %d underpayment errors",
            sum(F$ie[tr] & over[tr]), sum(F$ie[tr] & !over[tr]))
      b$dirs <- fit_dirs(F$adf[tr, ], F$ie[tr], F$ed[tr], F$hh[tr], over[tr])
    }
    saveRDS(b, BUNDLE); stamp("bundle -> %s", BUNDLE)
  } else {
    te <- which(F$yr == TEST_YEAR)
    p <- predict_stack(b, F$adf[te, ], F$hh[te])
    ie <- F$ie[te]; ed <- F$ed[te]; st <- F$st[te]
    pile <- income_pile_rows(F$adf[te, ])
    # each P(error) arm also ranked by expected $ (its P(error) x the stack's E[$ | error])
    arms <- list(xgboost = p$p_xgb, ranger = p$p_rf, stack = p$p_error,
                 xgboost_dollars = p$p_xgb * p$size_if_error, ranger_dollars = p$p_rf * p$size_if_error,
                 stack_dollars = p$expected_dollars)
    pd <- NULL
    if (DIRS) {
      over <- F$adf$bendiff > 0
      stopifnot(!anyNA(over[F$ie]), all(F$adf$bendiff[F$ie] != 0))
      stamp("over/underpayment stacks: %d overpayment and %d underpayment errors in training",
            sum(F$ie[tr] & over[tr]), sum(F$ie[tr] & !over[tr]))
      bdir <- fit_dirs(F$adf[tr, ], F$ie[tr], F$ed[tr], F$hh[tr], over[tr])
      pd <- predict_dirs(bdir, F$adf[te, ], F$hh[te])
      arms$over_under_stack <- pd$p_error
      arms$over_under_stack_dollars <- pd$expected_dollars
    }
    # the level set from the test year's own FNS rate, where the file exists (the best
    # case: in use the latest published rate lags the cases scored by a year or more)
    per_fn <- Sys.getenv("PC_EVAL_PER", sprintf("additional_data/fns_payment_error_rates_fy%s.csv", TEST_YEAR))
    lv <- NULL
    if (file.exists(per_fn)) {
      lv <- set_level(p, st, F$adf$rawben[te], read_per(per_fn))
      arms$stack_dollars_fns_level <- lv$expected_dollars
      obs <- tapply(ed, st, sum) / tapply(F$adf$rawben[te], st, sum)
      lv$levels$sample_rate <- round(as.vector(obs[lv$levels$state]), 4)
    }
    lvd <- NULL
    if (file.exists(per_fn) && !is.null(pd)) {
      lvd <- level_dirs(pd, st, F$adf$rawben[te], read_rates(per_fn))
      arms$over_under_fns_level <- lvd$pd$p_error
      arms$over_under_fns_level_dollars <- lvd$pd$expected_dollars
    }
    res <- list()
    for (rd in c("all", "clean")) {
      keep <- if (rd == "all") rep(TRUE, length(te)) else !(F$mm[te] | pile)
      for (sh in c(0.05, 0.10)) res[[length(res) + 1]] <- cbind(reading = rd, score_arms(arms, ie, ed, st, keep, sh))
    }
    res <- do.call(rbind, res)
    keep <- rep(TRUE, length(te)); best1 <- if (mean(ie[top_share_by_state(p$p_xgb, st, 0.1)]) >=
                                                mean(ie[top_share_by_state(p$p_rf, st, 0.1)])) "xgboost" else "ranger"
    bd <- rbind(stack_vs_best_engine_10 = boot_diff(p$p_error, arms[[best1]], ie, ed, st, keep, 0.10),
                stack_vs_best_engine_05 = boot_diff(p$p_error, arms[[best1]], ie, ed, st, keep, 0.05),
                dollars_vs_p_error_10 = boot_diff(p$expected_dollars, p$p_error, ie, ed, st, keep, 0.10),
                dollars_vs_p_error_05 = boot_diff(p$expected_dollars, p$p_error, ie, ed, st, keep, 0.05))
    if (!is.null(lv))
      bd <- rbind(bd, fns_level_vs_dollars_10 = boot_diff(lv$expected_dollars, p$expected_dollars, ie, ed, st, keep, 0.10))
    if (!is.null(pd)) {
      for (sh in c(0.10, 0.05)) {
        bd <- rbind(bd, boot_diff(pd$p_error, p$p_error, ie, ed, st, keep, sh),
                    boot_diff(pd$expected_dollars, p$expected_dollars, ie, ed, st, keep, sh))
        rownames(bd)[nrow(bd) - 1:0] <- sprintf(c("over_under_vs_stack_%02d", "over_under_dollars_vs_stack_dollars_%02d"),
                                                round(100 * sh))
        if (!is.null(lvd)) {   # both arms at the FNS level, and against the unleveled stack
          bd <- rbind(bd, boot_diff(lvd$pd$p_error, p$p_error, ie, ed, st, keep, sh),
                      boot_diff(lvd$pd$expected_dollars, lv$expected_dollars, ie, ed, st, keep, sh),
                      boot_diff(lvd$pd$expected_dollars, p$expected_dollars, ie, ed, st, keep, sh))
          rownames(bd)[nrow(bd) - 2:0] <- sprintf(c("over_under_fns_level_vs_stack_%02d",
                                                    "over_under_fns_level_dollars_vs_stack_dollars_fns_level_%02d",
                                                    "over_under_fns_level_dollars_vs_stack_dollars_%02d"), round(100 * sh))
        }
      }
      # each direction's own list: precision and $ per flag for that direction's errors,
      # ranked by the combined stack or by that direction's stack
      ov <- ie & F$adf$bendiff[te] > 0
      dir_tab <- do.call(rbind, lapply(c("over", "under"), function(d) {
        yd <- if (d == "over") ov else ie & !ov; edd <- ifelse(yd, ed, 0)
        rk <- list(stack = p$p_error, stack_dollars = p$expected_dollars,
                   direction_stack = pd[[paste0("p_", d)]], direction_dollars = pd[[paste0("dollars_", d)]])
        if (!is.null(lvd)) rk$direction_dollars_fns_level <- lvd$pd[[paste0("dollars_", d)]]
        do.call(rbind, lapply(c(0.05, 0.10), function(sh) cbind(direction = d,
          score_arms(rk, yd, edd, st, keep, sh))))
      }))
      write.csv(dir_tab, file.path(OUT, sprintf("directions_%s_%s.csv", FR, WINDOW)), row.names = FALSE)
      print(dir_tab, row.names = FALSE)
    }
    # calibration: mean P(error) against the observed rate by decile of P(error)
    dec <- cut(rank(p$p_error, ties.method = "first"), 10, labels = FALSE)
    cal <- data.frame(decile = 1:10, mean_p = round(tapply(p$p_error, dec, mean), 4),
                      observed = round(tapply(ie, dec, mean), 4), n = as.vector(table(dec)))
    tag <- sprintf("%s_%s", FR, WINDOW)
    write.csv(res, file.path(OUT, sprintf("eval_%s.csv", tag)), row.names = FALSE)
    write.csv(cal, file.path(OUT, sprintf("calibration_%s.csv", tag)), row.names = FALSE)
    write.csv(data.frame(comparison = rownames(bd), best_engine = best1, bd),
              file.path(OUT, sprintf("bootstrap_%s.csv", tag)), row.names = FALSE)
    saveRDS(list(p = p, pd = pd, ie = ie, ed = ed, st = st, issued = F$adf$rawben[te], mm = F$mm[te],
                 pile = pile, over = F$adf$bendiff[te] > 0),
            file.path(OUT, sprintf("heldout_%s.rds", tag)))
    if (!is.null(lvd)) {
      obs_d <- function(w) tapply(ifelse(w, ed, 0), st, sum) / tapply(F$adf$rawben[te], st, sum)
      lvd$levels$sample_over <- round(as.vector(obs_d(ie & F$adf$bendiff[te] > 0)[lvd$levels$state]), 4)
      lvd$levels$sample_under <- round(as.vector(obs_d(ie & F$adf$bendiff[te] < 0)[lvd$levels$state]), 4)
      write.csv(lvd$levels, file.path(OUT, sprintf("levels_directions_%s.csv", tag)), row.names = FALSE)
      stamp("FNS over/under levels: overpayment offsets median %.3f (%.3f to %.3f); underpayment median %.3f (%.3f to %.3f)",
            median(lvd$levels$offset_over, na.rm = TRUE), min(lvd$levels$offset_over, na.rm = TRUE),
            max(lvd$levels$offset_over, na.rm = TRUE), median(lvd$levels$offset_under, na.rm = TRUE),
            min(lvd$levels$offset_under, na.rm = TRUE), max(lvd$levels$offset_under, na.rm = TRUE))
    }
    if (!is.null(lv)) {
      write.csv(lv$levels, file.path(OUT, sprintf("levels_%s.csv", tag)), row.names = FALSE)
      stamp("FNS level (%s): offsets median %.3f, range %.3f to %.3f", per_fn, median(lv$levels$offset, na.rm = TRUE),
            min(lv$levels$offset, na.rm = TRUE), max(lv$levels$offset, na.rm = TRUE))
    }
    print(res, row.names = FALSE); print(bd); print(cal, row.names = FALSE)
    stamp("total P(error) %.0f against %d errors in FY%s", sum(p$p_error), sum(ie), TEST_YEAR)
  }
}

if (MODE == "predict") {
  STATE <- Sys.getenv("PC_STATE"); stopifnot(nzchar(STATE), file.exists(BUNDLE))
  b <- readRDS(BUNDLE)
  inp <- Sys.getenv("PC_INPUT")
  if (nzchar(inp)) {
    df <- read.csv(inp, stringsAsFactors = FALSE, check.names = FALSE)
    id <- if ("CASE_ID" %in% names(df)) df$CASE_ID else seq_len(nrow(df))
  } else {   # the state's FY2022-24 frame rows: in-sample, a demonstration only
    reg <- readRDS(FRAMES[[FR]])
    df <- reg[reg$state == STATE & reg$fiscal_year %in% b$train_years, ]
    stopifnot(nrow(df) > 0)
    df <- prep_features(df, VOCAB19)$data
    id <- paste(df$fiscal_year, df$hhldno, sep = "-")
  }
  hh <- hh_group_of(df$cert_HH_size_FS_n)
  p <- predict_stack(b, df, hh)
  p_model <- p$p_error
  if (PER_CSV != "none") {   # PC_PER=none keeps the model's own level
    lv <- set_level(p, rep(STATE, nrow(df)), df$rawben, read_per(PER_CSV))$levels
    stopifnot(!is.na(lv$offset))
    p$p_error <- plogis(logit(p$p_error) + lv$offset); p$expected_dollars <- p$p_error * p$size_if_error
    stamp("%s: level set to the FNS rate %.2f%% (%s): logit offset %.3f; the model alone gave %.2f%%",
          STATE, 100 * lv$fns_rate, PER_CSV, lv$offset, 100 * lv$model_rate)
  }
  # the over/underpayment add-on: its own lists, each stack set to the state's FNS
  # overpayment or underpayment rate (the main ranking stays the combined stack)
  pd <- NULL
  if (!is.null(b$dirs)) {
    pd <- predict_dirs(b$dirs, df, hh)
    if (PER_CSV != "none") {
      ld <- level_dirs(pd, rep(STATE, nrow(df)), df$rawben, read_rates(PER_CSV))
      pd <- ld$pd; l <- ld$levels
      stopifnot(!is.na(l$offset_over), !is.na(l$offset_under))
      stamp("%s: overpayments set to %.2f%% (offset %.3f; model %.2f%%), underpayments to %.2f%% (offset %.3f; model %.2f%%)",
            STATE, 100 * l$fns_rate_over, l$offset_over, 100 * l$model_rate_over,
            100 * l$fns_rate_under, l$offset_under, 100 * l$model_rate_under)
    }
  }
  # the state's delivery-list rules that flag each case (explanation, not input)
  lst <- file.path("state_delivery_lists", sprintf("blended_delivery_%s_2022_2024_budget10.csv", STATE))
  why <- rep("", nrow(df))
  if (file.exists(lst)) {
    rl <- read.csv(lst, stringsAsFactors = FALSE); rl$hh <- as.character(rl$hh)
    fl <- flags_for_rules(rl, df, strata_of(hh))
    hits <- split(rep(rl$rank, lengths(fl)), factor(unlist(fl), levels = seq_len(nrow(df))))
    why <- vapply(hits, function(r) paste(sort(r), collapse = " "), "")
  }
  o <- data.frame(case = id, hh_group = hh, issued_benefit = df$rawben, p_error = round(p$p_error, 4),
                  expected_error_dollars = round(p$expected_dollars, 2),
                  priority_rank = rank(-p$expected_dollars, ties.method = "first"),
                  rank_by_p_error = rank(-p$p_error, ties.method = "first"),
                  p_error_before_level = round(p_model, 4),
                  p_xgboost = round(p$p_xgb, 4), p_ranger = round(p$p_rf, 4),
                  list_rules_flagging = why, stringsAsFactors = FALSE)
  if (!is.null(pd))
    o <- cbind(o, p_overpayment = round(pd$p_over, 4), expected_overpayment_dollars = round(pd$dollars_over, 2),
               overpayment_rank = rank(-pd$dollars_over, ties.method = "first"),
               p_underpayment = round(pd$p_under, 4), expected_underpayment_dollars = round(pd$dollars_under, 2),
               underpayment_rank = rank(-pd$dollars_under, ties.method = "first"))
  o <- o[order(o$priority_rank), ]
  fn <- file.path(OUT, sprintf("predictions_%s.csv", gsub(" ", "_", STATE)))
  write.csv(o, fn, row.names = FALSE)
  stamp("%s: %d cases scored -> %s (bundle %s, trained FY%s)", STATE, nrow(o), fn, BUNDLE,
        paste(b$train_years, collapse = "+"))
}
