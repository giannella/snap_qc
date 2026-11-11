# Exploration window of the case-scoring-by-rule-votes study
# (methods/vote_scoring/design_note.md, "Choosing on exploration").
# Rules mined on FY2017-18 (seeds 117-119, methods/vote_scoring/fy2019/pools);
# everything is fitted and judged on FY2019, out of fold, with five folds of
# states. Choices are made on seed 117 in the note's order:
#   1. vote definition (any member vs representative, R = 300), by log-loss
#   2. P(error) form (vote curve / best flagging rule's LCB / base rate), by log-loss
#   3. error-size model (size-group means / log benefit + household size / + vote), by gamma deviance
#   4. number of rules: the smallest R of 100/200/300 within 5% of R = 300 on
#      top-5% error dollars per reviewed case
# Then the paired SE of the gain over the simple ordering (2,000 within-state
# resamples) and the minimum detectable gain; the chosen curve and error-size
# model are refit on all of FY2019 for each seed and frozen.
#   Rscript methods/vote_scoring/explore_fy2019.R
# Opens FY2019 only. Writes methods/vote_scoring/fy2019/explore_*.csv and
# frozen_seed<seed>.rds.
Sys.setenv(SR_WINDOW = "fy2019")
source("methods/v270_seed_replicates/common.R")
source("methods/vote_scoring/vote_groups.R")
OUT <- "methods/vote_scoring/fy2019"
MIN_BIN_ERRS <- 50L; BUDGET <- 0.05; B <- 2000L; NFOLD <- 5L; RS <- c(100L, 200L, 300L)
set.seed(20261028)

F <- load_frame("final")
tr <- which(F$is_tr); te <- which(F$yr == TEST_YEAR); NT <- length(te)
S_tr <- strata_of(F$hh[tr]); S_te <- strata_of(F$hh[te])
num <- function(x) { x <- suppressWarnings(as.numeric(as.character(x))); x[is.na(x)] <- 0; x }
ie <- F$ie[te]; ed <- F$ed[te]; hh <- F$hh[te]; st <- F$st[te]; mm <- F$mm[te]
ben <- num(F$adf$rawben[te]); hsz <- num(F$adf$cert_HH_size_FS_n[te])
hsz_f <- factor(ifelse(hsz >= 6, "6+", as.character(pmax(1, hsz))), levels = c("1", "2", "3", "4", "5", "6+"))
base_mine <- vapply(HH_LEVELS, function(h) mean(F$ie[tr][F$hh[tr] == h]), numeric(1))
simple <- ben * base_mine[hh]
states <- sort(unique(st))
fold_of_state <- setNames(sample(rep(seq_len(NFOLD), length.out = length(states))), states)
fold <- unname(fold_of_state[st])
stamp("FY2019: %d cases, %d errors, %d states; folds hold %s states", NT, sum(ie), length(states),
      paste(tabulate(fold_of_state), collapse = "/"))

## ---- fitting pieces -----------------------------------------------------------
pava <- function(y, w) {          # weighted isotonic (non-decreasing) regression
  v <- y; ww <- w; len <- rep(1L, length(y)); m <- 0L
  vv <- numeric(0); wv <- numeric(0); lv <- integer(0)
  for (i in seq_along(y)) {
    vv <- c(vv, v[i]); wv <- c(wv, ww[i]); lv <- c(lv, 1L)
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
pred_iso1 <- function(f, x) f$rate[pmax(1L, findInterval(x, f$lo))]
fit_curve <- function(x, y, h) lapply(setNames(nm = HH_LEVELS), function(l) fit_iso1(x[h == l], y[h == l]))
pred_curve <- function(fc, x, h) { p <- numeric(length(x)); for (l in HH_LEVELS) { i <- h == l; p[i] <- pred_iso1(fc[[l]], x[i]) }; p }
logloss <- function(p, y) { p <- pmin(pmax(p, 1e-6), 1 - 1e-6); -mean(ifelse(y, log(p), log(1 - p))) }

SIZE_FORMS <- list(means = ed ~ hh, benefit_size = ed ~ lben + hsz, benefit_size_vote = ed ~ lben + hsz + vote)
size_data <- function(vote) data.frame(ed = ed, hh = factor(hh, levels = HH_LEVELS), lben = log(pmax(ben, 1)), hsz = hsz_f, vote = vote)
fit_size <- function(form, d, rows) {
  m <- glm(form, family = Gamma(link = "log"), data = d[rows[d$ed[rows] > 0], ], control = glm.control(maxit = 100))
  m[c("data", "model")] <- NULL
  m
}
gamma_dev <- function(y, mu) 2 * sum(-log(y / mu) + (y - mu) / mu)

# out-of-fold P(error) for a form; x = vote or best flagging LCB
oof_p <- function(form, x) {
  p <- numeric(NT)
  for (k in seq_len(NFOLD)) {
    fi <- fold != k; ho <- fold == k
    p[ho] <- switch(form,
      base = vapply(HH_LEVELS, function(l) mean(ie[fi & hh == l]), numeric(1))[hh[ho]],
      pred_curve(fit_curve(x[fi], ie[fi], hh[fi]), x[ho], hh[ho]))
  }
  p
}
oof_size <- function(sf, vote) {
  d <- size_data(vote); mu <- numeric(NT)
  for (k in seq_len(NFOLD)) mu[fold == k] <- predict(fit_size(SIZE_FORMS[[sf]], d, which(fold != k)), d[fold == k, ], type = "response")
  mu
}

# each state's top 5%; flagged indices into `idx` (default all FY2019 cases)
top_cases <- function(score, tieb, idx = seq_len(NT)) {
  unlist(lapply(split(idx, st[idx]), function(ix) {
    k <- max(1L, round(BUDGET * length(ix)))
    ix[order(-score[ix], -tieb[ix], runif(length(ix)))][seq_len(k)]
  }), use.names = FALSE)
}
per_case <- function(f) sum(ed[f]) / length(f)
zero <- numeric(NT)

## ---- per-seed votes on FY2019 ----------------------------------------------------
seed_votes <- function(seed) {
  pool <- read_pool(pool_file("final", seed, "admitted"))
  rules <- top_rules(pool)
  fl_tr <- flags_for_rules(rules, F$adf[tr, , drop = FALSE], S_tr)
  stopifnot(all(lengths(fl_tr) == rules$n))           # mining-year flags reproduce the pool's counts
  g <- build_groups(rules, fl_tr)
  need <- which(g$lead | paste(g$hh, g$group) %in% paste(allot(g, 300L, "any")$hh, allot(g, 300L, "any")$group))
  need <- sort(union(need, which(g$lead)))
  fl_te <- vector("list", nrow(g)); fl_te[need] <- flags_for_rules(g[need, , drop = FALSE], F$adf[te, , drop = FALSE], S_te)
  V <- list()
  for (def in c("any", "rep")) for (R in RS) {
    al <- allot(g, R, def); rows <- match(paste(al$hh, al$rule), paste(g$hh, g$rule))
    V[[paste(def, R)]] <- c(votes_of(al, fl_te[rows], NT),
      list(rules = nrow(al), groups = vapply(HH_LEVELS, function(l) length(unique(al$group[al$hh == l])), integer(1))))
  }
  stamp("seed %d: %d groups from %d top rules (%s by size group)", seed, sum(g$lead), nrow(g),
        paste(vapply(HH_LEVELS, function(l) sum(g$lead & g$hh == l), integer(1)), collapse = "/"))
  list(groups = g, V = V)
}
SV <- lapply(setNames(SEEDS, SEEDS), seed_votes)
saveRDS(lapply(SV, `[[`, "groups"), file.path(OUT, "explore_groups.rds"))

sizes <- bind_rows(lapply(names(SV), function(s) bind_rows(lapply(names(SV[[s]]$V), function(k) {
  v <- SV[[s]]$V[[k]]; d <- strsplit(k, " ")[[1]]
  data.frame(seed = s, def = d[1], R = as.integer(d[2]), rules = v$rules,
             groups_1 = v$groups[["1"]], groups_23 = v$groups[["2-3"]], groups_4 = v$groups[["4+"]],
             flagged_share = round(mean(v$vote > 0), 4), max_vote = max(v$vote))
}))))
write.csv(sizes, file.path(OUT, "explore_sizes.csv"), row.names = FALSE)
print(sizes, row.names = FALSE)

## ---- choices on seed 117 ---------------------------------------------------------
V117 <- SV[["117"]]$V
# 1. vote definition
step1 <- data.frame(def = c("any", "rep"), logloss = c(logloss(oof_p("vote", V117[["any 300"]]$vote), ie),
                                                       logloss(oof_p("vote", V117[["rep 300"]]$vote), ie)))
DEF <- step1$def[which.min(step1$logloss)]
stamp("step 1, vote definition: any %.5f, rep %.5f -> %s", step1$logloss[1], step1$logloss[2], DEF)
# 2. P(error) form
v300 <- V117[[paste(DEF, 300)]]
step2 <- data.frame(form = c("vote", "lcb", "base"),
  logloss = c(logloss(oof_p("vote", v300$vote), ie), logloss(oof_p("lcb", v300$maxlcb), ie), logloss(oof_p("base", NULL), ie)))
PFORM <- step2$form[which.min(step2$logloss)]
stamp("step 2, P(error) form: vote %.5f, lcb %.5f, base %.5f -> %s", step2$logloss[1], step2$logloss[2], step2$logloss[3], PFORM)
write.csv(bind_rows(mutate(step1, step = "1 vote definition (R=300)", choice = def) %>% select(step, choice, logloss),
                    mutate(step2, step = "2 P(error) form", choice = form) %>% select(step, choice, logloss)),
          file.path(OUT, "explore_logloss.csv"), row.names = FALSE)
# 3. error-size model
step3 <- data.frame(model = names(SIZE_FORMS), deviance = vapply(names(SIZE_FORMS), function(sf) {
  mu <- oof_size(sf, v300$vote); e <- ie & ed > 0; gamma_dev(ed[e], mu[e]) }, numeric(1)))
SFORM <- step3$model[which.min(step3$deviance)]
stamp("step 3, error-size model: %s -> %s", paste(sprintf("%s %.1f", step3$model, step3$deviance), collapse = ", "), SFORM)
write.csv(step3, file.path(OUT, "explore_size_deviance.csv"), row.names = FALSE)

score_oof <- function(v) {
  if (PFORM == "base") stop("base rate won step 2: the study stops at exploration")
  p <- oof_p(PFORM, if (PFORM == "vote") v$vote else v$maxlcb)
  p * oof_size(SFORM, v$vote)
}
# 4. number of rules
if (PFORM != "base") {
  step4 <- bind_rows(lapply(RS, function(R) {
    sc <- score_oof(V117[[paste(DEF, R)]]); f <- top_cases(sc, ben)
    data.frame(R = R, per_case = per_case(f), errors_found = sum(ie[f]), reviewed = length(f))
  }))
  step4$vs_300 <- step4$per_case / step4$per_case[step4$R == 300] - 1
  RCH <- min(step4$R[step4$vs_300 >= -0.05])
  stamp("step 4, number of rules: %s -> R = %d", paste(sprintf("R=%d $%.2f (%+.1f%%)", step4$R, step4$per_case, 100 * step4$vs_300), collapse = ", "), RCH)
  write.csv(step4, file.path(OUT, "explore_rules.csv"), row.names = FALSE)

  ## ---- gain on FY2019 and the minimum detectable gain --------------------------
  sim_f <- top_cases(simple, zero)
  gains <- bind_rows(lapply(names(SV), function(s) {
    sc <- score_oof(SV[[s]]$V[[paste(DEF, RCH)]]); f <- top_cases(sc, ben)
    keep <- which(!mm); f2 <- top_cases(sc, ben, keep); s2 <- top_cases(simple, zero, keep)
    data.frame(seed = s, score_per_case = per_case(f), simple_per_case = per_case(sim_f),
               gain = per_case(f) / per_case(sim_f) - 1, gain_no_mm = per_case(f2) / per_case(s2) - 1,
               mm_share_score = sum(ed[intersect(f, which(mm))]) / sum(ed[f]),
               mm_share_simple = sum(ed[intersect(sim_f, which(mm))]) / sum(ed[sim_f]))
  }))
  print(gains, row.names = FALSE)
  sc117 <- score_oof(V117[[paste(DEF, RCH)]])
  S_idx <- split(seq_len(NT), st)
  boot <- replicate(B, {
    draw <- unlist(lapply(S_idx, function(ix) ix[sample.int(length(ix), replace = TRUE)]), use.names = FALSE)
    a <- top_cases(sc117, ben, draw); b <- top_cases(simple, zero, draw)
    per_case(a) / per_case(b) - 1
  })
  se <- sd(boot)
  mdg <- data.frame(seed = 117, observed_gain = gains$gain[gains$seed == "117"], boot_se = se,
                    lo95 = quantile(boot, 0.025), hi95 = quantile(boot, 0.975),
                    between_seed_sd = sd(gains$gain),
                    mdg_50pct_power = 0.20 + 1.96 * se, mdg_80pct_power = 0.20 + (1.96 + 0.84) * se,
                    mdg_80pct_power_with_seeds = 0.20 + 2.80 * sqrt(se^2 + (1 + 1/3) * sd(gains$gain)^2))
  print(mdg, row.names = FALSE)
  write.csv(gains, file.path(OUT, "explore_gain_by_seed.csv"), row.names = FALSE)
  write.csv(mdg, file.path(OUT, "explore_mdg.csv"), row.names = FALSE)

  ## ---- freeze: refit the chosen curve and error-size model on all of FY2019 ----
  curves <- list()
  for (s in names(SV)) {
    v <- SV[[s]]$V[[paste(DEF, RCH)]]
    x <- if (PFORM == "vote") v$vote else v$maxlcb
    fc <- fit_curve(x, ie, hh); fs <- fit_size(SIZE_FORMS[[SFORM]], size_data(v$vote), seq_len(NT))
    saveRDS(list(seed = as.integer(s), frame = "final (1dbe8733)", train_years = TRAIN_YEARS, fit_year = TEST_YEAR,
                 def = DEF, R = RCH, p_form = PFORM, size_model = SFORM, curve = fc, size_fit = fs,
                 hsz_levels = levels(hsz_f), min_bin_errs = MIN_BIN_ERRS),
            file.path(OUT, sprintf("frozen_seed%s.rds", s)))
    curves[[s]] <- bind_rows(lapply(HH_LEVELS, function(l) with(fc[[l]],
      data.frame(seed = s, hh = l, from = lo, cases = n, errors = e, raw_rate = round(raw, 4), fitted_rate = round(rate, 4)))))
  }
  write.csv(bind_rows(curves), file.path(OUT, "explore_curves.csv"), row.names = FALSE)
  print(bind_rows(curves) %>% filter(seed == "117"), row.names = FALSE)
}
stamp("exploration done")
