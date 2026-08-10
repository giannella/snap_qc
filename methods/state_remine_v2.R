# State re-mine: representation runoff on typed mining (stage 1), then the
# production state pools with the winner (stage 2).
# Design note: methods/design_note_state_remine_2026-08-09.md (it wins where
# this script and the note could disagree). Frame prep and frozen-percentile
# machinery identical to methods/vocab_factorial_v2.R; walk identical to the
# findings-31 machinery.
#
# Stage 1 (runoff): per state, mine the four typed frames SEPARATELY, pooled
# across household sizes, under two vocabularies (ps / pct, tonight's
# national representation-contest definitions); admit per typed-frame base
# rate (BH FDR 10% + n >= 30); pool each arm's admitted rules on the 99%-LCB
# scale; walk within state (fill FY2022-23 to budget + 3x buffer, freeze,
# walk FY2024); paired readout across states.
# Decision rule (pre-stated in the note): median paired FY2024 precision
# delta (pct - ps) at the 5% budget among states admitting under BOTH arms;
# > 0 -> pct, else ps; < 15 paired states -> fall back to the national
# pct_vs_persize winner (factorial_paired_deltas.csv), logged.
#
# Stage 2 (production): winner vocabulary; per state, 4 typed frames
# (resumed from stage-1 winner cache) + any_error x 3 HH strata; admitted
# pools saved per state. No delivery lists.
#
# SMOKE=1: 3 states, tiny ensembles, own cache.
# Expects `reg_model_data`. Outputs -> methods/state_remine_v2/.

suppressMessages(library(dplyr))
source("rule_mining_helpers.R")

## ---- config -----------------------------------------------------------------
SMOKE <- identical(Sys.getenv("SMOKE"), "1")

SEED        <- 117
BUDGETS     <- c(0.05, 0.10)
BUFFER_MULT <- 3
LCB_Z       <- 2.326
FDR_ALPHA   <- 0.10
MIN_N       <- 30
TRAIN_YEARS <- c("2022", "2023")
TEST_YEAR   <- "2024"
XGB <- list(nrounds = 1000, max_depth = 4, eta = 0.02, subsample = 0.20)
RF  <- list(num_trees = 1000, max_depth = 4, mtry = 2, min_node_size = 20)
SIGNIF_DIGITS <- 3

EXPECT_ROWS_FRAME <- 231619L
EXPECT_TRAIN_ROWS <- 76031L; EXPECT_TRAIN_ERRS <- 8397L
EXPECT_TEST_ROWS  <- 39528L; EXPECT_TEST_ERRS  <- 4764L

TYPED <- c(earned_income   = "earned_overissuance",
           unearned_income = "unearned_overissuance",
           underissuance   = "underissuance",
           other_error     = "other_error")

BASE_FEATURES <- c(
  "HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size",
  "expedited_i", "cat_elig", "rawben_rel_max", "medical_deductions",
  "shelter_expenses_by_hh_size", "utilities", "married", "homeless",
  "percent_abawd", "unc_rawben_rel_max", "months_since_cert_n",
  "count_divisible_by_100")
PS_FEATURES  <- c("gross_by_hh_size", "earned_by_hh_size", "unearned_by_hh_size")
PCT_FEATURES <- c("rawgrinc_p", "rawearn_p", "rawunearn_p", "rawrent_p",
                  "rawmedded_p", "rawcsded_p", "rawdepded_p")
INCUMBENT_PS <- c("total_deductions_by_hh_size", "shelter_expenses_by_hh_size")
PCT_PURE_ADD <- c(PCT_FEATURES, "shelter_expenses_p", "total_deductions_p")
VOCABS <- list(
  ps  = c(BASE_FEATURES, PS_FEATURES),
  pct = c(setdiff(BASE_FEATURES, INCUMBENT_PS), PCT_PURE_ADD))

PCT_MAP <- c(rawgrinc_p  = "rawgross",
             rawearn_p   = "rawearn",
             rawunearn_p = "rawunearn",
             rawmedded_p = "medical_deductions",
             rawdepded_p = "rawdepded",
             rawcsded_p  = "rawcsded",
             rawrent_p   = "rawrent",
             shelter_expenses_p = "shelter_expenses",
             total_deductions_p = "total_deductions")

NATIONAL_DELTAS <- "methods/vocab_factorial_v2/factorial_paired_deltas.csv"
MIN_PAIRED_STATES <- 15

OUT_DIR <- "methods/state_remine_v2"
if (!exists("RESUME_FROM_CHECKPOINT")) RESUME_FROM_CHECKPOINT <- FALSE

if (SMOKE) {
  XGB$nrounds <- 40; RF$num_trees <- 40
  OUT_DIR <- file.path(OUT_DIR, "smoke")
}
CACHE_DIR <- file.path(OUT_DIR, "cache")
POOL_DIR  <- file.path(OUT_DIR, "pools")
dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(POOL_DIR, showWarnings = FALSE, recursive = TRUE)

HH_LEVELS <- c("1", "2-3", "4+")
hh_group_of <- function(n) {
  n <- suppressWarnings(as.numeric(as.character(n)))
  ifelse(is.na(n), NA_character_, ifelse(n <= 1, "1", ifelse(n <= 3, "2-3", "4+")))
}
stamp <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"),
                                   sprintf(...)))

## ---- frame (identical prep to the factorial) --------------------------------
stopifnot(nrow(reg_model_data) == EXPECT_ROWS_FRAME)
year_cpi <- {
  yd <- read.csv("additional_data/year_data.csv")
  setNames(yd$cpi, as.character(yd$year))
}
build_frozen_percentiles <- function(df) {
  yr_chr <- as.character(df$fiscal_year)
  stopifnot(!any(is.na(year_cpi[yr_chr])))
  defl <- year_cpi[yr_chr]
  cell  <- paste(as.character(df$state_name), as.integer(df$cert_HH_size_FS_n))
  state <- as.character(df$state_name)
  is_tr <- yr_chr %in% TRAIN_YEARS
  for (pc in names(PCT_MAP)) {
    src <- PCT_MAP[[pc]]
    if (!src %in% names(df)) stop("percentile source missing: ", src)
    v <- df[[src]]
    if (any(is.na(v)))
      stop(sprintf("NA in percentile source %s (%d rows) - hard stop",
                   src, sum(is.na(v))))
    x <- v / defl
    nz_tr <- is_tr & v != 0
    fit_cell  <- lapply(split(x[nz_tr], cell[nz_tr]),  sort)
    fit_state <- lapply(split(x[nz_tr], state[nz_tr]), sort)
    fit_nat   <- sort(x[nz_tr])
    out <- numeric(nrow(df))
    nzi <- which(v != 0)
    fit_of <- function(cl, stt) {
      f <- fit_cell[[cl]]
      if (is.null(f) || !length(f)) f <- fit_state[[stt]]
      if (is.null(f) || !length(f)) f <- fit_nat
      f
    }
    for (grp in split(nzi, cell[nzi])) {
      f <- fit_of(cell[grp[1]], state[grp[1]])
      out[grp] <- findInterval(x[grp], f) / length(f)
    }
    df[[pc]] <- out
  }
  df
}

adf0 <- reg_model_data %>% filter(fiscal_year %in% c(TRAIN_YEARS, TEST_YEAR))
adf0 <- build_frozen_percentiles(adf0)
pf  <- prep_features(adf0, unique(unlist(VOCABS)))
adf <- pf$data
for (vn in names(VOCABS)) {
  miss <- setdiff(VOCABS[[vn]], pf$features)
  if (length(miss)) stop(sprintf("vocab %s: features missing after prep: %s",
                                 vn, paste(miss, collapse = ", ")))
}
st <- as.character(adf$state)
yr <- as.character(adf$fiscal_year)
es <- as.character(adf$error_status)
ie_all <- !is.na(adf$over_threshold) & adf$over_threshold != 0
ed_all <- ifelse(ie_all, abs(ifelse(is.na(adf$total_error_amount), 0,
                                    adf$total_error_amount)), 0)
stopifnot(sum(yr %in% TRAIN_YEARS) == EXPECT_TRAIN_ROWS,
          sum(ie_all[yr %in% TRAIN_YEARS]) == EXPECT_TRAIN_ERRS,
          sum(yr == TEST_YEAR) == EXPECT_TEST_ROWS,
          sum(ie_all[yr == TEST_YEAR]) == EXPECT_TEST_ERRS)

STATES <- sort(unique(st))
if (SMOKE) STATES <- c("Washington", "Maine", "Mississippi")
stamp("frame ready: %d states, %d features prepped", length(STATES),
      length(pf$features))

## ---- shared helpers ---------------------------------------------------------
admit_rank_frame <- function(rdf, n, k, base_rate) {
  # ONE BH at FDR 10% across the frame's candidates, base_rate scalar or
  # per-rule vector (per-stratum base rates enter the p-values, the
  # multiplicity correction is joint - exactly the shipped builder's and
  # factorial's path, findings 19), AND n >= MIN_N; 99%-LCB order
  rdf$n <- as.integer(n); rdf$k <- k
  pvals <- pbinom(rdf$k - 1, rdf$n, base_rate, lower.tail = FALSE)
  m <- length(pvals); o <- order(pvals)
  thr <- max(c(0L, which(pvals[o] <= FDR_ALPHA * seq_len(m) / m)))
  bh <- rep(FALSE, m); if (thr > 0) bh[o[seq_len(thr)]] <- TRUE
  adm <- rdf[bh & rdf$n >= MIN_N, , drop = FALSE]
  if (!nrow(adm)) return(adm)
  adm$lcb <- wilson_lcb(adm$k, adm$n, LCB_Z)
  adm[order(-adm$lcb, -adm$n, adm$hh, adm$rule, method = "radix"), ,
      drop = FALSE]
}

walk_mask <- function(idx_tr, idx_te, n_tr, n_te, b) {
  cap <- floor(b * n_tr); cap_buf <- floor(BUFFER_MULT * b * n_tr)
  un <- rep(FALSE, n_tr); n_in <- 0L; frozen <- integer(0); buffer <- integer(0)
  for (i in seq_along(idx_tr)) {
    add <- sum(!un[idx_tr[[i]]]); if (add == 0) next
    if (n_in + add <= cap) { un[idx_tr[[i]]] <- TRUE; n_in <- n_in + add; frozen <- c(frozen, i) }
    else if (n_in + add <= cap_buf) { un[idx_tr[[i]]] <- TRUE; n_in <- n_in + add; buffer <- c(buffer, i) }
  }
  cap24 <- floor(b * n_te); un24 <- rep(FALSE, n_te)
  for (i in c(frozen, buffer)) {
    add <- sum(!un24[idx_te[[i]]])
    if (add > 0 && sum(un24) + add <= cap24) un24[idx_te[[i]]] <- TRUE
  }
  list(hit = un24, fill = n_in, cap_buf = cap_buf)
}

# mine one state's four typed frames under one vocabulary; returns the
# admitted pool (rules carry mined_frame, n, k, lcb), cached
mine_state_typed <- function(state, vocab_name) {
  fn <- file.path(CACHE_DIR, sprintf("typed_%s_%s_%d.rds",
                                     gsub(" ", "_", state), vocab_name, SEED))
  if (RESUME_FROM_CHECKPOINT && file.exists(fn)) return(readRDS(fn))
  rows_s <- which(st == state & yr %in% TRAIN_YEARS)
  trs <- adf[rows_s, , drop = FALSE]
  es_s <- es[rows_s]
  parts <- list()
  for (tf in names(TYPED)) {
    fr_rows <- which(es_s %in% c(TYPED[[tf]], "no_error"))
    ie_f <- es_s[fr_rows] == TYPED[[tf]]
    cat(sprintf("  [%s | %s | %s] %d rows, %d events\n",
                state, vocab_name, tf, length(fr_rows), sum(ie_f)))
    if (length(fr_rows) < 100 || sum(ie_f) < 10) next
    rdf <- mine_rule_vocabulary(
      trs, setNames(list(list(rows = fr_rows, ie = ie_f)), tf),
      list(all = seq_len(nrow(trs))), VOCABS[[vocab_name]],
      xgb = XGB, rf = RF, signif_digits = SIGNIF_DIGITS, seed = SEED,
      verbose = FALSE)
    if (is.null(rdf) || !nrow(rdf)) next
    # score on the typed frame; admit vs the frame's own base rate
    fl <- flags_for_rules(rdf, trs[fr_rows, , drop = FALSE],
                          list(all = seq_along(fr_rows)), label = "")
    n <- vapply(fl, length, integer(1))
    k <- vapply(fl, function(ix) sum(ie_f[ix]), numeric(1))
    adm <- admit_rank_frame(rdf, n, k, mean(ie_f))
    if (nrow(adm)) { adm$mined_frame <- tf; parts[[tf]] <- adm }
  }
  pool <- if (length(parts)) bind_rows(parts) else
    data.frame(hh = character(0), rule = character(0))
  if (nrow(pool)) {
    pool <- pool[order(-pool$lcb, -pool$n, pool$rule, method = "radix"), ,
                 drop = FALSE]
    rownames(pool) <- NULL
  }
  saveRDS(pool, fn)
  pool
}

## ---- stage 1: the runoff ----------------------------------------------------
stamp("=== stage 1: typed-mining representation runoff, %d states ===",
      length(STATES))
runoff <- list()
for (state in STATES) {
  tr_s <- which(st == state & yr %in% TRAIN_YEARS)
  te_s <- which(st == state & yr == TEST_YEAR)
  trs <- adf[tr_s, , drop = FALSE]; tes <- adf[te_s, , drop = FALSE]
  err_te <- ie_all[te_s]; doll_te <- ed_all[te_s]
  for (vn in names(VOCABS)) {
    pool <- mine_state_typed(state, vn)
    n_adm <- nrow(pool)
    if (n_adm) {
      idx_tr <- flags_for_rules(pool, trs, list(all = seq_len(nrow(trs))),
                                label = "")
      idx_te <- flags_for_rules(pool, tes, list(all = seq_len(nrow(tes))),
                                label = "")
    }
    for (b in BUDGETS) {
      if (n_adm) {
        wm <- walk_mask(idx_tr, idx_te, nrow(trs), nrow(tes), b)
        fl <- wm$hit
        runoff[[length(runoff) + 1]] <- data.frame(
          state = state, vocab = vn, budget = b, n_admitted = n_adm,
          n_te = nrow(tes), n_flagged = sum(fl),
          n_errors_caught = sum(fl & err_te),
          precision = round(sum(fl & err_te) / max(sum(fl), 1), 4),
          base_rate_te = round(mean(err_te), 4),
          dollar_recall = round(sum(doll_te[fl]) / max(sum(doll_te), 1), 4),
          fill_ratio = round(wm$fill / max(wm$cap_buf / BUFFER_MULT, 1), 3))
      } else {
        runoff[[length(runoff) + 1]] <- data.frame(
          state = state, vocab = vn, budget = b, n_admitted = 0L,
          n_te = nrow(tes), n_flagged = 0L, n_errors_caught = 0L,
          precision = NA_real_, base_rate_te = round(mean(err_te), 4),
          dollar_recall = NA_real_, fill_ratio = 0)
      }
    }
    if (n_adm) { rm(idx_tr, idx_te) }
  }
  stamp("  %s done", state)
}
runoff <- bind_rows(runoff)
write.csv(runoff, file.path(OUT_DIR, "state_runoff_readout.csv"),
          row.names = FALSE)

## ---- stage 1 decision (pre-stated rule) -------------------------------------
w5 <- runoff %>% filter(budget == 0.05) %>%
  select(state, vocab, n_admitted, precision, dollar_recall) %>%
  tidyr::pivot_wider(names_from = vocab,
                     values_from = c(n_admitted, precision, dollar_recall))
paired <- w5 %>% filter(n_admitted_ps > 0, n_admitted_pct > 0)
n_paired <- nrow(paired)
if (n_paired >= MIN_PAIRED_STATES) {
  d <- paired$precision_pct - paired$precision_ps
  dd <- paired$dollar_recall_pct - paired$dollar_recall_ps
  WINNER <- if (median(d) > 0) "pct" else "ps"
  stamp("RUNOFF (%d paired states): median d precision (pct-ps) %+.4f | mean %+.4f | harmed (< -0.05) %d | median d dollars %+.4f -> WINNER: %s",
        n_paired, median(d), mean(d), sum(d < -0.05), median(dd), WINNER)
} else if (file.exists(NATIONAL_DELTAS) &&
           "d_prec_pct_vs_persize" %in% names(read.csv(NATIONAL_DELTAS, nrows = 1))) {
  # mirror the factorial's own aggregation: per-state seed means, then the
  # median across states (review flag 5)
  nat <- read.csv(NATIONAL_DELTAS)
  nat5 <- nat %>% filter(budget == 0.05) %>% group_by(target) %>%
    summarise(dp = mean(d_prec_pct_vs_persize), .groups = "drop")
  nat_med <- median(nat5$dp)
  stopifnot(!is.na(nat_med))
  WINNER <- if (nat_med > 0) "pct" else "ps"
  stamp("RUNOFF UNREADABLE (%d paired states < %d): falling back to the national pct_vs_persize winner (median %+.4f) -> WINNER: %s",
        n_paired, MIN_PAIRED_STATES, nat_med, WINNER)
} else {
  WINNER <- "ps"
  stamp("RUNOFF UNREADABLE (%d paired states < %d) and no national pct_vs_persize deltas on disk: defaulting to ps (table-free tie-break), logged",
        n_paired, MIN_PAIRED_STATES)
}
adm_counts <- runoff %>% filter(budget == 0.05) %>%
  group_by(vocab) %>%
  summarise(states_admitting = sum(n_admitted > 0),
            median_admitted = median(n_admitted), .groups = "drop")
print(as.data.frame(adm_counts), row.names = FALSE)
writeLines(WINNER, file.path(OUT_DIR, "runoff_winner.txt"))

## ---- stage 2: production state pools with the winner ------------------------
stamp("=== stage 2: state pools with vocabulary '%s' ===", WINNER)
pool_summary <- list()
for (state in STATES) {
  fn <- file.path(POOL_DIR, sprintf("pool_%s_%s.rds", gsub(" ", "_", state),
                                    WINNER))
  if (RESUME_FROM_CHECKPOINT && file.exists(fn)) {
    pool <- readRDS(fn)
    pool_summary[[state]] <- data.frame(
      state = state, vocab = WINNER,
      n_typed = sum(pool$hh == "all"),
      n_anyerror = sum(pool$hh != "all"),
      n_total = nrow(pool))
    stamp("  %s: pool resumed (%d rules)", state, nrow(pool)); next
  }
  tr_s <- which(st == state & yr %in% TRAIN_YEARS)
  trs <- adf[tr_s, , drop = FALSE]
  # typed side: resume from the winner's stage-1 cache
  typed_pool <- mine_state_typed(state, WINNER)
  if (nrow(typed_pool)) typed_pool$hh <- "all"
  # any-error side: by HH strata
  hh_s <- hh_group_of(trs$cert_HH_size_FS_n)
  strata_s <- lapply(setNames(nm = HH_LEVELS), function(h) which(hh_s %in% h))
  ie_s <- ie_all[tr_s]
  ae <- mine_rule_vocabulary(
    trs, list(any_error = list(rows = seq_len(nrow(trs)), ie = ie_s)),
    strata_s, VOCABS[[WINNER]],
    xgb = XGB, rf = RF, signif_digits = SIGNIF_DIGITS, seed = SEED,
    verbose = TRUE)   # per-stratum rows AND events print (design note item 3)
  ae_adm <- NULL
  if (!is.null(ae) && nrow(ae)) {
    fl <- flags_for_rules(ae, trs, strata_s, label = "")
    n <- vapply(fl, length, integer(1))
    k <- vapply(fl, function(ix) sum(ie_s[ix]), numeric(1))
    base_by_hh_s <- vapply(strata_s, function(rows) mean(ie_s[rows]), numeric(1))
    # ONE BH across all strata's candidates, per-stratum base rates in the
    # p-values (review 2026-08-09 blocking item; findings 19 as shipped)
    adm <- admit_rank_frame(ae, n, k, base_by_hh_s[ae$hh])
    if (nrow(adm)) { adm$mined_frame <- "any_error"; ae_adm <- adm }
  }
  pool <- bind_rows(typed_pool,
                    if (!is.null(ae_adm)) ae_adm else NULL)
  # provenance: n/k/lcb are on the mined frame's own scale - typed rows on
  # the typed frame, any-error rows on the stratum any-error scale. A blend
  # consumer must NOT sort across scales (rule-pool-incomparability hazard)
  # and must dedup cross-frame duplicate rule texts (see pools/README.md).
  if (nrow(pool)) {
    pool$lcb_scale <- ifelse(pool$hh == "all",
                             paste0("typed:", pool$mined_frame),
                             "anyerror_stratum")
    pool$mined_frames <- NULL
  }
  saveRDS(pool, fn)
  pool_summary[[state]] <- data.frame(
    state = state, vocab = WINNER,
    n_typed = nrow(typed_pool),
    n_anyerror = if (!is.null(ae_adm)) nrow(ae_adm) else 0L,
    n_total = nrow(pool))
  stamp("  %s: %d typed + %d any-error rules -> %s", state,
        nrow(typed_pool), if (!is.null(ae_adm)) nrow(ae_adm) else 0L, fn)
  rm(trs); invisible(gc())
}
if (length(pool_summary)) {
  ps_df <- bind_rows(pool_summary)
  write.csv(ps_df, file.path(OUT_DIR, "state_pool_summary.csv"),
            row.names = FALSE)
  stamp("stage 2 done: median pool %d rules (typed median %d, any-error median %d)",
        median(ps_df$n_total), median(ps_df$n_typed), median(ps_df$n_anyerror))
}
stamp("outputs written to %s", OUT_DIR)
