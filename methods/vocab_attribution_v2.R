# Vocabulary attribution: does the Eric-confirmed 26-feature mining vocabulary
# beat the shipped-in-practice 16-feature vocabulary in the delivered budget
# lists, on a true future year, beyond seed noise?
#
# Design note: methods/design_note_vocab_attribution_2026-08-08.md (the note
# wins where this script and the note could disagree). Machinery adapted from
# methods/seed_stability_v2.R (findings 31): same mine -> score -> admit ->
# rank -> budget-walk path, same ten-state panel, same legacy walk, so the
# findings-31 seed-noise yardstick is like-for-like.
#
# Exactly one component varies: the mining feature vocabulary (16 vs 26).
# Both arms run on the SAME rebuilt frame (2026-08-08), same engines, same
# admission (BH FDR 10% + n >= 30), same 99% LCB ordering and tie-break,
# same seeds (117, 20260805, 31415), paired across arms.
#
# The seven percentile features are re-fit on FY2022-23 ONLY and frozen
# (per state x reported-HH-size cell empirical CDFs of CPI-deflated values,
# zeros pinned to 0, non-zeros ranked among non-zeros; Ben's construction
# with a train-only fit). The frame's as-built _p columns rank across all
# years (test-year leakage) and are OVERWRITTEN here before mining.
#
# Heavy train scoring goes through reduce_flags_for_rules (the unpatched
# evaluator OOMs this 29 GB host at national scale; hazard row). Mines and
# scored pools checkpoint to CACHE_DIR; set RESUME_FROM_CHECKPOINT <- TRUE
# in the runner so a killed run resumes.
#
# SMOKE=1 (environment): plumbing check - tiny ensembles, one seed, two
# panel states, small window; outputs under methods/vocab_attribution_v2/smoke/.
#
# Expects `reg_model_data`. Outputs -> methods/vocab_attribution_v2/ (CSVs).
# No writes to state_delivery_lists/, no CHANGELOG entry, no version bump.

suppressMessages(library(dplyr))
source("rule_mining_helpers.R")

## ---- config -----------------------------------------------------------------
SMOKE <- identical(Sys.getenv("SMOKE"), "1")

SEEDS       <- c(117, 20260805, 31415)
BUDGETS     <- c(0.05, 0.10)
TOPK_WINDOW <- 20000
BUFFER_MULT <- 3
LCB_Z       <- 2.326
FDR_ALPHA   <- 0.10
MIN_N       <- 30
TRAIN_YEARS <- c("2022", "2023")
TEST_YEAR   <- "2024"
XGB <- list(nrounds = 1000, max_depth = 4, eta = 0.02, subsample = 0.20)
RF  <- list(num_trees = 1000, max_depth = 4, mtry = 2, min_node_size = 20)
SIGNIF_DIGITS <- 3

# frame invariants, rebuilt frame of 2026-08-08 (design note item 3)
EXPECT_ROWS_FRAME <- 231619L
EXPECT_TRAIN_ROWS <- 76031L; EXPECT_TRAIN_ERRS <- 8397L
EXPECT_TEST_ROWS  <- 39528L; EXPECT_TEST_ERRS  <- 4764L

# the section 30/31 panel
TARGETS <- c("California", "Texas", "Michigan", "Massachusetts", "Arizona",
             "Washington", "Louisiana", "Maine", "New Jersey", "Mississippi")

# the 16 features every v2 mine has actually used (the three raw*_by_hh_size
# names in the finder vectors are absent from the frame and were silently
# dropped by prep_features; Gate-1 session 2026-08-08)
BASE_FEATURES <- c(
  "HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size",
  "expedited_i", "cat_elig", "rawben_rel_max", "medical_deductions",
  "shelter_expenses_by_hh_size", "utilities", "married", "homeless",
  "percent_abawd", "unc_rawben_rel_max", "months_since_cert_n",
  "count_divisible_by_100")
NEW_FEATURES <- c(
  "gross_by_hh_size", "earned_by_hh_size", "unearned_by_hh_size",
  "rawgrinc_p", "rawearn_p", "rawunearn_p", "rawrent_p",
  "rawmedded_p", "rawcsded_p", "rawdepded_p")
ARMS <- list(base = BASE_FEATURES, cand = c(BASE_FEATURES, NEW_FEATURES))

# frozen-percentile construction: percentile column -> source dollar column
# (frame names; rawusize -> cert_HH_size_FS_n, rawgrinc -> rawgross,
# rawmedded -> medical_deductions per the munging rename block)
PCT_MAP <- c(rawgrinc_p  = "rawgross",
             rawearn_p   = "rawearn",
             rawunearn_p = "rawunearn",
             rawmedded_p = "medical_deductions",
             rawdepded_p = "rawdepded",
             rawcsded_p  = "rawcsded",
             rawrent_p   = "rawrent")

OUT_DIR <- "methods/vocab_attribution_v2"
if (!exists("RESUME_FROM_CHECKPOINT")) RESUME_FROM_CHECKPOINT <- FALSE

if (SMOKE) {
  XGB$nrounds <- 40; RF$num_trees <- 40
  SEEDS <- c(117)
  TOPK_WINDOW <- 2000
  TARGETS <- c("Washington", "Maine")
  OUT_DIR <- file.path(OUT_DIR, "smoke")
}
CACHE_DIR <- file.path(OUT_DIR, "cache")
dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)

HH_LEVELS <- c("1", "2-3", "4+")
hh_group_of <- function(n) {
  n <- suppressWarnings(as.numeric(as.character(n)))
  ifelse(is.na(n), NA_character_, ifelse(n <= 1, "1", ifelse(n <= 3, "2-3", "4+")))
}
strata_of <- function(df) lapply(setNames(nm = HH_LEVELS), function(h)
  which(hh_group_of(df$cert_HH_size_FS_n) %in% h))
stamp <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"),
                                   sprintf(...)))

## ---- frame ------------------------------------------------------------------
stopifnot(nrow(reg_model_data) == EXPECT_ROWS_FRAME)

## frozen train-year percentiles, overwriting the as-built all-years columns.
## fit: FY2022-23 CPI-deflated non-zero values per state x reported-HH-size
## cell; apply: share of the FROZEN fit <= the (deflated) value; zeros -> 0.
## fallback for a cell with no non-zero train values: state-level fit, then
## national. Source NAs are a hard stop (design note deviation (c)).
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
    v <- df[[src]]
    if (any(is.na(v)))
      stop(sprintf("NA in percentile source %s (%d rows) - hard stop, design note (c)",
                   src, sum(is.na(v))))
    x <- v / defl
    nz_tr <- is_tr & v != 0
    fit_cell  <- lapply(split(x[nz_tr], cell[nz_tr]),  sort)
    fit_state <- lapply(split(x[nz_tr], state[nz_tr]), sort)
    fit_nat   <- sort(x[nz_tr])
    out <- numeric(nrow(df))   # zeros stay 0
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
stamp("frozen train-year percentiles rebuilt for %d columns (train fit rows: %d)",
      length(PCT_MAP), sum(as.character(adf0$fiscal_year) %in% TRAIN_YEARS))

pf  <- prep_features(adf0, unique(unlist(ARMS)))
adf <- pf$data
for (arm in names(ARMS)) {
  miss <- setdiff(ARMS[[arm]], pf$features)
  if (length(miss))
    stop(sprintf("arm %s: features missing after prep: %s - the study's contrast requires all of them",
                 arm, paste(miss, collapse = ", ")))
}
st <- as.character(adf$state)
yr <- as.character(adf$fiscal_year)
ie_all <- !is.na(adf$over_threshold) & adf$over_threshold != 0
ed_all <- ifelse(ie_all, abs(ifelse(is.na(adf$total_error_amount), 0,
                                    adf$total_error_amount)), 0)

tr_rows <- which(yr %in% TRAIN_YEARS)
train   <- adf[tr_rows, , drop = FALSE]
ie_tr   <- ie_all[tr_rows]
strata_tr <- strata_of(train)
base_by_hh <- vapply(strata_tr, function(rows) mean(ie_tr[rows]), numeric(1))

te_rows <- which(yr == TEST_YEAR)
stopifnot(nrow(train) == EXPECT_TRAIN_ROWS, sum(ie_tr) == EXPECT_TRAIN_ERRS,
          length(te_rows) == EXPECT_TEST_ROWS,
          sum(ie_all[te_rows]) == EXPECT_TEST_ERRS)
stamp("train %d rows / %d errors | FY%s %d rows / %d errors | %d states | strata base rates %s",
      nrow(train), sum(ie_tr), TEST_YEAR, length(te_rows), sum(ie_all[te_rows]),
      length(unique(st)), paste(sprintf("%s=%.4f", names(base_by_hh), base_by_hh),
                                collapse = " "))

## ---- per (arm, seed): mine -> score -> admit -> rank ------------------------
mine_one <- function(arm, seed) {
  fn <- file.path(CACHE_DIR, sprintf("mine_%s_%d.rds", arm, seed))
  if (RESUME_FROM_CHECKPOINT && file.exists(fn)) {
    rdf <- readRDS(fn)
    stamp("%s seed %d: resumed mine (%d raw rules)", arm, seed, nrow(rdf))
    return(rdf)
  }
  stamp("%s seed %d: mining (%d features) ...", arm, seed, length(ARMS[[arm]]))
  rdf <- mine_rule_vocabulary(
    train, list(any_error = list(rows = seq_len(nrow(train)), ie = ie_tr)),
    strata_tr, ARMS[[arm]], xgb = XGB, rf = RF,
    signif_digits = SIGNIF_DIGITS, seed = seed)
  saveRDS(rdf, fn)
  stamp("%s seed %d: mined %d raw rules -> %s", arm, seed, nrow(rdf), fn)
  rdf
}

score_one <- function(arm, seed, rdf) {
  fn <- file.path(CACHE_DIR, sprintf("scored_%s_%d.rds", arm, seed))
  if (RESUME_FROM_CHECKPOINT && file.exists(fn)) {
    stamp("%s seed %d: resumed scored pool", arm, seed)
    return(readRDS(fn))
  }
  stats <- reduce_flags_for_rules(
    rdf, train, strata_tr,
    fun = function(ix) c(length(ix), sum(ie_tr[ix])),
    label = sprintf("train-score %s seed %d", arm, seed))
  rdf$n <- as.integer(stats[, 1])
  rdf$k <- stats[, 2]
  saveRDS(rdf, fn)
  rdf
}

# admission and ranking exactly as seed_stability_v2.R (BH at FDR 10% vs the
# per-rule stratum base rate AND n >= MIN_N; pooled-national 99% LCB order;
# seed-independent tie-break)
admit_and_rank <- function(arm, seed, rdf) {
  pvals <- pbinom(rdf$k - 1, rdf$n, base_by_hh[rdf$hh], lower.tail = FALSE)
  m <- length(pvals); o <- order(pvals)
  thr <- max(c(0L, which(pvals[o] <= FDR_ALPHA * seq_len(m) / m)))
  bh <- rep(FALSE, m); if (thr > 0) bh[o[seq_len(thr)]] <- TRUE
  keep <- bh & rdf$n >= MIN_N
  adm <- rdf[keep, , drop = FALSE]
  adm$lcb <- wilson_lcb(adm$k, adm$n, LCB_Z)
  adm <- adm[order(-adm$lcb, -adm$n, adm$hh, adm$rule, method = "radix"), ,
             drop = FALSE]
  rownames(adm) <- NULL
  stamp("%s seed %d: %d of %d admitted; top: [hh %s, lcb %.4f, n %d] %s",
        arm, seed, nrow(adm), nrow(rdf), adm$hh[1], adm$lcb[1], adm$n[1],
        adm$rule[1])
  adm
}

pools <- list(); run_info <- list()
for (arm in names(ARMS)) for (seed in SEEDS) {
  key <- paste(arm, seed, sep = "_")
  rdf <- mine_one(arm, seed)
  rdf <- score_one(arm, seed, rdf)
  adm <- admit_and_rank(arm, seed, rdf)
  pools[[key]] <- adm
  run_info[[key]] <- data.frame(
    arm = arm, seed = seed, n_features = length(ARMS[[arm]]),
    n_raw_rules = nrow(rdf), n_admitted = nrow(adm),
    top_rule_hh = adm$hh[1], top_rule_lcb = round(adm$lcb[1], 4),
    top_rule = adm$rule[1], stringsAsFactors = FALSE)
  rm(rdf); invisible(gc())
}
write.csv(bind_rows(run_info), file.path(OUT_DIR, "vocab_attribution_run_info.csv"),
          row.names = FALSE)

## ---- new-feature usage in the candidate pools -------------------------------
usage <- list()
for (seed in SEEDS) {
  adm <- pools[[paste("cand", seed, sep = "_")]]
  win_n <- min(TOPK_WINDOW, nrow(adm))
  for (ft in NEW_FEATURES) {
    hit <- grepl(paste0("\\b", ft, "\\b"), adm$rule, perl = TRUE)
    usage[[length(usage) + 1]] <- data.frame(
      seed = seed, feature = ft,
      n_admitted_rules = sum(hit),
      best_rank = if (any(hit)) which(hit)[1] else NA_integer_,
      n_in_window = sum(hit[seq_len(win_n)]))
  }
}
usage <- bind_rows(usage)
write.csv(usage, file.path(OUT_DIR, "vocab_attribution_feature_usage.csv"),
          row.names = FALSE)

## ---- budget readout: the section 30/31 panel, legacy walk -------------------
# per pool: fill on the state's FY2022-23 caseload to core + 3x buffer in rank
# order (outcome-free), freeze, walk against FY2024 to that year's cap
# (walk_mask as in seed_stability_v2.R; findings 27 slack watched)
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
  # frozen/buffer rule identities kept so deployed-list feature usage is
  # measurable (review flag 1, 2026-08-08)
  list(hit = un24, slack = cap_buf - n_in, deployed = c(frozen, buffer))
}

NEW_FEAT_RE <- paste0("\\b(", paste(NEW_FEATURES, collapse = "|"), ")\\b")

windows <- lapply(pools, function(adm)
  adm[seq_len(min(TOPK_WINDOW, nrow(adm))), , drop = FALSE])

readout <- list()
for (target in TARGETS) {
  tr_s <- which(st == target & yr %in% TRAIN_YEARS)
  te_s <- which(st == target & yr == TEST_YEAR)
  trs <- adf[tr_s, , drop = FALSE]; tes <- adf[te_s, , drop = FALSE]
  strata_trs <- strata_of(trs); strata_tes <- strata_of(tes)
  err_te  <- ie_all[te_s]
  doll_te <- ed_all[te_s]
  band_te <- !is.na(tes$rawben_rel_max) &
    tes$rawben_rel_max >= 0.987 & tes$rawben_rel_max < 1
  for (key in names(pools)) {
    w <- windows[[key]]
    idx_tr <- flags_for_rules(w, trs, strata_trs, label = "")
    idx_te <- flags_for_rules(w, tes, strata_tes, label = "")
    for (b in BUDGETS) {
      wm <- walk_mask(idx_tr, idx_te, nrow(trs), nrow(tes), b)
      if (wm$slack > 0)
        stamp("  WARNING %s %s b=%.2f: slack %d (window may bind; findings 27)",
              target, key, b, wm$slack)
      fl <- wm$hit
      dep_rules <- w$rule[wm$deployed]
      readout[[length(readout) + 1]] <- data.frame(
        target = target, arm = sub("_.*", "", key),
        seed = as.integer(sub(".*_", "", key)), budget = b,
        n_te = nrow(tes), n_err_te = sum(err_te),
        n_flagged = sum(fl), n_errors_caught = sum(fl & err_te),
        precision = round(sum(fl & err_te) / max(sum(fl), 1), 4),
        base_rate_te = round(mean(err_te), 4),
        dollar_recall = round(sum(doll_te[fl]) / max(sum(doll_te), 1), 4),
        artifact_share_flagged = round(sum(fl & band_te) / max(sum(fl), 1), 4),
        n_rules_deployed = length(dep_rules),
        n_rules_deployed_newfeat = sum(grepl(NEW_FEAT_RE, dep_rules, perl = TRUE)),
        slack = wm$slack)
    }
    rm(idx_tr, idx_te); invisible(gc())
  }
  stamp("  %s done", target)
}
readout <- bind_rows(readout)
write.csv(readout, file.path(OUT_DIR, "vocab_attribution_budget_readout.csv"),
          row.names = FALSE)

## ---- paired deltas and the pre-registered summary ---------------------------
wide <- readout %>%
  select(target, arm, seed, budget, precision, dollar_recall) %>%
  tidyr::pivot_wider(names_from = arm,
                     values_from = c(precision, dollar_recall))
wide$d_precision <- wide$precision_cand - wide$precision_base
wide$d_dollar    <- wide$dollar_recall_cand - wide$dollar_recall_base
write.csv(wide, file.path(OUT_DIR, "vocab_attribution_paired_deltas.csv"),
          row.names = FALSE)

# per-state seed-mean deltas; companions per the mandatory-shipping rule
per_state <- wide %>%
  group_by(target, budget) %>%
  summarise(d_precision = mean(d_precision), d_dollar = mean(d_dollar),
            .groups = "drop")

# within-arm seed noise on the SAME metric: median across states of the
# max-minus-min across seeds of FY2024 precision, per arm and budget
noise <- readout %>%
  group_by(arm, budget, target) %>%
  summarise(spread = max(precision) - min(precision), .groups = "drop") %>%
  group_by(arm, budget) %>%
  summarise(median_seed_spread = round(median(spread), 4), .groups = "drop")
write.csv(noise, file.path(OUT_DIR, "vocab_attribution_seed_noise.csv"),
          row.names = FALSE)

stamp("=== vocabulary attribution summary (%d seeds x %d states) ===",
      length(SEEDS), length(TARGETS))
for (b in BUDGETS) {
  ps <- per_state %>% filter(budget == b)
  stamp("budget %.0f%%: paired delta precision median %+.4f | mean %+.4f | harmed tail (< -0.05) %d of %d | delta dollar-recall median %+.4f",
        100 * b, median(ps$d_precision), mean(ps$d_precision),
        sum(ps$d_precision < -0.05), nrow(ps), median(ps$d_dollar))
  nz <- noise %>% filter(budget == b)
  stamp("  seed-noise reference (median within-arm across-seed precision spread): %s",
        paste(sprintf("%s %.4f", nz$arm, nz$median_seed_spread), collapse = " | "))
}
u <- usage %>% group_by(feature) %>%
  summarise(rules = round(mean(n_admitted_rules)), best = min(best_rank),
            .groups = "drop") %>% arrange(best)
stamp("new-feature usage (mean admitted rules across seeds; best rank): %s",
      paste(sprintf("%s %d@%s", u$feature, u$rules, u$best), collapse = ", "))
dep <- readout %>% filter(arm == "cand") %>%
  mutate(share_new = n_rules_deployed_newfeat / pmax(n_rules_deployed, 1))
stamp("deployed-list usage (cand arm): median %.1f%% of deployed rules use a new feature; %d of %d cells deploy at least one",
      100 * median(dep$share_new), sum(dep$n_rules_deployed_newfeat > 0), nrow(dep))
n_slack <- sum(readout$slack > 0)
stamp("slack check (findings 27): %d of %d cells with slack > 0%s",
      n_slack, nrow(readout),
      if (n_slack > 0) " - INVALID BY CONSTRUCTION, re-run those cells from cache with a larger window before reading them" else "")
stamp("outputs written to %s", OUT_DIR)
