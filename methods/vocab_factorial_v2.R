# National vocabulary factorial, 2x2: {16-feature baseline, adopted
# 26-feature package} x {without, with shelter_expenses_p}. Design note:
# methods/design_note_vocab_factorial_2026-08-09.md (it wins over this script
# where they could disagree). Machinery identical to
# methods/vocab_attribution_v2.R (the 2026-08-08 two-arm study), which is the
# findings-31 mine -> score -> admit -> rank -> budget-walk path; the base and
# cand arms RESUME from that study's cached mines and scored pools (identical
# frame, config, vocabulary, seeds - documented in the design note), so
# tonight mines only the six outlier-arm variants.
#
# Pre-registered contrasts, paired by seed:
#   (1) base_slt - base      (shelter feature on the baseline vocabulary)
#   (2) cand_slt - cand      (shelter feature on the adopted package)
#   (3) cand - base          (free same-seed replication of 2026-08-08)
# The shelter feature carries a POSITIVE bar (median > 0 at the 5% budget,
# not contradicted by the mean and harmed-tail companions, with real
# deployed usage): it extends the per-state cutoff tables, so do-no-harm is
# not enough (results review, 2026-08-09).
#
# SMOKE=1: plumbing check with its own cache; tiny ensembles, one seed, two
# panel states. Expects `reg_model_data`. Outputs -> methods/vocab_factorial_v2/.
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

EXPECT_ROWS_FRAME <- 231619L
EXPECT_TRAIN_ROWS <- 76031L; EXPECT_TRAIN_ERRS <- 8397L
EXPECT_TEST_ROWS  <- 39528L; EXPECT_TEST_ERRS  <- 4764L

TARGETS <- c("California", "Texas", "Michigan", "Massachusetts", "Arizona",
             "Washington", "Louisiana", "Maine", "New Jersey", "Mississippi")

BASE_FEATURES <- c(
  "HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size",
  "expedited_i", "cat_elig", "rawben_rel_max", "medical_deductions",
  "shelter_expenses_by_hh_size", "utilities", "married", "homeless",
  "percent_abawd", "unc_rawben_rel_max", "months_since_cert_n",
  "count_divisible_by_100")
PKG_FEATURES <- c(
  "gross_by_hh_size", "earned_by_hh_size", "unearned_by_hh_size",
  "rawgrinc_p", "rawearn_p", "rawunearn_p", "rawrent_p",
  "rawmedded_p", "rawcsded_p", "rawdepded_p")
SLT_FEATURE <- "shelter_expenses_p"

# the representation contest (Eric 2026-08-09, corrected: the 16 already
# carries two _by_hh_size features, so the percentile arm must REPLACE the
# per-size representation, not sit on top of it). The five per-size fields:
# total_deductions, shelter_expenses (incumbent) + gross, earned, unearned.
PS_FEATURES  <- c("gross_by_hh_size", "earned_by_hh_size", "unearned_by_hh_size")
PCT_FEATURES <- c("rawgrinc_p", "rawearn_p", "rawunearn_p", "rawrent_p",
                  "rawmedded_p", "rawcsded_p", "rawdepded_p")
stopifnot(setequal(PKG_FEATURES, c(PS_FEATURES, PCT_FEATURES)))
INCUMBENT_PS <- c("total_deductions_by_hh_size", "shelter_expenses_by_hh_size")
PCT_PURE_ADD <- c(PCT_FEATURES, "shelter_expenses_p", "total_deductions_p")

# arm name -> vocabulary; base/cand names MUST match the 2026-08-08 study's
# cache keys (identical configs) so their mines and scored pools resume;
# ps_pure / pct_pure / *_slt are new keys and mine fresh
ARMS <- list(
  base     = BASE_FEATURES,
  cand     = c(BASE_FEATURES, PKG_FEATURES),
  ps_pure  = c(BASE_FEATURES, PS_FEATURES),   # all five _by_hh_size, zero _p
  pct_pure = c(setdiff(BASE_FEATURES, INCUMBENT_PS), PCT_PURE_ADD),
  base_slt = c(BASE_FEATURES, SLT_FEATURE),
  cand_slt = c(BASE_FEATURES, PKG_FEATURES, SLT_FEATURE))
ARM_NEW <- list(base = character(0), cand = PKG_FEATURES,
                ps_pure = PS_FEATURES, pct_pure = PCT_PURE_ADD,
                base_slt = SLT_FEATURE, cand_slt = c(PKG_FEATURES, SLT_FEATURE))

# percentile column -> source column (frame names). shelter_expenses and
# rawsltexp are byte-identical on the current frame (review flag 1);
# shelter_expenses is used for naming consistency with the shipped
# shelter_expenses_by_hh_size.
PCT_MAP <- c(rawgrinc_p  = "rawgross",
             rawearn_p   = "rawearn",
             rawunearn_p = "rawunearn",
             rawmedded_p = "medical_deductions",
             rawdepded_p = "rawdepded",
             rawcsded_p  = "rawcsded",
             rawrent_p   = "rawrent",
             shelter_expenses_p = "shelter_expenses",
             total_deductions_p = "total_deductions")

OUT_DIR   <- "methods/vocab_factorial_v2"
CACHE_DIR <- "methods/vocab_attribution_v2/cache"   # shared: base/cand resume
if (!exists("RESUME_FROM_CHECKPOINT")) RESUME_FROM_CHECKPOINT <- FALSE

if (SMOKE) {
  XGB$nrounds <- 40; RF$num_trees <- 40
  SEEDS <- c(117)
  TOPK_WINDOW <- 2000
  TARGETS <- c("Washington", "Maine")
  OUT_DIR <- file.path(OUT_DIR, "smoke")
  CACHE_DIR <- file.path(OUT_DIR, "cache")   # never touch the full-run cache
}
dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

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
stamp("frozen train-year percentiles rebuilt for %d columns", length(PCT_MAP))

pf  <- prep_features(adf0, unique(unlist(ARMS)))
adf <- pf$data
for (arm in names(ARMS)) {
  miss <- setdiff(ARMS[[arm]], pf$features)
  if (length(miss))
    stop(sprintf("arm %s: features missing after prep: %s",
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
stamp("train %d rows / %d errors | FY%s %d rows / %d errors | strata base rates %s",
      nrow(train), sum(ie_tr), TEST_YEAR, length(te_rows), sum(ie_all[te_rows]),
      paste(sprintf("%s=%.4f", names(base_by_hh), base_by_hh), collapse = " "))

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
run_info <- bind_rows(run_info)
write.csv(run_info, file.path(OUT_DIR, "factorial_run_info.csv"),
          row.names = FALSE)

# resume anchor (review flag 5): the resumed base/cand pools must reproduce
# the 2026-08-08 study's admitted counts and top rules exactly
if (!SMOKE) {
  prev <- read.csv("methods/vocab_attribution_v2/vocab_attribution_run_info.csv",
                   stringsAsFactors = FALSE)
  chk <- merge(run_info[run_info$arm %in% c("base", "cand"), ],
               prev, by = c("arm", "seed"), suffixes = c("", "_prev"))
  if (nrow(chk) != 6L ||
      any(chk$n_admitted != chk$n_admitted_prev) ||
      any(chk$top_rule != chk$top_rule_prev))
    stop("RESUME ANCHOR FAILED: base/cand pools do not reproduce the 2026-08-08 run_info; the shared cache or config has drifted. STOPPING.")
  stamp("resume anchor PASSED: base/cand admitted counts and top rules match the 2026-08-08 study for all 6 pools")
}

## ---- new-feature usage per arm ----------------------------------------------
usage <- list()
for (arm in names(ARMS)) {
  nf <- ARM_NEW[[arm]]
  if (!length(nf)) next
  for (seed in SEEDS) {
    adm <- pools[[paste(arm, seed, sep = "_")]]
    win_n <- min(TOPK_WINDOW, nrow(adm))
    for (ft in nf) {
      hit <- grepl(paste0("\\b", ft, "\\b"), adm$rule, perl = TRUE)
      usage[[length(usage) + 1]] <- data.frame(
        arm = arm, seed = seed, feature = ft,
        n_admitted_rules = sum(hit),
        best_rank = if (any(hit)) which(hit)[1] else NA_integer_,
        n_in_window = sum(hit[seq_len(win_n)]))
    }
  }
}
usage <- bind_rows(usage)
write.csv(usage, file.path(OUT_DIR, "factorial_feature_usage.csv"),
          row.names = FALSE)

## ---- budget readout: legacy findings-31 walk on the panel -------------------
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
  list(hit = un24, slack = cap_buf - n_in, deployed = c(frozen, buffer))
}

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
    arm <- sub("_[0-9]+$", "", key)
    seed <- as.integer(sub(".*_", "", key))
    w <- windows[[key]]
    idx_tr <- flags_for_rules(w, trs, strata_trs, label = "")
    idx_te <- flags_for_rules(w, tes, strata_tes, label = "")
    nf <- ARM_NEW[[arm]]
    nf_re <- if (length(nf)) paste0("\\b(", paste(nf, collapse = "|"), ")\\b") else NULL
    for (b in BUDGETS) {
      wm <- walk_mask(idx_tr, idx_te, nrow(trs), nrow(tes), b)
      if (wm$slack > 0)
        stamp("  WARNING %s %s b=%.2f: slack %d (window may bind; findings 27)",
              target, key, b, wm$slack)
      fl <- wm$hit
      dep_rules <- w$rule[wm$deployed]
      readout[[length(readout) + 1]] <- data.frame(
        target = target, arm = arm, seed = seed, budget = b,
        n_te = nrow(tes), n_err_te = sum(err_te),
        n_flagged = sum(fl), n_errors_caught = sum(fl & err_te),
        precision = round(sum(fl & err_te) / max(sum(fl), 1), 4),
        base_rate_te = round(mean(err_te), 4),
        dollar_recall = round(sum(doll_te[fl]) / max(sum(doll_te), 1), 4),
        artifact_share_flagged = round(sum(fl & band_te) / max(sum(fl), 1), 4),
        n_rules_deployed = length(dep_rules),
        n_rules_deployed_newfeat = if (is.null(nf_re)) 0L else
          sum(grepl(nf_re, dep_rules, perl = TRUE)),
        n_rules_deployed_slt =
          sum(grepl("\\bshelter_expenses_p\\b", dep_rules, perl = TRUE)),
        slack = wm$slack)
    }
    rm(idx_tr, idx_te); invisible(gc())
  }
  stamp("  %s done", target)
}
readout <- bind_rows(readout)
write.csv(readout, file.path(OUT_DIR, "factorial_budget_readout.csv"),
          row.names = FALSE)

## ---- paired contrasts and summary -------------------------------------------
wide <- readout %>%
  select(target, arm, seed, budget, precision, dollar_recall) %>%
  tidyr::pivot_wider(names_from = arm,
                     values_from = c(precision, dollar_recall))
CONTRASTS <- list(
  slt_on_base = c("base_slt", "base"),
  slt_on_cand = c("cand_slt", "cand"),
  pkg_replication = c("cand", "base"),
  persize_on_base = c("ps_pure", "base"),
  pct_replaces_persize_vs_base = c("pct_pure", "base"),
  pct_vs_persize = c("pct_pure", "ps_pure"))
for (cn in names(CONTRASTS)) {
  a <- CONTRASTS[[cn]][1]; bnm <- CONTRASTS[[cn]][2]
  wide[[paste0("d_prec_", cn)]] <-
    wide[[paste0("precision_", a)]] - wide[[paste0("precision_", bnm)]]
  wide[[paste0("d_doll_", cn)]] <-
    wide[[paste0("dollar_recall_", a)]] - wide[[paste0("dollar_recall_", bnm)]]
}
write.csv(wide, file.path(OUT_DIR, "factorial_paired_deltas.csv"),
          row.names = FALSE)

noise <- readout %>%
  group_by(arm, budget, target) %>%
  summarise(spread = max(precision) - min(precision), .groups = "drop") %>%
  group_by(arm, budget) %>%
  summarise(median_seed_spread = round(median(spread), 4), .groups = "drop")
write.csv(noise, file.path(OUT_DIR, "factorial_seed_noise.csv"),
          row.names = FALSE)

# sign-consistency companion (results review, 2026-08-09): per state and
# contrast, count of negative / positive paired cells across seeds x budgets
signs <- list()
for (cn in names(CONTRASTS)) {
  dc <- paste0("d_prec_", cn)
  s <- wide %>% group_by(target) %>%
    summarise(contrast = cn, n_cells = n(),
              n_neg = sum(.data[[dc]] < 0), n_pos = sum(.data[[dc]] > 0),
              mean_delta = round(mean(.data[[dc]]), 4), .groups = "drop")
  signs[[cn]] <- s
}
signs <- bind_rows(signs)
write.csv(signs, file.path(OUT_DIR, "factorial_sign_consistency.csv"),
          row.names = FALSE)

stamp("=== factorial summary (%d seeds x %d states) ===",
      length(SEEDS), length(TARGETS))
for (cn in names(CONTRASTS)) {
  dc <- paste0("d_prec_", cn); dd <- paste0("d_doll_", cn)
  for (b in BUDGETS) {
    ps <- wide %>% filter(budget == b) %>% group_by(target) %>%
      summarise(dp = mean(.data[[dc]]), ddl = mean(.data[[dd]]), .groups = "drop")
    stamp("%s @ %.0f%%: median %+.4f | mean %+.4f | harmed (< -0.05) %d of %d | median d dollars %+.4f",
          cn, 100 * b, median(ps$dp), mean(ps$dp),
          sum(ps$dp < -0.05), nrow(ps), median(ps$ddl))
  }
}
nz5 <- noise %>% filter(budget == 0.05)
stamp("seed-noise reference @5%% (median within-arm spread): %s",
      paste(sprintf("%s %.4f", nz5$arm, nz5$median_seed_spread), collapse = " | "))
u <- usage %>% filter(feature == SLT_FEATURE) %>%
  group_by(arm) %>%
  summarise(rules = round(mean(n_admitted_rules)),
            best = suppressWarnings(min(best_rank, na.rm = TRUE)),
            win = round(mean(n_in_window)), .groups = "drop")
if (nrow(u))
  stamp("shelter_expenses_p usage: %s",
        paste(sprintf("%s: %d admitted (best rank %s, %d in window)",
                      u$arm, u$rules, u$best, u$win), collapse = " | "))
dep <- readout %>% filter(arm %in% c("base_slt", "cand_slt")) %>%
  mutate(share_slt = n_rules_deployed_slt / pmax(n_rules_deployed, 1))
stamp("deployed shelter-rule share: %s; %d of %d slt-arm cells deploy at least one shelter rule",
      paste(sprintf("%s %.1f%%", c("base_slt", "cand_slt"),
                    100 * vapply(c("base_slt", "cand_slt"), function(a)
                      median(dep$share_slt[dep$arm == a]), numeric(1))),
            collapse = " | "),
      sum(dep$n_rules_deployed_slt > 0), nrow(dep))
n_slack <- sum(readout$slack > 0)
stamp("slack check (findings 27): %d of %d cells with slack > 0%s",
      n_slack, nrow(readout),
      if (n_slack > 0) " - INVALID BY CONSTRUCTION, re-walk those cells with a larger window" else "")
stamp("outputs written to %s", OUT_DIR)
