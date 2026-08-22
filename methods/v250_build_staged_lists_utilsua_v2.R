# v2.5.0 CANDIDATE build - STAGED, not shipped. Specified 2026-08-11:
# for each state, blend the per-size (19-variable) state any-error rules with
# the national rules mined on the same vocabulary, deliver frozen budget lists
# with characterization columns (joined by step 2/3 of the runner).
#
# STAGED in methods/v250_candidate_lists/. Promotion to state_delivery_lists/
# plus CHANGELOG and version bump is decided at review; nothing
# here touches the shipped v2.4.0 deliverable.
#
# Recipe (decided 2026-08-11): mine FRESH on ALL public years
# FY2022-24 (the shipped no-holdout delivery recipe, findings 15-16); the
# per-size vocabulary (16 shipped-in-practice + gross/earned/unearned per
# size = 19; the exploratory Â§Â§35-37 record informs this choice); any_error
# frame x coarse HH strata; NATIONAL pool + one pool PER STATE; admission
# ONE joint BH FDR 10% per mining unit with per-stratum base rates in the
# p-values AND n >= 30; 99%-LCB ordering; blend state + national on the one
# LCB scale (shipped builder semantics: sort, dedup by (hh, rule) keeping
# the higher-LCB copy); fill each state's FY2022-24 caseload to 5%/10% core
# + 3x buffer under the shipped fresh-share walk (f = 0.50) with the
# state-scale tolerated-gap treatment (Â§37 machinery record: exact refill
# is empirical-only at small pool scale; gaps reported per phase, never a
# crash). No outcome data enters the walk. Seed 117.
#
# ARTIFACT CHECK (2026-08-12, statistician's thresholds under our
# dominance/displacement/removal-invariance criterion): every rule is
# scored for its train-flag and train-error concentration on
# reconstruction-failure rows ("mismatch rows": recorded benefit at or
# above the max while the reconstructed uncapped benefit lands below it).
# A rule with >= 0.25 concentration on either is tagged as artifact.
# DISPLACEMENT HALT: if tagged rules exceed 2% of any admitted pool, or
# more than 1 sits in the national top 40 by LCB, or any in the top 10,
# the run STOPS before any list is built (upstream displacement is not
# repairable post hoc). Otherwise tagged rules are dropped BEFORE the
# fill, so shipped lists are artifact-free by construction; the mm_share
# and mm_inflation audit columns ride on every list. Mismatch definition
# (the conservative >= variant): recorded rawben >= benmax with
# rawben_uncapped < benmax - 568 rows of 44,029 recorded at-or-above-max
# on the 2026-08-12 rebuilt frame (the strict == variant is 492 of
# 43,690; both asserted small at runtime). Gate scope calibrated after
# two smoke misfires (statistician rulings, 2026-08-12): pool-share gate
# on FLAG-tagged rules per pool; head gates (top40/top10, either tag) on
# the NATIONAL pool and each state's shadow BLEND head only.
# ILLINOIS: the 2026-08-12 state-pool hold was LIFTED 2026-08-13 - the
# Illinois standard-deduction offset fix landed (IL applies $7, $4 from
# FY2025, below the federal table; the source of its +$2-3 recreated-high
# drift on 26% of clean cases). HOLD_STATE_POOLS is empty; Illinois
# blends like every other state.
#
# SMOKE=1: 3 states + national, tiny ensembles, own output dir.
# Expects `reg_model_data`. Outputs -> methods/v250_candidate_lists/.

suppressMessages(library(dplyr))
source("rule_mining_helpers.R")

SMOKE <- identical(Sys.getenv("SMOKE"), "1")

SEED        <- 117
BUDGETS     <- c(0.05, 0.10)
BUFFER_MULT <- 3
FRESH_MIN   <- 0.50
LCB_Z       <- 2.326
FDR_ALPHA   <- 0.10
MIN_N       <- 30
YEARS       <- c("2022", "2023", "2024")   # ALL public years; no holdout
XGB <- list(nrounds = 1000, max_depth = 4, eta = 0.02, subsample = 0.20)
RF  <- list(num_trees = 1000, max_depth = 4, mtry = 2, min_node_size = 20)
SIGNIF_DIGITS <- 3

EXPECT_ROWS  <- 115559L    # FY2022-24; row/error counts are fix-invariant
EXPECT_ERRS  <- 13161L     # (verified on the 2026-08-12 rebuilt frame)

BASE_FEATURES <- c(
  "HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size",
  "expedited_i", "bbce_state_i", "rawben_rel_max", "medical_deductions",
  "shelter_expenses_by_hh_size", "utilities_sua", "married", "homeless",
  "percent_abawd", "unc_rawben_rel_max", "months_since_cert_n",
  "count_divisible_by_100")
# bbce_state_i replaces the raw 4-level cat_elig (decided 2026-08-13): the
# FY2024 codebook recode made 1-vs-2 splits era-markers; the state-level
# Broad-Based Categorical Eligibility flag is the era-stable regime signal
PS_FEATURES <- c("gross_by_hh_size", "earned_by_hh_size", "unearned_by_hh_size")
VOCAB19 <- c(BASE_FEATURES, PS_FEATURES)
# 0/1 indicators canonicalized to "var >= 1" / "var <= 0" at mine time
BINARY_FEATURES <- c("children_i", "elderly_disabled_i", "expedited_i",
                     "married", "homeless", "bbce_state_i")

OUT_DIR <- "methods/v250_candidate_lists_utilsua"
if (!exists("RESUME_FROM_CHECKPOINT")) RESUME_FROM_CHECKPOINT <- FALSE
if (SMOKE) {
  XGB$nrounds <- 40; RF$num_trees <- 40
  OUT_DIR <- file.path(OUT_DIR, "smoke")
}
CACHE_DIR <- file.path(OUT_DIR, "cache")
dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)

HH_LEVELS <- c("1", "2-3", "4+")
hh_group_of <- function(n) {
  n <- suppressWarnings(as.numeric(as.character(n)))
  ifelse(is.na(n), NA_character_, ifelse(n <= 1, "1", ifelse(n <= 3, "2-3", "4+")))
}
stamp <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"),
                                   sprintf(...)))

## ---- frame ------------------------------------------------------------------
stopifnot(nrow(reg_model_data) == 231619L)
adf0 <- reg_model_data %>% filter(fiscal_year %in% YEARS)
# UTILITIES-SUA TIER VARIANT (2026-08-22; design + result in
# methods/v250_benchmark_2024_utilrel/): same construction as the
# benchmark driver, applied per state-year on the all-years slice.
# PROMOTED 2026-08-22: utilities_sua is a frame column (features.R
# add_sua_tier); the driver reads it rather than recomputing.
stopifnot("utilities_sua" %in% names(adf0))
pf  <- prep_features(adf0, VOCAB19)
adf <- pf$data
miss <- setdiff(VOCAB19, pf$features)
if (length(miss)) stop("features missing after prep: ", paste(miss, collapse = ", "))
st <- as.character(adf$state)
yr <- as.character(adf$fiscal_year)
ie_all <- !is.na(adf$over_threshold) & adf$over_threshold != 0
ed_all <- ifelse(ie_all, abs(ifelse(is.na(adf$total_error_amount), 0,
                                    adf$total_error_amount)), 0)
stopifnot(nrow(adf) == EXPECT_ROWS, sum(ie_all) == EXPECT_ERRS)
hh_all <- hh_group_of(adf$cert_HH_size_FS_n)
# mismatch rows: reconstruction-failure population (see header)
mm_all <- adf$rawben >= adf$benmax & adf$rawben_uncapped < adf$benmax
stamp("mismatch rows on this frame: %d (%.2f%% of rows, %.2f%% of errors)",
      sum(mm_all), 100 * mean(mm_all), 100 * sum(mm_all & ie_all) / sum(ie_all))
stopifnot(sum(mm_all) < 1000)   # the post-fix-frame assert: thousands = pre-fix frame
MM_TAG_SHARE  <- 0.25   # identification bar (either flags or errors)
MM_POOL_MAX   <- 0.02   # displacement: max tagged share of any admitted pool
MM_TOP40_MAX  <- 1L     # displacement: max tagged in national top 40 by LCB
MM_TOP10_MAX  <- 0L     # displacement: none in the national top 10
HOLD_STATE_POOLS <- character(0)  # Illinois hold LIFTED 2026-08-13 (the IL standard-deduction offset fix landed and is verified by the recon diagnostics)

STATES <- sort(unique(st))
if (SMOKE) STATES <- c("Washington", "Maine", "Mississippi", "Illinois")
stamp("frame ready: FY2022-24, %d rows, %d errors, %d states",
      nrow(adf), sum(ie_all), length(STATES))

## ---- shared: admission ------------------------------------------------------
admit_rank <- function(rdf, n, k, doll, base_rate) {
  # ONE joint BH at FDR 10% across THIS mining unit's candidates,
  # per-stratum base rates inside the p-values, AND n >= MIN_N; LCB order
  rdf$n <- as.integer(n); rdf$k <- k; rdf$doll <- doll
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

# artifact tagging + the displacement gate, applied to every admitted pool.
# mm_n/mm_k must ride on the pool as columns before this is called.
# Calibration ruling (statistician, 2026-08-12 smoke): the pool-share gate
# computes over FLAG-share-tagged rules only (mining crowd-out is the
# signature of flag-concentrated regions); error-share tags identify
# measurement CONTAMINATION (precision inflated by mismatch-row errors) -
# grounds for dropping pre-fill and reporting, never for halting on pool
# share. Top-rank stops apply to EITHER tag (an inflation-carried rule in
# the head is real displacement however it got there).
tag_and_gate <- function(adm, unit, head_gate = FALSE) {
  # head_gate: the top40/top10 stops apply to the NATIONAL pool and to each
  # blended head (via blend_head_gate below) - NOT to a state pool's
  # internal ordering, which does not map to deployment (second smoke
  # misfire, 2026-08-12: Washington's own-pool ranks 15/34 are nowhere near
  # any blend head). State pools get the flag-share gate + reporting.
  if (!nrow(adm)) { adm$mm_share_flags <- numeric(0); adm$mm_share_errors <- numeric(0)
                    adm$mm_inflation <- numeric(0); adm$artifact_i <- logical(0)
                    return(adm) }
  adm$mm_share_flags  <- round(adm$mm_n / adm$n, 4)
  adm$mm_share_errors <- round(ifelse(adm$k > 0, adm$mm_k / adm$k, 0), 4)
  adm$mm_inflation    <- round(adm$mm_k / adm$n, 4)   # additive precision inflation
  flag_tag <- adm$mm_share_flags >= MM_TAG_SHARE
  adm$artifact_i <- flag_tag | adm$mm_share_errors >= MM_TAG_SHARE
  share_flag <- mean(flag_tag)
  top40 <- sum(adm$artifact_i[seq_len(min(40L, nrow(adm)))])
  top10 <- sum(adm$artifact_i[seq_len(min(10L, nrow(adm)))])
  err_only <- adm$artifact_i & !flag_tag
  stamp("  [%s] artifact check: flag-tagged %d (%.2f%% of pool) | err-share-only %d (best rank %s, max LCB %s, median inflation %s) | own-pool top40 %d | top10 %d%s",
        unit, sum(flag_tag), 100 * share_flag, sum(err_only),
        if (any(err_only)) min(which(err_only)) else "-",
        if (any(err_only)) sprintf("%.3f", max(adm$lcb[err_only])) else "-",
        if (any(err_only)) sprintf("%.3f", median(adm$mm_inflation[err_only])) else "-",
        top40, top10, if (head_gate) " [head-gated]" else " [report only]")
  if (mean(adm$artifact_i) > 0.10)
    stamp("  [%s] WARNING: total tagged share %.1f%% exceeds 10%% - mismatch behaves differently at this scale; head gates and invariance still protect, but flag for review",
          unit, 100 * mean(adm$artifact_i))
  head_breach <- head_gate && (top40 > MM_TOP40_MAX || top10 > MM_TOP10_MAX)
  if (share_flag > MM_POOL_MAX || head_breach)
    stop(sprintf("DISPLACEMENT HALT [%s]: flag-tagged share %.3f (max %.2f) | top40 %d | top10 %d. Upstream displacement is not repairable post hoc; the run stops (statistician's gate, calibrated 2026-08-12).",
                 unit, share_flag, MM_POOL_MAX, top40, top10))
  adm
}

blend_head_gate <- function(pool_shadow, unit) {
  # the deployment-faithful head check: tagged rules in the top of the
  # BLENDED ordering (computed on the shadow, pre-drop blend)
  top40 <- sum(pool_shadow$artifact_i[seq_len(min(40L, nrow(pool_shadow)))])
  top10 <- sum(pool_shadow$artifact_i[seq_len(min(10L, nrow(pool_shadow)))])
  if (top40 > 0) {
    off <- which(pool_shadow$artifact_i[seq_len(min(40L, nrow(pool_shadow)))])
    for (r in off)
      stamp("  [blend head, %s] tagged rule at blend rank %d: lcb %.3f, mm_share_flags %.3f, mm_share_errors %.3f, mm_inflation %.3f | %s",
            unit, r, pool_shadow$lcb[r], pool_shadow$mm_share_flags[r],
            pool_shadow$mm_share_errors[r], pool_shadow$mm_inflation[r],
            pool_shadow$rule[r])
  }
  if (top40 > MM_TOP40_MAX || top10 > MM_TOP10_MAX)
    stop(sprintf("DISPLACEMENT HALT [blend head, %s]: tagged in blend top40 %d (max %d) | top10 %d (max %d) - genuine head displacement; the run stops (statistician's gate, calibrated 2026-08-12).",
                 unit, top40, MM_TOP40_MAX, top10, MM_TOP10_MAX))
  invisible(c(top40 = top40, top10 = top10))
}

## ---- national pool (19-var, any-error x strata, FY2022-24) ------------------
nat_fn <- file.path(CACHE_DIR, sprintf("national_pool_%d.rds", SEED))
if (RESUME_FROM_CHECKPOINT && file.exists(nat_fn)) {
  natl <- readRDS(nat_fn)
  if (!"mm_n" %in% names(natl))
    stop("cached national pool predates the artifact-check schema (or the 2026-08-12 frame rebuild); clear ", CACHE_DIR, " and re-run")
  stamp("national pool resumed: %d rules", nrow(natl))
} else {
  stamp("mining the national pool (19 features, %d rows) ...", nrow(adf))
  strata_nat <- lapply(setNames(nm = HH_LEVELS), function(h) which(hh_all %in% h))
  for (h in HH_LEVELS)
    stamp("  [national | stratum %s] %d rows, %d events",
          h, length(strata_nat[[h]]), sum(ie_all[strata_nat[[h]]]))
  rdf <- mine_rule_vocabulary(
    adf, list(any_error = list(rows = seq_len(nrow(adf)), ie = ie_all)),
    strata_nat, VOCAB19, xgb = XGB, rf = RF,
    signif_digits = SIGNIF_DIGITS, seed = SEED, verbose = TRUE,
    binary_features = BINARY_FEATURES)
  stamp("national raw rules: %d; scoring via the chunked reducer ...", nrow(rdf))
  sc <- reduce_flags_for_rules(
    rdf, adf, strata_nat,
    function(ix) c(length(ix), sum(ie_all[ix]), sum(ed_all[ix]),
                   sum(mm_all[ix]), sum(mm_all[ix] & ie_all[ix])),
    label = "national")
  base_by_hh <- vapply(strata_nat, function(rows) mean(ie_all[rows]), numeric(1))
  natl <- admit_rank(rdf, sc[, 1], sc[, 2], sc[, 3], base_by_hh[rdf$hh])
  ix_adm <- match(paste(natl$hh, natl$rule), paste(rdf$hh, rdf$rule))
  natl$mm_n <- sc[ix_adm, 4]; natl$mm_k <- sc[ix_adm, 5]
  saveRDS(natl, nat_fn)
  stamp("national pool admitted: %d rules", nrow(natl))
}
natl <- tag_and_gate(natl, "national", head_gate = TRUE)
natl$pool <- "national"

## ---- per-state pools + blended staged lists ---------------------------------
build_summary <- list()
for (state in STATES) {
  s_rows <- which(st == state)
  trs <- adf[s_rows, , drop = FALSE]
  ie_s <- ie_all[s_rows]; ed_s <- ed_all[s_rows]
  strata_s <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_all[s_rows] %in% h))

  own_fn <- file.path(CACHE_DIR, sprintf("state_pool_%s_%d.rds",
                                         gsub(" ", "_", state), SEED))
  mm_s <- mm_all[s_rows]
  if (RESUME_FROM_CHECKPOINT && file.exists(own_fn)) {
    own <- readRDS(own_fn)
    if (nrow(own) && !"mm_n" %in% names(own))
      stop("cached state pool for ", state, " predates the artifact-check schema; clear ", CACHE_DIR, " and re-run")
  } else {
    for (h in HH_LEVELS)
      cat(sprintf("  [%s | stratum %s] %d rows, %d events\n",
                  state, h, length(strata_s[[h]]), sum(ie_s[strata_s[[h]]])))
    rdf <- mine_rule_vocabulary(
      trs, list(any_error = list(rows = seq_len(nrow(trs)), ie = ie_s)),
      strata_s, VOCAB19, xgb = XGB, rf = RF,
      signif_digits = SIGNIF_DIGITS, seed = SEED, verbose = FALSE,
      binary_features = BINARY_FEATURES)
    own <- NULL
    if (!is.null(rdf) && nrow(rdf)) {
      fl <- flags_for_rules(rdf, trs, strata_s, label = "")
      n <- vapply(fl, length, integer(1))
      k <- vapply(fl, function(ix) sum(ie_s[ix]), numeric(1))
      dl <- vapply(fl, function(ix) sum(ed_s[ix]), numeric(1))
      mn <- vapply(fl, function(ix) sum(mm_s[ix]), numeric(1))
      mk <- vapply(fl, function(ix) sum(mm_s[ix] & ie_s[ix]), numeric(1))
      base_by_hh_s <- vapply(strata_s, function(rows) mean(ie_s[rows]), numeric(1))
      own <- admit_rank(rdf, n, k, dl, base_by_hh_s[rdf$hh])
      if (nrow(own)) {
        ix_adm <- match(paste(own$hh, own$rule), paste(rdf$hh, rdf$rule))
        own$mm_n <- mn[ix_adm]; own$mm_k <- mk[ix_adm]
      }
    }
    if (is.null(own)) own <- natl[0, setdiff(names(natl), c("pool")), drop = FALSE]
    saveRDS(own, own_fn)
  }
  own <- tag_and_gate(own, state)
  il_hold <- state %in% HOLD_STATE_POOLS
  if (il_hold && nrow(own))
    stamp("  [%s] STATE POOL HELD (decided 2026-08-12): %d rules mined+cached but not blended; lists are national-only",
          state, nrow(own))
  if (nrow(own)) own$pool <- "state"

  # one confidence scale; dedup (hh, rule) keeping the higher-LCB copy;
  # deterministic tie-break so reruns reproduce exactly. Held state pools
  # (Illinois) are excluded from the blend; tagged artifact rules are
  # dropped BEFORE the fill (lists artifact-free by construction), with
  # the count recorded per state in build_summary.
  cols <- c("hh", "rule", "engines", "mined_frames", "n", "k", "doll", "lcb",
            "mm_share_flags", "mm_share_errors", "mm_inflation", "artifact_i",
            "pool")
  blend_of <- function(a, b) bind_rows(a, b) %>%
    arrange(desc(lcb), desc(n), hh, rule) %>%
    distinct(hh, rule, .keep_all = TRUE)
  own_in <- if (nrow(own) && !il_hold) own[, cols] else NULL
  # shadow blend (unfiltered): the deployment-faithful head gate + counts
  pool_shadow <- blend_of(natl[, cols], own_in)
  blend_head_gate(pool_shadow, state)
  n_art_blend <- sum(pool_shadow$artifact_i)
  # visible blend: tagged rules dropped from EACH pool BEFORE dedup, so a
  # tagged higher-LCB copy cannot dedup away an untagged twin (review
  # advisory, 2026-08-12)
  pool <- blend_of(natl[!natl$artifact_i, cols],
                   if (!is.null(own_in)) own_in[!own_in$artifact_i, , drop = FALSE] else NULL)

  idx_tr <- flags_for_rules(pool, trs, strata_s, label = "")
  nfl <- lengths(idx_tr)

  for (b in BUDGETS) {
    cap <- floor(b * nrow(trs)); cap_buf <- floor(BUFFER_MULT * b * nrow(trs))
    # pass zero: legacy floor-0 scan fixes C0 / CT
    un <- rep(FALSE, nrow(trs)); n_in <- 0L; n_core <- 0L
    frozen <- integer(0); buffer <- integer(0)
    for (i in seq_along(idx_tr)) {
      add <- sum(!un[idx_tr[[i]]]); if (add == 0) next
      if (n_in + add <= cap) {
        un[idx_tr[[i]]] <- TRUE; n_in <- n_in + add; n_core <- n_core + add
        frozen <- c(frozen, i)
      } else if (n_in + add <= cap_buf) {
        un[idx_tr[[i]]] <- TRUE; n_in <- n_in + add; buffer <- c(buffer, i)
      }
    }
    gap_core <- 0L; gap_total <- 0L
    if (FRESH_MIN > 0) {
      C0 <- n_core; CT <- n_in
      un <- rep(FALSE, nrow(trs)); n_in <- 0L
      taken <- logical(length(idx_tr))
      frozen <- integer(0); buffer <- integer(0)
      for (ph in 1:2) {
        tgt <- if (ph == 1) C0 else CT
        for (ps in 1:2) {
          if (n_in >= tgt) break
          for (i in seq_along(idx_tr)) {
            if (taken[i]) next
            ix <- idx_tr[[i]]
            add <- sum(!un[ix])
            if (add == 0L) next
            if (ps == 1 && add / nfl[i] < FRESH_MIN) next
            if (n_in + add > tgt) next
            un[ix] <- TRUE; n_in <- n_in + add; taken[i] <- TRUE
            if (ph == 1) frozen <- c(frozen, i) else buffer <- c(buffer, i)
            if (n_in == tgt) break
          }
        }
        if (ph == 1) gap_core <- C0 - n_in
      }
      gap_total <- CT - n_in
      stopifnot(gap_core >= 0L, gap_total >= 0L)
    }
    sel <- c(frozen, buffer)
    hand <- data.frame(
      rank = seq_along(sel),
      role = rep(c("core", "buffer"), c(length(frozen), length(buffer))),
      rule = pool$rule[sel], hh = pool$hh[sel], pool = pool$pool[sel],
      engines = pool$engines[sel], mined_frames = pool$mined_frames[sel],
      n_flagged_train = pool$n[sel],
      precision_train = round(pool$k[sel] / pool$n[sel], 4),
      precision_train_lcb = round(pool$lcb[sel], 4),
      dollars_per_flag_train = round(pool$doll[sel] / pool$n[sel], 2),
      mm_share_flags = pool$mm_share_flags[sel],
      mm_share_errors = pool$mm_share_errors[sel],
      mm_inflation = pool$mm_inflation[sel],
      n_flagged_state = nfl[sel])
    un2 <- rep(FALSE, nrow(trs)); nn <- integer(length(sel))
    for (j in seq_along(sel)) {
      ix <- idx_tr[[sel[j]]]
      nn[j] <- sum(!un2[ix]); un2[ix] <- TRUE
    }
    hand$n_new_at_rank <- nn
    fn <- file.path(OUT_DIR, sprintf("blended_delivery_%s_2022_2024_budget%02.0f.csv",
                                     gsub(" ", "_", state), 100 * b))
    write.csv(hand, fn, row.names = FALSE)
    build_summary[[length(build_summary) + 1]] <- data.frame(
      state = state, budget = b, n_pool = nrow(pool),
      n_state_pool = nrow(own), n_core = length(frozen),
      n_buffer = length(buffer),
      n_state_rules_core = sum(pool$pool[frozen] == "state"),
      n_state_rules_buffer = sum(pool$pool[buffer] == "state"),
      n_artifact_dropped_blend = n_art_blend,
      n_state_pool_tagged = sum(own$artifact_i),
      state_pool_held = il_hold,
      fill_cases = n_in, fill_gap_core = gap_core,
      fill_gap_total = gap_total, cap_buf = cap_buf)
  }
  stamp("  %s: pool %d (state %d) -> lists written",
        state, nrow(pool), nrow(own))
  rm(trs, idx_tr); invisible(gc())
}
bs <- bind_rows(build_summary)
write.csv(bs, file.path(OUT_DIR, "build_summary.csv"), row.names = FALSE)
nz <- bs$fill_gap_total[bs$fill_gap_total > 0]
stamp("build done: %d lists | fill gaps: %d of %d cells > 0, max %d",
      2 * length(STATES), length(nz), nrow(bs), if (length(nz)) max(nz) else 0L)
stamp("ARTIFACT SUMMARY: national tagged %d of %d | state pools with any tagged rule: %d of %d | total dropped from blends (5%% cells): %d",
      sum(natl$artifact_i), nrow(natl),
      sum(tapply(bs$n_state_pool_tagged, bs$state, max) > 0), length(STATES),
      sum(bs$n_artifact_dropped_blend[bs$budget == 0.05]))

## ---- frame export for the characterization step (full precision) ------------
suppressMessages(library(readr))
KEYS <- c("state", "state_name", "yrmonth", "hhldno", "fiscal_year",
          "over_threshold", "total_error_amount", "cert_HH_size_FS_n",
          "error_status", "status")
readr::write_csv(adf0[, c(VOCAB19, KEYS)],
                 file.path(OUT_DIR, "frame_for_profiles.csv"))
stamp("frame export for characterization written (%d rows)", nrow(adf0))
stamp("STAGED build complete -> %s (not the shipped deliverable)", OUT_DIR)

