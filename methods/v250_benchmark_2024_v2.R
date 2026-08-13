# One-year-ahead benchmark for the v2.5.0 candidate recipe, on the
# 2026-08-12 rebuilt frame (Ben's smoothing fix). Mines FRESH (the earlier
# cached pools died with the frame rebuild): national + 49 state any-error
# pools on FY2022-23, per-size 19-feature vocabulary, joint BH + n >= 30,
# 99% LCB; blend per state (shipped semantics); fill FY2022-23 to 5%/10%
# core + 3x buffer under the shipped fresh-share walk (f = 0.50, tolerated
# per-phase gaps); freeze; walk FY2024 to that year's cap, outcome-free.
# Scorecard per state x budget with the mandatory companions, plus paired
# deltas against the shipped v2.4.0 scorecard.
#
# ARTIFACT CHECK + ILLINOIS HOLD: identical to the staged builder
# (methods/v250_build_staged_lists_v2.R header): mm_share audit columns on
# every rule, tagging at >= 0.25 concentration, DISPLACEMENT HALT at
# > 2% of a pool / > 1 in national top 40 / any in top 10, tagged rules
# dropped before the fill. REMOVAL INVARIANCE: any state whose blend
# contained a tagged rule is walked BOTH ways (with and without) and the
# FY2024 deltas recorded to invariance_check.csv; with zero tagged rules
# (the expected case given the measured expressibility ceiling) that file
# records the null result.
#
# Reading limits, pre-stated (review 2026-08-12, carried over):
# - single seed (117); prices the RECIPE, not the staged all-years lists.
# - the v2.4.0 baseline's bench lists were built 2026-08-02 with the
#   LEGACY floor-0 fill (pre-fresh-share; never rebuilt) - only the FY2024
#   cap-walk scoring operation is shared. The paired delta spans
#   vocabulary, pool source, the BUILD WALK (~+0.0118 median at 5% for
#   fresh-share on this era, findings 34), pool dedup, AND NOW ALSO THE
#   FRAME (rebuilt; error flags identical, reconstructed features and
#   error dollars changed). Right promotion comparison of packages; wrong
#   for attributing any single component.
# - per-state paired deltas carry a binomial SE of ~0.068 at a median
#   state's 5% budget (findings 30); read the 49-state aggregates.
# - dollar recall now uses total_error_amount = the file's recorded
#   raw-vs-corrected benefit difference (Eric 2026-08-12), aligned with
#   the error flag; v2.4.0's dollar columns were built from AMTERR.
#   Bounded within $5 per case by the reconciliation filter, but the
#   dollar deltas carry this definitional shift.
# - ILLINOIS's paired delta additionally includes losing its state pool:
#   the v2.4.0 baseline blended IL state rules, tonight's IL list is
#   national-only (the hold) - read IL's row with that in mind.
# - mismatch definition (conservative >=): rawben >= benmax with
#   rawben_uncapped < benmax; 568 of 44,029 at-or-above-max rows on the
#   rebuilt frame (strict == variant: 492), asserted < 1000 at runtime.
#
# SMOKE=1: 3 states, tiny ensembles, own output dir.
# Expects `reg_model_data`. Outputs -> methods/v250_benchmark_2024/.

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
TRAIN_YEARS <- c("2022", "2023")
TEST_YEAR   <- "2024"
XGB <- list(nrounds = 1000, max_depth = 4, eta = 0.02, subsample = 0.20)
RF  <- list(num_trees = 1000, max_depth = 4, mtry = 2, min_node_size = 20)
SIGNIF_DIGITS <- 3

EXPECT_TRAIN_ROWS <- 76031L; EXPECT_TRAIN_ERRS <- 8397L
EXPECT_TEST_ROWS  <- 39528L; EXPECT_TEST_ERRS  <- 4764L

BASE_FEATURES <- c(
  "HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size",
  "expedited_i", "cat_elig", "rawben_rel_max", "medical_deductions",
  "shelter_expenses_by_hh_size", "utilities", "married", "homeless",
  "percent_abawd", "unc_rawben_rel_max", "months_since_cert_n",
  "count_divisible_by_100")
VOCAB19 <- c(BASE_FEATURES,
             "gross_by_hh_size", "earned_by_hh_size", "unearned_by_hh_size")

V240_SCORECARD <- "methods/anyerror_blended_holdout_2024/holdout_metrics.json"
MM_TAG_SHARE <- 0.25; MM_POOL_MAX <- 0.02
MM_TOP40_MAX <- 1L;   MM_TOP10_MAX <- 0L
HOLD_STATE_POOLS <- c("Illinois")

OUT_DIR <- "methods/v250_benchmark_2024"
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
adf0 <- reg_model_data %>% filter(fiscal_year %in% c(TRAIN_YEARS, TEST_YEAR))
pf  <- prep_features(adf0, VOCAB19)
adf <- pf$data
stopifnot(length(setdiff(VOCAB19, pf$features)) == 0)
st <- as.character(adf$state)
yr <- as.character(adf$fiscal_year)
ie_all <- !is.na(adf$over_threshold) & adf$over_threshold != 0
ed_all <- ifelse(ie_all, abs(ifelse(is.na(adf$total_error_amount), 0,
                                    adf$total_error_amount)), 0)
stopifnot(sum(yr %in% TRAIN_YEARS) == EXPECT_TRAIN_ROWS,
          sum(ie_all[yr %in% TRAIN_YEARS]) == EXPECT_TRAIN_ERRS,
          sum(yr == TEST_YEAR) == EXPECT_TEST_ROWS,
          sum(ie_all[yr == TEST_YEAR]) == EXPECT_TEST_ERRS)
hh_all <- hh_group_of(adf$cert_HH_size_FS_n)
is_tr <- yr %in% TRAIN_YEARS
mm_all <- adf$rawben >= adf$benmax & adf$rawben_uncapped < adf$benmax
stamp("mismatch rows FY22-24: %d (train window: %d)", sum(mm_all),
      sum(mm_all & is_tr))
stopifnot(sum(mm_all) < 1000)   # post-fix-frame assert: thousands = pre-fix frame

STATES <- sort(unique(st))
if (SMOKE) STATES <- c("Washington", "Maine", "Mississippi", "Illinois")

## ---- shared: admission + artifact gate (mirrors the staged builder) ---------
admit_rank <- function(rdf, n, k, base_rate) {
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

# Calibration rulings (2026-08-12 smokes): pool-share gate over FLAG-tagged
# rules only, per pool; error-share tags = measurement contamination
# (dropped + reported, never pool-share-gated); head gates (top40/top10,
# either tag) on the NATIONAL pool and each state's shadow BLEND head only
# - a state pool's internal ordering is not a deployment object.
tag_and_gate <- function(adm, unit, head_gate = FALSE) {
  if (!nrow(adm)) { adm$mm_share_flags <- numeric(0); adm$mm_share_errors <- numeric(0)
                    adm$mm_inflation <- numeric(0); adm$artifact_i <- logical(0)
                    return(adm) }
  adm$mm_share_flags  <- round(adm$mm_n / adm$n, 4)
  adm$mm_share_errors <- round(ifelse(adm$k > 0, adm$mm_k / adm$k, 0), 4)
  adm$mm_inflation    <- round(adm$mm_k / adm$n, 4)
  flag_tag <- adm$mm_share_flags >= MM_TAG_SHARE
  adm$artifact_i <- flag_tag | adm$mm_share_errors >= MM_TAG_SHARE
  share_flag <- mean(flag_tag)
  top40 <- sum(adm$artifact_i[seq_len(min(40L, nrow(adm)))])
  top10 <- sum(adm$artifact_i[seq_len(min(10L, nrow(adm)))])
  err_only <- adm$artifact_i & !flag_tag
  stamp("  [%s] artifact check: flag-tagged %d (%.2f%%) | err-share-only %d (best rank %s) | own-pool top40 %d | top10 %d%s",
        unit, sum(flag_tag), 100 * share_flag, sum(err_only),
        if (any(err_only)) min(which(err_only)) else "-", top40, top10,
        if (head_gate) " [head-gated]" else " [report only]")
  if (mean(adm$artifact_i) > 0.10)
    stamp("  [%s] WARNING: total tagged share %.1f%% exceeds 10%% - flag for Eric",
          unit, 100 * mean(adm$artifact_i))
  head_breach <- head_gate && (top40 > MM_TOP40_MAX || top10 > MM_TOP10_MAX)
  if (share_flag > MM_POOL_MAX || head_breach)
    stop(sprintf("DISPLACEMENT HALT [%s]: flag-tagged share %.3f | top40 %d | top10 %d - run stops (statistician's gate, calibrated 2026-08-12).",
                 unit, share_flag, top40, top10))
  adm
}

blend_head_gate <- function(pool_shadow, unit) {
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
    stop(sprintf("DISPLACEMENT HALT [blend head, %s]: tagged in blend top40 %d (max %d) | top10 %d (max %d) - genuine head displacement; the run stops.",
                 unit, top40, MM_TOP40_MAX, top10, MM_TOP10_MAX))
  invisible(c(top40 = top40, top10 = top10))
}

walk_fill <- function(idx_tr, n_tr, b) {
  # pass zero + shipped fresh-share re-walk with tolerated per-phase gaps
  cap <- floor(b * n_tr); cap_buf <- floor(BUFFER_MULT * b * n_tr)
  nfl <- vapply(idx_tr, length, integer(1))
  un <- rep(FALSE, n_tr); n_in <- 0L; n_core <- 0L
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
    un <- rep(FALSE, n_tr); n_in <- 0L
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
  list(frozen = frozen, buffer = buffer, fill_cases = n_in,
       gap_core = gap_core, gap_total = gap_total)
}

score_2024 <- function(sel, idx_te, n_te, b, err_te, doll_te) {
  cap24 <- floor(b * n_te); un24 <- rep(FALSE, n_te)
  for (i in sel) {
    add <- sum(!un24[idx_te[[i]]])
    if (add > 0 && sum(un24) + add <= cap24) un24[idx_te[[i]]] <- TRUE
  }
  fl <- un24
  prec <- sum(fl & err_te) / max(sum(fl), 1)
  c(n_flagged = sum(fl), n_errors = sum(fl & err_te), precision = prec,
    dollar_recall = sum(doll_te[fl]) / max(sum(doll_te), 1))
}

## ---- national pool (FY2022-23) ----------------------------------------------
tr_rows <- which(is_tr)
trn <- adf[tr_rows, , drop = FALSE]
ie_tr <- ie_all[tr_rows]; ed_tr <- ed_all[tr_rows]; mm_tr <- mm_all[tr_rows]
strata_tr_nat <- lapply(setNames(nm = HH_LEVELS), function(h)
  which(hh_all[tr_rows] %in% h))
nat_fn <- file.path(CACHE_DIR, sprintf("bench_national_%d.rds", SEED))
if (RESUME_FROM_CHECKPOINT && file.exists(nat_fn)) {
  natl <- readRDS(nat_fn)
  stopifnot("mm_n" %in% names(natl))
  stamp("national bench pool resumed: %d rules", nrow(natl))
} else {
  stamp("mining the national bench pool (FY2022-23, %d rows) ...", nrow(trn))
  rdf <- mine_rule_vocabulary(
    trn, list(any_error = list(rows = seq_len(nrow(trn)), ie = ie_tr)),
    strata_tr_nat, VOCAB19, xgb = XGB, rf = RF,
    signif_digits = SIGNIF_DIGITS, seed = SEED, verbose = TRUE)
  stamp("national raw rules: %d; scoring via the chunked reducer ...", nrow(rdf))
  sc <- reduce_flags_for_rules(
    rdf, trn, strata_tr_nat,
    function(ix) c(length(ix), sum(ie_tr[ix]), sum(mm_tr[ix]),
                   sum(mm_tr[ix] & ie_tr[ix])),
    label = "bench national")
  base_by_hh <- vapply(strata_tr_nat, function(rows) mean(ie_tr[rows]), numeric(1))
  natl <- admit_rank(rdf, sc[, 1], sc[, 2], base_by_hh[rdf$hh])
  ix_adm <- match(paste(natl$hh, natl$rule), paste(rdf$hh, rdf$rule))
  natl$mm_n <- sc[ix_adm, 3]; natl$mm_k <- sc[ix_adm, 4]
  saveRDS(natl, nat_fn)
  stamp("national bench pool admitted: %d rules", nrow(natl))
}
natl <- tag_and_gate(natl, "bench national", head_gate = TRUE)
natl$pool <- "national"

## ---- per-state benchmark ----------------------------------------------------
v240 <- NULL
if (file.exists(V240_SCORECARD) && requireNamespace("jsonlite", quietly = TRUE))
  v240 <- jsonlite::fromJSON(V240_SCORECARD)$records

rows <- list(); inv_rows <- list()
for (state in STATES) {
  tr_s <- which(st == state & is_tr)
  te_s <- which(st == state & yr == TEST_YEAR)
  trs <- adf[tr_s, , drop = FALSE]; tes <- adf[te_s, , drop = FALSE]
  ie_s <- ie_all[tr_s]; mm_s <- mm_all[tr_s]
  err_te <- ie_all[te_s]; doll_te <- ed_all[te_s]
  strata_trs <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_all[tr_s] %in% h))
  strata_tes <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_all[te_s] %in% h))

  own_fn <- file.path(CACHE_DIR, sprintf("bench_state_%s_%d.rds",
                                         gsub(" ", "_", state), SEED))
  if (RESUME_FROM_CHECKPOINT && file.exists(own_fn)) {
    own <- readRDS(own_fn)
    if (nrow(own)) stopifnot("mm_n" %in% names(own))
  } else {
    rdf <- mine_rule_vocabulary(
      trs, list(any_error = list(rows = seq_len(nrow(trs)), ie = ie_s)),
      strata_trs, VOCAB19, xgb = XGB, rf = RF,
      signif_digits = SIGNIF_DIGITS, seed = SEED, verbose = FALSE)
    own <- NULL
    if (!is.null(rdf) && nrow(rdf)) {
      fl <- flags_for_rules(rdf, trs, strata_trs, label = "")
      n <- vapply(fl, length, integer(1))
      k <- vapply(fl, function(ix) sum(ie_s[ix]), numeric(1))
      mn <- vapply(fl, function(ix) sum(mm_s[ix]), numeric(1))
      mk <- vapply(fl, function(ix) sum(mm_s[ix] & ie_s[ix]), numeric(1))
      base_by_hh_s <- vapply(strata_trs, function(rows) mean(ie_s[rows]), numeric(1))
      own <- admit_rank(rdf, n, k, base_by_hh_s[rdf$hh])
      if (nrow(own)) {
        ix_adm <- match(paste(own$hh, own$rule), paste(rdf$hh, rdf$rule))
        own$mm_n <- mn[ix_adm]; own$mm_k <- mk[ix_adm]
      }
    }
    if (is.null(own)) own <- natl[0, setdiff(names(natl), "pool"), drop = FALSE]
    saveRDS(own, own_fn)
  }
  own <- tag_and_gate(own, state)
  il_hold <- state %in% HOLD_STATE_POOLS
  if (nrow(own)) own$pool <- "state"

  cols <- c("hh", "rule", "n", "k", "lcb", "mm_share_flags",
            "mm_share_errors", "mm_inflation", "artifact_i", "pool")
  blend_of <- function(a, b) bind_rows(a, b) %>%
    arrange(desc(lcb), desc(n), hh, rule) %>%
    distinct(hh, rule, .keep_all = TRUE)
  own_in <- if (nrow(own) && !il_hold) own[, cols] else NULL
  pool_all <- blend_of(natl[, cols], own_in)   # shadow: head gate + counts
  blend_head_gate(pool_all, state)
  n_art_blend <- sum(pool_all$artifact_i)
  # visible blend: tagged rules dropped from each pool BEFORE dedup
  pool <- blend_of(natl[!natl$artifact_i, cols],
                   if (!is.null(own_in)) own_in[!own_in$artifact_i, , drop = FALSE] else NULL)

  idx_tr <- flags_for_rules(pool, trs, strata_trs, label = "")
  idx_te <- flags_for_rules(pool, tes, strata_tes, label = "")

  for (b in BUDGETS) {
    wf <- walk_fill(idx_tr, nrow(trs), b)
    sel <- c(wf$frozen, wf$buffer)
    s24 <- score_2024(sel, idx_te, nrow(tes), b, err_te, doll_te)
    base <- mean(err_te)
    rows[[length(rows) + 1]] <- data.frame(
      state = state, budget = b, n_pool = nrow(pool),
      n_state_pool = nrow(own),   # the MINED size; state_pool_held says why not blended
      state_pool_held = il_hold, n_artifact_dropped = n_art_blend,
      n_core = length(wf$frozen), n_buffer = length(wf$buffer),
      n_state_rules_core = sum(pool$pool[wf$frozen] == "state"),
      n_te = nrow(tes), n_flagged = s24[["n_flagged"]],
      n_errors_caught = s24[["n_errors"]],
      precision = round(s24[["precision"]], 4),
      base_rate_te = round(base, 4),
      lift = round(s24[["precision"]] / max(base, 1e-9), 2),
      dollar_recall = round(s24[["dollar_recall"]], 4),
      fill_gap_core = wf$gap_core, fill_gap_total = wf$gap_total)
    # removal invariance: only defined when the blend actually contained
    # tagged rules - walk the unfiltered variant and record the delta
    if (n_art_blend > 0) {
      idx_tr_u <- flags_for_rules(pool_all, trs, strata_trs, label = "")
      idx_te_u <- flags_for_rules(pool_all, tes, strata_tes, label = "")
      wfu <- walk_fill(idx_tr_u, nrow(trs), b)
      s24u <- score_2024(c(wfu$frozen, wfu$buffer), idx_te_u, nrow(tes), b,
                         err_te, doll_te)
      inv_rows[[length(inv_rows) + 1]] <- data.frame(
        state = state, budget = b, n_artifact_in_blend = n_art_blend,
        precision_with = round(s24u[["precision"]], 4),
        precision_without = round(s24[["precision"]], 4),
        d_precision = round(s24[["precision"]] - s24u[["precision"]], 4),
        d_dollar_recall = round(s24[["dollar_recall"]] - s24u[["dollar_recall"]], 4))
      rm(idx_tr_u, idx_te_u)
    }
  }
  stamp("  %s done", state)
  rm(trs, tes, idx_tr, idx_te); invisible(gc())
}
bench <- bind_rows(rows)
write.csv(bench, file.path(OUT_DIR, "v250_benchmark_2024.csv"), row.names = FALSE)
inv <- if (length(inv_rows)) bind_rows(inv_rows) else
  data.frame(note = "zero artifact rules entered any blend; removal invariance holds trivially")
write.csv(inv, file.path(OUT_DIR, "invariance_check.csv"), row.names = FALSE)
if (requireNamespace("jsonlite", quietly = TRUE))
  jsonlite::write_json(
    list(built = "recipe benchmark: fresh mines train FY2022-23, walk FY2024, seed 117, rebuilt frame 2026-08-12",
         recipe = "v2.5.0 candidate (per-size 19-var, state + national blend, fresh-share walk f=0.50, artifact check, Illinois state pool held)",
         v240_comparison_note = "paired deltas span vocabulary + pool source + build walk (~+0.0118 median at 5% for fresh-share, findings 34) + pool dedup + the frame rebuild; error flags identical across frames, error dollars redefined (absbendiff)",
         records = bench),
    file.path(OUT_DIR, "v250_benchmark_2024.json"),
    dataframe = "rows", auto_unbox = TRUE, digits = 6)

## ---- summary with companions + v2.4.0 paired deltas -------------------------
for (b in BUDGETS) {
  s <- bench %>% filter(budget == b)
  stamp("BENCHMARK %d%%: median precision %.4f (mean %.4f) | median lift %.2fx | median dollar recall %.4f | below base rate: %d of %d",
        round(100 * b), median(s$precision), mean(s$precision),
        median(s$lift), median(s$dollar_recall),
        sum(s$precision < s$base_rate_te), nrow(s))
  nzc <- s$fill_gap_total[s$fill_gap_total > 0]
  stamp("  fill gaps: %d of %d cells > 0 (max total %d, max core %d) | artifact rules dropped from blends: %d",
        length(nzc), nrow(s), if (length(nzc)) max(nzc) else 0L,
        max(s$fill_gap_core), sum(s$n_artifact_dropped))
  if (!is.null(v240)) {
    v <- v240 %>% filter(budget_pct == round(100 * b)) %>%
      transmute(state, precision_v240 = precision_refill,
                dollars_v240 = error_dollars_flagged_holdout_unweighted_refill /
                  pmax(error_dollars_holdout_unweighted, 1))
    p <- s %>% inner_join(v, by = "state") %>%
      mutate(d = precision - precision_v240,
             dd = dollar_recall - dollars_v240)
    if (nrow(p)) {
      stamp("  vs shipped v2.4.0 (%d paired states): d precision median %+.4f | mean %+.4f | harmed (< -0.05) %d | helped (> +0.05) %d",
            nrow(p), median(p$d), mean(p$d), sum(p$d < -0.05), sum(p$d > 0.05))
      stamp("  dollars: d median %+.4f | mean %+.4f | harmed %d | helped %d  [delta spans vocab + pools + build walk + dedup + frame; see header]",
            median(p$dd), mean(p$dd), sum(p$dd < -0.05), sum(p$dd > 0.05))
    }
  }
}
paired_out <- if (!is.null(v240)) {
  bind_rows(lapply(BUDGETS, function(b) {
    v <- v240 %>% filter(budget_pct == round(100 * b)) %>%
      transmute(state, precision_v240 = precision_refill,
                dollars_v240 = round(
                  error_dollars_flagged_holdout_unweighted_refill /
                    pmax(error_dollars_holdout_unweighted, 1), 4))
    bench %>% filter(budget == b) %>% inner_join(v, by = "state") %>%
      mutate(d_precision = precision - precision_v240,
             d_dollar_recall = dollar_recall - dollars_v240)
  }))
} else bench
write.csv(paired_out, file.path(OUT_DIR, "v250_vs_v240_paired.csv"),
          row.names = FALSE)
stamp("benchmark written to %s", OUT_DIR)
