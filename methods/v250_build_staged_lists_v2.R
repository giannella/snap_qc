# v2.5.0 CANDIDATE build - STAGED, not shipped. Eric's instruction 2026-08-11:
# for each state, blend the per-size (19-variable) state any-error rules with
# the national rules mined on the same vocabulary, deliver frozen budget lists
# with characterization columns (joined by step 2/3 of the runner).
#
# STAGED in methods/v250_candidate_lists/. Promotion to state_delivery_lists/
# plus CHANGELOG and version bump is Eric's decision after review; nothing
# here touches the shipped v2.4.0 deliverable.
#
# Recipe (all rulings Eric's, 2026-08-11): mine FRESH on ALL public years
# FY2022-24 (the shipped no-holdout delivery recipe, findings 15-16); the
# per-size vocabulary (16 shipped-in-practice + gross/earned/unearned per
# size = 19; the exploratory §§35-37 record informs this choice); any_error
# frame x coarse HH strata; NATIONAL pool + one pool PER STATE; admission
# ONE joint BH FDR 10% per mining unit with per-stratum base rates in the
# p-values AND n >= 30; 99%-LCB ordering; blend state + national on the one
# LCB scale (shipped builder semantics: sort, dedup by (hh, rule) keeping
# the higher-LCB copy); fill each state's FY2022-24 caseload to 5%/10% core
# + 3x buffer under the shipped fresh-share walk (f = 0.50) with the
# state-scale tolerated-gap treatment (§37 machinery record: exact refill
# is empirical-only at small pool scale; gaps reported per phase, never a
# crash). No outcome data enters the walk. Seed 117.
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

EXPECT_ROWS  <- 115559L    # FY2022-24 of the 2026-08-08 frame
EXPECT_ERRS  <- 13161L

BASE_FEATURES <- c(
  "HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size",
  "expedited_i", "cat_elig", "rawben_rel_max", "medical_deductions",
  "shelter_expenses_by_hh_size", "utilities", "married", "homeless",
  "percent_abawd", "unc_rawben_rel_max", "months_since_cert_n",
  "count_divisible_by_100")
PS_FEATURES <- c("gross_by_hh_size", "earned_by_hh_size", "unearned_by_hh_size")
VOCAB19 <- c(BASE_FEATURES, PS_FEATURES)

OUT_DIR <- "methods/v250_candidate_lists"
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

STATES <- sort(unique(st))
if (SMOKE) STATES <- c("Washington", "Maine", "Mississippi")
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

## ---- national pool (19-var, any-error x strata, FY2022-24) ------------------
nat_fn <- file.path(CACHE_DIR, sprintf("national_pool_%d.rds", SEED))
if (RESUME_FROM_CHECKPOINT && file.exists(nat_fn)) {
  natl <- readRDS(nat_fn)
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
    signif_digits = SIGNIF_DIGITS, seed = SEED, verbose = TRUE)
  stamp("national raw rules: %d; scoring via the chunked reducer ...", nrow(rdf))
  sc <- reduce_flags_for_rules(
    rdf, adf, strata_nat,
    function(ix) c(length(ix), sum(ie_all[ix]), sum(ed_all[ix])),
    label = "national")
  base_by_hh <- vapply(strata_nat, function(rows) mean(ie_all[rows]), numeric(1))
  natl <- admit_rank(rdf, sc[, 1], sc[, 2], sc[, 3], base_by_hh[rdf$hh])
  saveRDS(natl, nat_fn)
  stamp("national pool admitted: %d rules", nrow(natl))
}
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
  if (RESUME_FROM_CHECKPOINT && file.exists(own_fn)) {
    own <- readRDS(own_fn)
  } else {
    for (h in HH_LEVELS)
      cat(sprintf("  [%s | stratum %s] %d rows, %d events\n",
                  state, h, length(strata_s[[h]]), sum(ie_s[strata_s[[h]]])))
    rdf <- mine_rule_vocabulary(
      trs, list(any_error = list(rows = seq_len(nrow(trs)), ie = ie_s)),
      strata_s, VOCAB19, xgb = XGB, rf = RF,
      signif_digits = SIGNIF_DIGITS, seed = SEED, verbose = FALSE)
    own <- NULL
    if (!is.null(rdf) && nrow(rdf)) {
      fl <- flags_for_rules(rdf, trs, strata_s, label = "")
      n <- vapply(fl, length, integer(1))
      k <- vapply(fl, function(ix) sum(ie_s[ix]), numeric(1))
      dl <- vapply(fl, function(ix) sum(ed_s[ix]), numeric(1))
      base_by_hh_s <- vapply(strata_s, function(rows) mean(ie_s[rows]), numeric(1))
      own <- admit_rank(rdf, n, k, dl, base_by_hh_s[rdf$hh])
    }
    if (is.null(own)) own <- natl[0, setdiff(names(natl), "pool"), drop = FALSE]
    saveRDS(own, own_fn)
  }
  if (nrow(own)) own$pool <- "state"

  # one confidence scale; dedup (hh, rule) keeping the higher-LCB copy;
  # deterministic tie-break so reruns reproduce exactly
  cols <- c("hh", "rule", "engines", "mined_frames", "n", "k", "doll", "lcb",
            "pool")
  pool <- bind_rows(natl[, cols], if (nrow(own)) own[, cols] else NULL) %>%
    arrange(desc(lcb), desc(n), hh, rule) %>%
    distinct(hh, rule, .keep_all = TRUE)

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

## ---- frame export for the characterization step (full precision) ------------
suppressMessages(library(readr))
KEYS <- c("state", "state_name", "yrmonth", "hhldno", "fiscal_year",
          "over_threshold", "total_error_amount", "cert_HH_size_FS_n",
          "error_status", "status")
readr::write_csv(adf0[, c(VOCAB19, KEYS)],
                 file.path(OUT_DIR, "frame_for_profiles.csv"))
stamp("frame export for characterization written (%d rows)", nrow(adf0))
stamp("STAGED build complete -> %s (not the shipped deliverable)", OUT_DIR)
