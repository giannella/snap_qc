# Mine inclusion rules on a state's INTERNAL case file and, by default, blend
# them with the pre-mined national pool on one confidence scale (NATIONAL
# below; "internal_rules_only" skips the national pool). This is the
# internal-data sibling of INCL_build_blended_delivery_list_v2.R (which mines
# both pools from the public QC file); the mechanics of the hybrid
# internal+national blend are here, and the open question is empirical:
# judge any list this produces on a HELD-OUT slice of internal data (mine on
# one period, score on a later one). The ranking alone is not evidence.
#
# Same recipe as the shipped v2.7 delivery lists: any-error target,
# household-size strata 1 / 2-3 / 4+, xgboost + ranger, admission by
# Benjamini-Hochberg FDR 10% against the stratum base rate AND n >= 30,
# ranked by the one-sided 99% Wilson lower confidence bound of precision.
# The national side is the published pool artifact
# state_delivery_lists/national_rule_pool_2022_2024_v270.rds (the admitted
# candidate rules behind the shipped v2.7 lists, mined 2026-09-24 on the
# CPI-adjusted public frame: dollar fields in 2026 dollars, utilities as the
# SUA tier). The v2.5.0 pool (raw utilities dollars, 60,920 rules) is kept
# at archive/national_rule_pool_2022_2024_v250.rds.
#
# NATIONAL picks the national side:
#   "published"           (default) blend with the published pool above.
#   "remine"              mine the national pool fresh (see below).
#   "internal_rules_only" no national pool: the output holds only the rules
#                         admitted from your file; no public data is read.
#
# NATIONAL = "remine" mines the national pool fresh from the public frame
# (PUBLIC_FRAME, years NATIONAL_YEARS) instead of reading the published pool,
# with the recipe that built the published pool: the national-pool step of
# methods/v250_build_staged_lists_utilsua_v2.R (19-feature vocabulary, 0/1
# indicators canonicalized, any-error target, household-size strata from
# cert_HH_size_FS_n, joint BH FDR 10% + n >= 30, 99% LCB order, the
# reconstruction-mismatch and income-pile tags, and the displacement halt).
# It does NOT use INCL_build_blended_delivery_list_v2.R, whose older recipe
# (raw utilities dollars, five typed + pooled frames, no artifact or pile
# tags) is not what the published pool was built with. The mine is slow and
# needs about 16 GB of memory; the result is saved to REMINED_POOL and reused
# on later runs (delete that file to mine again). On the frame the published
# pool came from it should reproduce that pool; on a later frame it will
# differ wherever the frame changed.
#
# Run with the working directory at the top of a snap_qc download (needs
# rule_mining_helpers.R and the packages dplyr, xgboost, ranger). Edit the
# block below, then source().
#
# Your file: one row per case, with these columns --
#   the 19 model features (build them per features.R / DATA_DICTIONARY.md):
#     HH_size_n, children_i, elderly_disabled_i, total_deductions_by_hh_size,
#     expedited_i, bbce_state_i, rawben_rel_max, medical_deductions,
#     shelter_expenses_by_hh_size, utilities_sua, married, homeless,
#     percent_abawd, unc_rawben_rel_max, months_since_cert_n,
#     count_divisible_by_100, gross_by_hh_size, earned_by_hh_size,
#     unearned_by_hh_size
#   over_threshold      1 = payment error over the federal threshold, else 0
#   total_error_amount  error dollars (0 / NA when no error)
# Missing feature values are fine: a case with a missing value is simply
# never flagged by a rule that tests it.
#
# Output: blended_rules_ranked.csv -- the FULL blend (every national rule +
# every admitted internal rule; not truncated to the budget) in blend rank
# order, each with its training stats (national or yours) and its
# performance on YOUR data. With NATIONAL = "internal_rules_only" it holds
# your rules alone. A budget summary prints to the console at the end.

suppressMessages(library(dplyr))
source("rule_mining_helpers.R")

## ---- edit these (or pre-set any of them before source()) -------------------
if (!exists("INTERNAL_CSV"))
  INTERNAL_CSV  <- "internal_cases.csv"                # your case file
if (!exists("NATIONAL_POOL"))
  NATIONAL_POOL <- "state_delivery_lists/national_rule_pool_2022_2024_v270.rds"
if (!exists("OUT_CSV"))
  OUT_CSV       <- "blended_rules_ranked.csv"
if (!exists("BUDGET"))
  BUDGET        <- 0.10                                # review budget, share of caseload
if (!exists("NATIONAL"))
  NATIONAL      <- "published"                         # "published", "remine" (slow), or "internal_rules_only"
if (!exists("PUBLIC_FRAME"))
  PUBLIC_FRAME  <- "additional_data/reg_model_data.rds" # public frame, used only when NATIONAL = "remine"
if (!exists("NATIONAL_YEARS"))
  NATIONAL_YEARS <- c("2022", "2023", "2024")          # public years to mine, used only when NATIONAL = "remine"
if (!exists("REMINED_POOL"))
  REMINED_POOL  <- "national_rule_pool_remined.rds"    # where a re-mined pool is saved and reused
## ----------------------------------------------------------------------------
NATIONAL <- match.arg(NATIONAL, c("published", "remine", "internal_rules_only"))

SEED <- 117; LCB_Z <- 2.326; FDR_ALPHA <- 0.10; MIN_N <- 30
if (!exists("XGB")) XGB <- list(nrounds = 1000, max_depth = 4, eta = 0.02, subsample = 0.20)
if (!exists("RF"))  RF  <- list(num_trees = 1000, max_depth = 4, mtry = 2, min_node_size = 20)
VOCAB19 <- c("HH_size_n", "children_i", "elderly_disabled_i",
             "total_deductions_by_hh_size", "expedited_i", "bbce_state_i",
             "rawben_rel_max", "medical_deductions",
             "shelter_expenses_by_hh_size", "utilities_sua", "married", "homeless",
             "percent_abawd", "unc_rawben_rel_max", "months_since_cert_n",
             "count_divisible_by_100", "gross_by_hh_size", "earned_by_hh_size",
             "unearned_by_hh_size")
BINARY_FEATURES <- c("children_i", "elderly_disabled_i", "expedited_i",
                     "married", "homeless", "bbce_state_i")

## ---- internal data ---------------------------------------------------------
internal <- read.csv(INTERNAL_CSV)
miss <- setdiff(c(VOCAB19, "over_threshold", "total_error_amount"), names(internal))
if (length(miss)) stop("internal file lacks columns: ", paste(miss, collapse = ", "))
# TRUE/FALSE (or True/False) indicator columns -> 1/0. Constant columns are
# fine: a within-state constant (e.g. bbce_state_i) is skipped by the miner
# but still evaluates in national rules that test it.
for (v in c(VOCAB19, "over_threshold")) {
  x <- internal[[v]]
  if (is.logical(x)) internal[[v]] <- as.integer(x)
  else if (is.character(x)) {
    xl <- toupper(trimws(x))
    if (all(xl %in% c("TRUE", "FALSE", "") | is.na(x)))
      internal[[v]] <- ifelse(xl == "TRUE", 1L, ifelse(xl == "FALSE", 0L, NA_integer_))
  }
}
pf  <- prep_features(internal, VOCAB19)
adf <- pf$data
ie  <- !is.na(adf$over_threshold) & adf$over_threshold != 0
ed  <- ifelse(ie, abs(ifelse(is.na(adf$total_error_amount), 0,
                             adf$total_error_amount)), 0)
hh  <- ifelse(adf$HH_size_n <= 1, "1", ifelse(adf$HH_size_n <= 3, "2-3", "4+"))
strata <- lapply(setNames(nm = c("1", "2-3", "4+")), function(h) which(hh == h))
cat(sprintf("internal: %d cases, %d errors (%.1f%%)\n",
            nrow(adf), sum(ie), 100 * mean(ie)))

## ---- mine + admit your own pool (shipped v2.5.0 semantics) -----------------
rdf <- mine_rule_vocabulary(
  adf, list(any_error = list(rows = seq_len(nrow(adf)), ie = ie)),
  strata, pf$features, xgb = XGB, rf = RF, signif_digits = 3, seed = SEED,
  verbose = TRUE, binary_features = BINARY_FEATURES)
fl <- flags_for_rules(rdf, adf, strata, label = "own")
rdf$n <- lengths(fl)
rdf$k <- vapply(fl, function(ix) sum(ie[ix]), numeric(1))
rdf$doll <- vapply(fl, function(ix) sum(ed[ix]), numeric(1))
base <- vapply(strata, function(r) mean(ie[r]), numeric(1))
pv   <- pbinom(rdf$k - 1, rdf$n, base[rdf$hh], lower.tail = FALSE)
o    <- order(pv); m <- length(pv)
thr  <- max(c(0L, which(pv[o] <= FDR_ALPHA * seq_len(m) / m)))
bh   <- rep(FALSE, m); if (thr > 0) bh[o[seq_len(thr)]] <- TRUE
own  <- rdf[bh & rdf$n >= MIN_N, , drop = FALSE]
own$lcb  <- wilson_lcb(own$k, own$n, LCB_Z)
own$pool <- "state_internal"
cat(sprintf("your pool: %d candidate rules mined, %d admitted\n", nrow(rdf), nrow(own)))

## ---- national pool: published, or re-mined from the public frame -----------
# The re-mine is the national-pool step of
# methods/v250_build_staged_lists_utilsua_v2.R (the build behind the published
# pool), with its per-rule mismatch and pile counts taken in one scoring pass.
MM_TAG_SHARE <- 0.25; MM_POOL_MAX <- 0.02; MM_TOP40_MAX <- 1L; MM_TOP10_MAX <- 0L
PILE_TAG_SHARE <- 0.25
mine_national_pool <- function() {
  pub <- if (exists("reg_model_data", envir = globalenv()))
    get("reg_model_data", envir = globalenv()) else readRDS(PUBLIC_FRAME)
  pub <- pub[as.character(pub$fiscal_year) %in% NATIONAL_YEARS, , drop = FALSE]
  need <- c(VOCAB19, "over_threshold", "total_error_amount", "cert_HH_size_FS_n",
            "rawben", "benmax", "rawben_uncapped", "correctednotes")
  miss <- setdiff(need, names(pub))
  if (length(miss)) stop("public frame lacks columns: ", paste(miss, collapse = ", "))
  pf_n <- prep_features(pub, VOCAB19)
  ndf  <- pf_n$data
  miss <- setdiff(VOCAB19, pf_n$features)
  if (length(miss)) stop("features missing or constant in the public frame: ", paste(miss, collapse = ", "))
  ie_n <- !is.na(ndf$over_threshold) & ndf$over_threshold != 0
  ed_n <- ifelse(ie_n, abs(ifelse(is.na(ndf$total_error_amount), 0,
                                  ndf$total_error_amount)), 0)
  hn   <- suppressWarnings(as.numeric(as.character(ndf$cert_HH_size_FS_n)))
  hh_n <- ifelse(is.na(hn), NA_character_, ifelse(hn <= 1, "1", ifelse(hn <= 3, "2-3", "4+")))
  strata_n <- lapply(setNames(nm = c("1", "2-3", "4+")), function(h) which(hh_n %in% h))
  # reconstruction-mismatch rows and income-pile rows, as in the v2.7 build
  mm   <- (ndf$rawben >= ndf$benmax & ndf$rawben_uncapped < ndf$benmax) %in% TRUE
  pile <- income_pile_rows(pub)
  cat(sprintf("public frame: years %s, %d cases, %d errors | mismatch rows %d | income-pile rows %d\n",
              paste(NATIONAL_YEARS, collapse = "+"), nrow(ndf), sum(ie_n), sum(mm), sum(pile)))
  rm(pub)
  rdf <- mine_rule_vocabulary(
    ndf, list(any_error = list(rows = seq_len(nrow(ndf)), ie = ie_n)),
    strata_n, VOCAB19, xgb = XGB, rf = RF, signif_digits = 3, seed = SEED,
    verbose = TRUE, binary_features = BINARY_FEATURES)
  sc <- reduce_flags_for_rules(
    rdf, ndf, strata_n,
    function(ix) c(length(ix), sum(ie_n[ix]), sum(ed_n[ix]),
                   sum(mm[ix]), sum(mm[ix] & ie_n[ix]),
                   sum(pile[ix]), sum(pile[ix] & ie_n[ix])),
    label = "national")
  # admission: ONE joint BH at FDR 10% across all candidates, per-stratum base
  # rates in the p-values, AND n >= MIN_N; then the 99% LCB order
  base_n <- vapply(strata_n, function(r) mean(ie_n[r]), numeric(1))
  rdf$n <- as.integer(sc[, 1]); rdf$k <- sc[, 2]; rdf$doll <- sc[, 3]
  rdf$mm_n <- sc[, 4]; rdf$mm_k <- sc[, 5]
  p_k <- sc[, 6]; p_e <- sc[, 7]
  pv <- pbinom(rdf$k - 1, rdf$n, base_n[rdf$hh], lower.tail = FALSE)
  o  <- order(pv); m <- length(pv)
  thr <- max(c(0L, which(pv[o] <= FDR_ALPHA * seq_len(m) / m)))
  bh <- rep(FALSE, m); if (thr > 0) bh[o[seq_len(thr)]] <- TRUE
  keep <- bh & rdf$n >= MIN_N
  pool <- rdf[keep, , drop = FALSE]; p_k <- p_k[keep]; p_e <- p_e[keep]
  pool$lcb <- wilson_lcb(pool$k, pool$n, LCB_Z)
  o <- order(-pool$lcb, -pool$n, pool$hh, pool$rule, method = "radix")
  pool <- pool[o, , drop = FALSE]; p_k <- p_k[o]; p_e <- p_e[o]
  # reconstruction-mismatch tags and the displacement halt
  pool$mm_share_flags  <- round(pool$mm_n / pool$n, 4)
  pool$mm_share_errors <- round(ifelse(pool$k > 0, pool$mm_k / pool$k, 0), 4)
  pool$mm_inflation    <- round(pool$mm_k / pool$n, 4)
  flag_tag <- pool$mm_share_flags >= MM_TAG_SHARE
  pool$artifact_i <- flag_tag | pool$mm_share_errors >= MM_TAG_SHARE
  top40 <- sum(pool$artifact_i[seq_len(min(40L, nrow(pool)))])
  top10 <- sum(pool$artifact_i[seq_len(min(10L, nrow(pool)))])
  if (mean(flag_tag) > MM_POOL_MAX || top40 > MM_TOP40_MAX || top10 > MM_TOP10_MAX)
    stop(sprintf("DISPLACEMENT HALT [national re-mine]: flag-tagged share %.3f (max %.2f) | tagged in top40 %d (max %d) | top10 %d (max %d). The public frame's reconstruction mismatch is crowding the pool; do not use it.",
                 mean(flag_tag), MM_POOL_MAX, top40, MM_TOP40_MAX, top10, MM_TOP10_MAX))
  # income-pile tags (issue #29)
  pool$pile_share_flags  <- round(p_k / pool$n, 4)
  pool$pile_share_errors <- round(ifelse(pool$k > 0, p_e / pool$k, 0), 4)
  pool$pile_i <- pool$pile_share_flags >= PILE_TAG_SHARE |
                 pool$pile_share_errors >= PILE_TAG_SHARE
  pool$pool <- "national"
  rownames(pool) <- NULL
  attr(pool, "years") <- NATIONAL_YEARS
  cat(sprintf("national re-mine: %d candidate rules, %d admitted (%d mismatch-tagged, %d pile-tagged)\n",
              nrow(rdf), nrow(pool), sum(pool$artifact_i), sum(pool$pile_i)))
  pool
}

cols <- c("hh", "rule", "n", "k", "doll", "lcb", "pool")
if (NATIONAL == "internal_rules_only") {
  natl <- own[0, cols]
  cat("national pool: none (NATIONAL = \"internal_rules_only\")\n")
} else {
  if (NATIONAL == "remine") {
    if (file.exists(REMINED_POOL)) {
      natl <- readRDS(REMINED_POOL)
      if (!identical(attr(natl, "years"), NATIONAL_YEARS))
        stop(REMINED_POOL, " was mined on years ", paste(attr(natl, "years"), collapse = "+"),
             ", not ", paste(NATIONAL_YEARS, collapse = "+"), "; delete it to mine again")
      cat(sprintf("national pool: reusing the re-mined pool in %s (delete it to mine again)\n",
                  REMINED_POOL))
    } else {
      natl <- mine_national_pool()
      saveRDS(natl, REMINED_POOL)
      cat(sprintf("national pool: re-mined and saved to %s\n", REMINED_POOL))
    }
  } else {
    natl <- readRDS(NATIONAL_POOL)
  }
  ## artifact-tagged and pile-tagged rules are dropped from either pool
  tagged <- natl$mm_n / natl$n >= 0.25 |
            ifelse(natl$k > 0, natl$mm_k / natl$k, 0) >= 0.25
  # rules resting on the public frame's reconstruction income piles (issue #29;
  # pile_i is published on the pool artifact since v2.7.0) are dropped too
  pile_tagged <- if ("pile_i" %in% names(natl)) natl$pile_i & !tagged else rep(FALSE, nrow(natl))
  natl <- natl[!tagged & !pile_tagged, , drop = FALSE]
  natl$pool <- "national"
  cat(sprintf("national pool: %d rules (%d artifact-tagged and %d pile-tagged dropped)\n",
              nrow(natl), sum(tagged), sum(pile_tagged)))
}

## ---- blend on the one LCB scale, then score everything on YOUR data --------
blend <- bind_rows(natl[, cols], own[, cols]) %>%
  arrange(desc(lcb), desc(n), hh, rule) %>%
  distinct(hh, rule, .keep_all = TRUE) %>%
  rename(n_train = n, k_train = k, doll_train = doll)
idx <- flags_for_rules(blend, adf, strata, label = "blend")
blend$n_internal    <- lengths(idx)
blend$k_internal    <- vapply(idx, function(ix) sum(ie[ix]), numeric(1))
blend$doll_internal <- vapply(idx, function(ix) sum(ed[ix]), numeric(1))
blend$precision_internal <- ifelse(blend$n_internal > 0,
                                   blend$k_internal / blend$n_internal, NA)
blend$blend_rank <- seq_len(nrow(blend))
write.csv(blend, OUT_CSV, row.names = FALSE)
cat(sprintf("wrote %s: %d rules (%d national, %d yours)\n", OUT_CSV,
            nrow(blend), sum(blend$pool == "national"),
            sum(blend$pool == "state_internal")))

## ---- budget summary: walk the ranking until the caseload budget fills ------
cap <- ceiling(BUDGET * nrow(adf))
flagged <- logical(nrow(adf)); used <- 0L
for (i in seq_len(nrow(blend))) {
  new <- idx[[i]][!flagged[idx[[i]]]]
  if (sum(flagged) + length(new) > cap) break
  flagged[new] <- TRUE; used <- i
}
cat(sprintf("at the %.0f%% budget (%d cases): top %d rules, %d flagged, %d errors caught (precision %.1f%%, base %.1f%%), $%s\n",
            100 * BUDGET, cap, used, sum(flagged), sum(ie & flagged),
            100 * sum(ie & flagged) / max(sum(flagged), 1), 100 * mean(ie),
            format(round(sum(ed[flagged])), big.mark = ",")))
