# Mine inclusion rules on a state's INTERNAL case file and blend them with
# the pre-mined national pool on one confidence scale. This is the
# internal-data sibling of INCL_build_blended_delivery_list_v2.R (which mines
# both pools from the public QC file); the mechanics of the hybrid
# internal+national blend are here, and the open question is empirical:
# judge any list this produces on a HELD-OUT slice of internal data (mine on
# one period, score on a later one). The ranking alone is not evidence.
#
# Same recipe as the shipped v2.5.0 delivery lists: any-error target,
# household-size strata 1 / 2-3 / 4+, xgboost + ranger, admission by
# Benjamini-Hochberg FDR 10% against the stratum base rate AND n >= 30,
# ranked by the one-sided 99% Wilson lower confidence bound of precision.
# The national side is the published pool artifact
# state_delivery_lists/national_rule_pool_2022_2024_v250.rds (the 60,920
# admitted candidate rules behind the shipped v2.5.0 lists, mined 2026-08-13
# on the corrected public frame).
#
# Run with the working directory at the top of a snap_qc download (needs
# rule_mining_helpers.R and the packages dplyr, xgboost, ranger). Edit the
# block below, then source().
#
# Your file: one row per case, with these columns --
#   the 19 model features (build them per features.R / DATA_DICTIONARY.md):
#     HH_size_n, children_i, elderly_disabled_i, total_deductions_by_hh_size,
#     expedited_i, bbce_state_i, rawben_rel_max, medical_deductions,
#     shelter_expenses_by_hh_size, utilities, married, homeless,
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
# order, each with its national training stats and its performance on YOUR
# data. A budget summary prints to the console at the end.

suppressMessages(library(dplyr))
source("rule_mining_helpers.R")

## ---- edit these (or pre-set any of them before source()) -------------------
if (!exists("INTERNAL_CSV"))
  INTERNAL_CSV  <- "internal_cases.csv"                # your case file
if (!exists("NATIONAL_POOL"))
  NATIONAL_POOL <- "state_delivery_lists/national_rule_pool_2022_2024_v250.rds"
if (!exists("OUT_CSV"))
  OUT_CSV       <- "blended_rules_ranked.csv"
if (!exists("BUDGET"))
  BUDGET        <- 0.10                                # review budget, share of caseload
## ----------------------------------------------------------------------------

SEED <- 117; LCB_Z <- 2.326; FDR_ALPHA <- 0.10; MIN_N <- 30
if (!exists("XGB")) XGB <- list(nrounds = 1000, max_depth = 4, eta = 0.02, subsample = 0.20)
if (!exists("RF"))  RF  <- list(num_trees = 1000, max_depth = 4, mtry = 2, min_node_size = 20)
VOCAB19 <- c("HH_size_n", "children_i", "elderly_disabled_i",
             "total_deductions_by_hh_size", "expedited_i", "bbce_state_i",
             "rawben_rel_max", "medical_deductions",
             "shelter_expenses_by_hh_size", "utilities", "married", "homeless",
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

## ---- national pool (artifact-tagged rules dropped) -------------------------
natl <- readRDS(NATIONAL_POOL)
tagged <- natl$mm_n / natl$n >= 0.25 |
          ifelse(natl$k > 0, natl$mm_k / natl$k, 0) >= 0.25
natl <- natl[!tagged, , drop = FALSE]
natl$pool <- "national"
cat(sprintf("national pool: %d rules (%d artifact-tagged dropped)\n",
            nrow(natl), sum(tagged)))

## ---- blend on the one LCB scale, then score everything on YOUR data --------
cols  <- c("hh", "rule", "n", "k", "doll", "lcb", "pool")
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
