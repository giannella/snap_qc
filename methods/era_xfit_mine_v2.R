# Era validation, step 2b: the cross-fit arm's inputs (pre-registration E6).
# Mine the national vocabulary on a RANDOM HALF of the 2017-18 training cases
# (split seed 118), then score every candidate's n/k on the UNTOUCHED half.
# Ordering by the untouched half's estimates is selection-free by
# construction. Output (gitignored, disk only):
#   methods/state_similarity_v2/era_validation_train1718_test19/raw_vocab/
#     raw_xfit_national.rds   (n/k measured on the held-out half)

suppressMessages(library(dplyr))
source("rule_mining_helpers.R")
set.seed(117)

TRAIN_YEARS <- c("2017", "2018")
XGB <- list(nrounds = 1000, max_depth = 4, eta = 0.02, subsample = 0.20)
RF  <- list(num_trees = 1000, max_depth = 4, mtry = 2, min_node_size = 20)
SIGNIF_DIGITS <- 3
RAWDIR <- "methods/state_similarity_v2/era_validation_train1718_test19/raw_vocab"

features <- c(
  "HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size",
  "expedited_i", "cat_elig", "rawben_rel_max", "medical_deductions",
  "shelter_expenses_by_hh_size", "utilities", "married", "homeless",
  "rawearn_by_hh_size", "rawunearn_by_hh_size", "rawgross_by_hh_size",
  "percent_abawd", "unc_rawben_rel_max",
  "months_since_cert_n", "count_divisible_by_100"
)
HH_LEVELS <- c("1", "2-3", "4+")
hh_group_of <- function(n) {
  n <- suppressWarnings(as.numeric(as.character(n)))
  ifelse(is.na(n), NA_character_, ifelse(n <= 1, "1", ifelse(n <= 3, "2-3", "4+")))
}

pf <- prep_features(reg_model_data %>% filter(fiscal_year %in% TRAIN_YEARS), features)
adf <- pf$data; pv <- pf$features
ie_all <- !is.na(adf$over_threshold) & adf$over_threshold != 0

set.seed(118)
in_a <- sample(c(TRUE, FALSE), nrow(adf), replace = TRUE)
train_a <- adf[in_a, , drop = FALSE]     # mining half
train_b <- adf[!in_a, , drop = FALSE]    # estimation half (untouched)
ie_a <- ie_all[in_a]; ie_b <- ie_all[!in_a]
cat(sprintf("half A (mine): %d rows | half B (estimate): %d rows\n",
            nrow(train_a), nrow(train_b)))

strata_a <- lapply(setNames(nm = HH_LEVELS), function(h)
  which(hh_group_of(train_a$cert_HH_size_FS_n) %in% h))
strata_b <- lapply(setNames(nm = HH_LEVELS), function(h)
  which(hh_group_of(train_b$cert_HH_size_FS_n) %in% h))

rdf <- mine_rule_vocabulary(
  train_a, list(any_error = list(rows = seq_len(nrow(train_a)), ie = ie_a)),
  strata_a, pv, xgb = XGB, rf = RF, signif_digits = SIGNIF_DIGITS, seed = 117)
cat(sprintf("half-A raw candidates: %d\n", nrow(rdf)))

idx_b <- flags_for_rules(rdf, train_b, strata_b, label = "half-B estimates")
rdf$n <- lengths(idx_b)
rdf$k <- vapply(idx_b, function(ix) sum(ie_b[ix]), numeric(1))
base <- vapply(setNames(nm = HH_LEVELS), function(h)
  mean(ie_b[strata_b[[h]]]), numeric(1))
attr(rdf, "base_rates") <- base
saveRDS(rdf, file.path(RAWDIR, "raw_xfit_national.rds"))
cat(sprintf("wrote %s (%d rules, half-B stats)\n",
            file.path(RAWDIR, "raw_xfit_national.rds"), nrow(rdf)))
