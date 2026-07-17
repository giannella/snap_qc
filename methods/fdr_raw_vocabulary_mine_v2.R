# FDR-done-right, step 1: mine and score the RAW (unfiltered) any-error
# vocabulary for the national 2022-23 pool and each benchmark state's own
# pool, so admission rules can be tested as true build-time gates. The first
# FDR audition was invalid because cached pools were already support/base-rate
# filtered - BH admission on top was a no-op.
#
# For every raw candidate rule: n (flagged train cases), k (any-error hits),
# plus per-stratum base rates. NO filter of any kind is applied here.
# Outputs are LARGE and stay on disk only (gitignored):
#   methods/state_similarity_v2/transfer_benchmark_train2223_test24/
#     fdr_raw_vocab/raw_<key>.rds
#
# Mining is deterministic (seed 117), so these raw vocabularies are supersets
# of the cached filtered pools; the admission audition (step 2) walks from
# these through the identical downstream pipeline.

suppressMessages(library(dplyr))
source("rule_mining_helpers.R")
set.seed(117)

TARGETS <- c("Massachusetts", "Michigan", "North Carolina", "Connecticut",
             "Arizona", "Washington",
             "Louisiana", "Virginia", "California", "Texas",
             "Mississippi", "New Jersey", "Colorado",
             "Maine", "Maryland", "Missouri",
             "District of Columbia", "Tennessee")
if (!exists("TRAIN_YEARS")) TRAIN_YEARS <- c("2022", "2023")
XGB <- list(nrounds = 1000, max_depth = 4, eta = 0.02, subsample = 0.20)
RF  <- list(num_trees = 1000, max_depth = 4, mtry = 2, min_node_size = 20)
SIGNIF_DIGITS <- 3

out_dir <- "methods/state_similarity_v2/transfer_benchmark_train2223_test24"
if (!exists("RAWDIR")) RAWDIR <- file.path(out_dir, "fdr_raw_vocab")
dir.create(RAWDIR, showWarnings = FALSE)

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
st <- as.character(adf$state)
ie_all <- !is.na(adf$over_threshold) & adf$over_threshold != 0
cat(sprintf("train frame: %d rows (%s)\n", nrow(adf),
            paste(TRAIN_YEARS, collapse = "+")))

mine_raw <- function(pool_states, key) {
  fn <- file.path(RAWDIR, sprintf("raw_%s.rds", key))
  if (file.exists(fn)) { cat(sprintf("%s: cached\n", key)); return(invisible()) }
  rows <- which(st %in% pool_states)
  train <- adf[rows, , drop = FALSE]
  ie <- ie_all[rows]
  strata_tr <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(train$cert_HH_size_FS_n) %in% h))
  rdf <- mine_rule_vocabulary(
    train, list(any_error = list(rows = seq_len(nrow(train)), ie = ie)),
    strata_tr, pv, xgb = XGB, rf = RF,
    signif_digits = SIGNIF_DIGITS, seed = 117)
  idx <- flags_for_rules(rdf, train, strata_tr, label = sprintf("raw %s", key))
  rdf$n <- lengths(idx)
  rdf$k <- vapply(idx, function(ix) sum(ie[ix]), numeric(1))
  base <- vapply(setNames(nm = HH_LEVELS), function(h)
    mean(ie[strata_tr[[h]]]), numeric(1))
  attr(rdf, "base_rates") <- base
  attr(rdf, "n_train_rows") <- nrow(train)
  saveRDS(rdf, fn)
  cat(sprintf("%s: %d raw candidates -> %s\n", key, nrow(rdf), fn))
  rm(idx); invisible(gc())
}

mine_raw(sort(unique(st)), "national")
for (target in TARGETS) mine_raw(target, gsub("[^A-Za-z]", "", target))
cat("\n=== raw vocabulary mine complete ===\n")
