# Dollar-persistence check (opening move of the goal-metric track, 2026-07-16).
# Pre-registered expectation (domain argument): a rule's average error DOLLARS
# per flagged case is anchored to observable case characteristics (benefit
# levels, household size), so train -> test persistence should be strong even
# at modest support - at least as strong as precision's persistence.
# Uses the cached any-error benchmark pools (train 2022-23), scored per state
# on 2024. No mining.
# Output: methods/state_similarity_v2/transfer_benchmark_train2223_test24/
#         dollar_persistence.csv + printed summary.

suppressMessages(library(dplyr))
source("rule_mining_helpers.R")

TARGETS <- c("Massachusetts", "Michigan", "North Carolina", "Connecticut",
             "Arizona", "Washington",
             "Louisiana", "Virginia", "California", "Texas",
             "Mississippi", "New Jersey", "Colorado",
             "Maine", "Maryland", "Missouri",
             "District of Columbia", "Tennessee")
TRAIN_YEARS <- c("2022", "2023")
TEST_YEAR   <- "2024"
MIN_TEST_FLAGS <- 10   # a test estimate needs some support to be comparable

out_dir  <- "methods/state_similarity_v2/transfer_benchmark_train2223_test24"
CACHEOLD <- file.path(out_dir, "pool_cache")

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

adf <- prep_features(reg_model_data %>%
                       filter(fiscal_year %in% c(TRAIN_YEARS, TEST_YEAR)),
                     features)$data
yr <- as.character(adf$fiscal_year)
st <- as.character(adf$state)
ie_all <- !is.na(adf$over_threshold) & adf$over_threshold != 0
ed_all <- ifelse(ie_all, abs(ifelse(is.na(adf$total_error_amount), 0,
                                    adf$total_error_amount)), 0)

res <- list()
for (target in TARGETS) {
  own <- readRDS(file.path(CACHEOLD, sprintf("pool_%s.rds", gsub("[^A-Za-z]", "", target))))
  rows_tr <- which(st == target & yr %in% TRAIN_YEARS)
  rows_te <- which(st == target & yr == TEST_YEAR)
  tr <- adf[rows_tr, , drop = FALSE]; te <- adf[rows_te, , drop = FALSE]
  strata_tr <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(tr$cert_HH_size_FS_n) %in% h))
  strata_te <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(te$cert_HH_size_FS_n) %in% h))
  idx_tr <- flags_for_rules(own, tr, strata_tr, label = "")
  idx_te <- flags_for_rules(own, te, strata_te, label = "")
  ie_tr <- ie_all[rows_tr]; ed_tr <- ed_all[rows_tr]
  ie_te <- ie_all[rows_te]; ed_te <- ed_all[rows_te]

  n_tr <- lengths(idx_tr); n_te <- lengths(idx_te)
  keep <- n_tr >= 30 & n_te >= MIN_TEST_FLAGS
  s <- function(idx, v) vapply(idx, function(ix) sum(v[ix]), numeric(1))
  res[[length(res) + 1]] <- data.frame(
    target = target,
    rule = own$rule[keep], hh = own$hh[keep],
    n_train = n_tr[keep], n_test = n_te[keep],
    prec_train = s(idx_tr, ie_tr)[keep] / n_tr[keep],
    prec_test  = s(idx_te, ie_te)[keep] / n_te[keep],
    dpf_train  = s(idx_tr, ed_tr)[keep] / n_tr[keep],
    dpf_test   = s(idx_te, ed_te)[keep] / n_te[keep])
  cat(sprintf("%-22s %d rules usable\n", target, sum(keep)))
}
out <- bind_rows(res)
write.csv(out, file.path(out_dir, "dollar_persistence.csv"), row.names = FALSE)
cat(sprintf("wrote %s (%d rule-state rows)\n",
            file.path(out_dir, "dollar_persistence.csv"), nrow(out)))

band_of <- function(n) cut(n, c(29, 60, 120, 300, Inf),
                           labels = c("30-60", "61-120", "121-300", "300+"))
out$band <- band_of(out$n_train)
cat("\ntrain->test persistence (Spearman rank correlation) by train-support band:\n")
sm <- out %>% group_by(band) %>% summarise(
  n_rules = n(),
  dollars_per_flag = round(cor(dpf_train, dpf_test, method = "spearman"), 3),
  precision = round(cor(prec_train, prec_test, method = "spearman"), 3),
  .groups = "drop")
print(as.data.frame(sm), row.names = FALSE)
cat("\nsame, Pearson on log1p(dollars per flag):\n")
sm2 <- out %>% group_by(band) %>% summarise(
  n_rules = n(),
  log_dollars = round(cor(log1p(dpf_train), log1p(dpf_test)), 3),
  precision = round(cor(prec_train, prec_test), 3), .groups = "drop")
print(as.data.frame(sm2), row.names = FALSE)
