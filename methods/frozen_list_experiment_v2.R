# FROZEN-LIST experiment: the deliverable a state can hold in its hands.
#
# For each state: rank the national pool (mined on all states' 2022+2023),
# budget-fill against the STATE'S 2022-23 CASELOAD COVARIATES (no outcomes,
# no 2024 data) and freeze the contributing rules -- an explicit list of
# ~25-55 rules with thresholds. Then apply the frozen list to the state's
# 2024 cases and measure what actually happens:
#   workload_2024   realized fire rate (the calibration-drift question:
#                   how far it lands from the 5% / 10% it was sized to)
#   precision / recall / dollar_recall on 2024
# Compare with the same-year-filled benchmark rows (national_all), where the
# fill saw the 2024 caseload -- the difference is the price of freezing.
#
# Expects `reg_model_data`. Output:
#   methods/state_similarity_v2/transfer_benchmark_train2223_test24/
#     frozen_list_results.csv
#     frozen_list_rules_<state>.csv  (the handable per-state lists)

suppressMessages(library(dplyr))
source("rule_mining_helpers.R")

TARGETS <- c("Louisiana", "Washington", "Virginia", "Arizona", "Connecticut",
             "Michigan", "North Carolina", "California", "Texas",
             "Mississippi", "New Jersey", "Colorado",
             "Maine", "Maryland", "Missouri", "Massachusetts",
             "District of Columbia", "Tennessee")
BUDGETS <- c(0.05, 0.10)
out_dir <- "methods/state_similarity_v2/transfer_benchmark_train2223_test24"
list_dir <- file.path(out_dir, "frozen_lists")
dir.create(list_dir, showWarnings = FALSE)

features <- c(
  "HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size",
  "expedited_i", "cat_elig", "rawben_rel_max", "medical_deductions",
  "shelter_expenses_by_hh_size", "utilities", "married", "homeless",
  "rawearn_by_hh_size", "rawunearn_by_hh_size", "rawgross_by_hh_size",
  "percent_abawd", "unc_rawben_rel_max",
  "months_since_cert_n", "count_divisible_by_100"
)
hh_group_of <- function(n) {
  n <- suppressWarnings(as.numeric(as.character(n)))
  ifelse(is.na(n), NA_character_, ifelse(n <= 1, "1", ifelse(n <= 3, "2-3", "4+")))
}
targets_of <- function(df) {
  ie <- !is.na(df$over_threshold) & df$over_threshold != 0
  amt <- df$total_error_amount; amt[is.na(amt)] <- 0
  list(ie = ie, ed = ifelse(ie, abs(amt), 0))
}

adf <- prep_features(reg_model_data %>%
                       filter(fiscal_year %in% c("2022", "2023", "2024")),
                     features)$data
yr <- as.character(adf$fiscal_year)
st <- as.character(adf$state)

sig <- gsub("[^A-Za-z]", "", paste(sort(unique(st)), collapse = "_"))
key <- if (nchar(sig) <= 80) sig else
  sprintf("%s_%08x", substr(sig, 1, 60),
          sum(utf8ToInt(sig) * seq_along(utf8ToInt(sig))) %% .Machine$integer.max)
pool <- readRDS(file.path(out_dir, "pool_cache", sprintf("pool_%s.rds", key)))
ord <- order(-pool$precision_train_lcb)
cat(sprintf("pool: %d rules\n", nrow(pool)))

res <- list()
for (target in TARGETS) {
  tr <- adf[st == target & yr %in% c("2022", "2023"), , drop = FALSE]
  te <- adf[st == target & yr == "2024", , drop = FALSE]
  tg_te <- targets_of(te)
  strata_tr <- lapply(setNames(nm = c("1", "2-3", "4+")), function(h)
    which(hh_group_of(tr$cert_HH_size_FS_n) %in% h))
  strata_te <- lapply(setNames(nm = c("1", "2-3", "4+")), function(h)
    which(hh_group_of(te$cert_HH_size_FS_n) %in% h))
  idx_tr <- flags_for_rules(pool, tr, strata_tr, label = "")
  idx_te <- flags_for_rules(pool, te, strata_te, label = "")

  for (b in BUDGETS) {
    # freeze: fill against the 2022-23 caseload covariates only
    cap <- floor(b * nrow(tr))
    un <- rep(FALSE, nrow(tr)); n_in <- 0L; frozen <- integer(0)
    for (i in ord) {
      ix <- idx_tr[[i]]
      add <- sum(!un[ix])
      if (add > 0 && n_in + add <= cap) {
        un[ix] <- TRUE; n_in <- n_in + add; frozen <- c(frozen, i)
      }
    }
    # deploy frozen list on 2024
    un24 <- rep(FALSE, nrow(te))
    for (i in frozen) un24[idx_te[[i]]] <- TRUE
    n24 <- sum(un24); k24 <- sum(tg_te$ie[un24]); d24 <- sum(tg_te$ed[un24])
    res[[length(res) + 1]] <- data.frame(
      target = target, budget = b,
      n_rules_frozen = length(frozen),
      workload_calibrated = round(n_in / nrow(tr), 4),
      workload_2024 = round(n24 / nrow(te), 4),
      n_flagged_2024 = n24,
      precision = round(ifelse(n24 > 0, k24 / n24, NA), 4),
      recall = round(k24 / sum(tg_te$ie), 4),
      dollar_recall = round(d24 / sum(tg_te$ed), 4),
      target_base_rate = round(mean(tg_te$ie), 4))
    cat(sprintf("%-22s %2.0f%%: %3d rules | workload %4.1f%% -> %4.1f%% | prec %.3f | $%3.0f%%\n",
                target, 100 * b, length(frozen), 100 * n_in / nrow(tr),
                100 * n24 / nrow(te),
                ifelse(n24 > 0, k24 / n24, NA), 100 * d24 / sum(tg_te$ed)))
    if (b == 0.10) {
      hand <- pool[frozen, c("rule", "hh", "n_flagged_train",
                             "precision_train", "precision_train_lcb")]
      hand$rank <- seq_len(nrow(hand))
      write.csv(hand, file.path(list_dir, sprintf("frozen_list_%s.csv",
                                                  gsub(" ", "_", target))),
                row.names = FALSE)
    }
  }
}
out <- bind_rows(res)
write.csv(out, file.path(out_dir, "frozen_list_results.csv"), row.names = FALSE)
cat(sprintf("wrote %s + per-state lists in %s/\n",
            file.path(out_dir, "frozen_list_results.csv"), list_dir))
