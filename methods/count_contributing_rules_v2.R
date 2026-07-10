# How many rules actually BUILD the budget-filled union? The budget fill
# scans the whole national pool in confidence order and admits any rule whose
# new flags fit, so rules whose cases are already flagged are admitted while
# adding nothing ("used" counts are inflated to 10-20k). This counts the
# rules that add at least one new case -- deploying exactly those reproduces
# the identical union, so it is the length of the list a state would deploy.
# Three example states x 2 budgets; national_all pool, target 2024.
#
# Expects `reg_model_data`. Output:
#   methods/state_similarity_v2/transfer_benchmark_train2223_test24/
#     contributing_rules_summary.csv

suppressMessages(library(dplyr))
source("rule_mining_helpers.R")

EXAMPLE_STATES <- c("District of Columbia", "Louisiana", "California")
BUDGETS <- c(0.05, 0.10)
out_dir <- "methods/state_similarity_v2/transfer_benchmark_train2223_test24"

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

adf_all <- prep_features(reg_model_data %>%
                           filter(fiscal_year %in% c("2022", "2023", "2024")),
                         features)$data
sig <- gsub("[^A-Za-z]", "", paste(sort(unique(as.character(adf_all$state))), collapse = "_"))
key <- if (nchar(sig) <= 80) sig else
  sprintf("%s_%08x", substr(sig, 1, 60),
          sum(utf8ToInt(sig) * seq_along(utf8ToInt(sig))) %% .Machine$integer.max)
pool <- readRDS(file.path(out_dir, "pool_cache", sprintf("pool_%s.rds", key)))
cat(sprintf("national_all pool: %d rules\n", nrow(pool)))

adf24 <- adf_all[as.character(adf_all$fiscal_year) == "2024", , drop = FALSE]
st <- as.character(adf24$state)

res <- list()
for (target in EXAMPLE_STATES) {
  tgt <- adf24[st == target, , drop = FALSE]
  strata <- lapply(setNames(nm = c("1", "2-3", "4+")), function(h)
    which(hh_group_of(tgt$cert_HH_size_FS_n) %in% h))
  idx <- flags_for_rules(pool, tgt, strata, label = "")
  for (b in BUDGETS) {
    cap <- floor(b * nrow(tgt))
    un <- rep(FALSE, nrow(tgt)); n_in <- 0L; used <- 0L; contributing <- 0L
    for (i in order(-pool$precision_train_lcb)) {
      ix <- idx[[i]]
      add <- sum(!un[ix])
      if (n_in + add <= cap) {
        used <- used + 1L
        if (add > 0) { contributing <- contributing + 1L; un[ix] <- TRUE; n_in <- n_in + add }
      }
    }
    res[[length(res) + 1]] <- data.frame(
      target = target, budget = b, cap_cases = cap,
      rules_counted_used = used, rules_contributing = contributing)
    cat(sprintf("%-22s budget %2.0f%%: cap %3d | used %5d | contributing %3d\n",
                target, 100 * b, cap, used, contributing))
  }
}
out <- bind_rows(res)
write.csv(out, file.path(out_dir, "contributing_rules_summary.csv"), row.names = FALSE)
cat(sprintf("wrote %s\n", file.path(out_dir, "contributing_rules_summary.csv")))
