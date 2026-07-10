# How similar are the CONTRIBUTING rule sets across states? For each of the
# 18 workshop states, budget-fill the national_all pool on the state's 2024
# caseload and keep the rules that add at least one new case (the deployed
# short list). Then measure cross-state overlap: pairwise Jaccard, and how
# many states each deployed rule serves.
#
# Expects `reg_model_data`. Outputs:
#   methods/state_similarity_v2/transfer_benchmark_train2223_test24/
#     contributing_rules_by_state.csv     (rule x state incidence, long form)
#     contributing_overlap_jaccard_budget10.csv
#     contributing_overlap_summary.csv

suppressMessages(library(dplyr))
source("rule_mining_helpers.R")

TARGETS <- c("Louisiana", "Washington", "Virginia", "Arizona", "Connecticut",
             "Michigan", "North Carolina", "California", "Texas",
             "Mississippi", "New Jersey", "Colorado",
             "Maine", "Maryland", "Missouri", "Massachusetts",
             "District of Columbia", "Tennessee")
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
ord <- order(-pool$precision_train_lcb)
cat(sprintf("pool: %d rules\n", nrow(pool)))

adf24 <- adf_all[as.character(adf_all$fiscal_year) == "2024", , drop = FALSE]
st <- as.character(adf24$state)

sets <- list()   # sets[[budget]][[state]] = integer vector of pool row indices
inc <- list()
for (target in TARGETS) {
  tgt <- adf24[st == target, , drop = FALSE]
  strata <- lapply(setNames(nm = c("1", "2-3", "4+")), function(h)
    which(hh_group_of(tgt$cert_HH_size_FS_n) %in% h))
  idx <- flags_for_rules(pool, tgt, strata, label = "")
  for (b in BUDGETS) {
    cap <- floor(b * nrow(tgt))
    un <- rep(FALSE, nrow(tgt)); n_in <- 0L; contrib <- integer(0)
    for (i in ord) {
      ix <- idx[[i]]
      add <- sum(!un[ix])
      if (add > 0 && n_in + add <= cap) {
        un[ix] <- TRUE; n_in <- n_in + add; contrib <- c(contrib, i)
      }
    }
    bkey <- sprintf("%.2f", b)
    sets[[bkey]][[target]] <- contrib
    inc[[length(inc) + 1]] <- data.frame(
      target = target, budget = b, pool_row = contrib,
      hh = pool$hh[contrib], rule = pool$rule[contrib],
      stringsAsFactors = FALSE)
  }
  cat(sprintf("%-22s contributing: %3d (5%%) | %3d (10%%)\n", target,
              length(sets[["0.05"]][[target]]), length(sets[["0.10"]][[target]])))
}
write.csv(bind_rows(inc), file.path(out_dir, "contributing_rules_by_state.csv"),
          row.names = FALSE)

## overlap measures at each budget
summ <- list()
for (bkey in names(sets)) {
  ss <- sets[[bkey]]
  jac <- matrix(NA_real_, length(TARGETS), length(TARGETS),
                dimnames = list(TARGETS, TARGETS))
  for (a in TARGETS) for (b2 in TARGETS) {
    jac[a, b2] <- length(intersect(ss[[a]], ss[[b2]])) /
                  max(1, length(union(ss[[a]], ss[[b2]])))
  }
  if (bkey == "0.10")
    write.csv(round(jac, 3),
              file.path(out_dir, "contributing_overlap_jaccard_budget10.csv"))
  all_rules <- unlist(ss)
  freq <- table(all_rules)
  off <- jac[upper.tri(jac)]
  summ[[length(summ) + 1]] <- data.frame(
    budget = as.numeric(bkey),
    total_rule_slots = length(all_rules),
    distinct_rules = length(freq),
    rules_in_1_state = sum(freq == 1),
    rules_in_2plus = sum(freq >= 2),
    rules_in_5plus = sum(freq >= 5),
    rules_in_10plus = sum(freq >= 10),
    max_states_served = max(freq),
    median_pairwise_jaccard = round(median(off), 3),
    max_pairwise_jaccard = round(max(off), 3))
}
out <- bind_rows(summ)
write.csv(out, file.path(out_dir, "contributing_overlap_summary.csv"), row.names = FALSE)
print(as.data.frame(out))
cat("wrote overlap artifacts to", out_dir, "\n")
