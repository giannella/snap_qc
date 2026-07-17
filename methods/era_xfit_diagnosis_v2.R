# E6 diagnosis (pre-registration tripwire, 2026-07-17): the cross-fit arm
# confounded selection-free ORDERING with half-sized MINING. This run puts
# the two orderings on equal footing: the SAME half-A-mined vocabulary,
# the SAME admitted set, ordered two ways -
#   insample : Wilson LCB from half-A stats (the half the rules were mined
#              on - selection-cursed by construction)
#   honest   : Wilson LCB from half-B stats (untouched by mining -
#              selection-free by construction)
# walked as national-only lists on each state's 2019.
# Expectation (written before this run): honest >= insample at both budgets;
# if so, the winner's-curse understanding stands and E6's failure is
# attributed to the vocabulary-size confound.
# Output: methods/state_similarity_v2/era_validation_train1718_test19/
#         era_xfit_diagnosis.csv

suppressMessages(library(dplyr))
source("rule_mining_helpers.R")

TARGETS <- c("Massachusetts", "Michigan", "North Carolina", "Connecticut",
             "Arizona", "Washington",
             "Louisiana", "Virginia", "California", "Texas",
             "Mississippi", "New Jersey", "Colorado",
             "Maine", "Maryland", "Missouri",
             "District of Columbia", "Tennessee")
BUDGETS <- c(0.05, 0.10)
BUFFER_MULT <- 3
TRAIN_YEARS <- c("2017", "2018")
TEST_YEAR   <- "2019"
ERA_DIR <- "methods/state_similarity_v2/era_validation_train1718_test19"

features <- c(
  "HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size",
  "expedited_i", "cat_elig", "rawben_rel_max", "medical_deductions",
  "shelter_expenses_by_hh_size", "utilities", "married", "homeless",
  "rawearn_by_hh_size", "rawunearn_by_hh_size", "rawgross_by_hh_size",
  "percent_abawd", "unc_rawben_rel_max",
  "months_since_cert_n", "count_divisible_by_100")
HH_LEVELS <- c("1", "2-3", "4+")
hh_group_of <- function(n) {
  n <- suppressWarnings(as.numeric(as.character(n)))
  ifelse(is.na(n), NA_character_, ifelse(n <= 1, "1", ifelse(n <= 3, "2-3", "4+")))
}

pf <- prep_features(reg_model_data %>%
                      filter(fiscal_year %in% c(TRAIN_YEARS, TEST_YEAR)), features)
adf <- pf$data
yr <- as.character(adf$fiscal_year)
st <- as.character(adf$state)
in_tr <- yr %in% TRAIN_YEARS
ie_all <- !is.na(adf$over_threshold) & adf$over_threshold != 0
ed_all <- ifelse(ie_all, abs(ifelse(is.na(adf$total_error_amount), 0,
                                    adf$total_error_amount)), 0)

# rebuild the identical seed-118 split over the training rows
tr_rows <- which(in_tr)
tr_all <- adf[tr_rows, , drop = FALSE]
set.seed(118)
in_a <- sample(c(TRUE, FALSE), nrow(tr_all), replace = TRUE)
tra <- tr_all[in_a, , drop = FALSE]
ie_a <- ie_all[tr_rows][in_a]
strata_a <- lapply(setNames(nm = HH_LEVELS), function(h)
  which(hh_group_of(tra$cert_HH_size_FS_n) %in% h))

xf <- readRDS(file.path(ERA_DIR, "raw_vocab", "raw_xfit_national.rds"))
# half-A stats for the same rules
ck <- file.path(ERA_DIR, "raw_vocab", "raw_xfit_halfA_stats.rds")
if (file.exists(ck)) {
  ha <- readRDS(ck)
} else {
  idx_a <- flags_for_rules(xf, tra, strata_a, label = "half-A stats")
  ha <- data.frame(nA = lengths(idx_a),
                   kA = vapply(idx_a, function(ix) sum(ie_a[ix]), numeric(1)))
  saveRDS(ha, ck)
  rm(idx_a); invisible(gc())
}
xf$nA <- ha$nA; xf$kA <- ha$kA

# one admitted set for both arms: the production filter on half-B stats
base <- attr(xf, "base_rates")[xf$hh]
rawB <- ifelse(xf$n > 0, xf$k / xf$n, NA_real_)
adm <- !is.na(rawB) & xf$n >= 30 & rawB >= 0.05 & rawB > base & xf$nA > 0
p <- xf[adm, , drop = FALSE]
p$lcb_honest   <- wilson_lcb(p$k, p$n, 2.326)     # half-B: selection-free
p$lcb_insample <- wilson_lcb(p$kA, p$nA, 2.326)   # half-A: cursed
cat(sprintf("admitted (shared set): %d rules\n", nrow(p)))

walk_eval <- function(stat, idx_tr, idx_te, n_tr_rows, n_te_rows, ie_te, ed_te, b) {
  cap <- floor(b * n_tr_rows); cap_buf <- floor(BUFFER_MULT * b * n_tr_rows)
  un <- rep(FALSE, n_tr_rows); n_in <- 0L
  frozen <- integer(0); buffer <- integer(0)
  for (i in order(-stat)) {
    if (is.na(stat[i])) next
    add <- sum(!un[idx_tr[[i]]])
    if (add == 0) next
    if (n_in + add <= cap) {
      un[idx_tr[[i]]] <- TRUE; n_in <- n_in + add; frozen <- c(frozen, i)
    } else if (n_in + add <= cap_buf) {
      un[idx_tr[[i]]] <- TRUE; n_in <- n_in + add; buffer <- c(buffer, i)
    }
  }
  capT <- floor(b * n_te_rows)
  unT <- rep(FALSE, n_te_rows)
  for (i in c(frozen, buffer)) {
    add <- sum(!unT[idx_te[[i]]])
    if (add > 0 && sum(unT) + add <= capT) unT[idx_te[[i]]] <- TRUE
  }
  nb <- sum(unT)
  data.frame(workload = round(nb / n_te_rows, 4),
             precision = round(ifelse(nb > 0, sum(ie_te[unT]) / nb, NA), 4),
             dollar_recall = round(sum(ed_te[unT]) / sum(ed_te), 4))
}

res <- list()
for (target in TARGETS) {
  tr <- adf[st == target & in_tr, , drop = FALSE]
  te <- adf[st == target & yr == TEST_YEAR, , drop = FALSE]
  rows_te <- which(st == target & yr == TEST_YEAR)
  strata_tr <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(tr$cert_HH_size_FS_n) %in% h))
  strata_te <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(te$cert_HH_size_FS_n) %in% h))
  idx_tr <- flags_for_rules(p, tr, strata_tr, label = "")
  idx_te <- flags_for_rules(p, te, strata_te, label = "")
  ie_te <- ie_all[rows_te]; ed_te <- ed_all[rows_te]
  for (arm in c("honest", "insample")) {
    stat <- p[[paste0("lcb_", arm)]]
    for (b in BUDGETS) {
      res[[length(res) + 1]] <- cbind(
        data.frame(target = target, arm = arm, budget = b),
        walk_eval(stat, idx_tr, idx_te, nrow(tr), nrow(te), ie_te, ed_te, b))
    }
  }
  cat(sprintf("%-22s done\n", target))
}
out <- bind_rows(res)
write.csv(out, file.path(ERA_DIR, "era_xfit_diagnosis.csv"), row.names = FALSE)
cat("\nmedians (precision / dollar recall):\n")
print(as.data.frame(out %>% group_by(arm, budget) %>%
  summarise(med_prec = median(precision, na.rm = TRUE),
            med_dollars = median(dollar_recall, na.rm = TRUE), .groups = "drop")),
  row.names = FALSE)
