# ──────────────────────────────────────────────────────────────────────────────
# ESAP coverage check: is elderly_disabled_i adequately handled as a FEATURE,
# or do elderly/disabled households need their own mining frame?
#
# Two measurements on the INCL v2 shortlist (run after the driver):
#   1. how often elderly_disabled_i appears in shortlist rules (and which way);
#   2. THE decisive one: union hold-out recall among errors in elderly/disabled
#      households vs errors in other households, plus each group's share of
#      holdout errors vs share of union catches. Parity -> feature suffices;
#      a material gap -> add a dedicated elderly-target vocabulary (additive,
#      like the typed frames), NOT a carve-out stratum.
#
# Expects `reg_model_data`. Reads inclusion_rules_by_hh_size_v2/
# final_rules_highprecision_all_frames.csv.
# ──────────────────────────────────────────────────────────────────────────────

library(dplyr)
source("rule_mining_helpers.R")

HOLDOUT_YEARS <- c("2023")
TARGET_IS_ERROR <- quote(!is.na(over_threshold) & over_threshold != 0)
HH_SIZE_COL <- "cert_HH_size_FS_n"
hh_group_of <- function(n) {
  n <- suppressWarnings(as.numeric(as.character(n)))
  ifelse(is.na(n), NA_character_, ifelse(n <= 1, "1", ifelse(n <= 3, "2-3", "4+")))
}
features <- c(
  "HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size",
  "expedited_i", "cat_elig", "rawben_rel_max", "medical_deductions",
  "shelter_expenses_by_hh_size", "utilities", "married", "homeless",
  "rawearn_by_hh_size", "rawunearn_by_hh_size", "rawgross_by_hh_size",
  "percent_abawd", "unc_rawben_rel_max",
  "months_since_cert_n", "count_divisible_by_100"
)

rules <- read.csv("inclusion_rules_by_hh_size_v2/final_rules_highprecision_all_frames.csv",
                  stringsAsFactors = FALSE)
cat(sprintf("shortlist: %d rules\n\n", nrow(rules)))

## 1. indicator usage in the rules themselves
uses     <- grepl("elderly_disabled_i", rules$rule, fixed = TRUE)
# after 0/1 coercion, "> 0.5"-style conditions require TRUE; "<= 0.5" requires FALSE
requires_elderly <- grepl("elderly_disabled_i\\s*(>|>=)", rules$rule)
requires_non     <- grepl("elderly_disabled_i\\s*(<|<=)", rules$rule)
cat(sprintf("rules using elderly_disabled_i: %d of %d (%.0f%%) — %d require elderly/disabled, %d require NOT\n\n",
            sum(uses), nrow(rules), 100 * mean(uses),
            sum(requires_elderly), sum(requires_non)))

## 2. union coverage parity on the holdout
univ <- prep_features(reg_model_data %>% filter(fiscal_year %in% HOLDOUT_YEARS),
                      features)$data
ie <- eval(TARGET_IS_ERROR, envir = univ); ie[is.na(ie)] <- FALSE
eld <- univ$elderly_disabled_i == 1
strata <- lapply(setNames(nm = unique(rules$hh)), function(h)
  which(hh_group_of(univ[[HH_SIZE_COL]]) %in% h))

idx <- flags_for_rules(distinct(rules, rule, hh), univ, strata)
un <- rep(FALSE, nrow(univ)); for (ix in idx) un[ix] <- TRUE

grp_stats <- function(mask, label) {
  errs <- sum(ie & mask); caught <- sum(ie & mask & un)
  cat(sprintf("%-28s errors %5d | caught %4d | recall %.3f\n",
              label, errs, caught, ifelse(errs > 0, caught / errs, NA)))
  c(errs = errs, caught = caught)
}
cat("union hold-out recall by household group:\n")
e <- grp_stats(eld,  "elderly/disabled HHs")
o <- grp_stats(!eld, "other HHs")
cat(sprintf("\nelderly share of holdout errors: %.1f%% | share of union catches: %.1f%%\n",
            100 * e["errs"] / (e["errs"] + o["errs"]),
            100 * e["caught"] / (e["caught"] + o["caught"])))
cat(sprintf("union precision within elderly flags: %.3f | within other flags: %.3f\n",
            sum(ie & eld & un) / max(sum(eld & un), 1),
            sum(ie & !eld & un) / max(sum(!eld & un), 1)))
