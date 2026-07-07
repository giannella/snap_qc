# ──────────────────────────────────────────────────────────────────────────────
# Rule-set DIFF: how did a pipeline/data change move the mined rule content?
#
# Compares an OLD and NEW shortlist (same columns as the INCL v2 outputs) and
# classifies every rule:
#   exact      identical rule text + stratum in both sets
#   shifted    same structure (variables + directions per .rule_struct) in the
#              same stratum, thresholds moved — reported with mean |shift|
#   overlap    no structural twin, but a rule in the other set flags a highly
#              overlapping case set (Jaccard >= JACCARD_MATCH on EVAL_YEARS)
#   dropped /  present on one side only with no counterpart above
#   new
# Also reports where NEW-only rules' catches concentrate (e.g., share of their
# caught errors that are multi-element cases), to verify a data change did
# what it was supposed to.
#
# Usage: set OLD_CSV / NEW_CSV / EVAL_YEARS, source with reg_model_data loaded.
# ──────────────────────────────────────────────────────────────────────────────

suppressMessages(library(dplyr))
source("rule_mining_helpers.R")

OLD_CSV <- "inclusion_rules_by_hh_size_v2/run3_singleelement_frame/final_rules_highprecision_all_frames.csv"
NEW_CSV <- "inclusion_rules_by_hh_size_v2/final_rules_highprecision_all_frames.csv"
EVAL_YEARS <- c("2023")
JACCARD_MATCH <- 0.5
OUT_CSV <- "inclusion_rules_by_hh_size_v2/rule_diff_old_vs_new.csv"

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

old <- read.csv(OLD_CSV, stringsAsFactors = FALSE) %>%
  distinct(rule, hh, .keep_all = TRUE) %>% mutate(side = "old")
new <- read.csv(NEW_CSV, stringsAsFactors = FALSE) %>%
  distinct(rule, hh, .keep_all = TRUE) %>% mutate(side = "new")
cat(sprintf("old: %d rules | new: %d rules\n", nrow(old), nrow(new)))

## 1. exact matches
key_old <- paste(old$hh, old$rule); key_new <- paste(new$hh, new$rule)
old$status <- ifelse(key_old %in% key_new, "exact", NA)
new$status <- ifelse(key_new %in% key_old, "exact", NA)

## 2. structural matches (same signature + stratum), thresholds shifted
sig_of <- function(df) paste(df$hh, vapply(lapply(df$rule, .rule_struct),
                                           function(s) s$sig, ""))
old$sig <- sig_of(old); new$sig <- sig_of(new)
o_open <- is.na(old$status); n_open <- is.na(new$status)
shift_amt <- rep(NA_real_, nrow(new))
for (i in which(n_open)) {
  j <- which(o_open & old$sig == new$sig[i])
  if (length(j) > 0) {
    new$status[i] <- "shifted"
    old$status[j[1]] <- "shifted"; o_open[j[1]] <- FALSE
    a <- .rule_struct(new$rule[i]); b <- .rule_struct(old$rule[j[1]])
    if (length(a$thr) == length(b$thr))
      shift_amt[i] <- mean(abs(a$thr - b$thr) / pmax(abs(b$thr), 1e-9))
  }
}
new$mean_threshold_shift_pct <- round(100 * shift_amt, 1)

## 3. coverage-overlap matches on EVAL_YEARS for the remainder
univ <- prep_features(reg_model_data %>% filter(fiscal_year %in% EVAL_YEARS),
                      features)$data
strata <- lapply(setNames(nm = unique(c(old$hh, new$hh))), function(h)
  which(hh_group_of(univ[[HH_SIZE_COL]]) %in% h))
rem_old <- old[is.na(old$status), ]; rem_new <- new[is.na(new$status), ]
if (nrow(rem_old) > 0 && nrow(rem_new) > 0) {
  idx_o <- flags_for_rules(rem_old, univ, strata)
  idx_n <- flags_for_rules(rem_new, univ, strata)
  for (i in seq_len(nrow(rem_new))) {
    cand <- which(rem_old$hh == rem_new$hh[i])
    best <- 0
    for (j in cand) {
      u <- length(union(idx_n[[i]], idx_o[[j]]))
      jac <- if (u == 0) 0 else length(intersect(idx_n[[i]], idx_o[[j]])) / u
      if (jac > best) best <- jac
    }
    if (best >= JACCARD_MATCH) {
      new$status[new$rule == rem_new$rule[i] & new$hh == rem_new$hh[i]] <- "overlap"
    }
  }
  for (j in seq_len(nrow(rem_old))) {
    cand <- which(rem_new$hh == rem_old$hh[j])
    best <- 0
    for (i in cand) {
      u <- length(union(idx_n[[i]], idx_o[[j]]))
      jac <- if (u == 0) 0 else length(intersect(idx_n[[i]], idx_o[[j]])) / u
      if (jac > best) best <- jac
    }
    if (best >= JACCARD_MATCH)
      old$status[old$rule == rem_old$rule[j] & old$hh == rem_old$hh[j]] <- "overlap"
  }
}
new$status[is.na(new$status)] <- "new"
old$status[is.na(old$status)] <- "dropped"

## 4. summary + where NEW rules' catches concentrate
cat("\nNEW set composition:\n"); print(table(new$error_frame, new$status))
cat("\nOLD set fates:\n"); print(table(old$error_frame, old$status))

ot <- suppressWarnings(as.numeric(as.character(univ$over_threshold)))
ie <- !is.na(ot) & ot != 0
if ("second_element_i" %in% names(univ)) {
  brand_new <- new[new$status == "new", ]
  if (nrow(brand_new) > 0) {
    idx_b <- flags_for_rules(brand_new, univ, strata)
    caught <- unique(unlist(idx_b)); caught <- caught[ie[caught]]
    cat(sprintf("\nNEW-only rules on %s: %d errors caught; %.0f%% are multi-element cases (universe: %.0f%%)\n",
                paste(EVAL_YEARS, collapse = "/"), length(caught),
                100 * mean(univ$second_element_i[caught], na.rm = TRUE),
                100 * mean(univ$second_element_i[ie], na.rm = TRUE)))
  }
}
write.csv(bind_rows(old, new) %>% select(-sig),
          OUT_CSV, row.names = FALSE)
cat(sprintf("wrote %s\n", OUT_CSV))
