# Workbook headline with and without pile cases (2026-10-07, issue #29): the
# union of each workbook's shipped rules on the state's FY2022-24 demo cases,
# for the v2.7.0 rule sets (saved before the rebuild) and the pile-gated ones.
# By construction the all-cases figures must reproduce the workbooks' static
# union (workbook_headline_comparison.csv); the non-pile figures drop the
# income-pile cases from the caseload, which is the closer stand-in for a
# state's own case file.
#   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" methods/reconstruction_income_piles/headline_nonpile.R
setwd("C:/Users/ericg/snap_qc")
suppressMessages(library(dplyr))
source("rule_mining_helpers.R")
PKG <- "methods/excel_rules_for_states"
reg_model_data <- readRDS("reg_model_data.rds")
adf0 <- reg_model_data %>% filter(fiscal_year %in% c("2022", "2023", "2024"))
vocab <- c("HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size",
           "expedited_i", "bbce_state_i", "rawben_rel_max", "medical_deductions",
           "shelter_expenses_by_hh_size", "utilities_sua", "married", "homeless",
           "percent_abawd", "unc_rawben_rel_max", "months_since_cert_n",
           "count_divisible_by_100", "gross_by_hh_size", "earned_by_hh_size",
           "unearned_by_hh_size")
adf <- prep_features(adf0, vocab)$data
pile <- income_pile_rows(adf0)
ie <- !is.na(adf$over_threshold) & adf$over_threshold != 0
n <- suppressWarnings(as.numeric(as.character(adf$cert_HH_size_FS_n)))
hh <- ifelse(n <= 1, "1", ifelse(n <= 3, "2-3", "4+"))
cmp <- read.csv("methods/reconstruction_income_piles/workbook_headline_comparison.csv")
name_of <- c(setNames(state.name, state.abb), DC = "District of Columbia")
union_of <- function(eff_fn, rows) {
  e <- read.csv(eff_fn, check.names = FALSE)
  if ("ship" %in% names(e)) e <- e[as.logical(toupper(as.character(e$ship))), ]
  strata <- lapply(setNames(nm = c("1", "2-3", "4+")), function(h) which(hh[rows] == h))
  fl <- flags_for_rules(data.frame(rule = e$rule, hh = as.character(e$hh)), adf[rows, , drop = FALSE], strata)
  rows[sort(unique(unlist(fl)))]
}
out <- list()
for (i in seq_len(nrow(cmp))) {
  st <- cmp$state[i]; nm <- name_of[[st]]
  rows <- which(as.character(adf$state) == nm)
  for (v in c("old", "new")) {
    fn <- if (v == "old") file.path(PKG, ".build/pre_pilegate_2026-10-07/effective", sprintf("effective_rules_%s.csv", st))
          else file.path(PKG, ".build", sprintf("effective_rules_%s.csv", st))
    f <- union_of(fn, rows)
    out[[length(out) + 1]] <- data.frame(state = st, version = v, cases = length(rows),
      flagged = length(f), errors = sum(ie[f]),
      nonpile_cases = sum(!pile[rows]), flagged_nonpile = sum(!pile[f]), errors_nonpile = sum(ie[f] & !pile[f]),
      pile_flagged = sum(pile[f]), pile_errors = sum(pile[f] & ie[f]))
  }
}
r <- bind_rows(out)
chk <- r %>% select(state, version, flagged, errors) %>%
  tidyr::pivot_wider(names_from = version, values_from = c(flagged, errors)) %>%
  inner_join(cmp %>% select(state, flagged_old_wb = flagged_old, errors_old_wb = errors_old,
                            flagged_new_wb = flagged_new, errors_new_wb = errors_new), by = "state")
bad <- chk %>% filter(flagged_old != flagged_old_wb | errors_old != errors_old_wb |
                      flagged_new != flagged_new_wb | errors_new != errors_new_wb)
cat(sprintf("reproduces the workbooks' static union in %d of %d states\n", nrow(chk) - nrow(bad), nrow(chk)))
if (nrow(bad)) print(as.data.frame(bad))
write.csv(r, "methods/reconstruction_income_piles/workbook_headline_nonpile.csv", row.names = FALSE)
w <- r %>% mutate(p_all = errors / pmax(flagged, 1), p_np = errors_nonpile / pmax(flagged_nonpile, 1)) %>%
  select(state, version, p_all, p_np, pile_flagged, pile_errors) %>%
  tidyr::pivot_wider(names_from = version, values_from = c(p_all, p_np, pile_flagged, pile_errors)) %>%
  mutate(d_all_pp = round(100 * (p_all_new - p_all_old), 2), d_np_pp = round(100 * (p_np_new - p_np_old), 2),
         released = !state %in% c("DC", "GA"))
for (lab in c("released", "all")) {
  q <- if (lab == "released") w[w$released, ] else w
  cat(sprintf("%s (%d): within 3 points on all cases %d | on non-pile cases %d | non-pile change median %+.2f pp, mean %+.2f pp, worst %+.2f pp (%s)\n",
              lab, nrow(q), sum(q$d_all_pp >= -3), sum(q$d_np_pp >= -3), median(q$d_np_pp), mean(q$d_np_pp),
              min(q$d_np_pp), q$state[which.min(q$d_np_pp)]))
}
print(as.data.frame(w %>% arrange(d_all_pp) %>% head(6) %>%
  select(state, p_all_old, p_all_new, d_all_pp, p_np_old, p_np_new, d_np_pp, pile_errors_old, pile_errors_new)), digits = 3)
