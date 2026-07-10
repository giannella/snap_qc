# ──────────────────────────────────────────────────────────────────────────────
# Full accounting of above-threshold errors per state and fiscal year, from the
# RAW public QC files — BEFORE any exclusion or substantive transformation.
#
# Per state x fiscal year:
#   raw_cases            rows in the raw public file
#   errors_over_thr      cases with reported error amount (AMTERR) above the
#                        year's QC tolerance threshold
#   err_dollars_wtd      sum of AMTERR x FYWGT over those cases (annualized,
#                        weighted error dollars)
#   ineligible_units     error cases EXCLUDED from the public file entirely
#                        (from the technical manuals' exclusion counts,
#                        additional_data/snap_qc_exclusion_all_years.csv)
#
# Output: methods/state_error_accounting/errors_by_state_year_raw.csv
# ──────────────────────────────────────────────────────────────────────────────

suppressMessages({library(haven); library(dplyr)})
folder <- "C:/Users/ericg/qc/"

files <- c("qc_pub_fy2017.sav", "qc_pub_fy2018.sav", "qc_pub_fy2019.sav",
           "qc_pub_fy2022.sav", "qc_pub_fy2023.sav", "qc_pub_fy2024.sav")
raw <- bind_rows(lapply(files, function(f)
  read_sav(paste0(folder, "qc_data/", f)) %>%
    select(STATE, YRMONTH, AMTERR, FYWGT)))

state_data <- read.csv(paste0(folder, "additional_data/state_data.csv"))
raw$state <- setNames(state_data$state, as.character(state_data$fips))[as.character(raw$STATE)]
raw$fy <- ifelse(as.integer(substr(raw$YRMONTH, 5, 6)) >= 10,
                 as.integer(substr(raw$YRMONTH, 1, 4)) + 1L,
                 as.integer(substr(raw$YRMONTH, 1, 4)))
yd <- read.csv(paste0(folder, "additional_data/year_data.csv"))
raw$threshold <- setNames(yd$error_threshold, as.character(yd$year))[as.character(raw$fy)]
raw$is_err <- !is.na(raw$AMTERR) & raw$AMTERR > raw$threshold

acct <- raw %>%
  group_by(state, fy) %>%
  summarise(raw_cases = dplyr::n(),
            errors_over_thr = sum(is_err),
            err_dollars_wtd = round(sum(AMTERR[is_err] * FYWGT[is_err], na.rm = TRUE)),
            .groups = "drop")

ex <- read.csv(paste0(folder, "additional_data/snap_qc_exclusion_all_years.csv")) %>%
  group_by(state, fy = fiscal_year) %>%
  summarise(ineligible_units = sum(ineligible_units, na.rm = TRUE),
            completed_reviews = sum(units_with_complete_reviews, na.rm = TRUE),
            .groups = "drop")

acct <- acct %>%
  left_join(ex, by = c("state", "fy")) %>%
  arrange(state, fy)

dir.create("methods/state_error_accounting", showWarnings = FALSE)
write.csv(acct, "methods/state_error_accounting/errors_by_state_year_raw.csv", row.names = FALSE)
cat(sprintf("wrote %d state-year rows -> methods/state_error_accounting/errors_by_state_year_raw.csv\n",
            nrow(acct)))

cat("\nfocal states:\n")
print(acct %>% filter(state %in% c("Washington", "Louisiana", "Virginia"), fy >= 2022) %>%
        as.data.frame(), row.names = FALSE)
cat("\nnational totals by fiscal year:\n")
print(acct %>% group_by(fy) %>%
        summarise(raw_cases = sum(raw_cases), errors = sum(errors_over_thr),
                  err_dollars_wtd = sum(err_dollars_wtd),
                  ineligible = sum(ineligible_units, na.rm = TRUE), .groups = "drop") %>%
        as.data.frame(), row.names = FALSE)
