# Re-derive every number in modeling_findings §24 (munging row exclusions).
#
#   Rscript methods/verify_munging_exclusions.R
#
# Needs both frames:
#   reg_model_data.rds                                  (the production frame)
#   methods/munging_exclusion_check/reg_model_data_minexcl.rds
#     the minimal-exclusion rebuild. Not committed (it is ~60 MB and contains
#     rows the finding rejects). Rebuild it with
#     methods/test_munging_exclusions_minimal.R, which prints the same run log as
#     munging_exclusion_check/rebuild_run.log.
#
# Everything below is a check, not a pipeline step. Nothing is written to the
# production frame.

suppressMessages({library(dplyr); library(tidyr)})

A_PATH <- "reg_model_data.rds"
B_PATH <- "methods/munging_exclusion_check/reg_model_data_minexcl.rds"
B_ALT  <- "custom_one_off/snap_dashboard/.frames/reg_model_data_minexcl.rds"
if (!file.exists(B_PATH) && file.exists(B_ALT)) B_PATH <- B_ALT
for (f in c(A_PATH, B_PATH))
  if (!file.exists(f)) stop("missing ", f, " (see the header)")

ierr <- function(d) !is.na(d$over_threshold) & d$over_threshold != 0
amt  <- function(d) ifelse(is.na(d$total_error_amount), 0, abs(d$total_error_amount))

a <- readRDS(A_PATH); a$e <- ierr(a); a$d <- amt(a)
b <- readRDS(B_PATH); b$e <- ierr(b); b$d <- amt(b)

hr <- function(t) cat("\n== ", t, " ", strrep("=", max(0, 66 - nchar(t))), "\n", sep = "")

hr("1. frame totals")
cat(sprintf("production      : %s rows | %s errors | $%s | %d states\n",
            format(nrow(a), big.mark = ","), format(sum(a$e), big.mark = ","),
            format(round(sum(a$d[a$e])), big.mark = ","),
            length(unique(as.character(a$state)))))
cat(sprintf("minimal-exclusion: %s rows | %s errors | $%s | %d states\n",
            format(nrow(b), big.mark = ","), format(sum(b$e), big.mark = ","),
            format(round(sum(b$d[b$e])), big.mark = ","),
            length(unique(as.character(b$state)))))

hr("2. decomposition of the difference (must reconcile exactly)")
kept   <- b$amterr_reconciles
fy2020 <- as.character(b$fiscal_year) == "2020"
r1 <- nrow(a);                          e1 <- sum(a$e)
r2 <- sum(kept & fy2020);               e2 <- sum(b$e & kept & fy2020)
r3 <- sum(!kept);                       e3 <- sum(b$e & !kept)
cat(sprintf("production frame                         %8s rows  %7s errors\n",
            format(r1, big.mark = ","), format(e1, big.mark = ",")))
cat(sprintf("+ FY2020 rows passing the AMTERR filter   %8s rows  %7s errors\n",
            format(r2, big.mark = ","), format(e2, big.mark = ",")))
cat(sprintf("+ rows the AMTERR filter had dropped      %8s rows  %7s errors\n",
            format(r3, big.mark = ","), format(e3, big.mark = ",")))
cat(sprintf("= minimal-exclusion frame                 %8s rows  %7s errors\n",
            format(r1 + r2 + r3, big.mark = ","), format(e1 + e2 + e3, big.mark = ",")))
stopifnot(r1 + r2 + r3 == nrow(b), e1 + e2 + e3 == sum(b$e))
cat("reconciles exactly: TRUE\n")

hr("3. the AMTERR filter is additive-only (per-year identity check)")
A <- a %>% group_by(fy = as.character(fiscal_year)) %>%
  summarise(prod_rows = n(), prod_err = sum(e), .groups = "drop")
B <- b %>% filter(amterr_reconciles) %>% group_by(fy = as.character(fiscal_year)) %>%
  summarise(kept_rows = n(), kept_err = sum(e), .groups = "drop")
j <- inner_join(A, B, by = "fy") %>% arrange(fy)
print(as.data.frame(j))
cat(sprintf("years compared: %d | row mismatches: %d | error mismatches: %d\n",
            nrow(j), sum(j$prod_rows != j$kept_rows), sum(j$prod_err != j$kept_err)))

hr("4. FY2022-24: what the AMTERR filter excludes")
y <- c("2022", "2023", "2024")
s <- b %>% filter(as.character(fiscal_year) %in% y)
print(s %>% group_by(reconciles = amterr_reconciles) %>%
        summarise(rows = n(), errors = sum(e), err_rate = round(mean(e), 3),
                  err_dollars = round(sum(d[e])), .groups = "drop") %>% as.data.frame())

hr("5. does the pre-QC restoration converge on those rows?")
m <- readRDS(sub("reg_model_data_minexcl", "final_minexcl", B_PATH))
m <- m %>% filter(fiscal_year %in% c(2022, 2023, 2024))
m$resid <- abs(m$RAWBEN - m$rawben_recreated)
print(m %>% mutate(grp = ifelse(amterr_reconciles, "passes the filter", "excluded by it")) %>%
        group_by(grp) %>% summarise(
          rows = n(), resid_median = round(median(resid, na.rm = TRUE), 1),
          within_5_pct = round(100 * mean(resid <= 5, na.rm = TRUE), 1),
          over_50_pct = round(100 * mean(resid > 50, na.rm = TRUE), 1),
          no_change_pct = round(100 * mean(correctednotes == "no_change", na.rm = TRUE), 1),
          .groups = "drop") %>% as.data.frame())
ex <- m %>% filter(!amterr_reconciles)
cat(sprintf("excluded rows with AMTERR == 0: %d of %d (%.0f%%)\n",
            sum(ex$AMTERR == 0, na.rm = TRUE), nrow(ex),
            100 * mean(ex$AMTERR == 0, na.rm = TRUE)))
cat(sprintf("their |RAWBEN - FSBEN|: median $%.0f, mean $%.0f\n",
            median(ex$absbendiff, na.rm = TRUE), mean(ex$absbendiff, na.rm = TRUE)))

hr("6. Washington FY2022-24")
key <- c("yrmonth", "hhldno", "stratum")
sa <- a %>% filter(as.character(state) == "Washington", as.character(fiscal_year) %in% y)
sb <- b %>% filter(as.character(state) == "Washington", as.character(fiscal_year) %in% y)
cat(sprintf("production %d rows / %d errors | minimal-exclusion %d rows / %d errors\n",
            nrow(sa), sum(sa$e), nrow(sb), sum(sb$e)))
ov <- inner_join(sa %>% select(all_of(key), ea = e), sb %>% select(all_of(key), eb = e), by = key)
cat(sprintf("shared rows %d | error-flag disagreements %d\n", nrow(ov), sum(ov$ea != ov$eb)))
add <- anti_join(sb %>% select(all_of(key), e), sa %>% select(all_of(key)), by = key)
cat(sprintf("rows only in the minimal-exclusion frame: %d, of which %d errors (%.1f%%)\n",
            nrow(add), sum(add$e), 100 * mean(add$e)))
vars <- c("rawben_rel_max", "unc_rawben_rel_max", "total_deductions_by_hh_size",
          "shelter_expenses_by_hh_size", "utilities", "medical_deductions",
          "percent_abawd", "months_since_cert_n", "count_divisible_by_100",
          "HH_size_n", "cert_HH_size_FS_n", "total_error_amount")
mm <- inner_join(sa %>% select(all_of(c(key, vars))), sb %>% select(all_of(c(key, vars))),
                 by = key, suffix = c("_a", "_b"))
cat("feature disagreements on the shared rows:\n")
for (v in vars) {
  x <- suppressWarnings(as.numeric(mm[[paste0(v, "_a")]]))
  z <- suppressWarnings(as.numeric(mm[[paste0(v, "_b")]]))
  dd <- !(abs(x - z) < 1e-4 | (is.na(x) & is.na(z))); dd[is.na(dd)] <- TRUE
  if (sum(dd) > 0) cat(sprintf("  %-32s %d of %d\n", v, sum(dd), nrow(mm)))
}
cat("  (features not listed differ on 0 rows)\n")
cat("\nBENMAX filter: see methods/measure_benmax_filter.R and",
    "munging_exclusion_check/benmax_filter.log\n")
