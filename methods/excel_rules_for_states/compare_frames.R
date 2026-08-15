# Compare the repo's modelling frame with the minimal-exclusion rebuild.
#
#   Rscript custom_one_off/snap_dashboard/compare_frames.R [state]
#
# Reports, for each frame: rows, states, per-fiscal-year rows / errors / error
# rate / error dollars, and the same for one focus state (default Washington).
# Then reports what the relaxed exclusions actually added, using the flags the
# rebuild records (amterr_reconciles, shelter_fields_imputed).

suppressMessages(library(dplyr))

FOCUS <- if (length(commandArgs(trailingOnly = TRUE))) commandArgs(trailingOnly = TRUE)[1] else "Washington"
BASE <- "reg_model_data.rds"
NEW  <- "custom_one_off/snap_dashboard/.frames/reg_model_data_minexcl.rds"
for (f in c(BASE, NEW)) if (!file.exists(f)) stop("missing: ", f)

prep <- function(f, label) {
  d <- readRDS(f)
  d$is_err <- !is.na(d$over_threshold) & d$over_threshold != 0
  d$err_amt <- ifelse(is.na(d$total_error_amount), 0, abs(d$total_error_amount))
  d$frame <- label
  d
}
a <- prep(BASE, "repo")
b <- prep(NEW, "min_excl")

hr <- function(t) cat("\n", strrep("=", 74), "\n", t, "\n", strrep("=", 74), "\n", sep = "")

hr("TOTALS")
cat(sprintf("%-10s %10s %8s %8s %8s %14s\n",
            "frame", "rows", "cols", "states", "errors", "error $"))
for (d in list(a, b))
  cat(sprintf("%-10s %10s %8d %8d %8s %14s\n", d$frame[1],
              format(nrow(d), big.mark = ","), ncol(d),
              length(unique(as.character(d$state))),
              format(sum(d$is_err), big.mark = ","),
              format(round(sum(d$err_amt[d$is_err])), big.mark = ",")))
cat(sprintf("\nrows added by relaxing exclusions: %s (%.1f%% more)\n",
            format(nrow(b) - nrow(a), big.mark = ","),
            100 * (nrow(b) / nrow(a) - 1)))

by_year <- function(d, sub = NULL) {
  x <- if (is.null(sub)) d else d %>% filter(as.character(state) == sub)
  x %>% group_by(fiscal_year = as.character(fiscal_year)) %>%
    summarise(rows = n(), errors = sum(is_err),
              rate = round(mean(is_err), 4),
              err_dollars = round(sum(err_amt[is_err])), .groups = "drop")
}

hr("BY FISCAL YEAR, ALL STATES")
print(full_join(by_year(a), by_year(b), by = "fiscal_year",
                suffix = c("_repo", "_min")) %>% arrange(fiscal_year),
      n = Inf, width = Inf)

hr(paste("BY FISCAL YEAR,", toupper(FOCUS)))
print(full_join(by_year(a, FOCUS), by_year(b, FOCUS), by = "fiscal_year",
                suffix = c("_repo", "_min")) %>% arrange(fiscal_year),
      n = Inf, width = Inf)

hr("WHAT THE RELAXED EXCLUSIONS ADDED")
yrs <- c("2022", "2023", "2024")
newer <- b %>% filter(as.character(fiscal_year) %in% yrs)
cat(sprintf("restricted to FY%s (the years the delivery lists use): %s rows\n",
            paste(yrs, collapse = "+"), format(nrow(newer), big.mark = ",")))
if ("amterr_reconciles" %in% names(b)) {
  t <- newer %>% group_by(reconciles = amterr_reconciles) %>%
    summarise(rows = n(), errors = sum(is_err), rate = round(mean(is_err), 4),
              err_dollars = round(sum(err_amt[is_err])), .groups = "drop")
  cat("\nrows the AMTERR reconciliation filter used to drop (reconciles = FALSE):\n")
  print(t, n = Inf, width = Inf)
}
if ("shelter_fields_imputed" %in% names(b)) {
  t <- newer %>% group_by(shelter_imputed = shelter_fields_imputed) %>%
    summarise(rows = n(), errors = sum(is_err), rate = round(mean(is_err), 4),
              .groups = "drop")
  cat("\nrows with zero-filled RENT/UTIL (used to be dropped):\n")
  print(t, n = Inf, width = Inf)
}
if ("ded_fields_imputed" %in% names(b)) {
  cat("\nrows with zero-filled deduction fields (already kept in both frames):\n")
  print(newer %>% group_by(ded_imputed = ded_fields_imputed) %>%
          summarise(rows = n(), errors = sum(is_err),
                    rate = round(mean(is_err), 4), .groups = "drop"),
        n = Inf, width = Inf)
}

hr(paste(toupper(FOCUS), "FY2022-24: DOES THE OVERLAP AGREE?"))
key <- c("yrmonth", "hhldno", "stratum")
sa <- a %>% filter(as.character(state) == FOCUS, as.character(fiscal_year) %in% yrs)
sb <- b %>% filter(as.character(state) == FOCUS, as.character(fiscal_year) %in% yrs)
cat(sprintf("repo %d rows | min_excl %d rows\n", nrow(sa), nrow(sb)))
j <- inner_join(sa %>% select(all_of(key), is_err_a = is_err),
                sb %>% select(all_of(key), is_err_b = is_err), by = key)
cat(sprintf("rows in both: %d | error flag disagreements: %d\n",
            nrow(j), sum(j$is_err_a != j$is_err_b)))
onlyb <- anti_join(sb %>% select(all_of(key), is_err), sa %>% select(all_of(key)), by = key)
cat(sprintf("rows only in min_excl: %d (%d of them errors, %.1f%%)\n",
            nrow(onlyb), sum(onlyb$is_err),
            100 * ifelse(nrow(onlyb) > 0, mean(onlyb$is_err), 0)))
onlya <- anti_join(sa %>% select(all_of(key)), sb %>% select(all_of(key)), by = key)
cat(sprintf("rows only in repo: %d (should be 0)\n", nrow(onlya)))

hr("FEATURE AGREEMENT ON THE SHARED ROWS")
vars <- intersect(c("HH_size_n", "cert_HH_size_FS_n", "rawben_rel_max",
                    "unc_rawben_rel_max", "total_deductions_by_hh_size",
                    "shelter_expenses_by_hh_size", "utilities",
                    "medical_deductions", "percent_abawd", "months_since_cert_n",
                    "count_divisible_by_100", "total_error_amount"), names(a))
ja <- sa %>% select(all_of(c(key, vars)))
jb <- sb %>% select(all_of(c(key, vars)))
m <- inner_join(ja, jb, by = key, suffix = c("_a", "_b"))
cat(sprintf("%-32s %8s %7s\n", "feature", "n_diff", "pct"))
for (v in vars) {
  x <- suppressWarnings(as.numeric(m[[paste0(v, "_a")]]))
  y <- suppressWarnings(as.numeric(m[[paste0(v, "_b")]]))
  d <- !(abs(x - y) < 1e-4 | (is.na(x) & is.na(y)))
  d[is.na(d)] <- TRUE
  cat(sprintf("%-32s %8d %6.1f%%\n", v, sum(d), 100 * mean(d)))
}
cat("\nA nonzero count here means relaxing the exclusions changed a feature on a\n")
cat("row both frames contain, i.e. through the lookups and correction loops the\n")
cat("added rows feed, not just by adding rows.\n")
