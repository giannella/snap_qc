# Check the CPI-off frame (2026-10-03) against the final frame. The CPI-off
# frame is the tracked munging script run with its existing switch
# cpi_inflate_vars set to FALSE (and outputs redirected). Everything the CPI
# block does not touch must be identical; the raw dollar inputs must equal the
# final frame's review-year copies (*_nominal); the inflated dollar features
# must differ by the fiscal-year CPI factor.
#   Rscript methods/v270_seed_replicates/frame_check_cpioff.R
setwd("C:/Users/ericg/snap_qc")
a <- readRDS("archive_data/cpioff_build_2026-10-03/reg_model_data.rds")
b <- readRDS("reg_model_data.rds")
ok <- TRUE
chk <- function(lab, cond) { cat(sprintf("CHECK %-62s %s\n", lab, if (isTRUE(cond)) "PASS" else "FAIL")); if (!isTRUE(cond)) ok <<- FALSE }
same <- function(x, y) isTRUE(all.equal(x, y, check.attributes = FALSE))
chk("rows == 231619", nrow(a) == 231619L && nrow(b) == 231619L)
chk("row keys identical and in the same order", identical(paste(a$hhldno, a$fiscal_year, a$state), paste(b$hhldno, b$fiscal_year, b$state)))
for (v in c("over_threshold", "error_status", "total_error_amount", "rawben_rel_max", "unc_rawben_rel_max", "rawben", "benmax",
            "rawben_uncapped", "utilities_sua", "percent_abawd", "months_since_cert_n", "expedited_i", "elderly_disabled_i",
            "children_i", "married", "HH_size_n", "bbce_state_i", "count_divisible_by_100", "cert_HH_size_FS_n"))
  if (v %in% names(a) && v %in% names(b)) chk(paste(v, "identical"), same(a[[v]], b[[v]])) else chk(paste(v, "present in both"), FALSE)
# the export renames rawmedded -> medical_deductions and rawutil -> utilities in both frames
CPIOFF_COL <- c(rawearn = "rawearn", rawunearn = "rawunearn", rawmedded = "medical_deductions", rawdepded = "rawdepded",
                rawcsded = "rawcsded", rawrent = "rawrent", rawutil = "utilities", rawhomeless_ded = "rawhomeless_ded")
for (k in names(CPIOFF_COL)) {
  v <- CPIOFF_COL[[k]]; nv <- paste0(k, "_nominal")
  if (v %in% names(a) && nv %in% names(b)) chk(sprintf("CPI-off %s == final %s", v, nv), same(a[[v]], b[[nv]])) else chk(paste(v, "/", nv, "present"), FALSE)
}
chk("CPI-off frame carries no *_nominal columns (the CPI block did not run)", !any(grepl("_nominal$", names(a))))
chk("the only columns that differ are the final frame's eight *_nominal copies", length(setdiff(names(a), names(b))) == 0 && all(grepl("_nominal$", setdiff(names(b), names(a)))) && length(setdiff(names(b), names(a))) == 8)
yd <- read.csv("additional_data/year_data.csv", check.names = FALSE); names(yd)[1] <- "year"
fy <- as.character(a$fiscal_year)
cat("\nmedian final / CPI-off where both > 0, by fiscal year (expected: the October-CPI factor to 2026 for income; other fields also carry the 2026 tables):\n")
for (v in c("earned_by_hh_size", "unearned_by_hh_size", "gross_by_hh_size", "shelter_expenses_by_hh_size", "medical_deductions", "total_deductions_by_hh_size")) {
  r <- sapply(c("2017", "2018", "2019", "2022", "2023", "2024"), function(y) { s <- fy == y & a[[v]] > 0 & b[[v]] > 0; s[is.na(s)] <- FALSE; median(b[[v]][s] / a[[v]][s]) })
  cat(sprintf("  %-30s %s\n", v, paste(names(r), sprintf("%.3f", r), collapse = "  ")))
}
cat(sprintf("  %-30s %s\n", "expected factor (cpi 2026 / cpi FY)", paste(c(2017:2019, 2022:2024), sprintf("%.3f", 325.604 / yd$cpi[match(c(2017:2019, 2022:2024), yd$year)]), collapse = "  ")))
s <- fy %in% c("2022", "2023", "2024")
mm <- a$rawben >= a$benmax & a$rawben_uncapped < a$benmax
chk("mismatch rows FY2022-24 == 572 (as in every frame)", sum(mm[s], na.rm = TRUE) == 572)
chk("FY2022-23 rows 76031 with 8397 errors; FY2024 39528 with 4764",
    sum(fy %in% c("2022", "2023")) == 76031 && sum(a$over_threshold[fy %in% c("2022", "2023")] != 0, na.rm = TRUE) == 8397 &&
    sum(fy == "2024") == 39528 && sum(a$over_threshold[fy == "2024"] != 0, na.rm = TRUE) == 4764)
cat(if (ok) "=== FRAME CHECK OK\n" else "=== FRAME CHECK FAILED\n")
