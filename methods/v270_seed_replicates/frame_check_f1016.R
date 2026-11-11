# Check the 2026-10-16 frame (staging-v2.7 b53f340: Ben's lookup-table audit and
# homeless / child-support reconstruction fixes) against the v2.7 final frame.
# Hard checks (the chain stops on FAIL): same cases in the same order, the same
# FY2022-24 error flags and counts the study expects. Everything else is
# descriptive: how many FY2022-24 rows change on each mined feature, by year,
# and the SUA-tier transitions.
#   SR_ROOT=C:/Users/ericg/snap_qc_v27 Rscript methods/v270_seed_replicates/frame_check_f1016.R
source("methods/v270_seed_replicates/common.R")
a <- readRDS(FRAMES[["f1016"]])
b <- readRDS(FRAMES[["final"]])
ok <- TRUE
chk <- function(lab, cond) { cat(sprintf("CHECK %-62s %s\n", lab, if (isTRUE(cond)) "PASS" else "FAIL")); if (!isTRUE(cond)) ok <<- FALSE }
same <- function(x, y) isTRUE(all.equal(x, y, check.attributes = FALSE))
chk("rows == 231619", nrow(a) == 231619L && nrow(b) == 231619L)
chk("row keys identical and in the same order", identical(paste(a$hhldno, a$fiscal_year, a$state), paste(b$hhldno, b$fiscal_year, b$state)))
for (v in c("over_threshold", "error_status", "total_error_amount", "cert_HH_size_FS_n"))
  chk(paste(v, "identical"), v %in% names(a) && v %in% names(b) && same(a[[v]], b[[v]]))
fy <- as.character(a$fiscal_year)
chk("FY2022-23 rows 76031 with 8397 errors; FY2024 39528 with 4764",
    sum(fy %in% c("2022", "2023")) == 76031 && sum(a$over_threshold[fy %in% c("2022", "2023")] != 0, na.rm = TRUE) == 8397 &&
    sum(fy == "2024") == 39528 && sum(a$over_threshold[fy == "2024"] != 0, na.rm = TRUE) == 4764)
cat(sprintf("\ncolumns only in the new frame: %s\ncolumns only in the final frame: %s\n",
            paste(setdiff(names(a), names(b)), collapse = ", "), paste(setdiff(names(b), names(a)), collapse = ", ")))
s <- fy %in% c("2022", "2023", "2024")
diffrows <- function(x, y) { d <- !((is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & x == y)); d }
cat("\nFY2022-24 rows whose value changed, per mined feature (FY2022 / FY2023 / FY2024 / total of 115,559):\n")
for (v in c(VOCAB19, "rawben", "rawben_uncapped", "benmax")) {
  if (!(v %in% names(a) && v %in% names(b))) { cat(sprintf("  %-30s missing in one frame\n", v)); next }
  d <- diffrows(a[[v]], b[[v]])
  cat(sprintf("  %-30s %6d %6d %6d  %6d\n", v, sum(d & fy == "2022"), sum(d & fy == "2023"), sum(d & fy == "2024"), sum(d & s)))
}
cat("\nutilities_sua tier, FY2022-24 rows (rows: final frame; columns: new frame):\n")
print(table(final = b$utilities_sua[s], new = a$utilities_sua[s], useNA = "ifany"))
st <- as.character(a$state)
d <- diffrows(a$utilities_sua, b$utilities_sua) & s
if (any(d)) { cat("\nSUA-tier changes by state and fiscal year:\n"); print(table(st[d], fy[d])) }
mm_a <- a$rawben >= a$benmax & a$rawben_uncapped < a$benmax
mm_b <- b$rawben >= b$benmax & b$rawben_uncapped < b$benmax
cat(sprintf("\nmismatch rows FY2022-24 (rawben at max, uncapped below): final %d, new %d\n", sum(mm_b[s], na.rm = TRUE), sum(mm_a[s], na.rm = TRUE)))
chk("mismatch rows FY2022-24 below 1000 (load_frame's guard)", sum(mm_a[s], na.rm = TRUE) < 1000)
cat(if (ok) "=== FRAME CHECK OK\n" else "=== FRAME CHECK FAILED\n")
if (!ok) quit(save = "no", status = 1)
