d <- readRDS("reg_model_data.rds")
ie <- !is.na(d$over_threshold) & d$over_threshold != 0
yrs <- d$fiscal_year %in% c(2022, 2023, 2024)

audit <- function(rec, obs, label) {
  ok0 <- abs(rec - obs) <= 0; ok1 <- abs(rec - obs) <= 1; ok5 <- abs(rec - obs) <= 5
  cat("\n", label, "(FY2022-24):\n")
  cat("  exact:", round(mean(ok0[yrs]), 4), "| within $1:", round(mean(ok1[yrs]), 4),
      "| within $5:", round(mean(ok5[yrs]), 4), "\n")
  cat("  among NO-ERROR cases: within $1", round(mean(ok1[yrs & !ie]), 4),
      "| among ERROR cases: within $1", round(mean(ok1[yrs & ie]), 4), "\n")
  cat("  error rate when mismatch > $5:", round(mean(ie[yrs & !ok5]), 4),
      "| when match <= $1:", round(mean(ie[yrs & ok1]), 4), "\n")
  invisible(ok1)
}
ok1_raw <- audit(d$raw_benefit_amount, d$rawben, "RAW side: recreated vs recorded agency benefit")
audit(d$fsben_recreated, d$benefit_amount_FS, "FS side: recreated vs QC-corrected benefit")

cs <- tapply(!ok1_raw[yrs & !ie], d$state[yrs & !ie], mean)
cs <- sort(cs, decreasing = TRUE)
cat("\nRAW-side clean-case mismatch rate (> $1) by state - top 8 / bottom 3:\n")
print(round(head(cs, 8), 3)); print(round(tail(cs, 3), 3))

# at-max slice (the consult's definition) for continuity
atmax_rec <- d$rawben == d$benmax
mm <- atmax_rec & d$unc_rawben_rel_max < 1
cat("\nrecorded-at-max rows FY22-24:", sum(atmax_rec[yrs]),
    "| of them reconstruction lands below max:", sum(mm[yrs]),
    sprintf("(%.1f%%)", 100 * mean(mm[yrs & atmax_rec])),
    "| err rate of those:", round(mean(ie[yrs & mm]), 3), "\n")
