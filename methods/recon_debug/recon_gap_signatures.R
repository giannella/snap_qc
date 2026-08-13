# Debugging clues for the raw-side benefit reconstruction (for Ben).
# Clean cases only where noted: no-error rows, where the label cannot
# explain a recreated-vs-recorded gap.
d <- readRDS("reg_model_data.rds")
cat("net/benefit-ish columns:", paste(grep("net|NET", names(d), value = TRUE), collapse = ", "), "\n\n")

yrs <- d$fiscal_year %in% c(2022, 2023, 2024)
ie  <- !is.na(d$over_threshold) & d$over_threshold != 0
d <- d[yrs, ]; ie <- ie[yrs]
clean <- !ie

gap <- d$raw_benefit_amount - d$rawben       # recreated minus recorded, signed
mis <- abs(gap) > 1

cat("=== signed gap, CLEAN cases with |gap| > $1 (n =", sum(mis & clean), ") ===\n")
cat("recreated too HIGH (gap>0):", round(mean(gap[mis & clean] > 0), 3),
    "| too LOW:", round(mean(gap[mis & clean] < 0), 3), "\n")
cat("quantiles of gap:", paste(round(quantile(gap[mis & clean], c(.05,.25,.5,.75,.95))), collapse = " / "), "\n")

# does the agency-side net income exist to decompose the failure?
if ("rawnet" %in% names(d)) {
  net_rec <- d$rawnet
  has_net <- !is.na(net_rec)
  cat("\nrecorded raw net income present:", sum(has_net), "of", nrow(d), "\n")
  # step A: formula from RECORDED net vs recorded benefit (tests the net->benefit step)
  stepA <- pmin(pmax(floor(d$benmax - 0.3 * pmax(floor(net_rec), -1e9)), d$rawminimum_ben), d$benmax)
  a_ok <- abs(stepA - d$rawben) <= 1
  # step B: OUR computed net vs recorded net (tests the income/deduction rebuild)
  b_ok <- abs(d$rawnet_allow_negative - floor(net_rec)) <= 1
  m <- mis & clean & has_net
  cat("among clean mismatches with recorded net (n =", sum(m), "):\n")
  cat("  step A fails (net->benefit wrong even from recorded net):", round(mean(!a_ok[m]), 3), "\n")
  cat("  step B fails (our net differs from recorded net):", round(mean(!b_ok[m]), 3), "\n")
  cat("  both hold yet benefit differs (cap/min/proration zone):",
      round(mean(a_ok[m] & b_ok[m]), 3), "\n")
} else cat("\n(no recorded raw net column in the frame under 'rawnet')\n")

cat("\n=== fingerprints: |gap| equals a known component (clean mismatches) ===\n")
mc <- mis & clean
cands <- list(
  c("0.3 x homeless deduction",   "rawhomeless_ded"),
  c("0.3 x std deduction",        "rawstdded"),
  c("0.3 x medical deduction",    "rawmedded"),
  c("0.3 x smd amount",           "smd_amt"),
  c("0.3 x earned-ded (0.2earn)", "rawernded"),
  c("minimum benefit",            "rawminimum_ben"),
  c("0.3 x shelter ded",          "rawsltded"))
for (cc in cands) {
  v <- d[[cc[2]]]
  if (is.null(v)) { cat(sprintf("  %-42s (column %s absent)\n", cc[1], cc[2])); next }
  x <- if (grepl("^0.3", cc[1])) 0.3 * v else v
  cat(sprintf("  %-42s %6.3f\n", cc[1], mean(abs(abs(gap[mc]) - x[mc]) <= 1, na.rm = TRUE)))
}

cat("\n=== per-state structure of clean mismatches (top mismatch states) ===\n")
tab <- do.call(rbind, lapply(c("Illinois","Texas","Virginia","Maryland","Michigan"), function(s) {
  m <- mc & d$state == s
  data.frame(state = s, n = sum(m),
             share_gap_pos = round(mean(gap[m] > 0), 2),
             med_gap = median(gap[m]),
             share_expedited = round(mean(d$expedited_i[m] == 1 | d$expedited_i[m] == TRUE, na.rm = TRUE), 2),
             share_earned = round(mean(d$rawearn[m] > 0), 2),
             share_homeless = round(mean(d$homeless[m] == TRUE | d$homeless[m] == 1, na.rm = TRUE), 2))
}))
print(tab, row.names = FALSE)
cat("\nIllinois clean mismatch by year:\n")
for (y in c(2022, 2023, 2024)) {
  m <- clean & d$state == "Illinois" & d$fiscal_year == y
  cat("  FY", y, ": rate", round(mean(mis[m]), 3), " (n cases", sum(m), ")\n")
}
cat("\nIllinois gap value table (clean mismatches, top 6 exact gaps):\n")
print(head(sort(table(gap[mc & d$state == "Illinois"]), decreasing = TRUE), 6))
cat("\nall-state gap value table (clean mismatches, top 8 exact gaps):\n")
print(head(sort(table(gap[mc]), decreasing = TRUE), 8))
