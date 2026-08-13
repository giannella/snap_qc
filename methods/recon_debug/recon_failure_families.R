# Corrected decomposition: for each clean-case mismatch, is the failure in
# (A) the net->benefit step even using the RECORDED net income, or
# (B) our income/deduction rebuild of net income itself?
d <- readRDS("reg_model_data.rds")
yrs <- d$fiscal_year %in% c(2022, 2023, 2024)
ie  <- !is.na(d$over_threshold) & d$over_threshold != 0
d <- d[yrs, ]; ie <- ie[yrs]
clean <- !ie
gap <- d$raw_benefit_amount - d$rawben
mc  <- abs(gap) > 1 & clean

# CAVEAT (Eric 2026-08-12): the file's RAWNET is CORRECTED net income
# despite its name - there is no true agency-side net in the file. So the
# sharp split available is: does the file itself record a raw-vs-corrected
# benefit difference (absbendiff > 0, sub-threshold on clean cases) that
# our un-correction failed to reverse, or does the file show NO difference
# while our recreation still drifts (a pure formula/component bug)?
gapA <- d$absbendiff                     # |RAWBEN - FSBEN| per the file
a_fail <- !is.na(gapA) & gapA > 1        # file says raw and corrected differ
gapB <- gap                              # our recreated - recorded (the mismatch itself)
b_fail <- abs(gapB) > 1

cat("clean mismatches:", sum(mc), "\n")
cat("file shows a raw-vs-corrected benefit difference (>$1) among them:",
    round(mean(a_fail[mc]), 3), "\n")
cat("(baseline: among clean NON-mismatch cases that share is",
    round(mean(a_fail[clean & !b_fail]), 3), ")\n")

for (s in c("Illinois", "Texas", "Virginia", "Maryland", "Michigan")) {
  m <- mc & d$state == s
  cat(sprintf("\n%s (n=%d): file shows raw-vs-corrected benefit difference: %.2f\n",
              s, sum(m), mean(a_fail[m])))
  cat("  top recreated-minus-recorded gaps:",
      paste(names(head(sort(table(gap[m]), decreasing = TRUE), 5)), collapse = ", "), "\n")
}

# Illinois: the +2/+3 signature
m <- mc & d$state == "Illinois" & gap %in% c(2, 3)
cat("\nIllinois +2/+3 rows (n =", sum(m), "):\n")
cat("  file shows raw-vs-corrected difference:", round(mean(a_fail[m]), 2), "\n")
cat("  share with earned income:", round(mean(d$rawearn[m] > 0), 2),
    "| share with medical ded > 0:", round(mean(d$medical_deductions[m] > 0), 2),
    "| share HH size < 3:", round(mean(as.numeric(as.character(d$cert_HH_size_FS_n[m])) < 3, na.rm = TRUE), 2), "\n")
cat("  their absbendiff values:",
    paste(names(head(sort(table(gapA[m]), decreasing = TRUE), 4)), collapse = ", "), "\n")

# the recurring negative signatures
for (g in c(-9, -15, -39)) {
  m <- mc & gap == g
  cat(sprintf("\ngap %d rows (n=%d): file-difference share %.2f | top states: %s | share w/ earned %.2f\n",
              g, sum(m), mean(a_fail[m]),
              paste(names(head(sort(table(d$state[m]), decreasing = TRUE), 3)), collapse = ", "),
              round(mean(d$rawearn[m] > 0), 2)))
}
