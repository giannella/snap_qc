# Munging review 2026-10-30 (EXPLORATORY): real clean_split cases, AMOUNTs masked.
# On real multi-element cases whose AMOUNTs add up to the error (clean_split, capped path), rerun the tracked
# recreate_elements() with AMOUNT1-9 zeroed (as all_zero states report them) and with AMTERR copied into every
# slot, and compare the raw values with the AMOUNT-guided result. Also: how much of the error slot 1 carries in
# real clean_split cases. Needs scratch/pre7_env.RData (from synthetic_multielement.R). Writes only to scratch/.
setwd("C:/Users/ericg/snap_qc_v27")
out <- "methods/munging_review_2026-10-30/scratch/"
load(paste0(out, "pre7_env.RData"))
mydata <- calculate_raw_benefits(mydata)
mydata$amount_pattern <- amount_pattern(mydata)
el <- slot_matrix(mydata, "ELEMENT"); am <- slot_matrix(mydata, "AMOUNT"); am[is.na(am)] <- 0
cs <- which(mydata$amount_pattern %in% "clean_split")
nel <- rowSums(!is.na(el[cs, , drop = FALSE]))
share1 <- am[cs, 1] / rowSums(am[cs, , drop = FALSE])
cat("real clean_split cases:", length(cs), "; elements:", paste(names(table(nel)), table(nel), sep = ":", collapse = " "), "\n")
cat("slot-1 share of the summed AMOUNTs: quantiles\n"); print(round(quantile(share1, c(.1, .25, .5, .75, .9)), 2))
cat("slot 1 carries the largest AMOUNT in", round(100 * mean(am[cs, 1] == apply(am[cs, , drop = FALSE], 1, max)), 1), "% ;",
    "slot 1 >= 90% of the total in", round(100 * mean(share1 >= 0.9), 1), "%\n")
# all multi-element cases with informative-looking AMOUNTs (clean_split or other): same share
oth <- which(mydata$amount_pattern %in% "other")
s_o <- am[oth, 1] / pmax(rowSums(am[oth, , drop = FALSE]), 1)
cat("'other' pattern: slot-1 share quantiles\n"); print(round(quantile(s_o, c(.1, .25, .5, .75, .9)), 2))

sub <- mydata[cs, ]
base <- recreate_elements(sub)
vars <- c("rawearn", "rawunearn", "rawrent", "rawutil", "rawmedded", "rawdepded", "rawcsded")
cmp <- function(alt, label) {
  moved_b <- sapply(vars, function(v) base[[v]] != sub[[v]]); moved_a <- sapply(vars, function(v) alt[[v]] != sub[[v]])
  dif <- sapply(vars, function(v) abs(alt[[v]] - base[[v]]))
  tol <- sapply(vars, function(v) pmax(10, 0.02 * abs(base[[v]])))
  same <- rowSums(dif > tol) == 0
  cat(sprintf("%-14s n=%d  capped(base) %d | reached: base %.1f%% alt %.1f%% | all values agree with AMOUNT-guided: %.1f%% | same set of variables moved: %.1f%% | mean |diff| summed over vars $%.0f\n",
              label, nrow(sub), sum(base$cascade_type %in% "capped"),
              100 * mean(base$rawben_recreated == base$RAWBEN), 100 * mean(alt$rawben_recreated == alt$RAWBEN),
              100 * mean(same), 100 * mean(rowSums(moved_b != moved_a) == 0), mean(rowSums(dif))))
  invisible(same)
}
z <- sub; for (k in 1:9) z[[paste0("AMOUNT", k)]] <- ifelse(is.na(z[[paste0("ELEMENT", k)]]), NA, 0)
z$amount_pattern <- amount_pattern(z); alt0 <- recreate_elements(z); s0 <- cmp(alt0, "masked_zero")
cpy <- sub; for (k in 1:9) cpy[[paste0("AMOUNT", k)]] <- ifelse(is.na(cpy[[paste0("ELEMENT", k)]]), NA, cpy$AMTERR)
cpy$amount_pattern <- amount_pattern(cpy); altc <- recreate_elements(cpy); sc <- cmp(altc, "masked_copied")
cat("agreement by slot-1 share (masked_zero): share >= 0.9:", round(100 * mean(s0[share1 >= 0.9]), 1), "%; share < 0.9:",
    round(100 * mean(s0[share1 < 0.9]), 1), "%\n")
