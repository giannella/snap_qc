# Munging review 2026-10-30: does the earn_down / unearn_down donor ratio (agency / QC value) depend on the
# QC amount, household size, state or era? impute_at_bound pools donors nationally on the closing move alone.
setwd("C:/Users/ericg/snap_qc_v27")
cor <- readRDS("corrected.rds")
cl <- function(notes) { notes[is.na(notes)] <- ""
  m <- regmatches(notes, gregexpr("(^|[+])(earn|unearn|rent|med|dep|cs)_(up|down)(?=[+]|$)", notes, perl = TRUE))
  vapply(m, function(x) if (length(x)) sub("^[+]", "", x[length(x)]) else NA_character_, character(1)) }
c1 <- cl(cor$correctednotes)
for (sp in list(c("earn_down", "rawearn", "FSEARN"), c("unearn_down", "rawunearn", "FSUNEARN"))) {
  matched <- !grepl("_error", cor$correctednotes) & abs(cor$rawben_recreated - cor$RAWBEN) <= 5
  between <- !(cor$RAWBEN == cor$rawbenmax) & !(cor$rawminimum_ben > 0 & cor$RAWBEN == cor$rawminimum_ben)
  d <- which(matched & between & c1 %in% sp[1] & cor[[sp[3]]] > 0)
  ratio <- cor[[sp[2]]][d] / cor[[sp[3]]][d]; q <- cor[[sp[3]]][d]
  qq <- cut(q, quantile(q, 0:5/5), include.lowest = TRUE, dig.lab = 5)
  cat("\n==", sp[1], "donors:", length(d), "==\n")
  print(round(cbind(n = tapply(ratio, qq, length), median = tapply(ratio, qq, median), le10pct = tapply(ratio <= 0.1, qq, mean)), 3))
  sz <- pmin(cor$rawusize[d], 5)
  print(round(cbind(n = tapply(ratio, sz, length), median = tapply(ratio, sz, median), le10pct = tapply(ratio <= 0.1, sz, mean)), 3))
  era <- ifelse(cor$fiscal_year[d] <= 2019, "FY17-19", "FY22-24")
  print(round(cbind(n = tapply(ratio, era, length), median = tapply(ratio, era, median), le10pct = tapply(ratio <= 0.1, era, mean)), 3))
  st <- tapply(ratio <= 0.1, cor$state_name[d], mean); stn <- table(cor$state_name[d])
  cat("state share ratio<=0.1 (states with >=100 donors): quantiles\n"); print(round(quantile(st[stn >= 100], c(0, .1, .5, .9, 1)), 3))
  cat("KW test ratio ~ QC-amount quintile p =", signif(kruskal.test(ratio, qq)$p.value, 3),
      "; ~ state p =", signif(kruskal.test(ratio, factor(cor$state_name[d]))$p.value, 3), "\n")
}
