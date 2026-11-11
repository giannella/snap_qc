setwd("C:/Users/ericg/snap_qc_v27")
r <- readRDS("methods/munging_review_2026-10-30/scratch/b2_results.rds")$R_rw
a <- r[r$method=="F0_zero",]; b <- r[r$method=="F2_typical_first",]
cat("identical mae:", identical(a$mae, b$mae), "\n")
print(table(a$mix)[c("rent|earn","earn|rent","unearn|rent","rent|unearn")])
