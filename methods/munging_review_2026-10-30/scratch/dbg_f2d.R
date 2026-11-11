setwd("C:/Users/ericg/snap_qc_v27")
b2src <- readLines("methods/munging_review_2026-10-30/b2_mask_amounts.R")
invisible(capture.output(eval(parse(text = b2src[5:34]))))
b2src <- readLines("methods/munging_review_2026-10-30/b2_mask_amounts.R")
ev <- ev_rw; folds_seed <- 20261030
eval(parse(text = b2src[37:79]))
print(head(d2[, paste0("AMOUNT", 1:3)])); print(cl_mean); print(head(cl_vals))
