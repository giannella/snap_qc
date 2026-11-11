setwd("C:/Users/ericg/snap_qc_v27")
src <- readLines("methods/munging_review_2026-10-30/b2_mask_amounts.R")
invisible(capture.output(eval(parse(text = src[5:34]))))
ev <- ev_rw; sub <- mydata[ev, ]
E <- el[ev, , drop = FALSE]; P <- pres[ev, , drop = FALSE]; k <- rowSums(P)
cm <- c(cs=.412, dep=.455, earn=.562, med=.563, rent=.375, unearn=.404, util=.606)
z <- sub; for (j in 1:9) z[[paste0("AMOUNT", j)]] <- ifelse(P[, j], 0, NA); z$amount_pattern <- amount_pattern(z)
d2 <- z
for (i in seq_len(nrow(sub))) for (j in 1:k[i]) d2[[paste0("AMOUNT", j)]][i] <- round(1000 * cm[cls(E[i, j])])
d2$amount_pattern <- "other"
print(head(d2[, paste0("AMOUNT", 1:3)])); print(head(cls(E[1, P[1, ]])))
cat("class of AMOUNT1:", class(d2$AMOUNT1), "\n")
r1 <- recreate_elements(z); r2 <- recreate_elements(d2)
vars <- unique(unname(step7_vars))
same <- rowSums(sapply(vars, function(v) abs(r1[[v]] - r2[[v]]) > 0.5)) == 0
cat("F2 (full-sample means) same as F0 in", sum(same), "of", length(same), "\n")
