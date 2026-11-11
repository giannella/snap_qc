setwd("C:/Users/ericg/snap_qc_v27")
src <- readLines("methods/munging_review_2026-10-30/b2_mask_amounts.R")
eval(parse(text = src[5:34]))
ev <- ev_rw; sub <- mydata[ev, ]
E <- el[ev, , drop = FALSE]; A <- am[ev, , drop = FALSE]; P <- pres[ev, , drop = FALSE]
share <- A / rowSums(A)
cl_vals <- unlist(lapply(seq_len(nrow(E)), function(i) setNames(share[i, P[i, ]], cls(E[i, P[i, ]]))))
print(round(tapply(cl_vals, names(cl_vals), mean), 3)); print(table(names(cl_vals)))
# reverse-order test: put AMOUNT descending opposite to slot order, pattern other
d <- sub; k <- rowSums(P)
for (j in 1:9) { a <- paste0("AMOUNT", j); d[[a]] <- ifelse(P[, j], 100 * j, NA) }
d$amount_pattern <- "other"
z <- sub; for (j in 1:9) z[[paste0("AMOUNT", j)]] <- ifelse(P[, j], 0, NA); z$amount_pattern <- amount_pattern(z)
r1 <- recreate_elements(z); r2 <- recreate_elements(d)
vars <- unique(unname(step7_vars))
same <- rowSums(sapply(vars, function(v) abs(r1[[v]] - r2[[v]]) > 0.5)) == 0
cat("reverse order gives same values in", sum(same), "of", length(same), "\n")
cat("pattern of zero arm:\n"); print(table(z$amount_pattern))
cat("ctype zero arm:\n"); print(table(r1$cascade_type)); print(table(r2$cascade_type))
