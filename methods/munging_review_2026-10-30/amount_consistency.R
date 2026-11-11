# Munging review 2026-10-30: what AMOUNT1 means in amount1_only cases, and where the AMOUNT-guided (capped)
# path switches on and off by state-year. Writes nothing.
setwd("C:/Users/ericg/snap_qc_v27")
fin <- readRDS("final.rds"); cor <- readRDS("corrected.rds")
kf <- paste(fin$STATE, fin$fiscal_year, fin$HHLDNO); kc <- paste(cor$STATE, cor$fiscal_year, cor$HHLDNO)
cor <- cor[match(kf, kc), ]          # slots before the 97/26 block cleared them
a1 <- cor$amount_pattern %in% "amount1_only"
cat("amount1_only:", sum(a1), "; |AMOUNT1 - AMTERR| <= 2:", round(100 * mean(abs(cor$AMOUNT1[a1] - cor$AMTERR[a1]) <= 2), 1), "%\n")
oth <- cor$amount_pattern %in% "other"
am <- as.matrix(cor[oth, paste0("AMOUNT", 1:9)]); am[is.na(am)] <- 0
cat("other:", sum(oth), "; AMOUNT1 ~ AMTERR:", round(100 * mean(abs(am[, 1] - cor$AMTERR[oth]) <= 2), 1),
    "%; sum of AMOUNTs > AMTERR + 2:", round(100 * mean(rowSums(am) > cor$AMTERR[oth] + 2), 1),
    "%; some slot zero:", round(100 * mean(rowSums(am == 0 & !is.na(as.matrix(cor[oth, paste0("ELEMENT", 1:9)]))) > 0), 1), "%\n")
cat("capped (AMOUNT-guided) cases by state x FY (states with any):\n")
cp <- cor$cascade_type %in% "capped"
t <- table(cor$state_name[cp], cor$fiscal_year[cp]); print(t[order(-rowSums(t)), ][1:12, ])
mult <- !is.na(cor$amount_pattern)
cat("share of multi-element cases on the capped path, by FY:\n"); print(round(100 * tapply(cp[mult], cor$fiscal_year[mult], mean), 1))
cat("multi-element cases that are errors (over threshold):", sum(mult & cor$over_threshold == 1), "of", sum(cor$over_threshold == 1), "errors\n")
cat("multi-element error cases by pattern:\n"); print(table(cor$amount_pattern[mult & cor$over_threshold == 1]))
cat("multi-element error cases where Step 7 moved 2+ continuous/utility variables:",
    sum(mult & cor$over_threshold == 1 & lengths(regmatches(cor$correctednotes, gregexpr("(earn|unearn|rent|util|med|dep|cs)_(up|down)", cor$correctednotes))) >= 2), "\n")
