# Munging review 2026-10-30: does the nature 97 / agency 26 partial swap (which rewrites FSBEN and the error
# label) depend on the bound-smoothing draws? Runs the tracked partial-swap block twice on the same rows:
# (A) raw values at the Step 7 stop points (corrected.rds), (B) raw values after smoothing (final.rds).
# Writes nothing.
setwd("C:/Users/ericg/snap_qc_v27")
load("methods/munging_review_2026-10-30/scratch/pre7_env.RData")
src <- readLines("1_data_munging_and_raw_variable_reconstruction_for_using_public_qc_data.R")
s <- grep("^el_m <- slot_matrix\\(mydata, \"ELEMENT\"\\)", src); e <- grep("^mydata\\$no_action_partial_i <- no_action_partial", src) - 1
block <- src[s:e]
qc_elements <- read.csv("additional_data/qc_elements.csv"); qc_natures <- read.csv("additional_data/qc_natures.csv")
fin <- readRDS("final.rds"); cor <- readRDS("corrected.rds")
kf <- paste(fin$STATE, fin$fiscal_year, fin$HHLDNO); kc <- paste(cor$STATE, cor$fiscal_year, cor$HHLDNO)
cor <- cor[match(kf, kc), ]
run <- function(mydata) { eval(parse(text = block)); mydata }
A <- run(cor)
B0 <- cor; for (v in c("rawearn", "rawunearn", "rawrent", "rawutil", "rawmedded", "rawdepded", "rawcsded")) B0[[v]] <- fin[[v]]
B <- run(calculate_raw_benefits(B0))
p <- which(fin$no_action_partial_i)
cat("B reproduces final.rds FSBEN on partial rows:", mean(B$FSBEN[p] == fin$FSBEN[p]), "\n")
cat("partial rows:", length(p), "; FSBEN differs A vs B:", sum(A$FSBEN[p] != B$FSBEN[p]),
    "; over_threshold differs:", sum(A$over_threshold[p] != B$over_threshold[p]), "\n")
cat("of those, rows with an imputed value:", sum((A$FSBEN[p] != B$FSBEN[p]) & !is.na(fin$imputed_at_bound[p])), "\n")
print(table(A = A$over_threshold[p], B = B$over_threshold[p]))
