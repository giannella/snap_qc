# Runner: layers 1+3 audition (EB ranking x FDR admission), composed with
# layer 2's collapse at the neardup sweep's best setting for all5 (J=0.95).
JACCARD_SET <- c(1, 0.95)
reg_model_data <- readRDS("reg_model_data.rds")
source("methods/estimation_admission_sweep_v2.R")
