# Year-swap replication of the HH-strata comparison (deck comment 2026-07-12:
# "didn't we redo this for 2024?" -- we hadn't; the strata study was
# 2023-judged). Train 2022+2023, test 2024, same three schemes.
reg_model_data <- readRDS("reg_model_data.rds")
TRAIN_YEARS   <- c("2022", "2023")
HOLDOUT_YEARS <- c("2024")
OUT_DIR_OVERRIDE <- "methods/compare_hh_strata_v2/yearswap_train2223_test24"
source("methods/compare_hh_strata_v2.R")
cat("\n=== strata yearswap complete ===\n")
