# Smoke test of the f1027 list builder (40-tree models; methods/f1027_lists/smoke/)
setwd("C:/Users/ericg/snap_qc_v27")
Sys.setenv(SMOKE = "1", V250_OUT_DIR = "methods/f1027_lists")
reg_model_data <- readRDS("reg_model_data.rds")
RESUME_FROM_CHECKPOINT <- TRUE
source("methods/v250_build_staged_lists_utilsua_v2.R")
cat("SMOKE LISTS OK\n")
