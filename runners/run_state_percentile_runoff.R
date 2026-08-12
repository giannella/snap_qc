# Runner: EXPLORATORY state-scale percentile runoff (benp vs persize).
# Driver: methods/state_percentile_runoff_v2.R (the new S37).
# Usage:
#   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" runners/run_state_percentile_runoff.R > pctl_runoff.log 2>&1
setwd("C:/Users/ericg/snap_qc")
reg_model_data <- readRDS("reg_model_data.rds")
RESUME_FROM_CHECKPOINT <- TRUE
source("methods/state_percentile_runoff_v2.R")
