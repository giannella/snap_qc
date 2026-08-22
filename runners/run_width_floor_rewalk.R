# Runner: 49-state with/without narrow-interval-floor re-walk
# (methods/width_floor_rewalk/design_note.md). Evaluation only, no mining.
#   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" runners/run_width_floor_rewalk.R > width_floor_rewalk.log 2>&1
reg_model_data <- readRDS("reg_model_data.rds")
source("methods/width_floor_rewalk_v2.R")
