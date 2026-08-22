# Runner: utilities-relative vocabulary variant of the v2.5.0 benchmark
# (methods/v250_benchmark_2024_utilrel/design_note.md). Fresh mine, ~4 h;
# checkpoints per unit, so a killed run resumes at the last finished state.
#   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" runners/run_v250_benchmark_utilrel.R > v250_utilrel.log 2>&1
reg_model_data <- readRDS("reg_model_data.rds")
RESUME_FROM_CHECKPOINT <- TRUE
source("methods/v250_benchmark_2024_utilrel_v2.R")
