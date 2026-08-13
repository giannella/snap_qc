# Runner: the full v2.5.0 candidate cycle on the rebuilt frame (2026-08-12).
# Chain: (1) one-year-ahead benchmark (fresh FY2022-23 mines, walk FY2024,
# v2.4.0 paired deltas) -> methods/v250_benchmark_2024/; then (2) the staged
# production build + characterization via runners/run_v250_build.R ->
# methods/v250_candidate_lists/. Both carry the artifact check (displacement
# HALTS the chain) and the Illinois state-pool hold. Mines checkpoint per
# unit; a killed run resumes on relaunch.
# Usage:
#   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" runners/run_v250_cycle.R > v250_cycle.log 2>&1
setwd("C:/Users/ericg/snap_qc")
reg_model_data <- readRDS("reg_model_data.rds")
RESUME_FROM_CHECKPOINT <- TRUE
cat(sprintf("[%s] === CYCLE STEP 1: benchmark ===\n", format(Sys.time(), "%H:%M:%S")))
source("methods/v250_benchmark_2024_v2.R")
rm(list = setdiff(ls(), character(0))); invisible(gc())
cat(sprintf("[%s] === CYCLE STEP 2: staged production build ===\n",
            format(Sys.time(), "%H:%M:%S")))
source("runners/run_v250_build.R")
cat("V250 CYCLE COMPLETE\n")
