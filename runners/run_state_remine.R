# Runner: state re-mine (representation runoff -> production state pools)
# (methods/design_note_state_remine_2026-08-09.md)
# Usage: "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" runners/run_state_remine.R > state_remine.log 2>&1
# SMOKE=1 in the environment runs the plumbing check instead of the full study.
# Launch AFTER the national factorial has exited (shared machine).
reg_model_data <- readRDS("reg_model_data.rds")
cat(sprintf("reg_model_data loaded: %d rows, %d cols\n",
            nrow(reg_model_data), ncol(reg_model_data)))
RESUME_FROM_CHECKPOINT <- TRUE   # a killed run resumes at the last finished state
source("methods/state_remine_v2.R", echo = FALSE)
cat("\n=== state remine run complete ===\n")
