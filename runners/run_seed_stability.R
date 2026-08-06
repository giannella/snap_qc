# Runner: seed-stability study (methods/preregistration_seed_stability_2026-08-05.md)
# Usage: "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" runners/run_seed_stability.R > seed_stability.log 2>&1
# SMOKE=1 in the environment runs the plumbing check instead of the full study.
reg_model_data <- readRDS("reg_model_data.rds")
cat(sprintf("reg_model_data loaded: %d rows, %d cols\n",
            nrow(reg_model_data), ncol(reg_model_data)))
RESUME_FROM_CHECKPOINT <- TRUE   # a killed run resumes at the last finished stage
source("methods/seed_stability_v2.R", echo = FALSE)
cat("\n=== seed stability run complete ===\n")
