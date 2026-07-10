# Re-scoring runner: loads reg_model_data, resumes from the mined-rules
# checkpoint (skips the ~40 min of fits), and runs the full evaluation
# including the new any-error scoring pass.
reg_model_data <- readRDS("reg_model_data.rds")
cat(sprintf("reg_model_data loaded: %d rows, %d cols\n",
            nrow(reg_model_data), ncol(reg_model_data)))
RESUME_FROM_CHECKPOINT <- TRUE
source("methods/compare_optimal_vs_plus_RF_mtry1_rules.R", echo = FALSE)
cat("\n=== rescore complete ===\n")
