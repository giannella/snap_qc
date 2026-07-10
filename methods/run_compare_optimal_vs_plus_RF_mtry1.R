# Non-interactive runner for compare_optimal_vs_plus_RF_mtry1_rules.R:
# loads reg_model_data from the .rds snapshot, then sources the script.
reg_model_data <- readRDS("reg_model_data.rds")
cat(sprintf("reg_model_data loaded: %d rows, %d cols\n",
            nrow(reg_model_data), ncol(reg_model_data)))
source("methods/compare_optimal_vs_plus_RF_mtry1_rules.R", echo = FALSE)
cat("\n=== run complete ===\n")
