# Non-interactive runner for tune_followup_subsample_lcbz_v2.R
reg_model_data <- readRDS("reg_model_data.rds")
cat(sprintf("reg_model_data loaded: %d rows, %d cols\n",
            nrow(reg_model_data), ncol(reg_model_data)))
source("tune_followup_subsample_lcbz_v2.R", echo = FALSE)
cat("\n=== tuning follow-up complete ===\n")
