# Non-interactive runner for compare_anyerror_vs_typed_frames_v2.R
reg_model_data <- readRDS("reg_model_data.rds")
cat(sprintf("reg_model_data loaded: %d rows, %d cols\n",
            nrow(reg_model_data), ncol(reg_model_data)))
source("compare_anyerror_vs_typed_frames_v2.R", echo = FALSE)
cat("\n=== head-to-head run complete ===\n")
