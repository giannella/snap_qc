# Non-interactive runner for EXCL_find_exclusion_rules_by_hh_size_v2.R
reg_model_data <- readRDS("reg_model_data.rds")
cat(sprintf("reg_model_data loaded: %d rows, %d cols\n",
            nrow(reg_model_data), ncol(reg_model_data)))
source("EXCL_find_exclusion_rules_by_hh_size_v2.R", echo = FALSE)
cat("\n=== EXCL v2 run complete ===\n")
