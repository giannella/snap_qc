# Runner: vocabulary attribution study
# (methods/design_note_vocab_attribution_2026-08-08.md)
# Usage: "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" runners/run_vocab_attribution.R > vocab_attribution.log 2>&1
# SMOKE=1 in the environment runs the plumbing check instead of the full study.
reg_model_data <- readRDS("reg_model_data.rds")
cat(sprintf("reg_model_data loaded: %d rows, %d cols\n",
            nrow(reg_model_data), ncol(reg_model_data)))
RESUME_FROM_CHECKPOINT <- TRUE   # a killed run resumes at the last finished stage
source("methods/vocab_attribution_v2.R", echo = FALSE)
cat("\n=== vocabulary attribution run complete ===\n")
