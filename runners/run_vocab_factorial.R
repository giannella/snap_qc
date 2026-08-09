# Runner: national vocabulary factorial 2x2
# (methods/design_note_vocab_factorial_2026-08-09.md)
# Usage: "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" runners/run_vocab_factorial.R > vocab_factorial.log 2>&1
# SMOKE=1 in the environment runs the plumbing check instead of the full study.
reg_model_data <- readRDS("reg_model_data.rds")
cat(sprintf("reg_model_data loaded: %d rows, %d cols\n",
            nrow(reg_model_data), ncol(reg_model_data)))
RESUME_FROM_CHECKPOINT <- TRUE   # base/cand resume from the 2026-08-08 cache;
                                 # a killed run resumes at the last finished stage
source("methods/vocab_factorial_v2.R", echo = FALSE)
cat("\n=== vocabulary factorial run complete ===\n")
