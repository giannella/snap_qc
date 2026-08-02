# Runner: typed five-frame blended delivery lists for all 49 states, plus the
# 2024 holdout scorecard that prices them.
#
#   BENCH  mine 2022-2023 -> fill each state's list on its 2022-23 caseload ->
#          score frozen on 2024.  FINAL  mine 2022-2024 -> the delivered list.
#
# Long run (roughly 18-20 hours), split across nights. Everything checkpoints:
# pools are cached per (pool x frame x era) and the scorecard is appended per
# state, so relaunching this same command resumes where it left off -- finished
# states are skipped, an interrupted state re-mines only the frames that had not
# yet been written. Killing it at any point is safe.
#
# States run alphabetically. Pre-set DELIVERY_STATES before source() to pick a
# different subset; the national pool is always mined over all 49 regardless.
reg_model_data <- readRDS("reg_model_data.rds")
source("methods/build_typed_blended_delivery_v2.R")
