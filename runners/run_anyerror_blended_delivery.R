# Runner: any-error blended delivery lists for all 49 states, plus the 2024
# holdout scorecard that prices them.
#
#   BENCH  2022-2023 pool -> fill each state's list on its 2022-23 caseload ->
#          score frozen on 2024.  FINAL  2022-2024 pool -> the delivered list.
#
# Both eras' any-error mines are already cached, so this run does no mining:
# it rebuilds the 2022-23 merged pools (dedup only, once each) and then fills
# and scores. Everything checkpoints -- the scorecard is written per state and
# finished states are skipped on relaunch, so killing it is safe.
#
# States run alphabetically. Pre-set DELIVERY_STATES before source() to pick a
# subset; the national pool is always built over all 49 regardless.
reg_model_data <- readRDS("reg_model_data.rds")
source("methods/build_anyerror_blended_delivery_v2.R")
