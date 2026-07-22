# Runner: rebuild ALL 49 states' blended 2022-24 delivery lists on a FRESH pool
# cache so dollars_per_flag_train is populated for every row (national + own),
# giving one uniform 13-column schema across all states. Same settled recipe as
# the current builder defaults: any-error vocabulary + fdr10 admission.
# NOTE: this is a fresh mine, not a reproduction of the previously-committed
# lists. xgboost (nthread=1) and the filters are deterministic, but ranger is
# not thread-pinned, so re-mining on a different machine yields a different draw
# (the rebuilt lists differ from the prior committed ones by ~10-15% of rows).
# Also, 14 of the previously-committed 19 were still on the pre-v2.3.0
# raw-precision admission and are migrated to fdr10 here.
# Delete methods/delivery_pools_2022_2024_v3 before running (national
# re-mines ~once, then each state mines only its own small pool).
MINING_FRAMES <- "any_error"
reg_model_data <- readRDS("reg_model_data.rds")
STATES <- sort(unique(as.character(
  reg_model_data$state[reg_model_data$fiscal_year %in% c("2022","2023","2024")])))
for (DELIVERY_STATE in STATES) {
  cat(sprintf("\n########## %s ##########\n", DELIVERY_STATE))
  source("INCL_build_blended_delivery_list_v2.R")
}
cat(sprintf("\n=== all-49 delivery rebuild complete (%d states) ===\n", length(STATES)))
