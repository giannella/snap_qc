# Rebuild all delivery lists on the settled recipe: any-error vocabulary
# (the three 2026-07 audits left it the best configuration at the 5% budget),
# with the full provenance schema (pool / engines / mined_frames /
# n_flagged_state / n_new_at_rank).
MINING_FRAMES <- "any_error"
reg_model_data <- readRDS("reg_model_data.rds")
for (DELIVERY_STATE in c("Arizona", "California", "Colorado", "Connecticut",
                         "District of Columbia", "Louisiana", "Maine",
                         "Maryland", "Massachusetts", "Michigan", "Minnesota",
                         "Mississippi", "Missouri", "New Jersey",
                         "North Carolina", "Tennessee", "Texas", "Virginia",
                         "Washington")) {
  cat(sprintf("\n########## %s ##########\n", DELIVERY_STATE))
  source("INCL_build_blended_delivery_list_v2.R")
}
cat("\n=== any-error delivery rebuild complete ===\n")
