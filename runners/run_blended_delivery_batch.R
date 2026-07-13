# Batch runner: blended 2022-24 delivery lists for the 18 benchmark states
# (+ Minnesota). The national pool is mined once and cached, so each
# additional state only mines its own pool. Outputs -> state_delivery_lists/.
# MINING_FRAMES can be pre-set before source() (default: all five frames).
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
cat("\n=== delivery batch complete ===\n")
