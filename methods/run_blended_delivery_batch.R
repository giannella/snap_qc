# Batch runner: blended 2022-24 delivery lists for a set of states.
# The national all-years pool is cached after the first state, so each
# additional state only mines its own pool. Outputs -> state_delivery_lists/.
reg_model_data <- readRDS("reg_model_data.rds")
for (DELIVERY_STATE in c("Washington", "Virginia", "Michigan", "Maryland",
                         "Minnesota", "Arizona", "Colorado", "North Carolina")) {
  cat(sprintf("\n########## %s ##########\n", DELIVERY_STATE))
  source("methods/build_blended_delivery_list_v2.R")
}
cat("\n=== delivery batch complete ===\n")
