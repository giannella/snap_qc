# Runner: rebuild all delivery lists under the v2.3.0 admission test
# (fdr10, the builder default; any-error vocabulary; existing filenames).
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
cat("\n=== v2.3.0 delivery rebuild complete ===\n")
