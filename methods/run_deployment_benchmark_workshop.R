# Workshop extension of the time-shifted deployment benchmark: the six states
# from the workshop list not already in the main 12 (Mississippi, California,
# and New Jersey already are). Only the approaches the workshop charts use --
# the shared national_all pool comes from cache, so this just mines six small
# own-state pools and scores.
reg_model_data <- readRDS("reg_model_data.rds")
TARGETS <- c("Maine", "Maryland", "Missouri", "Massachusetts",
             "District of Columbia", "Tennessee")
APPROACHES <- c("own_state", "national_all")
OUT_CSV_NAME <- "deployment_menu_workshop_extension.csv"
source("methods/deployment_benchmark_train2223_test24.R")
cat("\n=== workshop extension complete ===\n")
