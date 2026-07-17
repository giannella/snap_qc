# Runner: the pre-registered era-validation sweep (train 2017-18, test 2019).
reg_model_data <- readRDS("reg_model_data.rds")
source("methods/era_validation_sweep_v2.R")
