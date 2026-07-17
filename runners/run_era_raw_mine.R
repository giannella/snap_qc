# Runner: raw any-error vocabulary mine on the 2017-18 era (pre-registration:
# methods/preregistration_era_validation_2026-07.md, compute plan step 1).
TRAIN_YEARS <- c("2017", "2018")
RAWDIR <- "methods/state_similarity_v2/era_validation_train1718_test19/raw_vocab"
dir.create(RAWDIR, recursive = TRUE, showWarnings = FALSE)
reg_model_data <- readRDS("reg_model_data.rds")
source("methods/fdr_raw_vocabulary_mine_v2.R")
