# Year-swap replication of the typed-vs-pooled head-to-head: train 2022+2023,
# test 2024 (the main run trains 2022+2024, tests 2023). Patches the comparison
# script's year config and output folder; everything else identical.
reg_model_data <- readRDS("reg_model_data.rds")
cat(sprintf("reg_model_data loaded: %d rows, %d cols\n",
            nrow(reg_model_data), ncol(reg_model_data)))
src <- readLines("methods/compare_anyerror_vs_typed_frames_v2.R")
src <- sub('^TRAIN_YEARS   <- c\\("2022", "2024"\\)$', 'TRAIN_YEARS   <- c("2022", "2023")', src)
src <- sub('^HOLDOUT_YEARS <- c\\("2023"\\)$',          'HOLDOUT_YEARS <- c("2024")', src)
src <- sub('^out_dir <- "methods/compare_anyerror_vs_typed_v2"$',
           'out_dir <- "methods/compare_anyerror_vs_typed_v2/yearswap_train2223_test24"', src)
stopifnot(sum(grepl("2022\", \"2023", src)) == 1,
          sum(grepl("yearswap_train2223_test24", src)) == 1)
eval(parse(text = src))
cat("\n=== year-swap head-to-head complete ===\n")
