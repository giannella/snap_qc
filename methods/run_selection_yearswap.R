# Year-swap replication of the MODEL-SELECTION studies: train 2022+2023,
# test 2024. The original runs (train 2022+2024, test 2023) chose the
# production engines and filter stringency; this replication asks whether
# those verdicts hold on a year that never influenced any design decision.
# Patches each study's year config and output folder; everything else
# identical (same seeds, grids, screens).
reg_model_data <- readRDS("reg_model_data.rds")
cat(sprintf("reg_model_data loaded: %d rows, %d cols\n",
            nrow(reg_model_data), ncol(reg_model_data)))

yearswap <- function(script, out_dir_line, out_dir_new) {
  src <- readLines(script)
  src <- sub('^TRAIN_YEARS   <- c\\("2022", "2024"\\)$',
             'TRAIN_YEARS   <- c("2022", "2023")', src)
  src <- sub('^HOLDOUT_YEARS <- c\\("2023"\\)$',
             'HOLDOUT_YEARS <- c("2024")', src)
  src <- sub(out_dir_line, out_dir_new, src)
  stopifnot(sum(grepl('2022", "2023', src, fixed = TRUE)) == 1,
            sum(grepl("yearswap_train2223_test24", src)) == 1)
  cat(sprintf("\n════════ year-swap: %s ════════\n", script))
  eval(parse(text = src), envir = globalenv())
}

yearswap("compare_engine_combos_v2.R",
         '^out_dir <- "methods/compare_engines_v2"$',
         'out_dir <- "methods/compare_engines_v2/yearswap_train2223_test24"')

yearswap("tune_followup_subsample_lcbz_v2.R",
         '^out_dir <- "methods/parameter_tuning_v2"$',
         'out_dir <- "methods/parameter_tuning_v2/yearswap_train2223_test24"')

cat("\n=== selection year-swap replications complete ===\n")
