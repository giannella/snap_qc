# Held-out test of the rule lists on frame f1109, by state (2026-11-11; Eric: "we need the
# performance on the most recent rebuild of the data. it has to be 1109 or later"). Lists mined on
# FY2022-23 (seed 117) and scored on FY2024, at 5% and 10% budgets, in two stages:
#   BENCH_STAGE=bench  methods/v250_benchmark_2024_utilrel_v2.R, the benchmark recipe (fresh
#                      national + 49 state any-error pools, joint BH + n >= 30, 99% LCB, artifact
#                      gate, fresh-share walk, 3x buffer), run on f1109. No pile gate. About 4 h,
#                      13 GB; checkpoints per pool and resumes on relaunch.
#   BENCH_STAGE=gate   methods/reconstruction_income_piles/gate_removal_test.R, window w2224, on the
#                      stage-1 pools: blended and national-only lists, pile gate off and on, scored
#                      on all FY2024 cases and on the non-pile cases. Blended with the gate on is the
#                      shipped recipe (runners/run_f1109_build.R). Its ungated blended arm on all
#                      cases must reproduce the stage-1 CSV cell for cell.
# Both scripts are read from disk and only their v2.7-frame constants are swapped for f1109's (each
# swap must match exactly once); the originals are unchanged. f1109: 237,557 rows; FY2022-23 77,750
# rows and 8,477 errors; FY2024 40,497 rows and 4,820 errors.
# Outputs: methods/f1109_list_bench/bench/ (stage 1, cache/ inside; smoke/ with SMOKE=1) and
# methods/f1109_list_bench/gate_scores_w2224[_smoke].csv (stage 2).
#   BENCH_STAGE=bench [SMOKE=1] "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" runners/run_f1109_list_bench.R
#   BENCH_STAGE=gate [SMOKE=1] [SEL_STATES=Alabama,Ohio] "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" runners/run_f1109_list_bench.R
ROOT <- "C:/Users/ericg/snap_qc_v27"; setwd(ROOT)
STAGE <- Sys.getenv("BENCH_STAGE"); stopifnot(STAGE %in% c("bench", "gate"))
SMOKE_RUN <- identical(Sys.getenv("SMOKE"), "1")
OUT <- "methods/f1109_list_bench"; BENCH_DIR <- file.path(OUT, "bench")
BENCH_RUN_DIR <- if (SMOKE_RUN) file.path(BENCH_DIR, "smoke") else BENCH_DIR   # where stage 1 writes
dir.create(BENCH_DIR, recursive = TRUE, showWarnings = FALSE)
# the working frame must be the tracked staging frame, f1109 (sha256 08e0e825...)
stopifnot(unname(tools::md5sum("reg_model_data.rds")) == unname(tools::md5sum("additional_data/reg_model_data.rds")),
          startsWith(digest::digest(file = "reg_model_data.rds", algo = "sha256"), "08e0e825"))

swap <- function(src, pairs) {
  for (p in pairs) {
    n <- lengths(regmatches(src, gregexpr(p[1], src, fixed = TRUE)))
    if (n != 1L) stop(sprintf("expected exactly one match of [%s], found %d", p[1], n))
    src <- sub(p[1], p[2], src, fixed = TRUE)
  }
  src
}
run <- function(file, pairs) {
  src <- swap(paste(readLines(file, warn = FALSE), collapse = "\n"), pairs)
  cat(sprintf("[%s] %s: running with %d f1109 substitutions\n", format(Sys.time(), "%H:%M:%S"), file, length(pairs)))
  eval(parse(text = src), envir = globalenv())
}
F1109_ROWS <- list(c("stopifnot(nrow(reg_model_data) == 231619L)", "stopifnot(nrow(reg_model_data) == 237557L)"))

if (STAGE == "bench") {
  reg_model_data <- readRDS("reg_model_data.rds")
  RESUME_FROM_CHECKPOINT <- TRUE
  Sys.setenv(BENCH_OUT_DIR = BENCH_DIR)          # the script appends smoke/ itself under SMOKE=1
  run("methods/v250_benchmark_2024_utilrel_v2.R", c(F1109_ROWS, list(
    c("EXPECT_TRAIN_ROWS <- 76031L; EXPECT_TRAIN_ERRS <- 8397L", "EXPECT_TRAIN_ROWS <- 77750L; EXPECT_TRAIN_ERRS <- 8477L"),
    c("EXPECT_TEST_ROWS  <- 39528L; EXPECT_TEST_ERRS  <- 4764L", "EXPECT_TEST_ROWS  <- 40497L; EXPECT_TEST_ERRS  <- 4820L"))))
}

if (STAGE == "gate") {
  Sys.setenv(SEL_WINDOW = "w2224", SEL_TAG = if (SMOKE_RUN) "smoke" else "")
  run("methods/reconstruction_income_piles/gate_removal_test.R", c(F1109_ROWS, list(
    c('setwd("C:/Users/ericg/snap_qc")', sprintf('setwd("%s")', ROOT)),
    c('cache = "methods/v270_cpi_benchmark/fy2024_cpi/cache"', sprintf('cache = "%s/cache"', BENCH_RUN_DIR)),
    c('bench = "methods/v270_cpi_benchmark/fy2024_cpi/v250_benchmark_2024.csv"',
      sprintf('bench = "%s/v250_benchmark_2024.csv"', BENCH_RUN_DIR)),
    c("expect_train = c(76031L, 8397L)", "expect_train = c(77750L, 8477L)"),
    c('OUT_DIR <- "methods/reconstruction_income_piles"', sprintf('OUT_DIR <- "%s"', OUT)))))
}
cat(sprintf("[%s] stage %s%s DONE\n", format(Sys.time(), "%H:%M:%S"), STAGE, if (SMOKE_RUN) " (smoke)" else ""))
