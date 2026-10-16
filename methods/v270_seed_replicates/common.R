# Shared setup for the v2.7 seed-replicate study (2026-10-02). See design_note.md.
#
# The recipe is not restated here. Every constant and function below is taken BY
# NAME from the shipped one-year-ahead benchmark
# (methods/v250_benchmark_2024_utilrel_v2.R): the engine settings, vocabulary,
# admission (one BH pass at FDR 10% and n >= 30, 99% Wilson LCB ordering), the
# fresh-share fill walk and the test-year scoring. Only the frame file and the
# seed vary across arms.
setwd("C:/Users/ericg/snap_qc")
suppressMessages(library(dplyr))
source("rule_mining_helpers.R")

BENCH <- "methods/v250_benchmark_2024_utilrel_v2.R"
WANT <- c("LCB_Z", "FDR_ALPHA", "MIN_N", "TRAIN_YEARS", "TEST_YEAR", "XGB", "RF",
          "SIGNIF_DIGITS", "BUDGETS", "BUFFER_MULT", "FRESH_MIN",
          "EXPECT_TRAIN_ROWS", "EXPECT_TRAIN_ERRS", "EXPECT_TEST_ROWS",
          "EXPECT_TEST_ERRS", "BASE_FEATURES", "VOCAB19", "BINARY_FEATURES",
          "MM_TAG_SHARE", "HH_LEVELS", "hh_group_of", "stamp", "admit_rank",
          "walk_fill", "score_2024")
for (e in parse(BENCH))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
      as.character(e[[2]]) %in% WANT) eval(e, globalenv())
stopifnot(all(vapply(WANT, exists, logical(1))))
stopifnot(identical(TRAIN_YEARS, c("2022", "2023")), identical(TEST_YEAR, "2024"),
          XGB$nrounds == 1000, RF$num_trees == 1000, LCB_Z == 2.326)

# the evaluation window (2026-10-04): fy2024 (mine FY2022-23, score FY2024;
# the default, nights 1-2) or fy2022 (mine FY2017-19, score FY2022, three
# years ahead across the excluded FY2020-21; night 3). The FY2017-19 benchmark
# script (v250_benchmark_era2_utilsua_variant_v2.R) has the same vocabulary,
# national mining block, admit_rank, tag_and_gate, walk_fill and score_2024 as
# the one read above; only the years and expected counts differ, set here as
# that script's runner set them (runners/run_v270_cpi_bench.R).
WINDOW <- Sys.getenv("SR_WINDOW", "fy2024")
stopifnot(WINDOW %in% c("fy2024", "fy2022"))
if (WINDOW == "fy2022") {
  TRAIN_YEARS <- c("2017", "2018", "2019"); TEST_YEAR <- "2022"
  EXPECT_TRAIN_ROWS <- 116060L; EXPECT_TRAIN_ERRS <- 10920L
  EXPECT_TEST_ROWS <- 36851L; EXPECT_TEST_ERRS <- 3985L
}
SMOKE <- identical(Sys.getenv("SMOKE"), "1")
OUT_DIR <- if (WINDOW == "fy2022") "methods/v270_seed_replicates/fy2022" else "methods/v270_seed_replicates"
if (SMOKE) { XGB$nrounds <- 40; RF$num_trees <- 40; OUT_DIR <- file.path(OUT_DIR, "smoke") }
POOL_DIR <- file.path(OUT_DIR, "pools")
dir.create(POOL_DIR, showWarnings = FALSE, recursive = TRUE)

# the frames under comparison (same 231,619 cases in the same order). f0925
# and final are the two compared on night 1; f0928 and f0929 are the steps in
# between (night 2). Step 1 carries TWO changes: the fiscal-year CPI keying and
# the SUA tier computed on review-year amounts (439 FY2022-24 rows tier 2 -> 1).
FRAMES <- c(
  nominal = "archive_data/reg_model_data_pre_cpi_rebuild_2026-09-24.rds",  # built 2026-09-21 on main: no CPI step, older code (night 2)
  cpioff = "archive_data/reg_model_data_cpioff_2026-10-03.rds",          # built 2026-10-03: the final frame's code with cpi_inflate_vars = FALSE (the clean CPI-off frame, night 2)
  f0925 = "archive_data/reg_model_data_pre_fycpi_2026-09-28.rds",   # built 2026-09-25: annual-average CPI keyed on the review month's calendar year
  f0928 = "archive_data/reg_model_data_pre_octcpi_2026-09-29.rds",  # built 2026-09-28: CPI keyed on FISCAL year + SUA tier on review-year amounts (step 1)
  f0929 = "archive_data/reg_model_data_pre_suatol_2026-09-29.rds",  # built 2026-09-29: October CPI keyed on fiscal year, no SUA tolerance (step 2)
  final = "reg_model_data.rds")                                      # built 2026-09-29: October CPI keyed on fiscal year, $10 SUA-tier tolerance (step 3)
# each frame's seed-117 national pool as the v2.7 benchmark arms cached it
BENCH_POOL <- c(
  nominal = "methods/v270_cpi_benchmark/fy2024_nominal/cache/bench_national_117.rds",
  f0925 ="methods/v270_cpi_benchmark/fy2024_cpi_run2_20260925/cache/bench_national_117.rds",
  final = "methods/v270_cpi_benchmark/fy2024_cpi/cache/bench_national_117.rds")
if (WINDOW == "fy2022") BENCH_POOL <- c(
  final = "methods/v270_cpi_benchmark/fy2022_cpi/cache/bench_national_117.rds",
  f0928 = "methods/v270_cpi_benchmark/fy2022_cpi_annualcpi_20260928/cache/bench_national_117.rds")
BENCH_CSV <- c(
  f0925 = "methods/v270_cpi_benchmark/fy2024_cpi_run2_20260925/v250_benchmark_2024.csv",
  final = "methods/v270_cpi_benchmark/fy2024_cpi/v250_benchmark_2024.csv")
SEEDS <- c(117L, 118L, 119L)

pool_file <- function(frame, seed, kind = c("admitted", "raw"))
  file.path(POOL_DIR, sprintf("%s_%s_seed%d.rds", match.arg(kind), frame, seed))

# the benchmark's frame block (its lines 112-163), verbatim in substance
load_frame <- function(id) {
  stopifnot(id %in% names(FRAMES))
  reg <- readRDS(FRAMES[[id]])
  stopifnot(nrow(reg) == 231619L)
  adf0 <- reg %>% filter(fiscal_year %in% c(TRAIN_YEARS, TEST_YEAR))
  stopifnot("utilities_sua" %in% names(adf0))
  pf <- prep_features(adf0, VOCAB19); adf <- pf$data
  stopifnot(length(setdiff(VOCAB19, pf$features)) == 0)
  st <- as.character(adf$state); yr <- as.character(adf$fiscal_year)
  ie_all <- !is.na(adf$over_threshold) & adf$over_threshold != 0
  ed_all <- ifelse(ie_all, abs(ifelse(is.na(adf$total_error_amount), 0,
                                      adf$total_error_amount)), 0)
  stopifnot(sum(yr %in% TRAIN_YEARS) == EXPECT_TRAIN_ROWS,
            sum(ie_all[yr %in% TRAIN_YEARS]) == EXPECT_TRAIN_ERRS,
            sum(yr == TEST_YEAR) == EXPECT_TEST_ROWS,
            sum(ie_all[yr == TEST_YEAR]) == EXPECT_TEST_ERRS)
  hh_all <- hh_group_of(adf$cert_HH_size_FS_n)
  is_tr <- yr %in% TRAIN_YEARS
  mm_all <- adf$rawben >= adf$benmax & adf$rawben_uncapped < adf$benmax
  stopifnot(sum(mm_all) < 1000)
  list(adf = adf, st = st, yr = yr, ie = ie_all, ed = ed_all, hh = hh_all,
       is_tr = is_tr, mm = mm_all)
}
strata_of <- function(hh) lapply(setNames(nm = HH_LEVELS), function(h) which(hh %in% h))

# artifact tag exactly as the benchmark's tag_and_gate() computes it; the
# benchmark drops tagged rules from the visible pool before the walk. The
# halting gates are left to the benchmark: this study reports the tagged count.
tag_artifacts <- function(adm) {
  adm$mm_share_flags  <- round(adm$mm_n / adm$n, 4)
  adm$mm_share_errors <- round(ifelse(adm$k > 0, adm$mm_k / adm$k, 0), 4)
  adm$artifact_i <- adm$mm_share_flags >= MM_TAG_SHARE | adm$mm_share_errors >= MM_TAG_SHARE
  adm
}
