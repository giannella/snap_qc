# Shared setup for the v2.7 seed-replicate study (2026-10-02). See design_note.md.
#
# The recipe is not restated here. Every constant and function below is taken BY
# NAME from the shipped one-year-ahead benchmark
# (methods/v250_benchmark_2024_utilrel_v2.R): the engine settings, vocabulary,
# admission (one BH pass at FDR 10% and n >= 30, 99% Wilson LCB ordering), the
# fresh-share fill walk and the test-year scoring. Only the frame file and the
# seed vary across arms.
# SR_ROOT (added 2026-10-16) runs the study from another checkout, e.g. the
# staging worktree; the frame files below are absolute so they resolve from either.
setwd(Sys.getenv("SR_ROOT", "C:/Users/ericg/snap_qc"))
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
# fy2019 (mine FY2017-18, score FY2019) is the exploration window of the
# vote-scoring study (methods/vote_scoring/design_note.md, 2026-10-27); its
# pools live in that study's folder, never beside the fy2024 pools.
# deploy (2026-10-29) mines FY2022-24 for the vote rules the workbooks ship
# (methods/vote_scoring/export_vote_rules.R); it has no test year.
WINDOW <- Sys.getenv("SR_WINDOW", "fy2024")
stopifnot(WINDOW %in% c("fy2024", "fy2022", "fy2019", "deploy"))
if (WINDOW == "fy2022") {
  TRAIN_YEARS <- c("2017", "2018", "2019"); TEST_YEAR <- "2022"
  EXPECT_TRAIN_ROWS <- 116060L; EXPECT_TRAIN_ERRS <- 10920L
  EXPECT_TEST_ROWS <- 36851L; EXPECT_TEST_ERRS <- 3985L
}
if (WINDOW == "fy2019") {
  TRAIN_YEARS <- c("2017", "2018"); TEST_YEAR <- "2019"
  EXPECT_TRAIN_ROWS <- 77905L; EXPECT_TRAIN_ERRS <- 7048L
  EXPECT_TEST_ROWS <- 38155L; EXPECT_TEST_ERRS <- 3872L
}
if (WINDOW == "deploy") {
  TRAIN_YEARS <- c("2022", "2023", "2024"); TEST_YEAR <- character(0)
  EXPECT_TRAIN_ROWS <- 115559L; EXPECT_TRAIN_ERRS <- 13161L
  EXPECT_TEST_ROWS <- 0L; EXPECT_TEST_ERRS <- 0L
}
SMOKE <- identical(Sys.getenv("SMOKE"), "1")
OUT_DIR <- switch(WINDOW, fy2022 = "methods/v270_seed_replicates/fy2022",
                  fy2019 = "methods/vote_scoring/fy2019", deploy = "methods/vote_scoring/deploy",
                  "methods/v270_seed_replicates")
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
  final = "reg_model_data.rds",                                      # built 2026-09-29: October CPI keyed on fiscal year, $10 SUA-tier tolerance (step 3)
  f1016 = "archive_data/reg_model_data_f1016_2026-10-16.rds")        # built 2026-10-16 on staging-v2.7 b53f340: the final frame's code plus Ben's lookup-table audit and homeless / child-support reconstruction fixes
FRAMES <- setNames(file.path("C:/Users/ericg/snap_qc", FRAMES), names(FRAMES))
# f1027 (2026-10-29): the staging frame itself, built 2026-10-27 on Ben's reconstruction fixes
# (ec4d190, tracked at c058459), sha256 eb989fda. The vote rules, the vote table and the
# state lists are re-mined on it on staging (Eric, 2026-10-29).
FRAMES[["f1027"]] <- "C:/Users/ericg/snap_qc_v27/archive_data/reg_model_data_f1027_eb989fda.rds"
# f1030 (2026-10-30): Ben's reconstruction rewrite (ben/state-options to 2570391, merged at
# 6a63a4d), sha256 e3714039. NOT the same cases: 231,602 rows (17 dropped), 118 error labels
# changed (nature 97 / agency 26 variances are no longer errors), FY2022-24 pile rows 1,399 -> 208.
FRAMES[["f1030"]] <- "C:/Users/ericg/snap_qc_v27/archive_data/reg_model_data_f1030_e3714039.rds"
# f1104 (2026-11-04): Ben's donor matching (ben/state-options to 7d9fe2d, merged at cc051b4),
# sha256 4de2373e. 231,551 rows: 51 error cases with fewer than 10 donors dropped (Eric agreed),
# one FY2022-24 error label now clean, FY2022-24 pile rows 208 -> 212 (frame_check_f1104.R).
FRAMES[["f1104"]] <- "C:/Users/ericg/snap_qc_v27/archive_data/reg_model_data_f1104_4de2373e.rds"
# f1109 (2026-11-09): Ben's 2026-11-09 munging (staging 8b8d99f), sha256 08e0e825. over_threshold and
# total_error_amount come from AMTERR; nature 97 / agency 26 variances are flagged, not removed, so 237,557
# rows (6,006 more than f1104); FY2022-24 pile rows 212 -> 208 (frame_check_f1109.R).
FRAMES[["f1109"]] <- "C:/Users/ericg/snap_qc_v27/additional_data/reg_model_data.rds"
FRAME_ROWS <- c(f1030 = 231602L, f1104 = 231551L, f1109 = 237557L)   # every other frame: 231,619
# rows and errors in each window on f1030 (frame_check_f1030.R): train rows, train errors, test rows, test errors
EXPECT_F1030 <- list(fy2024 = c(76028L, 8404L, 39528L, 4760L), fy2022 = c(116046L, 10914L, 36850L, 3989L),
                     fy2019 = c(77898L, 7049L, 38148L, 3865L), deploy = c(115556L, 13164L, 0L, 0L))
EXPECT_F1104 <- list(fy2024 = c(76009L, 8385L, 39519L, 4750L), fy2022 = c(116023L, 10891L, 36844L, 3983L),
                     fy2019 = c(77882L, 7033L, 38141L, 3858L), deploy = c(115528L, 13135L, 0L, 0L))
EXPECT_F1109 <- list(fy2024 = c(77750L, 8477L, 40497L, 4820L), fy2022 = c(119310L, 11048L, 37680L, 4022L),
                     fy2019 = c(80071L, 7122L, 39239L, 3926L), deploy = c(118247L, 13297L, 0L, 0L))
# each frame's seed-117 national pool as the v2.7 benchmark arms cached it
BENCH_POOL <- c(
  nominal = "methods/v270_cpi_benchmark/fy2024_nominal/cache/bench_national_117.rds",
  f0925 ="methods/v270_cpi_benchmark/fy2024_cpi_run2_20260925/cache/bench_national_117.rds",
  final = "methods/v270_cpi_benchmark/fy2024_cpi/cache/bench_national_117.rds")
if (WINDOW == "fy2022") BENCH_POOL <- c(
  final = "methods/v270_cpi_benchmark/fy2022_cpi/cache/bench_national_117.rds",
  f0928 = "methods/v270_cpi_benchmark/fy2022_cpi_annualcpi_20260928/cache/bench_national_117.rds")
if (WINDOW %in% c("fy2019", "deploy")) BENCH_POOL <- character(0)   # no cached pool to anchor against
BENCH_CSV <- c(
  f0925 = "methods/v270_cpi_benchmark/fy2024_cpi_run2_20260925/v250_benchmark_2024.csv",
  final = "methods/v270_cpi_benchmark/fy2024_cpi/v250_benchmark_2024.csv")
SEEDS <- c(117L, 118L, 119L)

pool_file <- function(frame, seed, kind = c("admitted", "raw"))
  file.path(POOL_DIR, sprintf("%s_%s_seed%d.rds", match.arg(kind), frame, seed))

# Training years a pool was mined on. Pools mined since 2026-10-27 carry them
# as attr "train_years"; older pools are identified by the folder they sit in.
pool_train_years <- function(pool, fn) {
  ty <- attr(pool, "train_years")
  if (!is.null(ty)) return(ty)
  fn <- gsub("\\\\", "/", fn)
  if (grepl("v270_seed_replicates/fy2022/|v270_cpi_benchmark/fy2022", fn)) return(c("2017", "2018", "2019"))
  if (grepl("v270_seed_replicates/pools/|v270_cpi_benchmark/fy2024", fn)) return(c("2022", "2023"))
  stop("no training years recorded for pool ", fn)
}
read_pool <- function(fn, expect_years = TRAIN_YEARS) {
  pool <- readRDS(fn)
  ty <- pool_train_years(pool, fn)
  if (!identical(as.character(ty), as.character(expect_years)))
    stop(sprintf("pool %s was mined on FY%s, expected FY%s", fn,
                 paste(ty, collapse = "+"), paste(expect_years, collapse = "+")))
  pool
}

# the benchmark's frame block (its lines 112-163), verbatim in substance
load_frame <- function(id) {
  stopifnot(id %in% names(FRAMES))
  reg <- readRDS(FRAMES[[id]])
  stopifnot(nrow(reg) == (if (id %in% names(FRAME_ROWS)) FRAME_ROWS[[id]] else 231619L))
  ex <- if (id == "f1030") EXPECT_F1030[[WINDOW]] else if (id == "f1104") EXPECT_F1104[[WINDOW]] else
    if (id == "f1109") EXPECT_F1109[[WINDOW]] else
    c(EXPECT_TRAIN_ROWS, EXPECT_TRAIN_ERRS, EXPECT_TEST_ROWS, EXPECT_TEST_ERRS)
  adf0 <- reg %>% filter(fiscal_year %in% c(TRAIN_YEARS, TEST_YEAR))
  stopifnot("utilities_sua" %in% names(adf0))
  pf <- prep_features(adf0, VOCAB19); adf <- pf$data
  stopifnot(length(setdiff(VOCAB19, pf$features)) == 0)
  st <- as.character(adf$state); yr <- as.character(adf$fiscal_year)
  ie_all <- !is.na(adf$over_threshold) & adf$over_threshold != 0
  ed_all <- ifelse(ie_all, abs(ifelse(is.na(adf$total_error_amount), 0,
                                      adf$total_error_amount)), 0)
  stopifnot(sum(yr %in% TRAIN_YEARS) == ex[1],
            sum(ie_all[yr %in% TRAIN_YEARS]) == ex[2],
            sum(yr == TEST_YEAR) == ex[3],
            sum(ie_all[yr == TEST_YEAR]) == ex[4])
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
