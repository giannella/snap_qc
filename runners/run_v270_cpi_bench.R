# Runner: the v2.7 CPI-frame MEASUREMENT (2026-09-24). Not a ship/no-ship
# test (the project lead's call: the CPI adjustment makes the data truer to
# its era, so the question is only how much one-year-ahead precision and
# recall move, for the record of how the pipeline has evolved).
#
# Two windows x two arms, the shipped benchmark recipe verbatim (national +
# state pools, joint BH + n >= 30, 99% LCB, artifact gates, fresh-share walk,
# 5% / 10% budgets, seed 117), one component varying per window: the frame.
#   window fy2024: methods/v250_benchmark_2024_utilrel_v2.R      (mine FY2022-23, walk FY2024)
#   window fy2019: methods/v250_benchmark_era2_utilsua_variant_v2.R (mine FY2017-18, walk FY2019)
#   window fy2022: the same script (mine FY2017-19, walk FY2022, three years
#                  ahead across the excluded FY2020-21)
#   arm nominal:   archive_data/reg_model_data_pre_cpi_rebuild_2026-09-24.rds
#                  (main at 485811f, the 2026-09-21 rebuild)
#   arm cpi:       reg_model_data.rds (staging-v2.7, rebuilt 2026-09-28:
#                  dollar amounts inflated to 2026 by fiscal year with the
#                  2026 deduction, shelter, SUA and SMD tables; standard +
#                  homeless deductions in total deductions; benefit ratios
#                  and SUA tier in review-year terms)
# Outputs -> methods/v270_cpi_benchmark/<window>_<arm>/ (each arm ~3.5-4 h,
# ~13 GB; at most two arms at once on this host). Each arm checkpoints per
# unit and resumes on relaunch. Readout: methods/v270_cpi_benchmark/readout.R.
#   CPI_WINDOW=fy2024 CPI_ARM=nominal "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" runners/run_v270_cpi_bench.R > v270_cpi_fy2024_nominal.log 2>&1
setwd("C:/Users/ericg/snap_qc")
WINDOW <- Sys.getenv("CPI_WINDOW"); ARM <- Sys.getenv("CPI_ARM")
stopifnot(WINDOW %in% c("fy2024", "fy2019", "fy2022"), ARM %in% c("nominal", "cpi"))
if (WINDOW == "fy2022")
  Sys.setenv(BENCH_TRAIN_YEARS = "2017,2018,2019", BENCH_TEST_YEAR = "2022",
             BENCH_EXPECT = "116060,10920,36851,3985")
FRAME <- if (ARM == "nominal") "archive_data/reg_model_data_pre_cpi_rebuild_2026-09-24.rds" else "reg_model_data.rds"
stopifnot(file.exists(FRAME))
reg_model_data <- readRDS(FRAME)
cat(sprintf("[%s] window %s | arm %s | frame %s (%d rows)\n",
            format(Sys.time(), "%H:%M:%S"), WINDOW, ARM, FRAME, nrow(reg_model_data)))
RESUME_FROM_CHECKPOINT <- TRUE
Sys.setenv(BENCH_OUT_DIR = sprintf("methods/v270_cpi_benchmark/%s_%s", WINDOW, ARM))
source(if (WINDOW == "fy2024") "methods/v250_benchmark_2024_utilrel_v2.R"
       else "methods/v250_benchmark_era2_utilsua_variant_v2.R")
cat(sprintf("[%s] window %s | arm %s DONE -> %s\n",
            format(Sys.time(), "%H:%M:%S"), WINDOW, ARM, Sys.getenv("BENCH_OUT_DIR")))
