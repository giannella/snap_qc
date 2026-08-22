# Runner: era-2 replication of the utilities SUA-tier variant
# (methods/v250_benchmark_2024_utilrel/era2_design_note.md): two fresh
# mines on the current frame, FY2017-18 -> FY2019, baseline then variant.
# ~8 h; each arm checkpoints per unit. SMOKE=1 smokes both arms.
#   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" runners/run_v250_benchmark_era2.R > v250_era2.log 2>&1
setwd("C:/Users/ericg/snap_qc")
reg_model_data <- readRDS("reg_model_data.rds")
RESUME_FROM_CHECKPOINT <- TRUE
for (arm in c("baseline", "variant")) {
  cat(sprintf("\n[%s] ===== ERA-2 ARM: %s =====\n",
              format(Sys.time(), "%H:%M:%S"), arm))
  e <- new.env(parent = globalenv())
  assign("reg_model_data", reg_model_data, envir = e)
  assign("RESUME_FROM_CHECKPOINT", RESUME_FROM_CHECKPOINT, envir = e)
  source(if (arm == "baseline") "methods/v250_benchmark_era2_baseline_v2.R"
         else "methods/v250_benchmark_era2_utilsua_variant_v2.R", local = e)
  rm(e); invisible(gc())
}
cat(sprintf("\n[%s] era-2 replication: both arms done\n",
            format(Sys.time(), "%H:%M:%S")))
