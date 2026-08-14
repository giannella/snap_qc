# Runner: three-arm frozen lists (state / national / blended) per state,
# evaluation-only from the cached v2.5.0 benchmark pools.
#   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" runners/run_threearm_2024.R > threearm_2024.log 2>&1
setwd(dirname(dirname(normalizePath(sub("--file=", "",
  grep("^--file=", commandArgs(FALSE), value = TRUE)[1])))))
reg_model_data <- readRDS("reg_model_data.rds")
source("methods/threearm_lists_2024_v2.R")
