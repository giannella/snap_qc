# Runner: STAGED build of the utilities-SUA tier VARIANT lists (2026-08-22;
# design + result in methods/v250_benchmark_2024_utilrel/). Same three
# chained steps as runners/run_v250_build.R, redirected to
# methods/v250_candidate_lists_utilsua/. NOTHING here touches
# state_delivery_lists/ (promotion is a project-lead decision after the
# era-2 replication).
#   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" runners/run_v250_build_utilsua.R > v250_build_utilsua.log 2>&1
setwd("C:/Users/ericg/snap_qc")
reg_model_data <- readRDS("reg_model_data.rds")
RESUME_FROM_CHECKPOINT <- TRUE
source("methods/v250_build_staged_lists_utilsua_v2.R")

STAGE <- "methods/v250_candidate_lists_utilsua"
Sys.setenv(V250_STAGE_DIR = file.path(getwd(), STAGE),
           V250_UTIL_FEATURE = "utilities_sua")

## ---- step 2: characterization (python) --------------------------------------
cat(sprintf("[%s] step 2: characterization sheet ...\n",
            format(Sys.time(), "%H:%M:%S")))
rc <- system2("python", c("methods/v250_characterize_lists.py"),
              stdout = "", stderr = "")
if (!identical(rc, 0L)) {
  cat("STEP 2 FAILED (exit ", rc, ") - staged lists are intact; rerun after fixing.\n")
  quit(save = "no", status = 1)
}

## ---- step 3: join CURATED characterization columns (as run_v250_build.R) ----
CURATED <- c("n_error_cases", "element_groups_to_75", "nature_groups_to_75",
             "found_in_case_record", "share_overissuance",
             "timing_at_certification", "cause_agency")
cat(sprintf("[%s] step 3: joining curated characterization columns ...\n",
            format(Sys.time(), "%H:%M:%S")))
suppressMessages(library(dplyr))
prof <- read.csv(file.path(STAGE, "rule_characterization_v250.csv"),
                 check.names = FALSE)
stopifnot(all(c(CURATED, "n_cases_flagged") %in% names(prof)))
lists <- Sys.glob(file.path(STAGE, "blended_delivery_*.csv"))
stopifnot(length(lists) > 0)
n_joined <- 0L
for (fn in lists) {
  lst <- read.csv(fn, check.names = FALSE)
  if (any(c("n_error_cases", "n_error_cases_national") %in% names(lst)))
    next
  merged <- merge(lst, prof, by = c("hh", "rule"), all.x = TRUE, sort = FALSE)
  merged <- merged[order(merged$rank), ]
  stopifnot(nrow(merged) == nrow(lst), !any(is.na(merged$n_error_cases)))
  natl_rows <- merged$pool == "national"
  stopifnot(all(merged$n_flagged_train[natl_rows] ==
                merged$n_cases_flagged[natl_rows]))
  merged <- merged[, c(names(lst), CURATED)]
  names(merged)[names(merged) == "n_error_cases"] <- "n_error_cases_national"
  write.csv(merged, fn, row.names = FALSE)
  n_joined <- n_joined + 1L
}
cat(sprintf("[%s] step 3 done: %d curated columns joined onto %d lists\n",
            format(Sys.time(), "%H:%M:%S"), length(CURATED), n_joined))
cat("STAGED utilities-SUA variant build COMPLETE (not the shipped deliverable).\n")
