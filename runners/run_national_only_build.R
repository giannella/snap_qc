# Runner: NATIONAL-ONLY delivery lists for the states where the FY2024
# three-arm benchmark says national >= blended (decided 2026-08-14).
# Four chained steps:
#   1. selection + walk -> methods/national_only_lists/ (R driver;
#      evaluation-only from the cached production national pool)
#   2. characterization sheet for the lists' rules (python, section 29)
#   3. join the curated characterization columns onto every list
#   4. copy the lists into state_delivery_lists/ and extend the shipped
#      full characterization sheet with any newly characterized rules
# A step-2/3 failure leaves the staged lists intact; rerun after fixing.
# Usage:
#   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" runners/run_national_only_build.R > national_only_build.log 2>&1
setwd("C:/Users/ericg/snap_qc")
reg_model_data <- readRDS("reg_model_data.rds")
source("methods/build_national_only_lists_v2.R")

STAGE <- "methods/national_only_lists"

## ---- step 2: characterization (python, stage-dir + glob overrides) ----------
cat(sprintf("[%s] step 2: characterization sheet ...\n",
            format(Sys.time(), "%H:%M:%S")))
Sys.setenv(V250_STAGE_DIR = normalizePath(STAGE),
           V250_LIST_GLOB = "national_delivery_*.csv")
rc <- system2("python", c("methods/v250_characterize_lists.py"),
              stdout = "", stderr = "")
Sys.unsetenv(c("V250_STAGE_DIR", "V250_LIST_GLOB"))
if (!identical(rc, 0L)) {
  cat("STEP 2 FAILED (exit ", rc, ") - staged lists are intact; rerun after fixing.\n")
  quit(save = "no", status = 1)
}

## ---- step 3: join curated columns (same block as the blended chain) ---------
CURATED <- c("n_error_cases", "element_groups_to_75", "nature_groups_to_75",
             "found_in_case_record", "share_overissuance",
             "timing_at_certification", "cause_agency")
cat(sprintf("[%s] step 3: joining curated characterization columns ...\n",
            format(Sys.time(), "%H:%M:%S")))
suppressMessages(library(dplyr))
prof <- read.csv(file.path(STAGE, "rule_characterization_v250.csv"),
                 check.names = FALSE)
stopifnot(all(c(CURATED, "n_cases_flagged") %in% names(prof)))
lists <- Sys.glob(file.path(STAGE, "national_delivery_*.csv"))
stopifnot(length(lists) > 0)
n_joined <- 0L
for (fn in lists) {
  lst <- read.csv(fn, check.names = FALSE)
  if (any(c("n_error_cases", "n_error_cases_national") %in% names(lst)))
    next
  merged <- merge(lst, prof, by = c("hh", "rule"), all.x = TRUE, sort = FALSE)
  merged <- merged[order(merged$rank), ]
  stopifnot(nrow(merged) == nrow(lst), !any(is.na(merged$n_error_cases)))
  # cross-language flag-count identity (all rows are national-pool rows)
  stopifnot(all(merged$n_flagged_train == merged$n_cases_flagged))
  merged <- merged[, c(names(lst), CURATED)]
  names(merged)[names(merged) == "n_error_cases"] <- "n_error_cases_national"
  write.csv(merged, fn, row.names = FALSE)
  n_joined <- n_joined + 1L
}
cat(sprintf("[%s] step 3 done: %d lists joined\n",
            format(Sys.time(), "%H:%M:%S"), n_joined))

## ---- step 4: ship into state_delivery_lists/ --------------------------------
ship <- Sys.glob(file.path(STAGE, "national_delivery_*.csv"))
file.copy(ship, "state_delivery_lists/", overwrite = TRUE)
# extend the shipped full characterization sheet with any new rules
full_fn <- "state_delivery_lists/rule_characterization.csv"
full <- read.csv(full_fn, check.names = FALSE)
new_prof <- prof[!paste(prof$hh, prof$rule) %in% paste(full$hh, full$rule), ,
                 drop = FALSE]
if (nrow(new_prof)) {
  stopifnot(identical(names(full), names(new_prof)))
  write.csv(bind_rows(full, new_prof), full_fn, row.names = FALSE)
}
cat(sprintf("[%s] step 4 done: %d lists shipped to state_delivery_lists/; %d new rules appended to rule_characterization.csv\n",
            format(Sys.time(), "%H:%M:%S"), length(ship), nrow(new_prof)))
cat("NATIONAL-ONLY BUILD COMPLETE.\n")
