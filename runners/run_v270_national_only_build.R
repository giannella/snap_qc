# Runner: v2.7 NATIONAL-ONLY delivery lists (2026-09-24), the same four
# chained steps as runners/run_national_only_build.R, pointed at the v2.7
# national pool (methods/v270_candidate_lists/cache/national_pool_117.rds,
# mined by runners/run_v270_build.R, which must have completed) and staged
# in methods/v270_national_only_lists/. The state x budget selection is the
# unchanged FY2024 three-arm rule (methods/threearm_2024/). Step 4 ships the
# lists into state_delivery_lists/ and appends any newly characterized rules
# to the shipped characterization sheet. Nothing is committed.
#   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" runners/run_v270_national_only_build.R > v270_national_only_build.log 2>&1
setwd("C:/Users/ericg/snap_qc")
reg_model_data <- readRDS("reg_model_data.rds")
STAGE <- "methods/v270_national_only_lists"
Sys.setenv(NATL_POOL_CACHE = "methods/v270_candidate_lists/cache/national_pool_117.rds",
           NATL_OUT_DIR = STAGE,
           NATL_FFP_SRC = "methods/v270_candidate_lists/frame_for_profiles.csv",
           V250_UTIL_FEATURE = "utilities_sua")
stopifnot(file.exists(Sys.getenv("NATL_POOL_CACHE")),
          file.exists(Sys.getenv("NATL_FFP_SRC")))
source("methods/build_national_only_lists_v2.R")
stopifnot(identical(OUT_DIR, STAGE))

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
  stopifnot(all(merged$n_flagged_train == merged$n_cases_flagged))
  merged <- merged[, c(names(lst), CURATED)]
  names(merged)[names(merged) == "n_error_cases"] <- "n_error_cases_national"
  write.csv(merged, fn, row.names = FALSE)
  n_joined <- n_joined + 1L
}
cat(sprintf("[%s] step 3 done: %d lists joined\n",
            format(Sys.time(), "%H:%M:%S"), n_joined))

## ---- step 4: ship into state_delivery_lists/ --------------------------------
# the v2.6.0 national-only lists (83 files, v2.5.0 pool) are removed first so
# a state x budget cell no longer selected does not linger as a stale file
old <- Sys.glob("state_delivery_lists/national_delivery_*.csv")
invisible(file.remove(old))
ship <- Sys.glob(file.path(STAGE, "national_delivery_*.csv"))
stopifnot(all(file.copy(ship, "state_delivery_lists/", overwrite = TRUE)))
full_fn <- "state_delivery_lists/rule_characterization.csv"
full <- read.csv(full_fn, check.names = FALSE)
new_prof <- prof[!paste(prof$hh, prof$rule) %in% paste(full$hh, full$rule), ,
                 drop = FALSE]
if (nrow(new_prof)) {
  stopifnot(identical(names(full), names(new_prof)))
  write.csv(bind_rows(full, new_prof), full_fn, row.names = FALSE)
}
cat(sprintf("[%s] step 4 done: %d old lists removed, %d lists shipped to state_delivery_lists/; %d new rules appended to rule_characterization.csv\n",
            format(Sys.time(), "%H:%M:%S"), length(old), length(ship), nrow(new_prof)))
cat("V2.7 NATIONAL-ONLY BUILD COMPLETE (shipped into state_delivery_lists/; not committed).\n")
