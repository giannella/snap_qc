# Runner: the v2.7 production build. A FRESH FY2022-24 mine on the v2.7
# frame (dollar amounts inflated to 2026 by fiscal year with the 2026
# deduction, shelter, SUA and SMD tables; standard and homeless deductions in
# total deductions; benefit ratios and SUA tier in review-year terms), with
# the shipped recipe verbatim
# (methods/v250_build_staged_lists_utilsua_v2.R: xgboost + ranger, 19-variable
# vocabulary with utilities_sua, joint BH FDR 10% + n >= 30, 99% LCB ordering,
# artifact gates, fresh-share walk f = 0.50, seed 117).
#
# Four chained steps:
#   1. staged build -> methods/v270_candidate_lists/ (own cache dir, so the
#      mine is fresh; a killed run resumes from its checkpoints)
#   2. characterization sheet (python, as runners/run_v250_build_utilsua.R)
#   3. join the curated characterization columns onto every list
#   4. PROMOTE into state_delivery_lists/: the 98 blended lists, the
#      characterization sheet, and the national pool artifact
#      national_rule_pool_2022_2024_v270.rds. Nothing is committed.
# A step-2/3/4 failure leaves the staged lists intact; rerun after fixing.
#   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" runners/run_v270_build.R > v270_build.log 2>&1
setwd("C:/Users/ericg/snap_qc")
reg_model_data <- readRDS("reg_model_data.rds")
RESUME_FROM_CHECKPOINT <- TRUE
STAGE <- "methods/v270_candidate_lists"
Sys.setenv(V250_OUT_DIR = STAGE)
source("methods/v250_build_staged_lists_utilsua_v2.R")
stopifnot(identical(OUT_DIR, STAGE))

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

## ---- step 4: PROMOTE into state_delivery_lists/ (v2.7) ----------------------
cat(sprintf("[%s] step 4: promoting into state_delivery_lists/ ...\n",
            format(Sys.time(), "%H:%M:%S")))
stopifnot(length(lists) == 98L)
stopifnot(all(file.copy(lists, "state_delivery_lists/", overwrite = TRUE)))
stopifnot(file.copy(file.path(STAGE, "rule_characterization_v250.csv"),
                    "state_delivery_lists/rule_characterization.csv",
                    overwrite = TRUE))
pool_src <- file.path(STAGE, "cache", "national_pool_117.rds")
stopifnot(file.exists(pool_src))
natl <- readRDS(pool_src)
stopifnot(is.data.frame(natl), all(c("hh", "rule", "n", "k", "lcb") %in% names(natl)))
saveRDS(natl, "state_delivery_lists/national_rule_pool_2022_2024_v270.rds")
cat(sprintf("[%s] step 4 done: 98 blended lists + rule_characterization.csv (%d rules) + national pool artifact (%d rules, %d testing utilities_sua) promoted\n",
            format(Sys.time(), "%H:%M:%S"), nrow(prof), nrow(natl),
            sum(grepl("utilities_sua", natl$rule))))
cat("V2.7 BLENDED BUILD COMPLETE (promoted into state_delivery_lists/; not committed).\n")
