# Runner: v2.5.0 CANDIDATE build (STAGED). Three chained steps:
#   1. mine + blend + fill -> methods/v250_candidate_lists/ (R driver)
#   2. characterization sheet for the new rules (python, section-29 machinery)
#   3. join the characterization columns onto every staged list CSV
# A step-2/3 failure leaves the lists intact and is reported, not fatal to
# them; rerun the runner (RESUME_FROM_CHECKPOINT skips finished mines).
# NOTE: do not run this runner with SMOKE=1 - the driver would write lists
# to smoke/ while steps 2-3 read the parent dir (set V250_STAGE_DIR when
# smoking the python step by hand).
# Usage:
#   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" runners/run_v250_build.R > v250_build.log 2>&1
setwd("C:/Users/ericg/snap_qc")
reg_model_data <- readRDS("reg_model_data.rds")
RESUME_FROM_CHECKPOINT <- TRUE
source("methods/v250_build_staged_lists_v2.R")

## ---- step 2: characterization (python) --------------------------------------
cat(sprintf("[%s] step 2: characterization sheet ...\n",
            format(Sys.time(), "%H:%M:%S")))
rc <- system2("python", c("methods/v250_characterize_lists.py"),
              stdout = "", stderr = "")
if (!identical(rc, 0L)) {
  cat("STEP 2 FAILED (exit ", rc, ") - staged lists are intact; rerun after fixing.\n")
  quit(save = "no", status = 1)
}

## ---- step 3: join CURATED characterization columns onto the lists -----------
# Eric's selection (2026-08-11): the readable headlines plus five shares and
# one support column. The FULL ~150-column sheet stays separate
# (rule_characterization_v250.csv) for anyone who wants to join the rest.
CURATED <- c("n_error_cases", "element_groups_to_75", "nature_groups_to_75",
             "found_in_case_record", "share_overissuance",
             "timing_at_certification", "cause_agency", "cause_client")
cat(sprintf("[%s] step 3: joining curated characterization columns ...\n",
            format(Sys.time(), "%H:%M:%S")))
suppressMessages(library(dplyr))
prof <- read.csv("methods/v250_candidate_lists/rule_characterization_v250.csv",
                 check.names = FALSE)
stopifnot(all(c(CURATED, "n_cases_flagged") %in% names(prof)))
lists <- Sys.glob("methods/v250_candidate_lists/blended_delivery_*.csv")
stopifnot(length(lists) > 0)
n_joined <- 0L
for (fn in lists) {
  lst <- read.csv(fn, check.names = FALSE)
  if ("n_error_cases" %in% names(lst)) next   # already joined (rerun)
  # merge the FULL sheet so the integrity guards can see every field,
  # then deliver only the curated block
  merged <- merge(lst, prof, by = c("hh", "rule"), all.x = TRUE, sort = FALSE)
  merged <- merged[order(merged$rank), ]
  stopifnot(nrow(merged) == nrow(lst), !any(is.na(merged$n_error_cases)))
  # review 4a: cross-language flag-count identity for national-pool rows -
  # R's n_flagged_train (rds, full precision) must equal python's
  # n_cases_flagged (round-trip CSV export). Converts any export/parser/eval
  # drift (the 2026-08-06 ULP erratum's failure mode) into a loud stop.
  natl_rows <- merged$pool == "national"
  stopifnot(all(merged$n_flagged_train[natl_rows] ==
                merged$n_cases_flagged[natl_rows]))
  # shipped column order first, curated characterization block after
  merged <- merged[, c(names(lst), CURATED)]
  write.csv(merged, fn, row.names = FALSE)
  n_joined <- n_joined + 1L
}
cat(sprintf("[%s] step 3 done: %d curated columns joined onto %d lists (full sheet: rule_characterization_v250.csv)\n",
            format(Sys.time(), "%H:%M:%S"), length(CURATED), n_joined))
cat("STAGED v2.5.0 candidate build COMPLETE (not the shipped deliverable).\n")
