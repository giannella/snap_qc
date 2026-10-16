# Runner: national-only lists for every state and budget (decided 2026-10-06).
# The v2.7 build (runners/run_v270_national_only_build.R plus
# runners/run_v270_national_flip_build.R, committed in b1823c2) shipped 83 of
# the 98 state x budget cells under the August selection rule. Every state now
# gets both list types, and the state's own internal validation decides
# between them; the public-data comparison behind that decision is in
# methods/v270_national_only_selection/ (score_windows.R, selection_v270.csv).
# This runner builds only the 15 missing cells, on the same v2.7 national pool
# and builder, plus one existing cell as a control: the control must reproduce
# its shipped file column for column before anything is copied. Same chain as
# the flip runner: build, characterize, join the curated columns, ship the new
# cells, append newly characterized rules to the rule sheet. Nothing is committed.
#   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" runners/run_v270_national_only_fill.R > v270_national_only_fill.log 2>&1
setwd("C:/Users/ericg/snap_qc")
suppressMessages(library(dplyr))
fn_of <- function(dir, state, budget)
  file.path(dir, sprintf("national_delivery_%s_2022_2024_budget%02.0f.csv", gsub(" ", "_", state), 100 * budget))

grid <- expand.grid(state = sort(unique(read.csv("methods/v270_national_only_selection/selection_v270.csv")$state)),
                    budget = c(0.05, 0.10), stringsAsFactors = FALSE)
stopifnot(nrow(grid) == 98L)
missing <- grid[!file.exists(fn_of("state_delivery_lists", grid$state, grid$budget)), ]
stopifnot(nrow(missing) == 15L)
CONTROL <- data.frame(state = "Alabama", budget = 0.05)
stopifnot(file.exists(fn_of("state_delivery_lists", CONTROL$state, CONTROL$budget)))
SELECT_OVERRIDE <- rbind(missing, CONTROL)
cat(sprintf("building %d missing cells + control %s %.0f%%: %s\n", nrow(missing), CONTROL$state,
            100 * CONTROL$budget, paste(missing$state, missing$budget, collapse = " | ")))

reg_model_data <- readRDS("reg_model_data.rds")
STAGE <- "methods/v270_national_only_fill_lists"
unlink(STAGE, recursive = TRUE)
Sys.setenv(NATL_POOL_CACHE = "methods/v270_candidate_lists/cache/national_pool_117.rds",
           NATL_OUT_DIR = STAGE,
           NATL_FFP_SRC = "methods/v270_candidate_lists/frame_for_profiles.csv",
           V250_UTIL_FEATURE = "utilities_sua")
stopifnot(file.exists(Sys.getenv("NATL_POOL_CACHE")), file.exists(Sys.getenv("NATL_FFP_SRC")))
source("methods/build_national_only_lists_v2.R")
stopifnot(identical(OUT_DIR, STAGE))

## ---- step 2: characterization (python, stage-dir + glob overrides) ----------
cat(sprintf("[%s] step 2: characterization sheet ...\n", format(Sys.time(), "%H:%M:%S")))
Sys.setenv(V250_STAGE_DIR = normalizePath(STAGE), V250_LIST_GLOB = "national_delivery_*.csv")
rc <- system2("python", c("methods/v250_characterize_lists.py"), stdout = "", stderr = "")
Sys.unsetenv(c("V250_STAGE_DIR", "V250_LIST_GLOB"))
if (!identical(rc, 0L)) {
  cat("STEP 2 FAILED (exit ", rc, ") - staged lists are intact; rerun after fixing.\n")
  quit(save = "no", status = 1)
}

## ---- step 3: join curated columns (same block as the other chains) ----------
CURATED <- c("n_error_cases", "element_groups_to_75", "nature_groups_to_75",
             "found_in_case_record", "share_overissuance",
             "timing_at_certification", "cause_agency")
prof <- read.csv(file.path(STAGE, "rule_characterization_v250.csv"), check.names = FALSE)
stopifnot(all(c(CURATED, "n_cases_flagged") %in% names(prof)))
lists <- Sys.glob(file.path(STAGE, "national_delivery_*.csv"))
stopifnot(length(lists) == nrow(SELECT_OVERRIDE))
for (fn in lists) {
  lst <- read.csv(fn, check.names = FALSE)
  if (any(c("n_error_cases", "n_error_cases_national") %in% names(lst))) next
  merged <- merge(lst, prof, by = c("hh", "rule"), all.x = TRUE, sort = FALSE)
  merged <- merged[order(merged$rank), ]
  stopifnot(nrow(merged) == nrow(lst), !any(is.na(merged$n_error_cases)))
  stopifnot(all(merged$n_flagged_train == merged$n_cases_flagged))
  merged <- merged[, c(names(lst), CURATED)]
  names(merged)[names(merged) == "n_error_cases"] <- "n_error_cases_national"
  write.csv(merged, fn, row.names = FALSE)
}

## ---- control: the rebuilt existing cell must equal its shipped file ---------
ctl_new <- read.csv(fn_of(STAGE, CONTROL$state, CONTROL$budget), check.names = FALSE)
ctl_old <- read.csv(fn_of("state_delivery_lists", CONTROL$state, CONTROL$budget), check.names = FALSE)
rownames(ctl_new) <- rownames(ctl_old) <- NULL
ok <- isTRUE(all.equal(ctl_new, ctl_old, check.attributes = FALSE))
if (!ok) {
  print(all.equal(ctl_new, ctl_old, check.attributes = FALSE))
  cat("CONTROL FAILED: the builder does not reproduce the shipped list; nothing shipped.\n")
  quit(save = "no", status = 1)
}
cat(sprintf("control %s %.0f%%: rebuilt list equals the shipped file (%d rules)\n",
            CONTROL$state, 100 * CONTROL$budget, nrow(ctl_new)))

## ---- step 4: ship ONLY the missing cells; append new rules to the sheet -----
ship <- fn_of(STAGE, missing$state, missing$budget)
stopifnot(all(file.exists(ship)))
stopifnot(all(file.copy(ship, "state_delivery_lists/", overwrite = FALSE)))
full_fn <- "state_delivery_lists/rule_characterization.csv"
full <- read.csv(full_fn, check.names = FALSE)
new_prof <- prof[!paste(prof$hh, prof$rule) %in% paste(full$hh, full$rule), , drop = FALSE]
if (nrow(new_prof)) {
  stopifnot(identical(names(full), names(new_prof)))
  write.csv(bind_rows(full, new_prof), full_fn, row.names = FALSE)
}
n_now <- length(Sys.glob("state_delivery_lists/national_delivery_*.csv"))
stopifnot(n_now == 98L)
cat(sprintf("[%s] step 4 done: %d lists shipped (98 national-only lists now); %d new rules appended to rule_characterization.csv\n",
            format(Sys.time(), "%H:%M:%S"), length(ship), nrow(new_prof)))
cat("V2.7 NATIONAL-ONLY FILL COMPLETE (shipped into state_delivery_lists/; not committed).\n")
