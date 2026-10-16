# Runner: v2.7 national-only lists for the FLIP CELLS (2026-10-06): the same
# chain and the same flip-cell computation as runners/run_national_flip_build.R
# (Michigan both budgets, California 10%, Maine 10%; decided 2026-08-14 that
# both list types ship where the national-vs-blended verdict reversed between
# the July study and the three-arm evaluation), pointed at the v2.7 national
# pool (methods/v270_candidate_lists/cache/national_pool_117.rds, mined by
# runners/run_v270_build.R) and staged in methods/v270_national_flip_lists/.
# Run AFTER runners/run_v270_national_only_build.R, which removes every
# national-only file before shipping its 79 cells. Step 4 ships only the flip
# cells and appends any newly characterized rules. Nothing is committed.
#   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" runners/run_v270_national_flip_build.R > v270_national_flip_build.log 2>&1
setwd("C:/Users/ericg/snap_qc")
suppressMessages(library(dplyr))

## ---- flip cells, computed from the two committed evaluations (unchanged) ----
src <- "methods/state_similarity_v2/transfer_benchmark_train2223_test24"
old <- inner_join(
  read.csv(file.path(src, "frozen_list_results.csv")) %>%
    select(state = target, budget, natl_old = precision_deployed),
  read.csv(file.path(src, "blended_frozen_results.csv")) %>%
    filter(variant == "lcb99") %>%
    select(state = target, budget, blend_old = precision),
  by = c("state", "budget"))
new <- read.csv("methods/threearm_2024/threearm_results_2024.csv") %>%
  filter(arm %in% c("national", "blended")) %>%
  select(state, arm, budget, precision) %>%
  tidyr::pivot_wider(names_from = arm, values_from = precision, names_prefix = "p_")
verdict <- function(natl, blend) ifelse(natl > blend, "national", ifelse(blend > natl, "blended", "tie"))
flips <- old %>% inner_join(new, by = c("state", "budget")) %>%
  mutate(v_old = verdict(natl_old, blend_old), v_new = verdict(p_national, p_blended)) %>%
  filter((v_old == "national" & v_new == "blended") | (v_old == "blended" & v_new == "national"))
SELECT_OVERRIDE <- flips %>% filter(v_new == "blended") %>% select(state, budget)
stopifnot(nrow(SELECT_OVERRIDE) == 4L)
cat(sprintf("flip cells lacking a national list: %s\n",
            paste(SELECT_OVERRIDE$state, SELECT_OVERRIDE$budget, collapse = " | ")))

reg_model_data <- readRDS("reg_model_data.rds")
STAGE <- "methods/v270_national_flip_lists"
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
stopifnot(length(lists) == 4L)
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

## ---- step 4: ship ONLY the flip-cell lists into state_delivery_lists/ -------
ship <- file.path(STAGE, sprintf("national_delivery_%s_2022_2024_budget%02.0f.csv",
                                 gsub(" ", "_", SELECT_OVERRIDE$state), 100 * SELECT_OVERRIDE$budget))
stopifnot(all(file.exists(ship)))
stopifnot(all(file.copy(ship, "state_delivery_lists/", overwrite = TRUE)))
full_fn <- "state_delivery_lists/rule_characterization.csv"
full <- read.csv(full_fn, check.names = FALSE)
new_prof <- prof[!paste(prof$hh, prof$rule) %in% paste(full$hh, full$rule), , drop = FALSE]
if (nrow(new_prof)) {
  stopifnot(identical(names(full), names(new_prof)))
  write.csv(bind_rows(full, new_prof), full_fn, row.names = FALSE)
}
cat(sprintf("[%s] step 4 done: %d flip-cell lists shipped; %d new rules appended to rule_characterization.csv\n",
            format(Sys.time(), "%H:%M:%S"), length(ship), nrow(new_prof)))
cat("V2.7 NATIONAL FLIP-CELL BUILD COMPLETE (shipped into state_delivery_lists/; not committed).\n")
