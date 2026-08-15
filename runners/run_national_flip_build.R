# Runner: national-only lists for the FLIP STATES (decided 2026-08-14):
# cells where the strict national-vs-blended verdict REVERSED between
# the July 18-state study (methods/state_similarity_v2/) and the current
# three-arm evaluation (methods/threearm_2024/), and the current verdict
# is blended - so no national list shipped under the benchmark rule.
# The verdict is not stable for these states, so both list types ship.
# Flip cells whose current verdict is national already have their lists.
# Same chain as runners/run_national_only_build.R with SELECT_OVERRIDE.
# Usage:
#   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" runners/run_national_flip_build.R > national_flip_build.log 2>&1
setwd("C:/Users/ericg/snap_qc")
suppressMessages(library(dplyr))

## ---- flip cells, computed from the two committed evaluations ----------------
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
  tidyr::pivot_wider(names_from = arm, values_from = precision,
                     names_prefix = "p_")
verdict <- function(natl, blend)
  ifelse(natl > blend, "national", ifelse(blend > natl, "blended", "tie"))
flips <- old %>% inner_join(new, by = c("state", "budget")) %>%
  mutate(v_old = verdict(natl_old, blend_old),
         v_new = verdict(p_national, p_blended)) %>%
  filter((v_old == "national" & v_new == "blended") |
         (v_old == "blended" & v_new == "national"))
dir.create("methods/national_only_lists", showWarnings = FALSE)
write.csv(flips, "methods/national_only_lists/selection_flips_2024.csv",
          row.names = FALSE)
SELECT_OVERRIDE <- flips %>% filter(v_new == "blended") %>%
  select(state, budget)
stopifnot(nrow(SELECT_OVERRIDE) > 0)
cat(sprintf("flip cells lacking a national list: %s\n",
            paste(SELECT_OVERRIDE$state, SELECT_OVERRIDE$budget, collapse = " | ")))

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

## ---- step 3: join curated columns -------------------------------------------
CURATED <- c("n_error_cases", "element_groups_to_75", "nature_groups_to_75",
             "found_in_case_record", "share_overissuance",
             "timing_at_certification", "cause_agency")
cat(sprintf("[%s] step 3: joining curated characterization columns ...\n",
            format(Sys.time(), "%H:%M:%S")))
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

## ---- step 4: ship ONLY the flip-cell lists into state_delivery_lists/ -------
ship <- file.path(STAGE, sprintf("national_delivery_%s_2022_2024_budget%02.0f.csv",
                                 gsub(" ", "_", SELECT_OVERRIDE$state),
                                 100 * SELECT_OVERRIDE$budget))
stopifnot(all(file.exists(ship)))
file.copy(ship, "state_delivery_lists/", overwrite = TRUE)
full_fn <- "state_delivery_lists/rule_characterization.csv"
full <- read.csv(full_fn, check.names = FALSE)
new_prof <- prof[!paste(prof$hh, prof$rule) %in% paste(full$hh, full$rule), ,
                 drop = FALSE]
if (nrow(new_prof)) {
  stopifnot(identical(names(full), names(new_prof)))
  write.csv(bind_rows(full, new_prof), full_fn, row.names = FALSE)
}
cat(sprintf("[%s] step 4 done: %d flip-state lists shipped; %d new rules appended to rule_characterization.csv\n",
            format(Sys.time(), "%H:%M:%S"), length(ship), nrow(new_prof)))
cat("NATIONAL FLIP-STATE BUILD COMPLETE.\n")
