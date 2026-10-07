# Runner: v2.7.0 delivery lists with the PILE GATE (2026-10-07, issue #29).
# Rules with >= 0.25 of their training flags or errors on reconstruction
# income-pile rows are dropped before the fill, in both builders. No mining:
# the v2.7 pools are copied from methods/v270_candidate_lists/cache (mined by
# runners/run_v270_build.R) into this run's own stage, so the earlier staged
# lists stay as they were. Every state gets both list types at both budgets.
#   1. blended lists: methods/v250_build_staged_lists_utilsua_v2.R (RESUME)
#   2. the published national pool artifact, now carrying the artifact and
#      pile tags (INCL_mine_internal_and_blend_with_national_v2.R drops both)
#   3. characterization + curated columns on the blended lists
#   4. national-only lists for all 98 cells: methods/build_national_only_lists_v2.R
#   5. characterization + curated columns on those; one rule sheet
#   6. checks: 98 + 98 lists, no fill gaps, no pile-tagged or artifact-tagged
#      rule on any list, every rule characterized
#   7. ship into state_delivery_lists/ (same 196 filenames, overwritten)
# Nothing is committed.
#   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" runners/run_v270_pilegate_build.R > v270_pilegate_build.log 2>&1
setwd("C:/Users/ericg/snap_qc")
suppressMessages(library(dplyr))
STAGE <- "methods/v270_pilegate_lists"
NSTAGE <- file.path(STAGE, "national")
SRC_CACHE <- "methods/v270_candidate_lists/cache"
stamp0 <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), sprintf(...)))
stopifnot(!identical(Sys.getenv("V250_PILE_GATE"), "0"))
unlink(STAGE, recursive = TRUE)
dir.create(file.path(STAGE, "cache"), recursive = TRUE)
caches <- Sys.glob(file.path(SRC_CACHE, "*_117.rds"))
stopifnot(length(caches) == 50L)   # national + 49 state pools
stopifnot(all(file.copy(caches, file.path(STAGE, "cache"))))
stamp0("copied %d cached pools into %s", length(caches), STAGE)

CURATED <- c("n_error_cases", "element_groups_to_75", "nature_groups_to_75",
             "found_in_case_record", "share_overissuance",
             "timing_at_certification", "cause_agency")
characterize <- function(dir, glob) {
  Sys.setenv(V250_STAGE_DIR = normalizePath(dir), V250_LIST_GLOB = glob,
             V250_UTIL_FEATURE = "utilities_sua")
  rc <- system2("python", c("methods/v250_characterize_lists.py"), stdout = "", stderr = "")
  Sys.unsetenv(c("V250_STAGE_DIR", "V250_LIST_GLOB"))
  if (!identical(rc, 0L)) { cat("CHARACTERIZATION FAILED in", dir, "\n"); quit(save = "no", status = 1) }
  prof <- read.csv(file.path(dir, "rule_characterization_v250.csv"), check.names = FALSE)
  stopifnot(all(c(CURATED, "n_cases_flagged") %in% names(prof)))
  for (fn in Sys.glob(file.path(dir, glob))) {
    lst <- read.csv(fn, check.names = FALSE)
    stopifnot(!any(c("n_error_cases", "n_error_cases_national") %in% names(lst)))
    merged <- merge(lst, prof, by = c("hh", "rule"), all.x = TRUE, sort = FALSE)
    merged <- merged[order(merged$rank), ]
    stopifnot(nrow(merged) == nrow(lst), !any(is.na(merged$n_error_cases)))
    natl_rows <- merged$pool == "national"
    stopifnot(all(merged$n_flagged_train[natl_rows] == merged$n_cases_flagged[natl_rows]))
    merged <- merged[, c(names(lst), CURATED)]
    names(merged)[names(merged) == "n_error_cases"] <- "n_error_cases_national"
    write.csv(merged, fn, row.names = FALSE)
  }
  prof
}

## ---- 1. blended lists -------------------------------------------------------
reg_model_data <- readRDS("reg_model_data.rds")
RESUME_FROM_CHECKPOINT <- TRUE
Sys.setenv(V250_OUT_DIR = STAGE)
source("methods/v250_build_staged_lists_utilsua_v2.R")
stopifnot(identical(OUT_DIR, STAGE), PILE_GATE)
bs_blend <- bs

## ---- 2. the published national pool artifact (tags included) ----------------
pub <- natl
stopifnot(all(c("hh", "rule", "n", "k", "lcb", "mm_n", "mm_k", "artifact_i",
                "pile_share_flags", "pile_share_errors", "pile_i") %in% names(pub)))
saveRDS(pub, file.path(STAGE, "national_rule_pool_2022_2024_v270.rds"))
stamp0("national pool artifact: %d rules, %d artifact-tagged, %d pile-tagged",
       nrow(pub), sum(pub$artifact_i), sum(pub$pile_i))

## ---- 3. characterize the blended lists --------------------------------------
stamp0("characterizing blended lists ...")
prof_b <- characterize(STAGE, "blended_delivery_*.csv")

## ---- 4. national-only lists, all 98 cells -----------------------------------
SELECT_OVERRIDE <- expand.grid(state = sort(unique(as.character(reg_model_data$state))),
                               budget = c(0.05, 0.10), stringsAsFactors = FALSE)
SELECT_OVERRIDE <- SELECT_OVERRIDE[SELECT_OVERRIDE$state %in% STATES, ]
stopifnot(nrow(SELECT_OVERRIDE) == 98L)
Sys.setenv(NATL_POOL_CACHE = file.path(STAGE, "cache", "national_pool_117.rds"),
           NATL_OUT_DIR = NSTAGE,
           NATL_FFP_SRC = file.path(STAGE, "frame_for_profiles.csv"),
           V250_UTIL_FEATURE = "utilities_sua")
source("methods/build_national_only_lists_v2.R")
stopifnot(identical(OUT_DIR, NSTAGE), PILE_GATE)
bs_natl <- bs

## ---- 5. characterize the national-only lists; one rule sheet ----------------
stamp0("characterizing national-only lists ...")
prof_n <- characterize(NSTAGE, "national_delivery_*.csv")
stopifnot(identical(names(prof_b), names(prof_n)))
sheet <- bind_rows(prof_b, prof_n[!paste(prof_n$hh, prof_n$rule) %in% paste(prof_b$hh, prof_b$rule), ])
write.csv(sheet, file.path(STAGE, "rule_characterization.csv"), row.names = FALSE)

## ---- 6. checks --------------------------------------------------------------
lb <- Sys.glob(file.path(STAGE, "blended_delivery_*_2022_2024_budget*.csv"))
ln <- Sys.glob(file.path(NSTAGE, "national_delivery_*_2022_2024_budget*.csv"))
stopifnot(length(lb) == 98L, length(ln) == 98L)
stopifnot(all(bs_blend$fill_gap_total == 0), all(bs_natl$fill_gap_total == 0))
keys <- paste(sheet$hh, sheet$rule)
n_rules <- 0L
for (fn in c(lb, ln)) {
  x <- read.csv(fn, check.names = FALSE)
  stopifnot(all(x$pile_share_flags < 0.25), all(x$pile_share_errors < 0.25),
            all(x$mm_share_flags < 0.25), all(x$mm_share_errors < 0.25),
            all(paste(x$hh, x$rule) %in% keys))
  n_rules <- n_rules + nrow(x)
}
old <- c(Sys.glob("state_delivery_lists/blended_delivery_*_2022_2024_budget*.csv"),
         Sys.glob("state_delivery_lists/national_delivery_*_2022_2024_budget*.csv"))
stopifnot(setequal(basename(old), basename(c(lb, ln))))
stamp0("checks passed: 196 lists (%d list rows), 0 fill gaps, no tagged rule on any list, rule sheet %d rules",
       n_rules, nrow(sheet))

## ---- 7. ship ----------------------------------------------------------------
stopifnot(all(file.copy(c(lb, ln), "state_delivery_lists/", overwrite = TRUE)))
stopifnot(file.copy(file.path(STAGE, "rule_characterization.csv"),
                    "state_delivery_lists/rule_characterization.csv", overwrite = TRUE))
stopifnot(file.copy(file.path(STAGE, "national_rule_pool_2022_2024_v270.rds"),
                    "state_delivery_lists/national_rule_pool_2022_2024_v270.rds", overwrite = TRUE))
stamp0("shipped 196 lists, the rule sheet and the national pool artifact into state_delivery_lists/")
cat("V2.7 PILE-GATE BUILD COMPLETE (not committed).\n")
