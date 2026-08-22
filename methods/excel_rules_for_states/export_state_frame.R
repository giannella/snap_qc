# Export one state's rows from the munged modelling frame, prepped exactly as
# the rule miner preps them, for the Python dashboard to consume.
#
# The dashboard used to rebuild its features from the public .sav files in
# Python. That reproduced the row universe exactly but not the feature values:
# it skipped the pre-QC variable restoration that
# 1_data_munging_and_raw_variable_reconstruction_for_using_public_qc_data.R
# performs (correct_variables <- TRUE), so 21 of Washington's 114 core rules
# never fired at all. The delivery rules were mined on the restored values, so
# the dashboard has to score against the same ones. This script is the bridge.
#
# Two things are deliberately delegated rather than reimplemented:
#   * the frame itself comes from reg_model_data.rds, saved by the munging
#     script, so no variable is derived twice;
#   * the 0/1 coercion of logicals and two-level factors comes from
#     prep_features() in rule_mining_helpers.R, the same function the delivery
#     builder calls, so rule text and column values cannot drift apart.
#
# Usage (paths relative to the snap_qc repo root, which must be the working
# directory or passed as --repo):
#
#   Rscript export_state_frame.R --state "Washington" --out wa_frame.csv \
#           [--years 2022,2023,2024] [--repo /path/to/snap_qc]

suppressMessages(library(dplyr))

args <- commandArgs(trailingOnly = TRUE)
arg <- function(name, default = NULL) {
  i <- match(paste0("--", name), args)
  if (is.na(i) || i == length(args)) return(default)
  args[i + 1]
}

STATE <- arg("state")
OUT   <- arg("out")
YEARS <- strsplit(arg("years", "2022,2023,2024"), ",")[[1]]
REPO  <- arg("repo", getwd())
if (is.null(STATE) || is.null(OUT)) stop("need --state and --out")

setwd(REPO)
if (!file.exists("rule_mining_helpers.R"))
  stop("run from the snap_qc repo root, or pass --repo")
source("rule_mining_helpers.R")

RDS <- "reg_model_data.rds"
if (!file.exists(RDS))
  stop(RDS, " not found. It is written by ",
       "1_data_munging_and_raw_variable_reconstruction_for_using_public_qc_data.R ",
       "and is gitignored, so it has to be built (or copied) before this runs.")

# The feature vocabulary the delivery lists were mined with. This is the union
# of every variable appearing in the 49 tracked blended budget10 lists as of
# 2026-08-15 (bbce_state_i replaced cat_elig in the 2026-08-13 rebuild).
# prep_features() silently drops any name absent from the frame, which is why
# this list can carry more than the frame has.
FEATURES <- c(
  "HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size",
  "expedited_i", "bbce_state_i", "rawben_rel_max", "medical_deductions",
  "shelter_expenses_by_hh_size", "utilities", "married", "homeless",
  "earned_by_hh_size", "unearned_by_hh_size", "gross_by_hh_size",
  "percent_abawd", "unc_rawben_rel_max",
  "months_since_cert_n", "count_divisible_by_100",
  "utilities_sua"
)

hh_group_of <- function(n) {
  n <- suppressWarnings(as.numeric(as.character(n)))
  ifelse(is.na(n), NA_character_, ifelse(n <= 1, "1", ifelse(n <= 3, "2-3", "4+")))
}

cat(sprintf("reading %s ...\n", RDS))
d <- readRDS(RDS)
cat(sprintf("frame: %d rows\n", nrow(d)))

# SUA tier (vocabulary variant, 2026-08-22; methods/v250_benchmark_2024_utilrel/
# design_note.md): computed per state-year exactly as the variant mine
# computes it, so the workbook's demo sits on the mined scale. Harmless
# when a delivery list does not use it (prep_features keeps the column;
# the workbook only emits features its rules reference).
mode_pos <- function(x) {
  x <- round(x[x > 0])
  if (!length(x)) return(NA_real_)
  as.numeric(names(sort(table(x), decreasing = TRUE))[1])
}
d <- d %>% group_by(state_name, fiscal_year) %>%
  mutate(utilities_sua = ifelse(utilities <= 0, 0L,
                                ifelse(utilities < mode_pos(utilities) - 200,
                                       1L, 2L))) %>%
  ungroup()

states_present <- unique(as.character(d$state))
if (!STATE %in% states_present)
  stop("state '", STATE, "' not in the frame. Available: ",
       paste(head(sort(states_present), 60), collapse = ", "))

w <- d %>% filter(as.character(state) == STATE,
                  as.character(fiscal_year) %in% YEARS)
cat(sprintf("%s, FY%s: %d rows\n", STATE, paste(YEARS, collapse = "+"), nrow(w)))
if (nrow(w) == 0) stop("no rows for that state/year combination")

# Coerce exactly as the miner does; `pf$features` is what actually survived.
pf <- prep_features(w, FEATURES)
w  <- pf$data
cat(sprintf("features prepped: %d of %d requested (%s)\n",
            length(pf$features), length(FEATURES),
            paste(setdiff(FEATURES, pf$features), collapse = ", ")))

# prep_features() drops features with a single value, right for mining but not
# for scoring: a national rule can reference a feature that is constant within
# one state (e.g. bbce_state_i in a BBCE state). Re-append any requested
# feature that exists in the frame but fell out, coerced 0/1 the same way.
dropped_present <- setdiff(intersect(FEATURES, names(w)), pf$features)
if (length(dropped_present)) {
  for (v in dropped_present)
    if (is.logical(w[[v]])) w[[v]] <- as.integer(w[[v]])
  pf$features <- c(pf$features, dropped_present)
  cat(sprintf("re-appended constant-in-state features: %s\n",
              paste(dropped_present, collapse = ", ")))
}

keys <- intersect(c("yrmonth", "hhldno", "stratum"), names(w))
extra <- intersect(c("cert_HH_size_FS_n", "over_threshold", "total_error_amount",
                     "error_status", "second_element_i", "ded_fields_imputed",
                     "fiscal_year",
                     # reconstructed (pre-QC-review) input-level fields: the
                     # workbook's Data tab demo carries these, so its figures
                     # sit on the same scale the rules were mined on
                     "rawearn", "rawunearn", "rawdepded", "rawcsded", "rawrent",
                     "rawhomeless_ded", "fsnkid", "fsnelder", "fsndis",
                     "count_abawd", "cat_elig",
                     # QC outcome pair + review disposition: rawben is the
                     # benefit as issued (reported, not reconstructed) and
                     # benefit_amount_FS the QC-corrected one; their rounded
                     # absolute difference IS total_error_amount by
                     # construction (munging line 165), so the workbook's
                     # recomputed outcome matches the frame exactly. status:
                     # 1 = correct, 2 = overissuance, 3 = underissuance
                     # (4 = ineligible never occurs in the public files).
                     "rawben", "benefit_amount_FS", "status",
                     # child support expense (QC FSCSEXP): shipped so the
                     # workbook's CHILD_SUPPORT_EXPENSES input column can
                     # demo the exclusion-state ask (2026-08-21); the
                     # munging already standardizes exclusion-state records
                     # to the deduction treatment, so these amounts are
                     # reflected in rawcsded for those rows
                     "fscsexp"), names(w))
out <- w[, c(keys, extra, pf$features), drop = FALSE]
out$hh_group    <- hh_group_of(w$cert_HH_size_FS_n)
out$hh_size_raw <- suppressWarnings(as.numeric(as.character(w$cert_HH_size_FS_n)))
# is_error follows the pipeline's convention: over_threshold != 0, NA = not an error
out$is_error <- as.integer(!is.na(out$over_threshold) & out$over_threshold != 0)

cat(sprintf("errors: %d of %d (%.1f%%)\n",
            sum(out$is_error), nrow(out), 100 * mean(out$is_error)))
print(table(out$hh_group, useNA = "ifany"))
dir.create(dirname(OUT), showWarnings = FALSE, recursive = TRUE)
write.csv(out, OUT, row.names = FALSE)
cat(sprintf("wrote %s (%d rows, %d cols)\n", OUT, nrow(out), ncol(out)))
