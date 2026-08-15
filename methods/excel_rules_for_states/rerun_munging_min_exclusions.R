# Re-run the munging script with row exclusions minimised, writing the result
# into this folder instead of the repo root.
#
# OUTCOME (2026-07-29): this experiment was run and the answer was that the
# munging script's existing exclusions are correct. Nothing was changed in the
# pipeline as a result. Kept as the reproducible record of the check, NOT as a
# recommended path. In short:
#
#   * relaxing the AMTERR reconciliation filter added 19,095 rows carrying
#     12,782 apparent errors, and both are artefacts. The rows are exactly the
#     ones where the two benefit figures and the reported error amount
#     contradict each other, and the error label is derived from that same
#     contradiction (67% "error rate" against 11% elsewhere; reported error
#     amount is 0 on 59% of them while the benefit figures differ by a median
#     $93). The pre-QC restoration also fails on them: the benefit recomputed
#     from the restored fields misses RAWBEN by a median $51 against $0 on the
#     rows that pass the filter, with 29.3% landing within $5 against 95.5%,
#     and 50.0% off by more than $50 against 2.1%. So they have an
#     untrustworthy label AND unreconstructed features. (The
#     correctednotes == "no_change" share does NOT discriminate: 64.5% vs
#     63.5%. Most cases have no error element to correct.)
#   * FY2020 and FY2021 are not to be used at all (see below).
#   * the RENT/UTIL relaxation affects 0 rows.
#
# Verification worth keeping: on the six years both frames share, the rows that
# pass the AMTERR filter reproduce the repo frame's error count exactly, year by
# year, zero mismatches. Relaxing the filter changes nothing about existing
# rows; it only adds rows.
#
# The real script is NOT edited or forked. It is read, a small set of explicit
# substitutions is applied to its text, each one printed, and the result is
# evaluated. That way this stays honest about exactly what was changed and
# cannot silently drift from the pipeline.
#
#   Rscript custom_one_off/snap_dashboard/rerun_munging_min_exclusions.R
#
# Writes:
#   custom_one_off/snap_dashboard/.frames/reg_model_data_minexcl.rds
#   custom_one_off/snap_dashboard/.frames/final_minexcl.rds     (intermediate)
#
# The repo's reg_model_data.rds and final.rds are left untouched; the script
# refuses to run if the substitutions that redirect the saves do not apply.
#
# WHAT IS RELAXED
#   1. the AMTERR reconciliation filter (|absbendiff - AMTERR| <= 5) no longer
#      drops rows; it records `amterr_reconciles` instead.
#   2. NA RENT/UTIL are zero-filled with a `shelter_fields_imputed` flag rather
#      than dropped, matching what the script already does for the optional
#      deduction fields. (Measured: this affects 0 rows.)
#
# WHAT IS NOT RELAXED, AND WHY
#   FY2020 and FY2021 stay excluded (exclude_2020_2021 remains TRUE). Those two
#   years are not to be used: the data is poor and misleading, and the practices
#   states used were qualitatively different, so pooling them with FY2017-19 and
#   FY2022-24 would mix eras rather than add data. Decided 2026-07-29.
#
#   Because FY2020/21 are out, the FOURTH exclusion below is moot in practice.
#   It was missed when this runner was first written and is
#   line ~367, `mydata <- mydata[mydata$BENMAX == mydata$rawbenmax, ]`.
#   Measured by measure_benmax_filter.R, it drops 9,456 rows, all of them FY2021
#   and 100% of that year, because FY2021's BENMAX carries the pandemic 15%
#   boost that the additional_data lookup does not (BENMAX/rawbenmax is
#   1.147-1.151 on every dropped row). It removes nothing from any other year.
#   With FY2021 excluded by the decision above, it has nothing left to remove.
#
#   Alaska, Hawaii, Guam and the Virgin Islands stay excluded. The
#   max-allotment and standard-deduction lookups in additional_data/ are keyed
#   by year and household size only, i.e. they hold the 48-state values, so
#   those four would get the wrong maximum benefit and therefore a wrong
#   rawben_rel_max. Keeping them would add rows carrying broken features, which
#   is not the same thing as keeping data.

FOLDER  <- normalizePath(".", winslash = "/")
SCRIPT  <- "1_data_munging_and_raw_variable_reconstruction_for_using_public_qc_data.R"
OUT_DIR <- file.path(FOLDER, "custom_one_off/snap_dashboard/.frames")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
MINEXCL_FINAL <- file.path(OUT_DIR, "final_minexcl.rds")
MINEXCL_RDS   <- file.path(OUT_DIR, "reg_model_data_minexcl.rds")

if (!file.exists(SCRIPT))
  stop("run from the snap_qc repo root; ", SCRIPT, " not found")

src <- readLines(SCRIPT, warn = FALSE)
txt <- paste(src, collapse = "\n")

subs <- list(
  list(
    what = "stop dropping rows whose AMTERR does not reconcile with |RAWBEN - FSBEN|",
    from = "mydata <- mydata[abs(mydata$absbendiff - mydata$AMTERR) <= 5, ]",
    to = paste(
      "mydata$amterr_reconciles <- !is.na(mydata$absbendiff) & !is.na(mydata$AMTERR) &",
      "  abs(mydata$absbendiff - mydata$AMTERR) <= 5   # MINEXCL: flag, do not drop",
      "cat(\"MINEXCL: kept\", sum(!mydata$amterr_reconciles), \"rows that the\",",
      "    \"AMTERR reconciliation filter would have dropped\\n\")",
      sep = "\n")
  ),
  list(
    what = "zero-fill NA RENT/UTIL instead of dropping those rows",
    from = paste("mydata <- mydata %>%",
                 "  filter(!is.na(RENT), !is.na(UTIL))", sep = "\n"),
    to = paste(
      "mydata$shelter_fields_imputed <- is.na(mydata$RENT) | is.na(mydata$UTIL)   # MINEXCL",
      "cat(\"MINEXCL: zero-filled RENT/UTIL on\", sum(mydata$shelter_fields_imputed),",
      "    \"rows instead of dropping them\\n\")",
      "mydata$RENT[is.na(mydata$RENT)] <- 0",
      "mydata$UTIL[is.na(mydata$UTIL)] <- 0",
      sep = "\n")
  ),
  list(
    what = "redirect the intermediate save away from the repo",
    from = "saveRDS(mydata, paste0(folder, \"final.rds\"))",
    to   = "saveRDS(mydata, MINEXCL_FINAL)   # MINEXCL"
  ),
  list(
    what = "read the intermediate back from the redirected path",
    from = "df <- readRDS(paste0(folder, \"final.rds\"))",
    to   = "df <- readRDS(MINEXCL_FINAL)   # MINEXCL"
  ),
  list(
    what = "redirect the modelling frame away from the repo's reg_model_data.rds",
    from = "saveRDS(reg_model_data, \"reg_model_data.rds\")",
    to   = "saveRDS(reg_model_data, MINEXCL_RDS)   # MINEXCL"
  )
)

cat("SUBSTITUTIONS\n")
for (s in subs) {
  n <- length(gregexpr(s$from, txt, fixed = TRUE)[[1]])
  if (n != 1 || gregexpr(s$from, txt, fixed = TRUE)[[1]][1] == -1)
    stop("substitution '", s$what, "' matched ", max(n, 0),
         " times, expected exactly 1. The munging script has changed; ",
         "update this runner rather than guessing.")
  txt <- sub(s$from, s$to, txt, fixed = TRUE)
  cat(sprintf("  ok  %s\n", s$what))
}
if (grepl('saveRDS(reg_model_data, "reg_model_data.rds")', txt, fixed = TRUE))
  stop("refusing to run: the repo's reg_model_data.rds is still a save target")
if (!grepl("exclude_2020_2021 <- TRUE", txt, fixed = TRUE))
  stop("refusing to run: FY2020/FY2021 must stay excluded")
cat("  ok  FY2020 and FY2021 remain excluded (checked, not assumed)
")

cat(sprintf("\noutputs\n  %s\n  %s\n\n", MINEXCL_FINAL, MINEXCL_RDS))
cat("running the munged pipeline with exclusions minimised ...\n")
flush.console()

t0 <- Sys.time()
env <- new.env(parent = globalenv())
assign("MINEXCL_FINAL", MINEXCL_FINAL, envir = env)
assign("MINEXCL_RDS", MINEXCL_RDS, envir = env)
eval(parse(text = txt), envir = env)
cat(sprintf("\nelapsed: %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))

d <- get("reg_model_data", envir = env)
cat(sprintf("minimal-exclusion frame: %d rows, %d cols\n", nrow(d), ncol(d)))
