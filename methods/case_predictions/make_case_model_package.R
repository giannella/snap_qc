# Assemble the state-facing model package from the repo.
#   Rscript methods/case_predictions/make_case_model_package.R     (from the repo root)
# Writes methods/case_predictions/dist/snap_case_model/ and dist/snap_case_model_<version>.zip.
# The source (code, guide, model card, templates) is methods/case_predictions/snap_case_model/;
# this script adds the model file, the lookup tables, the state rule lists and a worked example.
# Env: CM_BUNDLE (default s46/out_f1109/deploy_B_bundle.rds), CM_VERSION (default v2.8.0),
#      CM_EXAMPLE_STATE (default Washington), PYTHON.
suppressMessages(library(xgboost))
source("methods/case_predictions/case_model_dev_helpers.R")
SRC <- "methods/case_predictions/snap_case_model"
BUNDLE <- Sys.getenv("CM_BUNDLE", "methods/case_predictions/s46/out_f1109/deploy_B_bundle.rds")
VERSION <- Sys.getenv("CM_VERSION", "v2.8.0")
EX_STATE <- Sys.getenv("CM_EXAMPLE_STATE", "Washington")
DIST <- "methods/case_predictions/dist"; OUT <- file.path(DIST, "snap_case_model")
say <- function(...) cat(sprintf(...), "\n", sep = "")

unlink(OUT, recursive = TRUE); dir.create(OUT, recursive = TRUE)
# code, guide, templates
for (f in list.files(SRC, recursive = TRUE)) {
  dir.create(dirname(file.path(OUT, f)), showWarnings = FALSE, recursive = TRUE)
  stopifnot(file.copy(file.path(SRC, f), file.path(OUT, f)))
}

# model: the deploy bundle's ten seed pairs and what is needed to score with them; the
# tuning tables and the FNS-rate levels (not applied by this package) are left out
b <- readRDS(BUNDLE)
stopifnot(b$frame == "f1109", length(b$models) == 10, !b$pct)
m <- b[c("model", "inputs", "base_inputs", "states", "pct", "train_years", "size_offset_glm", "cpi_by_year",
         "models", "frame", "frame_sha256", "staging_commit", "built")]
m$package_version <- VERSION
m$source_bundle_sha256 <- digest::digest(file = BUNDLE, algo = "sha256")
m$xgboost_version <- as.character(packageVersion("xgboost"))
m$r_version <- R.version.string
dir.create(file.path(OUT, "model"))
mf <- file.path(OUT, "model", "snap_case_model.rds"); saveRDS(m, mf)
writeLines(c(sprintf("package %s", VERSION),
             sprintf("snap_case_model.rds sha256 %s", digest::digest(file = mf, algo = "sha256")),
             sprintf("built from %s (sha256 %s), frame %s (sha256 %s), commit %s",
                     basename(BUNDLE), m$source_bundle_sha256, m$frame, m$frame_sha256, m$staging_commit),
             sprintf("training years FY%s", paste(m$train_years, collapse = ", ")),
             sprintf("built with %s, xgboost %s", m$r_version, m$xgboost_version)),
           file.path(OUT, "model", "model_info.txt"))

# lookups: the repo's federal and state tables, plus the per-state options from states.py
dir.create(file.path(OUT, "lookups"))
for (f in c("year_data.csv", "standard_deductions.csv", "max_allotments.csv", "state_sua.csv", "standard_medical_deductions.csv"))
  stopifnot(file.copy(file.path("additional_data", f), file.path(OUT, "lookups", f)))
opts <- state_options_from_states_py()
stopifnot(setequal(opts$state_name, m$states))
write.csv(opts[opts$std_ded_offset_col != "" | opts$prior_year_tables, ], file.path(OUT, "lookups", "state_options.csv"), row.names = FALSE)

# rules: the state delivery lists (the lists behind the Excel workbooks)
dir.create(file.path(OUT, "rules"))
lists <- sprintf("state_delivery_lists/blended_delivery_%s_2022_2024_budget10.csv", gsub(" ", "_", m$states))
stopifnot(all(file.exists(lists)))
for (f in lists) stopifnot(file.copy(f, file.path(OUT, "rules", basename(f))))

# example: one state's FY2024 public QC cases in the input layout. These cases were in
# the training data, so the example shows how to run the model, not how well it does.
reg <- readRDS("additional_data/reg_model_data.rds")
stopifnot(digest::digest(file = "additional_data/reg_model_data.rds", algo = "sha256") == m$frame_sha256)
w <- reg[as.character(reg$state) == EX_STATE & as.character(reg$fiscal_year) == "2024", ]
dir.create(file.path(OUT, "example"))
write.csv(frame_to_input(w), file.path(OUT, "example", "example_input.csv"), row.names = FALSE, na = "")
write.csv(data.frame(state = EX_STATE, source = "public SNAP QC cases, FY2024 (in the model's training data)", n_cases = nrow(w)),
          file.path(OUT, "example", "example_info.csv"), row.names = FALSE)
source(file.path(OUT, "predict.R"), chdir = TRUE)
ex <- predict_error(read.csv(file.path(OUT, "example", "example_input.csv"), stringsAsFactors = FALSE), EX_STATE)
write.csv(ex, file.path(OUT, "example", "expected_predictions.csv"), row.names = FALSE, na = "")

# zip
zf <- normalizePath(file.path(DIST, sprintf("snap_case_model_%s.zip", VERSION)), mustWork = FALSE); unlink(zf)
zip::zip(zf, "snap_case_model", root = DIST)
say("package -> %s (%d files); zip -> %s (%.1f MB)", OUT, length(list.files(OUT, recursive = TRUE)), zf, file.size(zf) / 2^20)
