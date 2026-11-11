# Shared by make_case_model_package.R and validate_case_model_package.R (repo-side only;
# not shipped). Run from the repo root.

# The frame's rows in the package's input layout: an R copy of the workbook's raw_frame()
# (methods/excel_rules_for_states/make_input_workbook.py). Dollar amounts are the
# reconstructed pre-QC-review values in each case's own fiscal-year dollars; blanks the
# frame zero-imputes are written as zeros, and EXPEDITED, HOMELESS_FLAG and
# MONTHS_SINCE_CERT stay blank where the frame has NA.
frame_to_input <- function(w) {
  num <- function(x) suppressWarnings(as.numeric(as.character(x)))
  z <- function(x) { x <- num(x); x[is.na(x)] <- 0; x }
  lg <- function(x) if (is.logical(x)) as.numeric(x) else if (is.factor(x) || is.character(x)) {
    v <- as.character(x); ifelse(is.na(v), NA_real_, as.numeric(v == "TRUE")) } else num(x)
  data.frame(
    CASE_ID = paste0("SAMPLE-", w$fiscal_year, "-", trimws(as.character(w$hhldno))),
    REVIEW_FISCAL_YEAR = z(as.character(w$fiscal_year)),
    HOUSEHOLD_SIZE = z(w$cert_HH_size_FS_n),
    NUM_CHILDREN = z(w$fsnkid), NUM_ELDERLY = z(w$fsnelder), NUM_DISABLED = z(w$fsndis),
    NUM_ABAWD = z(w$count_abawd),
    MARRIED_FLAG = z(w$married),
    EXPEDITED = lg(w$expedited_i),
    CATEGORICALLY_ELIGIBLE = as.numeric(!is.na(num(w$cat_elig)) & num(w$cat_elig) >= 1),
    HOMELESS_FLAG = lg(w$homeless),
    MONTHS_SINCE_CERT = num(w$months_since_cert_n),
    EARNED_INCOME = z(w$rawearn_nominal), UNEARNED_INCOME = z(w$rawunearn_nominal),
    MEDICAL_DEDUCTION = z(w$rawmedded_nominal), DEPENDENT_CARE_DEDUCTION = z(w$rawdepded_nominal),
    CHILD_SUPPORT_EXPENSES = z(w$rawcsded_nominal), HOMELESS_DEDUCTION = z(w$rawhomeless_ded_nominal),
    RENT = z(w$rawrent_nominal), UTILITY_COSTS = z(w$rawutil_nominal),
    ORIGINAL_BENEFIT_AMOUNT = z(w$rawben),
    stringsAsFactors = FALSE)
}

# the frame's own model inputs, coerced to numbers as the training code's prep_features does
frame_features <- function(w, base_inputs) {
  out <- lapply(base_inputs, function(v) { x <- w[[v]]
    if (is.logical(x)) as.numeric(x) else if (is.factor(x) || is.character(x)) as.numeric(as.character(x) == "TRUE") else as.numeric(x) })
  as.data.frame(setNames(out, base_inputs))
}

# per-state options from the workbook's states.py (the single source): Illinois's
# standard-deduction offset and the states whose benefit chain tries last year's tables
state_options_from_states_py <- function(py = Sys.getenv("PYTHON", "python")) {
  code <- paste("import sys, csv; sys.path.insert(0, 'methods/excel_rules_for_states'); import states",
                "w = csv.writer(sys.stdout, lineterminator='\\n'); w.writerow(['state_name', 'std_ded_offset_col', 'prior_year_tables'])",
                "[w.writerow([s['name'], s.get('std_ded_offset_col') or '', bool(s.get('prior_year_tables'))]) for s in states.STATES.values()]",
                sep = "\n")
  f <- tempfile(fileext = ".py"); writeLines(code, f)
  txt <- system2(py, f, stdout = TRUE); stopifnot(is.null(attr(txt, "status")))
  o <- read.csv(text = txt, stringsAsFactors = FALSE, colClasses = "character")
  o$prior_year_tables <- toupper(o$prior_year_tables) == "TRUE"
  o
}
