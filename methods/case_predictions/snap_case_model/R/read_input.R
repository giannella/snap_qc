# Read and check a state's case file before scoring.

STATE_CODES <- c(setNames(datasets::state.name, datasets::state.abb), DC = "District of Columbia")

# "WA", "wa", "Washington" or "washington" -> "Washington"
resolve_state <- function(state, known) {
  s <- trimws(state)
  if (toupper(s) %in% names(STATE_CODES)) s <- STATE_CODES[[toupper(s)]]
  hit <- known[tolower(known) == tolower(s)]
  if (!length(hit)) stop(sprintf("state '%s' is not one the model was trained on. Use a name or two-letter code from: %s",
                                 state, paste(known, collapse = ", ")), call. = FALSE)
  hit
}

.as_number <- function(v, col) {
  if (is.numeric(v)) return(as.numeric(v))
  if (is.logical(v)) return(as.numeric(v))
  s <- toupper(trimws(as.character(v)))
  s[s == ""] <- NA
  map <- c("TRUE" = 1, "FALSE" = 0, "Y" = 1, "N" = 0, "YES" = 1, "NO" = 0)
  out <- suppressWarnings(as.numeric(gsub("[$,]", "", s)))
  yn <- !is.na(s) & s %in% names(map); out[yn] <- map[s[yn]]
  bad <- which(!is.na(s) & is.na(out))
  if (length(bad)) stop(sprintf("column %s has values that are not numbers, e.g. '%s' in row(s) %s",
                                col, v[bad[1]], paste(head(bad, 5), collapse = ", ")), call. = FALSE)
  out
}

# What to do when a state does not record a column, as measured on FY2024 public QC cases in all
# 49 states: the overlap of each state's top 10% with its full-input ranking when one column is
# blanked, or filled with 0 (snap_qc repo, methods/case_predictions/case_model_validation/column_effects_fy2024.csv). The model saw almost no blanks in training (under 0.1% of cases
# for any input), so a column left blank throughout distorts the scores (median overlap 0.39-0.88).
# For these columns most cases are 0 anyway, and filling 0 keeps the median overlap at 0.94-1.00:
ZERO_IF_NOT_RECORDED <- c("NUM_ELDERLY", "NUM_DISABLED", "NUM_ABAWD", "MARRIED_FLAG", "EXPEDITED", "HOMELESS_FLAG",
                          "MEDICAL_DEDUCTION", "DEPENDENT_CARE_DEDUCTION", "CHILD_SUPPORT_EXPENSES", "HOMELESS_DEDUCTION")
# the rest carry the ranking (median overlap 0.32-0.92 even with 0 filled); without them the model should not be used
ESSENTIAL <- c("REVIEW_FISCAL_YEAR", "HOUSEHOLD_SIZE", "NUM_CHILDREN", "CATEGORICALLY_ELIGIBLE", "MONTHS_SINCE_CERT",
               "EARNED_INCOME", "UNEARNED_INCOME", "RENT", "UTILITY_COSTS")

# returns the cleaned data frame; stops with a plain message on anything that would
# make the scores wrong, and warns on values that look suspicious
check_input <- function(df, allow_empty_columns = FALSE) {   # allow_empty_columns: testing only
  need <- setdiff(INPUT_COLUMNS, "CASE_ID")
  miss <- setdiff(need, names(df))
  if (length(miss)) stop(sprintf("the input is missing %d column(s): %s.\nSee templates/input_columns.csv for what each column means.",
                                 length(miss), paste(miss, collapse = ", ")), call. = FALSE)
  if (!"CASE_ID" %in% names(df)) df$CASE_ID <- seq_len(nrow(df))
  for (c in need) df[[c]] <- .as_number(df[[c]], c)
  empty <- need[vapply(need, function(c) all(is.na(df[[c]])), TRUE)]
  e1 <- intersect(empty, ESSENTIAL); e2 <- intersect(empty, ZERO_IF_NOT_RECORDED)
  if (!allow_empty_columns && (length(e1) || length(e2))) stop(paste0("some columns are blank for every case.",
    if (length(e1)) sprintf("\n  The model cannot rank cases without: %s.", paste(e1, collapse = ", ")) else "",
    if (length(e2)) sprintf("\n  If your system does not record %s, fill the column with 0 (README.md, 'Columns you do not have').",
                            paste(e2, collapse = ", ")) else ""), call. = FALSE)
  if (anyNA(df$REVIEW_FISCAL_YEAR)) stop("REVIEW_FISCAL_YEAR is blank in some rows; every case needs a fiscal year", call. = FALSE)
  if (anyNA(df$HOUSEHOLD_SIZE)) stop("HOUSEHOLD_SIZE is blank in some rows; every case needs a household size", call. = FALSE)
  warn <- character(0)
  for (c in setdiff(need, "ORIGINAL_BENEFIT_AMOUNT")) { s <- mean(is.na(df[[c]]))
    if (s > 0.01) warn <- c(warn, sprintf("%.0f%% of cases have a blank %s; their scores are less reliable (see inputs_blank)", 100 * s, c)) }
  chk <- function(bad, msg) if (any(bad, na.rm = TRUE)) warn <<- c(warn, sprintf("%d case(s): %s", sum(bad, na.rm = TRUE), msg))
  chk(df$REVIEW_FISCAL_YEAR < 2017, "REVIEW_FISCAL_YEAR before 2017, earlier than the tables")
  chk(df$HOUSEHOLD_SIZE < 1 | df$HOUSEHOLD_SIZE > 20 | df$HOUSEHOLD_SIZE != round(df$HOUSEHOLD_SIZE), "HOUSEHOLD_SIZE not a whole number from 1 to 20")
  for (c in c("MARRIED_FLAG", "EXPEDITED", "CATEGORICALLY_ELIGIBLE", "HOMELESS_FLAG"))
    chk(!df[[c]] %in% c(0, 1, NA), paste(c, "not 0 or 1"))
  for (c in c("EARNED_INCOME", "UNEARNED_INCOME", "MEDICAL_DEDUCTION", "DEPENDENT_CARE_DEDUCTION",
              "CHILD_SUPPORT_EXPENSES", "HOMELESS_DEDUCTION", "RENT", "UTILITY_COSTS", "ORIGINAL_BENEFIT_AMOUNT"))
    chk(df[[c]] < 0, paste(c, "negative"))
  chk(df$NUM_CHILDREN + df$NUM_ELDERLY > df$HOUSEHOLD_SIZE, "more children plus elderly members than HOUSEHOLD_SIZE")
  if (anyDuplicated(df$CASE_ID)) warn <- c(warn, sprintf("%d repeated CASE_ID value(s)", sum(duplicated(df$CASE_ID))))
  attr(df, "warnings") <- warn
  df
}
