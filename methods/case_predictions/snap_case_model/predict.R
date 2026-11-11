# SNAP payment error model: score a state's cases with the pre-trained model.
#
# From a command prompt, in this folder:
#   Rscript predict.R --state WA --input my_cases.csv                 writes my_cases_predictions.csv
#   Rscript predict.R --state WA --input my_cases.csv --output out.csv
#   Rscript predict.R --check                                          confirms this install reproduces the example
#
# From R:
#   source("predict.R")
#   pred <- predict_error(read_cases("my_cases.csv"), state = "Washington")
#
# The input is one row per case with the columns in templates/input_columns.csv (the same
# columns as Step 2 of the state's Excel workbook; extra columns are ignored). The output
# has one row per case, highest expected error dollars first. See README.md.

.pkg_dir <- function() {   # this file's folder, whether sourced or run with Rscript
  for (i in rev(seq_len(sys.nframe()))) { o <- sys.frame(i)$ofile
    if (!is.null(o)) return(if (file.exists(o)) dirname(normalizePath(o)) else getwd()) }   # getwd(): source(..., chdir = TRUE)
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (length(f)) return(dirname(normalizePath(f[1])))
  getwd()
}
PKG_DIR <- .pkg_dir()
for (f in c("build_features.R", "read_input.R", "model.R", "explain.R")) source(file.path(PKG_DIR, "R", f))

# read a case file, keeping CASE_ID exactly as written (000123 stays 000123)
read_cases <- function(path) {
  hdr <- names(read.csv(path, nrows = 1, check.names = FALSE, fileEncoding = "UTF-8-BOM"))
  cls <- if ("CASE_ID" %in% hdr) c(CASE_ID = "character") else NA
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, colClasses = cls, fileEncoding = "UTF-8-BOM")
}

.model_cache <- new.env()
get_model <- function() {
  if (is.null(.model_cache$m)) .model_cache$m <- load_model(file.path(PKG_DIR, "model", "snap_case_model.rds"))
  .model_cache$m
}

# cases: a data frame of the input columns. state: a name or two-letter code.
# Returns one row per case, ordered by priority_rank.
predict_error <- function(cases, state, quiet = FALSE, .allow_empty_columns = FALSE) {
  m <- get_model()
  state <- resolve_state(state, m$states)
  df <- check_input(as.data.frame(cases, stringsAsFactors = FALSE), .allow_empty_columns)
  tables <- load_tables(file.path(PKG_DIR, "lookups"))
  feat <- build_features(df, state, tables)

  # the size model starts from the recomputed benefit, as in training; where the inputs
  # needed to recompute it are blank, the issued benefit stands in, and failing that the
  # benefit recomputed with the blanks counted as 0
  ben <- attr(feat, "recomputed_benefit"); n_issued <- sum(is.na(ben))
  ben[is.na(ben)] <- df$ORIGINAL_BENEFIT_AMOUNT[is.na(ben)]; n_zero <- sum(is.na(ben))
  ben[is.na(ben)] <- attr(feat, "recomputed_benefit_blanks_as_0")[is.na(ben)]
  s <- score_cases(feat, state, ben, m)

  # fiscal years the tables or the training do not cover
  fy <- sort(unique(df$REVIEW_FISCAL_YEAR)); yw <- character(0)
  last_tab <- min(max(tables$yd$year), max(tables$ma$year), max(tables$sd$year))
  sua <- .state_row(tables$sua, state); last_sua <- suppressWarnings(max(as.numeric(names(sua))[sua > 0]))
  if (any(fy > last_tab)) yw <- c(yw, sprintf("FY%s is after the federal tables in lookups/ (last FY%d): scores use FY%d's tables; add the new year's rows before relying on them",
                                              paste(fy[fy > last_tab], collapse = ", "), last_tab, last_tab))
  if (any(fy > last_sua)) yw <- c(yw, sprintf("lookups/state_sua.csv has no %s utility standard after FY%d, so the utility tier is blank for FY%s cases with utility costs; add the standard",
                                              state, last_sua, paste(fy[fy > last_sua], collapse = ", ")))
  if (any(fy %in% 2020:2021)) yw <- c(yw, "the model was not trained on FY2020-21 cases")
  dollars <- s$p_error * s$size

  lst <- file.path(PKG_DIR, "rules", sprintf("blended_delivery_%s_2022_2024_budget10.csv", gsub(" ", "_", state)))
  why <- if (file.exists(lst)) rules_flagging(feat, read.csv(lst, stringsAsFactors = FALSE)) else NULL

  out <- data.frame(
    CASE_ID = df$CASE_ID,
    priority_rank = rank(-dollars, ties.method = "first", na.last = TRUE),
    expected_error_dollars = round(dollars, 2),
    p_error = round(s$p_error, 4),
    error_size_if_error = round(s$size, 2),
    rank_by_p_error = rank(-s$p_error, ties.method = "first", na.last = TRUE),
    hh_group = hh_group_of(df$HOUSEHOLD_SIZE),
    issued_benefit = df$ORIGINAL_BENEFIT_AMOUNT,
    inputs_blank = rowSums(is.na(feat[m$base_inputs])),
    list_rules_flagging = if (is.null(why)) "" else why$ranks,
    stringsAsFactors = FALSE)
  out <- out[order(out$priority_rank), ]; rownames(out) <- NULL

  if (!quiet) {
    say <- function(...) cat(sprintf(...), "\n", sep = "")
    say("Scored %d cases for %s (fiscal year %s).", nrow(out), state, paste(sort(unique(df$REVIEW_FISCAL_YEAR)), collapse = ", "))
    bb <- attr(feat, "bbce_by_year")
    say("  Broad-based categorical eligibility read from the file: %s",
        paste(sprintf("FY%s %s", names(bb), ifelse(bb == 1, "yes", "no")), collapse = "; "))
    nb <- colSums(is.na(feat[m$base_inputs])); nb <- nb[nb > 0]
    if (length(nb)) say("  Blank model inputs (the model scores these as missing): %s",
                        paste(sprintf("%s %d", names(nb), nb), collapse = "; "))
    for (w in c(yw, attr(df, "warnings"))) say("  Check: %s", w)
    if (n_issued) say("  %d case(s) lacked an input for recomputing the benefit, so the size model started from the issued benefit%s.",
                      n_issued, if (n_zero) sprintf(" (for %d with no issued benefit either, from the benefit recomputed with blanks counted as 0)", n_zero) else "")
    say("  Mean error probability %.3f; mean expected error $%.2f per case (FY2024 dollars).", mean(s$p_error), mean(dollars, na.rm = TRUE))
    if (is.null(why)) say("  No rule list for %s in rules/: list_rules_flagging left blank.", state)
    else say("  list_rules_flagging: ranks of the %d rules in rules/%s (rules using count_divisible_by_100 are not checked).",
             why$n_rules, basename(lst))
  }
  out
}

# ---- command line -------------------------------------------------------------------
.main <- function(args) {
  arg <- function(name) { i <- match(paste0("--", name), args); if (is.na(i) || i == length(args)) NULL else args[i + 1] }
  if ("--help" %in% args || !length(args)) {
    cat("Usage:\n  Rscript predict.R --state WA --input my_cases.csv [--output predictions.csv]\n  Rscript predict.R --check\n")
    return(invisible())
  }
  if ("--check" %in% args) {
    ex <- file.path(PKG_DIR, "example")
    info <- read.csv(file.path(ex, "example_info.csv"), stringsAsFactors = FALSE)
    got <- predict_error(read_cases(file.path(ex, "example_input.csv")), info$state, quiet = TRUE)
    want <- read.csv(file.path(ex, "expected_predictions.csv"), stringsAsFactors = FALSE)
    num <- c("priority_rank", "expected_error_dollars", "p_error", "error_size_if_error", "rank_by_p_error")
    ok <- nrow(got) == nrow(want) && identical(as.character(got$CASE_ID), as.character(want$CASE_ID)) &&
      all(abs(as.matrix(got[num]) - as.matrix(want[num])) <= 0.011) &&
      identical(ifelse(is.na(got$list_rules_flagging), "", got$list_rules_flagging),
                ifelse(is.na(want$list_rules_flagging), "", want$list_rules_flagging))
    cat(if (ok) sprintf("check passed: %d example cases (%s) reproduce the expected predictions\n", nrow(got), info$state)
        else "CHECK FAILED: this install does not reproduce the example predictions; see the end of README.md\n")
    if (!ok) quit(status = 1)
    return(invisible())
  }
  state <- arg("state"); input <- arg("input")
  if (is.null(state) || is.null(input)) stop("give both --state and --input (or --check); see --help", call. = FALSE)
  output <- arg("output"); if (is.null(output)) output <- sub("(\\.csv)?$", "_predictions.csv", input, ignore.case = TRUE)
  pred <- predict_error(read_cases(input), state)
  write.csv(pred, output, row.names = FALSE, na = "")
  cat(sprintf("Wrote %s\n", output))
}
if (sys.nframe() == 0L) .main(commandArgs(trailingOnly = TRUE))
