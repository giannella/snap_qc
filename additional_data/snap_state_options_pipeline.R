# ==============================================================================
# SNAP State Options Report -> tidy dataset
# ==============================================================================
# Parses FNS "SNAP State Options Report" PDFs, in which each jurisdiction gets
# a page (sometimes two) laid out as a two-column table:
#
#     Policy Option            |  Option Selection
#     -------------------------|--------------------------------
#     Program administration   |  State administered
#     Reporting requirements   |  Simplified reporting only
#     ...
#
# Strategy: use word-level coordinates from pdf_data() to reconstruct rows and
# columns geometrically. pdf_text() flattens the two columns into one stream and
# is not recoverable when option selections wrap across lines.
#
# Usage:
#   source("snap_state_options_pipeline.R")
#   res <- run_pipeline(".")             # reads the sources listed in EDITIONS
#   res$wide; res$long; res$diagnostics
#
# Requires poppler >= 0.73 for pdf_data() (check with pdftools::poppler_config()).
# ==============================================================================

library(pdftools)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(tibble)
library(readr)

# ------------------------------------------------------------------------------
# 0. The corpus
# ------------------------------------------------------------------------------

# Sources are listed rather than discovered by globbing, because the file names
# do not agree on what their year means and one edition is available from two
# sources of unequal quality. `year` is always the edition's PUBLICATION year,
# which is the as-of year plus one.
#
# Paths are relative to the directory passed to run_pipeline(). A .pdf is read by
# parse_report(); a .csv is read by parse_legacy_csv() and is expected in the
# long state/year/category/status form.
EDITIONS <- tribble(
  ~year, ~path,                                                    ~note,
  2016L, "state options reports/12-State_Options_Oct1_2015.pdf",   "12th ed, as of Oct 1 2015, published Apr 15 2016. Preferred over snap_options_2016.csv, which is the same edition with ~24 cells carrying text from a neighbouring row",
  2017L, "state options reports/state_options_revised_2016.pdf",   "13th ed, as of Oct 1 2016; named for its as-of year. Preferred over snap_options_2017.csv, which is the same edition with page footers and neighbouring rows folded into ~76 cells",
  2018L, "snap_options_2018.csv",                                  "14th ed, as of Oct 1 2017; covers 45 of 53 jurisdictions (stops at Utah)",
  2023L, "state options reports/snap_stateOptionsReport_2023.pdf", "15th ed, as of Oct 1 2022",
  2024L, "state options reports/snap-stateOptionsReport_2024.pdf", "16th ed, as of Oct 1 2023",
  2025L, "state options reports/snap-stateOptionsReport_2025.pdf", "17th ed"
)

# Present but not read. snap_stateOptionsReport_2018.pdf (14th ed) has no
# per-jurisdiction table at all: it discusses each option in prose and lists the
# jurisdictions that chose it, one page per option, which needs a different
# parser. snap_options_2016.csv and snap_options_2017.csv are superseded by the
# PDFs of the same editions, see the notes above.
#
# Editions are identified from the cover page, not the file name: the 12th is
# "Options as of October 1, 2015" published April 2016, and the 13th is "as of
# October 1, 2016" revised August 2017. That is the source of the off-by-one
# between the CSV file names (publication year) and state_options_revised_2016.pdf
# (as-of year).

# ------------------------------------------------------------------------------
# 1. Reference vectors
# ------------------------------------------------------------------------------

JURISDICTIONS <- c(
  "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
  "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia",
  "Guam", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas",
  "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan",
  "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada",
  "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina",
  "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania",
  "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas",
  "Utah", "Vermont", "Virgin Islands", "Virginia", "Washington",
  "West Virginia", "Wisconsin", "Wyoming"
)

# Ordered crosswalk: first matching pattern wins, so put specific before general.
# Patterns are matched against the *normalized* label (lowercase, punctuation and
# footnote markers stripped). Extend this when a new edition renames an option --
# run_pipeline() reports unmatched labels rather than silently dropping them.
# Order matters twice over. Waivers come first because the 17th edition's waiver
# table restates option names ("Shorten Certification Periods for Certain
# Households Waiver" is not the certification-period option). Within the options,
# cert_periods precedes reporting because the 15th edition names that option
# "Certification Periods for Households With Simplified Reporting Requirements",
# which the reporting pattern would otherwise claim.
OPTION_CROSSWALK <- tribble(
  ~variable,               ~pattern,
  # -- waivers (17th edition adds a per-jurisdiction waiver table) -------------
  "abawd_waiver",          "(abawd|able.?bodied|time limit).*waiver",
  "waiver_address_change", "change of address.*waiver",
  "waiver_on_demand_interview", "unscheduled interview|on demand.*waiver",
  "waiver_early_denial",   "early denial|before the 30th day",
  "waiver_pre_release",    "pre.?release application",
  "waiver_recert_interview", "recertification interview",
  "waiver_reinstatement",  "reinstate eligibility|reinstatement waiver",
  "waiver_shorten_cert",   "shorten certification period",
  "waiver_telephonic_signature", "telephonic signature",
  "waiver_text_alerts",    "text message alert",
  # -- policy options ---------------------------------------------------------
  "program_admin",         "program admin|state or county admin",
  # 11th-14th editions call this "Simplified Reporting - Certification Length".
  "cert_periods",          "certification period|certification length",
  "reporting",             "^reporting requirement|^reporting|change reporting",
  "self_employment",       "self.?employment",
  "suas",                  "standard utility allowance|utility expense|\\bsua",
  "ineligible_pre_prwora", "pre.?prwora|pre.?welfare reform",
  "ineligible_post_prwora","post.?prwora|post.?welfare reform",
  "child_support_disq",    "child support.*(disqualif|cooperat)|cooperation with child support",
  "child_support",         "child support",
  "comparable_disq",       "comparable disqualif",
  "drug_felony",           "drug felon|felony drug",
  # The 16th edition splits work requirements into two rows; the household one
  # has no 15th-edition counterpart, so it needs its own variable and has to be
  # matched before the general work-requirements pattern.
  "work_req_household",    "work requirement.*(entire household|disqualify the entire)",
  "work_requirements",     "work requirement|general work",
  "snap_et",               "employment and training|\\be&?t\\b",
  "abawd_exemptions",      "(discretionary|percent).*exempt",
  "bbce",                  "broad.?based categorical|\\bbbce",
  "tba",                   "transitional benefit|\\btba\\b",
  "pledge_states",         "pledge state",
  # The 16th and 17th editions report ESAP and SMD as separate rows; the 15th
  # reports one combined "Demonstrations" row. Keep all three variables and
  # reconcile them downstream.
  "esap",                  "^esap$",
  "smd",                   "^smd$",
  "demonstrations",        "demonstration|standard medical deduction|elderly simplified",
  "cap",                   "combined application|\\bcap\\b"
)

# The 19 columns of snap_state_options_2023.csv, in that file's order. These are
# the options the 15th edition reports; later editions add options that have no
# counterpart here (see OPTION_CROSSWALK) and are carried in the long file only.
CANONICAL_COLUMNS <- tribble(
  ~variable,                ~column,
  "program_admin",          "ProgramAdministration",
  "reporting",              "ReportingRequirements",
  "cert_periods",           "CertificationPeriods",
  "self_employment",        "SelfEmploymentIncome",
  "suas",                   "SUAs",
  "ineligible_pre_prwora",  "IneligibleNoncitizensPrePRWORA",
  "ineligible_post_prwora", "IneligibleNoncitizensPostPRWORA",
  "child_support",          "ChildSupportPayments",
  "child_support_disq",     "ChildSupportDisqualifications",
  "comparable_disq",        "ComparableDisqualification",
  "drug_felony",            "DrugFelonyDisqualifications",
  "work_requirements",      "WorkRequirements",
  "snap_et",                "SNAPETPrograms",
  "abawd_waiver",           "ABAWDTimeLimitWaiver",
  "abawd_exemptions",       "ABAWDDiscretionaryExemptions",
  "bbce",                   "BBCE",
  "tba",                    "TBA",
  "demonstrations",         "DemonstrationsElderlyDisability",
  "cap",                    "CAP"
)

# ------------------------------------------------------------------------------
# 2. Text normalization helpers
# ------------------------------------------------------------------------------

normalize_label <- function(x) {
  x %>%
    str_replace_all("\\s+", " ") %>%
    str_remove_all("[\u00b9\u00b2\u00b3\u2070-\u209f]") %>%   # superscript footnotes
    str_remove_all("\\s*\\d+\\s*$") %>%                        # trailing footnote digits
    # Punctuation becomes a SPACE, not nothing: editions differ in hyphen and
    # dash use ("Child-Support-Related" in the 15th edition, an en dash in the
    # 16th), and deleting the character would fuse the words on one side only.
    str_replace_all("[^[:alnum:][:space:]&]", " ") %>%
    str_squish() %>%
    str_to_lower()
}

clean_cell <- function(x) {
  x %>%
    str_replace_all("\\s+", " ") %>%
    str_replace_all("(?<=[a-z])-\\s(?=[a-z])", "") %>%  # rejoin hyphen line breaks
    str_squish()
}

# ------------------------------------------------------------------------------
# 3. Geometry: group words into visual lines
# ------------------------------------------------------------------------------

# pdf_data() gives one row per word with x, y (top-left), width, height.
# Words on the same printed line share a y within a tolerance derived from the
# font height, so cluster on y with a running threshold rather than rounding.
group_lines <- function(page, y_tol_frac = 0.6) {
  if (nrow(page) == 0) return(page %>% mutate(line_id = integer()))
  tol <- median(page$height, na.rm = TRUE) * y_tol_frac
  page <- page %>% arrange(y, x)
  line_id <- integer(nrow(page))
  current <- 1L
  anchor <- page$y[1]
  for (i in seq_len(nrow(page))) {
    if (page$y[i] - anchor > tol) {
      current <- current + 1L
      anchor <- page$y[i]
    }
    line_id[i] <- current
  }
  page %>% mutate(line_id = line_id)
}

# ------------------------------------------------------------------------------
# 4. Page-level structure detection
# ------------------------------------------------------------------------------

# The jurisdiction name is printed at the top-left of its page. Continuation
# pages have no name; those are carried forward by parse_report().
detect_jurisdiction <- function(page_lines, top_n_lines = 4) {
  head_text <- page_lines %>%
    filter(line_id <= top_n_lines) %>%
    group_by(line_id) %>%
    summarise(txt = paste(text, collapse = " "), .groups = "drop") %>%
    pull(txt)
  if (length(head_text) == 0) return(NA_character_)

  # Longest name first so "West Virginia" is not matched as "Virginia".
  for (j in JURISDICTIONS[order(-nchar(JURISDICTIONS))]) {
    if (any(str_detect(head_text, fixed(j)))) return(j)
  }
  NA_character_
}

# Find the x coordinate separating the label column from the selection column.
# Preferred: the header row, which is the only line known to contain exactly one
# word-run per column, so the widest gap between its words is the column gutter.
# Editions label the header differently ("Policy Option / Option Selection",
# "Option / Option Selection", "Waiver / Implementation", "Project /
# Implementation"), so match the right-hand word rather than the left.
# Fallback: the widest gap in the distribution of word start positions.
HEADER_PATTERN <- "option selection|^waiver implementation$|^project implementation$"

# A page is a jurisdiction page only if it announces itself as one. Front matter
# discusses the same options in prose and lists State names, so a page cannot be
# admitted on a name match alone. The 15th-17th editions announce it with the
# column header; the 13th edition's per-jurisdiction pages carry no column header
# at all and are announced by the "SNAP State Agency Profiles" running title, in
# which case detect_split_x falls back to the widest-gap heuristic.
page_line_text <- function(page_lines) {
  page_lines %>%
    group_by(line_id) %>%
    summarise(txt = str_squish(str_to_lower(paste(text, collapse = " "))),
              .groups = "drop") %>%
    pull(txt)
}

has_header_row <- function(page_lines) {
  txt <- page_line_text(page_lines)
  # A column header can sit anywhere on the page: the 17th edition starts a new
  # sub-table half way down.
  if (any(str_detect(txt, HEADER_PATTERN))) return(TRUE)
  # The 13th edition's profile pages have no column header and are recognised by
  # their running title. That is not enough on its own: the 15th edition's
  # contents page uses the same words as its own heading and then lists every
  # jurisdiction against a page number, which parses as a full page of option
  # rows. Contents pages are set with dot leaders and the tables never are.
  if (!any(str_detect(head(txt, 2), "^snap state agency profiles"))) return(FALSE)
  mean(str_detect(txt, "\\.{4,}")) < 0.3
}

detect_split_x <- function(page_lines) {
  hdr <- page_lines %>%
    group_by(line_id) %>%
    summarise(txt = str_squish(str_to_lower(paste(text, collapse = " "))),
              .groups = "drop")
  hit <- hdr %>% filter(str_detect(txt, HEADER_PATTERN))

  if (nrow(hit) > 0) {
    w <- page_lines %>% filter(line_id == hit$line_id[1]) %>% arrange(x)
    if (nrow(w) >= 2) {
      # The gutter is the widest space between the end of one word and the start
      # of the next. Split at the LEFT EDGE of the right-hand column, not at the
      # middle of the gutter: labels run much longer than the header word, so a
      # midpoint split throws the tail of every long label into the value column.
      gap <- w$x[-1] - (w$x[-nrow(w)] + w$width[-nrow(w)])
      k <- which.max(gap)
      if (gap[k] > 20) return(w$x[k + 1] - 5)
    }
  }

  xs <- sort(unique(page_lines$x))
  if (length(xs) < 2) return(NA_real_)
  gaps <- diff(xs)
  # Restrict to the middle of the page so margins do not win the gap contest.
  page_w <- max(page_lines$x + page_lines$width)
  mid <- which(xs[-length(xs)] > page_w * 0.15 & xs[-length(xs)] < page_w * 0.75)
  if (length(mid) == 0) return(NA_real_)
  best <- mid[which.max(gaps[mid])]
  xs[best] + gaps[best] / 2
}

# ------------------------------------------------------------------------------
# 5. Parse a single page into label / value pairs
# ------------------------------------------------------------------------------

# Lines that are table furniture rather than data: sub-table titles, column
# headers, and the running footer. The 17th edition (2025) splits a jurisdiction
# into three sub-tables, each with its own title line and header row.
FURNITURE_PATTERN <- paste(
  "policy option", "option selection",
  "^waivers?$", "^demonstration projects?$",
  "^waiver implementation$", "^project implementation$",
  "^snap state agency profiles",
  "^page \\d+", "state options report",
  sep = "|")

ROW_GAP_PT <- 13.5

parse_page <- function(page, page_no, row_gap_pt = ROW_GAP_PT) {
  empty <- tibble(page = integer(), jurisdiction = character(),
                  has_header = logical(),
                  label_raw = character(), value_raw = character())
  if (nrow(page) == 0) return(empty)

  pl <- group_lines(page)
  jur <- detect_jurisdiction(pl)
  hdr <- has_header_row(pl)
  split_x <- detect_split_x(pl)
  if (is.na(split_x)) return(empty)

  lines <- pl %>%
    mutate(col = if_else(x < split_x, "label", "value")) %>%
    group_by(line_id) %>%
    summarise(ytop  = min(y),
              label = paste(text[col == "label"], collapse = " "),
              value = paste(text[col == "value"], collapse = " "),
              .groups = "drop") %>%
    arrange(line_id)

  # Row boundaries come from vertical spacing, not from which cell happens to be
  # empty. Both cells can wrap on the same line (17th edition waivers) and the
  # label can sit BETWEEN two lines of its own value (13th edition profiles), so
  # no rule based on which side is empty works.
  #
  # ROW_GAP_PT is in points because the physical line spacing is what is stable
  # across editions, not the reported glyph height: body text measures 13 pt in
  # the 15th-17th editions and 9 pt in the 13th, yet both set wrapped lines
  # 12-13 pt apart. New rows start 14 pt or more below the last, so the cut goes
  # at 13.5. That is one point of margin. It is enough for every page of the
  # 15th-17th editions, but the 13th edition sets its densest pages at a 14 pt
  # row pitch, where a row starting 13 pt below the last is indistinguishable
  # from a wrapped line; see the known-defect note in check_profile_edition().
  # Check the label inventory before trusting a new edition's numbers.
  gaps <- c(Inf, diff(lines$ytop))
  lines$rec <- cumsum(gaps > row_gap_pt)

  rows <- lines %>%
    group_by(rec) %>%
    summarise(label = str_squish(paste(label, collapse = " ")),
              value = str_squish(paste(value, collapse = " ")),
              .groups = "drop")

  # Drop furniture and the jurisdiction title line.
  rows <- rows %>%
    filter(!str_detect(str_squish(str_to_lower(paste(label, value))),
                       FURNITURE_PATTERN))
  # The jurisdiction title line. In the 13th edition it reads "Alabama-Food
  # Assistance Program" and runs wide enough to spill past the column split, so
  # it cannot be recognised by an empty value side alone. No option label begins
  # with a jurisdiction name, so anchoring at the start is safe.
  if (!is.na(jur)) {
    rows <- rows %>% filter(!str_starts(label, fixed(jur)))
  }
  rows <- rows %>% filter(label != "" | value != "")

  tibble(
    page = page_no,
    jurisdiction = jur,
    has_header = hdr,
    label_raw = clean_cell(rows$label),
    value_raw = clean_cell(rows$value)
  )
}

`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

# ------------------------------------------------------------------------------
# 6. Parse a whole report
# ------------------------------------------------------------------------------

parse_report <- function(path, report_year = NA_integer_,
                         row_gap_pt = ROW_GAP_PT) {
  message("Reading ", basename(path))
  pages <- pdf_data(path)

  raw <- imap_dfr(pages, ~ parse_page(.x, .y, row_gap_pt = row_gap_pt))

  # Keep only pages with a two-column header, then carry the jurisdiction
  # forward across continuation pages (a jurisdiction is named once, on its
  # first page). Restricting to header pages first is what keeps front matter
  # out: it names States in prose and would otherwise start a carry-forward.
  raw <- raw %>%
    filter(has_header) %>%
    mutate(jurisdiction = {
      j <- jurisdiction
      for (i in seq_along(j)) if (is.na(j[i]) && i > 1) j[i] <- j[i - 1]
      j
    }) %>%
    filter(!is.na(jurisdiction),
           !is.na(label_raw), label_raw != "", value_raw != "")

  raw %>%
    mutate(report_year = report_year,
           source = basename(path),
           label_norm = normalize_label(label_raw),
           variable = map_chr(label_norm, match_variable)) %>%
    select(report_year, source, jurisdiction, page, variable,
           label_raw, label_norm, value_raw)
}

# ------------------------------------------------------------------------------
# 6b. Editions already extracted to a long CSV
# ------------------------------------------------------------------------------

# Earlier editions are laid out as prose plus per-option lists of States rather
# than as a per-jurisdiction table, and were extracted elsewhere into a long
# state/year/category/status file (snap_options_<edition>.csv). Read them through
# the same crosswalk so option names are matched in exactly one place.
#
# Years: the file name carries the EDITION year while the file's own `year`
# column carries the as-of year, which is one lower (the 14th edition, the 2018
# file, reports options in effect as of October 1, 2017). report_year is taken
# from the file name, so it means the same thing here as it does for the PDFs.
parse_legacy_csv <- function(path, report_year = NA_integer_) {
  message("Reading ", basename(path))
  d <- read_csv(path, show_col_types = FALSE)
  missing_cols <- setdiff(c("state", "category", "status"), names(d))
  if (length(missing_cols)) {
    stop(basename(path), " is missing column(s): ",
         paste(missing_cols, collapse = ", "))
  }
  d %>%
    transmute(report_year  = report_year,
              source       = basename(path),
              jurisdiction = str_squish(state),
              page         = NA_integer_,
              label_raw    = clean_cell(category),
              label_norm   = normalize_label(category),
              value_raw    = clean_cell(status)) %>%
    filter(!is.na(jurisdiction), jurisdiction != "",
           !is.na(value_raw), value_raw != "") %>%
    mutate(variable = map_chr(label_norm, match_variable)) %>%
    select(report_year, source, jurisdiction, page, variable,
           label_raw, label_norm, value_raw)
}

match_variable <- function(label_norm) {
  if (is.na(label_norm) || label_norm == "") return(NA_character_)
  for (i in seq_len(nrow(OPTION_CROSSWALK))) {
    if (str_detect(label_norm, OPTION_CROSSWALK$pattern[i])) {
      return(OPTION_CROSSWALK$variable[i])
    }
  }
  NA_character_
}

# ------------------------------------------------------------------------------
# 7. Driver + validation
# ------------------------------------------------------------------------------

run_pipeline <- function(dir = ".", editions = EDITIONS, write_dir = NULL) {
  if (anyDuplicated(editions$year)) {
    stop("EDITIONS lists the same year twice: ",
         paste(editions$year[duplicated(editions$year)], collapse = ", "),
         ". One edition, one source.")
  }
  paths <- file.path(dir, editions$path)
  missing_files <- paths[!file.exists(paths)]
  if (length(missing_files)) {
    stop("EDITIONS names file(s) that do not exist:\n",
         paste0("  ", missing_files, collapse = "\n"))
  }

  long <- map2_dfr(paths, editions$year, function(p, y) {
    if (str_detect(str_to_lower(p), "\\.csv$")) parse_legacy_csv(p, y)
    else parse_report(p, y)
  })

  # An edition whose pages carry no two-column option table yields nothing.
  # Surface that instead of returning a silently short panel.
  empty_editions <- setdiff(editions$year, unique(long$report_year))
  if (length(empty_editions)) {
    warning("no per-jurisdiction option table found in edition(s): ",
            paste(sort(empty_editions), collapse = ", "))
  }

  long <- long %>% arrange(report_year, jurisdiction)

  wide <- build_panel(long)

  diagnostics <- list(
    rows_per_edition = long %>%
      group_by(report_year) %>%
      summarise(jurisdictions = n_distinct(jurisdiction),
                options_read = n(),
                unmatched = sum(is.na(variable)), .groups = "drop"),
    unmatched_labels = long %>%
      filter(is.na(variable)) %>%
      count(report_year, label_norm, sort = TRUE),
    options_per_jurisdiction = long %>%
      filter(!is.na(variable)) %>%
      distinct(report_year, jurisdiction, variable) %>%
      count(report_year, jurisdiction, name = "n_options") %>%
      arrange(n_options),
    missing_jurisdictions = long %>%
      distinct(report_year, jurisdiction) %>%
      group_by(report_year) %>%
      summarise(missing = paste(setdiff(JURISDICTIONS, jurisdiction),
                                collapse = "; "), .groups = "drop"),
    empty_cells = wide %>%
      group_by(Year) %>%
      summarise(across(-State, ~ sum(is.na(.x))), .groups = "drop") %>%
      pivot_longer(-Year, names_to = "column", values_to = "n_missing") %>%
      filter(n_missing > 0) %>%
      arrange(desc(n_missing))
  )

  if (!is.null(write_dir)) {
    dir.create(write_dir, showWarnings = FALSE, recursive = TRUE)
    # na = "" so that an option an edition did not report reads as an empty
    # cell rather than the literal string "NA".
    write_csv(wide, file.path(write_dir, "snap_state_options_all_years.csv"), na = "")
    write_csv(long, file.path(write_dir, "snap_state_options_long_all_years.csv"), na = "")
    write_csv(diagnostics$unmatched_labels,
              file.path(write_dir, "diag_unmatched_labels.csv"))
  }

  list(long = long, wide = wide, diagnostics = diagnostics)
}

# ------------------------------------------------------------------------------
# 7b. Cross-edition panel on the snap_state_options_2023.csv schema
# ------------------------------------------------------------------------------

# The 16th and 17th editions report ESAP and SMD as two yes/no rows where the
# 15th reports one combined row. Fold the pair back into the 15th edition's
# four-level vocabulary so the column means the same thing in every year.
fold_esap_smd <- function(long) {
  pair <- long %>% filter(variable %in% c("esap", "smd"))
  if (nrow(pair) == 0) return(long[0, ] %>% select(report_year, jurisdiction, variable, value_raw))
  pair %>%
    select(report_year, jurisdiction, variable, value_raw) %>%
    pivot_wider(names_from = variable, values_from = value_raw) %>%
    mutate(
      has_esap = !is.na(.data$esap) & str_detect(str_to_lower(.data$esap), "^esap$"),
      has_smd  = !is.na(.data$smd)  & str_detect(str_to_lower(.data$smd),  "^smd$"),
      variable = "demonstrations",
      value_raw = case_when(
        has_smd & has_esap ~ "SMD and ESAP",
        has_smd            ~ "SMD only",
        has_esap           ~ "ESAP only",
        TRUE               ~ "No SMD or ESAP")) %>%
    select(report_year, jurisdiction, variable, value_raw)
}

# One row per jurisdiction-year, carrying exactly the columns of
# snap_state_options_2023.csv. A column an edition does not report stays NA
# rather than being imputed: the 17th edition drops self-employment income and
# both ineligible-noncitizen options, and the 15th edition omits the ABAWD waiver
# row for Connecticut.
build_panel <- function(long) {
  cells <- long %>%
    filter(!is.na(variable), variable != "esap", variable != "smd") %>%
    select(report_year, jurisdiction, variable, value_raw) %>%
    bind_rows(fold_esap_smd(long)) %>%
    filter(variable %in% CANONICAL_COLUMNS$variable) %>%
    # If an edition repeats an option on a continuation page, keep the first.
    group_by(report_year, jurisdiction, variable) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    left_join(CANONICAL_COLUMNS, by = "variable")

  wide <- cells %>%
    select(Year = report_year, State = jurisdiction, column, value_raw) %>%
    pivot_wider(names_from = column, values_from = value_raw)

  for (nm in CANONICAL_COLUMNS$column) if (!nm %in% names(wide)) wide[[nm]] <- NA_character_

  wide %>%
    select(Year, State, all_of(CANONICAL_COLUMNS$column)) %>%
    arrange(Year, State)
}

# ------------------------------------------------------------------------------
# 8. Cross-edition comparison
# ------------------------------------------------------------------------------

# Which jurisdictions changed a given option between two editions?
compare_editions <- function(wide, from_year, to_year) {
  wide %>%
    filter(report_year %in% c(from_year, to_year)) %>%
    pivot_longer(-c(report_year, jurisdiction),
                 names_to = "variable", values_to = "value") %>%
    pivot_wider(names_from = report_year, values_from = value,
                names_prefix = "y") %>%
    rename(from = !!paste0("y", from_year), to = !!paste0("y", to_year)) %>%
    filter(!is.na(from), !is.na(to),
           str_squish(str_to_lower(from)) != str_squish(str_to_lower(to))) %>%
    arrange(variable, jurisdiction)
}

# ------------------------------------------------------------------------------
# 9. Layout inspection (run this first on any new edition)
# ------------------------------------------------------------------------------

# Prints the reconstructed two-column view of one page so you can confirm the
# split point and row grouping before trusting a full run.
preview_page <- function(path, page_no) {
  pg <- pdf_data(path)[[page_no]]
  pl <- group_lines(pg)
  sx <- detect_split_x(pl)
  cat("Detected jurisdiction:", detect_jurisdiction(pl), "\n")
  cat("Detected split x:", round(sx, 1), "\n\n")
  print(parse_page(pg, page_no) %>% select(label_raw, value_raw), n = 40)
  invisible(pl)
}
