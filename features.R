# Libraries
library(here)
library(dplyr)

# Flags
using_qc_data <- TRUE 

# If using_qc_data is false, set the following variables with 
# your dataset's column names
state_col_map <- c(
  fiscal_year = "FISCAL_YEAR",
  state_name  = "STATE",
  rawusize    = "HOUSEHOLD_SIZE",
  rawgrinc    = "GROSS_INCOME",
  rawearn     = "EARNED_INCOME",
  rawunearn   = "UNEARNED_INCOME",
  rawmedded   = "MEDICAL_DEDUCTION",
  rawdepded   = "DEPENDENT_CARE_DEDUCTION",
  rawcsded    = "CHILD_SUPPORT_DEDUCTION",
  rawrent     = "RENT",
  rawutil     = "UTILITY_COSTS"
)

rename_cols <- function(data, map = state_col_map, qc = using_qc_data) {
  if (qc) return(data)
  dplyr::rename(data, dplyr::any_of(map))
}

# Additional data
folder <- paste0(here(), "/")
year_data <- read.csv(paste0(folder, "additional_data/year_data.csv"))

#' Add a within-cell percentile column. For `col`, adds 
#' `<col>_p`: the proportion of non-missing observations in the
#' same state_name x rawusize cell whose value is <= this one.
#' Values are CPI-deflated before ranking.
add_percentile <- function(data, col,
                           group_vars = c("rawusize", "state_name"),
                           suffix = "_p") {
  
  new_name <- paste0(rlang::as_name(rlang::ensym(col)), suffix)
  
  data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::mutate("{new_name}" := dplyr::if_else(
      {{ col }} == 0,
      0,
      dplyr::cume_dist(dplyr::na_if({{ col }}, 0) / cpi)
    )) |>
    dplyr::ungroup()
}

#' Standard utility allowance (SUA) anchor for a state-year: the MODE of the
#' positive utility amounts, i.e. the dominant standard allowance in that
#' state-year. v1 (2026-08-22): self-computed from the data, no external
#' SUA table. Semantics, pinned: positive values only, rounded to whole
#' dollars, ties broken to the SMALLEST tied value; NA when a cell has no
#' positive amounts. The published FNS SUA tables are the intended
#' refinement path for an externally anchored version.
sua_anchor <- function(util) {
  x <- round(util[!is.na(util) & util > 0])
  if (!length(x)) return(NA_real_)
  tb <- table(x)
  as.numeric(names(tb)[which(tb == max(tb))[1]])
}

#' SUA tier, 3 levels, per state-year (v1, 2026-08-22; design + one-year-
#' ahead result in methods/v250_benchmark_2024_utilrel/):
#'   0 = no utility amount
#'   1 = positive but below (anchor - band)
#'   2 = at or above (anchor - band): the HIGH-SUA cluster
#' The $200 band (`sua_high_band`) pulls within-state HIGH-SUA variants
#' into tier 2: household-size schedules (e.g. Virginia 375 / 476) and
#' regional schedules (e.g. New York 877 / 988 / 1,062), which sit close
#' to each other relative to the lower allowances. The band census
#' (methods/vocab_hygiene_census/utilities_high_band_census.csv) shows it
#' is empty in most state-years and captures the variants where they
#' exist. A fixed dollar band is proportionally looser where the anchor
#' is low; that is a known v1 property, left for the SUA-structure review.
#' Rules mined on this tier keep meaning the same thing when SUA levels
#' reset each October, which dollar thresholds on utilities do not.
sua_high_band <- 200

add_sua_tier <- function(data, util_col = "rawutil",
                         group_vars = c("state_name", "fiscal_year")) {
  data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::mutate(utilities_sua = {
      u <- .data[[util_col]]
      a <- sua_anchor(u)
      dplyr::case_when(
        is.na(u) | u <= 0 ~ 0L,
        is.na(a)          ~ 2L,   # positive amounts with no anchor: never occurs on the frame
        u < a - sua_high_band ~ 1L,
        TRUE              ~ 2L)
    }) |>
    dplyr::ungroup()
}

#' Add all engineered features
add_features <- function(data) {
  data |>
    rename_cols() |>
    dplyr::mutate(at_max_ben = rawben_uncapped >= rawbenmax) |>
    add_sua_tier() |>
    dplyr::left_join(dplyr::select(year_data, year, cpi),       
                     by = c("fiscal_year" = "year")) |>
    add_percentile(rawgrinc) |>
    add_percentile(rawearn) |>
    add_percentile(rawunearn) |>
    add_percentile(rawmedded) |>
    add_percentile(rawdepded) |>
    add_percentile(rawcsded) |>
    add_percentile(rawrent) |>
    dplyr::select(-cpi)                                    
}