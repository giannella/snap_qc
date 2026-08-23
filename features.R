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
state_sua <- read.csv(paste0(folder, "additional_data/state_sua.csv"))

#' Join a wide state x year lookup (state_name + X2017...X2026) onto a frame.
add_state_year_col <- function(data, lookup, value_col,
                               key = "state_name", year_col = "fiscal_year") {
  
  long <- lookup |>
    tidyr::pivot_longer(
      cols         = -dplyr::all_of(key),
      names_to     = year_col,
      names_prefix = "X",
      values_to    = value_col
    ) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(year_col), as.integer))
  
  long <- dplyr::mutate(long,
                        dplyr::across(dplyr::all_of(value_col), \(x) dplyr::na_if(x, 0)))
  
  dplyr::left_join(data, long, by = c(key, year_col))
}


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

#' SUA tier, 3 levels, per state-year
#'   0 = no utility amount
#'   1 = below the heating/cooling standard
#'   2 = at or above the heating/cooling standard
add_sua_tier <- function(data, util_col = "rawutil", anchor_col = "max_sua") {
  data |>
    dplyr::mutate(utilities_sua = dplyr::case_when(
      is.na(.data[[util_col]])   | .data[[util_col]]   <= 0 ~ 0L,
      is.na(.data[[anchor_col]]) | .data[[anchor_col]] <= 0 ~ NA_integer_,
      .data[[util_col]] < .data[[anchor_col]]               ~ 1L,
      TRUE                                                  ~ 2L
    ))
}

#' Add all external data needed for calculations
add_external_data <- function(data) {
  data |>
    rename_cols() |>
    add_state_year_col(state_sua, "max_sua") |>
    dplyr::left_join(dplyr::select(year_data, year, cpi),       
                     by = c("fiscal_year" = "year")) |>
    dplyr::select(-cpi)                                    
}

#' Add all engineered features
add_features <- function(data) {
  data |>
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