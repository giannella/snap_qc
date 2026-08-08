# Libraries
library(here)
library(dplyr)

# Flags
using_qc_data <- TRUE 

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

#' Add all engineered features
add_features <- function(data) {
  data |>
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