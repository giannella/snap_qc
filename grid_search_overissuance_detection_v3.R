library(dplyr)
library(purrr)
library(tidyr)

# ── Grid search for optimal overissuance error detection ─────────────────────
#
# Optimizes for:
# 1. Dollar recall: % of total overissuance error dollars captured
# 2. Precision: % of flagged cases that are actual errors (overthreshold==1)
#
# Searches across combinations of:
#   - earned_by_hh_size >= threshold (lower bound)
#   - rawben_rel_max between lower and upper bounds
#   - shelter_by_hh_size <= threshold (upper bound)
#   - shelter_to_gross_income_ratio >= threshold (lower bound)
#   - rawben_rel_max >= threshold (lower bound)


generate_overissuance_grid <- function(data,
                                       error_amount_var = "total_error_amount",
                                       error_threshold = 52,
                                       earned_by_hh_size_range    = seq(0, 600, by = 50),
                                       rawben_rel_max_range_lower  = seq(0, 1, by = 0.1),
                                       rawben_rel_max_range_upper  = seq(0, 1, by = 0.1),
                                       shelter_by_hh_size_range = seq(0, 800, by = 100),
                                       shelter_to_gross_income_ratio_range = seq(0, 2, by = 0.1)) {

  # Filter out underissuance cases, keep overissuance and amount correct
  filtered_data <- data %>%
    filter(tolower(status) != "underissuance")

  # Get overissuance subset for calculating total error (for recall)
  overiss_data <- filtered_data %>%
    filter(tolower(status) == "overissuance")

  # Multiply error amounts by weight (fywgt) to get contribution to error rate
  total_error_dollars <- sum(overiss_data[[error_amount_var]] * overiss_data$fywgt, na.rm = TRUE)
  total_cases <- nrow(filtered_data)  # All cases (overissuance + amount correct)

  if (nrow(overiss_data) == 0 || total_error_dollars == 0) {
    warning("No overissuance error amount in this subset")
    return(data.frame())
  }

  cat("  Total cases (overissuance + amount correct):", total_cases, "\n")
  cat("  Overissuance cases:", nrow(overiss_data), "\n")
  cat("  Total overissuance error dollars: $", format(round(total_error_dollars, 2), big.mark = ","), "\n", sep = "")

  # Create grid of threshold combinations
  threshold_grid <- expand.grid(
    earned_by_hh_size_thresh      = earned_by_hh_size_range,
    rawben_rel_max_thresh_lower   = rawben_rel_max_range_lower,  # Lower bound
    rawben_rel_max_thresh_upper   = rawben_rel_max_range_upper,  # Upper bound
    shelter_by_hh_size_thresh  = shelter_by_hh_size_range,
    shelter_to_gross_income_ratio_thresh = shelter_to_gross_income_ratio_range,
    stringsAsFactors = FALSE
  ) %>%
    # Only keep combinations where lower <= upper
    filter(rawben_rel_max_thresh_lower <= rawben_rel_max_thresh_upper)

  cat("  Testing", format(nrow(threshold_grid), big.mark = ","), "threshold combinations...\n")

  # Calculate metrics for each threshold combination
  calc_metrics <- function(earned_thresh, rawben_lower, rawben_upper, ded_thresh, shelter_ratio_thresh, data_to_use, total_error) {

    # Flag cases that meet all thresholds:
    #   - earned_by_hh_size  >= earned_thresh     (lower bound)
    #   - rawben_rel_max     >= rawben_lower  AND  < rawben_upper  (band)
    #   - shelter_by_hh_size <= ded_thresh     (upper bound)
    #   - shelter_to_gross_income_ratio >= shelter_ratio_thresh  (lower bound)
    flagged <- data_to_use$earned_by_hh_size >= earned_thresh &
               data_to_use$rawben_rel_max     >= rawben_lower &
               data_to_use$rawben_rel_max     <  rawben_upper &
               data_to_use$shelter_by_hh_size <= ded_thresh &
               data_to_use$shelter_to_gross_income_ratio >= shelter_ratio_thresh

    flagged[is.na(flagged)] <- FALSE

    # For RECALL: only count overissuance errors captured
    is_overissuance   <- tolower(data_to_use$status) == "overissuance"
    overiss_flagged   <- flagged & is_overissuance

    # WEIGHTED: for recall calculation (contribution to error rate)
    error_dollars_captured_weighted <- sum(
      data_to_use[[error_amount_var]][overiss_flagged] * data_to_use$fywgt[overiss_flagged],
      na.rm = TRUE
    )

    # UNWEIGHTED: for dollars per case (average error per flagged case)
    sum_error_all_flagged <- sum(data_to_use[[error_amount_var]][flagged], na.rm = TRUE)

    # For PRECISION: flagged cases where total_error_amount > error_threshold
    n_flagged            <- sum(flagged, na.rm = TRUE)
    n_sig_errors_flagged <- sum(data_to_use[[error_amount_var]][flagged] > error_threshold, na.rm = TRUE)
    n_errors_flagged     <- sum(data_to_use[[error_amount_var]][flagged] > 0, na.rm = TRUE)
    # False positives = flagged cases that do NOT exceed the error threshold
    n_false_positives    <- sum(
      data_to_use[[error_amount_var]][flagged] <= error_threshold,
      na.rm = TRUE
    )

    list(
      error_dollars_captured = error_dollars_captured_weighted,
      n_flagged              = n_flagged,
      n_sig_errors_flagged   = n_sig_errors_flagged,
      n_errors_flagged       = n_errors_flagged,
      n_false_positives      = n_false_positives,
      dollar_recall          = error_dollars_captured_weighted / total_error,
      precision              = ifelse(n_flagged > 0, n_sig_errors_flagged / n_flagged, 0),
      pct_any_error          = ifelse(n_flagged > 0, n_errors_flagged / n_flagged, 0),
      fpr_flagged            = ifelse(n_flagged > 0, n_false_positives / n_flagged, 0),
      dollars_per_case       = ifelse(n_flagged > 0, sum_error_all_flagged / n_flagged, 0)
    )
  }

  results <- threshold_grid %>%
    mutate(
      metrics = pmap(
        list(earned_by_hh_size_thresh,
             rawben_rel_max_thresh_lower,
             rawben_rel_max_thresh_upper,
             shelter_by_hh_size_thresh,
             shelter_to_gross_income_ratio_thresh),
        ~ calc_metrics(..1, ..2, ..3, ..4, ..5, filtered_data, total_error_dollars)
      )
    ) %>%
    unnest_wider(metrics) %>%
    mutate(
      pct_flagged         = n_flagged / total_cases,
      total_n             = total_cases,
      total_error_dollars = total_error_dollars
    ) %>%
    filter(n_flagged > 0)  # Only keep combinations that flag at least one case

  return(results)
}


# ── Run grid search by geography (state or region) ───────────────────────────

run_overissuance_grid_by_geography <- function(data,
                                               geography_var = "state",
                                               error_amount_var = "total_error_amount",
                                               error_threshold = 52,
                                               earned_by_hh_size_range     = seq(0, 600, by = 50),
                                               rawben_rel_max_range_lower  = seq(0, 1, by = 0.1),
                                               rawben_rel_max_range_upper  = seq(0, 1, by = 0.1),
                                               shelter_by_hh_size_range = seq(0, 800, by = 100),
                                               shelter_to_gross_income_ratio_range = seq(0, 2, by = 0.1),
                                               min_error_dollars = 1000) {

  geographies <- unique(data[[geography_var]])
  geographies <- geographies[!is.na(geographies)]

  all_results <- list()

  for (geo in geographies) {
    cat("\n", toupper(geography_var), ":", geo, "\n")
    cat(rep("=", 60), "\n", sep = "")

    geo_data <- data %>% filter(!!sym(geography_var) == geo)

    # Filter out underissuance, keep overissuance and amount correct
    filtered_geo_data <- geo_data %>% filter(tolower(status) != "underissuance")

    # Check overissuance totals (weighted by fywgt)
    overiss_data  <- filtered_geo_data %>% filter(tolower(status) == "overissuance")
    total_error   <- sum(overiss_data[[error_amount_var]] * overiss_data$fywgt, na.rm = TRUE)
    n_error_cases <- sum(overiss_data[[error_amount_var]] > 0, na.rm = TRUE)
    n_sig_errors  <- sum(overiss_data[[error_amount_var]] > error_threshold, na.rm = TRUE)

    cat("  Total cases (overissuance + amount correct):", nrow(filtered_geo_data), "\n")
    cat("  Overissuance cases:", nrow(overiss_data), "\n")
    cat("  Total overissuance error: $", format(round(total_error, 2), big.mark = ","), "\n", sep = "")
    cat("  Overissuance error cases (>$0):", n_error_cases, "\n")
    cat("  Significant overissuance errors (error > $52):", n_sig_errors, "\n")

    if (total_error < min_error_dollars) {
      cat("  Skipping - less than $", format(min_error_dollars, big.mark = ","), " in total errors\n", sep = "")
      next
    }

    geo_results <- generate_overissuance_grid(
      data                        = geo_data,
      error_amount_var            = error_amount_var,
      error_threshold             = error_threshold,
      earned_by_hh_size_range     = earned_by_hh_size_range,
      rawben_rel_max_range_lower  = rawben_rel_max_range_lower,
      rawben_rel_max_range_upper  = rawben_rel_max_range_upper,
      shelter_by_hh_size_range = shelter_by_hh_size_range,
      shelter_to_gross_income_ratio_range = shelter_to_gross_income_ratio_range
    )

    if (nrow(geo_results) > 0) {
      geo_results <- geo_results %>%
        mutate(
          geography_type        = geography_var,
          geography_value       = geo,
          geography_total_error = total_error,
          .before = everything()
        )

      all_results[[geo]] <- geo_results

      # Show top 5 by dollar recall
      cat("\n  Top 5 rules by dollar recall:\n")
      geo_results %>%
        arrange(desc(dollar_recall)) %>%
        head(5) %>%
        select(earned_by_hh_size_thresh,
               rawben_rel_max_thresh_lower, rawben_rel_max_thresh_upper,
               shelter_by_hh_size_thresh, shelter_to_gross_income_ratio_thresh,
               dollar_recall, precision, dollars_per_case,
               n_flagged, pct_flagged) %>%
        mutate(
          dollar_recall    = paste0(round(dollar_recall * 100, 1), "%"),
          precision        = paste0(round(precision * 100, 1), "%"),
          dollars_per_case = paste0("$", round(dollars_per_case, 0)),
          pct_flagged      = paste0(round(pct_flagged * 100, 1), "%")
        ) %>%
        print()

      # Show top 5 with precision >= 50%
      cat("\n  Top 5 rules with precision >= 50%:\n")
      geo_results %>%
        filter(precision >= 0.50) %>%
        arrange(desc(dollar_recall)) %>%
        head(5) %>%
        select(earned_by_hh_size_thresh,
               rawben_rel_max_thresh_lower, rawben_rel_max_thresh_upper,
               shelter_by_hh_size_thresh, shelter_to_gross_income_ratio_thresh,
               dollar_recall, precision, dollars_per_case,
               n_flagged, pct_flagged) %>%
        mutate(
          dollar_recall    = paste0(round(dollar_recall * 100, 1), "%"),
          precision        = paste0(round(precision * 100, 1), "%"),
          dollars_per_case = paste0("$", round(dollars_per_case, 0)),
          pct_flagged      = paste0(round(pct_flagged * 100, 1), "%")
        ) %>%
        print()
    }
  }

  # Combine all results
  combined_results <- bind_rows(all_results)

  cat("\n", rep("=", 60), "\n", sep = "")
  cat("OVERALL SUMMARY\n")
  cat(rep("=", 60), "\n", sep = "")
  cat("Total geographies processed:", length(all_results), "\n")
  cat("Total threshold combinations tested:", format(nrow(combined_results), big.mark = ","), "\n")

  return(combined_results)
}


# ── Find optimal thresholds for each geography ───────────────────────────────

find_optimal_overissuance_thresholds <- function(grid_results,
                                                  optimize_for    = "dollars_per_case",
                                                  min_precision   = 0.50,
                                                  max_flagged_pct = 0.30,
                                                  min_dollar_recall = 0.0) {

  optimal <- grid_results %>%
    filter(precision     >= min_precision,
           pct_flagged   <= max_flagged_pct,
           dollar_recall >= min_dollar_recall) %>%
    group_by(geography_type, geography_value) %>%
    arrange(desc(!!sym(optimize_for))) %>%
    slice(1) %>%
    ungroup() %>%
    select(geography_type, geography_value, geography_total_error,
           earned_by_hh_size_thresh,
           rawben_rel_max_thresh_lower, rawben_rel_max_thresh_upper,
           shelter_by_hh_size_thresh, shelter_to_gross_income_ratio_thresh,
           dollar_recall, error_dollars_captured, total_error_dollars,
           precision, n_flagged, n_sig_errors_flagged,
           pct_flagged, dollars_per_case, total_n)

  return(optimal)
}


# ── Example usage ─────────────────────────────────────────────────────────────
#
# # Run grid search by state
# results <- run_overissuance_grid_by_geography(
#   data                        = income_and_clean_data,
#   geography_var               = "state",
#   error_amount_var            = "total_error_amount",
#   error_threshold             = 52,
#   earned_by_hh_size_range     = seq(0, 600, by = 50),
#   rawben_rel_max_range_lower  = seq(0, 1, by = 0.1),
#   rawben_rel_max_range_upper  = seq(0, 1, by = 0.1),
#   shelter_by_hh_size_range = seq(0, 800, by = 100),
#   shelter_to_gross_income_ratio_range = seq(0, 2, by = 0.1),
#   min_error_dollars           = 5000
# )
#
# # Option 1: Maximize dollars per case (efficiency) while maintaining precision
# optimal <- find_optimal_overissuance_thresholds(
#   grid_results      = results,
#   optimize_for      = "dollars_per_case",
#   min_precision     = 0.50,
#   max_flagged_pct   = 0.30,
#   min_dollar_recall = 0.20
# )
#
# # Option 2: Maximize dollar recall (coverage) while maintaining precision
# optimal_recall <- find_optimal_overissuance_thresholds(
#   grid_results    = results,
#   optimize_for    = "dollar_recall",
#   min_precision   = 0.50,
#   max_flagged_pct = 0.30
# )
#
# # Option 3: Balance between efficiency and coverage
# optimal_balanced <- find_optimal_overissuance_thresholds(
#   grid_results      = results,
#   optimize_for      = "dollars_per_case",
#   min_precision     = 0.50,
#   max_flagged_pct   = 0.30,
#   min_dollar_recall = 0.30
# )
#
# # View results
# optimal %>%
#   arrange(desc(dollars_per_case)) %>%
#   print()
#
# # Save results
# write.csv(results,  "overissuance_grid_search_full.csv",      row.names = FALSE)
# write.csv(optimal,  "overissuance_optimal_thresholds.csv",    row.names = FALSE)
