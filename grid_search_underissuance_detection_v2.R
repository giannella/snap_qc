library(dplyr)
library(purrr)
library(tidyr)

# ── Grid search for optimal underissuance error detection ────────────────────
#
# Optimizes for:
# 1. Dollar recall: % of total underissuance error dollars captured
# 2. Precision: % of flagged cases that are actual errors (overthreshold==1)
#
# Searches across combinations of:
#   - earned_by_hh_size > threshold (higher earned income)
#   - rawben_rel_max < threshold (lower benefit relative to max)
#   - shelter_by_hh_size < threshold (lower deductions)

generate_underissuance_grid <- function(data, 
                                        error_amount_var = "total_error_amount",
                                        overthreshold_var = "overthreshold",
                                        earned_by_hh_size_range = seq(0, 600, by = 50),
                                        rawben_rel_max_range = seq(0.5, 1.0, by = 0.05),
                                        shelter_by_hh_size_range = seq(0, 800, by = 100)) {
  
  # Filter out overissuance cases, keep underissuance and amount correct
  filtered_data <- data %>%
    filter(tolower(status) != "overissuance")
  
  # Get underissuance subset for calculating total error (for recall)
  underiss_data <- filtered_data %>%
    filter(tolower(status) == "underissuance")
  
  # Check if there's any data
  # Multiply error amounts by weight (fywgt) to get contribution to error rate
  total_error_dollars <- sum(underiss_data[[error_amount_var]] * underiss_data$fywgt, na.rm = TRUE)
  total_cases <- nrow(filtered_data)  # All cases (underissuance + amount correct)
  
  if(nrow(underiss_data) == 0 || total_error_dollars == 0) {
    warning("No underissuance error amount in this subset")
    return(data.frame())
  }
  
  cat("  Total cases (underissuance + amount correct):", total_cases, "\n")
  cat("  Underissuance cases:", nrow(underiss_data), "\n")
  cat("  Total underissuance error dollars: $", format(round(total_error_dollars, 2), big.mark = ","), "\n", sep = "")
  
  # Create grid of threshold combinations
  threshold_grid <- expand.grid(
    earned_by_hh_size_thresh = earned_by_hh_size_range,
    rawben_rel_max_thresh_lower = rawben_rel_max_range,  # Lower bound
    rawben_rel_max_thresh_upper = rawben_rel_max_range,  # Upper bound
    shelter_by_hh_size_thresh = shelter_by_hh_size_range,
    stringsAsFactors = FALSE
  ) %>%
    # Only keep combinations where lower <= upper
    filter(rawben_rel_max_thresh_lower <= rawben_rel_max_thresh_upper)
  
  cat("  Testing", format(nrow(threshold_grid), big.mark = ","), "threshold combinations...\n")
  
  # Calculate metrics for each combination
  calc_metrics <- function(earned_thresh, rb_thresh_lower, rb_thresh_upper, td_thresh, data_to_use, total_error) {
    # Flag cases that meet all thresholds (from underissuance + amount correct cases)
    # - earned_by_hh_size GREATER THAN OR EQUAL TO threshold
    # - rawben_rel_max BETWEEN lower and upper thresholds
    # - shelter_by_hh_size LESS THAN threshold (lower deductions)
    flagged <- data_to_use$earned_by_hh_size >= earned_thresh & 
               data_to_use$rawben_rel_max >= rb_thresh_lower &
               data_to_use$rawben_rel_max < rb_thresh_upper &
               data_to_use$shelter_by_hh_size < td_thresh
    
    flagged[is.na(flagged)] <- FALSE
    
    # For RECALL: only count underissuance errors captured
    is_underissuance <- tolower(data_to_use$status) == "underissuance"
    underiss_flagged <- flagged & is_underissuance
    
    # Calculate metrics
    # WEIGHTED: for recall calculation (contribution to error rate)
    error_dollars_captured_weighted <- sum(data_to_use[[error_amount_var]][underiss_flagged] * data_to_use$fywgt[underiss_flagged], na.rm = TRUE)
    
    # UNWEIGHTED: for dollars per case (average error per flagged case)
    sum_error_all_flagged <- sum(data_to_use[[error_amount_var]][flagged], na.rm = TRUE)
    
    # For PRECISION: count all flagged cases (underissuance + amount correct)
    n_flagged <- sum(flagged, na.rm = TRUE)
    n_sig_errors_flagged <- sum(data_to_use[[overthreshold_var]][flagged] == 1, na.rm = TRUE)
    n_errors_flagged <- sum(data_to_use[[error_amount_var]][flagged] > 0, na.rm = TRUE)
    # False positives = cases that are NOT significant errors (overthreshold != 1) OR are correct (error_amount == 0)
    n_false_positives <- sum((data_to_use[[overthreshold_var]][flagged] != 1 | data_to_use[[error_amount_var]][flagged] == 0), na.rm = TRUE)
    
    list(
      error_dollars_captured = error_dollars_captured_weighted,
      n_flagged = n_flagged,
      n_sig_errors_flagged = n_sig_errors_flagged,
      n_errors_flagged = n_errors_flagged,
      n_false_positives = n_false_positives,
      dollar_recall = error_dollars_captured_weighted / total_error,
      precision = ifelse(n_flagged > 0, n_sig_errors_flagged / n_flagged, 0),
      pct_any_error = ifelse(n_flagged > 0, n_errors_flagged / n_flagged, 0),
      fpr_flagged = ifelse(n_flagged > 0, n_false_positives / n_flagged, 0),
      dollars_per_case = ifelse(n_flagged > 0, sum_error_all_flagged / n_flagged, 0)
    )
  }
  
  results <- threshold_grid %>%
    mutate(
      metrics = pmap(
        list(earned_by_hh_size_thresh, rawben_rel_max_thresh_lower, rawben_rel_max_thresh_upper, shelter_by_hh_size_thresh),
        ~ calc_metrics(..1, ..2, ..3, ..4, filtered_data, total_error_dollars)
      )
    ) %>%
    unnest_wider(metrics) %>%
    mutate(
      pct_flagged = n_flagged / total_cases,
      total_n = total_cases,
      total_error_dollars = total_error_dollars
    ) %>%
    filter(n_flagged > 0)  # Only keep combinations that flag at least one case
  
  return(results)
}


# Run grid search by geography (state or region)
run_underissuance_grid_by_geography <- function(data,
                                                geography_var = "state",
                                                error_amount_var = "total_error_amount",
                                                overthreshold_var = "overthreshold",
                                                earned_by_hh_size_range = seq(0, 600, by = 50),
                                                rawben_rel_max_range = seq(0.5, 1.0, by = 0.05),
                                                shelter_by_hh_size_range = seq(0, 800, by = 100),
                                                min_error_dollars = 1000) {
  
  geographies <- unique(data[[geography_var]])
  geographies <- geographies[!is.na(geographies)]
  
  all_results <- list()
  
  for (geo in geographies) {
    cat("\n", toupper(geography_var), ":", geo, "\n")
    cat(rep("=", 60), "\n", sep = "")
    
    geo_data <- data %>% filter(!!sym(geography_var) == geo)
    
    # Filter out overissuance, keep underissuance and amount correct
    filtered_geo_data <- geo_data %>% filter(tolower(status) != "overissuance")
    
    # Check underissuance totals (weighted by fywgt)
    underiss_data <- filtered_geo_data %>% filter(tolower(status) == "underissuance")
    total_error <- sum(underiss_data[[error_amount_var]] * underiss_data$fywgt, na.rm = TRUE)
    n_error_cases <- sum(underiss_data[[error_amount_var]] > 0, na.rm = TRUE)
    n_sig_errors <- sum(underiss_data[[overthreshold_var]] == 1, na.rm = TRUE)
    
    cat("  Total cases (underissuance + amount correct):", nrow(filtered_geo_data), "\n")
    cat("  Underissuance cases:", nrow(underiss_data), "\n")
    cat("  Total underissuance error: $", format(round(total_error, 2), big.mark = ","), "\n", sep = "")
    cat("  Underissuance error cases (>$0):", n_error_cases, "\n")
    cat("  Significant underissuance errors (overthreshold==1):", n_sig_errors, "\n")
    
    if (total_error < min_error_dollars) {
      cat("  Skipping - less than $", format(min_error_dollars, big.mark = ","), " in total errors\n", sep = "")
      next
    }
    
    geo_results <- generate_underissuance_grid(
      data = geo_data,
      error_amount_var = error_amount_var,
      overthreshold_var = overthreshold_var,
      earned_by_hh_size_range = earned_by_hh_size_range,
      rawben_rel_max_range = rawben_rel_max_range,
      shelter_by_hh_size_range = shelter_by_hh_size_range
    )
    
    if (nrow(geo_results) > 0) {
      geo_results <- geo_results %>%
        mutate(
          geography_type = geography_var,
          geography_value = geo,
          geography_total_error = total_error,
          .before = everything()
        )
      
      all_results[[geo]] <- geo_results
      
      # Show top 5 by dollar recall
      cat("\n  Top 5 rules by dollar recall:\n")
      geo_results %>%
        arrange(desc(dollar_recall)) %>%
        head(5) %>%
        select(earned_by_hh_size_thresh, rawben_rel_max_thresh_lower, rawben_rel_max_thresh_upper,
               shelter_by_hh_size_thresh,
               dollar_recall, precision, dollars_per_case, 
               n_flagged, pct_flagged) %>%
        mutate(
          dollar_recall = paste0(round(dollar_recall * 100, 1), "%"),
          precision = paste0(round(precision * 100, 1), "%"),
          dollars_per_case = paste0("$", round(dollars_per_case, 0)),
          pct_flagged = paste0(round(pct_flagged * 100, 1), "%")
        ) %>%
        print()
      
      # Show top 5 with precision >= 50%
      cat("\n  Top 5 rules with precision >= 50%:\n")
      geo_results %>%
        filter(precision >= 0.50) %>%
        arrange(desc(dollar_recall)) %>%
        head(5) %>%
        select(earned_by_hh_size_thresh, rawben_rel_max_thresh_lower, rawben_rel_max_thresh_upper,
               shelter_by_hh_size_thresh,
               dollar_recall, precision, dollars_per_case, 
               n_flagged, pct_flagged) %>%
        mutate(
          dollar_recall = paste0(round(dollar_recall * 100, 1), "%"),
          precision = paste0(round(precision * 100, 1), "%"),
          dollars_per_case = paste0("$", round(dollars_per_case, 0)),
          pct_flagged = paste0(round(pct_flagged * 100, 1), "%")
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


# Find optimal thresholds for each geography
find_optimal_underissuance_thresholds <- function(grid_results, 
                                                   optimize_for = "dollars_per_case",  # Changed default to dollars_per_case
                                                   min_precision = 0.50,
                                                   max_flagged_pct = 0.30,
                                                   min_dollar_recall = 0.0) {  # Optional: ensure minimum recall
  
  optimal <- grid_results %>%
    filter(precision >= min_precision,
           pct_flagged <= max_flagged_pct,
           dollar_recall >= min_dollar_recall) %>%
    group_by(geography_type, geography_value) %>%
    arrange(desc(!!sym(optimize_for))) %>%
    slice(1) %>%
    ungroup() %>%
    select(geography_type, geography_value, geography_total_error,
           earned_by_hh_size_thresh, rawben_rel_max_thresh_lower, rawben_rel_max_thresh_upper,
           shelter_by_hh_size_thresh,
           dollar_recall, error_dollars_captured, total_error_dollars,
           precision, n_flagged, n_sig_errors_flagged, 
           pct_flagged, dollars_per_case, total_n)
  
  return(optimal)
}


# Example usage:
# 
# # Run grid search by state
# results <- run_underissuance_grid_by_geography(
#   data = income_and_clean_data,
#   geography_var = "state",
#   error_amount_var = "total_error_amount",
#   overthreshold_var = "overthreshold",
#   earned_by_hh_size_range = seq(0, 600, by = 50),
#   rawben_rel_max_range = seq(0.5, 1.0, by = 0.05),
#   shelter_by_hh_size_range = seq(0, 800, by = 100),
#   min_error_dollars = 5000
# )
# 
# # Option 1: Maximize dollars per case (efficiency) while maintaining precision
# optimal <- find_optimal_underissuance_thresholds(
#   grid_results = results,
#   optimize_for = "dollars_per_case",  # Maximize efficiency
#   min_precision = 0.50,                # Keep precision above 50%
#   max_flagged_pct = 0.30,              # Don't flag more than 30% of cases
#   min_dollar_recall = 0.20             # Optional: ensure at least 20% recall
# )
# 
# # Option 2: Maximize dollar recall (coverage) while maintaining precision
# optimal_recall <- find_optimal_underissuance_thresholds(
#   grid_results = results,
#   optimize_for = "dollar_recall",      # Maximize coverage
#   min_precision = 0.50,
#   max_flagged_pct = 0.30
# )
# 
# # Option 3: Balance between efficiency and coverage
# # First filter to reasonable recall, then maximize efficiency
# optimal_balanced <- find_optimal_underissuance_thresholds(
#   grid_results = results,
#   optimize_for = "dollars_per_case",
#   min_precision = 0.50,
#   max_flagged_pct = 0.30,
#   min_dollar_recall = 0.30             # Require at least 30% recall
# )
# 
# # View results
# optimal %>%
#   arrange(desc(dollars_per_case)) %>%
#   print()
# 
# # Save results
# write.csv(results, "underissuance_grid_search_full.csv", row.names = FALSE)
# write.csv(optimal, "underissuance_optimal_thresholds.csv", row.names = FALSE)
