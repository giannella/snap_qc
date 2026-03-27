library(dplyr)

# ── Apply optimal thresholds to test data ────────────────────────────────────
#
# Takes optimal thresholds derived from training data and applies them to
# test data (e.g., 2023 data) to evaluate performance
#
# optimal_thresholds: output from find_optimal_overissuance_thresholds()
# test_data: new data to apply the rules to (e.g., 2023 data)

apply_optimal_overissuance_thresholds_to_test <- function(optimal_thresholds,
                                                           test_data,
                                                           geography_var     = "state",
                                                           error_amount_var  = "total_error_amount",
                                                           overthreshold_var = "overthreshold",
                                                           error_finding_var = "e_findg1") {

  # Get unique geographies from optimal thresholds
  geographies <- unique(optimal_thresholds$geography_value)

  all_results <- list()

  for (geo in geographies) {
    cat("\n", toupper(geography_var), ":", geo, "\n")
    cat(rep("=", 60), "\n", sep = "")

    # Get the optimal thresholds for this geography
    geo_thresholds <- optimal_thresholds %>%
      filter(geography_value == geo)

    if (nrow(geo_thresholds) == 0) {
      cat("  No optimal thresholds found for this geography. Skipping.\n")
      next
    }

    # Extract threshold values
    earned_thresh       <- geo_thresholds$earned_by_hh_size_thresh[1]
    gross_thresh_lower  <- geo_thresholds$gross_by_hh_size_thresh_lower[1]
    gross_thresh_upper  <- geo_thresholds$gross_by_hh_size_thresh_upper[1]
    deductions_thresh   <- geo_thresholds$deductions_by_hh_size_thresh[1]

    cat("  Applying thresholds:\n")
    cat("    earned_by_hh_size >=", earned_thresh, "\n")
    cat("    gross_by_hh_size  >=", gross_thresh_lower, "and <", gross_thresh_upper, "\n")
    cat("    deductions_by_hh_size >=", deductions_thresh, "\n\n")

    # Filter test data to this geography (ALL cases, not just overissuance)
    geo_test_data <- test_data %>%
      filter(!!sym(geography_var) == geo)

    # Get overissuance subset for calculating total error
    overiss_test_data <- geo_test_data %>%
      filter(tolower(!!sym(error_finding_var)) == "overissuance")

    if (nrow(overiss_test_data) == 0) {
      cat("  No overissuance test data for this geography. Skipping.\n")
      next
    }

    # DEBUG: Check the data
    cat("  DEBUG INFO:\n")
    cat("    Rows in geo_test_data (all):", nrow(geo_test_data), "\n")
    cat("    Rows in overiss_test_data:", nrow(overiss_test_data), "\n")
    cat("    Non-NA error amounts:", sum(!is.na(overiss_test_data[[error_amount_var]])), "\n")
    cat("    Non-zero error amounts:", sum(overiss_test_data[[error_amount_var]] > 0, na.rm = TRUE), "\n")

    # Check if fywgt exists
    if (!"fywgt" %in% names(geo_test_data)) {
      cat("    WARNING: fywgt column not found in test data! Using weight of 1.\n")
      geo_test_data$fywgt    <- 1
      overiss_test_data$fywgt <- 1
    }

    cat("    Non-NA weights:", sum(!is.na(overiss_test_data$fywgt)), "\n")
    cat("    Non-zero weights:", sum(overiss_test_data$fywgt > 0, na.rm = TRUE), "\n")
    cat("    Sum of error_amount (unweighted):", sum(overiss_test_data[[error_amount_var]], na.rm = TRUE), "\n")
    cat("    Sum of fywgt:", sum(overiss_test_data$fywgt, na.rm = TRUE), "\n")

    # Calculate total error in test data (weighted)
    valid_rows       <- !is.na(overiss_test_data[[error_amount_var]]) & !is.na(overiss_test_data$fywgt)
    total_test_error <- sum(overiss_test_data[[error_amount_var]][valid_rows] * overiss_test_data$fywgt[valid_rows])
    total_test_cases <- nrow(geo_test_data)  # All cases for precision calculation
    n_test_sig_errors <- sum(overiss_test_data[[overthreshold_var]] == 1, na.rm = TRUE)

    cat("    Valid rows for weighting:", sum(valid_rows), "\n")
    cat("    Calculated total_test_error:", total_test_error, "\n")

    cat("\n  Test data summary:\n")
    cat("    Total cases (all):", total_test_cases, "\n")
    cat("    Total overissuance cases:", nrow(overiss_test_data), "\n")
    cat("    Total weighted error: $", format(round(total_test_error, 2), big.mark = ","), "\n", sep = "")
    cat("    Significant errors (overthreshold==1):", n_test_sig_errors, "\n\n")

    # Apply the thresholds to ALL cases
    flagged <- geo_test_data$earned_by_hh_size >= earned_thresh &
               geo_test_data$gross_by_hh_size  >= gross_thresh_lower &
               geo_test_data$gross_by_hh_size  <  gross_thresh_upper &
               geo_test_data$deductions_by_hh_size >= deductions_thresh

    flagged[is.na(flagged)] <- FALSE

    # Calculate performance metrics
    # For RECALL: count overissuance errors captured (weighted)
    is_underissuance      <- tolower(geo_test_data[[error_finding_var]]) == "underissuance"
    is_overissuance       <- tolower(geo_test_data[[error_finding_var]]) == "overissuance"
    is_any_issuance_error <- is_underissuance | is_overissuance

    issuance_errors_flagged <- flagged & is_any_issuance_error

    # For weighted: only use rows where BOTH error_amount and fywgt are non-NA
    valid_issuance_flagged <- issuance_errors_flagged &
                              !is.na(geo_test_data[[error_amount_var]]) &
                              !is.na(geo_test_data$fywgt)
    error_dollars_captured <- sum(
      geo_test_data[[error_amount_var]][valid_issuance_flagged] * geo_test_data$fywgt[valid_issuance_flagged]
    )

    # Calculate total significant error (weighted) for this geography - for recall denominator
    sig_errors_in_geo <- geo_test_data[[overthreshold_var]] == 1 &
                         !is.na(geo_test_data[[error_amount_var]]) &
                         !is.na(geo_test_data$fywgt)
    total_sig_error_weighted <- sum(
      geo_test_data[[error_amount_var]][sig_errors_in_geo] * geo_test_data$fywgt[sig_errors_in_geo]
    )

    # For PRECISION: count significant errors among flagged cases
    sig_errors_flagged_logical <- flagged & geo_test_data[[overthreshold_var]] == 1
    error_dollars_sig_only     <- sum(geo_test_data[[error_amount_var]][sig_errors_flagged_logical], na.rm = TRUE)

    n_flagged            <- sum(flagged, na.rm = TRUE)
    n_sig_errors_flagged <- sum(geo_test_data[[overthreshold_var]][flagged] == 1, na.rm = TRUE)
    n_errors_flagged     <- sum(geo_test_data[[error_amount_var]][flagged] > 0, na.rm = TRUE)
    # False positives = cases that are NOT significant errors OR are correct
    n_false_positives    <- sum(
      (geo_test_data[[overthreshold_var]][flagged] != 1 | geo_test_data[[error_amount_var]][flagged] == 0),
      na.rm = TRUE
    )

    # Store results
    geo_result <- data.frame(
      geography_type  = geography_var,
      geography_value = geo,

      # Training thresholds
      earned_by_hh_size_thresh       = earned_thresh,
      gross_by_hh_size_thresh_lower  = gross_thresh_lower,
      gross_by_hh_size_thresh_upper  = gross_thresh_upper,
      deductions_by_hh_size_thresh   = deductions_thresh,

      # Training performance (from optimal table)
      train_total_error    = geo_thresholds$geography_total_error[1],
      train_error_captured = geo_thresholds$error_dollars_captured[1],
      train_dollar_recall  = geo_thresholds$dollar_recall[1],
      train_precision      = geo_thresholds$precision[1],
      train_n_flagged      = geo_thresholds$n_flagged[1],
      train_pct_flagged    = geo_thresholds$pct_flagged[1],

      # Test performance
      test_total_error          = total_sig_error_weighted,
      test_error_captured       = error_dollars_captured,
      test_dollar_recall        = ifelse(total_sig_error_weighted > 0, error_dollars_captured / total_sig_error_weighted, 0),
      test_precision            = ifelse(n_flagged > 0, n_sig_errors_flagged / n_flagged, 0),
      test_n_flagged            = n_flagged,
      test_n_sig_errors_flagged = n_sig_errors_flagged,
      test_n_errors_flagged     = n_errors_flagged,
      test_n_false_positives    = n_false_positives,
      test_pct_flagged          = n_flagged / total_test_cases,
      test_total_cases          = total_test_cases,
      test_dollars_per_case     = ifelse(n_sig_errors_flagged > 0, error_dollars_sig_only / n_sig_errors_flagged, 0),

      # Performance comparison
      recall_change    = (error_dollars_captured / total_sig_error_weighted) - geo_thresholds$dollar_recall[1],
      precision_change = (n_sig_errors_flagged / ifelse(n_flagged > 0, n_flagged, 1)) - geo_thresholds$precision[1],

      stringsAsFactors = FALSE
    )

    all_results[[geo]] <- geo_result

    # Print test results
    cat("  Test results:\n")
    cat("    Flagged cases:", n_flagged, "(", round(n_flagged / total_test_cases * 100, 1), "%)\n", sep = "")
    cat("    Weighted error captured: $", format(round(error_dollars_captured, 2), big.mark = ","), "\n", sep = "")
    cat("    Dollar recall:", round(error_dollars_captured / total_test_error * 100, 1), "%\n", sep = "")
    cat("    Precision:", round(ifelse(n_flagged > 0, n_sig_errors_flagged / n_flagged, 0) * 100, 1), "%\n", sep = "")

    # Compare to training
    cat("\n  Performance vs training:\n")
    cat("    Dollar recall change:",
        sprintf("%+.1f%%", ((error_dollars_captured / total_test_error) - geo_thresholds$dollar_recall[1]) * 100),
        "\n")
    cat("    Precision change:",
        sprintf("%+.1f%%", ((n_sig_errors_flagged / ifelse(n_flagged > 0, n_flagged, 1)) - geo_thresholds$precision[1]) * 100),
        "\n")
  }

  # Combine all results
  results_df <- bind_rows(all_results)

  # Print overall summary
  cat("\n\n", rep("=", 60), "\n", sep = "")
  cat("OVERALL TEST PERFORMANCE SUMMARY\n")
  cat(rep("=", 60), "\n", sep = "")

  cat("\nWeighted average performance:\n")
  cat("  Dollar recall (test):",
      round(weighted.mean(results_df$test_dollar_recall, results_df$test_total_error, na.rm = TRUE) * 100, 1),
      "%\n", sep = "")
  cat("  Precision (test):",
      round(weighted.mean(results_df$test_precision, results_df$test_n_flagged, na.rm = TRUE) * 100, 1),
      "%\n", sep = "")
  cat("  Total test error captured: $",
      format(round(sum(results_df$test_error_captured, na.rm = TRUE), 2), big.mark = ","),
      "\n", sep = "")
  cat("  Total test error: $",
      format(round(sum(results_df$test_total_error, na.rm = TRUE), 2), big.mark = ","),
      "\n", sep = "")

  cat("\nGeographies tested:", nrow(results_df), "\n")
  cat("Mean recall change:", sprintf("%+.1f%%", mean(results_df$recall_change, na.rm = TRUE) * 100), "\n")
  cat("Mean precision change:", sprintf("%+.1f%%", mean(results_df$precision_change, na.rm = TRUE) * 100), "\n")

  return(results_df)
}


# ── Example usage ─────────────────────────────────────────────────────────────
#
# # Step 1: Get optimal thresholds from training data (e.g., 2022)
# train_results <- run_overissuance_grid_by_geography(
#   data                        = train_data_2022,
#   geography_var               = "state",
#   error_amount_var            = "total_error_amount",
#   overthreshold_var           = "overthreshold",
#   earned_by_hh_size_range     = seq(0, 600, by = 50),
#   gross_by_hh_size_range      = seq(0, 1000, by = 100),
#   deductions_by_hh_size_range = seq(0, 800, by = 100),
#   min_error_dollars           = 5000
# )
#
# optimal <- find_optimal_overissuance_thresholds(
#   grid_results      = train_results,
#   optimize_for      = "dollar_recall",
#   min_precision     = 0.50,
#   max_flagged_pct   = 0.30
# )
#
# # Step 2: Apply to test data (e.g., 2023)
# test_results <- apply_optimal_overissuance_thresholds_to_test(
#   optimal_thresholds = optimal,
#   test_data          = test_data_2023,
#   geography_var      = "state",
#   error_amount_var   = "total_error_amount",
#   overthreshold_var  = "overthreshold",
#   error_finding_var  = "status_c"   # or "e_findg1" depending on your test data
# )
#
# # Step 3: View and save results
# test_results %>%
#   arrange(desc(test_dollar_recall)) %>%
#   print()
#
# write.csv(test_results, "test_performance_overissuance_2023.csv", row.names = FALSE)
#
# # Step 4: Identify geographies where performance degraded
# test_results %>%
#   filter(recall_change < -0.05 | precision_change < -0.05) %>%
#   select(geography_value, train_dollar_recall, test_dollar_recall, recall_change,
#          train_precision, test_precision, precision_change) %>%
#   arrange(recall_change)
