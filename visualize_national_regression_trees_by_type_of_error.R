
##written with some Claude Code support - particularly for functions

library(rpart)
library(rpart.plot)
library(dplyr)

source("tree_visualization_helpers/regression_tree_plots_simplified.R")
source("tree_visualization_helpers/regression_tree_functions.R")

income_and_clean_data <- reg_model_data %>%
  filter(year %in% c("2022","2023","2024")) %>%
  mutate(
    deductions_by_hh_size    = fstotded / cert_HH_size_FS_n,
    HH_size_rel_cert_HH_size = HH_size_n / cert_HH_size_FS_n
  )

cat(sprintf("\n=== Data Summary ===\n"))
income_and_clean_data %>%
  mutate(
    category = case_when(
      is.na(error_status) ~ "No error",
      TRUE ~ error_status
    )
  ) %>%
  count(category) %>%
  mutate(pct = n / sum(n) * 100) %>%
  print()


# â”€â”€ Define predictor variables â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

features <- c(
  "cert_HH_size_FS_n",            # certified household size
  "HH_size_rel_cert_HH_size",     # ratio of people in HH to cert HH size
  "children_i",                   # children indicator
  "elderly_disabled_i",          # combined indicator (was elderly_or_disabled_i)
  "deductions_by_hh_size",          # deductions by HH size
  "expedited_i",                  # expedited service
  "bbce_state_i",                 # state runs BBCE (replaced cat_elig 2026-08-13; FY2024 recode)
  "rawben_rel_max",
  "medical_deductions",           # was med_expenses
  "shelter_expenses",
  "utilities",
  "married",
  "shelter_to_gross_ratio",       # was shelter_to_gross_income_ratio
  "homeless",
  "earned_by_hh_size",
  "unearned_by_hh_size",
  "gross_by_hh_size",
  "lf_composition",
  "n_income_types", 
  "n_deduction_types", 
  "count_divisible_by_100",       # the frame has no count_divisible_by_10; this
                                  # is a different threshold, not a rename
#  "DemonstrationsElderlyDisability",  # comes from snap_state_options_2023.csv,
#  which the current munging script does not join onto the frame
#  "rawben_no_cap_rel_max",
  "months_since_cert_n"
)


tree_results <- list()
tree_models <- list()

error_types <- c("earned_overissuance", "underissuance", "unearned_overissuance")

for (error_type in error_types) {
  
  #cat("\n\nâ•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•\n")
  #cat("Processing:", error_type, "\n")
  #cat("â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•\n\n")
  
  # Filter to this error type + clean cases only
  # Excludes other income error types and non-income errors
  # status_clean was tolower(trimws(status)) back when status carried its value
  # labels; the saved frame keeps status as the raw code, where the munging
  # script reads 2 as overissuance and 3 as underissuance, leaving 1 as the
  # "amount correct" cases this filter wants.
  subset_data <- income_and_clean_data %>%
    filter(error_status == error_type | status == 1)
  
  if (nrow(subset_data) == 0) {
    cat("  No cases for", error_type, "â€” skipping\n")
    next
  }
  
  cat(sprintf("  N = %d\n", nrow(subset_data)))
  cat(sprintf("  Mean error: $%.2f\n", mean(subset_data$total_error_amount, na.rm = TRUE)))
  cat(sprintf("  Median error: $%.2f\n", median(subset_data$total_error_amount, na.rm = TRUE)))
  
  # Use only the specified features that exist in the data
  predictor_vars <- features[features %in% names(subset_data)]
  
  # Check for and remove any constant or all-NA predictors
  predictor_vars <- predictor_vars[sapply(subset_data[predictor_vars], function(x) {
    !all(is.na(x)) && length(unique(x[!is.na(x)])) > 1
  })]

  cat(sprintf("  Predictors: %d variables\n", length(predictor_vars)))
  
  formula <- as.formula(paste("total_error_amount ~", paste(predictor_vars, collapse = " + ")))
  
  # Fit regression tree
  tree_model <- rpart(
    formula,
    data = subset_data,
    method = "anova",
    control = rpart.control(
      cp = 0.000001,
      minsplit = 20,
      maxdepth = 6,
      xval = 10
    )
  )
  
  n_splits <- sum(tree_model$frame$var != "<leaf>")
  cat(sprintf("  Splits created: %d\n", n_splits))
  
  if (n_splits == 0) {
    cat("  Warning: Tree is a stump (no splits)\n")
  }
  
  # Store model
  tree_models[[error_type]] <- tree_model
  
  # Get split info
  split_info <- get_tree_split_info(tree_model, n_top = 10)
  
  # Calculate metrics
  error_capture <- calculate_error_capture_by_depth(tree_model, subset_data, 
                                                    "total_error_amount", max_depth = 6)
  fpr <- calculate_fpr(tree_model, subset_data, "total_error_amount")
  
  predicted_vals <- predict(tree_model, subset_data)
  total_actual <- sum(subset_data$total_error_amount, na.rm = TRUE)
  total_predicted <- sum(predicted_vals, na.rm = TRUE)
  
  # Summary results
  results <- split_info %>%
    mutate(
      error_type = error_type,
      n_cases = nrow(subset_data),
      mean_error = mean(subset_data$total_error_amount, na.rm = TRUE),
      median_error = median(subset_data$total_error_amount, na.rm = TRUE),
      total_actual_error = total_actual,
      total_predicted_error = total_predicted,
      fpr = fpr,
      error_capture_depth_1 = error_capture[1],
      error_capture_depth_2 = error_capture[2],
      error_capture_depth_3 = error_capture[3],
      error_capture_depth_4 = error_capture[4],
      error_capture_depth_5 = error_capture[5],
      error_capture_depth_6 = error_capture[6],
      .before = everything()
    )
  
  tree_results[[error_type]] <- results
  
  # Plot tree
  plot_path <- paste0("income_error_trees/", error_type, "_2022_2024_any_timeper_small.png")
  dir.create("income_error_trees", showWarnings = FALSE, recursive = TRUE)
  
  plot_pooled_tree(
    tree_model = tree_model,
    main_title = paste("Regression Tree â€”", 
                       gsub("_", " ", tools::toTitleCase(error_type)),
                       "Errors"),
    predictor_vars = predictor_vars,
    save_path = plot_path,
    width_inches = 75,
    height_inches = 20,
    dpi = 300
  )
}

combined_results <- bind_rows(tree_results)

combined_results %>%
  select(error_type, n_cases, mean_error, 
         error_capture_depth_1:error_capture_depth_6) %>%
  distinct() %>%
  print()

write.csv(combined_results, "income_error_trees/tree_split_results.csv", row.names = FALSE)


