library(tidyverse)
library(rpart)
library(rpart.plot)

source("helper_functions/regression_tree_plots_simplified.R")
source("helper_functions/regression_tree_functions.R")

states <- unique(reg_model_data$state)

table(reg_model_data$element1)
names(reg_model_data)

#main decision here is whether to use rawben_no_cap_rel_max (harder to construct) vs. rawben_rel_max (easier to construct)
#those are defined in the 1_data_munging_and_income_var_recovery.R script

features <- c(
  "cert_HH_size_FS_n",            # certified household size
  "HH_size_rel_cert_HH_size",     # ratio of people in HH to cert HH size
  "children_i",                   # children indicator
  "elderly_or_disabled_i",       # combined indicator
  "deductions_by_hh_size",          # deductions by HH size
  "expedited_i",                  # expedited service
  "cat_elig",                     # categorical eligibility 
  "rawben_rel_max",
  "med_expenses",
  "shelter_expenses",
  "utilities",
  "married",
  "shelter_to_gross_income_ratio",
  "homeless",
  "earned_by_hh_size",
  "unearned_by_hh_size",
  "gross_by_hh_size",
  "lf_composition",
  "n_income_types", 
  "n_deduction_types", 
  "count_divisible_by_10",
  "DemonstrationsElderlyDisability",
#  "rawben_no_cap_rel_max",
 "months_since_cert_n"
)

#check that all features in the model since this will fail silently in the function
setdiff(features, names(reg_model_data))

tree_results <- list()
tree_models <- list()

for (state in states) {
  
  # Filter to this state
  subset_data <- reg_model_data %>%
    filter(state == !!state)
  
  if (nrow(subset_data) == 0) {
    cat("  No cases for", state, "— skipping\n")
    next
  }
  
  cat(sprintf("\n=== State: %s ===\n", state))
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
  
  if (length(predictor_vars) == 0) {
    cat("  No valid predictors — skipping\n")
    next
  }
  
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
  tree_models[[state]] <- tree_model
  
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
      state = state,
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
  
  tree_results[[state]] <- results
  
  # Plot tree
  plot_path <- paste0("state_income_error_trees_any_timeper/", state, "_reg_2017_2023_income_errors.png")
  dir.create("state_income_error_trees_any_timeper", showWarnings = FALSE, recursive = TRUE)
  
  plot_pooled_tree(
    tree_model    = tree_model,
    main_title    = paste("Regression Tree for Income Errors in", state),
    subset_data   = subset_data,
    predictor_vars = predictor_vars,
    save_path     = plot_path,                                              # PNG
    pdf_path      = sub("\\.png$", ".pdf", plot_path),                     # PDF (same name, .pdf)
    width_inches  = 50,
    height_inches = 20,
    dpi           = 300
  )
}

tree_results_df <- bind_rows(tree_results)

reg_model_data %>% group_by(state) %>% filter(total_error_amount>50) %>% tally() %>% print(n=100)

top5_splits <- tree_results_df %>%
  group_by(state) %>%
  filter(split_order <= 5) %>%
  select(state, split_order, variable, split_value, node_mean, n_cases) %>%
  arrange(state, split_order) %>%
  ungroup()

print(top5_splits)

state_splits <- top5_splits
write.csv(top5_splits, file="first5_splits_by_state.csv", row.names=F)

# tree_results_df %>%
#   group_by(state) %>%
#   slice_max(node_mean, n = 10) %>%
#   select(state, variable, split_value, node_mean, split_order) %>%
#   arrange(state, desc(node_mean))
# 
# library(purrr)
# 
# importance_df <- map_dfr(names(tree_models), function(s) {
#   imp <- tree_models[[s]]$variable.importance
#   if (is.null(imp)) return(NULL)
#   tibble(state = s, variable = names(imp), importance = imp)
# })
# 
# # Rank within each state
# importance_ranked <- importance_df %>%
#   group_by(state) %>%
#   mutate(rank = rank(-importance)) %>%
#   arrange(state, rank) %>%
#   ungroup()
# 
# print(importance_ranked)
# 
# top5_importance <- importance_ranked %>%
#   group_by(state) %>%
#   slice_min(rank, n = 5) %>%
#   select(state, rank, variable, importance)
# write.csv(top5_importance, file="top5_importance.csv", row.names=F)
#
