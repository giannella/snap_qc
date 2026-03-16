

reg_model_data <- df_cert_only %>% select("payment_error_i",features,"rescaled_weight","race_ethnicity","total_error_amount", "unit_composition_error", "element1", "error_flag")

table(reg_model_data$error_flag, reg_model_data$payment_error_i)

reg_model_data <- reg_model_data %>% filter(!(payment_error_i=="No" & error_flag==1))

reg_model_data <- reg_model_data %>% select(-c(HH_head_LF_status_c, ReportingRequirements, BBCE, rescaled_weight, race_ethnicity, unit_composition_error,
                                       gross_inc_to_poverty_FS, raw_gross, raw_net, wages_salaries, utilities, regioncd, payment_error_i, error_flag))

reg_model_data$rawben_rel_max[reg_model_data$rawben_rel_max>1] <- 1
table(model_data$rawben_rel_max>1)
#model_data$rawben_rel_max <- NULL

table(reg_model_data$element1)
#379 errors where rawben_rel_max is higher than 1, suggesting a HH composition error
#model_data$year <- NULL
reg_model_data$element1 <- NULL

reg_model_data$cat_elig <- as.integer(reg_model_data$cat_elig!="0")
reg_model_data$non_elderly_disabled_i <- as.integer(reg_model_data$non_elderly_disabled_i=="TRUE")
reg_model_data$homeless <- as.integer(reg_model_data$homeless=="TRUE")
reg_model_data$rent_mortage <- NULL
reg_model_data <- drop_na(reg_model_data) 

summary(reg_model_data)

## should be revised to be about number of errors and avg dollars per error, but currently
# measures what proportion of total error dollars are captured by predictions at each depth
calculate_error_capture_by_depth <- function(tree_model, data, outcome_var, max_depth = 5) {
  
  frame <- tree_model$frame
  
  actual <- as.numeric(data[[outcome_var]])
  
  if (all(is.na(actual))) {
    return(rep(NA, max_depth))
  }
  
  total_error_amount <- sum(actual, na.rm = TRUE)
  
  if (is.na(total_error_amount) || total_error_amount == 0) {
    return(rep(NA, max_depth))
  }
  
  # Get node assignments for each observation
  node_assignments <- as.numeric(rownames(tree_model$frame)[tree_model$where])
  
  # Calculate depth for each node using binary node numbering
  node_depths <- floor(log2(as.numeric(rownames(frame))))
  
  # Get predicted values for all observations
  predicted_vals <- predict(tree_model, data)
  error_capture <- numeric(max_depth)
  for (d in 1:max_depth) {
    
    # Nodes at this depth or shallower
    nodes_at_depth <- which(node_depths <= d)
    
    # Observations assigned to those nodes
    obs_in_nodes <- which(tree_model$where %in% nodes_at_depth)
    
    # Sum of actual error amounts for obs where the tree predicts a non-zero amount
    # "Captured" = tree assigns a positive predicted value AND the case has actual error
    errors_captured <- sum(
      actual[obs_in_nodes[predicted_vals[obs_in_nodes] > 0]],
      na.rm = TRUE
    )
    
    error_capture[d] <- errors_captured / total_error_amount
  }
  
  return(error_capture)
}


# Calculate false positive rate in dollar terms
# FP rate = predicted error dollars on true-zero cases / total predicted error dollars
calculate_fpr <- function(tree_model, data, outcome_var) {
  
  actual      <- as.numeric(data[[outcome_var]])
  predicted   <- predict(tree_model, data)
  
  if (all(is.na(actual)) || all(is.na(predicted))) return(NA)
  
  # Observations the tree flags as having an error (predicted amount > 0)
  flagged <- predicted > 0
  
  # Of flagged observations, how many are true zeros?
  false_positives_amt <- sum(predicted[flagged & actual == 0], na.rm = TRUE)
  total_predicted_amt <- sum(predicted[flagged],               na.rm = TRUE)
  
  if (is.na(total_predicted_amt) || total_predicted_amt == 0) return(NA)
  
  fpr <- false_positives_amt / total_predicted_amt
  return(fpr)
}


# Extract split information with actual threshold values
get_tree_split_info <- function(tree_model, n_top = 5) {
  
  frame  <- tree_model$frame
  splits <- tree_model$splits
  
  # No splits produced (stump)
  if (is.null(splits) || length(splits) == 0 || nrow(frame) == 1) {
    return(data.frame(
      split_order  = 1:n_top,
      variable     = NA_character_,
      split_value  = NA_real_,
      node         = NA_integer_,
      node_mean    = NA_real_,
      stringsAsFactors = FALSE
    ))
  }
  
  split_info    <- list()
  split_counter <- 1
  
  for (i in seq_len(nrow(frame))) {
    if (frame$var[i] != "<leaf>") {
      
      if (!is.null(splits) && is.matrix(splits) && split_counter <= nrow(splits)) {
        
        split_info[[split_counter]] <- list(
          variable    = frame$var[i],
          split_value = splits[split_counter, "index"],
          node        = as.integer(rownames(frame)[i]),
          # Mean predicted error amount at this node (yval for regression trees)
          node_mean   = frame$yval[i]
        )
        split_counter <- split_counter + 1
      }
    }
  }
  
  top_splits <- split_info[seq_len(min(n_top, length(split_info)))]
  
  if (length(top_splits) > 0) {
    split_df <- data.frame(
      split_order = seq_along(top_splits),
      variable    = sapply(top_splits, `[[`, "variable"),
      split_value = sapply(top_splits, `[[`, "split_value"),
      node        = sapply(top_splits, `[[`, "node"),
      node_mean   = sapply(top_splits, `[[`, "node_mean"),
      stringsAsFactors = FALSE
    )
  } else {
    split_df <- data.frame(
      split_order = integer(0),
      variable    = character(0),
      split_value = numeric(0),
      node        = integer(0),
      node_mean   = numeric(0),
      stringsAsFactors = FALSE
    )
  }
  
  # Pad to n_top rows
  if (nrow(split_df) < n_top) {
    padding <- data.frame(
      split_order = (nrow(split_df) + 1):n_top,
      variable    = NA_character_,
      split_value = NA_real_,
      node        = NA_integer_,
      node_mean   = NA_real_,
      stringsAsFactors = FALSE
    )
    split_df <- rbind(split_df, padding)
  }
  
  return(split_df)
}


# Main function: fit regression trees and extract comprehensive results
fit_trees_comprehensive <- function(data, outcome_var, state_var = "state", n_top = 5,
                                    maxdepth = 5, minsplit = 20, cp = 0.001) {
  
  results_list <- list()
  tree_models  <- list()
  
  states <- unique(data[[state_var]])
  
  for (s in states) {
    cat("Processing state:", s, "\n")
    
    state_data <- data %>% filter(!!sym(state_var) == s)
    
    # Outcome must be numeric for regression trees
    state_data[[outcome_var]] <- as.numeric(state_data[[outcome_var]])
    
    # Report zero-inflation rate as a diagnostic
    zero_pct <- mean(state_data[[outcome_var]] == 0, na.rm = TRUE)
    cat(sprintf("  Zero rate: %.1f%%  |  N = %d  |  Mean error (non-zero): $%.2f\n",
                zero_pct * 100,
                nrow(state_data),
                mean(state_data[[outcome_var]][state_data[[outcome_var]] > 0], na.rm = TRUE)))
    
    predictor_vars <- setdiff(names(state_data), c(state_var, outcome_var))
    formula <- as.formula(paste(outcome_var, "~", paste(predictor_vars, collapse = " + ")))
    
    # Regression tree (method = "anova")
    # With ~90% zeros, cp and minsplit may need tuning per state;
    # these defaults are a reasonable starting point.
    tree_model <- rpart(
      formula,
      data    = state_data,
      method  = "anova",
      control = rpart.control(
        cp       = cp,
        minsplit = minsplit,
        maxdepth = maxdepth,
        xval     = 10
      )
    )
    
    n_splits <- sum(tree_model$frame$var != "<leaf>")
    if (n_splits == 0) {
      cat("  Warning: No splits created for", s, "\n")
    }
    
    tree_models[[s]] <- tree_model
    
    split_info   <- get_tree_split_info(tree_model, n_top = n_top)
    error_capture <- calculate_error_capture_by_depth(tree_model, state_data, outcome_var, max_depth = 5)
    fpr          <- calculate_fpr(tree_model, state_data, outcome_var)
    
    # Summary stats on predicted vs actual error amounts
    predicted_vals   <- predict(tree_model, state_data)
    mean_pred_error  <- mean(predicted_vals[predicted_vals > 0], na.rm = TRUE)
    total_actual_err <- sum(state_data[[outcome_var]], na.rm = TRUE)
    total_pred_err   <- sum(predicted_vals, na.rm = TRUE)
    
    results_list[[s]] <- split_info %>%
      mutate(
        state                 = s,
        fpr                   = fpr,
        total_actual_error    = total_actual_err,
        total_predicted_error = total_pred_err,
        mean_predicted_error  = mean_pred_error,
        error_capture_depth_1 = error_capture[1],
        error_capture_depth_2 = error_capture[2],
        error_capture_depth_3 = error_capture[3],
        error_capture_depth_4 = error_capture[4],
        error_capture_depth_5 = error_capture[5]
      ) %>%
      select(state, split_order, variable, split_value, node, node_mean, fpr,
             total_actual_error, total_predicted_error, mean_predicted_error,
             error_capture_depth_1:error_capture_depth_5)
  }
  
  results_df <- bind_rows(results_list)
  
  return(list(
    results     = results_df,
    tree_models = tree_models
  ))
}

plot_state_tree <- function(tree_models, state_name,
                            main_title = NULL,
                            save_path = NULL,
                            width_inches = 30,
                            height_inches = 12,
                            dpi = 300) {
  
  if (!state_name %in% names(tree_models)) {
    stop(paste("State", state_name, "not found in tree models"))
  }
  
  tree_model <- tree_models[[state_name]]
  
  if (is.null(main_title)) {
    main_title <- paste("Regression Tree for Error Amount at Certification in", state_name, "2017-2022")
  }
  
  if (!is.null(save_path)) {
    width_px  <- width_inches * dpi
    height_px <- height_inches * dpi
    png(save_path, width = width_px, height = height_px, res = dpi)
  }
  
  # For regression trees:
  #   extra = 1  → show number of observations in each node
  #   extra = 101 → show n and mean response (most useful here)
  # type  = 4  → label all nodes with split variable and value
  # box.palette: blue gradient suits a continuous dollar amount;
  #   low values (near $0) are light, high values are dark blue.
  rpart.plot(tree_model,
             main          = main_title,
             type          = 4,
             extra         = 101,        # n + mean predicted error amount
             fallen.leaves = FALSE,
             branch.lty    = 1,
             shadow.col    = "gray",
             box.palette   = "Blues",    # continuous palette for dollar amounts
             cex           = 1,
             tweak         = 1.2)
  
  # ── Colour-scale legend (maps to mean predicted error amount) ──────────────
  blues <- colorRampPalette(c("#08306b", "#deebf7"))(100)  # dark at top (i=1), light at bottom (i=100)
  
  legend_x      <- 0.025
  legend_y      <- 0.975
  legend_width  <- 0.015
  legend_height <- 0.20
  
  for (i in 1:100) {
    rect(legend_x,
         legend_y - legend_height * (i / 100),
         legend_x + legend_width,
         legend_y - legend_height * ((i - 1) / 100),
         col    = blues[i],
         border = NA)
  }
  rect(legend_x, legend_y - legend_height,
       legend_x + legend_width, legend_y,
       border = "black")
  
  text(legend_x + legend_width + 0.002, legend_y,
       "High $", pos = 4, cex = 1.1)
  text(legend_x + legend_width + 0.002, legend_y - legend_height,
       "$0", pos = 4, cex = 1.1)
  text(legend_x + legend_width / 2, legend_y + 0.04,
       "Mean error amount", cex = 1.1, font = 2)
  
  # ── How-to-read annotation ────────────────────────────────────────────────
  text(x = 0.90, y = 0.98,
       paste0('How to read this: each node shows the mean predicted\n',
              'error amount ($) and the number of cases (n).\n',
              'Splits route cases toward higher or lower error amounts.\n',
              'Only public data excluding ineligible cases is used.'),
       cex = 1.1, adj = c(1, 1))
  
  if (!is.null(save_path)) {
    dev.off()
    cat("Tree plot saved to:", save_path, "\n")
  }
}


plot_all_trees <- function(tree_models, output_dir = "tree_plots",
                           width_inches = 30,
                           height_inches = 12,
                           dpi = 300) {
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  for (state_name in names(tree_models)) {
    file_path <- file.path(output_dir,
                           paste0(state_name, "_2017_2022_error_amount_regression_tree.png"))
    plot_state_tree(tree_models, state_name,
                    save_path     = file_path,
                    width_inches  = width_inches,
                    height_inches = height_inches,
                    dpi           = dpi)
  }
  
  cat("All tree plots saved to:", output_dir, "\n")
}

rn_model_data <- reg_model_data
rn_model_data <- rn_model_data %>% rename(household_size = cert_HH_size_FS_n,
                                          HH_members_to_participating = HH_size_rel_cert_HH_size,
                                          #labor_force_status = HH_head_LF_status_c,
                                          children_present = children_i,
                                          total_deductions = total_deductions_fs,
                                          elderly_present = elderly_i,
                                          disabled_present = non_elderly_disabled_i,
                                          expedited = expedited_i, 
                                          categorical_eligibility = cat_elig,
                                          benefit_relative_to_max_allotment = rawben_rel_max,
                                          earned_income = earned_income,
                                          household_member_works = anyone_working, 
                                          shelter_expenses_relative_to_gross_income = shelter_to_gross_income_ratio,
                                          medical_expenses = med_expenses
                                          
                                          
)
rn_model_data_2023 <- rn_model_data %>% filter(year %in% "2023")
rn_model_data_2023$year <- NULL
rn_model_data_2017_2022 <- rn_model_data %>% filter(year != "2023") 
rn_model_data_2017_2022$year <- NULL
rn_model_data_2017_2022$state <- NULL
rn_model_data$year <- NULL
rn_model_data$state <- NULL
rn_model_data$state <- "US"

# Usage example:
# ==============
results_output <- fit_trees_comprehensive(
  data = rn_model_data_2017_2022,
  outcome_var = "total_error_amount",
  state_var = "state",
  n_top = 5,
  maxdepth = 5,   # Maximum tree depth (default = 10)
  minsplit = 10,   # Minimum observations to split (default = 20)
  cp = 0.000001       # Complexity parameter (default = 0.001)
)



for(state in names(results_output$tree_models)) {
  results_output$tree_models[[state]]$frame$var <- 
    gsub("_", " ", results_output$tree_models[[state]]$frame$var)
}


# Access results dataframe
results_df <- results_output$results
write.csv(results_df, "state_split_vars.csv", row.names=F)

# Plot a specific state's tree (high quality)
plot_state_tree(results_output$tree_models, "District_of_Columbia",
                save_path = "DC_regtree_w_uncounted_2017_2022.png",
                width_inches = 60, height_inches = 16, dpi = 300)

# Plot all trees and save to files (high quality, 300 dpi, 14x12 inches)
plot_all_trees(results_output$tree_models,
               output_dir = "state_plots_2017_2023_ex_rule1")

