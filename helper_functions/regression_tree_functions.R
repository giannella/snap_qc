# Calculate error amount capture by depth
# Measures what proportion of total error dollars are captured by predictions at each depth
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
