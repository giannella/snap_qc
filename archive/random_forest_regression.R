library(ranger)
library(yardstick)
library(dplyr)

features <- c(
  "cert_HH_size_FS_n",            # certified household size
  "HH_size_rel_cert_HH_size",     # ratio of people in HH to cert HH size
  "children_i",                   # children indicator
  "elderly_i",                    # elderly indicator
#  "non_elderly_disabled_i",       # disabled indicator
  "deductions_by_hh_size",          # deductions by HH size
#  "expedited_i",                  # expedited service
  "cat_elig",                     # categorical eligibility 
  "rawben_rel_max",
  "med_expenses",
  "shelter_expenses",
  "utilities",
#  "married",
  "shelter_to_gross_income_ratio",
  "homeless",
  "earned_by_hh_size",
  "unearned_by_hh_size",
  "gross_by_hh_size",
  "lf_composition"
)

cores <- parallel::detectCores()

# ── Compare ───────────────────────────────────────────────────────────────────
delta <- augmented$pr_auc - baseline$pr_auc
cat(sprintf(
  "\nPR-AUC delta: %+.4f  (%s)\n",
  delta,
  ifelse(delta > 0, "augmented model is better", "baseline is better or equal")
))

### treating the outcome as continuous, we'll try to minimize RMSE
fit_and_evaluate <- function(formula, data, seed = 42, num.trees = 500) {
  set.seed(seed)
  
  model <- ranger(
    formula    = formula,
    data       = data,
    num.trees  = num.trees,
    importance = "impurity"
  )
  
  # OOB predictions
  results <- tibble(
    truth    = data[[as.character(formula[[2]])]],
    estimate = model$predictions
  )
  
  rmse_val <- rmse(results, truth = truth, estimate = estimate)
  
  list(
    model      = model,
    rmse       = rmse_val$.estimate,
    importance = sort(importance(model), decreasing = TRUE)
  )
}


baseline_features  <- unique(features)
baseline  <- fit_and_evaluate(reformulate(baseline_features,  "total_error_amount"), income_and_clean_data)

cat("Baseline RMSE:", round(baseline$rmse, 4), "\n")
print(baseline$importance)

#Ben, could imagine you make a loop that inserts new variables into augmented_features and then runs augmented, storing the PR-AUC results for each new variable. 
eval_table <- data.frame(row.names=c("feature","rmse","importance"))

FEATURE_i <- "rawben_rel_max" # just an example, delete
augmented_features <- unique(c(features, paste0(FEATURE_i)))
augmented <- fit_and_evaluate(reformulate(augmented_features, "over_threshold"), income_and_clean_data)

new_row_in_table <- data.frame(cbind(paste(FEATURE_i), baseline$rmse, which(names(baseline$importance) == paste(FEATURE_i))))
eval_table <- bind_rows(eval_table, new_row_in_table)





#for classification - types of errors in the data:
table(income_and_clean_data$error_status, income_and_clean_data$over_threshold)

####for classification####
# ── Helper: fit a ranger model and return OOB PR-AUC ──────────────────────────
fit_and_evaluate <- function(formula, data, seed = 111) {
  set.seed(seed)
  
  model <- ranger(
    formula      = formula,
    data         = data,
    num.trees    = 1000,
    min_n = 14,
    mtry = 3,
    importance   = "impurity",        # Gini impurity-based variable importance
    probability  = TRUE,              # needed to get class probabilities for PR-AUC
    oob.error    = TRUE,
    num.threads = cores
  )
  
  # OOB predicted probabilities for the positive class (assumes binary outcome)
  oob_probs <- model$predictions[, 2]
  
  # Build a results tibble for yardstick
  results <- tibble(
    truth    = data[[as.character(formula[[2]])]],   # extract outcome column
    estimate = oob_probs
  )
  
  # PR-AUC via yardstick (outcome must be a factor; positive class = first level)
  pr <- pr_auc(results, truth = truth, estimate = estimate, event_level = "second")
  
  list(
    model    = model,
    pr_auc   = pr$.estimate,
    importance = sort(importance(model), decreasing = TRUE)
  )
}

