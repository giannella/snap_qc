library(ggplot2)
library(dplyr)

# Function to generate PR curve data for a single state
generate_pr_curve <- function(state_data, 
                              income_range, 
                              rawben_range, 
                              shelter_range) {
  
  # Check if there's any data
  if(nrow(state_data) == 0 || sum(state_data$error_i == 1) == 0) {
    return(data.frame())
  }
  
  # Create grid of threshold combinations
  threshold_grid <- expand.grid(
    income_thresh = income_range,
    rawben_thresh = rawben_range,
    shelter_thresh = shelter_range
  )
  
  # Calculate metrics for each combination
  results <- threshold_grid %>%
    rowwise() %>%
    mutate(
      tp = sum(state_data$earned_income >= income_thresh & 
                 state_data$rawben_rel_max >= rawben_thresh & 
                 state_data$shelter_to_gross_income_ratio < shelter_thresh &
                 state_data$error_i == 1, na.rm = TRUE),
      
      fp = sum(state_data$earned_income >= income_thresh & 
                 state_data$rawben_rel_max >= rawben_thresh & 
                 state_data$shelter_to_gross_income_ratio < shelter_thresh &
                 state_data$error_i == 0, na.rm = TRUE),
      
      fn = sum(!(state_data$earned_income >= income_thresh & 
                   state_data$rawben_rel_max >= rawben_thresh & 
                   state_data$shelter_to_gross_income_ratio < shelter_thresh) &
                 state_data$error_i == 1, na.rm = TRUE),
      
      tn = sum(!(state_data$earned_income >= income_thresh & 
                   state_data$rawben_rel_max >= rawben_thresh & 
                   state_data$shelter_to_gross_income_ratio < shelter_thresh) &
                 state_data$error_i == 0, na.rm = TRUE),
      
      # Calculate rates
      recall = ifelse(tp + fn > 0, tp / (tp + fn), 0),
      precision = ifelse(tp + fp > 0, tp / (tp + fp), 0),
      fpr = ifelse(fp + tn > 0, fp / (fp + tn), 0),
      n_pred_pos = tp + fp
    ) %>%
    ungroup() %>%
    filter(n_pred_pos > 0)  # Only keep combinations that make predictions
  
  return(results)
}

# Generate PR curves for all states (or select specific ones)
# Use same ranges as optimization
income_range <- seq(200, 1300, by = 50)
rawben_range <- seq(0.1, 1.0, by = .05)
shelter_range <- seq(0, 4, by = 0.05)

# Generate for all states (this may take a while)
pr_curves <- model_data %>%
  group_by(state) %>%
  #filter(n() >= 30, sum(error_i == 1) >= 5) %>%
  group_modify(~ generate_pr_curve(
    .x,
    income_range = income_range,
    rawben_range = rawben_range,
    shelter_range = shelter_range
  )) %>%
  ungroup()

# Plot 1: Faceted PR curves for all states
ggplot(pr_curves, aes(x = recall, y = precision)) +
  #geom_line(alpha = 0.3) +
  geom_point(alpha = 0.1, size = 0.5) +
  facet_wrap(~ state, scales = "free") +
  theme_minimal() +
  labs(title = "Precision-Recall Performance across Potential Thresholds for Key Criteria",
       x = "Recall (True Positive Rate)",
       y = "Precision") + xlim(0,1) + ylim(0,1) +
  theme(strip.text = element_text(size = 8))
ggsave(filename="state_specific_PR_points.png", device="png", width = 20, height = 20, units = "in", dpi=300)


# Plot 2: All states on one plot (colored by state)
ggplot(pr_curves, aes(x = recall, y = precision, color = state)) +
  geom_line(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Precision-Recall Curves - All States",
       x = "Recall (True Positive Rate)",
       y = "Precision") +
  theme(legend.position = "none")

# Plot 3: Select specific states for clearer visualization
top_states <- model_data %>%
  group_by(state) %>%
  summarise(n_errors = sum(error_i == 1)) %>%
  arrange(desc(n_errors)) %>%
  slice(1:9) %>%  # Top 9 states by number of errors
  pull(state)

pr_curves %>%
  filter(state %in% top_states) %>%
  ggplot(aes(x = recall, y = precision, color = state)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ state, ncol = 3) +
  theme_minimal() +
  labs(title = "Precision-Recall Curves - Top 9 States by Error Count",
       x = "Recall (TPR)",
       y = "Precision") +
  theme(legend.position = "none",
        strip.text = element_text(size = 10, face = "bold"))


ggsave(filename="state_comparisons_PR.png", device="png", width = 20, height = 20, units = "in", dpi=300)

# Plot 4: Interactive plot for exploring specific states
# Add FPR as color gradient to see the constraint visually
pr_curves %>%
  filter(state %in% top_states) %>%
  ggplot(aes(x = recall, y = precision, color = fpr)) +
  geom_point(alpha = 0.5, size = 1.5) +
  scale_color_gradient(low = "blue", high = "red", 
                       name = "FPR",
                       limits = c(0, 1)) +
  facet_wrap(~ state, ncol = 3) +
  theme_minimal() +
  labs(title = "Precision-Recall Curves with FPR Gradient",
       subtitle = "Blue = low FPR, Red = high FPR",
       x = "Recall (TPR)",
       y = "Precision")

# Create a summary table showing the tradeoff options
# For each state, show different precision/recall options
tradeoff_summary <- pr_curves %>%
  group_by(state) %>%
  arrange(desc(recall)) %>%
  mutate(
    option = case_when(
      precision >= 0.75 ~ "High Precision (≥75%)",
      precision >= 0.5 ~ "Medium Precision (>50%)",
      precision >= 0.33 ~ "Low Precision (33%)",
      TRUE ~ "Very Low Precision (<33%)"
    )
  ) %>%
  group_by(state, option) %>%
  slice_max(recall, n = 1) %>%  # Best recall for each precision tier
  ungroup() %>%
  select(state, option, precision, recall, fpr, n_pred_pos, 
         income_thresh, rawben_thresh, shelter_thresh)

# View the tradeoff options
tradeoff_summary %>%
  arrange(state, desc(precision)) %>%
  print(n = 100)

# Create a decision-support visualization
# Show precision vs recall vs FPR constraint for each state
pr_curves %>%
  filter(state %in% top_states) %>%
  mutate(meets_fpr_constraint = fpr < 0.5) %>%
  ggplot(aes(x = recall, y = precision)) +
  geom_point(aes(color = meets_fpr_constraint, alpha = meets_fpr_constraint), 
             size = 1) +
  scale_color_manual(values = c("TRUE" = "darkgreen", "FALSE" = "red"),
                     name = "FPR < 0.5") +
  scale_alpha_manual(values = c("TRUE" = 0.6, "FALSE" = 0.2),
                     guide = "none") +
  facet_wrap(~ state, ncol = 3) +
  theme_minimal() +
  labs(title = "Precision-Recall Trade-offs by State",
       subtitle = "Green points meet FPR < 0.5 constraint",
       x = "Recall (TPR)",
       y = "Precision")

# Export PR curve data for further analysis
write.csv(pr_curves, "state_pr_curves.csv", row.names = FALSE)
write.csv(tradeoff_summary, "state_tradeoff_options.csv", row.names = FALSE)