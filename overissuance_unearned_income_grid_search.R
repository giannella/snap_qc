source("grid_search_overissuance_detection_unearned.R")

#── Example usage ─────────────────────────────────────────────────────────────

grid_search_data_2 <- grid_search_data

grid_search_data_2$shelter_expenses <- grid_search_data_2$shelter_by_hh_size

# Run grid search by state
results_unearn <- run_overissuance_grid_by_geography(
  data                        = grid_search_data_2[grid_search_data_2$state=="Washington",],
  geography_var               = "state",
  error_amount_var            = "total_error_amount",
  error_threshold             = 52,
  unearned_by_hh_size_range     = seq(0, 1000, by = 100),
  shelter_expenses_range      = seq(100, 1500, by = 100),
  deductions_by_hh_size_range = seq(2000, 2000, by = 2000),
  rawben_rel_max_range        = seq(0.4, 1, by = 0.05),
  hh_size_range              = 1,
  min_error_dollars           = 5000
)


# Option 2: Maximize dollar recall (coverage) while maintaining precision
optimal_recall <- find_optimal_overissuance_thresholds(
  grid_results    = results_unearn,
  optimize_for    = "dollar_recall",
  min_precision   = 0.4,
  max_flagged_pct = 1
)

##test with handcoding
grid_search_data <- grid_search_data %>% mutate(nc_flag2 = 
                                                  case_when(
                                                    (unearned_by_hh_size>= 450 & rawben_rel_max >= .4 & shelter_expenses > 1800) ~ TRUE,
                                                    TRUE ~ FALSE),
                                                dc_flag = 
                                                  case_when(
                                                    (unearned_by_hh_size>=  0 & rawben_rel_max >= .4 & shelter_expenses >= 600 & shelter_expenses<1600 & cert_HH_size_FS_n >=2) ~ TRUE,
                                                    TRUE ~ FALSE),
                                                ct_flag = 
                                                  case_when(
                                                    (earned_by_hh_size> 1 & rawben_rel_max > .6 & deductions_by_hh_size < 300) ~ TRUE,
                                                    TRUE ~ FALSE),
                                                wa_flag = 
                                                  case_when(
                                                    (unearned_by_hh_size>= 300 & rawben_rel_max > .85 & shelter_expenses >= 1400 & shelter_expenses<1800) ~ TRUE,
                                                    TRUE ~ FALSE),
                                                tn_flag = 
                                                  case_when(
                                                    (earned_by_hh_size> 1 &  deductions_by_hh_size < 62) ~ TRUE,
                                                    TRUE ~ FALSE))

grid_search_data %>% filter(state=="Tennessee" & (timeper1=="at time of most recent action by agency" |  is.na(timeper1))) %>% group_by(tn_flag, over_threshold) %>% tally()


grid_search_data %>% filter(state=="Washington" ) %>% group_by(wa_flag, over_threshold) %>% 
  summarize(mean(total_error_amount), n=n())


grid_search_data %>% filter(state=="North_Carolina" & status=="amount correct") %>% group_by(nc_flag2, over_threshold) %>% 
  summarize(mean(total_error_amount), n=n())


# Option 3: Balance between efficiency and coverage
optimal_balanced <- find_optimal_overissuance_thresholds(
  grid_results      = results_unearn,
  optimize_for      = "dollars_per_case",
  min_precision     = 0.33,
  max_flagged_pct   = 1,
  min_dollar_recall = 0.05
)

# View results
optimal %>%
  arrange(desc(dollars_per_case)) %>%
  print()

# Save results
write.csv(results,  "overissuance_grid_search_full.csv",      row.names = FALSE)
write.csv(optimal_recall,  "overissuance_unearned_optimal_thresholds_alt.csv",    row.names = FALSE)
