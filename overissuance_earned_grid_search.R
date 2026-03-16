###oVERISSUANCE earned grid###

source("apply_overissuance_thresholds_to_test_data.R")
source("grid_search_overissuance_detection_v3.R")
grid_search_data <- income_and_clean_data %>% filter(year %in% c("2021","2022","2023"))

grid_search_data <- grid_search_data %>% filter(timeper1=="at time of most recent action by agency" | is.na(timeper1))

grid_search_data$shelter_by_hh_size <- grid_search_data$shelter_expenses / grid_search_data$cert_HH_size_FS_n

#NEXT TRY:
#RAWBEN_rel_MAX >= .99 & ceRT_hh_SIZE >=4


# Run grid search by state
results <- run_overissuance_grid_by_geography(
  data                        = grid_search_data[grid_search_data$state=="Washington",],
  geography_var               = "state",
  error_amount_var            = "total_error_amount",
  error_threshold             = 52,
  earned_by_hh_size_range     = seq(1, 501, by = 50),
  rawben_rel_max_range_lower  = seq(0.4, 1, by = 0.05),
  rawben_rel_max_range_upper  = seq(1, 1, by = 1),
  shelter_by_hh_size_range = seq(0, 2000, by = 100),
  shelter_to_gross_income_ratio_range = seq(0, 2, by = 2),
  min_error_dollars           = 5000
)




# Option 2: Maximize dollar recall (coverage) while maintaining precision
optimal_recall <- find_optimal_overissuance_thresholds(
  grid_results    = results,
  optimize_for    = "n_sig_errors_flagged",
  min_precision   = 0.2,
  max_flagged_pct = 1
)


#test by handcoding
grid_search_data <- grid_search_data %>% mutate(nc_flag = 
                                                  case_when(
                                                    (earned_by_hh_size> 100 & rawben_rel_max > .4 & deductions_by_hh_size < 150) ~ TRUE,
                                                    TRUE ~ FALSE),
                                                dc_flag = 
                                                  case_when(
                                                    (earned_by_hh_size> 1 & rawben_rel_max > .4  & deductions_by_hh_size < 400) ~ TRUE,
                                                    TRUE ~ FALSE),
                                                ct_flag = 
                                                  case_when(
                                                    (earned_by_hh_size> 1 & rawben_rel_max > .6 & deductions_by_hh_size < 300) ~ TRUE,
                                                    TRUE ~ FALSE),
                                                wa_flag = 
                                                  case_when(
                                                    (earned_by_hh_size> 300 & rawben_rel_max > .6 & shelter_by_hh_size < 300) ~ TRUE,
                                                    TRUE ~ FALSE)
                                                )
grid_search_data %>% filter(state=="Washington" & (timeper1=="at time of most recent action by agency" | is.na(timeper1))) %>% group_by(wa_flag, over_threshold) %>% 
  summarize(mean(total_error_amount), n=n())

grid_search_data %>% filter(state=="North_Carolina" & (timeper1=="at time of most recent action by agency" | is.na(timeper1))) %>% group_by(nc_flag, amterr>52) %>% 
  summarize(mean(total_error_amount), n=n())

grid_search_data %>% filter(state=="North_Carolina" & (timeper1=="at time of most recent action by agency" | is.na(timeper1))) %>% group_by(nc_flag, amterr>52) %>% 
  summarize(mean(total_error_amount), n=n())

grid_search_data %>% filter(state=="District_of_Columbia") %>% group_by(dc_flag, over_threshold) %>% 
  summarize(mean(total_error_amount), n=n())

grid_search_data <- grid_search_data %>% mutate(alternate_deductions_by_hh_size = total_deductions_fs / cert_HH_size_FS_n)
grid_search_data <- grid_search_data %>% mutate(nc_flag2 = 
                                                  case_when(
                                                    (earned_by_hh_size> 1 & rawben_rel_max > .4  & shelter_to_gross_income_ratio >= 1 & deductions_by_hh_size <150) ~ TRUE,
                                                    TRUE ~ FALSE),
                                                dc_flag2 = 
                                                  case_when(
                                                    (earned_by_hh_size> 1 & rawben_rel_max > .4  & shelter_to_gross_income_ratio >= 1) ~ TRUE,
                                                    TRUE ~ FALSE),
                                                ct_flag2 = 
                                                  case_when(
                                                    (earned_by_hh_size> 1 & rawben_rel_max > .6  & shelter_to_gross_income_ratio >=1) ~ TRUE,
                                                    TRUE ~ FALSE),
                                                wa_flag2 = 
                                                  case_when(
                                                    (earned_by_hh_size> 1 & rawben_rel_max > .6 & shelter_to_gross_income_ratio >= 1) ~ TRUE,
                                                    TRUE ~ FALSE)
)
grid_search_data %>% filter(state=="North_Carolina" & (timeper1=="at time of most recent action by agency" | is.na(timeper1))) %>% group_by(nc_flag2, amterr>52) %>% tally()


# Option 3: Balance between efficiency and coverage
optimal_balanced <- find_optimal_overissuance_thresholds(
  grid_results      = results,
  optimize_for      = "dollars_per_case",
  min_precision     = 0.25,
  max_flagged_pct   = 1,
  min_dollar_recall = 0.05
)

# View results
optimal %>%
  arrange(desc(dollars_per_case)) %>%
  print()

# Save results
write.csv(results,  "overissuance_grid_search_full.csv",      row.names = FALSE)
write.csv(optimal_recall,  "overissuance_optimal_thresholds_earned_benmax_deductions_alt.csv",    row.names = FALSE)
