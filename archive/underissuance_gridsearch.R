library(dplyr)
library(purrr)
library(tidyr)
source("apply_thresholds_to_test_data.R")
source("grid_search_underissuance_detection_v2.R")

income_and_clean_data <- reg_model_data %>%
  filter(
    !is.na(error_status) |  # Has an income error, OR
      (is.na(error_status) & (is.na(element1) | status_clean == "amount correct"))  # No error at all
  ) %>%
  mutate(
    # For non-error cases, set total_error_amount to 0
    total_error_amount = case_when(
      is.na(error_status) ~ 0,
      TRUE ~ total_error_amount
    )
  )

reg_model_data$gross_by_hh_size = (reg_model_data$unearned_income + reg_model_data$earned_income) / reg_model_data$cert_HH_size_FS_n
summary(reg_model_data$gross_by_hh_size)
summary(reg_model_data$fsgrinc / reg_model_data$cert_HH_size_FS_n)
table(reg_model_data$unearned_income + reg_model_data$earned_income == reg_model_data$gross_income)
table(reg_model_data$earned_income==reg_model_data$recovered_fsearn)

orig <- orig %>% rename(status_c = status)
orig <- orig %>% rename(total_error_amount = amterr)

train_data_2017_2021 <- orig %>% filter(year %in% c("2017","2018","2019","2020","2021"))

train_data_2021_2023 <- orig %>% filter(year %in% c("2021","2022","2023"))
train_data_2021_2023 <- train_data_2021_2023 %>% mutate(deductions_by_hh_size = fstotded / fsusize) 

test_data_2021_2023 <- orig %>% filter(year %in% c("2021","2022","2023"))
test_data_2021_2023 <- test_data_2021_2023 %>% mutate(deductions_by_hh_size = fstotded / fsusize) 


grid_search_data$overthreshold <- grid_search_data$over_threshold
# Example usage:
# 
# Step 1: Get optimal thresholds from training data (e.g., 2022)
train_results <- run_underissuance_grid_by_geography(
  data = grid_search_data[grid_search_data$state=="Washington",],
  geography_var = "state",
  error_amount_var = "total_error_amount",
  overthreshold_var = "overthreshold",
  earned_by_hh_size_range = seq(0, 600, by = 50),
  rawben_rel_max_range = seq(0.1, 0.9, by = 0.05),
  shelter_by_hh_size_range = seq(0, 2000, by = 100),
  min_error_dollars = 500
)


# Option 3: Balance between efficiency and coverage
# First filter to reasonable recall, then maximize efficiency
optimal_balanced <- find_optimal_underissuance_thresholds(
  grid_results = train_results,
  optimize_for = "dollar_recall",
  min_precision = 0.4,
  max_flagged_pct = 1,
  min_dollar_recall = 0.1            # Require at least 30% recall
)
write.csv(optimal_balanced, "optimal_balanced_underissuance_022326.csv", row.names=F)



#Step 2: Apply to test data (e.g., 2023)
test_results <- apply_optimal_thresholds_to_test(
  optimal_thresholds = optimal_balanced,
  test_data = grid_search_data,
  geography_var = "state",
  error_amount_var = "total_error_amount",
  overthreshold_var = "overthreshold",
  error_finding_var = "status"  # Use this if your test data uses status_c instead of e_findg1
)
#Step 3: View and save results
test_results %>%
  arrange(desc(test_dollar_recall)) %>%
  print()

write.csv(test_results, "test_performance_2022_2023_overall_022626.csv", row.names = FALSE)

# Step 4: Identify states where performance degraded
test_results %>%
  filter(recall_change < -0.05 | precision_change < -0.05) %>%
  select(geography_value, train_dollar_recall, test_dollar_recall, recall_change,
         train_precision, test_precision, precision_change) %>%
  arrange(recall_change)


#manual test
grid_search_data <- grid_search_data %>% mutate(wa_flag3 = 
                                                  case_when(
                                                    (earned_by_hh_size>= 200 & rawben_rel_max >= .45 & rawben_rel_max < .55 & deductions_by_hh_size <350) ~ TRUE,
                                                    TRUE ~ FALSE),
                                                dc_flag3 = 
                                                  case_when(
                                                    (earned_by_hh_size>= 50 & rawben_rel_max >= .15 & rawben_rel_max < 0.6 & deductions_by_hh_size < 450) ~ TRUE,
                                                    TRUE ~ FALSE))
                                                
grid_search_data %>% filter(state=="Washington") %>% group_by(wa_flag3, over_threshold) %>% summarize(mean(total_error_amount), n=n())
grid_search_data %>% filter(state=="District_of_Columbia") %>% group_by(dc_flag3, over_threshold) %>% summarize(mean(total_error_amount), n=n())


# Example usage:
# 
# Step 1: Get optimal thresholds from training data (e.g., 2022)

summary(test_data_2023$total_error_amount[tolower(test_data_2023$status_c) == "underissuance"])
# Step 3: View and save results

###DEBUG and test with manually coded
train_data_2021_2023$hard_code_DC <- F
train_data_2021_2023$hard_code_DC <- train_data_2021_2023$earned_by_hh_size >= 50 & train_data_2021_2023$deductions_by_hh_size < 400 & train_data_2021_2023$rawben_rel_max<.6 & train_data_2021_2023$rawben_rel_max>.1
table(train_data_2021_2023$hard_code_DC)
train_data_2021_2023 %>% filter(state=="District_of_Columbia") %>% group_by(hard_code_DC, status_c) %>% tally()

test_data_2022_2023$hard_code_AL <- F
test_data_2022_2023$hard_code_AL <- test_data_2022_2023$earned_by_hh_size >= 0 & test_data_2022_2023$total_deductions_fs < 100 & test_data_2022_2023$rawben_rel_max<.95
table(test_data_2022_2023$hard_code_AL)
test_data_2022_2023 %>% filter(state=="Alabama") %>% group_by(hard_code_AL, status_c) %>% summarize(avg_error = mean(total_error_amount), count=sum(error_flag))

test_data_2022_2023$hard_code_Iowa <- test_data_2022_2023$earned_by_hh_size >= 50 & test_data_2022_2023$total_deductions_fs < 800 & test_data_2022_2023$rawben_rel_max<.90
table(test_data_2022_2023$hard_code_Iowa)
test_data_2022_2023 %>% filter(state=="Iowa") %>% group_by(hard_code_Iowa, status_c) %>% summarize(avg_error = mean(total_error_amount), count=sum(error_flag), n=n())

test_data_2022_2023$hard_code_DC <- test_data_2022_2023$earned_by_hh_size >= 100 & test_data_2022_2023$total_deductions_fs < 200 & test_data_2022_2023$rawben_rel_max<.80
test_data_2022_2023 %>% filter(state=="District_of_Columbia") %>% group_by(hard_code_DC, status_c) %>% summarize(avg_error = mean(total_error_amount), count=sum(error_flag), n=n())
table(test_data_2022_2023$hard_code_DC)


error_totals <- test_data_2021_2023 %>% filter(overthreshold==1) %>% group_by(state) %>% summarize(total_error_check = sum(total_error_amount*fywgt, na.rm=T))

train_data_2021_2023$hard_code_MD <- F
train_data_2021_2023$hard_code_MD <- train_data_2021_2023$earned_by_hh_size >= 0 & train_data_2021_2023$deductions_by_hh_size < 100 & train_data_2021_2023$rawben_rel_max<.6 & train_data_2021_2023$rawben_rel_max>.5
table(train_data_2021_2023$hard_code_MD)
train_data_2021_2023 %>% filter(state=="Maryland") %>% group_by(hard_code_MD, status_c) %>% tally()
