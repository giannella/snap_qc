#manually verify some results from trees
test_income_and_clean_data <- income_and_clean_data %>% filter(!error_status %in% c("underissuance","unearned_overissuance"))


#from earned_overissuance_2022_2023_any_timeper.png, 
# we choose a node where we had rawben_no_cap_rel_max >=1 AND rawben_no_cap_rel_max <1 
test_income_and_clean_data$avg214_n172 <- test_income_and_clean_data$earned_by_hh_size>=0.069 &test_income_and_clean_data$deductions_by_hh_size< 124 & 
  test_income_and_clean_data$shelter_to_gross_income_ratio< 0.5 & test_income_and_clean_data$deductions_by_hh_size< 124 & test_income_and_clean_data$rawben_no_cap_rel_max==1
table(test_income_and_clean_data$avg214_n172)
#172 cases fall into this group

mean(test_income_and_clean_data$total_error_amount[test_income_and_clean_data$avg214_n172], na.rm=T)
#213.78

#from the connecticut tree 2017-2023 using no_cap Connecticut_reg_2017_2023_income_errors.png
reg_model_data %>% filter(state=="Connecticut" & rawben_no_cap_rel_max==1 & shelter_to_gross_income_ratio>=2) %>% summarize(n=n(), avg_error = mean(total_error_amount))
#22 cases, avg error of $230, so matches

wa_test <- reg_model_data %>% filter(state=="Washington")

wa_test$exclusion_rule <- wa_test$earned_by_hh_size < 7 &wa_test$shelter_expenses < 2136 & 
  wa_test$rawben_no_cap_rel_max> 1

wa_test %>% filter(exclusion_rule==TRUE) %>% summarize(n=n(), mean(total_error_amount))
tree_model$frame[, c("var", "n")]
tree_model$splits[, c("count", "ncat", "improve", "index")]
