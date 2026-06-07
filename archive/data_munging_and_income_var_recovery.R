library(tidyverse)

df <- read_csv("model_variables_new.csv", show_col_types = FALSE)
table(df$state)
names(df)

case_details <- read_csv("case_details.csv")
names(case_details)

df <- merge(df, case_details, by="case_id")

state_policies <- read_csv("snap_state_options_2023.csv", show_col_types = FALSE)
names(state_policies)
state_policies <- state_policies[,c("State","ReportingRequirements","CertificationPeriods","BBCE","DemonstrationsElderlyDisability","SUAs","CAP")]

df <- merge(df, state_policies, by.x="state", by.y="State")

#over threshold should only be for overissuance and error > 58, will correct next
table(df$over_threshold, df$status)
df <- df %>%
  filter(year > 2016) %>%
  filter(!is.na(status)) %>%
  mutate(overpayment_error_i = as.numeric(
    if_else(status == "overissuance" & over_threshold==1, 1, 0)))

#### variable clearning / recoding ###
df$overpayment_error_i <- as.factor(df$overpayment_error_i)

check_number_of_income_errors_at_cert <- df %>% filter(element1 %in% list_of_income_error_elements & timeper1=="at time of most recent action by agency") 
rm(check_number_of_income_errors_at_cert)
#14621 income errors at cert for 2017-2023 data

check_number_of_income_errors_anytime <- df %>% filter(element1 %in% list_of_income_error_elements) 
rm(check_number_of_income_errors_anytime)
#46637 income errors at any time per

#if we can't get the total error amount to be close to the difference between fsben and rawben, we drop the case. 
errors <- subset(df, status!="amount correct")
no_errors <- subset(df, status =="amount correct")
errors <- errors %>% mutate(final_to_raw_ben_diff = abs(fsben - rawben))
table(abs(errors$final_to_raw_ben_diff - errors$amterr) <= 5)
errors <- subset(errors, abs(final_to_raw_ben_diff - amterr)<=5)
orig <- bind_rows(errors, no_errors)


reg_model_data <- orig %>%
  rename(cert_HH_size_FS_n = fsusize, #FNS Tech Docs "Constucted certified unit size"
         benefit_amount_FS = fsben, # "Final calculated benefit"
         net_income_FS = fsnetinc, #Final net countable unit income
         gross_inc_to_poverty_FS = tpov, #"Gross income/poverty level ratio"
         raw_benefit_amount = rawben, #"Reported SNAP benefit received"
         maximum_benefit_for_HH_size = benmax, #Maximum benefit amount
         total_error_amount = amterr, #"Amount of benefit in error"
         children_i = children_present,
         elderly_i = elderly_present,
         months_since_cert_n = lastcert, #Months since last SNAP certification
         months_certification_period_n = certmth, # months in certification period
         #status_c = status, #1 amount correct, 2 overissuance, 3 underissuance
         total_deductions_fs = fstotde2, #Total deductions
         total_assets_fs = fsasset, #total countable assets under state rules
         people_in_HH_n = ctprhh, #number of people in household
         action_type_c = actntype, #most recent action type
         adjusted_allotment = benfix) %>%  #"benefit amount adjusted for errors"
  mutate(state = as.factor(state),
         year = as.factor(year),
         #HH_head_LF_status_c = as.factor(HH_head_LF_status_c),
         cert_HH_size_FS_n = as.integer(cert_HH_size_FS_n),
         HH_size_rel_cert_HH_size = people_in_HH_n / cert_HH_size_FS_n,
         rawben_rel_max = raw_benefit_amount / maximum_benefit_for_HH_size,
         other_unearned = fsothun,
         self_employment_income = fsslfemp,
         ssi_income = fsssi,
         wages_salaries = fswages,
         med_expenses = fsmedexp,
         shelter_expenses =  fssltexp,
         rent_mortage = rent,
         utilities = util) %>%
  mutate(homeless = as.factor(homeded!="Not homeless"))


#some states are missing timeper1 more often than others, meaning that we're probably losing representativeness of what's actually happening at cert
table(df$state[!is.na(df$element1)], is.na(df$timeper1[!is.na(df$element1)]))

table(df$state[!is.na(df$element1)], df$timeper1[!is.na(df$element1)])


reg_model_data <- reg_model_data %>%
  filter(timeper1 %in% c("at time of most recent action by agency") & action_type_c %in% c("certification","recertification")
         | status=="amount correct" & action_type_c %in% c("certification","recertification"))


reg_model_data <- reg_model_data %>%
  rowwise() %>%
  mutate(
    min_age = min(c_across(age1:age17), na.rm = TRUE),
    max_age = max(c_across(age1:age17), na.rm = TRUE)
  ) %>%
  ungroup()
table(reg_model_data$max_age)

reg_model_data %>% filter(cert_HH_size_FS_n==1) %>% select(age1, min_age, max_age) %>% sample_n(50)

reg_model_data <- reg_model_data %>% mutate(lf_composition = case_when(
  min_age > 17 & max_age <60 ~ "all working age",
  max_age < 18 | min_age > 59 ~ "no working age",
  TRUE ~ "mixed age"
))

table(reg_model_data$lf_composition)
reg_model_data$lf_composition <- factor(reg_model_data$lf_composition)

reg_model_data <- reg_model_data %>% mutate(expedited_i = expedser<3) # 1 and 2 are for expedited, 2 is for not timely; 3 means not expedited 

reg_model_data$rawben_rel_max[reg_model_data$rawben_rel_max>1] <- 1

reg_model_data <- reg_model_data %>% mutate(non_elderly_disabled_i = as.factor(fsndis>0)) 
reg_model_data <- reg_model_data %>%
  mutate(married = as.integer(if_any(rel1:rel17, ~ .x == "Spouse")))

reg_model_data$married[is.na(reg_model_data$married)] <- 0

summary(reg_model_data$shelter_expenses)

list_of_income_error_elements <- c("wages and salaries",
                                   "self-employment",
                                   "other earned income",
                                   "rsdi benefits",
                                   "veterans' benefits",
                                   "ssi and/or state ssi supplement",
                                   "unemployment compensation",
                                   "workers' compensation",
                                   "other government benefits",
                                   "contributions",
                                   "deemed income",
                                   "tanf, pa, or ga",
                                   "educational grants/scholarships/loans",
                                   "other unearned income",
                                   "child support payments received from absent parent")


# subset to income errors based on element1

test  <- reg_model_data  %>%
  filter(element1 %in% list_of_income_error_elements | status == "amount correct")

table(reg_model_data$element1[reg_model_data$status!="amount correct"])
#drop 5 records where error>0 and status=="amount correct"
reg_model_data <- subset(reg_model_data, !(status=="amount correct" & amterr>0))

# Use rawben and fsben to recover income
#
# Strategy: 
# 1. Use rawben to calculate what original gross income was
# 2. Compare to current fsgrinc to find the change
# 3. Attribute change to earned vs unearned based on element1


reg_model_data  <- reg_model_data  %>%
  filter(raw_benefit_amount <= maximum_benefit_for_HH_size) %>%
  mutate(
    # ── Identify income error types ───────────────────────────────────────────
    has_earned_inc_error = !is.na(element1) & tolower(element1) %in% c(
      "wages and salaries",
      "self-employment",
      "other earned income"
    ),
    
    has_unearned_inc_error = !is.na(element1) & tolower(element1) %in% c(
      "rsdi benefits",
      "veterans' benefits",
      "ssi and/or state ssi supplement",
      "unemployment compensation",
      "workers' compensation",
      "other government benefits",
      "contributions",
      "deemed income",
      "tanf, pa, or ga",
      "educational grants/scholarships/loans",
      "other unearned income",
      "child support payments received from absent parent"
    ),
    
    # ── Back-calculate net income before correction ───────────────────────────
    original_fsnetinc = case_when(
      status == "amount correct"  ~ net_income_FS,
      raw_benefit_amount <= minimum_ben       ~ (maximum_benefit_for_HH_size - minimum_ben) / 0.30,
      TRUE                        ~ (maximum_benefit_for_HH_size - raw_benefit_amount) / 0.30
    ),
    
    # ── Recover original earned income (algebraic direct solution) ────────────
    #
    # original_fsnetinc = original_fsearn + fsunearn - original_fstotded
    # original_fstotded = fstotded - fsearn*0.20 + original_fsearn*0.20
    # Solving for original_fsearn:
    #   original_fsearn = (original_fsnetinc - fsunearn + fstotded - fsearn*0.20) / 0.80
    
    recovered_fsearn = case_when(
      has_earned_inc_error ~
        pmax(0, (original_fsnetinc - fsunearn + fstotded - fsearn * 0.20) / 0.80),
      TRUE ~ fsearn
    ),
    
    # ── Recover original unearned income ──────────────────────────────────────
    #
    # fstotded unchanged for unearned errors (no deduction tied to unearned income)
    # original_fsunearn = original_fsnetinc - fsearn + fstotded
    
    recovered_fsunearn = case_when(
      has_unearned_inc_error ~
        pmax(0, original_fsnetinc - fsearn + fstotded),
      TRUE ~ fsunearn
    ),
    
    # ── Reconstruct original fstotded for earned error cases ──────────────────
    #
    # Only the 20% earned income deduction changes with earned income
    
    original_fstotded = case_when(
      has_earned_inc_error ~
        fstotded - (fsearn * 0.20) + (recovered_fsearn * 0.20),
      TRUE ~ fstotded
    ),
    
    # ── Verification ──────────────────────────────────────────────────────────
    
    verify_original_grinc   = recovered_fsearn + recovered_fsunearn,
    verify_original_netinc  = verify_original_grinc - original_fstotded,
    verify_original_benefit = case_when(
      verify_original_netinc <= 0 ~ maximum_benefit_for_HH_size,
      TRUE ~ pmax(minimum_ben, maximum_benefit_for_HH_size - (0.30 * verify_original_netinc))
    ),
    
    rawben_error = case_when(
      status == "amount correct" ~ abs(verify_original_benefit - benefit_amount_FS),
      TRUE                       ~ abs(verify_original_benefit - raw_benefit_amount)
    ),
    good_match = rawben_error < 1
  )

reg_model_data$benefit_amount_FS
table(reg_model_data$good_match)
table(reg_model_data$rawben_error<5, reg_model_data$status)

table(reg_model_data$has_unearned_inc_error)

# Diagnostic summary
cat("\n=== Overall Match Quality ===\n")
reg_model_data %>%
  filter(status != "amount correct" & (has_earned_inc_error | has_unearned_inc_error)) %>%
  summarise(
    n = n(),
    pct_good_match = mean(good_match, na.rm = TRUE) * 100,
    mean_rawben_error = mean(rawben_error, na.rm = TRUE),
    median_rawben_error = median(rawben_error, na.rm = TRUE),
    max_rawben_error = max(rawben_error, na.rm = TRUE)
  ) %>%
  print()

cat("\n=== By Error Type ===\n")
reg_model_data %>%
  filter(status != "amount correct") %>%
  mutate(
    error_type = case_when(
      has_earned_inc_error ~ "Earned income error",
      has_unearned_inc_error ~ "Unearned income error",
      TRUE ~ "Non-income error"
    )
  ) %>%
  group_by(error_type) %>%
  summarise(
    n = n(),
    pct_good_match = mean(good_match, na.rm = TRUE) * 100,
    mean_error = mean(rawben_error, na.rm = TRUE),
    median_error = median(rawben_error, na.rm = TRUE)
  ) %>%
  print()

# Add error_category for underissuance and overissuance for earned and unearned 
reg_model_data <- reg_model_data %>%
  mutate(
    error_category = case_when(
      has_earned_inc_error ~ "earned_income",
      has_unearned_inc_error ~ "unearned_income",
      TRUE ~ "other"
    ),
    
    # Ensure status is clean
    status_clean = tolower(trimws(status)),
    
    # Create combined category for tree stratification
    error_status = case_when(
      error_category == "earned_income" & status_clean == "overissuance" ~ "earned_overissuance",
      error_category == "earned_income" & status_clean == "underissuance" ~ "underissuance",
      error_category == "unearned_income" & status_clean == "overissuance" ~ "unearned_overissuance",
      error_category == "unearned_income" & status_clean == "underissuance" ~ "underissuance",
      TRUE ~ NA_character_
    )
  )


reg_model_data$rawnet_allow_negative <- reg_model_data$recovered_fsearn + reg_model_data$recovered_fsunearn - reg_model_data$original_fstotded
reg_model_data$rawben_no_cap <- reg_model_data$maximum_benefit_for_HH_size - (0.3 * reg_model_data$rawnet_allow_negative) # Rawben without cap (+0.0255)
reg_model_data$rawben_no_cap_rel_max <- reg_model_data$rawben_no_cap / reg_model_data$maximum_benefit_for_HH_size # Rawben without cap rel max (+0.0018)


reg_model_data <- reg_model_data %>%  mutate(shelter_to_gross_income_ratio = as.integer(as.numeric(shelter_expenses) / as.numeric(recovered_fsearn + recovered_fsunearn + 1)))

reg_model_data <- reg_model_data %>% mutate(earned_by_hh_size = recovered_fsearn / cert_HH_size_FS_n, 
                                            unearned_by_hh_size = recovered_fsunearn / cert_HH_size_FS_n,
                                            gross_by_hh_size = as.integer((recovered_fsearn + recovered_fsunearn) / (cert_HH_size_FS_n)),
                                            deductions_by_hh_size = original_fstotded / cert_HH_size_FS_n)



# ── Income type variables ─────────────────────────────────────────────────────
income_vars <- c(
  "FSWAGES",    # Wages and salaries
  "FSSLFEMP",   # Self-employment income
  "FSOTHERN",   # Other earned income
  "FSSSI",      # SSI benefits
  "FSTANF",     # TANF payments
  "FSGA",       # General Assistance benefits
  "FSSOCSEC",   # Social Security income
  "FSUNEMP",    # Unemployment compensation
  "FSVET",      # Veterans' benefits
  "FSWCOMP",    # Workers' compensation
  "FSEDLOAN",   # Educational grants and loans
  "FSCSUPRT",   # Child support payment income received
  "FSDEEM",     # Deemed income
  "FSCONT",     # Contributions / charity / in-kind
  "FSOTHGOV",   # Other government benefits
  "FSOTHUN",    # Other unearned income
  "FSDIVER",    # State diversion payments
  "FSWGESUP",   # Wage supplementation income
  "FSENERGY",   # Energy assistance income
  "FSEITC",     # Earned income tax credit
  "FSFOSTER"    # Foster care income
)

# ── Deduction type variables ──────────────────────────────────────────────────
deduction_vars <- c(
  "FSSTDDED",     # Standard deduction
  "FSERNDED",     # Earned income deduction
  "FSDEPDED",     # Dependent care deduction
  "FSSLTDED",     # Excess shelter expense deduction
  "FSMEDDED",     # Medical expense deduction
  "FSCSDED",      # Child support payment deduction
  "HOMELESS_DED"  # Homeless household shelter deduction
)

# ── Create count variables ────────────────────────────────────────────────────
# n_income_types: number of distinct income sources with a value > 0
# n_deduction_types: number of distinct deductions with a value > 0

#switched from reg to income df here

income_and_clean_data <- income_and_clean_data %>%
  mutate(
    n_income_types = rowSums(
      across(all_of(tolower(income_vars)), ~ !is.na(.) & . > 0),
      na.rm = TRUE
    ),
    n_deduction_types = rowSums(
      across(all_of(tolower(deduction_vars)), ~ !is.na(.) & . > 0),
      na.rm = TRUE
    )
  )

all_vars <- c(income_vars, deduction_vars)
setdiff(tolower(all_vars), names(income_and_clean_data))

income_and_clean_data$count_divisible_by_100 <- rowSums(
  sapply(income_and_clean_data[, tolower(all_vars)], function(x) x > 0 & x %% 100 == 0),
  na.rm = TRUE
)

# ── Quick check ───────────────────────────────────────────────────────────────
cat("=== n_income_types ===\n")
print(table(income_and_clean_data$n_income_types, useNA = "ifany"))

cat("\n=== n_deduction_types ===\n")
print(table(income_and_clean_data$n_deduction_types, useNA = "ifany"))


summary(reg_model_data[301:306])
