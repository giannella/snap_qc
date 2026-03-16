#############################################
### 0. Packages (NEW)
#############################################

library(tidyverse)
library(tidymodels)
library(readr)
library(vip)        # variable importance
library(themis)     # optional, for imbalance
library(parsnip)
library(stringr)
library(nnet)
library(sjPlot)
library(patchwork)


tidymodels_prefer()

#############################################
### 1. Read & basic filter
#############################################

df <- read_csv("C:/Users/ericg/snapqc/SNAP_error_rate_support/model/model_variables_new.csv", show_col_types = FALSE)

case_details <- read_csv("case_details.csv")

case_details <- case_details[,c("case_id","error_threshold","over_threshold")]
df <- merge(df, case_details, by="case_id")

state_policies <- read_csv("snap_state_options_2023.csv", show_col_types = FALSE)
state_policies <- state_policies[,c("State","ReportingRequirements","CertificationPeriods","BBCE","DemonstrationsElderlyDisability","SUAs","CAP")]

df <- merge(df, state_policies, by.x="state", by.y="State")

#over threshold should only be for overissuance and error > 58, will correct next
table(df$over_threshold, df$status)
df <- df %>%
  filter(year > 2016) %>%
  filter(!is.na(status)) %>%
  mutate(payment_error_i = as.factor(
      if_else(status %in% c("overissuance","underissuance") & over_threshold==1, "Yes", "No")))
table(df$payment_error_i)

#### variable clearning / recoding ###

df$error_flag <- as.factor(as.character(df$error_flag))

df <- df %>% mutate(expedited_i = expedser<3) # 1 and 2 are for expedited, 2 is for not timely; 3 means not expedited 

#make a categorical outcome measure that indicates whether no error ("correct"), error is underpayment or overpayment under threshold, or overpayment over threshold
df$outcome <- "correct"
df$outcome[df$error_flag=="1"] <- "uncounted_error"
df$outcome[df$payment_error_i=="Yes"] <- "above_threshold_error"
df$outcome <- as.factor(df$outcome)
table(df$outcome)

df <- df %>% mutate(HH_head_LF_status_c = case_when(
  empsta1 == "Not in labor force and not looking for work" ~ "not in labor force",
  empsta1 == "Unemployed and looking for work" ~ "unemployed and searching",
  empsta1 %in% c("Active-duty military",
                 "Migrant farm laborer",
                 "Nonmigrant farm laborer","Self-employed, farming") ~ "other",
  empsta1 %in% c("Self-employed, nonfarming") ~ "self-employed",
  empsta1 == "Employed by other" ~ "employed by other",
  TRUE ~ NA
))

table(df$HH_head_LF_status_c, df$empsta1)
df <- df %>%
  mutate(anyone_working = as.integer(if_any(empsta1:empsta17, ~ .x %in% c("Self-employed, nonfarming","Employed by other"))))

df <- df %>%
  mutate(anyone_self_employed = as.integer(if_any(empsta1:empsta17, ~ .x %in% c("Self-employed, nonfarming"))))

df <- df %>%
  mutate(anyone_employed_by_other = as.integer(if_any(empsta1:empsta17, ~ .x %in% c("Employed by other"))))

table(df$anyone_working)
df <- df %>% mutate(HH_head_LF_status_c = case_when(
  empsta1 == "Not in labor force and not looking for work" ~ "not in labor force",
  empsta1 == "Unemployed and looking for work" ~ "unemployed and searching",
  empsta1 %in% c("Migrant farm laborer","Nonmigrant farm laborer",
                 "Self-employed, farming","Active-duty military") ~ "other",
  empsta1 == "Self-employed, nonfarming" ~ "self-employed",
  empsta1 == "Employed by other" ~ "employed by other",
  TRUE ~ NA
))

table(df$HH_head_LF_status_c)
df <- df %>%
  mutate(married = as.integer(if_any(rel1:rel17, ~ .x == "Spouse")))

df$married[is.na(df$married)] <- 0

df <- df %>% mutate(race_ethnicity = case_when(
  raceth1 %in% c("hispanic_ai_an", "hispanic_ai_an_and_black","hispanic_ai_an_and_white",
                 "hispanic_asian","hispanic_asian_and_white", "hispanic_black_and_white",
                 "hispanic_black_or_african_american","hispanic_multiple_races_other",
                 "hispanic_native_hawaiian_or_pacific_islander","hispanic_white") ~ "Hispanic",
  raceth1 %in% c("not_hispanic_american_indian_or_alaska_native") ~ "American Indian or Alaska Native",
  raceth1 %in% c("not_hispanic_black_or_african_american","not_hispanic_black_and_white") ~ "Black",
  raceth1 %in% c("not_hispanic_white") ~ "White",
  raceth1 %in% c("not_hispanic_native_hawaiian_or_pacific_islander","not_hispanic_ai_an_and_black",
                 "not_hispanic_asian","not_hispanic_asian_and_white","not_hispanic_ai_an_and_white") ~ "AAPI",
  raceth1 %in% c("race_ethnicity_not_available_application_not_found","race_ethnicity_not_recorded") ~ "missing",
  TRUE ~ NA #captures not_hispanic_multiple_races_other
))


table(df$race_ethnicity, df$raceth1)
table(df$raceth1[is.na(df$race_ethnicity)])

df <- df %>% mutate(unit_composition_error = as.integer(if_any(element1:element9, ~ .x == "unit composition")))
df$unit_composition_error[df$raw_benefit_amount > df$maximum_benefit_for_HH_size] <- 1
df$unit_composition_error[is.na(df$unit_composition_error)] <- 0
df$unit_composition_error <- factor(df$unit_composition_error)
table(df$unit_composition_error)

df <- df %>% mutate(non_elderly_disabled_i = as.factor(fsndis>0)) 

df <- df %>%
  group_by(state, year) %>%
  mutate(rescaled_weight = fywgt / mean(fywgt, na.rm = TRUE))

df <- df %>% 
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
         status_c = status, #1 amount correct, 2 overissuance, 3 underissuance
         total_deductions_fs = fstotde2, #Total deductions
         total_assets_fs = fsasset, #total countable assets under state rules
         people_in_HH_n = ctprhh, #number of people in household
         action_type_c = actntype, #most recetn action type
         adjusted_allotment = benfix) %>%  #"benefit amount adjusted for errors"   
  mutate(state = as.factor(state),
         year = as.factor(year), 
         HH_head_LF_status_c = as.factor(HH_head_LF_status_c),
         cert_HH_size_FS_n = as.integer(cert_HH_size_FS_n),
         HH_size_rel_cert_HH_size = people_in_HH_n / cert_HH_size_FS_n,
         rawben_rel_max = raw_benefit_amount / maximum_benefit_for_HH_size,
         earned_income = fsearn,
         gross_income = fsgrinc,
         other_unearned = fsothun,
         self_employment_income = fsslfemp,
         ssi_income = fsssi,
         unearned_income = fsunearn,
         wages_salaries = fswages,
         raw_gross = rawgross,
         raw_net = rawnet,
         med_expenses = fsmedexp,
         shelter_expenses =  fssltexp,
         rent_mortage = rent,
         utilities = util) %>% 
  mutate(homeless = as.factor(homeded!="Not homeless"))

df <- df %>%  mutate(shelter_to_gross_income_ratio = as.numeric(shelter_expenses) / as.numeric(gross_income + 1))
summary(df$shelter_to_gross_income_ratio)

#############################################
### 2. Dataset split by error timing
#############################################

#roughly  80% of multi-error cases have same timing. This would only get us another 1300-1500 observations
table(df$timeper1, df$timeper2)[,2] %>% prop.table()

table(df$timeper1, df$action_type_c) %>% prop.table()

df_init_cert_only <- df %>%
  filter(timeper1 == "at time of most recent action by agency" & action_type_c=="certification" | status_c=="amount correct" & action_type_c=="certification")

df_cert_only <- df %>%
  filter(timeper1 %in% c("at time of most recent action by agency") & action_type_c %in% c("certification","recertification")
         | status_c=="amount correct" & action_type_c %in% c("certification","recertification"))

df_cert_only %>% group_by(timeper1, nature1) %>% tally() %>% print(n=100)
table(df_cert_only$outcome)



#############################################
### 3. Feature set
#############################################

features <- c(
  "cert_HH_size_FS_n",            # certified household size
  "HH_size_rel_cert_HH_size", #ratio of people in HH to cert HH size
  # "people_in_HH_n", #hh size
  "children_i",   # children indicator
  "gross_inc_to_poverty_FS",
  #  "action_type_c",#most recent action type
  "elderly_i",    # elderly indicator
  #"months_since_cert_n",#months_since_last cert
  "non_elderly_disabled_i",# disabled indicator
  "HH_head_LF_status_c", #lfstatus
  "total_deductions_fs",           # total deductions
  #  "net_income_FS",           # net income (optional but useful)
  # "abwdst1",            # (keep if exists)
  "expedited_i",           # expedited service
  "cat_elig",           # categorical eligibility 
  #  "amount_rel_max", #FS ben (corrected) rel max
  "rawben_rel_max",
  #  "raw_benefit_amount", #benefit amount
  "ReportingRequirements",
  #  "CertificationPeriods",
  "state",
  "year",
  "earned_income",
  "other_unearned",
  "self_employment_income",
  "ssi_income",
  "unearned_income",
  "wages_salaries",
  "raw_gross",
  "raw_net",
  "med_expenses",
  "shelter_expenses",
  "rent_mortage",
  "utilities",
  "anyone_working",
  "married",
  "shelter_to_gross_income_ratio",
  "BBCE",
  "homeless",
  "regioncd"
)

model_data <- df_cert_only %>% select("payment_error_i",features,"rescaled_weight","race_ethnicity","total_error_amount", "unit_composition_error")
model_data <- drop_na(model_data) 

#############################################
### 4. Preprocessing recipe
#############################################

make_recipe <- function(data){
  recipe(
    payment_error_i ~ ., 
    data = data %>% select(all_of(c("payment_error_i", features)))) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_zv(all_predictors()) #%>%
  #step_normalize(all_numeric_predictors())
}

#############################################
### 5. Models (RF + Boosted Trees)
#############################################

cores <- parallel::detectCores()

#########################
rf_spec <- rand_forest(
  mtry = 12,          
  trees = 2500,        
  min_n = 30
) %>%
  set_engine("ranger", num.threads = cores, importance="impurity") %>%
  set_mode("classification")

boost_spec <- boost_tree(
  mtry = 4, trees = 1500, min_n = 9, tree_depth = 8,
  learn_rate = 0.0512  , loss_reduction = 0.000183, sample_size = 0.168,
  stop_iter = 40
) |>
  set_engine("xgboost", nthread=5) |>
  set_mode("classification") |>
  translate()

multinom_spec <- multinom_reg() %>% 
  set_engine("nnet", penalty=0) %>% 
  set_mode("classification")


#############################################
### 6. Workflow + CV function
#############################################

run_ml <- function(model_spec, recipe, split) {
  wf <- workflow() %>%
    add_model(model_spec) %>%
    add_recipe(recipe)
  
  fit <- fit(wf, data = training(split))
  
  preds <- predict(fit, testing(split), type = "prob") %>%
    bind_cols(testing(split) %>% select(payment_error_i))
  
  # Build probability column list dynamically from truth levels
  lvls <- levels(testing(split)$payment_error_i)
  prob_cols <- paste0(".pred_", lvls)
  
  # Keep only columns that actually exist (in case a class is missing)
  prob_cols <- prob_cols[prob_cols %in% names(preds)]
  
  auc <- roc_auc(
    data  = preds,
    truth = payment_error_i,
    .pred_Yes
  )
  
  list(fit = fit, auc = auc)
}

tune_ml <- function(model_spec, recipe, folds, grid = 20) {
  wf <- workflow() %>%
    add_model(model_spec) %>%
    add_recipe(recipe)
  
  tune_grid(
    wf,
    resamples = folds,
    grid = grid,
    metrics = metric_set(roc_auc)
  )
}

make_split <- function(data, prop = 3/4) {
  initial_split(data, prop = prop, strata = payment_error_i)
}

make_folds <- function(split, v = 5) {
  vfold_cv(training(split), v = v, strata = payment_error_i)
}


#############################################
### 7. Run models (Certification errors)
#############################################

model_data <- model_data %>% select(-c(rescaled_weight,race_ethnicity,total_error_amount))
names(model_data)

#########SPLIT RUN###########

# Fix the random numbers by setting the seed 
# This enables the analysis to be reproducible when random numbers are used 
table(model_data$unit_composition_error)
unit_composition_df <- model_data %>% filter(unit_composition_error=="1" & payment_error_i=="Yes" | payment_error_i == "No")
summary(unit_composition_df$total_error_amount[unit_composition_df$payment_error_i=="Yes"])
names(unit_composition_df)

unit_composition_df <- unit_composition_df %>% select(-c(unit_composition_error,total_error_amount,rescaled_weight,race_ethnicity,state, HH_head_LF_status_c, ReportingRequirements))
names(unit_composition_df)
unit_composition_df_no_state <- unit_composition_df
unit_composition_df_no_state$state <- NULL



rf_recipe <- recipe(payment_error_i ~ ., data = unit_composition_df)
rf_res <- workflow() %>% add_model(rf_spec) %>% 
  add_recipe(rf_recipe)

data_split <- initial_split(unit_composition_df, prop = 3/4, strata=payment_error_i)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

set.seed(111)
rf_fit <- fit(rf_res, train_data)

preds <- predict(rf_fit, test_data, type = "prob") %>%
  bind_cols(test_data %>% select(payment_error_i))

auc <- roc_auc(
  data = preds,
  truth = outcome,
  c(.pred_above_threshold_error, .pred_correct, .pred_uncounted_error))

###regression models###


m7a <- multinom(outcome ~ cert_HH_size_FS_n + children_i + elderly_i + non_elderly_disabled_i +
             HH_head_LF_status_c + HH_size_rel_cert_HH_size + total_deductions_fs + cert_HH_size_FS_n * poly(rawben_rel_max,2) + year + state, data=model_data, maxit=1000)
summary(m7a)

m7a.counted.pl <- glm(as.numeric(outcome=="above_threshold_error") ~ cert_HH_size_FS_n + children_i +total_deductions_fs* elderly_i + total_deductions_fs*non_elderly_disabled_i +
                  HH_head_LF_status_c + HH_size_rel_cert_HH_size + total_deductions_fs + married + cert_HH_size_FS_n * poly(rawben_rel_max,2) + year + state, data=model_data)
summary(m7a.counted.pl)

m7b.uncounted.pl <- glm(as.numeric(outcome=="uncounted_error") ~ cert_HH_size_FS_n + children_i + total_deductions_fs*elderly_i + total_deductions_fs*non_elderly_disabled_i +
                HH_head_LF_status_c + HH_size_rel_cert_HH_size + total_deductions_fs + cert_HH_size_FS_n * poly(rawben_rel_max,2) + year + state, data=model_data)
summary(m7b.uncounted.pl)
table(model_data$income_error_c)

m7c.sp <- glm(as.numeric(outcome!="correct") ~ cert_HH_size_FS_n + children_i + total_deductions_fs* elderly_i + total_deductions_fs*non_elderly_disabled_i +
                HH_head_LF_status_c + HH_size_rel_cert_HH_size + total_deductions_fs + cert_HH_size_FS_n * rawben_rel_max + year + state, data=model_data)


model_data_ols <- model_data
model_data_ols <- model_data %>% mutate(elderly_or_disabled_i = as.numeric(elderly_i | non_elderly_disabled_i))

m7a.sp <- glm(as.numeric(outcome=="above_threshold_error") ~ cert_HH_size_FS_n + children_i +splines::ns(total_deductions_fs, 3) * elderly_or_disabled_i +
                HH_head_LF_status_c + HH_size_rel_cert_HH_size + cert_HH_size_FS_n * poly(rawben_rel_max,3) + year + state, data=model_data_ols)
summary(m7a.sp)

m7b.sp <- glm(as.numeric(outcome=="uncounted_error") ~ cert_HH_size_FS_n + children_i + splines::ns(total_deductions_fs, 3) * elderly_or_disabled_i +
                HH_head_LF_status_c + HH_size_rel_cert_HH_size + cert_HH_size_FS_n * poly(rawben_rel_max,3) + year + state, data=model_data_ols)
summary(m7b.sp)

table(model)

p1 <- plot_model(m7a.counted.pl, type="pred", terms=c("rawben_rel_max", "cert_HH_size_FS_n[1,2,4]"), ci.lvl=.95, axis.lim=list(c(0,1),c(0,.5))) + theme_bw() +ylab(NULL) + labs(title="counted payment errors", caption="Note: 76,135 cases, including 4,107 counted overpayment errors, 7,798 errors below threshold, 
                                                                                                                                                                                64,230 with no error. Logistic regression models using covariates in Table 1.") + xlab(NULL) + scale_color_viridis_d() + scale_fill_viridis_d()

p2 <- plot_model(m7b.uncounted.pl, type="pred", terms=c("rawben_rel_max", "cert_HH_size_FS_n[1,2,4]"), axis.lim=list(c(0,1),c(0,.5))) + theme_bw() +ylab("probability of error") + labs(title="uncounted payment errors") + xlab("benefit amount as proportion of max") + scale_color_viridis_d() + scale_fill_viridis_d()

p3 <- plot_model(m7c.sp, type="pred", terms=c("rawben_rel_max", "cert_HH_size_FS_n[1,2,4]"), axis.lim=list(c(0,1),c(0,.3))) + theme_bw() +ylab(NULL) + labs(title="any payment errors") + xlab(NULL) + scale_color_viridis_d() + scale_fill_viridis_d()


p2 + p1 + plot_layout(guides = "collect") & theme(legend.position = "left", legend.justification ="left") &
  labs(fill = "# in HH", color="# in HH") 

quantile(model_data$rawben_rel_max,.29)

ggsave(filename="hhSize_benAmountRelMax_payment_error_types.png", device="png", width = 8, height = 4, units = "in", dpi=300)


table(model_data_ols$total_deductions_fs>1300)
p3 <- plot_model(m7a.sp, type="pred", terms=c("total_deductions_fs", "elderly_or_disabled_i"), axis.lim=list(c(0,1000),c(0,.5))) + theme_bw() +ylab(NULL) + labs(title="counted payment errors") + xlab(NULL)

p4 <- plot_model(m7b.sp, type="pred", terms=c("total_deductions_fs", "elderly_or_disabled_i"), axis.lim=list(c(0,1000),c(0,.5))) + theme_bw() +ylab("probability of error") + labs(title="uncounted payment errors") + xlab("sum of deductions")

p4 + p3 + plot_layout(guides = "collect")  &
  labs(fill = "elderly or disabled", color="elderly or disabled") & theme(legend.position = "left", legend.justification ="left") &
  scale_color_discrete(labels = c("0" = "No", "1" = "Yes")) & scale_fill_discrete(labels = c("0" = "No",
                                                                                           "1" = "Yes")) 

ggsave(filename="deductions_by_elderly_or_disabled.png", device="png", width = 8, height = 4, units = "in", dpi=300)
#& scale_color_viridis_d(option="cividis") & scale_fill_viridis_d(option="cividis") &

#plot_model(m7a.sp, type="pred", terms=c("rawben_rel_max", "cert_HH_size_FS_n")) ,
#  "cert_HH_size_FS_n", "HH_head_LF_status_cunemployed and searching", 
#  "children_i"))

#######check logistic model accuracy with a holdout####
test <- predict(m7a, data=model_data, type="class")

model_data$test <- test
table(model_data$test, model_data$outcome) %>% prop.table
table(model_data$overpayment_error_i)

model_data_train <- split(model_data) 

train_data <- training(data_split)
test_data  <- testing(data_split)
names(model_data)
library(lme4)


model_data <- model_data %>% filter(HH_head_LF_status_c!="other") 
model_data$HH_head_LF_status_c <- droplevels(model_data$HH_head_LF_status_c)
table(model_data$HH_head_LF_status_c)
model_data$HH_head_LF_status_c <- factor(model_data$HH_head_LF_status_c, levels=c("not in labor force","employed by other","self-employed","unemployed and searching"))
table(model_data$state, model_data$expedited_i)


m7a.counted.hm <- glmer(payment_error_i ~ cert_HH_size_FS_n + children_i + elderly_i + non_elderly_disabled_i +
                        HH_head_LF_status_c + HH_size_rel_cert_HH_size + total_deductions_fs + gross_inc_to_poverty_FS + 
                          homeless + (1 | state), data=model_data, glmerControl(autoscale = TRUE), family=binomial)

summary(m7a.counted.hm)

m7a.counted.hm_rs1 <- glmer(payment_error_i ~ cert_HH_size_FS_n + children_i + elderly_i + non_elderly_disabled_i +
                          HH_head_LF_status_c + total_deductions_fs + rawben_rel_max  + (1 + HH_head_LF_status_c | state), 
                          data=model_data, nAGQ=0, glmerControl(autoscale = TRUE, optimizer = "nloptwrap", calc.derivs = FALSE), 
                          verbose=TRUE, family=binomial, weights=rescaled_weight)

summary(m7a.counted.hm_rs1)

m7a.counted.hm_rs2 <- glmer(payment_error_i ~ cert_HH_size_FS_n + children_i + elderly_i + non_elderly_disabled_i +
                              HH_head_LF_status_c + total_deductions_fs + rawben_rel_max + 
                              (1 + children_i + elderly_i + non_elderly_disabled_i | state), 
                            data=model_data, nAGQ=0, glmerControl(autoscale = TRUE, optimizer = "nloptwrap", calc.derivs = FALSE), 
                            verbose=TRUE, family=binomial, weights=rescaled_weight)

summary(m7a.counted.hm_rs2)

m7a.counted.hm_rs3 <- glmer(payment_error_i ~ cert_HH_size_FS_n + children_i + elderly_i + non_elderly_disabled_i +
                              HH_head_LF_status_c + total_deductions_fs + rawben_rel_max + (1 + rawben_rel_max | state), 
                            data=model_data, nAGQ=0, glmerControl(autoscale = TRUE, optimizer = "nloptwrap", calc.derivs = FALSE), 
                            verbose=TRUE, family=binomial, weights=rescaled_weight)

model_data_hh <- model_data %>% filter(cert_HH_size_FS_n < 5)

m7a.counted.hm_rs4 <- glmer(payment_error_i ~ cert_HH_size_FS_n + children_i + elderly_i + non_elderly_disabled_i +
                              HH_head_LF_status_c + HH_size_rel_cert_HH_size + total_deductions_fs + rawben_rel_max + 
                              (1 + cert_HH_size_FS_n | state), data=model_data_hh, nAGQ=0, 
                            glmerControl(autoscale = TRUE, optimizer = "nloptwrap", calc.derivs = FALSE), 
                            verbose=TRUE, family=binomial, weights=rescaled_weight)

summary(m7a.counted.hm_rs4)

#plot_lf_status <- plot_model(m7a.counted.hm_rs1,type="pred",
#                            terms=c("state","HH_head_LF_status_c"), pred.type="re") + coord_flip()
  

plot_lf_status <- plot_model(m7a.counted.hm_rs1, auto.label = FALSE, pred.type = "re", sort.est = F, axis.lim=c(0.5,5), colors = c("gray", "darkred")) + labs(title="change in odds of error by labor force status and state") + theme_bw()

plot_lf_status$data$facet <- ifelse(plot_lf_status$data$facet == "HH_head_LF_status_cemployed by other", "employed by other", 
                                   ifelse(plot_lf_status$data$facet == "HH_head_LF_status_cself-employed", "self-employed", 
                                          ifelse(plot_lf_status$data$facet == "HH_head_LF_status_cunemployed and searching", "unemployed and searching", 
                                                 ifelse(plot_lf_status$data$facet == "state (Intercept)", "state effect",
                                                        NA))))

plot_data <- plot_lf_status$data
plot_data_filtered <- plot_data[plot_data$facet != "state effect", ]

plot_data_filtered$term <- factor(plot_data_filtered$term , 
                                     levels = rev(sort(unique(plot_data_filtered$term))))

plot_lf_status_filtered <- ggplot(plot_data_filtered, 
                                   aes(x = estimate, y = term, color = group)) +
  geom_vline(xintercept = 1, linewidth = 1, color = "black") +  # Add thick line at x=1
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  facet_wrap(~facet) +
  scale_color_manual(values = c("gray", "darkred")) +
  labs(title = "change in odds of error by labor force status and state",
       x = "odds ratio",
       y = NULL, 
       caption = "N=190,591 with 15,690 errors. Logistic regression with random slopes for labor force statuses nested by state.") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank()) 

plot_lf_status_filtered
ggsave(filename="error_by_lf_status_and_state.png", device="png", width = 8, height = 10, units = "in", dpi=300)


plot_lf_status + ylim(c(0,3)) 

plot_hh_chars <- plot_model(m7a.counted.hm_rs2, auto.label = FALSE, type="re",sort.est = F, axis.lim=c(0.5,5), colors = c("gray", "darkred")) + labs(title="change in odds of error by household composition and state") + theme_bw()


plot_hh_chars$data$facet <- ifelse(plot_hh_chars$data$facet == "children_i", "children", 
                                   ifelse(plot_hh_chars$data$facet == "elderly_i", "elderly", 
                                          ifelse(plot_hh_chars$data$facet == "non_elderly_disabled_i", "disabled (non-elderly)", 
                                                 ifelse(plot_hh_chars$data$facet == "state (Intercept)", "state effect",
                                                        NA))))

plot_hh_data <- plot_hh_chars$data
unique(plot_hh_data$facet)
plot_hh_data_filtered <- plot_hh_data[plot_hh_data$facet != "state effect", ]

plot_hh_data_filtered$term <- factor(plot_hh_data_filtered$term , 
                                  levels = rev(sort(unique(plot_hh_data_filtered$term))))


plot_hh_chars_filtered <- ggplot(plot_hh_data_filtered, 
                                  aes(x = estimate, y = term, color = group)) +
  geom_vline(xintercept = 1, linewidth = 1, color = "black") +  # Add thick line at x=1
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  facet_wrap(~facet) +
  scale_color_manual(values = c("gray", "darkred")) +
  labs(title = "change in odds of error by household composition and state",
       x = "odds ratio",
       y = NULL, caption="N=190,591 with 15,690 errors. Logistic regression with random slopes for household characteristics nested by state.") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank()) 

plot_hh_chars_filtered

ggsave(filename="pred_error_by_hh_chars_and_state.png", device="png", width = 8, height = 10, units = "in", dpi=300)

plot_model(m7a.counted.hm_rs4,type="pred")

plot_hh_size_state <- plot_model(m7a.counted.hm_rs4,type="pred",
           terms=c("state","cert_HH_size_FS_n"), pred.type="re") + coord_flip()

plot_hh_size_state +
  scale_color_brewer(palette = "YlOrBr") +
  labs(title = "predicted probability of error by household size and state",
       x = NULL,
       y = "probability of error",
       fill="household size", color="household size",
       caption = "N=190,591 with 15,690 errors. Logistic regression with random slopes for household characteristics nested by state.") +
  theme_dark() +
  theme(legend.position = "left",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(), panel.border = element_rect(size = 1)) 


ggsave(filename="pred_error_by_hh_size_and_state.png", device="png", width = 8, height = 10, units = "in", dpi=300)
levels(model_data_hh$state)
model_data_hh$state <- factor(model_data_hh$state, 
                           levels = rev(sort(unique(model_data_hh$state))))

