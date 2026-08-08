# 0. Libraries
library(ranger)
library(yardstick)
library(dplyr)
library(tidyr)
library(here)
library(ggplot2)
library(scales)
library(haven)
options(max.print = 10000)

correct_variables <- TRUE
apply_correction_smoothing <- TRUE
exclude_2020_2021 <- TRUE
exclude_MFIP <- TRUE
exclude_SSI_CAP <- TRUE

# 1. Load data
folder <- paste0(here(), "/")

data_2017 <- read_sav(paste0(folder, "qc_data/qc_pub_fy2017.sav"))
data_2018 <- read_sav(paste0(folder, "qc_data/qc_pub_fy2018.sav"))
data_2019 <- read_sav(paste0(folder, "qc_data/qc_pub_fy2019.sav"))
if (exclude_2020_2021) {
  data_2020 <- NULL
  data_2021 <- NULL
} else {
  # Merge 2020 data which was split into three parts
  data_2020 <- bind_rows(
    read_sav(paste0(folder, "qc_data/qc_pub_fy2020.sav")),
    read_sav(paste0(folder, "qc_data/qc_pub_fy2020_per1.sav")),
    read_sav(paste0(folder, "qc_data/qc_pub_fy2020_per2.sav"))
  )
  data_2021 <- read_sav(paste0(folder, "qc_data/qc_pub_fy2021.sav"))
}
data_2022 <- read_sav(paste0(folder, "qc_data/qc_pub_fy2022.sav"))
data_2023 <- read_sav(paste0(folder, "qc_data/qc_pub_fy2023.sav"))
data_2024 <- read_sav(paste0(folder, "qc_data/qc_pub_fy2024.sav"))

# Merge all years
mydata <- bind_rows(
  data_2017,
  data_2018,
  data_2019,
  data_2020,
  data_2021,
  data_2022,
  data_2023,
  data_2024
)

rm(  data_2017,
     data_2018,
     data_2019,
     data_2020,
     data_2021,
     data_2022,
     data_2023,
     data_2024)

# Add state names to dataset
state_data <- read.csv(paste0(folder, "additional_data/state_data.csv"))
fips_to_state <- setNames(state_data$state, as.character(state_data$fips))
mydata$state_name <- fips_to_state[as.character(mydata$STATE)]

# Add year and month separately to dataset
mydata$year <- as.integer(substr(mydata$YRMONTH, 1, 4))
mydata$month <- as.integer(substr(mydata$YRMONTH, 5, 6))
mydata$fiscal_year <- ifelse(mydata$month >= 10, mydata$year + 1, mydata$year)
nrow(mydata) # 272416

# Add element and nature labels
qc_elements <- read.csv(paste0(folder, "additional_data/qc_elements.csv"))
qc_natures <- read.csv(paste0(folder, "additional_data/qc_natures.csv"))

# Join element descriptions
mydata <- mydata %>%
  left_join(
    qc_elements %>% rename(element = description),
    by = c("ELEMENT1" = "code")
  )

# Join nature descriptions
mydata <- mydata %>%
  left_join(
    qc_natures %>% rename(nature = description),
    by = c("NATURE1" = "code")
  )

# Add standard medical deduction amounts by year
smd_by_year <- read.csv(paste0(folder, "additional_data/standard_medical_deductions.csv"))
smd_long <- smd_by_year %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "fiscal_year",
    names_prefix = "X",
    values_to = "smd_amt"
  ) %>%
  mutate(
    fiscal_year = as.integer(fiscal_year),
    smd_amt = na_if(smd_amt, 0)
  )

mydata <- mydata %>%
  left_join(smd_long, by = c("state_name", "fiscal_year"))

# Correlations between variables
mean(abs(mydata$FSBEN - mydata$RAWBEN) <= 1, na.rm = TRUE) * 100 # 61.93%
mean(abs(mydata$FSNETINC - mydata$RAWNET) <= 1, na.rm = TRUE) * 100 # 93.52%
mean(abs(mydata$FSUSIZE - mydata$RAWHSIZE) <= 1, na.rm = TRUE) * 100 # 95.71%
mean(abs(mydata$FSGRINC - mydata$RAWGROSS) <= 1, na.rm = TRUE) * 100 # 99.40%
mean(abs(mydata$FSERNDED - mydata$RAWERND) <= 1, na.rm = TRUE) * 100 # 99.62%
mean(abs(mydata$FSMEDDED - mydata$FSMEDEXP) <= 1, na.rm = TRUE) * 100 # 99.96%
mean(abs(mydata$FSCSDED - mydata$FSCSEXP) <= 1, na.rm = TRUE) * 100 # 99.94%
mean(abs(mydata$FSSLTDED - mydata$SHELDED) <= 1, na.rm = TRUE) * 100 # 92.36%

# 2. Filter dataset 

# Load other dataset with the number of dropped observations
exclusions <- read.csv(paste0(folder, "additional_data/snap_qc_exclusion_all_years.csv"))

## If you want to be very careful, you could exclude state-months where there are more than 1 or even 0 cases not included
## this drops a lot of state-months and likely a lot of useful signal for most purposes, so it's commented out by default. 
# # Get state/year/month combos where ineligible_units (exclusions) > 1
# ineligible_combos <- exclusions[exclusions$ineligible_units > 1, 
#                                 c("state", "calendar_year", "month_num")]
# mydata <- mydata %>%
#   anti_join(ineligible_combos,
#             by = c("state_name" = "state",
#                    "year" = "calendar_year",
#                    "month" = "month_num"))
# nrow(mydata) # 141977

# See how often states are reporting element2
# We may want to drop certain states that only report one element

mydata <- mydata %>%
  group_by(state_name) %>%
  mutate(pct_element2 = sum(!is.na(ELEMENT2) & !is.na(ELEMENT1)) / sum(!is.na(ELEMENT1))) %>%
  ungroup()

# Drop all states with below 30% - optional
# mydata <- mydata[mydata$pct_element2 >= 0.30, ]
# nrow(mydata)

# Keep 48 states and DC only (related to max allotment)
mydata <- mydata[!mydata$state_name %in% c("Alaska", "Hawaii", "Guam", "Virgin Islands"), ]
nrow(mydata) # 135980

# Exclude MFIP and SSI_CAP cases
# Due to non-standard benefits calculations
if (exclude_MFIP)    mydata <- mydata[mydata$MN_FIP  %in% 0, ]
if (exclude_SSI_CAP) mydata <- mydata[mydata$SSI_CAP %in% 0, ]
nrow(mydata)

# If you wanted to be very careful, you could drop all observations with second error elements 
## we lose even more signal from errors for most purposes so commenting out by default
# (to focus on single error rows)
#mydata <- mydata[is.na(mydata$ELEMENT2), ]
#nrow(mydata) # 125307

# Drop all rows if we can't get the amterr to be close 
# to the difference between fsben and rawben
mydata$absbendiff <- abs(mydata$RAWBEN - mydata$FSBEN)
mydata <- mydata[abs(mydata$absbendiff - mydata$AMTERR) <= 5, ]
nrow(mydata) # 117000

# Deduction-field NAs come in state-level blocks (e.g., WA, MS, MN leave the
# optional deduction fields unrecorded for a subset of cases) and almost
# certainly mean "not claimed / not recorded", not unusable data. Dropping
# them cost some states ~16% of their cases. Zero-fill instead, with a flag
# (2026-07-07; previously these rows were dropped).
ded_na_fields <- c("FSDEPDED", "FSMEDDED", "FSCSDED", "FSSTDDED", "HOMELESS_DED")
mydata$ded_fields_imputed <- rowSums(is.na(mydata[ded_na_fields])) > 0
for (v in ded_na_fields) mydata[[v]][is.na(mydata[[v]])] <- 0
cat("rows with zero-filled deduction fields:", sum(mydata$ded_fields_imputed), "\n")

# Drop only rows where core shelter fields are NA (rarely triggers)
mydata <- mydata %>%
  filter(!is.na(RENT), !is.na(UTIL))
nrow(mydata) # was 113754 when the deduction fields were dropped instead

# Track whether a second error element was reported. Multi-element cases are
# KEPT (they are ~30% of error cases); the indicator is for QA/reporting, not
# recommended as a mining feature — states report second elements very
# inconsistently, so it would encode reporting practice, not error risk.
mydata$second_element_i <- !is.na(mydata$ELEMENT2)

# 3. Create error threshold by year
year_data <- read.csv(paste0(folder, "additional_data/year_data.csv"))
threshold_by_year <- setNames(year_data$error_threshold, as.character(year_data$year))

# Create over_threshold using the correct threshold for each row's year
# Make sure to use fiscal year created earlier.
mydata$threshold <- threshold_by_year[as.character(mydata$fiscal_year)]
mydata$over_threshold <- as.factor(ifelse(
  abs(mydata$absbendiff) > threshold_by_year[as.character(mydata$fiscal_year)],
  1, 0
))

# Max shelter deduction by year
max_shelter_by_year <- setNames(year_data$max_shelter_deduction, as.character(year_data$year))
mydata$max_shelter_deduction <- max_shelter_by_year[as.character(mydata$fiscal_year)]

# No max shelter deduction if a household member is elderly or has a disability
mydata$max_shelter_deduction <- ifelse(
  mydata$FSNELDER + mydata$FSNDIS > 0,
  Inf,
  mydata$max_shelter_deduction
)

# Medicare Part B Premium by Year
medicare_part_b_premium_by_year <- setNames(year_data$medicare_part_b_premium, as.character(year_data$year))
mydata$medicare_part_b_premium <- medicare_part_b_premium_by_year[as.character(mydata$fiscal_year)]

# Step 4. Recreate the FS benefit using the formula inputs

# Adjust minimum ben (it should be $0 for households >2)
mydata$fsminimum_ben <- ifelse(mydata$FSUSIZE < 3, mydata$MINIMUM_BEN, 0)

# Recalculate FSBEN uncapped
mydata$fsnet_before_shelter <- mydata$FSGRINC - (
  mydata$FSERNDED +
    mydata$FSDEPDED +
    mydata$FSMEDDED +
    mydata$FSCSDED +
    mydata$FSSTDDED
)
mydata$fsnet_adjusted_half <- pmax(mydata$fsnet_before_shelter * 0.5, 0)
mydata$fssltded_uncapped <- mydata$RENT + mydata$UTIL- mydata$fsnet_adjusted_half
mydata$fssltded_recalculated <- ifelse(
  is.infinite(mydata$max_shelter_deduction),
  pmax(mydata$fssltded_uncapped, 0),
  pmin(pmax(mydata$fssltded_uncapped, 0), mydata$max_shelter_deduction)
)
mydata$fsnet_allow_negative = mydata$fsnet_before_shelter - (
  mydata$fssltded_recalculated +
    mydata$HOMELESS_DED
)
mydata$fsnet_allow_negative <- floor(mydata$fsnet_allow_negative)
mydata$fsben_uncapped <- mydata$BENMAX - (0.3 * mydata$fsnet_allow_negative)
mydata$fsben_uncapped <- floor(mydata$fsben_uncapped)
mydata$fsben_recreated <- pmax(mydata$fsben_uncapped, mydata$fsminimum_ben)
mydata$fsben_recreated <- pmin(mydata$fsben_recreated, mydata$BENMAX)
mydata$unc_fsben_rel_max <- mydata$fsben_uncapped / mydata$BENMAX

# CHECK - should be >99.9%
mean(mydata$fsben_recreated == mydata$FSBEN) * 100 

# Step 5. Create uncorrected aka "raw" variables for all helpful variables

# Set default values (0 if n/a)
# Creates new variable names following this format: "FSEARN" -> "rawearn"
vars <- c(
  "FSUSIZE",
  "FSGRINC",
  "FSEARN",
  "FSUNEARN",
  "FSERNDED",
  "FSMEDDED",
  "FSDEPDED",
  "FSCSDED",
  "FSSLTDED",
  "FSSLTEXP",
  "RENT",
  "UTIL",
  "FSSTDDED",
  "HOMELESS_DED",
  "BENMAX",
  "MINIMUM_BEN"
)
for (v in vars) {
  newname <- paste0("raw", tolower(sub("^FS", "", v)))
  print(paste("Creating:", newname))
  mydata[[newname]] <- ifelse(is.na(mydata[[v]]), 0, mydata[[v]])
}

# Recalculate raw income formula
calculate_raw_benefits <- function(mydata) {
  
  mydata$rawernded <- mydata$rawearn * 0.2
  mydata$rawgrinc <- mydata$rawearn + mydata$rawunearn
  mydata$rawnet_before_shelter <- mydata$rawgrinc - (
    mydata$rawernded +
      mydata$rawdepded +
      mydata$rawmedded +
      mydata$rawcsded +
      mydata$rawstdded
  )
  mydata$rawnet_adjusted_half <- pmax(mydata$rawnet_before_shelter * 0.5, 0)
  mydata$rawsltexp <- mydata$rawrent + mydata$rawutil
  mydata$rawsltded_uncapped <- mydata$rawsltexp - mydata$rawnet_adjusted_half
  mydata$rawsltded_uncapped <- floor(mydata$rawsltded_uncapped)
  mydata$rawsltded <- ifelse(
    is.infinite(mydata$max_shelter_deduction),
    pmax(mydata$rawsltded_uncapped, 0),
    pmin(pmax(mydata$rawsltded_uncapped, 0), mydata$max_shelter_deduction)
  )
  mydata$rawsltded <- floor(mydata$rawsltded)
  mydata$rawnet_allow_negative = mydata$rawnet_before_shelter - (
    mydata$rawsltded +
      mydata$rawhomeless_ded
  )
  mydata$rawnet_allow_negative <- floor(mydata$rawnet_allow_negative)
  mydata$rawben_uncapped <- mydata$rawbenmax - (0.3 * mydata$rawnet_allow_negative)
  mydata$rawben_uncapped <- floor(mydata$rawben_uncapped)
  mydata$rawben_recreated <- pmax(mydata$rawben_uncapped, mydata$rawminimum_ben)
  mydata$rawben_recreated <- pmin(mydata$rawben_recreated, mydata$rawbenmax)
  mydata$rawnet_capped = pmax(mydata$rawnet_allow_negative, 0) 
  mydata$unc_rawben_rel_max <- mydata$rawben_uncapped / mydata$rawbenmax
  mydata$at_max <- (mydata$rawben_uncapped + 5) >= mydata$rawbenmax
  
  mydata
  
}

# Corrected variable notes 
mydata$correctednotes <- "no_change"
mydata$correctedamount <- 0

# Step 6. Unit Composition Errors
# Difficulty is that standard deduction and maximum benefit will both change

# Read in the maximum benefits based on unit size
max_allotments <- read.csv(paste0(folder, "additional_data/max_allotments.csv"))
max_allotments_long <- reshape(
  max_allotments,
  varying = names(max_allotments)[-1],
  v.names = "rawbenmax",
  timevar = "hh_size",
  times = as.integer(gsub("X", "", names(max_allotments)[-1])),
  direction = "long"
)
max_allotments_long <- max_allotments_long[, c("year", "hh_size", "rawbenmax")]
get_max_allotment <- function(hh_size, fiscal_year) {
  result <- max_allotments_long[
    max_allotments_long$hh_size == hh_size & 
      max_allotments_long$year == fiscal_year, 
    "rawbenmax"
  ]
  if(length(result) == 0) return(NA)
  return(result)
}

# Read in the standard deductions based on unit size
standard_deductions <- read.csv(paste0(folder, "additional_data/standard_deductions.csv"))
standard_deductions_long <- reshape(
  standard_deductions,
  varying = names(standard_deductions)[-1],
  v.names = "rawstdded",
  timevar = "hh_size",
  times = as.integer(gsub("X", "", names(standard_deductions)[-1])),
  direction = "long"
)
standard_deductions_long <- standard_deductions_long[, c("year", "hh_size", "rawstdded")]

get_standard_deduction <- function(hh_size, fiscal_year) {
  result <- standard_deductions_long[
    standard_deductions_long$hh_size == hh_size & 
      standard_deductions_long$year == fiscal_year, 
    "rawstdded"
  ]
  if(length(result) == 0) return(NA)
  return(result)
}

# Confirm rawbenmax matches the MAXBEN in data
# Some states included the additional 2021 pandemic 15% allotment boost
# Drop these rows
if("rawbenmax" %in% names(mydata)) mydata$rawbenmax <- NULL
mydata <- mydata %>%
  left_join(
    max_allotments_long,
    by = c("fiscal_year" = "year", "rawusize" = "hh_size")
  )
mydata <- mydata[mydata$BENMAX == mydata$rawbenmax, ]
nrow(mydata) # 109800

# For unit comp errors we find what the rawbenmax should be as it'll change if household size changes
# And same thing with the shelter allowance

# Subtract 1 to rawusize for element 150, excluded eligible person
# Only natures that don't affect expenses
mydata$rawusize <- ifelse(
  mydata$ELEMENT1 == 150 & mydata$NATURE1 %in% c(12, 14, 16),
  mydata$rawusize - 1,
  mydata$rawusize
)
mydata$correctednotes <- ifelse(
  mydata$ELEMENT1 == 150 & mydata$NATURE1 %in% c(12, 14, 16),
  "hhsize_up",
  mydata$correctednotes
)

# Add 1 from rawusize for element 150, ineligible person included
mydata$rawusize <- ifelse(
  mydata$ELEMENT1 == 150 & mydata$NATURE1 %in% c(7),
  mydata$rawusize + 1,
  mydata$rawusize
)
mydata$correctednotes <- ifelse(
  mydata$ELEMENT1 == 150 & mydata$NATURE1 %in% c(7),
  "hhsize_down",
  mydata$correctednotes
)

# Update maxben and standard deduction with new household sizes
if (correct_variables) {
  mydata$rawbenmax <- mapply(
    get_max_allotment,
    hh_size = mydata$rawusize,
    fiscal_year = mydata$fiscal_year
  )
}
if (correct_variables) {
  mydata$rawstdded <- mapply(
    get_standard_deduction,
    hh_size = mydata$rawusize,
    fiscal_year = mydata$fiscal_year
  )
}
if (correct_variables) {
  mydata$rawminimum_ben <- ifelse(mydata$rawusize < 3, mydata$MINIMUM_BEN, 0)
}

# Step 7. Adjust other variables
# This method shifts variables up or down by $3 until the equation
# balances or hit another limiting parameter (going below $0)

income_shift <- 3
max_iterations <- 1000

adjust_income <- function(mydata, col, elements, prefix, max_iter = max_iterations) {
  
  eligible <- mydata$ELEMENT1 %in% elements
  original <- mydata[[col]]  # save before any changes
  
  for (i in seq_len(max_iter)) {
    mydata <- calculate_raw_benefits(mydata)
    
    direction <- mydata$RAWBEN - mydata$fsben_uncapped
    diff <- mydata$RAWBEN - mydata$rawben_recreated
    diff_matches <- abs(diff) <= 3
    
    done <- ((direction > 0 & mydata[[col]] <= 0) | mydata$rawben_recreated <= 0 | 
               (direction < 0 & mydata$rawben_recreated < mydata$RAWBEN) |
               (direction > 0 & mydata$rawben_recreated > mydata$RAWBEN) | 
               (direction < 0 & mydata$rawben_uncapped < 0) | !eligible)
    if (all(done, na.rm = TRUE)) break
    
    mydata[[col]] <- ifelse(
      !done & direction > 0,
      pmax(mydata[[col]] - income_shift, 0),
      ifelse(
        !done & direction < 0,
        mydata[[col]] + income_shift,
        mydata[[col]]
      )
    )
    
  }
  
  # Set correctednotes after loop completes
  changed <- mydata[[col]] != original
  hit_zero <- (direction > 0) & (mydata[[col]] <= 0 | mydata$rawben_recreated <= 0)
  
  mydata$correctednotes <- ifelse(
    eligible & hit_zero & !diff_matches, paste0(prefix, "_error"),
    ifelse(
      eligible & changed & mydata$RAWBEN > mydata$fsben_uncapped, paste0(prefix, "_down"),
      ifelse(
        eligible & changed & mydata$RAWBEN < mydata$fsben_uncapped, paste0(prefix, "_up"),
        ifelse(
          eligible, paste0(prefix, "_no_change"),
          mydata$correctednotes
        )
      )
    )
  )
  
  mydata$correctedamount <- ifelse(
    eligible,
    mydata[[col]] - original,
    mydata$correctedamount
  )
  
  mydata
}

adjust_shelter <- function(mydata, col, elements, prefix, max_iter = max_iterations) {
  
  eligible <- mydata$ELEMENT1 %in% elements
  original <- mydata[[col]]  # save before any changes
  
  for (i in seq_len(max_iter)) {
    mydata <- calculate_raw_benefits(mydata)
    
    direction <- mydata$RAWBEN - mydata$FSBEN
    diff <- mydata$RAWBEN - mydata$rawben_recreated
    diff_matches <- abs(diff) <= 3
    
    done <- (diff_matches | (mydata[[col]] <= 0 & direction < 0) | mydata$rawben_recreated <= 0 |
               (direction > 0 & mydata$rawben_recreated > mydata$RAWBEN) |
               (direction < 0 & mydata$rawben_recreated < mydata$RAWBEN) |
               (direction > 0 & mydata$rawsltded_uncapped >= mydata$max_shelter_deduction) |
               (direction < 0 & mydata$rawsltded <= 0) | !eligible)
    
    if (all(done, na.rm = TRUE)) break
    
    mydata[[col]] <- ifelse(
      !done & direction < 0,
      pmax(mydata[[col]] - income_shift, 0),
      ifelse(
        !done & direction > 0,
        mydata[[col]] + income_shift,
        mydata[[col]]
      )
    )
    
  }
  
  # Set correctednotes after loop completes
  hit_zero <- (mydata[[col]] <= 0) & !diff_matches & direction < 0
  hit_cap <- !diff_matches & direction > 0 & mydata$rawsltded >= mydata$max_shelter_deduction
  
  mydata$correctednotes <- ifelse(
    eligible & (hit_zero | hit_cap) & prefix != "util", paste0(prefix, "_error"),
    ifelse(
      eligible & direction > 0, paste0(prefix, "_up"),
      ifelse(
        eligible & direction < 0, paste0(prefix, "_down"),
        ifelse(
          eligible, paste0(prefix, "_no_change"),
          mydata$correctednotes
        )
      )
    )
  )
  
  mydata$correctedamount <- ifelse(
    eligible,
    mydata[[col]] - original,
    mydata$correctedamount
  )
  
  mydata
}

adjust_other <- function(mydata, col, elements, prefix, max_iter = max_iterations) {
  
  eligible <- mydata$ELEMENT1 %in% elements
  original <- mydata[[col]]  # save before any changes
  
  for (i in seq_len(max_iter)) {
    mydata <- calculate_raw_benefits(mydata)
    
    direction <- mydata$RAWBEN - mydata$FSBEN
    diff <- mydata$RAWBEN - mydata$rawben_recreated
    diff_matches <- abs(diff) <= 3
    
    done <- (diff_matches | (mydata[[col]] <= 0 & direction < 0) | mydata$rawben_recreated <= 0 |
               (direction > 0 & mydata$rawben_recreated > mydata$RAWBEN) |
               (direction < 0 & mydata$rawben_recreated < mydata$RAWBEN) |
               (direction < 0 & mydata[[col]] <= 0) | !eligible)
    
    if (all(done, na.rm = TRUE)) break
    
    mydata[[col]] <- ifelse(
      !done & direction < 0,
      pmax(mydata[[col]] - income_shift, 0),
      ifelse(
        !done & direction > 0,
        mydata[[col]] + income_shift,
        mydata[[col]]
      )
    )
    
  }
  
  # Set correctednotes after loop completes
  hit_zero <- (mydata[[col]] <= 0) & !diff_matches & direction < 0
  hit_cap <- !diff_matches & direction > 0 
  
  mydata$correctednotes <- ifelse(
    eligible & (hit_zero | hit_cap), paste0(prefix, "_error"),
    ifelse(
      eligible & direction > 0, paste0(prefix, "_up"),
      ifelse(
        eligible & direction < 0, paste0(prefix, "_down"),
        ifelse(
          eligible, paste0(prefix, "_no_change"),
          mydata$correctednotes
        )
      )
    )
  )
  
  mydata$correctedamount <- ifelse(
    eligible,
    mydata[[col]] - original,
    mydata$correctedamount
  )
  
  mydata
}


if (correct_variables){
  
  mydata <- adjust_income(mydata, 
                          col = "rawunearn", 
                          elements = c(331, 332, 333, 334, 335, 336, 342, 343, 344, 345, 346, 350), 
                          prefix = "unearn")
  mydata <- adjust_income(mydata, 
                          col = "rawearn", 
                          elements = c(311, 312, 314, 321), 
                          prefix = "earn")
  mydata <- adjust_shelter(mydata, 
                           col = "rawrent", 
                           elements = c(363), 
                           prefix = "rent")
  mydata <- adjust_shelter(mydata, 
                           col = "rawutil", 
                           elements = c(364), 
                           prefix = "util")
  mydata <- adjust_other(mydata, 
                         col = "rawmedded", 
                         elements = c(365), 
                         prefix = "med")
  mydata <- adjust_other(mydata, 
                         col = "rawdepded", 
                         elements = c(323), 
                         prefix = "dep")
  mydata <- adjust_other(mydata, 
                         col = "rawcsded", 
                         elements = c(366), 
                         prefix = "cs")
  
  # For util, we have first set it to what we want it to be
  # This isn't possible, as there are fixed utility allowances.
  # Find the nearest allowance for the given state/year combo and set to that
  valid_util <- mydata %>%
    group_by(state_name, year) %>%
    count(UTIL) %>%
    filter(n > 5) %>%
    select(state_name, year, UTIL)
  mydata <- mydata %>%
    rowwise() %>%
    mutate(
      rawutil = {
        if (!grepl("util", correctednotes)) rawutil
        else {
          valid <- valid_util$UTIL[valid_util$state_name == state_name & valid_util$year == year]
          if (correctednotes == "util_up") {
            valid <- valid[valid > UTIL]
            if (length(valid) == 0 || is.na(rawutil)) rawutil
            else valid[which.min(abs(valid - rawutil))]
          } else if (correctednotes == "util_down") {
            valid <- valid[valid < UTIL]
            if (length(valid) == 0 || is.na(rawutil)) 0
            else valid[which.min(abs(valid - rawutil))]
          } else rawutil
        }
      }
    ) %>%
    ungroup()
}


# Step 8. Evaluate corrections
mydata <- calculate_raw_benefits(mydata)

# If we can recreate the rawben for all cases, we have corrected the dataset
testing_margin <- 5
keep_rows <- rep(TRUE, nrow(mydata)) # Optionally edit which rows for testing

# 1. Overall percent within $testing_margin
overall <- round(
  mean(
    abs(mydata$RAWBEN[keep_rows] - mydata$rawben_recreated[keep_rows]) <= testing_margin,
    na.rm = TRUE
  ) * 100,
  4
)

# 2. Percent within $testing_margin where over_threshold == 1
overthr_num <- sum(
  abs(
    mydata$RAWBEN[keep_rows & mydata$over_threshold == 1] -
      mydata$rawben_recreated[keep_rows & mydata$over_threshold == 1]
  ) <= testing_margin,
  na.rm = TRUE
)

overthr_den <- sum(!is.na(
  mydata$RAWBEN[keep_rows & mydata$over_threshold == 1] -
    mydata$rawben_recreated[keep_rows & mydata$over_threshold == 1]
))

overthr <- round(overthr_num / overthr_den * 100, 4)

# See results
overall
cat(overthr_num, "of", overthr_den, "=", overthr, "%\n")

### Variable smoothing - chart to remove 1.0 spikes

# Function to see what percent at max and chart
at_max <- function(data, tolerance = 0.01) {
  subset_data <- data[data$rawusize == 3, ]
  print(mean(subset_data$unc_rawben_rel_max >= (1 - tolerance) &
               subset_data$unc_rawben_rel_max <= (1 + tolerance), na.rm = TRUE) * 100)
  
  x_var <- "unc_rawben_rel_max"
  hh_size <- 3
  min_obs <- 30
  
  chart_data <- mydata %>%
    filter(rawusize == hh_size, .data[[x_var]] >= 0) %>%
    mutate(bin = round(.data[[x_var]] / 0.02) * 0.02) %>%
    group_by(bin) %>%
    summarise(
      error_rate = mean(over_threshold == 1, na.rm = TRUE),
      n = n()
    ) %>%
    filter(n > min_obs) %>%
    mutate(fraction = n / sum(n))
  
  ggplot(chart_data, aes(x = bin)) +
    geom_line(aes(y = error_rate, color = "Error Rate")) +
    geom_point(aes(y = error_rate, color = "Error Rate")) +
    geom_line(aes(y = fraction, color = "Fraction of Cases")) +
    geom_point(aes(y = fraction, color = "Fraction of Cases")) +
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(values = c("Error Rate" = "black", "Fraction of Cases" = "blue")) +
    labs(
      title = paste0("Error Rate vs Fraction of Cases\n(Household Size = ", hh_size, ")"),
      x = paste0("Ratio of Uncapped Benefit to Max Benefit", "\n(2% Bins; Min ", min_obs, " Obs)"),
      y = "Percent",
      color = ""
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}
mydata <- calculate_raw_benefits(mydata)
at_max(mydata) # 5.83

# If we reduced earned income down by over 75%, and at max, set it to $0
if (apply_correction_smoothing) {
  mydata$rawearn <- ifelse(
    mydata$correctednotes == "earn_down" &
      mydata$at_max &
      abs(mydata$correctedamount) > 0.75 * mydata$FSEARN,
    0,
    mydata$rawearn
  )
  mydata <- calculate_raw_benefits(mydata)
  at_max(mydata) # 5.43
}

# If we reduced unearned income down by over 75%, and at max, set it to $0
if (apply_correction_smoothing) {
  mydata$rawunearn <- ifelse(
    mydata$correctednotes == "unearn_down" &
      mydata$at_max &
      abs(mydata$correctedamount) > 0.75 * mydata$FSUNEARN,
    0,
    mydata$rawunearn
  )
  mydata <- calculate_raw_benefits(mydata)
  at_max(mydata) # 5.29
}

# Scale deduction variables to match mean values for other households
# that are at max or above (e.g. make the data equally represent)
scale_by_ratio <- function(data, notes_value, col_name) {
  
  # Calculate comparison and ratios
  comparison <- data %>%
    filter(correctednotes %in% c(notes_value, "no_change"),
           at_max,
           .data[[col_name]] != 0) %>%
    group_by(rawusize, correctednotes) %>%
    summarise(mean_val = mean(.data[[col_name]], na.rm = TRUE),
              n = n(),
              .groups = "drop") %>%
    arrange(rawusize, correctednotes)
  
  ratios <- comparison %>%
    select(rawusize, correctednotes, mean_val) %>%
    tidyr::pivot_wider(names_from = correctednotes, values_from = mean_val) %>%
    mutate(ratio = .data[[notes_value]] / no_change)
  
  print(ratios)
  
  # Apply scaling
  target_rows <- !is.na(data$correctednotes) & 
    data$correctednotes == notes_value & 
    !is.na(data$at_max) &
    data$at_max
  
  data[[col_name]][target_rows] <- sapply(which(target_rows), function(i) {
    size <- data$rawusize[i]
    ratio <- ratios$ratio[ratios$rawusize == size]
    if (length(ratio) == 0 || is.na(ratio)) ratio <- 1
    data[[col_name]][i] * ratio
  })
  
  data
}

# Scale variables
if (apply_correction_smoothing) {
  mydata <- scale_by_ratio(mydata, "dep_up", "rawdepded")
  mydata <- calculate_raw_benefits(mydata)
  at_max(mydata) # 5.23
  mydata <- scale_by_ratio(mydata, "cs_up", "rawcsded")
  mydata <- calculate_raw_benefits(mydata)
  at_max(mydata) # 5.21
  mydata <- scale_by_ratio(mydata, "util_up", "rawsltded")
  mydata <- calculate_raw_benefits(mydata)
  at_max(mydata) # 4.70
  mydata <- scale_by_ratio(mydata, "rent_up", "rawsltded")
  mydata <- calculate_raw_benefits(mydata)
  at_max(mydata) # 4.70
  mydata <- scale_by_ratio(mydata, "med_up", "rawmedded")
  mydata <- calculate_raw_benefits(mydata)
  at_max(mydata) # 4.69
  mydata <- scale_by_ratio(mydata, "earn_down", "rawearn")
  mydata <- calculate_raw_benefits(mydata)
  at_max(mydata) # 3.33
  mydata <- scale_by_ratio(mydata, "unearn_down", "rawunearn")
  mydata <- calculate_raw_benefits(mydata)
  at_max(mydata) # 2.23
}

mydata <- mydata %>% mutate(raw_total_deductions = rawdepded + rawcsded +
                              rawsltded + rawmedded + rawernded)

#### Add additional features from features.R
source("features.R")
mydata <- add_features(mydata)

#### Misc calculations for write up ####

# How often states included a second error element
# "0% - 50%"
table(mydata$pct_element2)

# Elements that could not be corrected
mydata %>%
  filter(abs(RAWBEN - rawben_recreated) > 5, over_threshold == 1, correctednotes == "no_change") %>%
  count(ELEMENT1) %>%
  arrange(desc(n)) %>%
  print(n = Inf)

# Correlation between element 2 reporting and correction errors
# "p-value = 0.0001086"
result <- mydata %>%
  group_by(state_name) %>%
  summarise(
    n_errors     = sum(grepl("error", correctednotes, ignore.case = TRUE)),
    n_total      = n(),
    ratio        = n_errors / n_total,
    pct_element2 = first(pct_element2)
  ) %>%
  arrange(desc(ratio))

cor(result$ratio, result$pct_element2, use = "complete.obs")
cor.test(result$ratio, result$pct_element2)

# Save data
#write_sav(mydata, paste0(folder, "final.sav"))
saveRDS(mydata, paste0(folder, "final.rds"))

#df <- mydata
#rm(mydata)
df <- readRDS(paste0(folder, "final.rds"))

#### variable cleaning / recoding ###
names(df) <- tolower(names(df))

df <- df %>%
  mutate(
    min_age = do.call(pmin, c(across(num_range("age", 1:18)), na.rm = TRUE)),
    max_age = do.call(pmax, c(across(num_range("age", 1:18)), na.rm = TRUE))
  )
summary(df$min_age)
summary(df$max_age)

df <- df %>%
  mutate(children_present = if_else(fsnkid > 0, T, F))
table(df$min_age, df$children_present)

df <- df %>%
  mutate(
    elderly_present = if_else(fsnelder > 0, T, F))
table(df$max_age, df$elderly_present)


df <- df %>%
  mutate(
    lf_composition = if_else(min_age > 17 & max_age < 60, "all working age",
                             if_else(max_age < 18 | min_age > 59, "no working age",
                                     "mixed ages"))
  )


# ABWDST1 to ABWDST18
# 2 = Able-bodied adult without dependents (ABAWD) meeting work requirements in 7 CFR.273.24(a)(1)
# 3 = ABAWD in a waived area
# 4 = ABAWD exempt for one month based on discretionary exemption
# 5 = ABAWD receiving a time-limited month and not meeting work requirements

df <- df %>%
  mutate(count_abawd = rowSums(across(num_range("abwdst", 1:18), ~ .x %in% 2:5)),
         pct_abawd = count_abawd / certhhsz)
#df %>% select(count_abawd, certhhsz, pct_abawd, abwdst1, age1) %>% sample_n(size=20)

df$lf_composition <- factor(df$lf_composition)

df <- df %>% mutate(expedited_i = expedser<3) # 1 and 2 are for expedited, 2 is for not timely; 3 means not expedited

df <- df %>% mutate(non_elderly_disabled_i = as.factor(fsndis>0))
df <- df %>%
  mutate(married = as.integer(if_any(rel1:rel16, ~ .x == 2)))

df$married[is.na(df$married)] <- 0


table(df$lf_composition, df$count_abawd>0)
table(df$status)
df$rawgross <- NULL
reg_model_data <- df %>%
  rename(cert_HH_size_FS_n = rawusize, #reported unit size for case (raw variable)
         single_female_head = fsngmom, #Indicator of single-female–headed unit
         benefit_amount_FS = fsben, # "Final calculated benefit"
         net_income_allow_neg = rawnet_allow_negative, #Final net countable unit income
         gross_inc_to_poverty_FS = tpov, #"Gross income/poverty level ratio"
         raw_benefit_amount = rawben_recreated, #"Reported SNAP benefit received"
         maximum_benefit_for_HH_size = rawbenmax, #Maximum benefit amount
         total_error_amount = amterr, #"Amount of benefit in error"
         children_i = children_present,
         elderly_i = elderly_present,
         months_since_cert_n = lastcert, #Months since last SNAP certification
         months_certification_period_n = certmth, # months in certification period
         total_deductions = raw_total_deductions, #Total deductions
         total_assets_fs = fsasset, #total countable assets under state rules
         people_in_HH_n = ctprhh, #number of people in household
         action_type_c = actntype, #most recent action type
         adjusted_allotment = benfix,
         medical_deductions = rawmedded,
         percent_abawd = pct_abawd,
         rawgross = rawgrinc,
         utilities = rawutil) 

reg_model_data <- reg_model_data %>% 
  mutate(state = as.factor(state_name),
         year = as.factor(fiscal_year),
         HH_size_n = as.integer(cert_HH_size_FS_n),
         earned_by_hh_size = rawearn / HH_size_n,
         unearned_by_hh_size = rawunearn / HH_size_n,
         rawben_rel_max = raw_benefit_amount / maximum_benefit_for_HH_size, 
         gross_by_hh_size =  rawgross / HH_size_n,
         shelter_expenses = rawrent + utilities,
         shelter_to_gross_ratio = (rawrent + utilities) / rawgross,
         shelter_expenses_by_hh_size = shelter_expenses/ HH_size_n,
         medical_deductions_by_hh_size = medical_deductions / HH_size_n,
         elderly_disabled_i = fsnelder>0 | fsndis>0,
         total_deductions_by_hh_size = total_deductions / HH_size_n,
         homeless = as.factor(homeded!=1))


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

reg_model_data <- reg_model_data %>%
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

reg_model_data$count_divisible_by_100 <- rowSums(
  sapply(reg_model_data[, tolower(all_vars)], function(x) x > 0 & x %% 100 == 0),
  na.rm = TRUE
)

reg_model_data  <- reg_model_data  %>%
  mutate(has_earned_inc_error = !is.na(element) & tolower(element) %in% c(
      "wages and salaries",
      "self-employment",
      "other earned income"),
    has_unearned_inc_error = !is.na(element) & tolower(element) %in% c(
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
    ))

reg_model_data <- reg_model_data %>%
  mutate(
    error_status = case_when(
      has_earned_inc_error & status == 2 & over_threshold==1 ~ "earned_overissuance",
      has_earned_inc_error & status == 3 & over_threshold==1~ "underissuance",
      has_unearned_inc_error & status == 2& over_threshold==1 ~ "unearned_overissuance",
      has_unearned_inc_error & status == 3 & over_threshold==1~ "underissuance",
      (!has_earned_inc_error & !has_unearned_inc_error) & status %in% c(2,3) & over_threshold==1 ~ "other_error",
      over_threshold==0 ~ "no_error",
      TRUE ~ NA_character_), 
    error_timing = case_when(
      timeper1 == 2 ~ "at_certification",
      timeper1 %in% c(1,3) ~ "before_or_after_certification",
      TRUE ~ "unknown"
    ),
    most_recent_action = case_when(
      action_type_c == 1 ~ "certification",
      action_type_c == 2 ~ "recertification",
      TRUE ~ NA
    )
  )


# Cache the modeling frame for the run_*.R runners (added 2026-07-07)
saveRDS(reg_model_data, "reg_model_data.rds")
cat("saved reg_model_data.rds:", nrow(reg_model_data), "rows\n")
