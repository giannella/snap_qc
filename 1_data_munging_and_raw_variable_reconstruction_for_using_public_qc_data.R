# Constraints: see methods/known_constraints.md#munging
#
# Key variables generated or derived in this script:
#
# correctednotes: Audit label describing the correction applied to a case, such as: "earn_down", "unearn_up", "rent_error", or "no_change".
# correctedamount: Signed dollar change made to the variable being corrected. Positive values mean the variable was increased; negative values mean it was decreased.
# fsben_recreated: Benefit recreated from the original QC inputs before correction adjustments (Compared with FSBEN).
# fsben_uncapped: Benefit calculated from the original QC inputs before minimum and maximum benefit limits are applied.
# rawben_recreated: Benefit recalculated after missing values are filled and correction adjustments are applied. 
# rawben_uncapped: Reconstructed benefit after correction adjustments but before minimum and maximum benefit limits are applied.
# rawbenmax: Maximum benefit amount looked up for the working household size and fiscal year.
# rawstdded: Standard deduction looked up for the working household size and fiscal year, with the applicable state adjustment.
# rawusize: Working household size used during benefit reconstruction. It starts from FSUSIZE and may be adjusted for unit-composition errors.
#
# Key variables from the public QC data:
#
# AMTERR: QC-reported dollar amount of the identified benefit error.
# FSBEN: Corrected benefit amount determined by the QC review/calculation: the amount the household should have received.
# RAWBEN: Original benefit amount issued by the agency.
# absbendiff: Public QC field representing the absolute difference between the original issued benefit and the corrected/reviewed benefit.
# FSGRINC: Final gross countable household income reported in the QC data.
# FSUSIZE: Reported household size in the QC data.
# FSERNDED, FSDEPDED, FSMEDDED, FSCSDED, FSSTDDED: Reported earned-income, dependent-care, medical, child-support, and standard deductions.

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
if (!correct_variables) apply_correction_smoothing <- FALSE
exclude_2020_2021 <- TRUE
exclude_MFIP <- TRUE
exclude_SSI_CAP <- TRUE
cpi_inflate_vars <- TRUE
modeling_target_year <- 2026

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
mydata$state_name <- unname(fips_to_state[as.character(mydata$STATE)])

# Add year and month separately to dataset
mydata$year <- as.integer(substr(mydata$YRMONTH, 1, 4))
mydata$month <- as.integer(substr(mydata$YRMONTH, 5, 6))
mydata$fiscal_year <- ifelse(mydata$month >= 10, mydata$year + 1, mydata$year)

# Add element and nature labels
qc_elements <- read.csv(paste0(folder, "additional_data/qc_elements.csv"))
qc_natures <- read.csv(paste0(folder, "additional_data/qc_natures.csv"))

# Join element and nature descriptions
mydata <- mydata %>%
  left_join(qc_elements %>% rename(element = description), by = c("ELEMENT1" = "code")) %>%
  left_join(qc_natures %>% rename(nature = description), by = c("NATURE1" = "code"))

# Add in external SNAP data
source("features.R")
mydata <- add_external_data(mydata)

# Save data as a checkpoint
saveRDS(mydata, paste0(folder, "all_years.rds"))
mydata <- readRDS(paste0(folder, "all_years.rds"))

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

# Keep 48 states and DC only (related to max allotment)
mydata <- mydata[!mydata$state_name %in% c("Alaska", "Hawaii", "Guam", "Virgin Islands"), ]

# Exclude MFIP and SSI_CAP cases
# Due to non-standard benefits calculations
if (exclude_MFIP)    mydata <- mydata[mydata$MN_FIP  %in% 0, ]
if (exclude_SSI_CAP) mydata <- mydata[mydata$SSI_CAP %in% 0, ]

# If you wanted to be very careful, you could drop all observations with second error elements 
## we lose even more signal from errors for most purposes so commenting out by default
# (to focus on single error rows)
#mydata <- mydata[is.na(mydata$ELEMENT2), ]

# Drop all rows if we can't get the amterr to be close 
# to the difference between fsben and rawben
mydata <- mydata[abs(mydata$absbendiff - mydata$AMTERR) <= 5, ]

# Deduction-field NAs only occur on MFIP and SSI-CAP cases, which are dropped
# above, so this zero-fill currently changes nothing (ded_fields_imputed is
# all FALSE). Revisit if those exclusions are turned off.
ded_na_fields <- c("FSDEPDED", "FSMEDDED", "FSCSDED", "FSSTDDED", "HOMELESS_DED")
mydata$ded_fields_imputed <- rowSums(is.na(mydata[ded_na_fields])) > 0
for (v in ded_na_fields) mydata[[v]][is.na(mydata[[v]])] <- 0
cat("rows with zero-filled deduction fields:", sum(mydata$ded_fields_imputed), "\n")

# Drop only rows where core shelter fields are NA (rarely triggers)
mydata <- mydata %>%
  filter(!is.na(RENT), !is.na(UTIL))

# Track whether a second error element was reported. Multi-element cases are
# KEPT (they are ~30% of error cases); the indicator is for QA/reporting, not
# recommended as a mining feature — states report second elements very
# inconsistently, so it would encode reporting practice, not error risk.
mydata$second_element_i <- !is.na(mydata$ELEMENT2)

# Step 3. Standardize data
mydata <- standardize_data(mydata)

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
mydata$fssltded_recalculated <- pmin(pmax(mydata$fssltded_uncapped, 0), mydata$max_shelter_deduction)
mydata$fssltded_recalculated <- ifelse(mydata$HOMELESS_DED > 0, 0, mydata$fssltded_recalculated)
mydata$fsnet_allow_negative <- floor(mydata$fsnet_before_shelter - (mydata$fssltded_recalculated + mydata$HOMELESS_DED))
mydata$fsben_uncapped <- floor(mydata$BENMAX - (0.3 * mydata$fsnet_allow_negative))
mydata$fsben_recreated <- pmin(pmax(mydata$fsben_uncapped, mydata$fsminimum_ben), mydata$BENMAX)
mydata$unc_fsben_rel_max <- mydata$fsben_uncapped / mydata$BENMAX

# CHECK - should be 100%
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
calculate_raw_benefits <- function(mydata, skip_benefits = FALSE) {
  
  mydata$rawernded <- floor(mydata$rawearn * 0.2)
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
  mydata$rawsltded <- pmin(pmax(mydata$rawsltded_uncapped, 0), mydata$max_shelter_deduction)
  mydata$rawsltded <- ifelse(mydata$rawhomeless_ded > 0, 0, mydata$rawsltded)
  
  if (skip_benefits == FALSE){
    mydata$rawnet_allow_negative <- floor(mydata$rawnet_before_shelter - (mydata$rawsltded + mydata$rawhomeless_ded))
    mydata$rawben_uncapped <- floor(mydata$rawbenmax - (0.3 * mydata$rawnet_allow_negative))
    mydata$rawben_recreated <- pmin(pmax(mydata$rawben_uncapped, mydata$rawminimum_ben), mydata$rawbenmax)
    mydata$rawnet_capped = pmax(mydata$rawnet_allow_negative, 0) 
    mydata$unc_rawben_rel_max <- mydata$rawben_uncapped / mydata$rawbenmax
    mydata$at_max_ben <- as.integer(mydata$rawben_uncapped >= mydata$rawbenmax)
  }
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
max_allotment_key <- paste(max_allotments_long$year, max_allotments_long$hh_size)
stopifnot(!anyDuplicated(max_allotment_key))
get_max_allotment <- function(hh_size, fiscal_year) {
  max_allotments_long$rawbenmax[match(paste(fiscal_year, hh_size), max_allotment_key)]
}

# Read in the standard deductions based on unit size
standard_deductions <- read.csv(paste0(folder, "additional_data/standard_deductions.csv"))
std_ded_size_cols <- grep("^X[0-9]+$", names(standard_deductions), value = TRUE)
standard_deductions_long <- reshape(
  standard_deductions[, c("year", std_ded_size_cols)],
  varying   = std_ded_size_cols,
  v.names   = "rawstdded",
  timevar   = "hh_size",
  times     = as.integer(gsub("X", "", std_ded_size_cols)),
  direction = "long"
)
standard_deductions_long <- standard_deductions_long[, c("year", "hh_size", "rawstdded")]

std_ded_key <- paste(standard_deductions_long$year, standard_deductions_long$hh_size)
stopifnot(!anyDuplicated(std_ded_key))

il_stdded_offsets <- standard_deductions[, c("year", "IL_OFFSET")]
stopifnot(!anyDuplicated(il_stdded_offsets$year))
get_standard_deduction <- function(state_name, hh_size, fiscal_year) {
  result <- standard_deductions_long$rawstdded[match(paste(fiscal_year, hh_size), std_ded_key)]
  fiscal_year <- rep_len(fiscal_year, length(result))
  il <- !is.na(result) & rep_len(state_name, length(result)) %in% "Illinois"
  off <- il_stdded_offsets$IL_OFFSET[match(fiscal_year[il], il_stdded_offsets$year)]
  stopifnot(!anyNA(off))
  result[il] <- result[il] - off
  result
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

# Element 150: excluded eligible person (natures that don't affect expenses) -> subtract 1;
# ineligible person included -> add 1
el150_excluded <- correct_variables & mydata$ELEMENT1 == 150 & mydata$NATURE1 %in% c(12, 14, 16)
el150_included <- correct_variables & mydata$ELEMENT1 == 150 & mydata$NATURE1 %in% c(7)
mydata$rawusize       <- ifelse(el150_excluded, mydata$rawusize - 1, mydata$rawusize)
mydata$correctednotes <- ifelse(el150_excluded, "hhsize_up", mydata$correctednotes)
mydata$rawusize       <- ifelse(el150_included, mydata$rawusize + 1, mydata$rawusize)
mydata$correctednotes <- ifelse(el150_included, "hhsize_down", mydata$correctednotes)

# Unit composition shift for ELEMENT2+, applied before the ELEMENT1 adjustment in Step 7
mydata$hhsize_secondary_shift <- 0
secondary_hh_element <- rep(FALSE, nrow(mydata))
if (correct_variables) {
  for (k in 2:9) {
    el_col <- paste0("ELEMENT", k)
    na_col <- paste0("NATURE", k)
    if (!all(c(el_col, na_col) %in% names(mydata))) next
    excluded_eligible   <- mydata[[el_col]] %in% 150 & mydata[[na_col]] %in% c(12, 14, 16)
    included_ineligible <- mydata[[el_col]] %in% 150 & mydata[[na_col]] %in% c(7)
    mydata$hhsize_secondary_shift <- mydata$hhsize_secondary_shift - excluded_eligible + included_ineligible
    secondary_hh_element <- secondary_hh_element | excluded_eligible | included_ineligible
  }
  mydata$rawusize <- mydata$rawusize + mydata$hhsize_secondary_shift
}

# Paid more than the max for the corrected size (and not last year's max): add a person
over_max <- which(correct_variables & mydata$RAWBEN > mydata$BENMAX & mydata$rawusize == mydata$FSUSIZE & !secondary_hh_element)
prev_max <- get_max_allotment(mydata$FSUSIZE[over_max], mydata$fiscal_year[over_max] - 1)
over_max <- over_max[is.na(prev_max) | mydata$RAWBEN[over_max] != prev_max]
mydata$rawusize[over_max] <- mydata$rawusize[over_max] + 1
mydata$correctednotes[over_max] <- "hhsize_down"

# Drop cases where the rawusize becomes 0
mydata <- mydata[mydata$rawusize > 0, ]

# Update maxben and standard deduction with new household sizes
if (correct_variables) {
  mydata$rawbenmax <- get_max_allotment(mydata$rawusize, mydata$fiscal_year)
  mydata$rawstdded <- get_standard_deduction(mydata$state_name, mydata$rawusize, mydata$fiscal_year)
  mydata$rawminimum_ben <- ifelse(mydata$rawusize < 3, mydata$MINIMUM_BEN, 0)
}

# Utility allowances by state, month and household size (one file per fiscal year)
util_allowances <- list.files(paste0(folder, "additional_data/utilities"), pattern = "\\.csv$", full.names = TRUE) %>%
  lapply(read.csv) %>%
  bind_rows()
util_cols <- c("hcsua", "bua", "phone", "electricity", "water", "sewer", "trash", "gas")

# Utility tiers for a case: $0 plus every allowance in effect for its state, review month and household size
util_tiers <- function(state, year, month, hh_size) {
  ym <- year * 100 + month
  a <- util_allowances[util_allowances$state_name == state &
                         util_allowances$yrmonth_start <= ym & util_allowances$yrmonth_end >= ym &
                         util_allowances$hh_size_min <= hh_size & util_allowances$hh_size_max >= hh_size, util_cols]
  if (nrow(a) == 0) stop("No utility allowances for ", state, " ", ym, " household size ", hh_size)
  a <- unlist(a, use.names = FALSE)
  sort(unique(c(0, a[!is.na(a)])))
}

# Utility element in ELEMENT2+, applied before the ELEMENT1 adjustment in Step 7
# Nature 52 (agency left it out): next step down. Nature 53 (agency included it): top step
# A step must be at least util_min_step away, so near-identical allowances count as the same step
util_min_step <- 50
mydata$util_secondary_shift <- 0
if (correct_variables) {
  util_nature <- rep(NA_real_, nrow(mydata))
  for (k in 2:9) {
    el_col <- paste0("ELEMENT", k)
    na_col <- paste0("NATURE", k)
    if (!all(c(el_col, na_col) %in% names(mydata))) next
    hit <- is.na(util_nature) & mydata[[el_col]] %in% 364 & mydata[[na_col]] %in% c(52, 53)
    util_nature[hit] <- mydata[[na_col]][hit]
  }
  for (i in which(!is.na(util_nature) & !(mydata$ELEMENT1 %in% 364))) {
    tiers <- util_tiers(mydata$state_name[i], mydata$year[i], mydata$month[i], as.numeric(mydata$rawusize[i]))
    u <- as.numeric(mydata$UTIL[i])
    lower <- tiers[tiers <= u - util_min_step]
    new_util <- if (util_nature[i] == 52) {
      if (length(lower) > 0) max(lower) else u
    } else {
      if (max(tiers) - u >= util_min_step) max(tiers) else u
    }
    mydata$util_secondary_shift[i] <- new_util - u
    mydata$rawutil[i] <- new_util
  }
}

# Step 7. Adjust other variables
# This method shifts variables up or down by $1 until the equation
# balances or hit another limiting parameter (going below $0)

income_shift <- 1
max_iterations <- 3000
matching_tolerance <- 0

# Direction for the adjust_* loops (> 0 = raise the benefit), from the reconstructed benefit before any adjustment
start_direction <- function(mydata) {
  mydata <- calculate_raw_benefits(mydata)
  mydata$RAWBEN - mydata$rawben_recreated
}

# Which way NATURE1 says the budgeted value was off (-1 = lower than QC, +1 = higher, 0 = not stated)
nature_direction <- function(nature, income) {
  if (income) ifelse(nature %in% c(33, 35, 37, 38, 39, 46), -1, ifelse(nature %in% c(40, 44, 58), 1, 0))
  else ifelse(nature %in% c(52, 57), -1, ifelse(nature %in% c(53, 56), 1, 0))
}

adjust_income <- function(mydata, col, elements, prefix, max_iter = 11000) {
  
  eligible <- mydata$ELEMENT1 %in% elements
  original <- mydata[[col]]  # save before any changes
  direction <- start_direction(mydata)
  # Don't move the variable against NATURE1
  conflict <- (nature_direction(mydata$NATURE1, income = TRUE) * sign(direction)) %in% 1
  
  for (i in seq_len(max_iter)) {
    mydata <- calculate_raw_benefits(mydata)
    
    diff <- mydata$RAWBEN - mydata$rawben_recreated
    diff_matches <- abs(diff) <= matching_tolerance
    
    done <- ((diff_matches | direction > 0 & mydata[[col]] <= 0) | conflict | 
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
    eligible & (conflict | hit_zero & !diff_matches), paste0(prefix, "_error"),
    ifelse(
      eligible & changed & direction > 0, paste0(prefix, "_down"),
      ifelse(
        eligible & changed & direction < 0, paste0(prefix, "_up"),
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
  direction <- start_direction(mydata)
  # Don't move the variable against NATURE1
  conflict <- (nature_direction(mydata$NATURE1, income = FALSE) * sign(direction)) %in% -1
  
  for (i in seq_len(max_iter)) {
    mydata <- calculate_raw_benefits(mydata)
    
    diff <- mydata$RAWBEN - mydata$rawben_recreated
    diff_matches <- abs(diff) <= matching_tolerance
    
    done <- (diff_matches | (mydata[[col]] <= 0 & direction < 0) | conflict |
               (direction > 0 & mydata$rawben_recreated > mydata$RAWBEN) |
               (direction > 0 & mydata$rawben_recreated >= mydata$rawbenmax) |
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
  changed <- mydata[[col]] != original
  
  mydata$correctednotes <- ifelse(
    eligible & (conflict | (hit_zero | hit_cap) & prefix != "util"), paste0(prefix, "_error"),
    ifelse(
      eligible & changed & direction > 0, paste0(prefix, "_up"),
      ifelse(
        eligible & changed & direction < 0, paste0(prefix, "_down"),
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
  direction <- start_direction(mydata)
  # Don't move the variable against NATURE1
  conflict <- (nature_direction(mydata$NATURE1, income = FALSE) * sign(direction)) %in% -1
  
  for (i in seq_len(max_iter)) {
    mydata <- calculate_raw_benefits(mydata)
    
    diff <- mydata$RAWBEN - mydata$rawben_recreated
    diff_matches <- abs(diff) <= matching_tolerance
    
    done <- (diff_matches | (mydata[[col]] <= 0 & direction < 0) | conflict |
               (direction > 0 & mydata$rawben_recreated > mydata$RAWBEN) |
               (direction > 0 & mydata$rawben_recreated >= mydata$rawbenmax) |
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
    eligible & (conflict | hit_zero | hit_cap), paste0(prefix, "_error"),
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

# Utilities only take allowance steps: try each step in the needed direction and
# keep the one whose recreated benefit is closest to RAWBEN (ties: smallest move)
adjust_util_steps <- function(mydata, col, elements, prefix) {
  
  eligible <- mydata$ELEMENT1 %in% elements
  original <- mydata[[col]]
  mydata <- calculate_raw_benefits(mydata)
  start_gap <- abs(mydata$RAWBEN - mydata$rawben_recreated)
  direction <- mydata$RAWBEN - mydata$rawben_recreated
  # Don't move the variable against NATURE1
  conflict <- (nature_direction(mydata$NATURE1, income = FALSE) * sign(direction)) %in% -1
  notes <- ifelse(direction == 0, paste0(prefix, "_no_change"), paste0(prefix, "_error"))
  
  for (i in which(eligible & !conflict & direction != 0)) {
    tiers <- util_tiers(mydata$state_name[i], mydata$year[i], mydata$month[i], as.numeric(mydata$rawusize[i]))
    cand <- if (direction[i] > 0) tiers[tiers > original[i]] else tiers[tiers < original[i]]
    if (length(cand) == 0) next
    trial <- mydata[rep(i, length(cand)), ]
    trial[[col]] <- cand
    gap <- abs(calculate_raw_benefits(trial)$rawben_recreated - mydata$RAWBEN[i])
    best <- order(gap, abs(cand - original[i]))[1]
    if (gap[best] >= start_gap[i]) next
    mydata[[col]][i] <- cand[best]
    notes[i] <- paste0(prefix, ifelse(cand[best] > original[i], "_up", "_down"))
  }
  
  mydata <- calculate_raw_benefits(mydata)
  mydata$correctednotes <- ifelse(eligible, notes, mydata$correctednotes)
  mydata$correctedamount <- ifelse(eligible, mydata[[col]] - original, mydata$correctedamount)
  mydata
}

# Run an adjust_* function on its eligible rows only
on_eligible_rows <- function(adjust_fn, mydata, col, elements, prefix) {
  rows <- which(mydata$ELEMENT1 %in% elements)
  sub <- adjust_fn(mydata[rows, ], col, elements, prefix)
  mydata[[col]][rows] <- sub[[col]]
  mydata$correctednotes[rows] <- sub$correctednotes
  mydata$correctedamount[rows] <- sub$correctedamount
  mydata
}

fix_smd_adjustments <- function(mydata) {
  mydata <- mydata |>
    mutate(
      rawmedded = case_when(
        rawmedded == FSMEDDED          ~ rawmedded,
        is.na(smd_amt) | smd_amt <= 0  ~ rawmedded,
        abs(smd_amt - rawmedded) <= 5  ~ smd_amt,
        smd_amt <= rawmedded           ~ rawmedded,
        correctednotes == "med_up"     ~ smd_amt,
        correctednotes == "med_down"   ~ case_when(
          smd_amt < FSMEDDED & abs(smd_amt - rawmedded) <= 30 ~ smd_amt, 
          .default = 0
        ),
        .default = rawmedded
      )
    )
  
  mydata <- calculate_raw_benefits(mydata)
  
  mydata
}

if (correct_variables){
  
  # Homeless cases with a rent/utility error: assume no homeless deduction
  hl <- mydata$rawhomeless_ded > 0 & mydata$ELEMENT1 %in% c(363, 364)
  mydata$rawhomeless_ded[hl] <- 0
  
  mydata <- on_eligible_rows(adjust_income, mydata, 
                          col = "rawunearn", 
                          elements = c(331, 332, 333, 334, 335, 336, 342, 343, 344, 345, 346, 350), 
                          prefix = "unearn")
  mydata <- on_eligible_rows(adjust_income, mydata, 
                          col = "rawearn", 
                          elements = c(311, 312, 314), 
                          prefix = "earn")
  mydata <- on_eligible_rows(adjust_shelter, mydata, 
                           col = "rawrent", 
                           elements = c(363), 
                           prefix = "rent")
  mydata <- on_eligible_rows(adjust_util_steps, mydata, 
                           col = "rawutil", 
                           elements = c(364), 
                           prefix = "util")
  mydata <- on_eligible_rows(adjust_other, mydata, 
                         col = "rawmedded", 
                         elements = c(365), 
                         prefix = "med")
  mydata <- fix_smd_adjustments(mydata)
  mydata <- on_eligible_rows(adjust_other, mydata, 
                         col = "rawdepded", 
                         elements = c(323), 
                         prefix = "dep")
  mydata <- on_eligible_rows(adjust_other, mydata, 
                         col = "rawcsded", 
                         elements = c(366), 
                         prefix = "cs")
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

# Save data as a checkpoint
saveRDS(mydata, paste0(folder, "corrected.rds"))
mydata <- readRDS(paste0(folder, "corrected.rds"))

### Variable smoothing
mydata <- calculate_raw_benefits(mydata)

# If a value was cut by >80% and is now <=$5, set it to $0
if (apply_correction_smoothing) {
  raw_map <- c(
    rawunearn = "FSUNEARN",
    rawearn   = "FSEARN",
    rawrent   = "RENT",
    rawmedded = "FSMEDDED",
    rawdepded = "FSDEPDED",
    rawcsded  = "FSCSDED"
  )
  
  for (raw in names(raw_map)) {
    orig <- mydata[[raw_map[[raw]]]]
    cur  <- mydata[[raw]]
    mydata[[raw]] <- ifelse(
      !is.na(orig) & !is.na(cur) & orig > 0 & cur > 0 &
        cur <= 5 & cur < 0.2 * orig,
      0,
      cur
    )
  }
  mydata <- calculate_raw_benefits(mydata)
}

# If we reduced earned income down by over 75%, and at max, set it to $0
if (apply_correction_smoothing) {
  mydata$rawearn <- ifelse(
    mydata$correctednotes == "earn_down" &
      mydata$at_max_ben &
      abs(mydata$correctedamount) > 0.75 * mydata$FSEARN,
    0,
    mydata$rawearn
  )
  mydata <- calculate_raw_benefits(mydata)
}

# If we reduced unearned income down by over 75%, and at max, set it to $0
if (apply_correction_smoothing) {
  mydata$rawunearn <- ifelse(
    mydata$correctednotes == "unearn_down" &
      mydata$at_max_ben &
      abs(mydata$correctedamount) > 0.75 * mydata$FSUNEARN,
    0,
    mydata$rawunearn
  )
  mydata <- calculate_raw_benefits(mydata)
}

# Scale deduction variables to match mean values for other households
# that are at max or above (e.g. make the data equally represent)
scale_by_ratio <- function(data, notes_value, col_name) {
  
  # Direction the adjust_* loop already moved this variable
  suffix <- sub("^.*_", "", notes_value)
  
  data$at_max_ben <- as.integer(data$rawben_uncapped >= data$rawbenmax)
  
  # Calculate comparison and ratios
  comparison <- data %>%
    filter(correctednotes %in% c(notes_value, "no_change"),
           at_max_ben == 1,
           .data[[col_name]] != 0) %>%
    group_by(rawusize, correctednotes) %>%
    summarise(mean_val = mean(.data[[col_name]], na.rm = TRUE),
              n = n(),
              .groups = "drop") %>%
    arrange(rawusize, correctednotes)
  
  ratios <- comparison %>%
    select(rawusize, correctednotes, mean_val) %>%
    tidyr::pivot_wider(names_from = correctednotes, values_from = mean_val) %>%
    # so smoothing can only move in the direction already applied
    mutate(ratio = no_change / .data[[notes_value]],
           ratio = if (suffix == "up") pmax(ratio, 1) else pmin(ratio, 1))
  
  print(ratios)
  
  # Apply scaling
  target_rows <- !is.na(data$correctednotes) & 
    data$correctednotes == notes_value & 
    !is.na(data$at_max_ben) &
    data$at_max_ben == 1
  
  data[[col_name]][target_rows] <- sapply(which(target_rows), function(i) {
    size <- data$rawusize[i]
    ratio <- ratios$ratio[ratios$rawusize == size]
    if (length(ratio) == 0 || is.na(ratio)) ratio <- 1
    round(data[[col_name]][i] * ratio)
  })
  
  data
}

# Scale variables
if (apply_correction_smoothing) {
  mydata <- scale_by_ratio(mydata, "dep_up", "rawdepded")
  mydata <- calculate_raw_benefits(mydata)
  mydata <- scale_by_ratio(mydata, "cs_up", "rawcsded")
  mydata <- calculate_raw_benefits(mydata)
  mydata <- scale_by_ratio(mydata, "rent_up", "rawrent")
  mydata <- calculate_raw_benefits(mydata)
  mydata <- scale_by_ratio(mydata, "earn_down", "rawearn")
  mydata <- calculate_raw_benefits(mydata)
  mydata <- scale_by_ratio(mydata, "unearn_down", "rawunearn")
  mydata <- calculate_raw_benefits(mydata)
}

add_total_deductions <- function(data) {
  data$raw_total_deductions <- rowSums(data[c("rawdepded", "rawcsded", "rawsltded", "rawmedded",
                                              "rawhomeless_ded", "rawernded", "rawstdded")], na.rm = TRUE)
  data
}
mydata <- add_total_deductions(mydata)

# Fold secondary household size shifts into correctednotes, e.g. "hhsize_up+earn_down"
sec_note <- ifelse(mydata$hhsize_secondary_shift < 0, "hhsize_up",
                   ifelse(mydata$hhsize_secondary_shift > 0, "hhsize_down", NA))
util_note <- ifelse(mydata$util_secondary_shift < 0, "util_down",
                    ifelse(mydata$util_secondary_shift > 0, "util_up", NA))
sec_note <- ifelse(is.na(sec_note), util_note, ifelse(is.na(util_note), sec_note, paste0(sec_note, "+", util_note)))
has_sec <- !is.na(sec_note)
mydata$correctednotes[has_sec] <- ifelse(
  mydata$correctednotes[has_sec] == "no_change",
  sec_note[has_sec],
  paste0(sec_note[has_sec], "+", mydata$correctednotes[has_sec])
)

# Nature 97 / agency 26 (change not required to be reported or acted upon): the budgeted benefit was correct, so the FS side is set to the reconstructed raw side and the variance fields are cleared (flagged in no_action_required_i)
no_action_required <- mydata$NATURE1 %in% 97 | mydata$AGENCY1 %in% 26
fs_map <- c(FSUSIZE = "rawusize", FSGRINC = "rawgrinc", FSEARN = "rawearn", FSUNEARN = "rawunearn", FSERNDED = "rawernded", FSMEDDED = "rawmedded", FSDEPDED = "rawdepded", FSCSDED = "rawcsded", FSSLTDED = "rawsltded", FSSLTEXP = "rawsltexp", RENT = "rawrent", UTIL = "rawutil", FSSTDDED = "rawstdded", HOMELESS_DED = "rawhomeless_ded", BENMAX = "rawbenmax",
            fsminimum_ben = "rawminimum_ben", fsnet_before_shelter = "rawnet_before_shelter", fsnet_adjusted_half = "rawnet_adjusted_half", fssltded_uncapped = "rawsltded_uncapped", fssltded_recalculated = "rawsltded", fsnet_allow_negative = "rawnet_allow_negative", FSNETINC = "rawnet_capped", fsben_uncapped = "rawben_uncapped", fsben_recreated = "rawben_recreated", unc_fsben_rel_max = "unc_rawben_rel_max", FSBEN = "rawben_recreated")
for (v in names(fs_map)) mydata[[v]][no_action_required] <- as.numeric(mydata[[fs_map[[v]]]][no_action_required])
for (v in c("AMTERR", "bendiff", "absbendiff", "over_threshold")) mydata[[v]][no_action_required] <- 0
mydata$STATUS[no_action_required] <- 1
mydata$no_action_required_i <- no_action_required
for (v in grep("^(ELEMENT|NATURE|AGENCY|AMOUNT|DISCOV|E_FINDG|OCCDATE|TIMEPER|VERIF)[1-9]$", names(mydata), value = TRUE)) mydata[[v]][no_action_required] <- NA
mydata[no_action_required, c("element", "nature")] <- NA
mydata$second_element_i[no_action_required] <- FALSE

# Save data
saveRDS(mydata, paste0(folder, "final.rds"))
mydata <- readRDS(paste0(folder, "final.rds"))

#### Add additional features from features.R
df <- add_features(mydata)

#### CPI-inflate data from features.R
if (cpi_inflate_vars) {
  
  # Reset max_sua and smd_amt amount to modeling_target_year
  df$max_sua <- NULL
  df <- add_state_year_col(df, state_sua, "max_sua", year = modeling_target_year)
  df$smd_amt <- NULL
  df <- add_state_year_col(df, smd_by_year, "smd_amt", year = modeling_target_year)
  
  # Inflation adjust variables
  cpi_vars <- c("rawearn", "rawunearn", "rawmedded", "rawdepded", "rawcsded", "rawrent", "rawutil")
  # the review-year amounts, kept for the state workbook's input block: a
  # state pastes amounts in its own review year's dollars and the workbook
  # repeats this inflation step itself
  for (v in c(cpi_vars, "rawhomeless_ded")) df[[paste0(v, "_nominal")]] <- df[[v]]
  # keyed on the review's fiscal year, so the workbook can repeat the step
  # from REVIEW_FISCAL_YEAR alone
  df <- cpi_inflate(df, cpi_vars, modeling_target_year, year_col = "fiscal_year",
                    overwrite = TRUE)
  df$rawhomeless_ded[!is.na(df$rawhomeless_ded) & df$rawhomeless_ded > 0] <- year_data$homeless_standard[year_data$year == modeling_target_year]
  df$rawstdded <- get_standard_deduction(df$state_name, df$rawusize, modeling_target_year)
  
  # Additional calculations needed post-inflation
  df$max_shelter_deduction <- year_data$max_shelter_deduction[year_data$year == modeling_target_year]
  df$max_shelter_deduction <- ifelse(df$FSNELDER + df$FSNDIS > 0, Inf, df$max_shelter_deduction)
  
  # Benefit chain left nominal (project lead, 2026-09-25: the mined ratios
  # rawben_rel_max and unc_rawben_rel_max are NOT CPI-adjusted): deductions
  # are recomputed on the inflated inputs, the benefit amounts and max
  # allotments are not
  skip_benefits <- TRUE
  if (skip_benefits == FALSE){
    df$rawbenmax <- get_max_allotment(df$rawusize, modeling_target_year)
    df$rawminimum_ben <- year_data$min_allotment[year_data$year == modeling_target_year]
    df$rawminimum_ben <- ifelse(df$rawusize < 3, df$rawminimum_ben, 0)
  }
  df <- calculate_raw_benefits(df, skip_benefits = skip_benefits)
  df <- add_total_deductions(df)
}

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
         # share of the RECONSTRUCTED unit (rawusize, the size every other
         # per-member feature divides by; 2026-09-25, was certhhsz, the
         # reported size, which differs on 0.6% of cases after the
         # unit-composition correction)
         pct_abawd = count_abawd / rawusize)
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
         total_error_amount = absbendiff, #"Amount of benefit in error"
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

# State-level regime flag replacing the raw cat_elig as a mining feature
# (decided 2026-08-13). bbce_state_i = the state runs Broad-Based
# Categorical Eligibility (BBCE) in that fiscal year, derived as the
# state-year share of cases with cat_elig >= 1 reaching 0.5 - the share
# distribution is bimodal (non-BBCE states ~0.1-0.25, BBCE states ~1.0)
# with no state flipping across 2022-24. The raw 4-level cat_elig stays
# in the frame for diagnostics but must NOT be a mining feature: the
# FY2024 codebook recode moved the mass of eligible units from code 1 to
# code 2, so any 1-vs-2 split encodes the reporting era, and code 3
# exists only from FY2024.
reg_model_data <- reg_model_data %>%
  group_by(state_name, fiscal_year) %>%
  mutate(bbce_state_i = mean(cat_elig >= 1, na.rm = TRUE) >= 0.5) %>%
  ungroup()


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
