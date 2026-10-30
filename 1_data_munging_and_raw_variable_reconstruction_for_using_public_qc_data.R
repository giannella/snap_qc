# Constraints: see methods/known_constraints.md#munging
#
# Key variables generated or derived in this script:
#
# correctednotes: Audit label describing the correction applied to a case, such as: "earn_down", "unearn_up", "rent_error", or "no_change". Cases with several Step 7 variables list each one moved (e.g. "util_down+med_up"), with "+cascade_error" when RAWBEN is not reached; an ELEMENT1 household size or homeless note stays in front (e.g. "hhsize_up+earn_down"). "prior_year_tables": ELEMENT1 520 / 361 matched with last year's maximum allotment, standard deduction or shelter cap.
# correctedamount: Signed dollar change made to the variable being corrected. Positive values mean the variable was increased; negative values mean it was decreased.
# fsben_recreated: Benefit recreated from the original QC inputs before correction adjustments (Compared with FSBEN).
# fsben_uncapped: Benefit calculated from the original QC inputs before minimum and maximum benefit limits are applied.
# rawben_recreated: Benefit recalculated after missing values are filled and correction adjustments are applied. 
# rawben_uncapped: Reconstructed benefit after correction adjustments but before minimum and maximum benefit limits are applied.
# rawbenmax: Maximum benefit amount looked up for the working household size and fiscal year.
# rawstdded: Standard deduction looked up for the working household size and fiscal year, with the applicable state adjustment.
# rawusize: Working household size used during benefit reconstruction. It starts from FSUSIZE and may be adjusted for unit-composition errors.
# amount_pattern: How a multi-element case's AMOUNT1-9 relate to its error: all_zero, amount1_only, copied, even_split, clean_split, or other.
# cascade_type, cascade_hit_limit: Cases with several Step 7 variables; "capped" when each element moved by its own AMOUNT, otherwise "uncapped"; hit_limit when a continuous variable was maxed out before the next took over.
# imputed_at_bound: "max" / "min" when impute_at_bound redrew a value for a case stuck on that benefit limit (QA flag, not a modeling feature).
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

# Household size elements in any element slot: 150 (unit composition) and the
# person-eligibility elements 111 student, 130 citizenship, 151 disqualification,
# 160-166 work rules, 170 SSN. The nature gives the direction:
#   eligible person excluded (150: natures 12, 13, 14, 15, 16; others: nature 6) -> subtract 1
#   (150 nature 13 and person-element nature 6 in ELEMENT1: the excluded member's income is also looked for at
#   the end of Step 6)
#   ineligible person included (nature 7)                               -> add 1
# One person per case however many slots carry one (the same person is often coded
# under 150 and a person element); cases whose slots disagree on direction are left alone
hh_elements <- c(111, 130, 150, 151, 160:166, 170)
hh_direction <- function(element, nature) {
  ifelse(element %in% 150 & nature %in% c(12, 13, 14, 15, 16), -1,
         ifelse(element %in% setdiff(hh_elements, 150) & nature %in% 6, -1,
                ifelse(element %in% hh_elements & nature %in% 7, 1, 0)))
}
hh_down <- hh_up <- rep(FALSE, nrow(mydata))
for (k in 1:9) {
  el_col <- paste0("ELEMENT", k)
  na_col <- paste0("NATURE", k)
  if (!all(c(el_col, na_col) %in% names(mydata))) next
  dir_k <- hh_direction(mydata[[el_col]], mydata[[na_col]])
  hh_down <- hh_down | dir_k < 0
  hh_up   <- hh_up | dir_k > 0
}
hh_shift <- if (correct_variables) (hh_up & !hh_down) - (hh_down & !hh_up) else rep(0, nrow(mydata))
mydata$rawusize <- mydata$rawusize + hh_shift

# ELEMENT1 hits are noted here; ELEMENT2+ hits are folded into correctednotes after Step 7
hh_first <- hh_shift != 0 & hh_direction(mydata$ELEMENT1, mydata$NATURE1) == hh_shift
mydata$correctednotes[hh_first & hh_shift < 0] <- "hhsize_up"
mydata$correctednotes[hh_first & hh_shift > 0] <- "hhsize_down"
mydata$hhsize_secondary_shift <- ifelse(hh_first, 0, hh_shift)
# Marked as a column (not a vector) so it stays aligned when size-0 rows are dropped below
mydata$two_person_check <- correct_variables & hh_first & hh_shift == -1
mydata$excluded_member_check <- correct_variables & hh_first & hh_shift == -1 &
  ((mydata$ELEMENT1 %in% 150 & mydata$NATURE1 %in% 13) | (mydata$ELEMENT1 %in% setdiff(hh_elements, 150) & mydata$NATURE1 %in% 6))
# Any household size element with a direction keeps the over-max rule below from adding a person
hh_element_any <- correct_variables & (hh_up | hh_down)

# Paid more than the max for the corrected size (and not last year's max): add a person
over_max <- which(correct_variables & mydata$RAWBEN > mydata$BENMAX & mydata$rawusize == mydata$FSUSIZE & !hh_element_any)
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

# Utility errors in any slot are corrected in Step 7 (recreate_elements). A step between allowances must be
# at least util_min_step, so near-identical allowances count as the same step
util_min_step <- 50
mydata$util_secondary_shift <- 0

# ELEMENT1 an eligible person excluded who may have had income (150 nature 13 "with income"; person elements
# nature 6, which does not say): the size shift above removed the person; the agency would also have left out
# that person's income. 150 natures 12, 14, 15 and 16 (no income, resources, expenses, newborn) are not
# searched. The person-level
# income fields add up to FSEARN (earned) and FSUNEARN (unearned), so try each eligible member
# (FSAFIL = 1) with income: raw income = FSEARN / FSUNEARN minus that member's income, kept
# only if the benefit then matches RAWBEN exactly. If no member matches, or members with
# different incomes both match, income is left as is (the person is still removed).
# Utilities are corrected later (Step 7), so when no member matches and ELEMENT2-9 holds a utility
# error with nature 52 or 53, the search is retried with the utility set by its nature (52: the next
# allowance at least util_min_step down; 53: the top allowance); a match keeps that utility too
earned_types   <- c("WAGES", "SLFEMP", "OTHERN", "RENTMINC")
unearned_types <- c("ALIMNY", "ANNUITY", "CONT", "CSUPRT", "DEEM", "DIVER", "EDLOAN", "ENERGY",
                    "FOSTER", "GA", "GOVDIV", "GOVINTER", "GOVROY", "INTER", "OLDAGE", "OTHGOV",
                    "OTHUN", "PENSION", "RENTINC", "SOCSEC", "SSI", "STRIKE", "SURV", "TANF",
                    "TRUST", "UNEMP", "VET", "WCOMP", "UNK", "WGESUP")
person_income <- function(data, types, k) {
  cols <- intersect(paste0(types, k), names(data))
  Reduce(`+`, lapply(cols, function(v) { x <- as.numeric(data[[v]]); x[is.na(x)] <- 0; x }), rep(0, nrow(data)))
}
# Member whose income, taken out of FSEARN / FSUNEARN, makes the benefit match RAWBEN exactly (utilities at `util`)
find_member <- function(sub, fs_earn, fs_unearn, util) {
  match_earn <- match_unearn <- rep(NA_real_, nrow(sub))
  ambiguous <- rep(FALSE, nrow(sub))
  for (k in 1:18) {
    afil <- paste0("FSAFIL", k)
    if (!afil %in% names(sub)) next
    e <- person_income(sub, earned_types, k)
    u <- person_income(sub, unearned_types, k)
    cand <- as.numeric(sub[[afil]]) %in% 1 & (e + u) > 0
    if (!any(cand)) next
    trial <- sub
    trial$rawearn   <- fs_earn - e
    trial$rawunearn <- fs_unearn - u
    trial$rawutil   <- util
    hit <- cand & (calculate_raw_benefits(trial)$rawben_recreated == sub$RAWBEN) %in% TRUE
    ambiguous <- ambiguous | (hit & !is.na(match_earn) & (match_earn != e | match_unearn != u))
    first <- hit & is.na(match_earn)
    match_earn[first]   <- e[first]
    match_unearn[first] <- u[first]
  }
  list(earn = match_earn, unearn = match_unearn, ambiguous = ambiguous,
       use = !is.na(match_earn) & !ambiguous)
}
el150_income <- which(mydata$excluded_member_check)
mydata$excluded_member_check <- NULL
# Person elements (nature 6) do not say the person had income: search only when the size change alone misses RAWBEN
# (otherwise, at the maximum benefit, removing any income still "matches")
if (length(el150_income) > 0) {
  size_only <- calculate_raw_benefits(mydata[el150_income, ])$rawben_recreated == mydata$RAWBEN[el150_income]
  el150_income <- el150_income[!(size_only %in% TRUE & !(mydata$ELEMENT1[el150_income] %in% 150))]
}
if (length(el150_income) > 0) {
  sub <- mydata[el150_income, ]
  fs_earn   <- ifelse(is.na(sub$FSEARN), 0, as.numeric(sub$FSEARN))
  fs_unearn <- ifelse(is.na(sub$FSUNEARN), 0, as.numeric(sub$FSUNEARN))
  m <- find_member(sub, fs_earn, fs_unearn, as.numeric(sub$rawutil))
  
  # Retry with a utility error in ELEMENT2-9 (nature 52 / 53) set by its nature
  util_nature <- rep(NA_real_, nrow(sub))
  for (k in 2:9) {
    el_col <- paste0("ELEMENT", k)
    na_col <- paste0("NATURE", k)
    if (!all(c(el_col, na_col) %in% names(sub))) next
    hit <- is.na(util_nature) & sub[[el_col]] %in% 364 & sub[[na_col]] %in% c(52, 53)
    util_nature[hit] <- as.numeric(sub[[na_col]][hit])
  }
  util_alt <- as.numeric(sub$rawutil)
  for (r in which(!m$use & !is.na(util_nature))) {
    tiers <- tryCatch(util_tiers(sub$state_name[r], sub$year[r], sub$month[r], as.numeric(sub$rawusize[r])),
                      error = function(e) numeric(0))
    lower <- tiers[tiers <= util_alt[r] - util_min_step]
    if (util_nature[r] == 52 && length(lower) > 0) util_alt[r] <- max(lower)
    if (util_nature[r] == 53 && length(tiers) > 0 && max(tiers) - util_alt[r] >= util_min_step) util_alt[r] <- max(tiers)
  }
  retry <- !m$use & util_alt != as.numeric(sub$rawutil)
  util_match <- rep(FALSE, nrow(sub))
  if (any(retry)) {
    m2 <- find_member(sub, fs_earn, fs_unearn, util_alt)
    util_match <- retry & m2$use
    m$earn[util_match]   <- m2$earn[util_match]
    m$unearn[util_match] <- m2$unearn[util_match]
    m$use[util_match]    <- TRUE
  }
  
  use  <- m$use
  rows <- el150_income[use]
  mydata$rawearn[rows]   <- fs_earn[use] - m$earn[use]
  mydata$rawunearn[rows] <- fs_unearn[use] - m$unearn[use]
  mydata$correctednotes[rows]  <- paste0(mydata$correctednotes[rows], "+income_down")
  mydata$correctedamount[rows] <- -(m$earn[use] + m$unearn[use])
  # The utility that made the retry match is kept (folded into the note after Step 8, e.g. "util_up+hhsize_up+income_down")
  util_rows <- el150_income[util_match]
  mydata$util_secondary_shift[util_rows] <- util_alt[util_match] - as.numeric(mydata$rawutil[util_rows])
  mydata$rawutil[util_rows] <- util_alt[util_match]
  cat("excluded member: income removed on", length(rows), "of", length(el150_income),
      "cases (", sum(util_match), "after the utility retry,", sum(m$ambiguous), "ambiguous )\n")
}

# Step 7. Adjust the cited variables (recreate_elements, below)
# Discrete variables first (household size and the homeless deduction above, then utilities), then the
# continuous ones (income, rent, medical, dependent care, child support) close the rest of the gap to RAWBEN.
# Medical in standard medical deduction (SMD) states only takes $0, the SMD or more.

# Which way NATURE says the budgeted value was off (-1 = lower than QC, +1 = higher, 0 = not stated)
nature_direction <- function(nature, income) {
  if (income) ifelse(nature %in% c(33, 35, 37, 38, 39, 46), -1, ifelse(nature %in% c(40, 44, 58), 1, 0))
  else ifelse(nature %in% c(52, 57), -1, ifelse(nature %in% c(53, 56), 1, 0))
}

# Which way E_FINDG (2 = overissuance, 3 = underissuance) says the budgeted value was off, on the
# nature_direction scale: overissuance means income was budgeted too low or a deduction too high
efindg_direction <- function(efindg, income) {
  over  <- efindg %in% 2
  under <- efindg %in% 3
  if (income) ifelse(over, -1, ifelse(under, 1, 0))
  else ifelse(over, 1, ifelse(under, -1, 0))
}

# Direction check: a move (need: -1 down, +1 up) is blocked only when no recorded code supports it. If E_FINDG and
# NATURE disagree, the one that points the needed way (the way QC's own benefit result points) lets it through.
# nature_first (ELEMENT2-9 of multi-element cases whose AMOUNTs do not add up, where states usually copy the case's
# E_FINDG into every slot): NATURE alone decides, E_FINDG only when NATURE is blank
direction_blocked <- function(efindg, nature, income, need, nature_first = FALSE) {
  ef    <- efindg_direction(efindg, income)
  nat   <- nature_direction(nature, income)
  codes <- if (nature_first) (if (nat != 0) nat else ef) else c(ef, nat)
  codes <- codes[codes != 0]
  length(codes) > 0 && all(codes != need)
}

# Step 7 elements: every slot whose element Step 7 adjusts. Nature 97 / agency 26 elements are recreated exactly
# like any other; the no-action step after Step 8 then takes their effect out of the error.
# Continuous variables are adjusted only when ELEMENT1 is a Step 7 element or a household size / homeless error
# corrected above; other ELEMENT1s (standard deduction, arithmetic computation, ...) may account for the gap
# themselves.
# AMOUNTi is the benefit change QC attributes to element i and E_FINDGi its direction; many states leave
# AMOUNTi at $0 or repeat AMTERR in every slot.
#   capped (clean_split AMOUNTs, every element a Step 7 one, no household size or homeless shift): utilities,
#     then largest AMOUNT, each element moving the benefit by its own AMOUNT; the last one targets RAWBEN.
#   otherwise: utilities first (alone: the allowance closest to RAWBEN; with other elements: the closest one at
#     least util_min_step away that does not pass RAWBEN; ELEMENT1 not recreated: util_nature_step), then the
#     continuous variables, largest AMOUNT first (ties in slot order), each moving toward RAWBEN as far as it
#     can before the next takes the rest (medical in SMD states: $0, the SMD or more).
#   Leftovers: a cut that leaves zero_floor or less, or cuts zero_cut or more, goes to $0 instead when $0 lands
#     within zero_margin of RAWBEN (later variables then stay put), or when a later variable takes the rest
#     and the case then lands within zero_margin. A $5 benefit change is about $17-21 of a variable, so a leftover that small only absorbs
#     rounding. Not medical in SMD states (the SMD is a real amount).
#   An element whose recorded codes all point the other way is not moved (see direction_blocked).
amount_tolerance <- 2
zero_cut    <- 0.9
zero_floor  <- 25
zero_margin <- 5
step7_vars <- c(setNames(rep("rawearn", 3), c(311, 312, 314)),
                setNames(rep("rawunearn", 12), c(331:336, 342:346, 350)),
                c("363" = "rawrent", "364" = "rawutil", "365" = "rawmedded", "323" = "rawdepded", "366" = "rawcsded"))
step7_prefix <- c(rawearn = "earn", rawunearn = "unearn", rawrent = "rent", rawutil = "util",
                  rawmedded = "med", rawdepded = "dep", rawcsded = "cs")
step7_elements <- as.numeric(names(step7_vars))

# One column per element slot (1-9) as a numeric matrix
slot_matrix <- function(mydata, prefix) {
  matrix(unlist(lapply(1:9, function(k) {
    v <- paste0(prefix, k)
    if (v %in% names(mydata)) as.numeric(mydata[[v]]) else rep(NA_real_, nrow(mydata))
  })), ncol = 9)
}

# How a case's AMOUNTs relate to its error (NA for cases with fewer than two elements):
# all_zero, amount1_only, copied (AMTERR in every slot), even_split, clean_split, other
amount_pattern <- function(mydata) {
  el  <- slot_matrix(mydata, "ELEMENT")
  amt <- slot_matrix(mydata, "AMOUNT")
  fnd <- slot_matrix(mydata, "E_FINDG")
  present <- !is.na(el)
  amt[!present | is.na(amt)] <- 0
  sgn <- (present & fnd %in% 2) - (present & fnd %in% 3)
  nel   <- rowSums(present)
  n_pos <- rowSums(amt > 0)
  amax  <- apply(ifelse(present, amt, -Inf), 1, max)
  amin  <- apply(ifelse(present, amt, Inf), 1, min)
  err   <- as.numeric(mydata$AMTERR)
  net   <- as.numeric(mydata$RAWBEN) - as.numeric(mydata$FSBEN)
  known  <- n_pos == nel & rowSums(sgn != 0) == nel
  copied <- n_pos == nel & amax == amin & abs(amax - err) <= 1
  even   <- n_pos == nel & !copied & amax - amin <= 1 & abs(rowSums(amt) - err) <= nel
  clean  <- known & !copied & !even & abs(rowSums(amt * sgn) - net) <= amount_tolerance
  ifelse(nel < 2, NA_character_,
  ifelse(n_pos == 0, "all_zero",
  ifelse(n_pos == 1 & amt[, 1] > 0, "amount1_only",
  ifelse(copied, "copied",
  ifelse(even, "even_split",
  ifelse(clean, "clean_split", "other"))))))
}

benefit_inputs <- c("rawearn", "rawunearn", "rawdepded", "rawmedded", "rawcsded", "rawstdded", "rawrent",
                    "rawutil", "max_shelter_deduction", "rawhomeless_ded", "rawbenmax", "rawminimum_ben")

# Recreated benefit for case i of `bi` (a list of the benefit inputs) with `col` set to each of `values`
benefit_at <- function(bi, i, col, values) {
  trial <- lapply(bi, function(x) rep(x[i], length(values)))
  trial[[col]] <- values
  calculate_raw_benefits(trial)$rawben_recreated
}

# Values to try: the current value, $0 and the case's allowances for utilities; medical in SMD states: the
# current value, $0, the SMD and $1 steps above it; otherwise the current value and $1 steps from $0 to $11,000
# (income) or $3,000 (other) above it
candidate_values <- function(mydata, bi, i, col) {
  cur <- bi[[col]][i]
  if (col == "rawutil") {
    tiers <- tryCatch(util_tiers(mydata$state_name[i], mydata$year[i], mydata$month[i], as.numeric(mydata$rawusize[i])),
                      error = function(e) numeric(0))
    return(sort(unique(c(cur, tiers))))
  }
  # Standard medical deduction (SMD) states: $0, the SMD, or an actual deduction above it
  smd <- if (col == "rawmedded") as.numeric(mydata$smd_amt[i]) else NA
  if (!is.na(smd) && smd > 0) return(sort(unique(c(cur, 0, seq(smd, max(cur, smd) + 3000)))))
  sort(unique(c(cur, seq(0, max(cur, 0) + if (col %in% c("rawearn", "rawunearn")) 11000 else 3000))))
}

# Value of `col` that brings the benefit closest to `target` (ties: smallest move); stays put unless it gets
# closer. min_step: skip values closer than this to the current one. no_overshoot: skip values past `target`
move_toward <- function(mydata, bi, i, col, target, min_step = 0, no_overshoot = FALSE) {
  cur  <- bi[[col]][i]
  cand <- candidate_values(mydata, bi, i, col)
  cand <- cand[cand == cur | abs(cand - cur) >= min_step]
  ben  <- benefit_at(bi, i, col, cand)
  keep <- which(cand == cur)[1]
  gap  <- abs(ben - target)
  if (no_overshoot) gap[(target - ben) * (target - ben[keep]) < 0] <- Inf
  best <- order(gap, abs(cand - cur))[1]
  if (gap[best] < gap[keep]) list(value = cand[best], benefit = ben[best]) else list(value = cur, benefit = ben[keep])
}

# Utility whose gap may not be its own (ELEMENT1 not recreated, or a later slot of a no-action case): the nature
# alone sets it. Nature 52 (agency left it out): the next allowance at least util_min_step down; nature 53
# (agency included it): the top allowance, if at least util_min_step up
util_nature_step <- function(mydata, bi, i, nature) {
  cur   <- bi$rawutil[i]
  tiers <- candidate_values(mydata, bi, i, "rawutil")
  lower <- tiers[tiers <= cur - util_min_step]
  if (nature %in% 52 && length(lower) > 0) return(max(lower))
  if (nature %in% 53 && max(tiers) - cur >= util_min_step) return(max(tiers))
  cur
}

# Louisiana's SMD demonstration began April 2023 (FY 2023 QC Technical Documentation, Table F.4), but the
# state-year lookup gives it the SMD for all of FY 2023
mydata$smd_amt[mydata$state_name %in% "Louisiana" & mydata$fiscal_year == 2023 & as.numeric(mydata$YRMONTH) < 202304] <- 0

recreate_elements <- function(mydata) {
  bi  <- lapply(mydata[benefit_inputs], as.numeric)
  el  <- slot_matrix(mydata, "ELEMENT")
  na  <- slot_matrix(mydata, "NATURE")
  ag  <- slot_matrix(mydata, "AGENCY")
  amt <- slot_matrix(mydata, "AMOUNT")
  fnd <- slot_matrix(mydata, "E_FINDG")
  amt[is.na(amt)] <- 0
  present <- !is.na(el)
  usable  <- present & el %in% step7_elements
  recreatable <- mydata$ELEMENT1 %in% step7_elements |
    (mydata$ELEMENT1 %in% hh_elements & mydata$rawusize != as.numeric(mydata$FSUSIZE)) %in% TRUE |
    (mydata$ELEMENT1 %in% 362 & grepl("^homeless_", mydata$correctednotes))
  capped <- (mydata$amount_pattern %in% "clean_split" & rowSums(usable) == rowSums(present) &
               mydata$rawusize == as.numeric(mydata$FSUSIZE) & mydata$homeless_secondary_shift == 0) %in% TRUE
  nature_first <- !(mydata$amount_pattern %in% "clean_split")
  smd_state <- !is.na(as.numeric(mydata$smd_amt)) & as.numeric(mydata$smd_amt) > 0
  notes  <- mydata$correctednotes
  amount <- mydata$correctedamount
  util_shift <- mydata$util_secondary_shift
  ctype  <- mydata$cascade_type
  climit <- mydata$cascade_hit_limit
  
  for (i in which(rowSums(usable) > 0)) {
    slots <- which(usable[i, ])
    slots <- slots[order(!(el[i, slots] %in% 364), -amt[i, slots], slots)]
    cols  <- unname(step7_vars[as.character(el[i, slots])])
    if (!recreatable[i] && !("rawutil" %in% cols)) next
    rawben <- as.numeric(mydata$RAWBEN[i])
    orig   <- vapply(unique(cols), function(v) bi[[v]][i], numeric(1))
    now    <- benefit_at(bi, i, "rawutil", bi$rawutil[i])
    single <- length(orig) == 1
    conflict  <- FALSE
    hit_limit <- FALSE
    zeroed    <- FALSE
    
    if (capped[i]) {
      target <- now
      for (j in seq_along(slots)) {
        target <- if (j == length(slots)) rawben else target + ifelse(fnd[i, slots[j]] == 2, 1, -1) * amt[i, slots[j]]
        res <- if (cols[j] == "rawutil") move_toward(mydata, bi, i, "rawutil", target, util_min_step, TRUE) else
          move_toward(mydata, bi, i, cols[j], target)
        bi[[cols[j]]][i] <- res$value
        now <- res$benefit
      }
    } else {
      # Utilities (discrete) first
      u <- which(cols == "rawutil")[1]
      if (!recreatable[i]) {
        bi$rawutil[i] <- util_nature_step(mydata, bi, i, na[i, slots[u]])
        util_shift[i] <- bi$rawutil[i] - orig[["rawutil"]]
        next
      }
      if (!is.na(u) && now != rawben) {
        need  <- sign(rawben - now)
        if (direction_blocked(fnd[i, slots[u]], na[i, slots[u]], FALSE, need, nature_first[i] && slots[u] > 1)) {
          conflict <- TRUE
        } else {
          res <- if (single) move_toward(mydata, bi, i, "rawutil", rawben) else
            move_toward(mydata, bi, i, "rawutil", rawben, util_min_step, TRUE)
          bi$rawutil[i] <- res$value
          now <- res$benefit
        }
      }
      # Continuous variables close the rest. A $0 that leans on a later variable is only kept if the case then
      # lands within zero_margin; otherwise the pass is rerun without it
      cont  <- which(cols != "rawutil")
      pass_start <- list(vals = lapply(bi[unique(cols[cont])], `[`, i), now = now, conflict = conflict,
                         hit_limit = hit_limit)
      lean_ok <- TRUE
      repeat {
        leaned <- FALSE
        settled <- FALSE
        for (j in cont) {
          if (now == rawben || settled) break
          income <- cols[j] %in% c("rawearn", "rawunearn")
          need   <- sign(rawben - now) * (if (income) -1 else 1)
          if (direction_blocked(fnd[i, slots[j]], na[i, slots[j]], income, need, nature_first[i] && slots[j] > 1)) {
            conflict <- TRUE
            next
          }
          # Medical in an SMD state is discrete below the SMD ($0 or the SMD), so it can stop short of RAWBEN
          smd_med <- cols[j] == "rawmedded" && smd_state[i]
          res <- move_toward(mydata, bi, i, cols[j], rawben)
          was <- bi[[cols[j]]][i]
          # Leftover to $0 (see the Step 7 notes above)
          if (!smd_med && res$value > 0 && res$value < was &&
              (res$value <= (1 - zero_cut) * was || res$value <= zero_floor)) {
            zero_ben <- benefit_at(bi, i, cols[j], 0)
            later <- setdiff(cols[seq_along(cols) > j], c("rawutil", cols[j]))
            zero_close <- abs(zero_ben - rawben) <= zero_margin
            if (zero_close || (lean_ok && length(later) > 0)) {
              res <- list(value = 0, benefit = zero_ben)
              zeroed <- TRUE
              leaned <- leaned || !zero_close
              settled <- zero_close   # within zero_margin: later variables do not chase the last few dollars
            }
          }
          bi[[cols[j]]][i] <- res$value
          now <- res$benefit
          if (now != rawben && j < length(slots) && !smd_med && !settled) hit_limit <- TRUE
        }
        if (!leaned || abs(now - rawben) <= zero_margin) break
        for (v in names(pass_start$vals)) bi[[v]][i] <- pass_start$vals[[v]]
        now <- pass_start$now; conflict <- pass_start$conflict; hit_limit <- pass_start$hit_limit; zeroed <- FALSE
        lean_ok <- FALSE
      }
    }
    # A cut taken to $0 counts as reaching RAWBEN when within zero_margin
    reached <- now == rawben || (zeroed && abs(now - rawben) <= zero_margin)
    
    # Notes: one variable keeps the single-variable labels (earn_down, rent_error, util_no_change, ...);
    # several list each variable moved, utilities first (so smoothing and the SMD step leave them alone)
    moved <- vapply(names(orig), function(v) bi[[v]][i] - orig[[v]], numeric(1))
    if (single) {
      p <- step7_prefix[[names(orig)]]
      note <- if (names(orig) == "rawutil" && moved != 0) paste0(p, if (moved > 0) "_up" else "_down") else
        if (conflict || !reached) paste0(p, "_error") else
        if (moved != 0) paste0(p, if (moved > 0) "_up" else "_down") else paste0(p, "_no_change")
      amt_i <- moved[[1]]
    } else {
      cm   <- moved[moved != 0]
      note <- if (length(cm) == 0) "no_change" else
        paste0(step7_prefix[names(cm)], ifelse(cm > 0, "_up", "_down"), collapse = "+")
      if (!reached) note <- if (length(cm) == 0) "cascade_error" else paste0(note, "+cascade_error")
      amt_i <- if (length(cm) > 0) cm[[1]] else 0
      ctype[i]  <- if (capped[i]) "capped" else "uncapped"
      climit[i] <- hit_limit
    }
    # A Step 7 ELEMENT1 replaces an earlier note; after an ELEMENT1 household size or homeless correction
    # that note stays in front (e.g. "hhsize_up+earn_down")
    if (notes[i] == "no_change" || el[i, 1] %in% step7_elements) {
      notes[i]  <- note
      amount[i] <- amt_i
    } else if (note != "no_change" && !grepl("_no_change$", note)) {
      notes[i] <- paste0(notes[i], "+", note)
    }
  }
  
  for (v in unique(step7_vars)) mydata[[v]] <- bi[[v]]
  mydata$correctednotes <- notes
  mydata$correctedamount <- amount
  mydata$util_secondary_shift <- util_shift
  mydata$cascade_type <- ctype
  mydata$cascade_hit_limit <- climit
  calculate_raw_benefits(mydata)
}

mydata$cascade_type <- NA_character_
mydata$cascade_hit_limit <- NA
mydata$amount_pattern <- amount_pattern(mydata)

mydata$homeless_secondary_shift <- 0
if (correct_variables){
  
  # Homeless cases with a rent/utility error: assume no homeless deduction
  hl <- mydata$rawhomeless_ded > 0 & mydata$ELEMENT1 %in% c(363, 364)
  mydata$rawhomeless_ded[hl] <- 0

  # Element 362 (homeless shelter deduction) in any element slot, set from its nature
  # Nature 53 (agency included it): the homeless standard for the review's fiscal year
  # Nature 52 (agency left it out): $0
  # Applied before the ELEMENT1 adjustment; ELEMENT2+ hits are folded into correctednotes after Step 7
  hl_nature <- rep(NA_real_, nrow(mydata))
  for (k in 1:9) {
    el_col <- paste0("ELEMENT", k)
    na_col <- paste0("NATURE", k)
    if (!all(c(el_col, na_col) %in% names(mydata))) next
    hit <- is.na(hl_nature) & mydata[[el_col]] %in% 362 & mydata[[na_col]] %in% c(52, 53)
    hl_nature[hit] <- mydata[[na_col]][hit]
  }
  el362 <- !is.na(hl_nature)
  hl_before <- mydata$rawhomeless_ded
  mydata$rawhomeless_ded[el362] <- ifelse(
    hl_nature[el362] == 53,
    year_data$homeless_standard[match(mydata$fiscal_year[el362], year_data$year)],
    0
  )
  hl_shift <- mydata$rawhomeless_ded - hl_before
  el362_first <- el362 & mydata$ELEMENT1 %in% 362 & mydata$NATURE1 %in% c(52, 53)
  mydata$correctednotes[el362_first] <- ifelse(hl_shift[el362_first] > 0, "homeless_up",
                                               ifelse(hl_shift[el362_first] < 0, "homeless_down", "homeless_no_change"))
  mydata$correctedamount[el362_first] <- hl_shift[el362_first]
  mydata$homeless_secondary_shift <- ifelse(el362 & !el362_first, hl_shift, 0)
  
  # Two people excluded, as a fallback after Step 7: the natures say "person(s)", but Step 6 takes off one
  # person. For eligible-person-excluded cases that still miss RAWBEN after Step 7 (so QC's cited elements had
  # their chance first), the case's Step 7 changes are set aside and one more person comes off, kept only on an
  # exact match (size stays at least 1); otherwise the Step 7 result stands:
  #   150 natures 12 / 16 (no income / newborn): the size alone
  #   150 nature 13 and person elements nature 6 (may have income): the size plus one or two members' income
  #     (FSAFIL = 1) taken out of FSEARN / FSUNEARN, kept only if exactly one income amount matches
  # Notes "hhsize_up_2" / "hhsize_up_2+income_down"
  two_idx  <- which(mydata$two_person_check)
  pre_vars <- c("rawearn", "rawunearn", "rawrent", "rawutil", "rawmedded", "rawdepded", "rawcsded", "correctednotes",
                "correctedamount", "util_secondary_shift", "cascade_type", "cascade_hit_limit")
  pre7 <- as.data.frame(mydata[two_idx, pre_vars])
  
  mydata <- recreate_elements(mydata)
  
  two_rows <- two_idx[(mydata$rawben_recreated[two_idx] != mydata$RAWBEN[two_idx]) %in% TRUE]
  if (length(two_rows) > 0) {
    p   <- match(two_rows, two_idx)
    sub <- mydata[two_rows, ]
    for (v in pre_vars) sub[[v]] <- pre7[[v]][p]
    no_income   <- sub$ELEMENT1 %in% 150 & sub$NATURE1 %in% c(12, 16)
    with_income <- (sub$ELEMENT1 %in% 150 & sub$NATURE1 %in% 13) |
      (sub$ELEMENT1 %in% setdiff(hh_elements, 150) & sub$NATURE1 %in% 6)
    size2 <- as.numeric(sub$rawusize) - 1
    try2  <- (no_income | with_income) & size2 >= 1
    inputs <- c("rawearn", "rawunearn", "rawdepded", "rawmedded", "rawcsded", "rawstdded", "rawrent", "rawutil",
                "max_shelter_deduction", "rawhomeless_ded", "rawbenmax", "rawminimum_ben")
    base <- lapply(sub[inputs], as.numeric)
    base$rawbenmax      <- get_max_allotment(pmax(size2, 1), sub$fiscal_year)
    base$rawstdded      <- get_standard_deduction(sub$state_name, pmax(size2, 1), sub$fiscal_year)
    base$rawminimum_ben <- ifelse(size2 < 3, ifelse(is.na(sub$MINIMUM_BEN), 0, as.numeric(sub$MINIMUM_BEN)), 0)
    
    # Size alone
    hit0 <- try2 & no_income & (calculate_raw_benefits(base)$rawben_recreated == sub$RAWBEN) %in% TRUE
    
    # Size plus one or two members' income
    by_member <- function(f) matrix(unlist(lapply(1:18, f)), ncol = 18)
    E <- by_member(function(k) person_income(sub, earned_types, k))
    U <- by_member(function(k) person_income(sub, unearned_types, k))
    has_income <- by_member(function(k) {
      afil <- paste0("FSAFIL", k)
      if (afil %in% names(sub)) as.numeric(sub[[afil]]) %in% 1 else rep(FALSE, nrow(sub))
    }) & (E + U) > 0
    amt <- e_take <- u_take <- rep(NA_real_, nrow(sub))
    ambiguous <- rep(FALSE, nrow(sub))
    for (ks in c(as.list(1:18), combn(18, 2, simplify = FALSE))) {
      valid <- try2 & with_income & rowSums(has_income[, ks, drop = FALSE]) == length(ks)
      if (!any(valid)) next
      e <- rowSums(E[, ks, drop = FALSE])
      u <- rowSums(U[, ks, drop = FALSE])
      b <- base
      b$rawearn   <- pmax(b$rawearn - e, 0)
      b$rawunearn <- pmax(b$rawunearn - u, 0)
      hit <- valid & (calculate_raw_benefits(b)$rawben_recreated == sub$RAWBEN) %in% TRUE
      ambiguous <- ambiguous | (hit & !is.na(amt) & amt != e + u)
      first <- hit & is.na(amt)
      amt[first] <- (e + u)[first]; e_take[first] <- e[first]; u_take[first] <- u[first]
    }
    hit_inc <- !is.na(amt) & !ambiguous
    
    use  <- hit0 | hit_inc
    rows <- two_rows[use]
    for (v in pre_vars) mydata[[v]][rows] <- pre7[[v]][p[use]]   # set the case's Step 7 changes aside
    mydata$rawusize[rows]       <- size2[use]
    mydata$rawbenmax[rows]      <- base$rawbenmax[use]
    mydata$rawstdded[rows]      <- base$rawstdded[use]
    mydata$rawminimum_ben[rows] <- base$rawminimum_ben[use]
    inc_rows <- two_rows[hit_inc]
    mydata$rawearn[inc_rows]   <- pmax(as.numeric(sub$rawearn[hit_inc]) - e_take[hit_inc], 0)
    mydata$rawunearn[inc_rows] <- pmax(as.numeric(sub$rawunearn[hit_inc]) - u_take[hit_inc], 0)
    mydata$correctednotes[rows] <- ifelse(hit_inc[use], "hhsize_up_2+income_down", "hhsize_up_2")
    mydata$correctedamount[inc_rows] <- -amt[hit_inc]
    cat("two people excluded:", sum(hit0), "on size alone and", sum(hit_inc), "with income removed, of",
        sum(try2), "still off after Step 7 (", sum(ambiguous), "ambiguous )\n")
  }
  
  # Last year's tables: ELEMENT1 520 (arithmetic computation) and 361 (standard deduction) cases often match
  # RAWBEN when the agency budgeted with the previous fiscal year's tables, mostly in October-December reviews
  # just after the annual update (520 nature 79 is "incorrect use of allotment tables"). For these cases still
  # off after Step 7, last year's maximum allotment, standard deduction and shelter cap are tried alone, in pairs,
  # then all three; the first exact match is kept. Note "prior_year_tables"
  py_idx <- which(mydata$ELEMENT1 %in% c(361, 520))
  py_sub <- calculate_raw_benefits(mydata[py_idx, ])
  off    <- (py_sub$rawben_recreated != py_sub$RAWBEN) %in% TRUE
  py_idx <- py_idx[off]
  py_sub <- py_sub[off, ]
  prev <- list(
    rawbenmax = get_max_allotment(py_sub$rawusize, py_sub$fiscal_year - 1),
    rawstdded = get_standard_deduction(py_sub$state_name, py_sub$rawusize, py_sub$fiscal_year - 1),
    max_shelter_deduction = ifelse(is.infinite(py_sub$max_shelter_deduction), Inf,
                                   year_data$max_shelter_deduction[match(py_sub$fiscal_year - 1, year_data$year)])
  )
  py_sets <- list("rawbenmax", "rawstdded", "max_shelter_deduction",
                  c("rawbenmax", "rawstdded"), c("rawbenmax", "max_shelter_deduction"),
                  c("rawstdded", "max_shelter_deduction"), c("rawbenmax", "rawstdded", "max_shelter_deduction"))
  py_set <- rep(0L, length(py_idx))
  for (ps in seq_along(py_sets)) {
    trial <- py_sub
    for (v in py_sets[[ps]]) trial[[v]] <- prev[[v]]
    hit <- py_set == 0 & (calculate_raw_benefits(trial)$rawben_recreated == py_sub$RAWBEN) %in% TRUE
    py_set[hit] <- ps
  }
  for (ps in seq_along(py_sets)) {
    hit <- py_set == ps
    for (v in py_sets[[ps]]) mydata[[v]][py_idx[hit]] <- prev[[v]][hit]
  }
  py_rows <- py_idx[py_set > 0]
  old <- sub("\\+?cascade_error$", "", mydata$correctednotes[py_rows])
  mydata$correctednotes[py_rows] <- ifelse(old %in% c("", "no_change"), "prior_year_tables", paste0(old, "+prior_year_tables"))
  cat("last year's tables:", length(py_rows), "of", length(py_idx), "ELEMENT1 361 / 520 cases still off after Step 7\n")
  print(table(set = sapply(py_sets, paste, collapse = "+")[py_set[py_set > 0]], ELEMENT1 = mydata$ELEMENT1[py_rows]))
}
mydata$two_person_check <- NULL


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
# A case whose RAWBEN sits on a benefit limit only gives a bound: Step 7 stops at the first value that reaches it,
# and any value further in the same direction gives the same benefit, so these cases pile up on the limit.
#   "max": RAWBEN = maximum allotment; lower income or higher deductions keep it.
#   "min": RAWBEN = minimum benefit (1-2 person units); higher income or lower deductions keep it.
# The last continuous variable moved ("rent_down+earn_up" -> earn) is redrawn from cases with the same correction
# between the two limits (value identified), far enough past the stop point to leave the limit. Moves toward $0
# use the donors' raw/QC ratio, moves up their raw - QC dollar difference; with no donor that far, moves up add a
# donor's difference and moves toward $0 draw uniformly down to $0. Draws toward $0 follow Step 7's zero rule.
# The benefit cannot change (checked at the end). Left alone: utilities, household size, the homeless deduction,
# and medical at a state's SMD amount or moved down in an SMD state.
impute_seed     <- 20261029
impute_vars <- c(earn = "rawearn", unearn = "rawunearn", rent = "rawrent",
                 med = "rawmedded", dep = "rawdepded", cs = "rawcsded")
impute_qc   <- c(rawearn = "FSEARN", rawunearn = "FSUNEARN", rawrent = "RENT",
                 rawmedded = "FSMEDDED", rawdepded = "FSDEPDED", rawcsded = "FSCSDED")

# Last continuous variable moved in a note: "hhsize_up+rent_down+earn_up" -> "earn_up"
closing_move <- function(notes) {
  notes[is.na(notes)] <- ""
  m <- regmatches(notes, gregexpr("(^|\\+)(earn|unearn|rent|med|dep|cs)_(up|down)(?=\\+|$)", notes, perl = TRUE))
  vapply(m, function(x) if (length(x)) sub("^\\+", "", x[length(x)]) else NA_character_, character(1))
}

impute_at_bound <- function(mydata, side = c("max", "min"), seed = impute_seed) {
  side <- match.arg(side)
  set.seed(seed)
  mydata  <- calculate_raw_benefits(mydata)
  if (!"imputed_at_bound" %in% names(mydata)) mydata$imputed_at_bound <- NA_character_
  notes   <- ifelse(is.na(mydata$correctednotes), "", mydata$correctednotes)
  closing <- closing_move(notes)
  rb      <- as.numeric(mydata$RAWBEN)
  maxb    <- as.numeric(mydata$rawbenmax)
  minb    <- as.numeric(mydata$rawminimum_ben)
  # Reached RAWBEN: exactly, or within zero_margin when Step 7 took a cut to $0 (its notes decide _error)
  matched <- !grepl("_error", notes) & (abs(mydata$rawben_recreated - rb) <= zero_margin) %in% TRUE
  at_max  <- (rb == maxb) %in% TRUE
  at_min  <- (minb > 0 & rb == minb) %in% TRUE
  at_lim  <- if (side == "max") at_max else at_min
  limit   <- if (side == "max") maxb else minb
  between <- !at_max & !at_min
  smd     <- (as.numeric(mydata$smd_amt) > 0) %in% TRUE
  ben_before <- mydata$rawben_recreated
  bi <- lapply(mydata[benefit_inputs], as.numeric)

  # First value past the stop point b that takes the uncapped benefit off the limit: the benefit only changes
  # every few dollars, so a value just past b can still sit exactly on the limit
  edge_past <- function(i, col, b, up, lim) {
    trial <- if (up) b + 1:60 else b - 1:60
    trial <- trial[trial >= 0]
    if (length(trial) == 0) return(b)
    t <- lapply(bi, function(x) rep(x[i], length(trial)))
    t[[col]] <- trial
    off <- trial[calculate_raw_benefits(t)$rawben_uncapped != lim]
    if (length(off) > 0) off[1] else if (up) b + 60 else 0
  }

  for (move in sort(unique(closing[matched & at_lim & !is.na(closing)]))) {
    col    <- impute_vars[[sub("_.*$", "", move)]]
    qc     <- impute_qc[[col]]
    up     <- grepl("_up$", move)
    income <- col %in% c("rawearn", "rawunearn")
    # Only moves that keep the benefit on this limit when taken further
    if (side == "max" && income == up) next   # max: income down, deductions up
    if (side == "min" && income != up) next   # min: income up, deductions down
    # Stuck: on the limit with this variable closing the gap, exactly where Step 7 stopped
    cen <- which(matched & at_lim & closing %in% move & (mydata$rawben_uncapped == limit) %in% TRUE)
    if (col == "rawmedded") {
      at_smd <- (mydata$rawmedded == mydata$smd_amt) %in% TRUE
      cen <- cen[!(smd[cen] & (at_smd[cen] | !up))]
    }
    if (length(cen) == 0) next

    # Donors: same closing correction, strictly between the limits, reached RAWBEN (value identified)
    don   <- which(matched & between & closing %in% move)
    q_d   <- as.numeric(mydata[[qc]][don])
    v_d   <- as.numeric(mydata[[col]][don])
    ratio <- (v_d / q_d)[!is.na(q_d) & q_d > 0]
    diff  <- (v_d - q_d)[!is.na(q_d)]
    n_fb  <- 0

    for (i in cen) {
      b  <- as.numeric(mydata[[col]][i])
      q  <- as.numeric(mydata[[qc]][i])
      q0 <- if (is.na(q)) 0 else q
      edge <- edge_past(i, col, b, up, limit[i])
      pool <- if (up) q0 + diff[diff >= edge - q0] else
        if (q0 > 0) floor(ratio[ratio <= edge / q0] * q0) else numeric(0)
      pool <- pool[if (up) pool >= edge else pool <= edge]
      new <- if (length(pool) > 0) pool[sample.int(length(pool), 1)] else {
        # no donor past the edge: up adds a donor's dollar difference beyond the edge; toward $0 draws
        # uniformly from $0 to the edge
        n_fb <- n_fb + 1
        if (up) {
          pos <- diff[diff > 0]
          edge + if (length(pos) > 0) pos[sample.int(length(pos), 1)] else 0
        } else floor(runif(1, 0, edge + 1))
      }
      if (!up && new > 0 && (new <= zero_floor || new <= (1 - zero_cut) * q0)) new <- 0
      mydata[[col]][i] <- new
    }
    mydata$imputed_at_bound[cen] <- side
    cat(sprintf("impute_at_bound %-3s %-12s stuck %5d  donors %6d  fallback %3d\n",
                side, move, length(cen), length(don), n_fb))
  }

  mydata <- calculate_raw_benefits(mydata)
  stopifnot("impute_at_bound changed a recreated benefit" =
              isTRUE(all.equal(mydata$rawben_recreated, ben_before)))
  mydata
}

mydata <- calculate_raw_benefits(mydata)
mydata$imputed_at_bound <- NA_character_
if (apply_correction_smoothing) {
  mydata <- impute_at_bound(mydata, "max")
  mydata <- impute_at_bound(mydata, "min")
}

add_total_deductions <- function(data) {
  data$raw_total_deductions <- rowSums(data[c("rawdepded", "rawcsded", "rawsltded", "rawmedded",
                                              "rawhomeless_ded", "rawernded", "rawstdded")], na.rm = TRUE)
  data
}
mydata <- add_total_deductions(mydata)

# Fold secondary household size, utility and homeless shifts into correctednotes, e.g. "hhsize_up+earn_down"
sec_note <- ifelse(mydata$hhsize_secondary_shift < 0, "hhsize_up",
                   ifelse(mydata$hhsize_secondary_shift > 0, "hhsize_down", NA))
util_note <- ifelse(mydata$util_secondary_shift < 0, "util_down",
                    ifelse(mydata$util_secondary_shift > 0, "util_up", NA))
homeless_note <- ifelse(mydata$homeless_secondary_shift < 0, "homeless_down",
                        ifelse(mydata$homeless_secondary_shift > 0, "homeless_up", NA))
join_notes <- function(a, b) ifelse(is.na(a), b, ifelse(is.na(b), a, paste0(a, "+", b)))
sec_note <- join_notes(join_notes(sec_note, util_note), homeless_note)
has_sec <- !is.na(sec_note)
mydata$correctednotes[has_sec] <- ifelse(
  mydata$correctednotes[has_sec] == "no_change",
  sec_note[has_sec],
  paste0(sec_note[has_sec], "+", mydata$correctednotes[has_sec])
)

# Nature 97 / agency 26 (change not required to be reported or acted upon): that variance is not an error.
# Every variance 97 / 26: the budgeted benefit was correct, so the FS side is set to the reconstructed raw side and
# the variance fields are cleared (no_action_required_i).
# Some variances 97 / 26 and some not (no_action_partial_i): the 97 / 26 elements' variables take the reconstructed
# (agency) value on the FS side too, FSBEN moves by the benefit change that makes, and the error amount, finding and
# over-threshold flag are recomputed from what is left. A variable that a real element also uses cannot be
# separated and stays in the error. The element / nature labels move to the first real element.
el_m <- slot_matrix(mydata, "ELEMENT")
na_m <- slot_matrix(mydata, "NATURE")
ag_m <- slot_matrix(mydata, "AGENCY")
present <- !is.na(el_m)
na_slot <- present & (na_m %in% 97 | ag_m %in% 26)
real_slot <- present & !na_slot
no_action_required <- rowSums(na_slot) > 0 & rowSums(real_slot) == 0
no_action_partial  <- rowSums(na_slot) > 0 & rowSums(real_slot) > 0

p <- which(no_action_partial)
if (length(p) > 0) {
  num <- function(x) { x <- as.numeric(x); x[is.na(x)] <- 0; x }
  fs_cols <- c(rawearn = "FSEARN", rawunearn = "FSUNEARN", rawrent = "RENT", rawutil = "UTIL",
               rawmedded = "FSMEDDED", rawdepded = "FSDEPDED", rawcsded = "FSCSDED")
  d <- mydata[p, ]
  fs_side <- list(rawearn = num(d$FSEARN), rawunearn = num(d$FSUNEARN), rawdepded = num(d$FSDEPDED),
                  rawmedded = num(d$FSMEDDED), rawcsded = num(d$FSCSDED), rawstdded = num(d$FSSTDDED),
                  rawrent = num(d$RENT), rawutil = num(d$UTIL), max_shelter_deduction = d$max_shelter_deduction,
                  rawhomeless_ded = num(d$HOMELESS_DED), rawbenmax = num(d$BENMAX), rawminimum_ben = num(d$fsminimum_ben))
  swapped <- fs_side
  for (v in unique(step7_vars)) {
    els <- as.numeric(names(step7_vars)[step7_vars == v])
    has_v <- matrix(el_m[p, , drop = FALSE] %in% els, ncol = 9)
    sw <- rowSums(na_slot[p, , drop = FALSE] & has_v) > 0 & rowSums(real_slot[p, , drop = FALSE] & has_v) == 0
    swapped[[v]][sw] <- as.numeric(d[[v]][sw])
    mydata[[fs_cols[[v]]]][p[sw]] <- as.numeric(d[[v]][sw])
  }
  # Benefit change from the swap, on the FS-side formula (so its rounding cancels out)
  shift <- calculate_raw_benefits(swapped)$rawben_recreated - calculate_raw_benefits(fs_side)$rawben_recreated
  old_diff <- as.numeric(mydata$bendiff[p])
  mydata$FSBEN[p] <- as.numeric(mydata$FSBEN[p]) + shift
  new_diff <- as.numeric(mydata$RAWBEN[p]) - as.numeric(mydata$FSBEN[p])
  mydata$bendiff[p] <- new_diff
  mydata$absbendiff[p] <- abs(new_diff)
  mydata$AMTERR[p] <- pmax(as.numeric(mydata$AMTERR[p]) + abs(new_diff) - abs(old_diff), 0)
  mydata$over_threshold[p] <- as.integer(abs(new_diff) > mydata$threshold[p])
  mydata$STATUS[p] <- ifelse(new_diff > 0, 2, ifelse(new_diff < 0, 3, 1))
  first_real <- apply(real_slot[p, , drop = FALSE], 1, function(x) which(x)[1])
  mydata$element[p] <- qc_elements$description[match(el_m[cbind(p, first_real)], qc_elements$code)]
  mydata$nature[p]  <- qc_natures$description[match(na_m[cbind(p, first_real)], qc_natures$code)]
  cat("nature 97 / agency 26 next to a real error:", length(p), "cases;", sum(shift != 0),
      "had the 97 / 26 part taken out of the error,", sum(abs(old_diff) > mydata$threshold[p] & !(abs(new_diff) > mydata$threshold[p])),
      "fell under the threshold\n")
}
mydata$no_action_partial_i <- no_action_partial

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
