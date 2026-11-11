# Build the model's 18 inputs from a state's raw case fields.
#
# This is an R copy of the feature code the Excel workbooks use
# (methods/excel_rules_for_states/make_input_workbook.py, mirror_features), which the
# workbook build checks against the research frame for every state. The input columns
# are the workbook's Step 2 columns, so one file serves both tools.
#
# Dollar amounts come in each case's own fiscal-year dollars. The benefit ratios and the
# utility tier are computed with that year's federal and state tables; every other
# dollar feature is inflated to FY2026 dollars (MODELING_YEAR), as the research frame is.
# A feature is left blank (NA) when any input it needs is blank; the model handles a
# blank the way it handled a missing value in training.

MODELING_YEAR <- 2026
SUA_TOLERANCE <- 10      # utility tier 2 = within $10 of the state's heating/cooling standard, or above it

# the input columns: the workbook's raw block, without the QC review outcome
INPUT_COLUMNS <- c(
  "CASE_ID", "REVIEW_FISCAL_YEAR", "HOUSEHOLD_SIZE",
  "NUM_CHILDREN", "NUM_ELDERLY", "NUM_DISABLED", "NUM_ABAWD",
  "MARRIED_FLAG", "EXPEDITED", "CATEGORICALLY_ELIGIBLE", "HOMELESS_FLAG",
  "MONTHS_SINCE_CERT",
  "EARNED_INCOME", "UNEARNED_INCOME",
  "MEDICAL_DEDUCTION", "DEPENDENT_CARE_DEDUCTION", "CHILD_SUPPORT_EXPENSES",
  "HOMELESS_DEDUCTION", "RENT", "UTILITY_COSTS",
  "ORIGINAL_BENEFIT_AMOUNT")

BEN_INPUTS <- c("REVIEW_FISCAL_YEAR", "HOUSEHOLD_SIZE", "EARNED_INCOME", "UNEARNED_INCOME",
                "MEDICAL_DEDUCTION", "DEPENDENT_CARE_DEDUCTION", "CHILD_SUPPORT_EXPENSES",
                "HOMELESS_DEDUCTION", "RENT", "UTILITY_COSTS", "NUM_ELDERLY", "NUM_DISABLED")

# which input columns each feature needs (a blank in any of them blanks the feature)
FEATURE_INPUTS <- list(
  HH_size_n                   = "HOUSEHOLD_SIZE",
  children_i                  = "NUM_CHILDREN",
  elderly_disabled_i          = c("NUM_ELDERLY", "NUM_DISABLED"),
  total_deductions_by_hh_size = BEN_INPUTS,
  expedited_i                 = "EXPEDITED",
  bbce_state_i                = c("REVIEW_FISCAL_YEAR", "CATEGORICALLY_ELIGIBLE"),
  rawben_rel_max              = BEN_INPUTS,
  medical_deductions          = c("MEDICAL_DEDUCTION", "REVIEW_FISCAL_YEAR"),
  shelter_expenses_by_hh_size = c("RENT", "UTILITY_COSTS", "HOUSEHOLD_SIZE", "REVIEW_FISCAL_YEAR"),
  utilities_sua               = c("UTILITY_COSTS", "REVIEW_FISCAL_YEAR"),
  married                     = "MARRIED_FLAG",
  homeless                    = "HOMELESS_FLAG",
  percent_abawd               = c("NUM_ABAWD", "HOUSEHOLD_SIZE"),
  unc_rawben_rel_max          = BEN_INPUTS,
  months_since_cert_n         = "MONTHS_SINCE_CERT",
  gross_by_hh_size            = c("EARNED_INCOME", "UNEARNED_INCOME", "HOUSEHOLD_SIZE", "REVIEW_FISCAL_YEAR"),
  earned_by_hh_size           = c("EARNED_INCOME", "HOUSEHOLD_SIZE", "REVIEW_FISCAL_YEAR"),
  unearned_by_hh_size         = c("UNEARNED_INCOME", "HOUSEHOLD_SIZE", "REVIEW_FISCAL_YEAR"))

# Federal and state tables (copies of the repo's additional_data files)
load_tables <- function(dir) {
  rd <- function(f) { t <- read.csv(file.path(dir, f), check.names = FALSE, stringsAsFactors = FALSE,
                                    fileEncoding = "UTF-8-BOM")
    names(t) <- trimws(names(t)); t }
  yd <- rd("year_data.csv"); sd <- rd("standard_deductions.csv"); ma <- rd("max_allotments.csv")
  list(yd = yd[order(yd$year), ], sd = sd[order(sd$year), ], ma = ma[order(ma$year), ],
       sua = rd("state_sua.csv"), smd = rd("standard_medical_deductions.csv"),
       opts = rd("state_options.csv"))
}

# value of a by-year table at each year: the latest row at or before it (a year past
# the last row uses the last row, as the workbook does)
.by_year <- function(tbl, col, years) {
  i <- findInterval(years, tbl$year)
  if (any(i == 0, na.rm = TRUE)) stop("fiscal year ", min(years[i == 0], na.rm = TRUE),
                                      " is earlier than the tables (first year ", min(tbl$year), ")")
  tbl[[col]][i]
}
.by_year_size <- function(tbl, years, sizes) {
  i <- findInterval(years, tbl$year)
  if (any(i == 0, na.rm = TRUE)) stop("fiscal year earlier than the tables")
  m <- as.matrix(tbl[as.character(1:20)])
  m[cbind(i, sizes)]
}
.state_row <- function(tbl, state) {
  r <- tbl[tbl$state_name == state, , drop = FALSE]
  if (!nrow(r)) return(setNames(numeric(0), character(0)))
  yrs <- setdiff(names(r), "state_name")
  setNames(as.numeric(unlist(r[1, yrs])), yrs)
}
.at_year <- function(vals, years) {             # a state's by-year value; 0 where absent
  v <- unname(vals[as.character(years)]); v[is.na(v)] <- 0; v }

build_features <- function(raw, state, tables) {
  n <- nrow(raw)
  g <- function(c) { v <- raw[[c]]; v[is.na(v)] <- 0; as.numeric(v) }   # a blank counts as 0 in the arithmetic
  fl <- function(x) floor(x)
  opt <- tables$opts[tables$opts$state_name == state, , drop = FALSE]
  offset_col <- if (nrow(opt) && !is.na(opt$std_ded_offset_col[1])) trimws(opt$std_ded_offset_col[1]) else ""
  prior_year <- nrow(opt) > 0 && isTRUE(as.logical(opt$prior_year_tables[1]))
  yd <- tables$yd; sd <- tables$sd; ma <- tables$ma

  fy <- as.integer(g("REVIEW_FISCAL_YEAR"))
  my <- rep(MODELING_YEAR, n)
  sz20 <- as.integer(pmin(pmax(g("HOUSEHOLD_SIZE"), 1), 20))
  hh <- pmax(g("HOUSEHOLD_SIZE"), 1)

  stdded_at <- function(years) {
    v <- .by_year_size(sd, years, sz20)
    if (nzchar(offset_col)) v <- v - .by_year(sd, offset_col, years)
    v
  }
  eld <- as.integer(g("NUM_ELDERLY") + g("NUM_DISABLED") > 0)

  # benefit chain in the case's own fiscal year, with that year's tables
  stdded <- stdded_at(fy)
  benmax <- .by_year_size(ma, fy, sz20)
  ernded <- fl(g("EARNED_INCOME") * 0.2)
  maxsh <- ifelse(eld == 1, 1e9, .by_year(yd, "max_shelter_deduction", fy))
  minal <- .by_year(yd, "min_allotment", fy)
  ben_chain <- function(stdded, benmax, maxsh) {
    netbs <- g("EARNED_INCOME") + g("UNEARNED_INCOME") -
      (ernded + g("DEPENDENT_CARE_DEDUCTION") + g("MEDICAL_DEDUCTION") + g("CHILD_SUPPORT_EXPENSES") + stdded)
    sltded <- pmin(pmax(g("RENT") + g("UTILITY_COSTS") - pmax(netbs * 0.5, 0), 0), maxsh)
    sltded <- ifelse(g("HOMELESS_DEDUCTION") > 0, 0, sltded)
    netan <- fl(netbs - (sltded + g("HOMELESS_DEDUCTION")))
    benunc <- fl(benmax - 0.3 * netan)
    benrec <- pmin(pmax(benunc, ifelse(g("HOUSEHOLD_SIZE") < 3, minal, 0)), benmax)
    list(unc = benunc, rec = benrec)
  }
  bc <- ben_chain(stdded, benmax, maxsh); benunc <- bc$unc; benrec <- bc$rec

  if (prior_year) {
    # a case whose inputs miss the issued benefit with this year's maximum allotment,
    # standard deduction and shelter cap, but hit it exactly with last year's (alone,
    # in pairs, then all three; first hit kept), takes last year's
    issued <- suppressWarnings(as.numeric(raw$ORIGINAL_BENEFIT_AMOUNT))
    has_prev <- fy - 1L >= max(min(ma$year), min(sd$year), min(yd$year))   # no tables before the first year
    py <- ifelse(has_prev, fy - 1L, fy)
    prev <- list(max = .by_year_size(ma, py, sz20), std = stdded_at(py),
                 sh = ifelse(eld == 1, 1e9, .by_year(yd, "max_shelter_deduction", py)))
    cur <- list(max = benmax, std = stdded, sh = maxsh)
    todo <- has_prev & !is.na(issued) & benrec != issued
    for (combo in list("max", "std", "sh", c("max", "std"), c("max", "sh"), c("std", "sh"), c("max", "std", "sh"))) {
      t <- lapply(setNames(nm = names(cur)), function(k) if (k %in% combo) prev[[k]] else cur[[k]])
      r <- ben_chain(t$std, t$max, t$sh)
      hit <- todo & r$rec == issued
      benunc[hit] <- r$unc[hit]; benrec[hit] <- r$rec[hit]; benmax[hit] <- t$max[hit]
      todo <- todo & !hit
    }
  }

  # utility tier against the state's heating/cooling standard for the case's year
  sua <- .state_row(tables$sua, state)
  util <- g("UTILITY_COSTS")
  anchor <- .at_year(sua, fy)
  tier <- ifelse(util <= 0, 0, ifelse(anchor <= 0, NA_real_, ifelse(util + SUA_TOLERANCE < anchor, 1, 2)))

  # inflation to FY2026 dollars
  cpif <- .by_year(yd, "cpi", my) / .by_year(yd, "cpi", fy)
  infl <- function(c) fl(g(c) * cpif)
  earn_i <- infl("EARNED_INCOME"); unearn_i <- infl("UNEARNED_INCOME")
  dep_i <- infl("DEPENDENT_CARE_DEDUCTION"); cs_i <- infl("CHILD_SUPPORT_EXPENSES"); rent_i <- infl("RENT")
  smd <- .at_year(.state_row(tables$smd, state), MODELING_YEAR)
  med <- g("MEDICAL_DEDUCTION")
  med_i <- ifelse(smd > 0 & med > 0 & med < smd, fl(smd), fl(med * cpif))
  sua_my <- .at_year(sua, MODELING_YEAR)
  util_i <- ifelse(!is.na(tier) & tier == 2, if (sua_my > 0) fl(sua_my) else NA_real_, fl(util * cpif))
  hless_i <- ifelse(g("HOMELESS_DEDUCTION") > 0, .by_year(yd, "homeless_standard", my), g("HOMELESS_DEDUCTION"))
  stdded_i <- stdded_at(my)
  ernded_i <- fl(earn_i * 0.2)
  gross_i <- earn_i + unearn_i
  netbs_i <- gross_i - (ernded_i + dep_i + med_i + cs_i + stdded_i)
  maxsh_i <- ifelse(eld == 1, 1e9, .by_year(yd, "max_shelter_deduction", my))
  sltded_i <- pmin(pmax(rent_i + util_i - pmax(netbs_i * 0.5, 0), 0), maxsh_i)
  sltded_i <- ifelse(g("HOMELESS_DEDUCTION") > 0, 0, sltded_i)
  # BBCE: 1 when at least half of the file's cases in that fiscal year are categorically eligible
  fyshare <- ave(g("CATEGORICALLY_ELIGIBLE"), fy, FUN = mean)

  f <- data.frame(
    HH_size_n = g("HOUSEHOLD_SIZE"),
    children_i = as.numeric(g("NUM_CHILDREN") > 0),
    elderly_disabled_i = as.numeric(eld),
    total_deductions_by_hh_size = (dep_i + cs_i + sltded_i + med_i + hless_i + ernded_i + stdded_i) / hh,
    expedited_i = g("EXPEDITED"),
    bbce_state_i = as.numeric(fyshare >= 0.5),
    rawben_rel_max = benrec / benmax,
    medical_deductions = med_i,
    shelter_expenses_by_hh_size = (rent_i + util_i) / hh,
    utilities_sua = tier,
    married = g("MARRIED_FLAG"),
    homeless = g("HOMELESS_FLAG"),
    percent_abawd = g("NUM_ABAWD") / hh,
    unc_rawben_rel_max = benunc / benmax,
    months_since_cert_n = g("MONTHS_SINCE_CERT"),
    gross_by_hh_size = gross_i / hh,
    earned_by_hh_size = earn_i / hh,
    unearned_by_hh_size = unearn_i / hh)
  for (nm in names(FEATURE_INPUTS)) {
    gap <- Reduce(`|`, lapply(intersect(FEATURE_INPUTS[[nm]], names(raw)), function(c) is.na(raw[[c]])), FALSE)
    f[[nm]][gap] <- NA
  }
  # the recomputed benefit (review-year dollars) sets the size model's starting value, as in training
  ben_gap <- Reduce(`|`, lapply(BEN_INPUTS, function(c) is.na(raw[[c]])), FALSE)
  attr(f, "recomputed_benefit") <- ifelse(ben_gap, NA_real_, benrec)
  attr(f, "recomputed_benefit_blanks_as_0") <- benrec      # last resort when the issued benefit is blank too
  attr(f, "bbce_by_year") <- tapply(fyshare >= 0.5, fy, function(v) as.integer(v[1]))
  f
}
