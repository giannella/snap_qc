# Shared setup for the BBCE-limit feature exploration (2026-10-22). See design_note.md.
#
# Reuses the v2.7 seed-replicate study's setup in the three-years-ahead window
# (mine FY2017-19, score FY2022): the frame loader, the benchmark's admission
# (BH at FDR 10% and n >= 30, 99% Wilson LCB ordering), the fresh-share fill
# walk and the test-year scoring, all taken by name from the shipped benchmark.
# What this study changes: ranger alone (the arms are compared with each other,
# never with the xgboost + ranger pools on record) and the feature list.
# Night 3 (2026-10-24) adds a second window, SR_WINDOW=fy2024 (mine FY2022-23,
# score FY2024), with its outputs in fy2024/; fy2022 stays the default.
if (!nzchar(Sys.getenv("SR_WINDOW"))) Sys.setenv(SR_WINDOW = "fy2022")
if (!nzchar(Sys.getenv("SR_ROOT"))) Sys.setenv(SR_ROOT = "C:/Users/ericg/snap_qc_v27")
source(file.path(Sys.getenv("SR_ROOT"), "methods/v270_seed_replicates/common.R"))
stopifnot(WINDOW %in% c("fy2022", "fy2024"))
if (WINDOW == "fy2022") stopifnot(identical(TRAIN_YEARS, c("2017", "2018", "2019")), identical(TEST_YEAR, "2022")) else
  stopifnot(identical(TRAIN_YEARS, c("2022", "2023")), identical(TEST_YEAR, "2024"))

FRAME_ID <- "f1016"   # the current staging-v2.7 frame (Ben's 10-16 lookup and reconstruction fixes)
OUT_DIR <- paste0(if (SMOKE) "methods/bbce_limit_features/smoke" else "methods/bbce_limit_features",
                  if (WINDOW == "fy2024") "/fy2024" else "")
POOL_DIR <- file.path(OUT_DIR, "pools")
dir.create(POOL_DIR, showWarnings = FALSE, recursive = TRUE)

# the arms: the shipped 19 features plus the additions; ranger only
NEW <- c(dist = "dist_to_limit_pts", lim = "bbce_limit_pct", pov = "gross_pov_pct")
ARMS <- list(
  A = VOCAB19,
  B = c(VOCAB19, NEW[["dist"]]),
  C = c(VOCAB19, NEW[["lim"]]),
  D = c(VOCAB19, NEW[["dist"]], NEW[["lim"]]),
  E = c(VOCAB19, NEW[["pov"]], NEW[["lim"]]))
# night 2 (2026-10-23): the regimes plan's Tier 1, one arm with the six light
# state options (eight columns; see add_light_features)
LIGHT <- c("smd_gap_dollars", "smd_i", "esap_i", "cap_standard_i", "cap_modified_i",
           "county_admin_i", "se_other_method_i", "tba_i")
ARMS$T1 <- c(VOCAB19, LIGHT)
REGIME_FEATURES <- c("bbce_state_i", NEW[["lim"]], LIGHT)   # state-level: a rule limited by these is a regime-limited rule
BINARY_FEATURES <- c(BINARY_FEATURES, setdiff(LIGHT, "smd_gap_dollars"))   # 0/1 flags canonicalize to ">= 1" / "<= 0"
SEED_ARM <- as.integer(Sys.getenv("ARM_SEED", "117"))   # 117 for every arm; arm A is also run at 118 for a reading of ranger-only seed noise
stopifnot(!is.na(SEED_ARM))
# night 2: the forest's mtry (2, the shipped value, unless ARM_MTRY says otherwise)
MTRY_ARM <- as.integer(Sys.getenv("ARM_MTRY", "2"))
stopifnot(RF$mtry == 2L, MTRY_ARM %in% 2:4)
mtry_sfx <- if (MTRY_ARM == 2L) "" else sprintf("_mtry%d", MTRY_ARM)
# output tag: the arm letter at mtry 2 and seed 117; "_mtry<m>" and "_seed<s>"
# appended otherwise (night 1's tags and pool files are unchanged)
arm_tag <- function(arm) paste0(arm, mtry_sfx, if (SEED_ARM == 117L) "" else sprintf("_seed%d", SEED_ARM))

pool_file_arm <- function(arm, kind = c("admitted", "raw"))
  file.path(POOL_DIR, sprintf("%s_%s%s_seed%d.rds", match.arg(kind), arm, mtry_sfx, SEED_ARM))
# write to a temporary file, then rename: a cut-off mid-write cannot leave a
# truncated file that a relaunch would take for a finished one
save_atomic <- function(obj, fn) { tmp <- paste0(fn, ".tmp"); saveRDS(obj, tmp); stopifnot(file.rename(tmp, fn)) }
PILE_TAG_SHARE <- 0.25   # the shipped pile gate (methods/build_national_only_lists_v2.R)

# The three features, from review-year (unadjusted) amounts. The frame's
# rawearn / rawunearn are CPI-adjusted to FY2026; the *_nominal copies are the
# amounts as reviewed, which is what the poverty line and the state's limit
# apply to.
#   gross_pov_pct      reconstructed gross income as a percent of the HHS poverty
#                      guideline for the reported unit size; fiscal year t uses
#                      guideline year t-1 (this pairing reproduces the QC file's
#                      TPOV within 1 point on 99.96%+ of cases, every year)
#   bbce_limit_pct     the state's BBCE gross income limit that fiscal year, %
#                      of poverty (Ben's additional_data/bbce_income.csv, the
#                      general limit), 130 where the state has no BBCE
#   dist_to_limit_pts  gross_pov_pct - bbce_limit_pct
add_limit_features <- function(adf) {
  pg <- read.csv("additional_data/poverty_guidelines.csv")
  bi <- read.csv("additional_data/bbce_income.csv", check.names = FALSE, fileEncoding = "UTF-8-BOM")
  fy <- as.integer(as.character(adf$fiscal_year)); st <- as.character(adf$state)
  size <- as.numeric(as.character(adf$cert_HH_size_FS_n))
  region <- ifelse(st == "Alaska", "alaska", ifelse(st == "Hawaii", "hawaii", "contiguous"))
  r <- pg[match(paste(fy - 1L, region), paste(pg$guideline_year, pg$region)), ]
  stopifnot(!anyNA(r$p1), all(size >= 1))
  s8 <- pmin(size, 8)
  annual <- as.matrix(r[, paste0("p", 1:8)])[cbind(seq_along(s8), s8)] + pmax(size - 8, 0) * r$each_additional
  gross <- adf$rawearn_nominal + adf$rawunearn_nominal
  stopifnot(!anyNA(gross), st %in% bi$state_name)
  cells <- trimws(as.character(as.matrix(bi[, as.character(sort(unique(fy)))])))
  stopifnot(all(is.na(cells) | cells == "" | grepl("^[0-9]+$", cells)))   # a text cell must not silently become 130
  lim <- suppressWarnings(as.numeric(as.matrix(bi[, as.character(sort(unique(fy)))])[
    cbind(match(st, bi$state_name), match(as.character(fy), as.character(sort(unique(fy)))))]))
  adf$gross_pov_pct <- round(100 * gross / (annual / 12), 1)
  adf$bbce_limit_pct <- ifelse(is.na(lim), 130, lim)
  adf$dist_to_limit_pts <- adf$gross_pov_pct - adf$bbce_limit_pct
  # the BBCE flag the frame carries is derived from the QC file; the table's
  # limit is from policy sources. Report (never stop on) disagreement.
  dis <- !is.na(lim) != (adf$bbce_state_i == 1)
  stamp("limit features added: %d rows | BBCE flag vs table disagree on %d rows (%s)",
        nrow(adf), sum(dis), paste(head(unique(paste(st[dis], fy[dis])), 12), collapse = ", "))
  stopifnot(!anyNA(adf$gross_pov_pct), !anyNA(adf$bbce_limit_pct))
  adf
}

# The six light state options of the regimes plan's Tier 1 (eight columns),
# per state and fiscal year:
#   smd_gap_dollars    the state's standard medical deduction (Ben's
#                      additional_data/standard_medical_deductions.csv, the
#                      deducted amount; 0 where the state has none) minus (the
#                      Medicare Part B premium minus $35) for that year
#                      (year_data.csv, keyed on the fiscal year as the file is)
#   smd_i              the state has an SMD (that table's amount > 0)
#   esap_i             the options report lists ESAP
#   cap_standard_i, cap_modified_i   standard / modified CAP (neither = none)
#   county_admin_i     county- or region-administered
#   se_other_method_i  self-employment costs by a method other than actual
#                      costs (the 2016-17 editions ask "another method?": No =
#                      actual costs, Ben confirmed)
#   tba_i              transitional benefits
# Options-report editions: FY2017 -> 2017, FY2018 and FY2019 -> 2018 (no 2019
# edition), FY2022 -> 2023 (no 2022 edition). Every raw value must map.
OPT_EDITION <- c(`2017` = 2017L, `2018` = 2018L, `2019` = 2018L, `2022` = 2023L, `2023` = 2023L, `2024` = 2024L)
add_light_features <- function(adf) {
  op <- read.csv("additional_data/snap_state_options_all_years.csv", check.names = FALSE)
  smd <- read.csv("additional_data/standard_medical_deductions.csv", check.names = FALSE, fileEncoding = "UTF-8-BOM")
  yd <- read.csv("additional_data/year_data.csv", fileEncoding = "UTF-8-BOM")
  fy <- as.character(adf$fiscal_year); st <- as.character(adf$state)
  stopifnot(fy %in% names(OPT_EDITION))
  o <- op[match(paste(st, OPT_EDITION[fy]), paste(op$State, op$Year)), ]
  stopifnot(!anyNA(o$State))
  flag <- function(x, yes, no, what) {   # 1 / 0, stopping on any value matched by neither pattern or by both
    x <- trimws(as.character(x)); y <- grepl(yes, x); n <- grepl(no, x)
    bad <- unique(x[y == n]); if (length(bad)) stop(what, ": unmapped or ambiguous values: ", paste(bad, collapse = " | "))
    as.integer(y)
  }
  adf$esap_i <- flag(o$DemonstrationsElderlyDisability, "^(ESAP|ESAP only|ESAP & SMD|SMD & ESAP|SMD and ESAP|ESAP/SMD)$", "^(None|SMD|SMD only|No SMD or ESAP)$", "ESAP")
  adf$cap_standard_i <- flag(o$CAP, "^Standard CAP$", "^(No|No CAP|Modified CAP)$", "CAP standard")
  adf$cap_modified_i <- flag(o$CAP, "^Modified CAP$", "^(No|No CAP|Standard CAP)$", "CAP modified")
  adf$county_admin_i <- flag(o$ProgramAdministration, "ount", "^State.administered$", "administration")
  adf$se_other_method_i <- flag(o$SelfEmploymentIncome, "^(Another method|Flat percentage)", "^(No|Actual costs|Actual costs only)$", "self-employment")
  adf$tba_i <- flag(o$TBA, "^(Yes|TBA)$", "^(No|No TBA)$", "TBA")
  yrs <- sort(unique(fy))
  amt <- as.numeric(as.matrix(smd[, yrs])[cbind(match(st, smd$state_name), match(fy, yrs))])
  pb <- yd$medicare_part_b_premium[match(as.integer(fy), yd$year)]
  stopifnot(!anyNA(amt), !anyNA(pb))
  adf$smd_i <- as.integer(amt > 0)
  adf$smd_gap_dollars <- amt - (pb - 35)
  # the options report's SMD listing against the deduction table: report, never stop
  smd_opt <- grepl("SMD", o$DemonstrationsElderlyDisability) & !grepl("No SMD", o$DemonstrationsElderlyDisability)
  dis <- smd_opt != (adf$smd_i == 1)
  stamp("light features added | SMD: options report vs deduction table disagree on %d rows (%s)",
        sum(dis), paste(head(unique(paste(st[dis], fy[dis])), 12), collapse = ", "))
  for (v in LIGHT) stamp("  %s: %s", v, paste(names(table(adf[[v]]))[seq_len(min(6, length(table(adf[[v]]))))], collapse = " "))
  adf
}

load_frame_arm <- function() {
  F <- load_frame(FRAME_ID)
  F$adf <- add_light_features(add_limit_features(F$adf))
  F
}

# ranger alone, with the shipped forest settings (1000 trees, depth 4, mtry 2,
# min node 20) and the same canonicalization the pipeline applies; the same
# loop as mine_rule_vocabulary() minus its xgboost call
mine_ranger <- function(train, ie, strata_idx, features, seed) {
  stopifnot(MTRY_ARM <= length(features))
  parts <- list()
  for (h in names(strata_idx)) {
    ix <- strata_idx[[h]]
    rr <- canonicalize_rules(
      generate_rules_ranger(train[ix, , drop = FALSE], ie[ix], features,
                            num_trees = RF$num_trees, max_depth = RF$max_depth,
                            mtry = MTRY_ARM, min_node_size = RF$min_node_size, seed = seed),
      SIGNIF_DIGITS, binary_features = BINARY_FEATURES)
    stamp("  [mine any_error | HH %s] %d rows, %d errors -> ranger %d rules", h, length(ix), sum(ie[ix]), length(rr))
    if (length(rr)) parts[[length(parts) + 1L]] <- data.frame(hh = h, rule = rr, engines = "ranger",
                                                              mined_frames = "any_error", stringsAsFactors = FALSE)
  }
  out <- do.call(rbind, parts)
  out <- out[!duplicated(paste(out$hh, out$rule)), , drop = FALSE]
  out <- out[order(out$hh, out$rule, method = "radix"), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# the conditions of a rule on state-level features, as a rule of their own
# ("" when the rule has none)
regime_part <- function(rule) vapply(strsplit(rule, " & ", fixed = TRUE), function(a)
  paste(a[sub(" .*", "", a) %in% REGIME_FEATURES], collapse = " & "), "")
uses <- function(rule, feat) grepl(paste0("(^|& )", feat, " "), rule)
