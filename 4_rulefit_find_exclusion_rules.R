# ──────────────────────────────────────────────────────────────────────────────
# RuleFit for SNAP review-precision: finding efficient EXCLUSION criteria
#
# Setting: a state agency already has a pile of cases it flagged for review
# (`flagged_cases`). That pile mixes TRUE errors (worth a reviewer's time) with
# false positives. The goal is to find simple feature/threshold
# rules that let the agency safely exclude cases, leaving a more targeted set.
#
# OBJECTIVE TOGGLE  (set OBJECTIVE in Section 0):
#   "counts"   maximize PRECISION  = share of the retained pile that are errors.
#              Safety = don't drop true error CASES.  Cost = errors_lost (count).
#   "dollars"  maximize DOLLAR DENSITY = recoverable error $ per retained case.
#              Safety = don't drop error DOLLARS.      Cost = err_dollars_lost.
#
# Both views are computed for every rule regardless of the toggle; the toggle
# only decides which model is fit, how EXCLUDE/KEEP is called, how the shortlist
# is filtered/sorted, and which headline the portfolio prints.
#
# RuleFit implementation: {pre} (Prediction Rule Ensembles; Fokkema 2020, JSS).
#
# Thresholds are starting points learned from THIS pile. Use the
# threshold sweep (Section 6) to re-tune a single cutoff on internal data to the
# safety level you want (e.g. "keep 98% of error dollars").
# ──────────────────────────────────────────────────────────────────────────────

library(pre)
library(dplyr)

set.seed(42)

## ── 0. Config ─────────────────────────────────────────────────────────────────

# `flagged_cases` is expected in the environment: the agency's already-prioritized
# review pile (true errors + clean false-flags together).

OBJECTIVE <- "dollars"      # "counts" or "dollars"

features <- c(
  "cert_HH_size_FS_n",            # certified household size
  "children_i",                   # children indicator
  "elderly_disabled_i",           # combined indicator
 # "deductions_by_hh_size",        # deductions by HH size
  "expedited_i",                  # expedited service
  "cat_elig",                     # categorical eligibility
  "rawben_rel_max",
  "medical_deductions",
  "shelter_exp_by_hh_size",
  "utilities",
  "married",
  "shelter_to_gross_ratio",
  "homeless",
  "earned_by_hh_size",
  "unearned_by_hh_size",
  "gross_by_hh_size",
  "lf_composition",
  "percent_abawd",
  "n_income_types",
  "n_deduction_types",
  "unc_rawben_rel_max",
  #"months_since_cert_n",
  "count_divisible_by_100"
)

# ── DEFINE THE TARGET ─────────────────────────────────────────────────────────
# is_error: TRUE  = genuine error, the flag was right, KEEP for review
#           FALSE = clean, the flag was a false positive, candidate to EXCLUDE
# Edit this one expression to match how your data records a QC finding.
TARGET_IS_ERROR <- quote(!is.na(over_threshold) & over_threshold != 0)

# Dollars-at-stake column. REQUIRED when OBJECTIVE == "dollars".
# Magnitudes are used (abs), so over- and under-issuance both count as value.
ERR_AMT_COL <- "total_error_amount"

# Actionability filters for the SHORTLIST (full table is unfiltered).
MIN_WORKLOAD      <- 0.05   # a rule must remove at least 0.5% of the pile
MIN_PURITY        <- 0.90    # counts:  >=95% of removed cases must be clean
MIN_DOLLAR_RECALL <- 0.95    # dollars: a rule may sacrifice <=1% of error $ on its own

# Exclusion NET: greedily OR together a few RuleFit rules to cut as much workload
# as possible at each level of recall. Each rule already carries 2-3 variables
# (set by maxdepth). The net is reported at these recall floors.
NET_FLOORS       <- c(1.00, 0.99, 0.98, 0.95, 0.90)
NET_MIN_TRACE    <- 0.7     # stop extending the net below this recall
NET_EPS          <- 1        # smoothing so zero-cost rules score as "free" workload

out_dir <- "review_precision_rulefit"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

RF_PARAMS <- list(ntrees = 5000, maxdepth = 3L, learnrate = 0.001, type = "rules")

# Lasso penalty for selecting rules. "lambda.1se" is sparse (fewer, sturdier
# rules); "lambda.min" keeps more (use when 1se returns nothing). The script
# falls back from 1se to min automatically if 1se selects no rules.
PENALTY <- "lambda.1se"

stopifnot(OBJECTIVE %in% c("counts", "dollars"))

## ── 1. Helpers ────────────────────────────────────────────────────────────────

count_conditions <- function(x)
  vapply(gregexpr("&", x, fixed = TRUE), function(m) sum(m > 0) + 1L, integer(1))

# Evaluate a {pre} rule description as a logical flag over `data`; NA -> not matched.
flag_rule <- function(rule_desc, data) {
  out <- tryCatch(with(data, eval(parse(text = rule_desc))),
                  error = function(e) rep(NA, nrow(data)))
  out[is.na(out)] <- FALSE
  as.logical(out)
}

# Performance of a single EXCLUSION flag (TRUE = case would be dropped).
# Computes BOTH the count view and the dollar view. `err_dollars` is recoverable
# error $ per case (0 for clean); pass all-NA if dollars are unavailable.
exclusion_perf <- function(excl, is_error, err_dollars = NULL) {
  N         <- length(excl)
  n_excl    <- sum(excl)
  n_ret     <- N - n_excl
  total_err <- sum(is_error)
  base_prec <- total_err / N
  
  errors_lost     <- sum(excl & is_error)
  clean_excluded  <- sum(excl & !is_error)
  errors_retained <- sum(!excl & is_error)
  ret_prec        <- if (n_ret > 0) errors_retained / n_ret else NA_real_
  
  has_d <- !is.null(err_dollars) && any(!is.na(err_dollars))
  if (has_d) {
    ed <- err_dollars; ed[is.na(ed)] <- 0
    dollars_total    <- sum(ed)
    dollars_lost     <- sum(ed[excl])
    dollars_retained <- dollars_total - dollars_lost
    base_density     <- dollars_total / N
    ret_density      <- if (n_ret > 0) dollars_retained / n_ret else NA_real_
    mean_d_excl      <- if (n_excl > 0) dollars_lost / n_excl else NA_real_
    dollar_recall    <- if (dollars_total > 0) dollars_retained / dollars_total else NA_real_
  } else {
    dollars_lost <- base_density <- ret_density <- mean_d_excl <- dollar_recall <- NA_real_
  }
  
  tibble(
    # workload (shared)
    n_excluded              = n_excl,
    workload_cut_pct        = 100 * n_excl / N,
    n_retained              = n_ret,
    # count view
    clean_excluded          = clean_excluded,
    errors_lost             = errors_lost,
    exclusion_purity        = if (n_excl > 0) clean_excluded / n_excl else NA_real_,
    clean_per_error_lost    = if (errors_lost > 0) clean_excluded / errors_lost else Inf,
    retained_precision      = ret_prec,
    base_precision          = base_prec,
    precision_gain          = ret_prec - base_prec,
    recall_retained         = if (total_err > 0) errors_retained / total_err else NA_real_,
    # dollar view
    err_dollars_lost        = dollars_lost,
    mean_dollars_excluded   = mean_d_excl,
    retained_dollar_density = ret_density,
    base_dollar_density     = base_density,
    dollar_density_gain     = ret_density - base_density,
    dollar_recall_retained  = dollar_recall
  )
}

## ── 2. Prepare the pile ───────────────────────────────────────────────────────

is_error <- eval(TARGET_IS_ERROR, envir = flagged_cases)
is_error[is.na(is_error)] <- FALSE
flagged_cases$.is_error <- is_error

# Recoverable error dollars per case: |amount| for true errors, 0 for clean.
if (!is.na(ERR_AMT_COL) && ERR_AMT_COL %in% names(flagged_cases)) {
  raw_amt <- flagged_cases[[ERR_AMT_COL]]; raw_amt[is.na(raw_amt)] <- 0
  err_dollars_all <- ifelse(is_error, abs(raw_amt), 0)
} else {
  err_dollars_all <- rep(NA_real_, nrow(flagged_cases))
}
if (OBJECTIVE == "dollars" && all(is.na(err_dollars_all)))
  stop("OBJECTIVE = 'dollars' requires ERR_AMT_COL to be present in flagged_cases.")

N_total   <- nrow(flagged_cases)
base_prec <- mean(is_error)

cat(sprintf("\n=== Flagged pile  (objective: %s) ===\n", toupper(OBJECTIVE)))
cat(sprintf("  N flagged          : %d\n", N_total))
cat(sprintf("  True errors        : %d (%.1f%%)  <- base precision\n",
            sum(is_error), 100 * base_prec))
cat(sprintf("  Clean false-flags  : %d (%.1f%%)  <- room to cut\n",
            sum(!is_error), 100 * (1 - base_prec)))
if (!all(is.na(err_dollars_all)))
  cat(sprintf("  Error $ in pile    : $%s  ($%.0f avg per flagged case)\n",
              format(round(sum(err_dollars_all, na.rm = TRUE)), big.mark = ","),
              sum(err_dollars_all, na.rm = TRUE) / N_total))

# Keep predictors present and varying
pv <- features[features %in% names(flagged_cases)]
pv <- pv[sapply(flagged_cases[pv], function(x)
  !all(is.na(x)) && length(unique(x[!is.na(x)])) > 1)]
missing_feats <- setdiff(features, names(flagged_cases))
if (length(missing_feats))
  cat(sprintf("  NOTE: %d features not found and skipped: %s\n",
              length(missing_feats), paste(missing_feats, collapse = ", ")))

# glmnet step needs complete cases on modelled columns
model_cols <- c(".is_error", pv)
complete   <- stats::complete.cases(flagged_cases[model_cols])
model_data <- flagged_cases[complete, , drop = FALSE]
md_dollars <- err_dollars_all[complete]
cat(sprintf("  N (model)          : %d (dropped %d rows w/ NA in predictors)\n",
            nrow(model_data), sum(!complete)))

# {pre} (via ctree/partykit) needs numeric or factor inputs, not character/logical.
# Coerce, drop unused levels, then drop any predictor that became constant after
# complete-casing (a one-level factor or single-value column will error in the fit).
to_factor <- pv[vapply(model_data[pv],
                       function(x) is.character(x) || is.logical(x), logical(1))]
for (v in to_factor) model_data[[v]] <- factor(model_data[[v]])
model_data[pv] <- lapply(model_data[pv],
                         function(x) if (is.factor(x)) droplevels(x) else x)
keep <- vapply(model_data[pv], function(x) length(unique(x)) > 1, logical(1))
if (any(!keep)) {
  cat(sprintf("  NOTE: dropped %d now-constant predictor(s): %s\n",
              sum(!keep), paste(pv[!keep], collapse = ", ")))
  pv <- pv[keep]
}

## ── 3. Fit RuleFit (objective-dependent) ──────────────────────────────────────
# counts  -> classify clean vs error  (binomial)
# dollars -> regress recoverable error $ (gaussian); low-$ rules => exclusions

if (OBJECTIVE == "dollars") {
  model_data$.target <- md_dollars
  fam <- "gaussian"
} else {
  model_data$.target <- factor(ifelse(model_data$.is_error, "error", "clean"),
                               levels = c("error", "clean"))
  fam <- "binomial"
}

form <- as.formula(paste(".target ~", paste(pv, collapse = " + ")))

fit <- pre(
  formula   = form,
  data      = model_data[c(".target", pv)],
  family    = fam,
  ntrees    = RF_PARAMS$ntrees,
  maxdepth  = RF_PARAMS$maxdepth,
  learnrate = RF_PARAMS$learnrate,
  type      = RF_PARAMS$type,
  verbose = T, 
  tree.unbiased=F,
  use.grad=T,
  removeduplicates = T,
  removecomplements = T,
  nfolds = 5
)

get_rules <- function(pp)
  coef(fit, penalty.par.val = pp) %>%
  filter(rule != "(Intercept)", coefficient != 0)

rules0 <- get_rules(PENALTY)
if (nrow(rules0) == 0 && PENALTY == "lambda.1se") {
  cat("  No rules at lambda.1se -- retrying at lambda.min ...\n")
  PENALTY <- "lambda.min"
  rules0  <- get_rules(PENALTY)
}
if (nrow(rules0) == 0)
  stop("No rules at either penalty. This almost always means the TARGET is ",
       "(near-)constant. Check:  table(model_data$.is_error)  and  ",
       "summary(model_data$.target) . If the label looks wrong, fix ",
       "TARGET_IS_ERROR to match how flagged_cases encodes a confirmed error.")

imp <- pre::importance(fit, penalty.par.val = PENALTY, plot = FALSE)$baseimps
rules <- rules0 %>%
  left_join(select(imp, rule, imp), by = "rule") %>%
  rename(rule_id = rule, rule_text = description)

## ── 4. Evaluate every rule as an exclusion criterion ──────────────────────────
# Direction is decided empirically. counts: a rule is EXCLUDE if its matched cases
# are cleaner than the pile. dollars: EXCLUDE if its matched cases carry less
# recoverable $ per case than the pile average.

eval_one <- function(rd) {
  flag <- flag_rule(rd, model_data)
  perf <- exclusion_perf(flag, model_data$.is_error, md_dollars)
  if (OBJECTIVE == "dollars") {
    dens_in <- if (sum(flag) > 0) sum(md_dollars[flag]) / sum(flag) else NA_real_
    perf$role <- if (!is.na(dens_in) && dens_in < perf$base_dollar_density) "EXCLUDE" else "KEEP"
  } else {
    clean_in <- if (sum(flag) > 0) mean(!model_data$.is_error[flag]) else NA_real_
    perf$role <- if (!is.na(clean_in) && clean_in > (1 - base_prec)) "EXCLUDE" else "KEEP"
  }
  perf
}

rule_eval <- bind_rows(lapply(rules$rule_text, eval_one))

rule_table <- rules %>%
  bind_cols(rule_eval) %>%
  mutate(n_conditions = count_conditions(rule_text),
         coefficient  = round(coefficient, 3),
         importance   = round(imp, 3)) %>%
  transmute(
    rule_id, rule = rule_text, n_conditions, role, coefficient, importance,
    workload_cut_pct        = round(workload_cut_pct, 1),
    n_excluded, n_retained,
    # count view
    clean_excluded, errors_lost,
    exclusion_purity        = round(exclusion_purity, 3),
    clean_per_error_lost    = round(clean_per_error_lost, 1),
    retained_precision      = round(retained_precision, 3),
    base_precision          = round(base_precision, 3),
    precision_gain          = round(precision_gain, 3),
    recall_retained         = round(recall_retained, 3),
    # dollar view
    err_dollars_lost        = round(err_dollars_lost, 0),
    mean_dollars_excluded   = round(mean_dollars_excluded, 2),
    retained_dollar_density = round(retained_dollar_density, 2),
    base_dollar_density     = round(base_dollar_density, 2),
    dollar_density_gain     = round(dollar_density_gain, 2),
    dollar_recall_retained  = round(dollar_recall_retained, 3)
  ) %>%
  arrange(role, desc(workload_cut_pct))

cat("\n\n================= ALL SELECTED RULES =================\n")
print(as.data.frame(rule_table))
write.csv(rule_table, file.path(out_dir, "exclusion_rules_all.csv"), row.names = FALSE)

# Actionable shortlist: safe, non-trivial exclusions under the chosen objective
if (OBJECTIVE == "dollars") {
  shortlist <- rule_table %>%
    filter(role == "EXCLUDE",
           dollar_recall_retained >= MIN_DOLLAR_RECALL,
           workload_cut_pct / 100 >= MIN_WORKLOAD) %>%
    arrange(desc(n_excluded))
  cat(sprintf("\n=== SHORTLIST: dollar_recall >= %.2f, workload >= %.1f%% ===\n",
              MIN_DOLLAR_RECALL, 100 * MIN_WORKLOAD))
} else {
  shortlist <- rule_table %>%
    filter(role == "EXCLUDE",
           exclusion_purity >= MIN_PURITY,
           workload_cut_pct / 100 >= MIN_WORKLOAD) %>%
    arrange(desc(clean_excluded))
  cat(sprintf("\n=== SHORTLIST: purity >= %.2f, workload >= %.1f%% ===\n",
              MIN_PURITY, 100 * MIN_WORKLOAD))
}
print(as.data.frame(shortlist))
write.csv(shortlist, file.path(out_dir, "exclusion_rules_shortlist.csv"), row.names = FALSE)

## ── 5. Exclusion NET: greedily OR together RuleFit rules ──────────────────────
# RuleFit already gave us multi-variable rules. Here we build the "net": start
# excluding nothing, then repeatedly add the EXCLUDE-direction rule that removes
# the most clean cases per unit of protected value it risks (error $ under the
# dollar objective, error cases under counts). Union handles overlap. The path is
# the frontier; we read off the net at each recall floor.

pool <- rule_table %>% filter(role == "EXCLUDE") %>% pull(rule)
cat(sprintf("\n\n=== EXCLUSION NET  (candidate rules: %d) ===\n", length(pool)))

if (length(pool) == 0) {
  cat("No EXCLUDE-direction rules to build a net from.\n")
} else {
  ie       <- model_data$.is_error
  protect  <- if (OBJECTIVE == "dollars") {md <- md_dollars; md[is.na(md)] <- 0; md}
  else as.numeric(ie)            # value to protect: $ or error-cases
  val_tot  <- sum(protect)
  err_tot  <- sum(ie)
  N_m      <- nrow(model_data)
  flags    <- lapply(pool, flag_rule, data = model_data)
  
  # greedy forward selection
  excluded <- rep(FALSE, N_m); remaining <- seq_along(pool)
  path <- list(); step <- 0
  repeat {
    best <- NULL; best_score <- -Inf; best_new <- NULL
    for (k in remaining) {
      new_excl <- excluded | flags[[k]]
      newc     <- new_excl & !excluded
      d_cases  <- sum(newc); if (d_cases == 0) next
      d_cost   <- sum(protect[newc])           # $ or error-cases newly risked
      d_clean  <- sum(newc & !ie)              # clean cases newly removed
      score    <- d_clean / (d_cost + NET_EPS)
      if (score > best_score) { best_score <- score; best <- k; best_new <- new_excl }
    }
    if (is.null(best)) break
    excluded <- best_new; remaining <- setdiff(remaining, best); step <- step + 1
    cost_lost <- sum(protect[excluded]); err_lost <- sum(excluded & ie)
    path[[step]] <- tibble(
      step = step, rule_added = pool[best],
      n_excluded = sum(excluded), workload_cut_pct = 100 * sum(excluded) / N_m,
      recall_retained_obj    = (val_tot - cost_lost) / val_tot,
      dollar_recall_retained = if (!all(is.na(md_dollars)))
        (sum(md_dollars, na.rm = TRUE) - sum(md_dollars[excluded], na.rm = TRUE)) /
        sum(md_dollars, na.rm = TRUE) else NA_real_,
      recall_retained = (err_tot - err_lost) / err_tot,
      err_dollars_lost = if (!all(is.na(md_dollars))) sum(md_dollars[excluded], na.rm = TRUE) else NA_real_,
      errors_lost = err_lost)
    if (path[[step]]$recall_retained_obj < NET_MIN_TRACE || length(remaining) == 0) break
  }
  net_path <- bind_rows(path) %>%
    mutate(across(c(workload_cut_pct, recall_retained_obj, dollar_recall_retained,
                    recall_retained), ~ round(.x, 4)),
           err_dollars_lost = round(err_dollars_lost, 0))
  
  cat("\n-- frontier (each row ORs in one more rule) --\n")
  print(as.data.frame(net_path %>% select(step, n_excluded, workload_cut_pct,
                                          recall_retained_obj, errors_lost, rule_added)))
  write.csv(net_path, file.path(out_dir, "net_frontier_path.csv"), row.names = FALSE)
  
  # the net at each recall floor: most workload cut while retaining >= floor
  floor_col <- "recall_retained_obj"   # the objective's own recall ($ or counts)
  ops <- lapply(NET_FLOORS, function(fl) {
    ok <- net_path[net_path[[floor_col]] >= fl, , drop = FALSE]
    if (nrow(ok) == 0) return(NULL)
    pt <- ok[which.max(ok$workload_cut_pct), ]
    tibble(recall_floor = fl, workload_cut_pct = pt$workload_cut_pct,
           n_excluded = pt$n_excluded, recall_retained_obj = pt$recall_retained_obj,
           errors_lost = pt$errors_lost, err_dollars_lost = pt$err_dollars_lost,
           n_rules = pt$step,
           net = paste(net_path$rule_added[seq_len(pt$step)], collapse = "  OR  "))
  }) %>% bind_rows()
  
  cat("\n-- the net at each recall floor --\n")
  print(as.data.frame(ops %>% select(recall_floor, workload_cut_pct, n_excluded,
                                     recall_retained_obj, errors_lost, n_rules)))
  write.csv(ops, file.path(out_dir, "net_operating_points.csv"), row.names = FALSE)
  
  cat("\n-- rules in each net --\n")
  for (i in seq_len(nrow(ops)))
    cat(sprintf("\n  recall >= %.2f  ->  cut %.1f%% workload, exclude a case if it matches ANY of:\n    %s\n",
                ops$recall_floor[i], ops$workload_cut_pct[i],
                gsub("  OR  ", "\n    OR ", ops$net[i])))
}

## ── 6. SINGLE VARIABLE EXCLUSION TUNING ──────────────────────────────────────────
# IF YOU WANT TO TUNE AN ENTIRE RULE, USE: 5_gridsearch_optimize_exclusion_rules.R
# Function below is only for tuning a single-variable exclusion cutoff.
# counts -> recall_retained OR dollars -> # dollar_recall_retained. 
#
#   data       : the flagged pile (must contain .is_error)
#   var        : numeric predictor to sweep
#   direction  : "<=" excludes LOW values, ">=" excludes HIGH values


sweep_exclusion <- function(data, var, direction = c("<=", ">="),
                            grid = NULL, n_grid = 25, err_dollars = md_dollars) {
  direction <- match.arg(direction)
  x  <- data[[var]]
  ie <- data$.is_error
  if (is.null(grid))
    grid <- quantile(x, probs = seq(0.05, 0.95, length.out = n_grid), na.rm = TRUE)
  one <- function(g) {
    excl <- if (direction == "<=") x <= g else x >= g
    excl[is.na(excl)] <- FALSE
    cbind(cutoff = g, exclusion_perf(excl, ie, err_dollars))
  }
  out <- do.call(rbind, lapply(unname(grid), one))
  as_tibble(out) %>%
    transmute(variable = var, direction = direction, cutoff,
              workload_cut_pct, recall_retained, retained_precision,
              dollar_recall_retained, retained_dollar_density) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))
}

#Example
sweep_exclusion(model_data, var = "rawben_rel_max", direction = "<=") %>%
  print(n = 25)

