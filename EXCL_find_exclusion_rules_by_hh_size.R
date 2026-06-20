# ──────────────────────────────────────────────────────────────────────────────
# RuleFit for SNAP review-precision: finding efficient EXCLUSION criteria
#
# Setting: a state agency already has a pile of cases it flagged for review
# (`flagged_cases`). That pile mixes TRUE errors (worth a reviewer's time) with
# false positives. The goal is to find simple feature/threshold
# rules that let the agency safely exclude cases, leaving a more targeted set.
#
# The model is fit separately within each household-size stratum (1, 2-3, 4+);
# every output row is tagged with its hh_size.
#
# OBJECTIVE TOGGLE  (set OBJECTIVE in Section 0):
#   "counts"   maximize PRECISION  = share of the retained pile that are errors.
#              Safety = don't drop true error CASES.  Cost = errors_lost (count).
#   "dollars"  maximize DOLLAR DENSITY = recoverable error $ per retained case.
#              Safety = don't drop error DOLLARS.      Cost = err_dollars_lost.
#
# The toggle decides which model is fit, how EXCLUDE/KEEP is called, how the shortlist
# is filtered/sorted, and which headline the portfolio prints. 
# Both counts and dollars will be computed either way.
#
# RuleFit implementation: {pre} (Prediction Rule Ensembles; Fokkema 2020, JSS).
# ──────────────────────────────────────────────────────────────────────────────

library(pre)
library(dplyr)

set.seed(111)

#dataset to be cleaned up - here it's very generic - starting with national data for FY22-24
flagged_cases <- reg_model_data %>% filter(fiscal_year>2019 & state=="Michigan")

## ── 0. Config ─────────────────────────────────────────────────────────────────

# `flagged_cases` is expected in the environment: the agency's already-prioritized
# review pile (true errors + clean false-flags together).

OBJECTIVE <- "dollars"      # "counts" or "dollars"

features <- c(
  "cert_HH_size_FS_n",            # certified household size (the stratifier)
  "children_i",                   # children indicator
  "elderly_disabled_i",           # combined indicator
  "total_deductions",        # deductions by HH size
  "expedited_i",                  # expedited service
  "cat_elig",                     # categorical eligibility
  "rawben_rel_max",
  "medical_deductions",
  "shelter_expenses",
  "utilities",
  "married",
  "shelter_to_gross_ratio",
  "homeless",
  "rawearn",
  "rawunearn",
  "rawgrinc",
  "lf_composition",
  "percent_abawd",
  "n_income_types",
  "n_deduction_types",
  "unc_rawben_rel_max",
  "months_since_cert_n",
  "count_divisible_by_100"
)

# Household-size stratification: cert_HH_size_FS_n collapsed to 1, 2-3, 4+.
HH_SIZE_COL <- "cert_HH_size_FS_n"
HH_LEVELS   <- c("1", "2-3", "4+")
hh_group_of <- function(n) { ifelse(n <= 1, "1", ifelse(n <= 3, "2-3", "4+")) }

# ── DEFINE THE TARGET ─────────────────────────────────────────────────────────
# is_error: TRUE  = genuine error, the flag was right, KEEP for review
#           FALSE = clean, the flag was a false positive, candidate to EXCLUDE
# Edit this one expression to match how your data records a QC finding.
TARGET_IS_ERROR <- quote(!is.na(over_threshold) & over_threshold != 0)

# Dollars-at-stake column. REQUIRED when OBJECTIVE == "dollars".
# Magnitudes are used (abs), so over- and under-issuance both count as value.
ERR_AMT_COL <- "total_error_amount"

# Actionability filters for the SHORTLIST (full table is unfiltered).
MIN_WORKLOAD      <- 0.05   # a rule must remove at least 5% of the pile
MIN_PURITY        <- 0.90   # counts:  >=90% of removed cases must be clean
MIN_DOLLAR_RECALL <- 0.95   # dollars: a rule may sacrifice <=5% of error $ on its own

# Exclusion NET: greedily OR together a few RuleFit rules to cut as much workload
# as possible at each level of recall. Each rule already carries 2-3 variables
# (set by maxdepth). The net is reported at these recall floors.
NET_FLOORS       <- c(1.00, 0.99, 0.98, 0.95, 0.90)
NET_MIN_TRACE    <- 0.7     # stop extending the net below this recall
NET_EPS          <- 1        # smoothing so zero-cost rules score as "free" workload

out_dir <- "exclusion_rules"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

RF_PARAMS <- list(ntrees = 2500, maxdepth = 4L, type = "rules",
                  learnrate = 0.01, use.grad = TRUE,
                  tree.unbiased     = FALSE,        
                  randomForest=F, sampfrac=0.5)

# Lasso penalty for selecting rules. "lambda.1se" is sparse (fewer, sturdier
# rules); "lambda.min" keeps more (use when 1se returns nothing). Each stratum
# falls back from 1se to min on its own if 1se selects no rules.
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

## ── 2. Per-stratum pipeline: prepare, fit, evaluate, build the net ────────────
# Runs the whole analysis on one household-size subset and tags every output with
# hh_size. Returns NULL tables for a stratum too small or with no selected rules.

run_for_hh <- function(flagged_cases, hh_label) {
  
  is_error <- eval(TARGET_IS_ERROR, envir = flagged_cases)
  is_error[is.na(is_error)] <- FALSE
  flagged_cases$.is_error <- is_error
  
  if (!is.na(ERR_AMT_COL) && ERR_AMT_COL %in% names(flagged_cases)) {
    raw_amt <- flagged_cases[[ERR_AMT_COL]]; raw_amt[is.na(raw_amt)] <- 0
    err_dollars_all <- ifelse(is_error, abs(raw_amt), 0)
  } else {
    err_dollars_all <- rep(NA_real_, nrow(flagged_cases))
  }
  if (OBJECTIVE == "dollars" && all(is.na(err_dollars_all)))
    stop("OBJECTIVE = 'dollars' requires ERR_AMT_COL to be present in flagged_cases.")
  
  base_prec <- mean(is_error)
  cat(sprintf("\n\n#################### HOUSEHOLD SIZE %s ####################\n", hh_label))
  cat(sprintf("  N flagged          : %d\n", nrow(flagged_cases)))
  cat(sprintf("  True errors        : %d (%.1f%%)\n", sum(is_error), 100 * base_prec))
  cat(sprintf("  Clean false-flags  : %d (%.1f%%)\n", sum(!is_error), 100 * (1 - base_prec)))
  
  # predictors present and varying, with the stratifier removed
  pv <- setdiff(features, HH_SIZE_COL)
  pv <- pv[pv %in% names(flagged_cases)]
  pv <- pv[sapply(flagged_cases[pv], function(x)
    !all(is.na(x)) && length(unique(x[!is.na(x)])) > 1)]
  
  # glmnet step needs complete cases on modelled columns
  model_cols <- c(".is_error", pv)
  complete   <- stats::complete.cases(flagged_cases[model_cols])
  model_data <- flagged_cases[complete, , drop = FALSE]
  md_dollars <- err_dollars_all[complete]
  cat(sprintf("  N (model)          : %d (dropped %d rows w/ NA in predictors)\n",
              nrow(model_data), sum(!complete)))
  
  # {pre} needs numeric/factor inputs; coerce, drop unused levels and constants
  to_factor <- pv[vapply(model_data[pv],
                         function(x) is.character(x) || is.logical(x), logical(1))]
  for (v in to_factor) model_data[[v]] <- factor(model_data[[v]])
  model_data[pv] <- lapply(model_data[pv],
                           function(x) if (is.factor(x)) droplevels(x) else x)
  pv <- pv[vapply(model_data[pv], function(x) length(unique(x)) > 1, logical(1))]
  
  empty <- list(rule_table = NULL, shortlist = NULL, net_path = NULL, ops = NULL)
  if (nrow(model_data) < 30 || length(pv) == 0) {
    cat("  too few rows or predictors; skipping this stratum\n")
    return(empty)
  }
  
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
    formula           = form,
    data              = model_data[c(".target", pv)],
    family            = fam,
    ntrees            = RF_PARAMS$ntrees,
    maxdepth          = RF_PARAMS$maxdepth,
    learnrate         = RF_PARAMS$learnrate,
    type              = RF_PARAMS$type,
    sampfrac = RF_PARAMS$sampfrac,
    verbose           = TRUE,
    tree.unbiased     = FALSE,
    use.grad          = TRUE,
    removeduplicates  = TRUE,
    removecomplements = TRUE,
    nfolds            = 5
  )
  
  get_rules <- function(pp)
    coef(fit, penalty.par.val = pp) %>%
    filter(rule != "(Intercept)", coefficient != 0)
  
  penalty <- PENALTY
  rules0  <- get_rules(penalty)
  if (nrow(rules0) == 0 && penalty == "lambda.1se") {
    cat("  No rules at lambda.1se -- retrying at lambda.min ...\n")
    penalty <- "lambda.min"
    rules0  <- get_rules(penalty)
  }
  if (nrow(rules0) == 0) {
    cat("  No rules at either penalty; skipping this stratum\n")
    return(empty)
  }
  
  imp <- pre::importance(fit, penalty.par.val = penalty, plot = FALSE)$baseimps
  rules <- rules0 %>%
    left_join(select(imp, rule, imp), by = "rule") %>%
    rename(rule_id = rule, rule_text = description)
  
  # Direction is decided empirically. counts: EXCLUDE if matched cases are cleaner
  # than the stratum. dollars: EXCLUDE if matched cases carry less $ per case.
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
      hh_size = hh_label,
      rule_id, rule = rule_text, n_conditions, role, coefficient, importance,
      workload_cut_pct        = round(workload_cut_pct, 1),
      n_excluded, n_retained,
      clean_excluded, errors_lost,
      exclusion_purity        = round(exclusion_purity, 3),
      clean_per_error_lost    = round(clean_per_error_lost, 1),
      retained_precision      = round(retained_precision, 3),
      base_precision          = round(base_precision, 3),
      precision_gain          = round(precision_gain, 3),
      recall_retained         = round(recall_retained, 3),
      err_dollars_lost        = round(err_dollars_lost, 0),
      mean_dollars_excluded   = round(mean_dollars_excluded, 2),
      retained_dollar_density = round(retained_dollar_density, 2),
      base_dollar_density     = round(base_dollar_density, 2),
      dollar_density_gain     = round(dollar_density_gain, 2),
      dollar_recall_retained  = round(dollar_recall_retained, 3)
    ) %>%
    arrange(role, desc(workload_cut_pct))
  
  if (OBJECTIVE == "dollars") {
    shortlist <- rule_table %>%
      filter(role == "EXCLUDE",
             dollar_recall_retained >= MIN_DOLLAR_RECALL,
             workload_cut_pct / 100 >= MIN_WORKLOAD) %>%
      arrange(desc(n_excluded))
  } else {
    shortlist <- rule_table %>%
      filter(role == "EXCLUDE",
             exclusion_purity >= MIN_PURITY,
             workload_cut_pct / 100 >= MIN_WORKLOAD) %>%
      arrange(desc(clean_excluded))
  }
  
  # Exclusion NET: greedily OR EXCLUDE-direction rules, reading off each floor.
  pool <- rule_table %>% filter(role == "EXCLUDE") %>% pull(rule)
  cat(sprintf("  EXCLUDE-direction candidate rules: %d\n", length(pool)))
  net_path <- NULL; ops <- NULL
  if (length(pool) > 0) {
    ie       <- model_data$.is_error
    protect  <- if (OBJECTIVE == "dollars") {md <- md_dollars; md[is.na(md)] <- 0; md} else as.numeric(ie)
    val_tot  <- sum(protect)
    err_tot  <- sum(ie)
    N_m      <- nrow(model_data)
    flags    <- lapply(pool, flag_rule, data = model_data)
    
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
        hh_size = hh_label, step = step, rule_added = pool[best],
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
    
    ops <- lapply(NET_FLOORS, function(fl) {
      ok <- net_path[net_path$recall_retained_obj >= fl, , drop = FALSE]
      if (nrow(ok) == 0) return(NULL)
      pt <- ok[which.max(ok$workload_cut_pct), ]
      tibble(hh_size = hh_label, recall_floor = fl, workload_cut_pct = pt$workload_cut_pct,
             n_excluded = pt$n_excluded, recall_retained_obj = pt$recall_retained_obj,
             errors_lost = pt$errors_lost, err_dollars_lost = pt$err_dollars_lost,
             n_rules = pt$step,
             net = paste(net_path$rule_added[seq_len(pt$step)], collapse = "  OR  "))
    }) %>% bind_rows()
  }
  
  list(rule_table = rule_table, shortlist = shortlist, net_path = net_path, ops = ops)
}

## ── 3. Run every household-size stratum and combine ───────────────────────────

groups  <- hh_group_of(flagged_cases[[HH_SIZE_COL]])
results <- lapply(HH_LEVELS, function(lab)
  run_for_hh(flagged_cases[!is.na(groups) & groups == lab, , drop = FALSE], lab))

rule_table_all <- bind_rows(lapply(results, `[[`, "rule_table"))
shortlist_all  <- bind_rows(lapply(results, `[[`, "shortlist"))
net_path_all   <- bind_rows(lapply(results, `[[`, "net_path"))
ops_all        <- bind_rows(lapply(results, `[[`, "ops"))

cat("\n\n================= ALL SELECTED RULES (by household size) =================\n")
print(as.data.frame(rule_table_all))
write.csv(rule_table_all, file.path(out_dir, "exclusion_rules_by_hh_size_all.csv"), row.names = FALSE)

cat("\n\n================= SHORTLIST (by household size) =================\n")
print(as.data.frame(shortlist_all))
write.csv(shortlist_all, file.path(out_dir, "exclusion_rules_by_hh_size_shortlist.csv"), row.names = FALSE)

write.csv(net_path_all, file.path(out_dir, "exclusion_rules_by_hh_size_net_frontier_path.csv"), row.names = FALSE)

cat("\n\n================= NET OPERATING POINTS (by household size) =================\n")
print(as.data.frame(ops_all %>% select(hh_size, recall_floor, workload_cut_pct,
                                       n_excluded, recall_retained_obj, errors_lost, n_rules)))
write.csv(ops_all, file.path(out_dir, "exclusion_rules_by_hh_size_net_operating_points.csv"), row.names = FALSE)

cat("\n-- rules in each net (by household size) --\n")
for (i in seq_len(nrow(ops_all)))
  cat(sprintf("\n  [HH %s]  recall >= %.2f  ->  cut %.1f%% workload, exclude a case if it matches ANY of:\n    %s\n",
              ops_all$hh_size[i], ops_all$recall_floor[i], ops_all$workload_cut_pct[i],
              gsub("  OR  ", "\n    OR ", ops_all$net[i])))


