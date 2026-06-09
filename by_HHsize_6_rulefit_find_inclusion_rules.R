# ──────────────────────────────────────────────────────────────────────────────
# RuleFit for SNAP review targeting: INCLUSION criteria (i.e., flag cases for review)
#
# Goal: maximize PRECISION (share of flagged cases that are true errors) subject to
# a minimum RECALL. Recall can be:
#   "counts"   recall = error CASES caught / all error cases
#   "dollars"  recall = error DOLLARS caught / all error dollars
#
# The model is fit separately within each household-size stratum (1, 2, 3, 4, 5+);
# every output row is tagged with its hh_size.
#
# RuleFit ({pre}) mines rules with 2-5 variables each (user defined by maxdepth). We keep
# the INCLUDE-direction rules and greedily combine them using OR # into a "net" of 
# rules that most precisely capture cases up to some level of recall (called a "floor"). 
# ──────────────────────────────────────────────────────────────────────────────

library(pre)
library(dplyr)

set.seed(117)

## ── 0. Config ─────────────────────────────────────────────────────────────────
reg_model_data$rawearn
# `reg_model_data` here = the labelled universe of cases (true errors + clean).
earned_income_df <- reg_model_data %>%
  filter(error_status %in% c("earned_overissuance", "no_error")) %>%
  filter(fiscal_year %in% c("2022", "2023", "2024"))
table(earned_income_df$element, earned_income_df$error_status)
table(earned_income_df$over_threshold, earned_income_df$error_status)

unearned_income_df <- reg_model_data %>%
  filter(error_status %in% c("unearned_overissuance", "no_error")) %>%
  filter(fiscal_year %in% c("2022", "2023", "2024"))
table(unearned_income_df$element, unearned_income_df$error_status)
table(unearned_income_df$over_threshold, unearned_income_df$error_status)

underissuance_df <- reg_model_data %>%
  filter(error_status %in% c("underissuance", "no_error")) %>%
  filter(fiscal_year %in% c("2022", "2023", "2024"))
table(underissuance_df$element, underissuance_df$error_status)
table(underissuance_df$over_threshold, underissuance_df$error_status)

# change focal_df depending on what kind of error you are exploring
focal_df <- earned_income_df

OBJECTIVE <- "dollars"      # "counts" or "dollars"

features <- c(
  "cert_HH_size_FS_n", "children_i", "elderly_disabled_i", "total_deductions",
  "expedited_i", "cat_elig", "rawben_rel_max", "medical_deductions",
  "shelter_expenses", "utilities", "married", "homeless",
  "rawearn", "rawunearn", "rawgross",
  "percent_abawd", "unc_rawben_rel_max", #"n_income_types", "n_deduction_types",
  "months_since_cert_n", "count_divisible_by_100"
) 
focal_df$over_threshold <- as.integer(as.character(focal_df$over_threshold))
setdiff(features, names(focal_df))
TARGET_IS_ERROR <- quote(!is.na(over_threshold) & over_threshold == 1)
ERR_AMT_COL     <- "total_error_amount"

# Household-size stratification. cert_HH_size_FS_n is collapsed to 1, 2, 3, 4, 5+
# and dropped from the predictors (we stratify on it rather than model it).
HH_SIZE_COL <- "cert_HH_size_FS_n"
HH_LEVELS   <- c("1", "2", "3", "4", "5+")
hh_group_of <- function(n) { g <- pmin(n, 5); ifelse(g == 5, "5+", as.character(g)) }

# Individual-rule shortlist (informational; the net does not depend on it)
MIN_SUPPORT   <- 0.000005   # a rule must FLAG at least 0.0005% of cases (footprint, not recall)
MIN_PRECISION <- 0.20    # a rule is "high precision" on its own if >= this

# Inclusion NET: greedily OR rules to climb recall while holding precision high.
# Each value is a MINIMUM RECALL the net must reach. Under OBJECTIVE = "dollars"
# this is recall of TOTAL ERROR DOLLARS (error $ caught / total error $); under
# "counts" it is recall of error cases. Set a single value to enforce one floor.
NET_FLOORS    <- c(0.20, 0.30, .40)
NET_EPS       <- 1       # one clean case; smooths the value-per-clean score

out_dir <- "review_targeting_rulefit_full_data"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

PENALTY   <- "lambda.1se"   # falls back to lambda.min if 1se selects nothing

stopifnot(OBJECTIVE %in% c("counts", "dollars"))

## ── 1. Helpers ────────────────────────────────────────────────────────────────

count_conditions <- function(x)
  vapply(gregexpr("&", x, fixed = TRUE), function(m) sum(m > 0) + 1L, integer(1))

flag_rule <- function(rule_desc, data) {
  out <- tryCatch(with(data, eval(parse(text = rule_desc))),
                  error = function(e) rep(NA, nrow(data)))
  out[is.na(out)] <- FALSE
  as.logical(out)
}

# Performance of an INCLUSION flag (TRUE = case flagged for review).
inclusion_perf <- function(flag, is_error, err_dollars = NULL) {
  N <- length(flag); n_flag <- sum(flag); total_err <- sum(is_error)
  tp <- sum(flag & is_error); base_rate <- total_err / N
  has_d <- !is.null(err_dollars) && any(!is.na(err_dollars))
  if (has_d) {
    ed <- err_dollars; ed[is.na(ed)] <- 0
    dollars_total <- sum(ed); dollars_caught <- sum(ed[flag])
    dollar_recall  <- if (dollars_total > 0) dollars_caught / dollars_total else NA_real_
    dollar_density <- if (n_flag > 0) dollars_caught / n_flag else NA_real_
  } else dollars_caught <- dollar_recall <- dollar_density <- NA_real_
  tibble(
    n_flagged = n_flag, workload_pct = 100 * n_flag / N,
    errors_caught = tp, clean_flagged = n_flag - tp,
    precision = if (n_flag > 0) tp / n_flag else NA_real_,
    recall = if (total_err > 0) tp / total_err else NA_real_,
    lift = if (n_flag > 0 && base_rate > 0) (tp / n_flag) / base_rate else NA_real_,
    err_dollars_caught = dollars_caught, dollar_recall = dollar_recall,
    dollar_density_flagged = dollar_density, base_rate = base_rate
  )
}

## ── 2. Per-stratum pipeline: prepare, fit, evaluate, build the net ────────────
# Runs the whole analysis on one household-size subset and tags every output with
# hh_size. Returns NULL tables for a stratum too small or with no selected rules.

run_for_hh <- function(focal_df, hh_label) {
  
  is_error <- eval(TARGET_IS_ERROR, envir = focal_df)
  is_error[is.na(is_error)] <- FALSE
  focal_df$.is_error <- is_error
  
  if (!is.na(ERR_AMT_COL) && ERR_AMT_COL %in% names(focal_df)) {
    raw_amt <- focal_df[[ERR_AMT_COL]]; raw_amt[is.na(raw_amt)] <- 0
    err_dollars_all <- ifelse(is_error, abs(raw_amt), 0)
  } else err_dollars_all <- rep(NA_real_, nrow(focal_df))
  if (OBJECTIVE == "dollars" && all(is.na(err_dollars_all)))
    stop("OBJECTIVE = 'dollars' requires ERR_AMT_COL present in focal_df.")
  
  cat(sprintf("\n\n#################### HOUSEHOLD SIZE %s ####################\n", hh_label))
  cat(sprintf("  N = %d | errors = %d (%.1f%%) | clean = %d\n",
              nrow(focal_df), sum(is_error), 100 * mean(is_error), sum(!is_error)))
  
  # predictors present and varying, with the stratifier removed
  pv <- setdiff(features, HH_SIZE_COL)
  pv <- pv[pv %in% names(focal_df)]
  pv <- pv[sapply(focal_df[pv], function(x)
    !all(is.na(x)) && length(unique(x[!is.na(x)])) > 1)]
  
  model_cols <- c(".is_error", pv)
  complete   <- stats::complete.cases(focal_df[model_cols])
  model_data <- focal_df[complete, , drop = FALSE]
  cat(sprintf("  N (model) = %d (dropped %d NA rows)\n", nrow(model_data), sum(!complete)))
  
  # pre() needs numeric/factor inputs; coerce char/logical, drop now-constant cols
  to_factor <- pv[vapply(model_data[pv], function(x) is.character(x) || is.logical(x), logical(1))]
  for (v in to_factor) model_data[[v]] <- factor(model_data[[v]])
  model_data[pv] <- lapply(model_data[pv], function(x) if (is.factor(x)) droplevels(x) else x)
  pv <- pv[vapply(model_data[pv], function(x) length(unique(x)) > 1, logical(1))]
  
  empty <- list(rule_table = NULL, shortlist = NULL, net_path = NULL, ops = NULL)
  if (nrow(model_data) < 30 || length(pv) == 0) {
    cat("  too few rows or predictors; skipping this stratum\n")
    return(empty)
  }
  
  # Class rebalancing: keep all errors, sample 14 clean cases per error (capped at
  # the clean cases available, so thin strata use all of them rather than erroring).
  has_errors <- model_data %>% filter(over_threshold == "1" & fiscal_year > 2019)
  no_errors  <- model_data %>% filter(over_threshold == "0" & fiscal_year > 2019)
  n_clean    <- min(nrow(has_errors) * 14, nrow(no_errors))
  no_errors_sampled <- no_errors %>% sample_n(size = n_clean)
  model_data <- bind_rows(has_errors, no_errors_sampled)
  model_data <- model_data %>% sample_n(size = nrow(model_data))
  table(model_data$over_threshold)
  
  # md_dollars must be recomputed from the rebalanced rows
  amt <- model_data[[ERR_AMT_COL]]; amt[is.na(amt)] <- 0
  md_dollars <- ifelse(model_data$.is_error, abs(amt), 0)
  ie         <- model_data$.is_error
  base_rate  <- mean(ie)
  base_dens  <- if (!all(is.na(md_dollars))) sum(md_dollars) / nrow(model_data) else NA_real_
  
  if (OBJECTIVE == "dollars") {
    model_data$.target <- md_dollars; fam <- "gaussian"
  } else {
    model_data$.target <- factor(ifelse(ie, "error", "clean"),
                                 levels = c("error", "clean")); fam <- "binomial"
  }
  form <- as.formula(paste(".target ~", paste(pv, collapse = " + ")))
  
  fit <- pre(
    formula           = form,
    data              = model_data[c(".target", pv)],
    family            = fam,
    ntrees            = 10000,
    maxdepth          = 4L,
    learnrate         = 0.001,
    type              = "rules",
    use.grad          = T,
    tree.unbiased     = F,   # F is rpart, much faster than ctree, also seems to work better
    sampfrac          = .5,
    removeduplicates  = TRUE,
    removecomplements = TRUE,
    nfolds            = 5,
    randomForest      = F,
    #mtry            = 3,
    verbose           = TRUE
  )
  # 
  get_rules <- function(pp)
    coef(fit, penalty.par.val = pp) %>% filter(rule != "(Intercept)", coefficient != 0)
  penalty <- PENALTY
  rules0  <- get_rules(penalty)
  if (nrow(rules0) == 0 && penalty == "lambda.1se") {
    cat("  No rules at lambda.1se -- retrying at lambda.min ...\n")
    penalty <- "lambda.min"; rules0 <- get_rules(penalty)
  }
  if (nrow(rules0) == 0) {
    cat("  No rules at either penalty; skipping this stratum\n")
    return(empty)
  }
  imp <- pre::importance(fit, penalty.par.val = penalty, plot = FALSE)$baseimps
  rules <- rules0 %>% left_join(select(imp, rule, imp), by = "rule") %>%
    rename(rule_id = rule, rule_text = description)
  
  eval_one <- function(rd) {
    flag <- flag_rule(rd, model_data)
    perf <- inclusion_perf(flag, ie, md_dollars)
    if (OBJECTIVE == "dollars") {
      dens_in <- if (sum(flag) > 0) sum(md_dollars[flag]) / sum(flag) else NA_real_
      perf$role <- if (!is.na(dens_in) && dens_in > base_dens) "INCLUDE" else "SKIP"
    } else {
      prec_in <- if (sum(flag) > 0) mean(ie[flag]) else NA_real_
      perf$role <- if (!is.na(prec_in) && prec_in > base_rate) "INCLUDE" else "SKIP"
    }
    perf
  }
  rule_eval <- bind_rows(lapply(rules$rule_text, eval_one))
  
  rule_table <- rules %>% bind_cols(rule_eval) %>%
    mutate(n_conditions = count_conditions(rule_text),
           coefficient = round(coefficient, 3), importance = round(imp, 3)) %>%
    transmute(hh_size = hh_label,
              rule_id, rule = rule_text, n_conditions, role, coefficient, importance,
              workload_pct = round(workload_pct, 1), n_flagged, errors_caught, clean_flagged,
              precision = round(precision, 3), recall = round(recall, 3),
              dollar_recall = round(dollar_recall, 3),
              lift = round(lift, 2), base_rate = round(base_rate, 3)) %>%
    arrange(role, desc(precision))
  
  shortlist <- rule_table %>%
    filter(role == "INCLUDE", precision >= MIN_PRECISION, workload_pct / 100 >= MIN_SUPPORT) %>%
    arrange(desc(precision))
  
  # Inclusion NET: greedily OR INCLUDE-direction rules; earliest crossing of each
  # recall floor is the highest-precision net that still hits it.
  pool <- rule_table %>% filter(role == "INCLUDE") %>% pull(rule)
  cat(sprintf("  INCLUDE-direction candidate rules: %d\n", length(pool)))
  net_path <- NULL; ops <- NULL
  if (length(pool) > 0) {
    protect <- if (OBJECTIVE == "dollars") {m <- md_dollars; m[is.na(m)] <- 0; m} else as.numeric(ie)
    val_tot <- sum(protect); err_tot <- sum(ie); N_m <- nrow(model_data)
    doll_tot <- if (!all(is.na(md_dollars))) sum(md_dollars, na.rm = TRUE) else NA_real_
    flags <- lapply(pool, flag_rule, data = model_data)
    
    flagged <- rep(FALSE, N_m); remaining <- seq_along(pool); path <- list(); step <- 0
    repeat {
      best <- NULL; best_score <- -Inf; best_new <- NULL
      for (k in remaining) {
        new_flag <- flagged | flags[[k]]; newc <- new_flag & !flagged
        d_cases <- sum(newc); if (d_cases == 0) next
        d_value <- sum(protect[newc])          # error value newly captured
        d_clean <- sum(newc & !ie)             # clean cases newly flagged (precision cost)
        score   <- d_value / (d_clean + NET_EPS)
        if (score > best_score) { best_score <- score; best <- k; best_new <- new_flag }
      }
      if (is.null(best)) break
      flagged <- best_new; remaining <- setdiff(remaining, best); step <- step + 1
      nfl <- sum(flagged); tp <- sum(flagged & ie)
      path[[step]] <- tibble(
        hh_size = hh_label, step = step, rule_added = pool[best], n_flagged = nfl,
        workload_pct = 100 * nfl / N_m,
        precision = tp / nfl,
        recall_obj = sum(protect[flagged]) / val_tot,
        recall = tp / err_tot,
        dollar_recall = if (!is.na(doll_tot) && doll_tot > 0) sum(md_dollars[flagged], na.rm = TRUE) / doll_tot else NA_real_,
        errors_caught = tp)
      if (path[[step]]$recall_obj >= max(NET_FLOORS) || length(remaining) == 0) break
    }
    net_path <- bind_rows(path) %>%
      mutate(across(c(workload_pct, precision, recall_obj, recall, dollar_recall), ~ round(.x, 4)))
    
    ops <- lapply(NET_FLOORS, function(fl) {
      ok <- net_path[net_path$recall_obj >= fl, , drop = FALSE]
      if (nrow(ok) == 0) return(NULL)
      pt <- ok[which.min(ok$step), ]
      tibble(hh_size = hh_label, recall_floor = fl, precision = pt$precision, recall_obj = pt$recall_obj,
             n_flagged = pt$n_flagged, workload_pct = pt$workload_pct,
             errors_caught = pt$errors_caught, n_rules = pt$step,
             net = paste(net_path$rule_added[seq_len(pt$step)], collapse = "  OR  "))
    }) %>% bind_rows()
  }
  
  list(rule_table = rule_table, shortlist = shortlist, net_path = net_path, ops = ops)
}

## ── 3. Run every household-size stratum and combine ───────────────────────────
 
groups  <- hh_group_of(focal_df[[HH_SIZE_COL]])
results <- lapply(HH_LEVELS, function(lab)
  run_for_hh(focal_df[!is.na(groups) & groups == lab, , drop = FALSE], lab))

rule_table_all <- bind_rows(lapply(results, `[[`, "rule_table"))
shortlist_all  <- bind_rows(lapply(results, `[[`, "shortlist"))
net_path_all   <- bind_rows(lapply(results, `[[`, "net_path"))
ops_all        <- bind_rows(lapply(results, `[[`, "ops"))

cat("\n\n================= ALL SELECTED RULES (by household size) =================\n")
print(as.data.frame(rule_table_all))
write.csv(rule_table_all, file.path(out_dir, "by_HHsize_inclusion_rules_all.csv"), row.names = FALSE)

cat("\n\n================= HIGH-PRECISION RULES (by household size) =================\n")
print(as.data.frame(shortlist_all))
write.csv(shortlist_all, file.path(out_dir, "by_HHsize_inclusion_rules_highprecision.csv"), row.names = FALSE)

write.csv(net_path_all, file.path(out_dir, "by_HHsize_net_frontier_path.csv"), row.names = FALSE)

cat("\n\n================= NET OPERATING POINTS (by household size) =================\n")
print(as.data.frame(ops_all %>% select(hh_size, recall_floor, precision, recall_obj,
                                       n_flagged, workload_pct, errors_caught, n_rules)))
write.csv(ops_all, file.path(out_dir, "HHsize_net_operating_points.csv"), row.names = FALSE)

cat("\n-- rules in each net (by household size) --\n")
for (i in seq_len(nrow(ops_all)))
  cat(sprintf("\n  [HH %s]  recall >= %.2f  ->  precision %.2f, flag %.1f%% of cases, FLAG a case if it matches ANY of:\n    %s\n",
              ops_all$hh_size[i], ops_all$recall_floor[i], ops_all$precision[i], ops_all$workload_pct[i],
              gsub("  OR  ", "\n    OR ", ops_all$net[i])))
