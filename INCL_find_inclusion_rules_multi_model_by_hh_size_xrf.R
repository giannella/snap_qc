# v1 (RuleFit/{pre}-based). Still supported; documented in the README legacy section.
# Recommended successor: INCL_find_inclusion_rules_by_hh_size_v2.R — see README "Migrating from v1 to v2".
# ──────────────────────────────────────────────────────────────────────────────
# RuleFit for SNAP review targeting: INCLUSION criteria — xrf (xgboost) variant
#
# Goal: maximize PRECISION (share of flagged cases that are true errors) subject to
# a minimum RECALL. Recall can be:
#   "counts"   recall = error CASES caught / all error cases
#   "dollars"  recall = error DOLLARS caught / all error dollars
#
# The model is fit separately within each household-size stratum (1, 2-3, 4+);
# every output row is tagged with its hh_size.
#
# Script loops through types of errors, builds rules for each type, combines them
# into a single rule list. If you do not have types of errors, delete the loop or
# comment out one of the data frames.
#
# Uses {xrf} (eXtreme RuleFit) instead of {pre}: xgboost grows the trees, glmnet
# selects rules. All downstream net-building logic is identical to the pre version.
# ──────────────────────────────────────────────────────────────────────────────

library(xrf)
library(dplyr)

set.seed(117)

## ── 0. Config ─────────────────────────────────────────────────────────────────

# `reg_model_data` here = the labelled universe of cases (true errors + clean).
earned_income_df <- reg_model_data %>%
  filter(error_status %in% c("earned_overissuance", "no_error")) %>%
  filter(fiscal_year %in% c("2022", "2023", "2024"))
table(earned_income_df$element, earned_income_df$error_status)
table(earned_income_df$over_threshold, earned_income_df$error_status)
table(earned_income_df$over_threshold, earned_income_df$error_status, earned_income_df$HH_size_n)

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

# Rule MINING objective for the xrf fit. Decoupled from OBJECTIVE: mining under
# "counts" (binomial) is more robust when errors are scarce, while OBJECTIVE still
# governs the net and recall floors (here, dollar recall).
FIT_OBJECTIVE <- "counts"   # "counts" or "dollars"

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

# Household-size stratification: cert_HH_size_FS_n collapsed to 1, 2-3, 4+.
HH_SIZE_COL <- "cert_HH_size_FS_n"
HH_LEVELS   <- c("1", "2-3", "4+")
hh_group_of <- function(n) { ifelse(n <= 1, "1", ifelse(n <= 3, "2-3", "4+")) }

# Individual-rule shortlist (informational; the net does not depend on it)
MIN_SUPPORT   <- 0.000005   # a rule must FLAG at least 0.0005% of cases
MIN_PRECISION <- 0.20       # a rule is "high precision" on its own if >= this

# Inclusion NET: greedily OR rules to climb recall while holding precision high.
NET_FLOORS    <- c(0.20, 0.30, .40, .50)
NET_EPS       <- 1       # one clean case; smooths the value-per-clean score

out_dir <- "inclusion_rules_by_hh_size"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

PENALTY   <- "lambda.min"   # falls back to lambda.min if 1se selects nothing

stopifnot(OBJECTIVE %in% c("counts", "dollars"))
stopifnot(FIT_OBJECTIVE %in% c("counts", "dollars"))

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

ERROR_TAG <- paste(sort(setdiff(unique(as.character(focal_df$error_status)), "no_error")),
                   collapse = "_")

out_file <- function(stem) file.path(out_dir, sprintf("xrf_%s_%s.csv", ERROR_TAG, stem))

.dir_of <- function(op) if (op %in% c("<", "<=")) "upper" else if (op %in% c(">", ">=")) "lower" else "eq"

.parse_cond <- function(cond) {
  cond <- trimws(cond)
  m <- regmatches(cond, regexec("^(.*?)\\s*(>=|<=|==|>|<)\\s*(-?[0-9]*\\.?[0-9]+)\\s*$", cond))[[1]]
  if (length(m) == 4)
    list(type = "num", var = trimws(m[2]), op = m[3], dir = .dir_of(m[3]),
         thr = as.numeric(m[4]), raw = cond)
  else
    list(type = "other", raw = cond)
}
.split_rule <- function(rule) lapply(strsplit(rule, " & ", fixed = TRUE)[[1]], .parse_cond)

# (1) collapse repeated same-variable, same-direction bounds to the binding one
simplify_rule <- function(rule) {
  conds <- .split_rule(rule)
  keep  <- rep(TRUE, length(conds))
  num   <- which(vapply(conds, function(c) c$type == "num" && c$dir != "eq", logical(1)))
  if (length(num) > 0) {
    grp <- vapply(num, function(i) paste(conds[[i]]$var, conds[[i]]$dir), character(1))
    for (g in unique(grp)) {
      idx <- num[grp == g]
      if (length(idx) < 2) next
      dir    <- conds[[idx[1]]]$dir
      thr    <- vapply(idx, function(i) conds[[i]]$thr, numeric(1))
      strict <- vapply(idx, function(i) conds[[i]]$op %in% c("<", ">"), logical(1))
      ord  <- if (dir == "upper") order(thr, !strict) else order(-thr, !strict)
      keep[setdiff(idx, idx[ord[1]])] <- FALSE
    }
  }
  raws <- vapply(conds, function(c) c$raw, character(1))
  keep <- keep & !duplicated(raws)
  paste(raws[keep], collapse = " & ")
}

.rule_struct <- function(rule) {
  conds <- .split_rule(rule)
  num <- Filter(function(c) c$type == "num",   conds)
  oth <- vapply(Filter(function(c) c$type == "other", conds), function(c) c$raw, character(1))
  keys <- vapply(num, function(c) paste0(c$var, "|", c$op), character(1))
  list(keys = keys,
       thr = setNames(vapply(num, function(c) c$thr, numeric(1)), keys),
       dir = setNames(vapply(num, function(c) c$dir, character(1)), keys),
       sig = paste(c(sort(keys), sort(oth)), collapse = " ;; "))
}

.is_superset <- function(a, b) {
  if (a$sig != b$sig || length(a$keys) == 0) return(FALSE)
  any_strict <- FALSE
  for (k in a$keys) {
    at <- a$thr[[k]]; bt <- b$thr[[k]]; d <- a$dir[[k]]
    if (d == "upper") { if (at < bt) return(FALSE); if (at > bt) any_strict <- TRUE }
    else if (d == "lower") { if (at > bt) return(FALSE); if (at < bt) any_strict <- TRUE }
    else if (at != bt) return(FALSE)
  }
  any_strict
}

tidy_rules <- function(rules) {
  if (nrow(rules) == 0) return(rules)
  rules$rule_text <- vapply(rules$rule_text, simplify_rule, character(1))

  imp_vec <- if ("imp" %in% names(rules)) rules$imp else rep(0, nrow(rules))
  rules <- rules[order(-ifelse(is.na(imp_vec), -Inf, imp_vec)), , drop = FALSE]
  rules <- rules[!duplicated(rules$rule_text), , drop = FALSE]

  structs <- lapply(rules$rule_text, .rule_struct)
  sig  <- vapply(structs, function(s) s$sig, character(1))
  drop <- rep(FALSE, nrow(rules))
  for (g in unique(sig)) {
    ix <- which(sig == g)
    if (length(ix) < 2) next
    for (a in ix) {
      if (drop[a]) next
      for (b in ix) {
        if (a != b && .is_superset(structs[[a]], structs[[b]])) { drop[a] <- TRUE; break }
      }
    }
  }
  rules[!drop, , drop = FALSE]
}


## ── 2. Per-stratum pipeline: prepare, fit, evaluate, build the net ────────────
form <- as.formula(paste(".target ~", paste(features, collapse = " + ")))


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

  pv <- setdiff(features, HH_SIZE_COL)
  pv <- pv[pv %in% names(focal_df)]
  pv <- pv[sapply(focal_df[pv], function(x)
    !all(is.na(x)) && length(unique(x[!is.na(x)])) > 1)]

  model_cols <- c(".is_error", pv)
  complete   <- stats::complete.cases(focal_df[model_cols])
  model_data <- focal_df[complete, , drop = FALSE]
  cat(sprintf("  N (model) = %d (dropped %d NA rows)\n", nrow(model_data), sum(!complete)))

  # xrf passes data through model.matrix internally so no manual factor coercion needed;
  # just drop now-constant predictors.
  pv <- pv[vapply(model_data[pv], function(x) length(unique(x)) > 1, logical(1))]

  empty <- list(rule_table = NULL, shortlist = NULL, net_path = NULL, ops = NULL)
  if (nrow(model_data) < 30 || length(pv) == 0) {
    cat("  too few rows or predictors; skipping this stratum\n")
    return(empty)
  }

  # Optional class rebalancing (off by default; uncomment to re-enable): keep all
  # errors, sample 14 clean cases per error.
  # has_errors <- model_data %>% filter(over_threshold == "1" & fiscal_year > 2019)
  # no_errors  <- model_data %>% filter(over_threshold == "0" & fiscal_year > 2019)
  # n_clean    <- min(nrow(has_errors) * 14, nrow(no_errors))
  # no_errors_sampled <- no_errors %>% sample_n(size = n_clean)
  # model_data <- bind_rows(has_errors, no_errors_sampled)
  # model_data <- model_data %>% sample_n(size = nrow(model_data))
  # table(model_data$over_threshold)

  amt <- model_data[[ERR_AMT_COL]]; amt[is.na(amt)] <- 0
  md_dollars <- ifelse(model_data$.is_error, abs(amt), 0)
  ie         <- model_data$.is_error
  base_rate  <- mean(ie)
  base_dens  <- if (!all(is.na(md_dollars))) sum(md_dollars) / nrow(model_data) else NA_real_

  if (FIT_OBJECTIVE == "dollars") {
    model_data$.target <- md_dollars; fam <- "gaussian"
  } else {
    model_data$.target <- as.integer(ie); fam <- "binomial"
  }
  form <- as.formula(paste(".target ~", paste(pv, collapse = " + ")))

  n_err <- sum(ie)
  md <- if (n_err < 500) 3L else 4L
  cat(sprintf("  max_depth = %d (errors = %d)\n", md, n_err))

  # ── xrf fit ──────────────────────────────────────────────────────────────────
  # xgb_control maps directly to pre() params:
  #   nrounds   ≈ ntrees      (total boosting rounds / trees)
  #   eta       ≈ learnrate
  #   subsample ≈ sampfrac
  #   max_depth ≈ maxdepth
  fit <- xrf(
    object     = form,
    data        = model_data[c(".target", pv)],
    family      = fam,
    xgb_control = list(
      nrounds           = 300,
      max_depth         = md,
      eta               = 0.02,
      subsample         = 0.5,
      colsample_bytree  = 0.8,
      min_child_weight  = 5
    ),
    lambda = PENALTY
  )

  # ── rule extraction ──────────────────────────────────────────────────────────
  # xrf stores rules as column names of its internal sparse matrix; coef() returns
  # a named vector where each name IS the rule string in "var op threshold" format.
  cf      <- coef(fit$glm, s = PENALTY)
  cf_mat  <- as.matrix(cf)
  cf_df   <- data.frame(
    rule_text   = rownames(cf_mat),
    coefficient = cf_mat[, 1],
    stringsAsFactors = FALSE
  )
  rules0 <- cf_df %>% filter(rule_text != "(Intercept)", coefficient != 0)

  if (nrow(rules0) == 0 && PENALTY == "lambda.1se") {
    cat("  No rules at lambda.1se -- retrying at lambda.min ...\n")
    cf     <- coef(fit$glm, s = "lambda.min")
    cf_mat <- as.matrix(cf)
    cf_df  <- data.frame(rule_text = rownames(cf_mat), coefficient = cf_mat[, 1],
                         stringsAsFactors = FALSE)
    rules0 <- cf_df %>% filter(rule_text != "(Intercept)", coefficient != 0)
  }
  if (nrow(rules0) == 0) {
    cat("  No rules at either penalty; skipping this stratum\n")
    return(empty)
  }

  # Use |coefficient| as importance proxy (xrf has no separate importance function).
  rules <- rules0 %>% mutate(
    rule_id = rule_text,
    imp     = abs(coefficient)
  )

  n_before <- nrow(rules)
  rules <- tidy_rules(rules)
  cat(sprintf("  rules after tidy (drop repeated bounds / supersets): %d of %d\n",
              nrow(rules), n_before))

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

  # Inclusion NET: greedily OR INCLUDE-direction rules.
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
        d_value <- sum(protect[newc])
        d_clean <- sum(newc & !ie)
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

out_file <- function(stem) file.path(out_dir, sprintf("xrf_%s_%s.csv", ERROR_TAG, stem))

df_list <- list(
  earned_income   = earned_income_df,
  unearned_income = unearned_income_df,
  underissuance   = underissuance_df
)

rule_tables <- list()
shortlists  <- list()
net_paths   <- list()
ops_list    <- list()

for (nm in names(df_list)) {
  focal_df <- df_list[[nm]]
  focal_df <- earned_income_df
  

  ERROR_TAG <- paste(sort(setdiff(unique(as.character(focal_df$error_status)), "no_error")),
                     collapse = "_")

  groups  <- hh_group_of(focal_df[[HH_SIZE_COL]])
  results <- lapply(HH_LEVELS, function(lab)
    run_for_hh(focal_df[!is.na(groups) & groups == lab, , drop = FALSE], lab))

  rule_table_all <- bind_rows(lapply(results, `[[`, "rule_table"))
  shortlist_all  <- bind_rows(lapply(results, `[[`, "shortlist"))
  net_path_all   <- bind_rows(lapply(results, `[[`, "net_path"))
  ops_all        <- bind_rows(lapply(results, `[[`, "ops"))

  rule_table_all$error_tag <- ERROR_TAG
  shortlist_all$error_tag  <- ERROR_TAG
  net_path_all$error_tag   <- ERROR_TAG
  ops_all$error_tag        <- ERROR_TAG

  rule_tables[[nm]] <- rule_table_all
  shortlists[[nm]]  <- shortlist_all
  net_paths[[nm]]   <- net_path_all
  ops_list[[nm]]    <- ops_all

  write.csv(rule_table_all, out_file("by_HHsize_inclusion_rules_all"),          row.names = FALSE)
  write.csv(shortlist_all,  out_file("by_HHsize_inclusion_rules_highprecision"), row.names = FALSE)
  write.csv(net_path_all,   out_file("by_HHsize_net_frontier_path"),             row.names = FALSE)
  write.csv(ops_all,        out_file("by_HHsize_net_operating_points"),          row.names = FALSE)
}

rule_table_combined <- bind_rows(rule_tables)
shortlist_combined  <- bind_rows(shortlists)
net_path_combined   <- bind_rows(net_paths)
ops_list_combined   <- bind_rows(ops_list)

ERROR_TAG <- "final"

write.csv(rule_table_combined, out_file("by_HHsize_inclusion_rules_all"),          row.names = FALSE)
write.csv(shortlist_combined,  out_file("by_HHsize_inclusion_rules_highprecision"), row.names = FALSE)
write.csv(net_path_combined,   out_file("by_HHsize_net_frontier_path"),            row.names = FALSE)
write.csv(ops_list_combined,   out_file("by_HHsize_net_operating_points"),         row.names = FALSE)

cat("\n-- rules in each net (by household size) --\n")
for (i in seq_len(nrow(ops_list_combined)))
  cat(sprintf("\n  [HH %s]  recall >= %.2f  ->  precision %.2f, flag %.1f%% of cases, FLAG a case if it matches ANY of:\n    %s\n",
              ops_list_combined$hh_size[i], ops_list_combined$recall_floor[i], ops_list_combined$precision[i], ops_list_combined$workload_pct[i],
              gsub("  OR  ", "\n    OR ", ops_list_combined$net[i])))
