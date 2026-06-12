# ──────────────────────────────────────────────────────────────────────────────
# Grid-search the high-precision INCLUSION rules on ONE state's data
#
# This reuses the grid-search logic of 7_gridsearch_optimize_inclusion_rules.R, but
# instead of one hand-pasted rule it reads the whole high-precision rule list that
# script 6 wrote (inclusion_rules_highprecision.csv, in the
# "review_targeting_rulefit_full_data" folder). For each rule it:
#
#   1. parses the rule text into conditions;
#   2. grid-searches the numeric thresholds (operators fixed) within the rule's own
#      household-size stratum of the selected state's data, holding binary-indicator
#      and categorical conditions (e.g. expedited_i %in% c("TRUE")) fixed;
#   3. keeps the thresholds that MAXIMIZE PRECISION while holding recall >= a floor;
#   4. DROPS the rule if its best precision is below PRECISION_FLOOR.
#
# Output: one table of state-optimized, high-precision rules, tagged with hh_size.
# ──────────────────────────────────────────────────────────────────────────────

library(dplyr)

## ── 0. Config ─────────────────────────────────────────────────────────────────

# `internal_data` expected in the environment (the universe of cases to search).
# Either pass an already state-filtered frame, or set STATE / STATE_COL below.
state_data <- reg_model_data %>% filter(state %in% c("Virginia") & fiscal_year>2019)

# Rule list(s) written by script 6. Point this at the file you want to optimize.
#RULES_CSV <- file.path("inclusion_rules",
#                       "by_HHsize_inclusion_rules_highprecision.csv")
#if you just read in the csv, you may want to exclude rules that do not flag many cases as these do not indicate any overall pattern
#rules_df <- read.csv(RULES_CSV, stringsAsFactors = FALSE, check.names = FALSE)

#This is to combine all the rules into a single grid search in the state
earned_overissuance_RULES_CSV <- read.csv(file.path("inclusion_rules",
                       "earned_overissuance_by_HHsize_inclusion_rules_highprecision.csv")) %>% 
  filter(n_flagged > 99) %>% mutate(flag_type="earned_overissuance")

unearned_overissuance_RULES_CSV <- read.csv(file.path("inclusion_rules",
                       "unearned_overissuance_by_HHsize_inclusion_rules_highprecision.csv")) %>% 
  filter(n_flagged > 99) %>% mutate(flag_type="unearned_overissuance")

underissuance_RULES_CSV <- read.csv(file.path("inclusion_rules","underissuance_by_HHsize_inclusion_rules_highprecision.csv")) %>% 
                                      filter(n_flagged > 99) %>% mutate(flag_type="underissuance")
                                    
rules_in <- bind_rows(earned_overissuance_RULES_CSV, unearned_overissuance_RULES_CSV, underissuance_RULES_CSV)
rules_in <- subset(rules_in, n_conditions>1) 

rules_in$rule_text <- rules_in$rule
if (!"imp" %in% names(rules_in))
  rules_in$imp <- if ("importance" %in% names(rules_in)) rules_in$importance else 0
rules_df <- bind_rows(lapply(split(rules_in, rules_in$hh_size), tidy_rules))
rules_df$rule <- rules_df$rule_text 

TARGET_IS_ERROR <- quote(!is.na(over_threshold) & over_threshold != 0)
ERR_AMT_COL     <- "total_error_amount"
OBJECTIVE       <- "dollars"     # recall basis: "dollars" or "counts"
RECALL_FLOOR    <- 0.02          # a tuned rule must still capture at least this share
PRECISION_FLOOR <- 0.20          # DROP any rule whose optimized precision is below this

# Household-size stratification: cert_HH_size_FS_n collapsed to 1, 2, 3, 4, 5+.
HH_SIZE_COL <- "cert_HH_size_FS_n"
HH_LEVELS   <- c("1", "2", "3", "4", "5+")
hh_group_of <- function(n) { g <- pmin(n, 5); ifelse(g == 5, "5+", as.character(g)) }

# Grid bounds and size controls.
GRID_LO_Q    <- 0.02
GRID_HI_Q    <- 0.98
MAX_GRID_PTS <- 20      # cap candidate thresholds per variable
MAX_COMBOS   <- 6000   # cap total combinations per rule (grids are thinned to fit)

# Rounding STEP per variable. The grid is multiples of `step`; the rule's original
# threshold is always added so the search never does worse than the rule as written.
# Explicit overrides win; otherwise a step is inferred from the data (ratio/count/$).
VAR_STEPS <- c(
  unc_rawben_rel_max = 0.05, rawben_rel_max = 0.05, shelter_to_gross_ratio = 0.05,
  percent_abawd = 0.05, lf_composition = 0.05,
  months_since_cert_n = 1, n_income_types = 1, n_deduction_types = 1,
  rawunearn = 50, rawearn = 50, rawgross = 50, utilities = 25,
  shelter_expenses = 50, total_deductions = 50, medical_deductions = 25,
  earned_by_hh_size = 50, unearned_by_hh_size = 50, gross_by_hh_size = 50,
  shelter_exp_by_hh_size = 25
)
DOLLAR_STEP <- 50    # fallback for dollar-scale variables
RATIO_STEP  <- 0.05  # fallback for ratio/proportion variables
COUNT_STEP  <- 1     # fallback for small integer counts
RATIO_MAX   <- 10    # 98th pctile <= this (and not integer) -> treat as a ratio
COUNT_MAX   <- 60    # integer-valued with 98th pctile <= this -> treat as a count

out_dir <- "state_highprecision_gridsearch"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
stopifnot(OBJECTIVE %in% c("dollars", "counts"))

recall_col <- if (OBJECTIVE == "dollars") "dollar_recall" else "recall"

## ── 1. Helpers ────────────────────────────────────────────────────────────────

apply_op <- function(x, op, t) switch(op,
  ">=" = x >= t, "<=" = x <= t, ">" = x > t, "<" = x < t, "==" = x == t,
  stop("unsupported operator: ", op))

# Candidate thresholds: multiples of `step` over the data's central range, built
# from finite values only and capped to MAX_GRID_PTS evenly spaced values.
snapped_grid <- function(x, step, max_pts = MAX_GRID_PTS, lo_q = GRID_LO_Q, hi_q = GRID_HI_Q) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(numeric(0))
  lo <- floor(quantile(x, lo_q) / step) * step
  hi <- ceiling(quantile(x, hi_q) / step) * step
  if (!is.finite(lo) || !is.finite(hi)) return(numeric(0))
  if (hi <= lo) hi <- lo + step
  g <- seq(lo, hi, by = step)
  if (length(g) > max_pts) g <- g[round(seq(1, length(g), length.out = max_pts))]
  unique(g)
}

# Thin a set of grids until their product is within budget (shrink the largest first).
thin_to_budget <- function(grids, max_combos) {
  repeat {
    sizes <- vapply(grids, length, integer(1))
    if (prod(sizes) <= max_combos || all(sizes <= 2)) break
    j <- which.max(sizes)
    keep <- max(2L, floor(length(grids[[j]]) / 1.5))
    grids[[j]] <- unique(grids[[j]][round(seq(1, length(grids[[j]]), length.out = keep))])
  }
  grids
}

# Choose a rounding step for a variable: explicit override, else inferred from data.
step_for <- function(var, x) {
  if (var %in% names(VAR_STEPS)) return(unname(VAR_STEPS[[var]]))
  xf <- x[is.finite(x)]
  if (length(xf) == 0) return(DOLLAR_STEP)
  hi     <- as.numeric(quantile(xf, 0.98))
  is_int <- all(abs(xf - round(xf)) < 1e-8)
  if (is_int && hi <= COUNT_MAX) return(COUNT_STEP)
  if (hi <= RATIO_MAX)           return(RATIO_STEP)
  DOLLAR_STEP
}

# Parse one condition "var OP number" into a numeric term, else keep it verbatim.
parse_condition <- function(cond) {
  cond <- trimws(cond)
  m <- regmatches(cond, regexec("^(.*?)\\s*(>=|<=|==|>|<)\\s*(-?[0-9]*\\.?[0-9]+)\\s*$", cond))[[1]]
  if (length(m) == 4)
    list(type = "numeric", var = trimws(m[2]), op = m[3], original = as.numeric(m[4]))
  else
    list(type = "fixed", expr = cond)
}

# Split a {pre} rule description on " & " into its conditions.
parse_rule <- function(rule_string)
  lapply(strsplit(rule_string, " & ", fixed = TRUE)[[1]], parse_condition)

# AND a set of verbatim condition strings over `data`; NA -> not matched.
eval_mask <- function(exprs, data) {
  if (length(exprs) == 0) return(rep(TRUE, nrow(data)))
  expr <- paste(sprintf("(%s)", exprs), collapse = " & ")
  out  <- tryCatch(with(data, eval(parse(text = expr))),
                   error = function(e) rep(NA, nrow(data)))
  out[is.na(out)] <- FALSE
  as.logical(out)
}

# Flag = fixed_mask AND (each grid term at its chosen threshold).
flag_for <- function(thresholds, fixed_mask, xs, ops) {
  g <- fixed_mask
  for (j in seq_along(xs)) {
    f <- apply_op(xs[[j]], ops[j], thresholds[j]); f[is.na(f)] <- FALSE
    g <- g & f
  }
  g
}

# Performance of an INCLUSION flag (TRUE = case flagged for review).
inclusion_perf <- function(flag, is_error, err_dollars) {
  N <- length(flag); n_flag <- sum(flag); total_err <- sum(is_error)
  tp <- sum(flag & is_error); base_rate <- total_err / N
  ed <- err_dollars; ed[is.na(ed)] <- 0
  dollars_total <- sum(ed); dollars_caught <- sum(ed[flag])
  tibble(
    n_flagged = n_flag, workload_pct = 100 * n_flag / N,
    errors_caught = tp, clean_flagged = n_flag - tp,
    precision = if (n_flag > 0) tp / n_flag else NA_real_,
    recall = if (total_err > 0) tp / total_err else NA_real_,
    dollar_recall = if (dollars_total > 0) dollars_caught / dollars_total else NA_real_,
    lift = if (n_flag > 0 && base_rate > 0) (tp / n_flag) / base_rate else NA_real_,
    err_dollars_caught = dollars_caught
  )
}

## ── 2. Optimize a single rule within its stratum ──────────────────────────────
# Returns a one-row tibble for a kept rule, or NULL if a needed variable is missing,
# nothing meets the recall floor, or the best precision is below PRECISION_FLOOR.

optimize_rule <- function(rule_string, rule_id, hh_label, dat, is_error, err_dollars, csv_prec) {
  conds <- parse_rule(rule_string)
  
  # A numeric condition is grid-searchable only if its variable is present, numeric,
  # and not binary; numeric vars missing from the data make the rule unusable.
  if (any(vapply(conds, function(ci) ci$type == "numeric" && !(ci$var %in% names(dat)), logical(1))))
    return(NULL)
  is_grid <- vapply(conds, function(ci) {
    if (ci$type != "numeric") return(FALSE)
    x <- dat[[ci$var]]
    is.numeric(x) && length(unique(x[is.finite(x)])) > 2
  }, logical(1))
  
  # Fixed conditions: binary-numeric become "var op original", categorical stay verbatim.
  fixed_exprs <- vapply(which(!is_grid), function(i) {
    ci <- conds[[i]]
    if (ci$type == "numeric") sprintf("%s %s %s", ci$var, ci$op, ci$original) else ci$expr
  }, character(1))
  fixed_mask <- eval_mask(fixed_exprs, dat)
  
  grid_idx <- which(is_grid)
  if (length(grid_idx) == 0) {
    best <- inclusion_perf(fixed_mask, is_error, err_dollars)
    base_perf <- best
    best_thr  <- numeric(0)
  } else {
    gvars <- vapply(grid_idx, function(i) conds[[i]]$var,      character(1))
    gops  <- vapply(grid_idx, function(i) conds[[i]]$op,       character(1))
    gorig <- vapply(grid_idx, function(i) conds[[i]]$original, numeric(1))
    xs    <- lapply(gvars, function(v) dat[[v]])
    
    grids <- lapply(seq_along(gvars), function(j) {
      g <- snapped_grid(dat[[gvars[j]]], step_for(gvars[j], dat[[gvars[j]]]))
      if (length(g) == 0) gorig[j] else g
    })
    grids <- thin_to_budget(grids, MAX_COMBOS)
    grids <- lapply(seq_along(grids),
                    function(j) sort(unique(c(grids[[j]], gorig[j]))))  # keep the original
    
    # Index the grid positionally (a rule may use the same variable twice, e.g. a
    # band x >= a & x < b, so columns cannot be keyed by variable name).
    combo <- as.matrix(expand.grid(grids, KEEP.OUT.ATTRS = FALSE))
    perfs <- bind_rows(lapply(seq_len(nrow(combo)), function(r)
      inclusion_perf(flag_for(combo[r, ], fixed_mask, xs, gops), is_error, err_dollars)))
    
    rc       <- perfs[[recall_col]]
    feas_idx <- which(!is.na(rc) & rc >= RECALL_FLOOR)
    if (length(feas_idx) == 0) return(NULL)
    prec      <- perfs$precision[feas_idx]
    best_i    <- feas_idx[which.max(ifelse(is.na(prec), -Inf, prec))]
    best      <- perfs[best_i, ]
    best_thr  <- as.numeric(combo[best_i, ])
    base_perf <- inclusion_perf(flag_for(gorig, fixed_mask, xs, gops), is_error, err_dollars)
  }
  
  if (is.na(best$precision) || best$precision < PRECISION_FLOOR) return(NULL)
  if (is.na(best[[recall_col]]) || best[[recall_col]] < RECALL_FLOOR) return(NULL)
  
  # Rebuild the optimized rule string in the original condition order.
  gi <- 0L
  opt_parts <- vapply(seq_along(conds), function(i) {
    ci <- conds[[i]]
    if (is_grid[i]) { gi <<- gi + 1L; sprintf("%s %s %s", ci$var, ci$op, best_thr[gi]) }
    else if (ci$type == "numeric") sprintf("%s %s %s", ci$var, ci$op, ci$original)
    else ci$expr
  }, character(1))
  
  tibble(
    hh_size = hh_label, rule_id = rule_id,
    n_conditions = length(conds), n_optimized = length(grid_idx),
    n_flagged = best$n_flagged, errors_caught = best$errors_caught,
    clean_flagged = best$clean_flagged,
    workload_pct = round(best$workload_pct, 2),
    precision = round(best$precision, 3),
    recall = round(best$recall, 3),
    dollar_recall = round(best$dollar_recall, 3),
    lift = round(best$lift, 2),
    precision_state_orig   = round(base_perf$precision, 3),
    precision_national_csv = round(csv_prec, 3),
    optimized_rule = paste(opt_parts, collapse = " & "),
    original_rule  = rule_string
  )
}


## ── 3. Load the state's data and the rule list ────────────────────────────────


stopifnot(HH_SIZE_COL %in% names(state_data))
if (OBJECTIVE == "dollars" && !(ERR_AMT_COL %in% names(state_data)))
  stop("OBJECTIVE = 'dollars' requires ERR_AMT_COL '", ERR_AMT_COL, "' in the data.")



stopifnot(all(c("hh_size", "rule") %in% names(rules_df)))
rules_df$hh_size <- as.character(rules_df$hh_size)
if (!"rule_id" %in% names(rules_df))   rules_df$rule_id   <- paste0("rule", seq_len(nrow(rules_df)))
if (!"precision" %in% names(rules_df)) rules_df$precision <- NA_real_
if ("role" %in% names(rules_df))       rules_df <- rules_df[rules_df$role == "INCLUDE", , drop = FALSE]

state_label <- if (is.na(STATE)) "all-data" else STATE
cat(sprintf("\n=== Optimizing %d rules on %s (objective: %s, recall floor %.2f, precision floor %.2f) ===\n",
            nrow(rules_df), state_label, toupper(OBJECTIVE), RECALL_FLOOR, PRECISION_FLOOR))

## ── 4. Optimize every rule within its household-size stratum ──────────────────

groups    <- hh_group_of(state_data[[HH_SIZE_COL]])
collected <- list()
n_seen    <- 0L

for (lab in HH_LEVELS) {
  dat     <- state_data[!is.na(groups) & groups == lab, , drop = FALSE]
  rules_h <- rules_df[rules_df$hh_size == lab, , drop = FALSE]
  cat(sprintf("\n#### HH %s : %d rows, %d candidate rules ####\n", lab, nrow(dat), nrow(rules_h)))
  if (nrow(dat) < 30 || nrow(rules_h) == 0) {
    cat("  too few rows or no rules for this stratum; skipping\n"); next
  }

  is_error <- eval(TARGET_IS_ERROR, envir = dat); is_error[is.na(is_error)] <- FALSE
  edv <- if (ERR_AMT_COL %in% names(dat)) dat[[ERR_AMT_COL]] else rep(0, nrow(dat))
  edv[is.na(edv)] <- 0
  err_dollars <- ifelse(is_error, abs(edv), 0)
  cat(sprintf("  errors = %d (%.1f%%) | error $ = $%s\n",
              sum(is_error), 100 * mean(is_error),
              format(round(sum(err_dollars)), big.mark = ",")))

  for (r in seq_len(nrow(rules_h))) {
    n_seen <- n_seen + 1L
    res <- optimize_rule(rules_h$rule[r], rules_h$rule_id[r], lab,
                         dat, is_error, err_dollars, rules_h$precision[r])
    if (!is.null(res)) collected[[length(collected) + 1L]] <- res
  }
  cat(sprintf("  kept %d of %d rules at precision >= %.2f\n",
              sum(vapply(collected, function(x) x$hh_size == lab, logical(1))),
              nrow(rules_h), PRECISION_FLOOR))
}

## ── 5. Output the state-optimized high-precision rules ────────────────────────

optimized <- bind_rows(collected)
if (nrow(optimized) > 0) optimized <- optimized %>% arrange(hh_size, desc(precision))

cat(sprintf("\n\n========== STATE-OPTIMIZED HIGH-PRECISION RULES (%s) ==========\n", state_label))
cat(sprintf("kept %d of %d candidate rules\n", nrow(optimized), n_seen))
if (nrow(optimized) > 0) {
  show_cols <- c("hh_size", "rule_id", "precision", recall_col, "n_flagged",
                 "errors_caught", "precision_state_orig", "precision_national_csv",
                 "optimized_rule")
  print(as.data.frame(optimized[, show_cols]))
}

optimized$rule_text <- optimized$optimized_rule
if (!"imp" %in% names(optimized))
  optimized$imp <- if ("importance" %in% names(optimized)) optimized$importance else 0
rules_out <- bind_rows(lapply(split(optimized, optimized$hh_size), tidy_rules))
rules_out$rule <- rules_out$rule_text 

fname <- sprintf("optimized_highprecision_rules_%s.csv",
                 if (is.na(STATE)) "all_data" else STATE)
write.csv(rules_out, file.path(out_dir, fname), row.names = FALSE)

## ── 6. Notes ──────────────────────────────────────────────────────────────────
# - precision            : optimized precision on the state's data (this run).
# - precision_state_orig : the rule's ORIGINAL thresholds, scored on the state's data.
# - precision_national_csv: precision recorded in the input CSV (script 6's universe).
# - Only numeric, non-binary conditions are tuned; binary indicators and categorical
#   (%in%) conditions are held as written. The original threshold is always kept in
#   each grid, so optimization never scores below the rule as written.
# - Steps are taken from VAR_STEPS or inferred. For a finer search on a variable,
#   add it to VAR_STEPS (e.g. rawunearn = 5) and/or raise MAX_GRID_PTS / MAX_COMBOS.
# - Thresholds are tuned in-sample on this state; confirm on a holdout before adopting.
