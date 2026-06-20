# ──────────────────────────────────────────────────────────────────────────────
# Grid-search the EXCLUSION shortlist on ONE state's data
#
# The exclusion counterpart of INCL_optimize_set_of_inclusion_rules_by_hh_size_for_a_state.R (which optimizes inclusion rules). It
# reads the exclusion rule list written by EXCL_find_exclusion_rules_by_hh_size.R (exclusion_rules_shortlist.csv
# or exclusion_rules_all.csv) and, for each EXCLUDE rule, within its own household-
# size stratum of the selected state's data:
#
#   1. parses the rule text into conditions;
#   2. grid-searches the numeric thresholds (operators fixed), holding binary and
#      categorical conditions (e.g. expedited_i %in% c("TRUE")) as written;
#   3. keeps the thresholds that MAXIMIZE the share of cases EXCLUDED (workload cut)
#      while RETAINING at least RETAIN_FLOOR of the error dollars (so the dollar
#      reduction stays small);
#   4. DROPS the rule if it cannot cut at least MIN_WORKLOAD_CUT of cases at that
#      safety level (optionally also a minimum exclusion purity).
#
# Where script 8 maximizes precision subject to a recall floor, this maximizes
# workload cut subject to a dollar-retention floor: cut large swaths of cases while
# minimizing the error dollars lost.
#
# Output: one table of state-optimized, high-coverage exclusion rules, by hh_size.
# ──────────────────────────────────────────────────────────────────────────────

library(dplyr)

## ── 0. Config ─────────────────────────────────────────────────────────────────

# `internal_data` expected in the environment (the universe of cases to search).
# Either pass an already state-filtered frame, or set STATE / STATE_COL below.
STATE_COL <- "state"            # column identifying the state; ignored if STATE is NA
STATE     <- "Michigan"   # NA = use internal_data as provided, unfiltered

#state_data could be internal data or just a subset of the public data:
state_data <- reg_model_data %>% filter(fiscal_year>2019) %>% filter(state=="Michigan")

# Exclusion rule list written by EXCL_find_exclusion_rules_by_hh_size.R. Point this at the file you want to optimize.
RULES_CSV <- file.path("exclusion_rules", "exclusion_rules_by_hh_size_shortlist.csv")

TARGET_IS_ERROR <- quote(!is.na(over_threshold) & over_threshold != 0)
ERR_AMT_COL     <- "total_error_amount"
OBJECTIVE       <- "dollars"     # safety basis: "dollars" (retain error $) or "counts" (retain error cases)
RETAIN_FLOOR    <- 0.97          # a tuned rule must RETAIN at least this share (lose <= 1 - this)
MIN_WORKLOAD_CUT<- 0.05          # DROP a rule that cannot exclude at least this share of cases
MIN_PURITY      <- 0             # optional: require >= this share of EXCLUDED cases to be clean (0 = off)

# Household-size stratification: cert_HH_size_FS_n collapsed to 1, 2-3, 4+.
HH_SIZE_COL <- "cert_HH_size_FS_n"
HH_LEVELS   <- c("1", "2-3", "4+")
hh_group_of <- function(n) { ifelse(n <= 1, "1", ifelse(n <= 3, "2-3", "4+")) }

# Grid bounds and size controls.
GRID_LO_Q    <- 0.02
GRID_HI_Q    <- 0.98
MAX_GRID_PTS <- 20      # cap candidate thresholds per variable
MAX_COMBOS   <- 12000   # cap total combinations per rule (grids are thinned to fit)

# Rounding STEP per variable. The grid is multiples of `step`; the rule's original
# threshold is always added so the search never does worse than the rule as written.
VAR_STEPS <- c(
  unc_rawben_rel_max = 0.05, rawben_rel_max = 0.05, shelter_to_gross_ratio = 0.05,
  percent_abawd = 0.05, lf_composition = 0.05,
  months_since_cert_n = 1, n_income_types = 1, n_deduction_types = 1,
  rawunearn = 50, rawearn = 50, rawgross = 50, rawgrinc = 50, utilities = 25,
  shelter_expenses = 50, total_deductions = 50, medical_deductions = 25,
  earned_by_hh_size = 50, unearned_by_hh_size = 50, gross_by_hh_size = 50,
  shelter_exp_by_hh_size = 25
)
DOLLAR_STEP <- 50    # fallback for dollar-scale variables
RATIO_STEP  <- 0.05  # fallback for ratio/proportion variables
COUNT_STEP  <- 1     # fallback for small integer counts
RATIO_MAX   <- 10    # 98th pctile <= this (and not integer) -> treat as a ratio
COUNT_MAX   <- 60    # integer-valued with 98th pctile <= this -> treat as a count

out_dir <- "exclusion_rules"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
stopifnot(OBJECTIVE %in% c("dollars", "counts"))

# the safety metric we hold above RETAIN_FLOOR while maximizing workload cut
retain_col <- if (OBJECTIVE == "dollars") "dollar_recall_retained" else "recall_retained"

## ── 1. Helpers ────────────────────────────────────────────────────────────────

apply_op <- function(x, op, t) switch(op,
  ">=" = x >= t, "<=" = x <= t, ">" = x > t, "<" = x < t, "==" = x == t,
  stop("unsupported operator: ", op))

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

parse_condition <- function(cond) {
  cond <- trimws(cond)
  m <- regmatches(cond, regexec("^(.*?)\\s*(>=|<=|==|>|<)\\s*(-?[0-9]*\\.?[0-9]+)\\s*$", cond))[[1]]
  if (length(m) == 4)
    list(type = "numeric", var = trimws(m[2]), op = m[3], original = as.numeric(m[4]))
  else
    list(type = "fixed", expr = cond)
}

parse_rule <- function(rule_string)
  lapply(strsplit(rule_string, " & ", fixed = TRUE)[[1]], parse_condition)

eval_mask <- function(exprs, data) {
  if (length(exprs) == 0) return(rep(TRUE, nrow(data)))
  expr <- paste(sprintf("(%s)", exprs), collapse = " & ")
  out  <- tryCatch(with(data, eval(parse(text = expr))),
                   error = function(e) rep(NA, nrow(data)))
  out[is.na(out)] <- FALSE
  as.logical(out)
}

# Exclusion flag = fixed_mask AND (each grid term at its chosen threshold).
flag_for <- function(thresholds, fixed_mask, xs, ops) {
  g <- fixed_mask
  for (j in seq_along(xs)) {
    f <- apply_op(xs[[j]], ops[j], thresholds[j]); f[is.na(f)] <- FALSE
    g <- g & f
  }
  g
}

# Performance of a single EXCLUSION flag (TRUE = case would be dropped from review).
# `excl` is the matched (excluded) set; retains everything it does not match.
exclusion_perf <- function(excl, is_error, err_dollars) {
  N <- length(excl); n_excl <- sum(excl); n_ret <- N - n_excl; total_err <- sum(is_error)
  errors_lost     <- sum(excl & is_error)
  clean_excluded  <- sum(excl & !is_error)
  errors_retained <- total_err - errors_lost
  ed <- err_dollars; ed[is.na(ed)] <- 0
  dollars_total    <- sum(ed); dollars_lost <- sum(ed[excl])
  dollars_retained <- dollars_total - dollars_lost
  tibble(
    n_excluded = n_excl, workload_cut_pct = 100 * n_excl / N, n_retained = n_ret,
    clean_excluded = clean_excluded, errors_lost = errors_lost,
    exclusion_purity = if (n_excl > 0) clean_excluded / n_excl else NA_real_,
    clean_per_error_lost = if (errors_lost > 0) clean_excluded / errors_lost else Inf,
    retained_precision = if (n_ret > 0) errors_retained / n_ret else NA_real_,
    recall_retained = if (total_err > 0) errors_retained / total_err else NA_real_,
    err_dollars_lost = dollars_lost,
    dollar_recall_retained = if (dollars_total > 0) dollars_retained / dollars_total else NA_real_
  )
}

## ── 2. Optimize a single exclusion rule within its stratum ────────────────────
# Returns a one-row tibble for a kept rule, or NULL if a needed variable is missing,
# nothing meets the retention floor, or the best workload cut is below MIN_WORKLOAD_CUT.

optimize_rule <- function(rule_string, rule_id, hh_label, dat, is_error, err_dollars, csv_retain) {
  conds <- parse_rule(rule_string)

  if (any(vapply(conds, function(ci) ci$type == "numeric" && !(ci$var %in% names(dat)), logical(1))))
    return(NULL)
  is_grid <- vapply(conds, function(ci) {
    if (ci$type != "numeric") return(FALSE)
    x <- dat[[ci$var]]
    is.numeric(x) && length(unique(x[is.finite(x)])) > 2
  }, logical(1))

  fixed_exprs <- vapply(which(!is_grid), function(i) {
    ci <- conds[[i]]
    if (ci$type == "numeric") sprintf("%s %s %s", ci$var, ci$op, ci$original) else ci$expr
  }, character(1))
  fixed_mask <- eval_mask(fixed_exprs, dat)

  grid_idx <- which(is_grid)
  if (length(grid_idx) == 0) {
    best <- exclusion_perf(fixed_mask, is_error, err_dollars)
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

    # Positional grid (a rule may use the same variable twice, e.g. a band).
    combo <- as.matrix(expand.grid(grids, KEEP.OUT.ATTRS = FALSE))
    perfs <- bind_rows(lapply(seq_len(nrow(combo)), function(r)
      exclusion_perf(flag_for(combo[r, ], fixed_mask, xs, gops), is_error, err_dollars)))

    # Feasible = retains enough (dollars or cases); among those, cut the MOST cases.
    rc       <- perfs[[retain_col]]
    feas_idx <- which(!is.na(rc) & rc >= RETAIN_FLOOR)
    if (length(feas_idx) == 0) return(NULL)
    wc       <- perfs$workload_cut_pct[feas_idx]
    best_i   <- feas_idx[which.max(ifelse(is.na(wc), -Inf, wc))]
    best     <- perfs[best_i, ]
    best_thr <- as.numeric(combo[best_i, ])
    base_perf<- exclusion_perf(flag_for(gorig, fixed_mask, xs, gops), is_error, err_dollars)
  }

  # Filters: must retain enough, must cut a meaningful share, optional purity.
  if (is.na(best[[retain_col]]) || best[[retain_col]] < RETAIN_FLOOR) return(NULL)
  if (is.na(best$workload_cut_pct) || best$workload_cut_pct / 100 < MIN_WORKLOAD_CUT) return(NULL)
  if (MIN_PURITY > 0 && (is.na(best$exclusion_purity) || best$exclusion_purity < MIN_PURITY)) return(NULL)

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
    n_excluded = best$n_excluded, n_retained = best$n_retained,
    workload_cut_pct = round(best$workload_cut_pct, 2),
    clean_excluded = best$clean_excluded, errors_lost = best$errors_lost,
    exclusion_purity = round(best$exclusion_purity, 3),
    clean_per_error_lost = round(best$clean_per_error_lost, 1),
    err_dollars_lost = round(best$err_dollars_lost, 0),
    dollar_recall_retained = round(best$dollar_recall_retained, 3),
    recall_retained = round(best$recall_retained, 3),
    retained_precision = round(best$retained_precision, 3),
    workload_cut_orig = round(base_perf$workload_cut_pct, 2),
    retain_state_orig = round(base_perf[[retain_col]], 3),
    retain_national_csv = round(csv_retain, 3),
    optimized_rule = paste(opt_parts, collapse = " & "),
    original_rule  = rule_string
  )
}

## ── 3. Load the state's data and the exclusion rule list ──────────────────────



stopifnot(HH_SIZE_COL %in% names(state_data))
if (OBJECTIVE == "dollars" && !(ERR_AMT_COL %in% names(state_data)))
  stop("OBJECTIVE = 'dollars' requires ERR_AMT_COL '", ERR_AMT_COL, "' in the data.")

rules_df <- read.csv(RULES_CSV, stringsAsFactors = FALSE, check.names = FALSE)
stopifnot(all(c("hh_size", "rule") %in% names(rules_df)))
rules_df$hh_size <- as.character(rules_df$hh_size)
if (!"rule_id" %in% names(rules_df)) rules_df$rule_id <- paste0("rule", seq_len(nrow(rules_df)))
if ("role" %in% names(rules_df))     rules_df <- rules_df[rules_df$role == "EXCLUDE", , drop = FALSE]
# carry the CSV's retention metric as a reference, if present
csv_retain_col <- if ("dollar_recall_retained" %in% names(rules_df) && OBJECTIVE == "dollars")
  "dollar_recall_retained" else if ("recall_retained" %in% names(rules_df)) "recall_retained" else NA
rules_df$.csv_retain <- if (!is.na(csv_retain_col)) rules_df[[csv_retain_col]] else NA_real_

state_label <- if (is.na(STATE)) "all-data" else STATE
cat(sprintf("\n=== Optimizing %d exclusion rules on %s (objective: %s, retain floor %.2f, min cut %.2f) ===\n",
            nrow(rules_df), state_label, toupper(OBJECTIVE), RETAIN_FLOOR, MIN_WORKLOAD_CUT))

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
                         dat, is_error, err_dollars, rules_h$.csv_retain[r])
    if (!is.null(res)) collected[[length(collected) + 1L]] <- res
  }
  cat(sprintf("  kept %d of %d rules (retain >= %.2f, cut >= %.0f%%)\n",
              sum(vapply(collected, function(x) x$hh_size == lab, logical(1))),
              nrow(rules_h), RETAIN_FLOOR, 100 * MIN_WORKLOAD_CUT))
}

## ── 5. Output the state-optimized high-coverage exclusion rules ───────────────

optimized <- bind_rows(collected)
if (nrow(optimized) > 0) optimized <- optimized %>% arrange(hh_size, desc(workload_cut_pct))

cat(sprintf("\n\n========== STATE-OPTIMIZED EXCLUSION RULES (%s) ==========\n", state_label))
cat(sprintf("kept %d of %d candidate rules\n", nrow(optimized), n_seen))
if (nrow(optimized) > 0) {
  show_cols <- c("hh_size", "rule_id", "workload_cut_pct", retain_col, "n_excluded",
                 "errors_lost", "err_dollars_lost", "workload_cut_orig", "optimized_rule")
  print(as.data.frame(optimized[, show_cols]))
}

fname <- sprintf("optimized_by_HH_size_exclusion_rules_%s.csv",
                 if (is.na(STATE)) "all_data" else STATE)
write.csv(optimized, file.path(out_dir, fname), row.names = FALSE)

## ── 6. Notes ──────────────────────────────────────────────────────────────────
# - workload_cut_pct        : optimized share of cases excluded (this run) - maximized.
# - dollar_recall_retained  : share of error $ KEPT after exclusion (the safety metric
#   held at or above RETAIN_FLOOR). recall_retained is the count-basis analogue.
# - workload_cut_orig       : cases the rule would cut at its ORIGINAL thresholds.
# - retain_national_csv     : retention recorded in the input CSV (script 4's universe).
# - The search maximizes cases cut subject to retaining >= RETAIN_FLOOR of error
#   dollars, so it pushes each threshold as far as it can without dropping protected
#   dollars. Only numeric, non-binary conditions are tuned; binary/categorical (%in%)
#   conditions are held as written, and the original threshold is always in the grid.
# - Tighten safety by raising RETAIN_FLOOR (e.g. 0.99); cut more aggressively by
#   lowering it. MIN_WORKLOAD_CUT drops rules that cannot remove a meaningful share.
# - Thresholds are tuned in-sample on this state; confirm on a holdout before adopting.
