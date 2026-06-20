# ──────────────────────────────────────────────────────────────────────────────
# Train / test inclusion rules across every state, then check cross-state stability
#
# Builds on INCL_optimize_set_of_inclusion_rules_by_hh_size_for_a_state.R. Where that script optimizes the
# rule list for ONE state, this script:
#
#   1. splits the public data into TRAINING years and held-out TEST years;
#   2. for EVERY state, grid-searches each rule's numeric thresholds on that state's
#      TRAINING data within the rule's household size (operators fixed; binary and
#      categorical conditions held as written) to maximize precision at a recall floor;
#   3. DROPS a (state, rule) whose TRAINING precision is below PREC_FLOOR (e.g. 0.20),
#      but still reports its held-out TEST performance even when TEST precision < floor;
#   4. examines STABILITY by rule: across the states where a rule is kept, do training
#      and testing precision track each other, or are they all over the place?
#
# Inputs in the environment:
#   flagged_cases  - public-use cases with a year column, a state column, the household
#                    size column, the QC target, the dollar column, and the rule variables.
# Plus the rule list written by INCL_find_inclusion_rules_multi_model_by_hh_size.R (RULES_CSV).
# ──────────────────────────────────────────────────────────────────────────────

library(dplyr)

## ── 0. Config ─────────────────────────────────────────────────────────────────

flagged_cases <- reg_model_data %>% filter(state=="Michigan")

YEAR_COL   <- "fiscal_year"
STATE_COL  <- "state"
TRAIN_YEARS <- c("2022","2023","2024")
TEST_YEARS  <- c("2018","2019")
STATES      <- NULL    # NULL = every state present in the data; or c("06","36",...) to subset

RULES_CSV <- file.path("inclusion_rules_by_hh_size", "final_by_HHsize_inclusion_rules_highprecision.csv")

TARGET_IS_ERROR <- quote(!is.na(over_threshold) & over_threshold != 0)
ERR_AMT_COL     <- "total_error_amount"
OBJECTIVE       <- "dollars"   # recall basis used for the floor: "dollars" or "counts"
RECALL_FLOOR    <- 0.02        # a tuned rule must still capture at least this share on TRAIN
PREC_FLOOR      <- 0.20        # DROP a (state, rule) whose TRAIN precision is below this

# Household-size stratification: cert_HH_size_FS_n collapsed to 1, 2-3, 4+.
HH_SIZE_COL <- "cert_HH_size_FS_n"
HH_LEVELS   <- c("1", "2-3", "4+")
hh_group_of <- function(n) { ifelse(n <= 1, "1", ifelse(n <= 3, "2-3", "4+")) }

# Minimum stratum sizes (per state, per household size).
MIN_TRAIN <- 50
MIN_TEST  <- 30
MIN_TRAIN_FLAGGED <- 10   # a (state, rule) must flag at least this many cases in the state's TRAIN data

# Grid controls (kept modest because this runs states x rules; raise for a finer search).
GRID_LO_Q    <- 0.02
GRID_HI_Q    <- 0.98
MAX_GRID_PTS <- 40
MAX_COMBOS   <- 5000

# Rounding step per variable (grids are built once from the full TRAINING data so the
# candidate thresholds are shared across states). Explicit overrides win; otherwise a
# step is inferred from the data (ratio / small-count / dollar).
VAR_STEPS <- c(
  unc_rawben_rel_max = 0.05, rawben_rel_max = 0.05, shelter_to_gross_ratio = 0.05,
  percent_abawd = 0.05, lf_composition = 0.05,
  months_since_cert_n = 1, n_income_types = 1, n_deduction_types = 1,
  rawunearn = 50, rawearn = 50, rawgross = 50, utilities = 25,
  shelter_expenses = 50, total_deductions = 50, medical_deductions = 25,
  earned_by_hh_size = 50, unearned_by_hh_size = 50, gross_by_hh_size = 50,
  shelter_exp_by_hh_size = 25
)
DOLLAR_STEP <- 50
RATIO_STEP  <- 0.05
COUNT_STEP  <- 1
RATIO_MAX   <- 10
COUNT_MAX   <- 60

# Stability thresholds: a rule is "stable" if, across states, the mean train->test
# precision drop is small AND the drop does not vary much.
STAB_GAP <- 0.15   # |mean(train precision - test precision)| within this
STAB_SD  <- 0.20   # sd of (train precision - test precision) within this

out_dir <- "state_train_test_rulecheck"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
stopifnot(OBJECTIVE %in% c("dollars", "counts"))
OBJ_DOLLARS <- OBJECTIVE == "dollars"

## ── 1. Helpers (shared with INCL_optimize_set_of_inclusion_rules_by_hh_size_for_a_state.R) ──────

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

# Target + recoverable error dollars for a data subset.
make_target <- function(df) {
  ie <- eval(TARGET_IS_ERROR, envir = df); ie[is.na(ie)] <- FALSE
  amt <- if (ERR_AMT_COL %in% names(df)) df[[ERR_AMT_COL]] else rep(0, nrow(df))
  amt[is.na(amt)] <- 0
  list(ie = ie, ed = ifelse(ie, abs(amt), 0))
}

## ── 2. Pre-process each rule ONCE against the full training data ──────────────
# Classifies conditions (grid-searchable numeric vs fixed), assigns steps, and builds
# the shared candidate grids. Returns NULL if a referenced variable is missing.

prep_rule <- function(rule_string, rule_id, hh, csv_prec, ref) {
  conds <- parse_rule(rule_string)
  if (any(vapply(conds, function(ci) ci$type == "numeric" && !(ci$var %in% names(ref)), logical(1))))
    return(NULL)
  is_grid <- vapply(conds, function(ci) {
    if (ci$type != "numeric") return(FALSE)
    x <- ref[[ci$var]]; is.numeric(x) && length(unique(x[is.finite(x)])) > 2
  }, logical(1))
  
  fixed_exprs <- vapply(which(!is_grid), function(i) {
    ci <- conds[[i]]
    if (ci$type == "numeric") sprintf("%s %s %s", ci$var, ci$op, ci$original) else ci$expr
  }, character(1))
  if (length(fixed_exprs) > 0) {
    fixed_vars <- sub("^\\s*([A-Za-z._][A-Za-z0-9._]*).*$", "\\1", fixed_exprs)
    if (any(!(fixed_vars %in% names(ref)))) return(NULL)
  }
  
  gi    <- which(is_grid)
  gvars <- vapply(gi, function(i) conds[[i]]$var,      character(1))
  gops  <- vapply(gi, function(i) conds[[i]]$op,       character(1))
  gorig <- vapply(gi, function(i) conds[[i]]$original, numeric(1))
  grids <- lapply(seq_along(gvars), function(j) {
    g <- snapped_grid(ref[[gvars[j]]], step_for(gvars[j], ref[[gvars[j]]]))
    if (length(g) == 0) gorig[j] else g
  })
  grids <- thin_to_budget(grids, MAX_COMBOS)
  grids <- lapply(seq_along(grids), function(j) sort(unique(c(grids[[j]], gorig[j]))))
  
  list(rule_id = rule_id, hh = as.character(hh), csv_prec = csv_prec,
       conds = conds, is_grid = is_grid, fixed_exprs = fixed_exprs,
       gvars = gvars, gops = gops, gorig = gorig, grids = grids)
}

rebuild_rule <- function(R, thr) {
  gi <- 0L
  parts <- vapply(seq_along(R$conds), function(i) {
    ci <- R$conds[[i]]
    if (R$is_grid[i]) { gi <<- gi + 1L; sprintf("%s %s %s", ci$var, ci$op, thr[gi]) }
    else if (ci$type == "numeric") sprintf("%s %s %s", ci$var, ci$op, ci$original)
    else ci$expr
  }, character(1))
  paste(parts, collapse = " & ")
}

## ── 3. Optimize on a TRAIN stratum, evaluate fixed thresholds on a TEST stratum ─

# Grid-search to MAX precision s.t. recall >= floor on the training stratum.
optimize_train <- function(R, tr) {
  tg <- make_target(tr); ie <- tg$ie; ed <- tg$ed
  total_err <- sum(ie); dollars_total <- sum(ed)
  if (total_err == 0) return(list(feasible = FALSE))
  
  fm  <- eval_mask(R$fixed_exprs, tr)
  sub <- which(fm)
  if (length(sub) == 0) return(list(feasible = FALSE))
  ie_s <- ie[sub]; ed_s <- ed[sub]
  
  rec_of <- function(flag, tp) if (OBJ_DOLLARS) (if (dollars_total > 0) sum(ed_s[flag]) / dollars_total else NA_real_)
  else (tp / total_err)
  
  if (length(R$gvars) == 0) {
    n <- length(sub); tp <- sum(ie_s); flag <- rep(TRUE, n)
    rec <- rec_of(flag, tp)
    if (n == 0 || n < MIN_TRAIN_FLAGGED || is.na(rec) || rec < RECALL_FLOOR) return(list(feasible = FALSE))
    return(list(feasible = TRUE, thr = numeric(0), precision = tp / n, recall = tp / total_err,
                dollar_recall = if (dollars_total > 0) sum(ed_s) / dollars_total else NA_real_,
                n_flagged = n, errors_caught = tp))
  }
  
  Ms <- lapply(seq_along(R$gvars), function(j) {
    x <- tr[[R$gvars[j]]][sub]; op <- R$gops[j]
    M <- sapply(R$grids[[j]], function(t) { f <- apply_op(x, op, t); f[is.na(f)] <- FALSE; f })
    if (is.null(dim(M))) M <- matrix(M, nrow = length(sub))
    M
  })
  idx <- as.matrix(expand.grid(lapply(R$grids, function(g) seq_along(g)), KEEP.OUT.ATTRS = FALSE))
  
  best_prec <- -Inf; best_idx <- NULL
  for (rI in seq_len(nrow(idx))) {
    flag <- rep(TRUE, length(sub))
    for (j in seq_along(Ms)) flag <- flag & Ms[[j]][, idx[rI, j]]
    n <- sum(flag); if (n == 0 || n < MIN_TRAIN_FLAGGED) next
    tp  <- sum(flag & ie_s)
    rec <- rec_of(flag, tp)
    if (is.na(rec) || rec < RECALL_FLOOR) next
    prec <- tp / n
    if (prec > best_prec) { best_prec <- prec; best_idx <- idx[rI, ] }
  }
  if (is.null(best_idx)) return(list(feasible = FALSE))
  
  flag <- rep(TRUE, length(sub))
  for (j in seq_along(Ms)) flag <- flag & Ms[[j]][, best_idx[j]]
  n <- sum(flag); tp <- sum(flag & ie_s)
  thr <- vapply(seq_along(best_idx), function(j) R$grids[[j]][best_idx[j]], numeric(1))
  list(feasible = TRUE, thr = thr, precision = tp / n, recall = tp / total_err,
       dollar_recall = if (dollars_total > 0) sum(ed_s[flag]) / dollars_total else NA_real_,
       n_flagged = n, errors_caught = tp)
}

# Apply fixed (already-optimized) thresholds to a test stratum.
eval_test <- function(R, thr, te) {
  tg <- make_target(te); ie <- tg$ie; ed <- tg$ed
  total_err <- sum(ie); dollars_total <- sum(ed)
  flag <- eval_mask(R$fixed_exprs, te)
  if (length(R$gvars) > 0) for (j in seq_along(R$gvars)) {
    f <- apply_op(te[[R$gvars[j]]], R$gops[j], thr[j]); f[is.na(f)] <- FALSE; flag <- flag & f
  }
  n <- sum(flag); tp <- sum(flag & ie)
  list(n_flagged = n, errors_caught = tp,
       precision = if (n > 0) tp / n else NA_real_,
       recall = if (total_err > 0) tp / total_err else NA_real_,
       dollar_recall = if (dollars_total > 0) sum(ed[flag]) / dollars_total else NA_real_)
}

## ── 4. Build train / test frames and pre-process the rules ────────────────────

stopifnot(YEAR_COL %in% names(flagged_cases), STATE_COL %in% names(flagged_cases),
          HH_SIZE_COL %in% names(flagged_cases))
if (OBJ_DOLLARS && !(ERR_AMT_COL %in% names(flagged_cases)))
  stop("OBJECTIVE = 'dollars' needs ERR_AMT_COL '", ERR_AMT_COL, "' in flagged_cases.")

yr <- as.character(flagged_cases[[YEAR_COL]])
train <- flagged_cases[yr %in% as.character(TRAIN_YEARS), , drop = FALSE]
test  <- flagged_cases[yr %in% as.character(TEST_YEARS),  , drop = FALSE]
cat(sprintf("Training years %s: %d rows | Test years %s: %d rows\n",
            paste(TRAIN_YEARS, collapse = "/"), nrow(train),
            paste(TEST_YEARS, collapse = "/"), nrow(test)))

rules_df <- read.csv(RULES_CSV, stringsAsFactors = FALSE, check.names = FALSE)
rules_df <- rules_df %>% sample_n(size=30)
stopifnot(all(c("hh_size", "rule") %in% names(rules_df)))
rules_df$hh_size <- as.character(rules_df$hh_size)
if (!"rule_id" %in% names(rules_df))   rules_df$rule_id   <- paste0("rule", seq_len(nrow(rules_df)))
if (!"precision" %in% names(rules_df)) rules_df$precision <- NA_real_
if ("role" %in% names(rules_df))       rules_df <- rules_df[rules_df$role == "INCLUDE", , drop = FALSE]

RULES <- Filter(Negate(is.null), lapply(seq_len(nrow(rules_df)), function(i)
  prep_rule(rules_df$rule[i], rules_df$rule_id[i], rules_df$hh_size[i], rules_df$precision[i], train)))
cat(sprintf("Prepared %d of %d rules (others reference variables not in the data).\n",
            length(RULES), nrow(rules_df)))

for (R in RULES) {
  if (length(R$gvars) == 0) next
  cat(sprintf("\n[%s | HH %s]\n", R$rule_id, R$hh))
  for (j in seq_along(R$gvars))
    cat(sprintf("  %-22s %s  (%d values): %s\n",
                R$gvars[j], R$gops[j], length(R$grids[[j]]),
                paste(R$grids[[j]], collapse = ", ")))
}

## ── 5. Loop states x rules: optimize on train, evaluate on test ───────────────

states <- if (is.null(STATES)) sort(unique(as.character(train[[STATE_COL]]))) else as.character(STATES)
rows <- list()

for (s in states) {
  tr_s <- train[as.character(train[[STATE_COL]]) == s, , drop = FALSE]
  te_s <- test[as.character(test[[STATE_COL]]) == s, , drop = FALSE]
  if (nrow(tr_s) == 0) next
  gtr <- hh_group_of(tr_s[[HH_SIZE_COL]])
  gte <- hh_group_of(te_s[[HH_SIZE_COL]])
  n_eval <- 0L
  
  for (R in RULES) {
    tr <- tr_s[!is.na(gtr) & gtr == R$hh, , drop = FALSE]
    if (nrow(tr) < MIN_TRAIN) next
    opt <- optimize_train(R, tr)
    if (!isTRUE(opt$feasible)) next
    n_eval <- n_eval + 1L
    
    te <- te_s[!is.na(gte) & gte == R$hh, , drop = FALSE]
    has_test <- nrow(te) >= MIN_TEST
    tst <- if (has_test) eval_test(R, opt$thr, te) else NULL
    
    rows[[length(rows) + 1L]] <- tibble(
      state = s, hh_size = R$hh, rule_id = R$rule_id,
      kept = !is.na(opt$precision) && opt$precision >= PREC_FLOOR && opt$n_flagged >= MIN_TRAIN_FLAGGED,
      n_train = nrow(tr), train_precision = round(opt$precision, 3),
      train_recall = round(opt$recall, 3), train_dollar_recall = round(opt$dollar_recall, 3),
      train_n_flagged = opt$n_flagged,
      n_test = nrow(te),
      test_precision = if (has_test) round(tst$precision, 3) else NA_real_,
      test_recall = if (has_test) round(tst$recall, 3) else NA_real_,
      test_dollar_recall = if (has_test) round(tst$dollar_recall, 3) else NA_real_,
      test_n_flagged = if (has_test) tst$n_flagged else NA_integer_,
      precision_drop = if (has_test) round(opt$precision - tst$precision, 3) else NA_real_,
      csv_precision = round(R$csv_prec, 3),
      optimized_rule = rebuild_rule(R, opt$thr)
    )
  }
  cat(sprintf("  state %-4s train %5d test %5d -> %d rules optimized\n",
              s, nrow(tr_s), nrow(te_s), n_eval))
}

results <- bind_rows(rows)
if (nrow(results) == 0) stop("No (state, rule) pairs met the minimum stratum sizes.")
results <- results %>% arrange(rule_id, state)

write.csv(results, file.path(out_dir, "CT_state_rule_train_2022_2023_2024_test_2018_2019.csv"), row.names = FALSE)
write.csv(results %>% filter(kept),
          file.path(out_dir, "CT_state_rule_train_test_kept.csv"), row.names = FALSE)

## ── 6. Stability by rule across states (kept rules with a test result) ────────

stab <- results %>%
  filter(kept, !is.na(test_precision)) %>%
  group_by(rule_id, hh_size) %>%
  summarise(
    n_states             = n(),
    mean_train_precision = round(mean(train_precision), 3),
    mean_test_precision  = round(mean(test_precision), 3),
    mean_drop            = round(mean(precision_drop), 3),
    sd_drop              = round(sd(precision_drop), 3),
    sd_test_precision    = round(sd(test_precision), 3),
    cor_train_test       = if (n() >= 3 && sd(train_precision) > 0 && sd(test_precision) > 0)
      round(cor(train_precision, test_precision), 3) else NA_real_,
    share_test_ge_floor  = round(mean(test_precision >= PREC_FLOOR), 3),
    .groups = "drop"
  ) %>%
  mutate(stability = dplyr::case_when(
    n_states < 2                                          ~ "single state",
    !is.na(sd_drop) & sd_drop <= STAB_SD &
      abs(mean_drop) <= STAB_GAP                          ~ "stable",
    TRUE                                                  ~ "unstable"
  )) %>%
  arrange(desc(n_states), desc(sd_drop))

write.csv(stab, file.path(out_dir, "rule_stability.csv"), row.names = FALSE)

## ── 7. Console summary ────────────────────────────────────────────────────────

cat(sprintf("\n(state, rule) pairs optimized: %d | kept (train precision >= %.2f, >= %d flagged): %d\n",
            nrow(results), PREC_FLOOR, MIN_TRAIN_FLAGGED, sum(results$kept)))
cat(sprintf("distinct rules kept in >=1 state: %d | states covered: %d\n",
            dplyr::n_distinct(results$rule_id[results$kept]),
            dplyr::n_distinct(results$state[results$kept])))

multi <- stab %>% filter(n_states >= 2)
cat(sprintf("\nRules kept in >=2 states: %d  (stable: %d, unstable: %d)\n",
            nrow(multi), sum(multi$stability == "stable"), sum(multi$stability == "unstable")))

cat("\n== Least stable rules (largest train->test variation across states) ==\n")
print(as.data.frame(head(multi, 15) %>%
                      select(rule_id, hh_size, n_states, mean_train_precision, mean_test_precision,
                             mean_drop, sd_drop, cor_train_test, share_test_ge_floor, stability)))

cat("\n== Most stable rules (test tracks train across states) ==\n")
print(as.data.frame(multi %>% filter(stability == "stable") %>%
                      arrange(sd_drop, abs(mean_drop)) %>% head(15) %>%
                      select(rule_id, hh_size, n_states, mean_train_precision, mean_test_precision,
                             mean_drop, sd_drop, cor_train_test, share_test_ge_floor)))

## ── 8. Notes ──────────────────────────────────────────────────────────────────
# - Selection is on TRAINING precision (>= PREC_FLOOR). TEST performance is reported
#   for every kept (state, rule), even when test precision is below the floor, so weak
#   out-of-sample behavior is visible rather than hidden.
# - Stability reads off the train->test precision gap across states: mean_drop is the
#   average optimism, sd_drop is how much that optimism varies state to state, and
#   cor_train_test asks whether states that look good in training also look good in test.
#   "unstable" = the gap is large or swings widely across states.
# - MIN_TRAIN_FLAGGED drops rules that flag only a handful of cases in a state: the
#   optimizer only considers threshold combinations that flag at least that many cases
#   on the state's training data, so tiny-support, high-precision artifacts are excluded.
# - Grids are shared across states (built from the full training data) so cross-state
#   comparison is apples-to-apples; only the chosen thresholds differ by state.
# - Runtime scales with states x rules x grid size. For a quick pass, set STATES to a
#   handful, or lower MAX_COMBOS / MAX_GRID_PTS.


## ── 4. Optional: test rules on hold-out years (pooled across all states) ──────
# Applies a set of mined rules (as written) to held-out data, each rule within its
# own household-size stratum, pooling every state together. Reuses flag_rule and
# inclusion_perf above. Pass any table that has a rule-text column and an hh_size
# column (e.g. rule_table_all or shortlist_all); if it also has an in-sample
# `precision` column, the result reports the train -> hold-out drop.
#
# Example:
#   holdout    <- reg_model_data %>% filter(fiscal_year %in% c("2018", "2019"))
#   holdout_hp <- test_rules_holdout(shortlist_all, holdout)
#   write.csv(holdout_hp, file.path(out_dir, "rules_holdout_pooled.csv"), row.names = FALSE)
