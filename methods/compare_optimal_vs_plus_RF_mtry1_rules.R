# ──────────────────────────────────────────────────────────────────────────────
# Does ADDING random-forest (mtry = 1) rules to the optimal boosted models help?
#
# Compares two ways of building the inclusion-rule pool, both stratified by
# household size 1 / 2-3 / 4+ (cert_HH_size_FS_n collapsed):
#
#   A. "Optimal (boosted only)"  — pre() with the settings used by
#      INCL_find_inclusion_rules_multi_model_by_hh_size.R: 5,000 rpart trees,
#      learnrate .005, sampfrac .25, adaptive maxdepth (3 where errors < 500,
#      else 4). No class rebalancing (the 14:1 block is kept commented out
#      below, as intended in the INCL scripts).
#   B. "Optimal + RF (mtry=1)"   — the SAME boosted rules PLUS the rules from a
#      second pre() fit per stratum with randomForest = TRUE: rule induction is
#      delegated to Breiman & Cutler's {randomForest} package (pre() fixes
#      learnrate to 0 and ignores sampfrac), with mtry = 1 so each split
#      considers ONE randomly chosen predictor. That forces splits onto
#      predictors the boosted fit would never pick, so the RF rules explore
#      different variable combinations. Requires {randomForest} installed.
#
# RULE_SOURCE controls how each fit's rules are harvested: "lasso" keeps only
# rules pre()'s internal lasso selects (nonzero coefficients), while "all" keeps
# EVERY rule the ensemble generated, so the train-precision threshold is the
# only screen. The lasso optimizes joint predictive fit, not per-rule precision,
# and tends to discard small-coverage high-precision rules — exactly the kind
# the threshold sweep wants — so "all" is the default here. Either way the two
# rule sets are POOLED per stratum and we deliberately do NOT rerun a joint
# lasso over the pooled set: it would zero out correlated rules, and redundant
# high-precision rules are wanted — states may drop rules on expert knowledge,
# so overlapping alternatives that catch the same errors are useful.
#
# Evaluation is a TRAIN-PRECISION THRESHOLD SWEEP, not a greedy net: each curve
# point keeps EVERY rule whose TRAIN precision clears a threshold (sweeping over
# THRESHOLD_GRID) and scores the UNION of the kept rules' flags on
# HOLDOUT_YEARS (all states pooled). An error caught by several rules counts
# once in the union, so redundant rules never overstate recall — but no rule is
# penalized for re-catching errors other rules already found, since per-rule
# precision is never marginalized. Nothing is selected on the test data.
# Produces (all prefixed "optimal_vs_plus_RF_mtry1"):
#   1. hold-out precision-recall curves of the union at each threshold (A vs B);
#   2. a Δ-precision panel (B − A) over a common recall grid;
#   3. a mean-precision summary table;
#   4. the pooled per-rule evaluation (train precision + hold-out performance)
#      and the high-precision shortlist, thresholded on TRAIN precision
#      (deliberately redundant; `source` marks boosted / rf / both);
#   5. an ANY-ERROR scoring pass: the same rule unions re-scored against the
#      full holdout universe (every case, every error type) — frame-relative
#      precision understates deployment precision because a flagged case with a
#      different error type is an operational win, not a false positive.
# ──────────────────────────────────────────────────────────────────────────────

library(pre)
library(dplyr)
library(ggplot2)
if (!requireNamespace("randomForest", quietly = TRUE))
  stop("The RF variant needs the {randomForest} package: install.packages('randomForest')")
set.seed(117)


## ── 0. Config ─────────────────────────────────────────────────────────────────

earned_income_df <- reg_model_data %>%
  filter(error_status %in% c("earned_overissuance", "no_error")) %>%
  filter(fiscal_year %in% c("2018","2019","2022","2023","2024"))

unearned_income_df <- reg_model_data %>%
  filter(error_status %in% c("unearned_overissuance", "no_error")) %>%
  filter(fiscal_year %in% c("2018","2019","2022","2023","2024"))

underissuance_df <- reg_model_data %>%
  filter(error_status %in% c("underissuance", "no_error")) %>%
  filter(fiscal_year %in% c("2018","2019","2022","2023","2024"))

DATA_DF       <- earned_income_df   # set to the error type you want to test
YEAR_COL      <- "fiscal_year"
TRAIN_YEARS   <- c("2022","2024")
HOLDOUT_YEARS <- c("2023")

TARGET_IS_ERROR <- quote(!is.na(over_threshold) & over_threshold != 0)
ERR_AMT_COL     <- "total_error_amount"
OBJECTIVE       <- "counts"   # "dollars" or "counts"; recall basis for the net / x-axis
FIT_OBJECTIVE   <- "counts"   # mining target for pre(); "counts" (binomial) is the
                              # optimal pipeline's choice even when OBJECTIVE = "dollars"
PENALTY         <- "lambda.min"

# Rule pool source: "all" keeps every rule the ensemble generated (pre() has
# already removed exact duplicates and complements), letting the train-precision
# threshold do all the selecting; "lasso" restores pre()'s own selection
# (nonzero lasso coefficients only). "all" gives a much larger pool — expect
# thousands of candidate rules per stratum and a slower evaluation pass.
RULE_SOURCE <- "all"   # "all" or "lasso"

# Train-precision threshold sweep: each curve point keeps every rule whose TRAIN
# precision >= the threshold; the union of kept rules is scored on the hold-out.
THRESHOLD_GRID    <- seq(0.05, 0.95, by = 0.05)
MIN_TRAIN_FLAGGED <- 10  # rules flagging fewer TRAIN cases than this are ignored
                         # (their train precision is too noisy to threshold on)

# Thresholding statistic. "wilson_lcb" thresholds each rule on the lower bound
# of a one-sided Wilson interval for its train precision, which counteracts the
# winner's curse: at the same 0.20 point estimate, a 4/20 rule (LCB ~0.09) is
# penalized far more than a 40/200 rule (LCB ~0.16). "raw" uses the point
# estimate. Raw train precision is always reported alongside. The diagnosis on
# the 2026-07-04 run showed the train->holdout decay is almost entirely
# selection noise, which is exactly what the LCB corrects.
THRESHOLD_STAT <- "wilson_lcb"   # "wilson_lcb" or "raw"
LCB_Z          <- 1.645          # z for the one-sided interval (1.645 = 95%)

# Early pruning: rules with train precision below this are dropped at MINING
# time (their flags are already computed for the direction screen, so this is
# free) and never reach the expensive pooled-evaluation stage. At the default —
# the lowest sweep threshold — pruning is lossless for every output. Raising it
# (e.g., 0.10) shrinks the pool and speeds evaluation substantially, but sweep
# points below it would understate recall, so the sweep drops them automatically.
PRUNE_MIN_PRECISION <- min(THRESHOLD_GRID)

# Mining results are checkpointed to an .rds right after the fits; set TRUE to
# skip re-mining and jump straight to evaluation (e.g., after an evaluation-stage
# crash, or to re-run the scoring with different THRESHOLD_GRID / MIN_* settings).
# A runner script may pre-set this variable before source()ing.
if (!exists("RESUME_FROM_CHECKPOINT")) RESUME_FROM_CHECKPOINT <- FALSE

# Household-size stratification, as in the optimal INCL pipeline.
HH_SIZE_COL <- "cert_HH_size_FS_n"
HH_LEVELS   <- c("1", "2-3", "4+")
hh_group_of <- function(n) {
  n <- suppressWarnings(as.numeric(as.character(n)))
  ifelse(is.na(n), NA_character_, ifelse(n <= 1, "1", ifelse(n <= 3, "2-3", "4+")))
}

# Features from INCL_find_inclusion_rules_multi_model_by_hh_size.R (the pipeline
# whose settings define "optimal" here).
features <- c(
  "HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size",
  "expedited_i", "cat_elig", "rawben_rel_max", "medical_deductions",
  "shelter_expenses_by_hh_size", "utilities", "married", "homeless",
  "rawearn_by_hh_size", "rawunearn_by_hh_size", "rawgross_by_hh_size",
  "percent_abawd", "unc_rawben_rel_max",
  "months_since_cert_n", "count_divisible_by_100"
)

# A: existing optimal boosted settings.
BOOST <- list(ntrees = 2500, learnrate = 0.01, sampfrac = 0.2)
# B's extra fit: number of randomForest trees to mine rules from. More trees =
# more candidate rules for the lasso; adjust if run time is a problem.
RF_NTREES <- 2500

# Memory guidance: pre() builds a rules-by-rows model matrix for its internal
# lasso even though RULE_SOURCE = "all" ignores the lasso's result; sparse=TRUE
# (set in both pre() calls) shrinks that matrix roughly 5-10x. The RF fit on the
# largest stratum is the peak-memory step (~40 GB+ observed dense at 2,500 trees).
# On a 16 GB machine: keep sparse = TRUE, set RF_NTREES to ~1000, and consider
# nfolds = 3 in the pre() calls.

# CLEAN_PER_ERROR <- 14      # rebalancing ratio; only used if the (commented-out)
                             # rebalancing block in mine_rules() is re-enabled
MIN_STRATUM     <- 30        # skip a stratum smaller than this
MIN_SUPPORT     <- 0.000005  # shortlist: min share of the hold-out stratum a rule must flag
MIN_PRECISION   <- 0.20      # shortlist: min TRAIN precision (hold-out precision is
                             # reported per rule, not filtered on)

APPROACH_A <- "Optimal (boosted only)"
APPROACH_B <- "Optimal + RF (mtry = 1)"

out_dir <- "methods/compare_models_by_HHsize_vs_pooled"
FILE_PREFIX <- "optimal_vs_plus_RF_mtry1"
out_path <- function(stem) file.path(out_dir, paste0(FILE_PREFIX, "_", stem))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
stopifnot(OBJECTIVE %in% c("dollars", "counts"))
stopifnot(FIT_OBJECTIVE %in% c("dollars", "counts"))
stopifnot(RULE_SOURCE %in% c("all", "lasso"))
stopifnot(THRESHOLD_STAT %in% c("wilson_lcb", "raw"))

cap <- sprintf("Boosted: %s rpart trees, depth 3/4, lr %g, sampfrac %.2f | RF: %s randomForest trees, mtry=1 | thresholds on train %s",
               format(BOOST$ntrees, big.mark = ","), BOOST$learnrate, BOOST$sampfrac,
               format(RF_NTREES, big.mark = ","), THRESHOLD_STAT)

## ── 1. Helpers ────────────────────────────────────────────────────────────────

flag_rule <- function(rule, data) {
  out <- tryCatch(with(data, eval(parse(text = rule))), error = function(e) rep(NA, nrow(data)))
  out[is.na(out)] <- FALSE
  as.logical(out)
}

make_target <- function(df) {
  ie <- eval(TARGET_IS_ERROR, envir = df); ie[is.na(ie)] <- FALSE
  amt <- if (ERR_AMT_COL %in% names(df)) df[[ERR_AMT_COL]] else rep(0, nrow(df))
  amt[is.na(amt)] <- 0
  list(ie = ie, ed = ifelse(ie, abs(amt), 0))
}

# One-sided Wilson lower bound for a binomial proportion (vectorized over k, n).
wilson_lcb <- function(k, n, z = LCB_Z) {
  p <- ifelse(n > 0, k / n, NA_real_); z2 <- z * z
  ifelse(n > 0,
         (p + z2 / (2 * n) - z * sqrt(p * (1 - p) / n + z2 / (4 * n * n))) / (1 + z2 / n),
         NA_real_)
}

# The statistic rules are thresholded on: raw train precision or its Wilson LCB.
train_stat_of <- function(k, n) {
  if (THRESHOLD_STAT == "wilson_lcb") wilson_lcb(k, n)
  else ifelse(n > 0, k / n, NA_real_)
}

# Fit one pre() model on a stratum and return its INCLUDE-direction rule strings.
# variant = "boosted" reproduces the optimal pipeline; "rf" is the random-forest
# mode described in the header. Both share the same rebalancing and screening so
# the only difference between A and B is the extra RF rules.
mine_rules <- function(df, variant = c("boosted", "rf")) {
  variant <- match.arg(variant)
  tg <- make_target(df)
  df$.is_error <- tg$ie

  pv <- setdiff(features, HH_SIZE_COL)
  pv <- pv[pv %in% names(df)]
  pv <- pv[sapply(df[pv], function(x) !all(is.na(x)) && length(unique(x[!is.na(x)])) > 1)]

  cc <- stats::complete.cases(df[c(".is_error", pv)])
  md <- df[cc, , drop = FALSE]
  if (nrow(md) < MIN_STRATUM || length(pv) == 0 || sum(md$.is_error) < 2)
    return(tibble(rule = character(0)))

  # Optional class rebalancing (off by default): keep every error, sample
  # CLEAN_PER_ERROR clean cases per error. Uncomment to re-enable.
  # err_rows   <- md[md$.is_error, , drop = FALSE]
  # clean_rows <- md[!md$.is_error, , drop = FALSE]
  # n_clean    <- min(nrow(err_rows) * CLEAN_PER_ERROR, nrow(clean_rows))
  # md <- bind_rows(err_rows, sample_n(clean_rows, n_clean))
  # md <- md[sample.int(nrow(md)), , drop = FALSE]

  ie  <- md$.is_error
  amt <- if (ERR_AMT_COL %in% names(md)) md[[ERR_AMT_COL]] else rep(0, nrow(md))
  amt[is.na(amt)] <- 0
  ed  <- ifelse(ie, abs(amt), 0)

  to_factor <- pv[vapply(md[pv], function(x) is.character(x) || is.logical(x), logical(1))]
  for (v in to_factor) md[[v]] <- factor(md[[v]])
  md[pv] <- lapply(md[pv], function(x) if (is.factor(x)) droplevels(x) else x)
  pv <- pv[vapply(md[pv], function(x) length(unique(x)) > 1, logical(1))]
  if (length(pv) < 2) return(tibble(rule = character(0)))

  if (FIT_OBJECTIVE == "dollars") { md$.target <- ed; fam <- "gaussian" }
  else { md$.target <- factor(ifelse(ie, "error", "clean"), levels = c("error", "clean")); fam <- "binomial" }

  # Shallower trees where errors are scarce, as in the optimal pipeline.
  n_err <- sum(ie)
  maxd  <- if (n_err < 500) 3L else 4L
  form  <- as.formula(paste(".target ~", paste(pv, collapse = " + ")))
  cat(sprintf("  [%s] predictors = %d | rows = %d | errors = %d | maxdepth = %d\n",
              variant, length(pv), nrow(md), n_err, maxd))

  fit <- tryCatch(
    if (variant == "boosted") {
      pre(formula = form, data = md[c(".target", pv)], family = fam,
          ntrees = BOOST$ntrees, maxdepth = maxd, learnrate = BOOST$learnrate,
          sampfrac = BOOST$sampfrac, type = "rules",
          use.grad = TRUE, tree.unbiased = FALSE, sparse = TRUE,
          removeduplicates = TRUE, removecomplements = TRUE,
          nfolds = 5, verbose = FALSE)
    } else {
      # randomForest = TRUE: {randomForest} grows the trees (learnrate fixed to
      # 0, sampfrac ignored); ntrees, mtry and maxdepth are passed through.
      pre(formula = form, data = md[c(".target", pv)], family = fam,
          ntrees = RF_NTREES, maxdepth = maxd, mtry = 1,
          randomForest = TRUE, type = "rules", sparse = TRUE,
          removeduplicates = TRUE, removecomplements = TRUE,
          nfolds = 5, verbose = FALSE)
    },
    error = function(e) { message("  pre() failed (", conditionMessage(e), ")"); NULL }
  )
  if (is.null(fit)) return(tibble(rule = character(0)))

  if (RULE_SOURCE == "all") {
    # every rule the ensemble generated (pre() already removed exact duplicates
    # and complements); the train-precision threshold downstream is the only
    # selection, so small-coverage high-precision rules the lasso would zero
    # out stay in the pool
    rules <- if (is.null(fit$rules)) character(0) else unique(fit$rules$description)
  } else {
    gr <- function(pp) coef(fit, penalty.par.val = pp) %>% filter(rule != "(Intercept)", coefficient != 0)
    pen <- PENALTY; r0 <- gr(pen)
    if (nrow(r0) == 0 && pen == "lambda.1se") { pen <- "lambda.min"; r0 <- gr(pen) }
    rules <- unique(r0$description)
  }
  if (length(rules) == 0) return(tibble(rule = character(0)))
  cat(sprintf("    candidate rules (%s): %d\n", RULE_SOURCE, length(rules)))

  # keep INCLUDE-direction rules (flagged subset dirtier / denser than base)
  # with at least MIN_TRAIN_FLAGGED cases behind them and train precision >=
  # PRUNE_MIN_PRECISION — rules failing any of these can never enter the sweep
  # or shortlist, so dropping them here shrinks the pool before the expensive
  # evaluation stage
  base_rate <- mean(ie); base_dens <- if (sum(ed) > 0) sum(ed) / length(ed) else NA_real_
  keep <- vapply(rules, function(rd) {
    f <- flag_rule(rd, md); nf <- sum(f)
    if (nf < MIN_TRAIN_FLAGGED) return(FALSE)
    prec <- mean(ie[f])
    if (prec < PRUNE_MIN_PRECISION) return(FALSE)
    if (OBJECTIVE == "dollars") (sum(ed[f]) / nf) > base_dens else prec > base_rate
  }, logical(1))
  tibble(rule = rules[keep])
}

# Train-precision threshold sweep. For each threshold in THRESHOLD_GRID, keep
# every rule whose TRAIN precision clears it (redundant rules included), take
# the UNION of the kept rules' hold-out flags, and record that union's operating
# point. Counting through the union means an error caught by several rules
# counts exactly once, so recall is never overstated; there is no marginal
# scoring, so a rule is not penalized for re-catching errors other rules
# already found.
# Sparse implementation: flag lists hold INTEGER INDEX vectors (which()), not
# full logical vectors — with tens of thousands of RF-mined rules, dense storage
# would need tens of GB of RAM. The sweep walks thresholds from HIGH to LOW,
# adding newly-qualified rules to one running union, so the whole grid costs a
# single pass over the rule flags.
sweep_thresholds <- function(idx_tr, idx_h, ie_tr, ie_h, ed_h) {
  if (length(idx_tr) == 0) return(tibble())
  n_tr    <- lengths(idx_tr)
  k_tr    <- vapply(idx_tr, function(ix) sum(ie_tr[ix]), numeric(1))
  stat_tr <- train_stat_of(k_tr, n_tr)   # raw precision or Wilson LCB
  usable  <- !is.na(stat_tr) & n_tr >= MIN_TRAIN_FLAGGED
  N_h <- length(ie_h); err_h <- sum(ie_h); dol_h <- sum(ed_h)

  # sweep points below the mining-time prune floor would understate recall
  # (their low-precision rules were never pooled), so they are dropped. The
  # prune is on RAW precision and LCB <= raw, so this stays lossless under
  # THRESHOLD_STAT = "wilson_lcb".
  grid <- THRESHOLD_GRID[THRESHOLD_GRID >= PRUNE_MIN_PRECISION]

  un <- rep(FALSE, N_h); in_union <- rep(FALSE, length(idx_tr)); out <- list()
  for (t in sort(grid, decreasing = TRUE)) {
    new_rules <- which(usable & !in_union & stat_tr >= t)
    for (k in new_rules) un[idx_h[[k]]] <- TRUE
    in_union[new_rules] <- TRUE
    if (!any(in_union)) next
    nfl <- sum(un); tp <- sum(un & ie_h)
    out[[length(out) + 1L]] <- tibble(
      threshold = t, n_rules = sum(in_union),
      n_flagged = nfl, workload = nfl / N_h,
      precision = if (nfl > 0) tp / nfl else NA_real_,
      recall = if (err_h > 0) tp / err_h else NA_real_,
      dollar_recall = if (dol_h > 0) sum(ed_h[un]) / dol_h else NA_real_)
  }
  out <- bind_rows(out)
  if (nrow(out) > 0) {
    out <- out[order(out$threshold), , drop = FALSE]
    out$x <- if (OBJECTIVE == "dollars") out$dollar_recall else out$recall
  }
  out
}

## ── 2. Split train / hold-out ─────────────────────────────────────────────────

yr    <- as.character(DATA_DF[[YEAR_COL]])
train <- DATA_DF[yr %in% as.character(TRAIN_YEARS), , drop = FALSE]
hold  <- DATA_DF[yr %in% as.character(HOLDOUT_YEARS), , drop = FALSE]
cat(sprintf("Train (%s): %d rows | Hold-out (%s): %d rows | states pooled\n",
            paste(TRAIN_YEARS, collapse = "/"), nrow(train),
            paste(HOLDOUT_YEARS, collapse = "/"), nrow(hold)))

grp_tr <- hh_group_of(train[[HH_SIZE_COL]])
grp_h  <- hh_group_of(hold[[HH_SIZE_COL]])
tg_tr  <- make_target(train); ie_tr <- tg_tr$ie; ed_tr <- tg_tr$ed
tg_h   <- make_target(hold);  ie_h  <- tg_h$ie;  ed_h  <- tg_h$ed

## ── 3. Mine both rule sets per stratum ────────────────────────────────────────

ckpt <- out_path("rule_sets_checkpoint.rds")
if (RESUME_FROM_CHECKPOINT && file.exists(ckpt)) {
  rule_sets <- readRDS(ckpt)
  cat(sprintf("Resumed %d mined rules from %s (skipping the fits)\n", nrow(rule_sets), ckpt))
} else {
  rule_sets <- bind_rows(lapply(HH_LEVELS, function(h) {
    sub <- train[!is.na(grp_tr) & grp_tr == h, , drop = FALSE]
    cat(sprintf("\n#################### HOUSEHOLD SIZE %s (%d train rows) ####################\n",
                h, nrow(sub)))
    rb <- mine_rules(sub, "boosted"); if (nrow(rb) > 0) { rb$hh <- h; rb$source <- "boosted" }
    rr <- mine_rules(sub, "rf");      if (nrow(rr) > 0) { rr$hh <- h; rr$source <- "rf" }
    cat(sprintf("  -> boosted rules: %d | rf rules: %d\n", nrow(rb), nrow(rr)))
    bind_rows(rb, rr)
  }))
  if (nrow(rule_sets) == 0) stop("No rules mined; cannot compare.")

  # A rule found by both fits collapses to one row tagged "both"; overlapping
  # (but non-identical) rules are kept on purpose.
  rule_sets <- rule_sets %>%
    group_by(hh, rule) %>%
    summarise(source = if (dplyr::n_distinct(source) > 1) "both" else dplyr::first(source),
              .groups = "drop")
  saveRDS(rule_sets, ckpt)
  cat(sprintf("Mined rules checkpointed to %s\n", ckpt))
}
cat(sprintf("\nPooled rule set: %d rules (%s)\n", nrow(rule_sets),
            paste(sprintf("%s: %d", names(table(rule_sets$source)), table(rule_sets$source)),
                  collapse = ", ")))

## ── 4. Train-precision threshold sweep, scored on the hold-out ────────────────

# A stratified rule only flags cases in its own stratum. Flags are stored as
# integer index vectors (which()) to keep memory flat regardless of pool size.
flag_idx_for <- function(rdf, data, grp) {
  n <- nrow(rdf); out <- vector("list", n)
  for (i in seq_len(n)) {
    out[[i]] <- which(flag_rule(rdf$rule[i], data) & (grp %in% rdf$hh[i]))
    if (i %% 5000 == 0) cat(sprintf("    flag evaluation: %d / %d rules\n", i, n))
  }
  out
}

cat(sprintf("\nEvaluating %d pooled rules on train ...\n", nrow(rule_sets)))
idx_tr_all <- flag_idx_for(rule_sets, train, grp_tr)
cat(sprintf("Evaluating %d pooled rules on hold-out ...\n", nrow(rule_sets)))
idx_h_all  <- flag_idx_for(rule_sets, hold,  grp_h)

# Per-rule TRAIN stats, kept on rule_sets so section 6 can report and shortlist
# on them. The threshold is applied to train precision only; hold-out precision
# is reported, never filtered on.
rule_sets$n_flagged_train <- lengths(idx_tr_all)
rule_sets$errors_train    <- vapply(idx_tr_all, function(ix) sum(ie_tr[ix]), numeric(1))
rule_sets$precision_train <- ifelse(rule_sets$n_flagged_train > 0,
                                    rule_sets$errors_train / rule_sets$n_flagged_train, NA_real_)
rule_sets$precision_train_lcb <- wilson_lcb(rule_sets$errors_train, rule_sets$n_flagged_train)

idx_A <- which(rule_sets$source %in% c("boosted", "both"))
idx_B <- seq_len(nrow(rule_sets))

overall <- bind_rows(
  sweep_thresholds(idx_tr_all[idx_A], idx_h_all[idx_A], ie_tr, ie_h, ed_h) %>%
    mutate(approach = APPROACH_A),
  sweep_thresholds(idx_tr_all[idx_B], idx_h_all[idx_B], ie_tr, ie_h, ed_h) %>%
    mutate(approach = APPROACH_B)
)
if (nrow(overall) == 0) stop("No rules cleared any train-precision threshold.")

approach_levels <- c(APPROACH_A, APPROACH_B)
approach_levels <- approach_levels[approach_levels %in% unique(overall$approach)]
overall$approach <- factor(overall$approach, levels = approach_levels)

write.csv(overall, out_path("pr_overall.csv"), row.names = FALSE)

## ── 4b. Any-error scoring pass ────────────────────────────────────────────────
# Frame-relative metrics understate deployment performance: a flagged case with
# a DIFFERENT error type counts as clean inside the frame but is an operational
# win. Re-score the same rule pools (thresholds still set by in-frame TRAIN
# precision) against the FULL holdout universe — every case in HOLDOUT_YEARS,
# with is_error = any over-threshold error regardless of type. The recall
# denominator changes accordingly: recall = share of ALL holdout errors caught.

univ  <- reg_model_data %>% filter(fiscal_year %in% HOLDOUT_YEARS)
ot_u  <- suppressWarnings(as.numeric(as.character(univ$over_threshold)))
ie_u  <- !is.na(ot_u) & ot_u != 0
amt_u <- univ[[ERR_AMT_COL]]; amt_u[is.na(amt_u)] <- 0
ed_u  <- ifelse(ie_u, abs(amt_u), 0)
grp_u <- hh_group_of(univ[[HH_SIZE_COL]])
cat(sprintf("\nAny-error universe (%s): %d rows | errors of any type: %d (%.2f%%)\n",
            paste(HOLDOUT_YEARS, collapse = "/"), nrow(univ), sum(ie_u), 100 * mean(ie_u)))

cat(sprintf("Evaluating %d pooled rules on the any-error universe ...\n", nrow(rule_sets)))
idx_u_all <- flag_idx_for(rule_sets, univ, grp_u)

overall_any <- bind_rows(
  sweep_thresholds(idx_tr_all[idx_A], idx_u_all[idx_A], ie_tr, ie_u, ed_u) %>%
    mutate(approach = APPROACH_A),
  sweep_thresholds(idx_tr_all[idx_B], idx_u_all[idx_B], ie_tr, ie_u, ed_u) %>%
    mutate(approach = APPROACH_B)
)
overall_any$approach <- factor(overall_any$approach, levels = approach_levels)
write.csv(overall_any, out_path("pr_overall_anyerror.csv"), row.names = FALSE)

## ── 5. Δ-precision (B − A) over a common recall grid ──────────────────────────

interp_prec <- function(df, grid) {
  df <- df[!is.na(df$x) & !is.na(df$precision), ]
  if (nrow(df) < 2) return(rep(NA_real_, length(grid)))
  approx(df$x, df$precision, xout = grid, ties = mean, rule = 1)$y
}

xmax <- suppressWarnings(min(tapply(overall$x, overall$approach,
                                    function(v) max(v, na.rm = TRUE)), na.rm = TRUE))
grid <- seq(0.02, ifelse(is.finite(xmax), xmax, 0.5), by = 0.02)

curve_A <- overall %>% filter(approach == APPROACH_A)
curve_B <- overall %>% filter(approach == APPROACH_B)
delta <- tibble(x = grid,
                approach = APPROACH_B,
                precision_plus_rf = interp_prec(curve_B, grid),
                precision_boosted = interp_prec(curve_A, grid),
                delta = interp_prec(curve_B, grid) - interp_prec(curve_A, grid))
write.csv(delta, out_path("pr_delta.csv"), row.names = FALSE)

summary_tbl <- overall %>%
  group_by(approach) %>%
  summarise(
    mean_precision = mean(interp_prec(pick(everything()), grid), na.rm = TRUE),
    max_recall     = max(x, na.rm = TRUE),
    n_thresholds   = dplyr::n(),
    .groups = "drop"
  ) %>%
  mutate(delta_vs_boosted = mean_precision - mean_precision[approach == APPROACH_A]) %>%
  arrange(match(approach, approach_levels))
write.csv(summary_tbl, out_path("summary.csv"), row.names = FALSE)

cat("\nMean precision over common recall range:\n")
print(as.data.frame(summary_tbl), digits = 3)
cat("(Positive delta_vs_boosted means the extra RF rules helped.)\n")

## ── 6. Pooled per-rule evaluation + high-precision shortlist ──────────────────
# This is the output that matters most: every pooled rule with its TRAIN
# precision (the thresholding basis) and its individual hold-out performance.
# Per-rule hold-out precision/recall are computed on each rule's OWN flag set —
# a rule is not penalized for re-catching errors other rules already found.
# Overlapping rules are DELIBERATELY kept — states may drop rules on expert
# knowledge, so redundant alternatives that catch the same errors are useful.
# `source` shows where each rule came from.

rule_eval <- bind_rows(lapply(seq_len(nrow(rule_sets)), function(i) {
  in_h  <- grp_h %in% rule_sets$hh[i]
  fi    <- idx_h_all[[i]]
  n_flag <- length(fi); tp <- sum(ie_h[fi])
  n_str  <- sum(in_h); err_str <- sum(ie_h & in_h); dol_str <- sum(ed_h[in_h])
  tibble(hh_size = rule_sets$hh[i], rule = rule_sets$rule[i], source = rule_sets$source[i],
         n_flagged_train = rule_sets$n_flagged_train[i],
         precision_train = round(rule_sets$precision_train[i], 3),
         precision_train_lcb = round(rule_sets$precision_train_lcb[i], 3),
         n_flagged = n_flag,
         workload_pct = round(100 * n_flag / max(n_str, 1), 2),
         errors_caught = tp, clean_flagged = n_flag - tp,
         precision_holdout = if (n_flag > 0) round(tp / n_flag, 3) else NA_real_,
         recall_holdout    = if (err_str > 0) round(tp / err_str, 3) else NA_real_,
         dollar_recall     = if (dol_str > 0) round(sum(ed_h[fi]) / dol_str, 3) else NA_real_)
}))

# deployment view: each rule's hold-out precision against errors of ANY type
rule_eval$precision_any_holdout <- vapply(seq_len(nrow(rule_sets)), function(i) {
  fi <- idx_u_all[[i]]
  if (length(fi) == 0) NA_real_ else round(sum(ie_u[fi]) / length(fi), 3)
}, numeric(1))

write.csv(rule_eval, out_path("holdout_rule_eval_combined.csv"), row.names = FALSE)

# Threshold on the TRAIN statistic (raw precision or Wilson LCB, per
# THRESHOLD_STAT); hold-out precision is reported alongside so you can see how
# each rule generalizes.
rule_eval$train_stat <- if (THRESHOLD_STAT == "wilson_lcb")
  rule_eval$precision_train_lcb else rule_eval$precision_train
shortlist <- rule_eval %>%
  filter(!is.na(train_stat), train_stat >= MIN_PRECISION,
         n_flagged_train >= MIN_TRAIN_FLAGGED,
         workload_pct / 100 >= MIN_SUPPORT) %>%
  arrange(hh_size, desc(train_stat))
write.csv(shortlist, out_path("highprecision_rules_combined.csv"), row.names = FALSE)

cat(sprintf("\nHigh-precision shortlist (train %s >= %.2f): %d rules (%s)\n",
            THRESHOLD_STAT, MIN_PRECISION, nrow(shortlist),
            paste(sprintf("%s: %d", names(table(shortlist$source)), table(shortlist$source)),
                  collapse = ", ")))

## ── 7. Plots ──────────────────────────────────────────────────────────────────

xlab <- if (OBJECTIVE == "dollars") "Recall of error dollars" else "Recall of errors"
cols <- setNames(c("#1b1b1b", "#0073b7"), approach_levels)

#if need to reset graphics device
graphics.off()   # close all
dev.list()       # should now be NULL

p1 <- ggplot(overall, aes(x, precision, color = approach)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.0) +
  geom_text(aes(label = sprintf("%.2f", threshold)), size = 2.4, vjust = -0.7,
            show.legend = FALSE, check_overlap = TRUE) +
  scale_color_manual(values = cols) +
  labs(x = xlab, y = "Hold-out precision of the union of kept rules",
       color = "Rule pool",
       title = "Adding RF (mtry = 1) rules to the optimal boosted models - earned income overissuance",
       subtitle = sprintf("Trained %s, scored on %s hold-out, all states pooled; point labels = train-precision threshold",
                          paste(TRAIN_YEARS, collapse = "/"), paste(HOLDOUT_YEARS, collapse = "/")),
       caption = cap) +
  theme_minimal(base_size = 12) + theme(legend.position = "top")

p2 <- ggplot(delta, aes(x, delta)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 0.8, color = "#0073b7") +
  labs(x = xlab, y = "Δ precision (with RF − boosted only)",
       title = "Precision gap from adding the RF (mtry = 1) rules",
       caption = cap) +
  theme_minimal(base_size = 12)
graphics.off()

save_png <- function(plot, file, w, h, dpi = 300) {
  png(file, width = w, height = h, units = "in", res = dpi, type = "cairo")
  on.exit(dev.off()); print(plot)
}

save_png(p1, out_path("pr_overall.png"), 8, 5)
save_png(p2, out_path("pr_delta.png"), 8, 3.6)

# frame-relative vs any-error scoring of the same unions. NOTE the x-axis
# denominator follows the scoring: frame errors only vs ALL holdout errors.
scored <- bind_rows(
  overall     %>% mutate(scoring = "frame only (mined error type)"),
  overall_any %>% mutate(scoring = "any error type"))
p3 <- ggplot(scored, aes(x, precision, color = approach, linetype = scoring)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.0) +
  scale_color_manual(values = cols) +
  labs(x = "Recall (denominator follows scoring: frame errors vs ALL errors)",
       y = "Hold-out precision of the union of kept rules",
       color = "Rule pool", linetype = "Scored against",
       title = "Frame-relative vs any-error scoring of the same rule unions",
       subtitle = "Any-error: flagged cases with a different error type count as wins; recall is over ALL holdout errors",
       caption = cap) +
  theme_minimal(base_size = 12) + theme(legend.position = "top")
save_png(p3, out_path("pr_overall_anyerror.png"), 8, 5.5)

if (requireNamespace("patchwork", quietly = TRUE)) {
  combined <- patchwork::wrap_plots(p1, p2, ncol = 1, heights = c(2, 1.2))
  save_png(combined, out_path("pr_with_delta.png"), 8, 8)
}

cat(sprintf("\nWrote plots, curve CSVs and rule lists to %s/ (prefix %s_)\n", out_dir, FILE_PREFIX))

## ── 8. Notes ──────────────────────────────────────────────────────────────────
# - Approach A reproduces the optimal INCL pipeline's mining (same settings and
#   INCLUDE-direction screen; no rebalancing); approach B is A's rules plus the
#   RF (mtry = 1) rules, so the curves isolate what the extra rules buy.
# - The RF variant sets randomForest = TRUE, which delegates rule induction to
#   Breiman & Cutler's {randomForest} package: pre() fixes learnrate to 0,
#   ignores sampfrac, and passes ntrees / mtry / maxdepth through to
#   randomForest(). The extracted rule text evaluates the same way downstream.
# - RULE_SOURCE = "all" bypasses pre()'s lasso selection entirely: the lasso
#   optimizes joint predictive fit and discards small-coverage high-precision
#   rules that contribute little deviance, which are exactly the rules the
#   threshold sweep values. "lasso" restores the selected-rules-only behavior
#   for comparison. In neither case is a joint lasso run over the pooled rules:
#   a joint refit would prune the redundant high-precision alternatives this
#   exercise is designed to generate.
# - Rule flags are stored as integer index vectors (sparse), so memory stays
#   flat even with 100k+ pooled rules, and the mined rule set is checkpointed to
#   *_rule_sets_checkpoint.rds — set RESUME_FROM_CHECKPOINT = TRUE to redo the
#   evaluation without re-mining.
# - Each curve point keeps every rule whose TRAIN precision clears that point's
#   threshold (rules flagging < MIN_TRAIN_FLAGGED train cases are ignored);
#   hold-out recall and precision are computed on the UNION of kept rules'
#   flags, so an error caught by several (redundant) rules counts exactly once
#   and recall is never overstated. Nothing is selected on the test data, which
#   keeps the comparison fair even though B's pool is a superset of A's.
# - Per-rule hold-out stats in section 6 are computed on each rule's OWN flag
#   set (not marginal), so redundancy is never penalized there; recall /
#   dollar_recall are WITHIN-STRATUM (errors in that rule's household-size
#   stratum), matching the per-stratum model design.
# - The any-error pass (section 4b, *_anyerror outputs, precision_any_holdout
#   column) re-scores the same unions against ALL over-threshold errors in the
#   holdout, regardless of type. Expect precision_any >= frame precision (extra
#   wins from other error types) but recall_any << frame recall (the
#   denominator grows to include errors these rules were never mined for).
# - To run a different error type, set DATA_DF to one of the frames at the top.
