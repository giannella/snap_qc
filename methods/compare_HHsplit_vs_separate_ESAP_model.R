# ──────────────────────────────────────────────────────────────────────────────
# Separate ESAP model test: precision-recall comparison of two stratification
# schemes (and nothing else — no pooled reference line):
#
#   - "1/2-3/4+"          household size collapsed to 1 / 2-3 / 4+ (baseline)
#   - "ESAP + 1/2-3/4+"   elderly/disabled cases (elderly_disabled_i == TRUE)
#                         are CARVED OUT first into their own "ESAP" stratum
#                         (any household size); the remaining cases are split
#                         1 / 2-3 / 4+ as in the baseline.
#
# The carve-out makes the four strata a clean partition of the data, mirroring
# how an ESAP-style process separates elderly/disabled households operationally,
# and keeps the two schemes directly comparable. Within
# the ESAP stratum elderly_disabled_i is constant and is automatically dropped
# from the predictors; in the baseline scheme it stays available as a feature,
# so the comparison shows whether a SEPARATE MODEL beats a mere FEATURE.
#
# For each scheme, rules are mined per stratum on TRAIN_YEARS with one shared
# RuleFit config. Instead of a greedy net, each point on a scheme's curve keeps
# EVERY rule whose TRAIN precision clears a threshold (sweeping the threshold
# over THRESHOLD_GRID) and scores the UNION of the kept rules' flags on
# HOLDOUT_YEARS (all states pooled). An error caught by several rules counts
# once in the union, so redundant rules never overstate recall — but no rule is
# penalized for re-catching errors other rules already found, since per-rule
# precision is never marginalized. Nothing is selected on the test data. The
# only thing that differs between the two curves is the stratification, so they
# isolate the effect of the ESAP carve-out.
#
# Produces (all prefixed "optimal_HH_split_test_separate_ESAP_model"):
#   1. overall precision-recall curves (one line per scheme), pooled across strata;
#   2. a Δ-precision panel: ESAP scheme MINUS the 1/2-3/4+ baseline over a common
#      recall grid (positive = the separate ESAP model beats the baseline);
#   3. a mean-precision summary table.
# Plus the underlying curve data as CSVs.
#
# Adapted from compare_hh_size_strata_schemes_model_performance.R; the only
# structural change is that group_fn now receives the whole data frame (it needs
# elderly_disabled_i as well as household size).
# ──────────────────────────────────────────────────────────────────────────────

library(pre)
library(dplyr)
library(ggplot2)
set.seed(111)

## ── 0. Config ─────────────────────────────────────────────────────────────────

income_overissuance_df <- reg_model_data %>%
  filter(error_status %in% c("earned_overissuance","unearned_overissuance", "no_error")) %>%
  filter(fiscal_year %in% c("2018","2019","2022","2023","2024"))

earned_income_df <- reg_model_data %>%
  filter(error_status %in% c("earned_overissuance", "no_error")) %>%
  filter(fiscal_year %in% c("2018","2019","2022","2023","2024"))

unearned_income_df <- reg_model_data %>%
  filter(error_status %in% c("unearned_overissuance", "no_error")) %>%
  filter(fiscal_year %in% c("2018","2019","2022","2023","2024"))

underissuance_df <- reg_model_data %>%
  filter(error_status %in% c("underissuance", "no_error")) %>%
  filter(fiscal_year %in% c("2018","2019","2022","2023","2024"))


DATA_DF       <- earned_income_df  # the labelled universe (errors + clean). Pre-filter
# it the same way script 6 does if you filter error_status.
YEAR_COL      <- "fiscal_year"
TRAIN_YEARS   <- c("2022","2024")
HOLDOUT_YEARS <- c("2023")

TARGET_IS_ERROR <- quote(!is.na(over_threshold) & over_threshold != 0)
ERR_AMT_COL     <- "total_error_amount"
OBJECTIVE       <- "counts"   # "dollars" or "counts"; sets the recall basis on the x-axis
PENALTY         <- "lambda.min"

# Train-precision threshold sweep: each curve point keeps every rule whose TRAIN
# precision >= the threshold; the union of kept rules is scored on the hold-out.
THRESHOLD_GRID    <- seq(0.05, 0.95, by = 0.05)
MIN_TRAIN_FLAGGED <- 5   # rules flagging fewer TRAIN cases than this are ignored
                         # (their train precision is too noisy to threshold on)

HH_SIZE_COL <- "HH_size_n"
ESAP_COL    <- "elderly_disabled_i"   # TRUE = elderly or disabled member in the HH

# ── Stratification schemes ────────────────────────────────────────────────────
# Each scheme has: a display name, the ordered factor levels, and a group_fn that
# maps a ROW OF THE DATA (the whole frame) to one of those levels. group_fn takes
# the data frame because the ESAP scheme needs elderly_disabled_i, not just
# household size. The BASELINE scheme is the reference in the Δ panel.
BASELINE_SCHEME <- "1/2-3/4+"

hh_split_of <- function(n) {
  n <- suppressWarnings(as.numeric(as.character(n)))
  ifelse(is.na(n), NA_character_,
         ifelse(n <= 1, "1",
                ifelse(n <= 3, "2-3", "4+")))
}

SCHEMES <- list(
  "1/2-3/4+" = list(
    levels   = c("1", "2-3", "4+"),
    group_fn = function(df) hh_split_of(df[[HH_SIZE_COL]])
  ),
  "ESAP + 1/2-3/4+" = list(
    levels   = c("ESAP", "1", "2-3", "4+"),
    group_fn = function(df) {
      esap <- df[[ESAP_COL]] %in% TRUE   # NA counts as non-ESAP
      ifelse(esap, "ESAP", hh_split_of(df[[HH_SIZE_COL]]))
    }
  )
)

MIN_STRATUM <- 30      # skip a hold-out stratum smaller than this
out_dir <- "compare_models_by_HHsize_vs_pooled"
FILE_PREFIX <- "optimal_HH_split_test_separate_ESAP_model"
out_path <- function(stem) file.path(out_dir, paste0(FILE_PREFIX, "_", stem))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
stopifnot(OBJECTIVE %in% c("dollars", "counts"))
stopifnot(BASELINE_SCHEME %in% names(SCHEMES))

# MUST match script 6 (paste your features vector and pre() settings here).
features <- c(
  "cert_HH_size_FS_n", "children_i", "elderly_disabled_i",
  "expedited_i", "cat_elig", "rawben_rel_max", "medical_deductions",
  "shelter_expenses_by_hh_size", "utilities", "married", "homeless",
  "earned_by_hh_size", "unearned_by_hh_size", "gross_by_hh_size",
  "percent_abawd", "unc_rawben_rel_max",
  "months_since_cert_n", "count_divisible_by_100"
)
RF <- list(ntrees = 2500,
           maxdepth = 4L,
           learnrate = 0.01,
           sampfrac = .2,
           randomForest=F,
           #mtry=2,
           use.grad          = TRUE,
           tree.unbiased     = FALSE,
           verbose=T)

#plot caption built from the RF list so it always matches the settings above
cap <- sprintf("RuleFit; ntrees=%s (rpart), depth=%d, sampfrac=%.2f, learnrate=%g; curves sweep train-precision thresholds",
               format(RF$ntrees, big.mark = ","), RF$maxdepth, RF$sampfrac, RF$learnrate)


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

# Fit RuleFit on a data subset and return the INCLUDE-direction rule strings.
mine_rules <- function(df, drop_hh = TRUE) {
  tg <- make_target(df); ie <- tg$ie; ed <- tg$ed
  df$.is_error <- ie

  pv <- if (drop_hh) setdiff(features, HH_SIZE_COL) else features
  pv <- pv[pv %in% names(df)]
  pv <- pv[sapply(df[pv], function(x) !all(is.na(x)) && length(unique(x[!is.na(x)])) > 1)]

  cc <- stats::complete.cases(df[c(".is_error", pv)])
  md <- df[cc, , drop = FALSE]; ed <- ed[cc]; ie <- md$.is_error
  if (nrow(md) < 30 || length(pv) == 0 || sum(ie) < 2) return(tibble(rule = character(0)))

  to_factor <- pv[vapply(md[pv], function(x) is.character(x) || is.logical(x), logical(1))]
  for (v in to_factor) md[[v]] <- factor(md[[v]])
  md[pv] <- lapply(md[pv], function(x) if (is.factor(x)) droplevels(x) else x)
  pv <- pv[vapply(md[pv], function(x) length(unique(x)) > 1, logical(1))]
  if (length(pv) == 0) return(tibble(rule = character(0)))

  if (OBJECTIVE == "dollars") { md$.target <- ed; fam <- "gaussian" }
  else { md$.target <- factor(ifelse(ie, "error", "clean"), levels = c("error", "clean")); fam <- "binomial" }

  cat(sprintf("  [%s] predictors = %d | rows = %d | errors = %d\n",
              if (drop_hh) "stratified" else "pooled", length(pv), nrow(md), sum(ie)))
  if (length(pv) < 2) {
    message("  fewer than 2 usable predictors after NA/constant filtering; skipping.")
    return(tibble(rule = character(0)))
  }

  fit <- tryCatch(
    pre(
      formula = as.formula(paste(".target ~", paste(pv, collapse = " + "))),
      data = md[c(".target", pv)], family = fam,
      ntrees = RF$ntrees, maxdepth = RF$maxdepth, learnrate = RF$learnrate, type = "rules",
      use.grad = TRUE, tree.unbiased = FALSE, sampfrac = RF$sampfrac,
      removeduplicates = TRUE, removecomplements = TRUE, nfolds = 5,
      randomForest = FALSE, verbose = FALSE
    ),
    error = function(e) { message("  pre() produced too few rules to fit (", conditionMessage(e), ")"); NULL }
  )
  if (is.null(fit)) return(tibble(rule = character(0)))
  gr <- function(pp) coef(fit, penalty.par.val = pp) %>% filter(rule != "(Intercept)", coefficient != 0)
  pen <- PENALTY; r0 <- gr(pen)
  if (nrow(r0) == 0 && pen == "lambda.1se") { pen <- "lambda.min"; r0 <- gr(pen) }
  if (nrow(r0) == 0) return(tibble(rule = character(0)))

  # keep INCLUDE-direction rules (matched subset dirtier / higher $ density than base)
  base_rate <- mean(ie); base_dens <- if (sum(ed) > 0) sum(ed) / length(ed) else NA_real_
  rules <- unique(r0$description)
  keep <- vapply(rules, function(rd) {
    f <- flag_rule(rd, md); if (sum(f) == 0) return(FALSE)
    if (OBJECTIVE == "dollars") (sum(ed[f]) / sum(f)) > base_dens else mean(ie[f]) > base_rate
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
sweep_thresholds <- function(flags_tr, flags_h, ie_tr, ie_h, ed_h) {
  if (length(flags_tr) == 0) return(tibble())
  n_tr    <- vapply(flags_tr, sum, numeric(1))
  prec_tr <- vapply(seq_along(flags_tr), function(i)
    if (n_tr[i] == 0) NA_real_ else sum(flags_tr[[i]] & ie_tr) / n_tr[i], numeric(1))
  usable  <- !is.na(prec_tr) & n_tr >= MIN_TRAIN_FLAGGED
  N_h <- length(ie_h); err_h <- sum(ie_h); dol_h <- sum(ed_h)

  out <- bind_rows(lapply(THRESHOLD_GRID, function(t) {
    keep <- which(usable & prec_tr >= t)
    if (length(keep) == 0) return(NULL)
    un  <- Reduce(`|`, flags_h[keep])
    nfl <- sum(un); tp <- sum(un & ie_h)
    tibble(threshold = t, n_rules = length(keep),
           n_flagged = nfl, workload = nfl / N_h,
           precision = if (nfl > 0) tp / nfl else NA_real_,
           recall = if (err_h > 0) tp / err_h else NA_real_,
           dollar_recall = if (dol_h > 0) sum(ed_h[un]) / dol_h else NA_real_)
  }))
  if (nrow(out) > 0) out$x <- if (OBJECTIVE == "dollars") out$dollar_recall else out$recall
  out
}

# Mine one stratified scheme, threshold its rules on TRAIN precision, score the
# union of kept rules on the hold-out; returns the sweep tibble tagged `approach`.
run_scheme <- function(scheme_name, scheme, train, hold) {
  lvls <- scheme$levels; gfn <- scheme$group_fn
  gtr  <- gfn(train)

  cat(sprintf("Mining scheme '%s' (%d strata) ...\n", scheme_name, length(lvls)))
  strat_rules <- bind_rows(lapply(lvls, function(h) {
    sub <- train[!is.na(gtr) & gtr == h, , drop = FALSE]
    mr  <- mine_rules(sub, drop_hh = TRUE)
    if (nrow(mr) > 0) mr$hh <- h
    mr
  }))
  cat(sprintf("  -> %d rules across %d strata\n",
              nrow(strat_rules), dplyr::n_distinct(strat_rules$hh)))
  if (nrow(strat_rules) == 0) return(list(rules = strat_rules, overall = tibble()))

  # per-rule precision is measured on TRAIN (stratum-restricted flags) ...
  tg_tr <- make_target(train); ie_tr <- tg_tr$ie
  flags_tr <- lapply(seq_len(nrow(strat_rules)), function(i)
    flag_rule(strat_rules$rule[i], train) & (gtr %in% strat_rules$hh[i]))

  # ... and the union of rules clearing each threshold is scored on the hold-out
  grp_h <- gfn(hold)
  tg_h  <- make_target(hold); ie_h <- tg_h$ie; ed_h <- tg_h$ed
  flags_h <- lapply(seq_len(nrow(strat_rules)), function(i)
    flag_rule(strat_rules$rule[i], hold) & (grp_h %in% strat_rules$hh[i]))

  overall <- sweep_thresholds(flags_tr, flags_h, ie_tr, ie_h, ed_h) %>%
    mutate(approach = scheme_name)
  list(rules = strat_rules %>% mutate(approach = scheme_name), overall = overall)
}

## ── 2. Split train / hold-out ─────────────────────────────────────────────────

yr    <- as.character(DATA_DF[[YEAR_COL]])
train <- DATA_DF[yr %in% as.character(TRAIN_YEARS), , drop = FALSE]
hold  <- DATA_DF[yr %in% as.character(HOLDOUT_YEARS), , drop = FALSE]
cat(sprintf("Train (%s): %d rows | Hold-out (%s): %d rows | states pooled\n",
            paste(TRAIN_YEARS, collapse = "/"), nrow(train),
            paste(HOLDOUT_YEARS, collapse = "/"), nrow(hold)))

## ── 3. Run both schemes ───────────────────────────────────────────────────────

scheme_runs <- lapply(names(SCHEMES), function(nm) run_scheme(nm, SCHEMES[[nm]], train, hold))
names(scheme_runs) <- names(SCHEMES)

overall_list <- lapply(scheme_runs, function(r) r$overall)
all_rules    <- bind_rows(lapply(scheme_runs, function(r) r$rules))

overall <- bind_rows(overall_list)
if (nrow(overall) == 0) stop("No rules mined for either scheme; cannot compare.")

# Lock a sensible plotting / legend order: schemes in config order.
approach_levels <- names(SCHEMES)
approach_levels <- approach_levels[approach_levels %in% unique(overall$approach)]
overall$approach <- factor(overall$approach, levels = approach_levels)

write.csv(overall, out_path("pr_overall_schemes.csv"), row.names = FALSE)

## ── 4. Δ-precision vs the baseline scheme over a common recall grid ────────────

interp_prec <- function(df, grid) {
  df <- df[!is.na(df$x) & !is.na(df$precision), ]
  if (nrow(df) < 2) return(rep(NA_real_, length(grid)))
  approx(df$x, df$precision, xout = grid, ties = mean, rule = 1)$y
}

base_curve <- overall %>% filter(approach == BASELINE_SCHEME)
if (nrow(base_curve) == 0) stop("Baseline scheme produced no curve; cannot compute deltas.")

# Common grid bounded by the shortest curve so we never extrapolate.
xmax <- suppressWarnings(min(tapply(overall$x, overall$approach,
                                    function(v) max(v, na.rm = TRUE)), na.rm = TRUE))
grid <- seq(0.02, ifelse(is.finite(xmax), xmax, 0.5), by = 0.02)

base_p <- interp_prec(base_curve, grid)
delta <- bind_rows(lapply(setdiff(approach_levels, BASELINE_SCHEME), function(nm) {
  cur <- overall %>% filter(approach == nm)
  tibble(x = grid,
         approach = nm,
         precision_scheme   = interp_prec(cur, grid),
         precision_baseline = base_p,
         delta = interp_prec(cur, grid) - base_p)
}))
delta$approach <- factor(delta$approach, levels = setdiff(approach_levels, BASELINE_SCHEME))
write.csv(delta, out_path("pr_delta_vs_baseline.csv"), row.names = FALSE)

## ── 5. Mean-precision summary across schemes ──────────────────────────────────

summary_tbl <- overall %>%
  group_by(approach) %>%
  summarise(
    mean_precision = mean(interp_prec(pick(everything()), grid), na.rm = TRUE),
    max_recall     = max(x, na.rm = TRUE),
    n_thresholds   = dplyr::n(),
    .groups = "drop"
  ) %>%
  mutate(delta_vs_baseline = mean_precision -
           mean_precision[approach == BASELINE_SCHEME]) %>%
  arrange(match(approach, approach_levels))
write.csv(summary_tbl, out_path("scheme_summary.csv"), row.names = FALSE)

cat("\nMean precision over common recall range (baseline = ", BASELINE_SCHEME, "):\n", sep = "")
print(as.data.frame(summary_tbl), digits = 3)
cat("(Positive delta_vs_baseline favours the separate ESAP model.)\n")

## ── 6. Plots ──────────────────────────────────────────────────────────────────

xlab <- if (OBJECTIVE == "dollars") "Recall of error dollars" else "Recall of errors"
pal  <- c("#1b1b1b", "#d1495b")
cols <- setNames(pal[seq_along(approach_levels)], approach_levels)

#if need to reset graphics device
graphics.off()   # close all
dev.list()       # should now be NULL


p1 <- ggplot(overall, aes(x, precision, color = approach)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.0) +
  geom_text(aes(label = sprintf("%.2f", threshold)), size = 2.4, vjust = -0.7,
            show.legend = FALSE, check_overlap = TRUE) +
  scale_color_manual(values = cols) +
  labs(x = xlab, y = "Hold-out precision of the union of kept rules",
       color = "Stratification",
       title = "Separate ESAP model vs 1/2-3/4+ - earned income overissuance",
       subtitle = sprintf("Trained %s, scored on %s hold-out, all states pooled; point labels = train-precision threshold",
                          paste(TRAIN_YEARS, collapse = "/"), paste(HOLDOUT_YEARS, collapse = "/")),
       caption = cap) +
  theme_minimal(base_size = 12) + theme(legend.position = "top")

p2 <- ggplot(delta, aes(x, delta, color = approach)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = cols[levels(delta$approach)]) +
  labs(x = xlab, y = sprintf("Δ precision (ESAP scheme − %s)", BASELINE_SCHEME),
       color = "Stratification",
       title = sprintf("Precision gap vs the %s scheme - earned income overissuance", BASELINE_SCHEME),
       caption = cap) +
  theme_minimal(base_size = 12) + theme(legend.position = "top")
graphics.off()

save_png <- function(plot, file, w, h, dpi = 300) {
  png(file, width = w, height = h, units = "in", res = dpi, type = "cairo")
  on.exit(dev.off()); print(plot)
}

save_png(p1, out_path("pr_overall_schemes.png"), 8, 5)
save_png(p2, out_path("pr_delta_vs_baseline.png"), 8, 3.6)

if (requireNamespace("patchwork", quietly = TRUE)) {
  combined <- patchwork::wrap_plots(p1, p2, ncol = 1, heights = c(2, 1.2))
  save_png(combined, out_path("pr_schemes_with_delta.png"), 8, 8)
}

cat(sprintf("\nWrote plots and curve CSVs to %s/ (prefix %s_)\n", out_dir, FILE_PREFIX))

## ── 7. Notes ──────────────────────────────────────────────────────────────────
# - Both schemes mine rules fresh on TRAIN per stratum with identical RuleFit
#   settings. Each curve point keeps every rule whose TRAIN precision clears
#   that point's threshold (rules flagging < MIN_TRAIN_FLAGGED train cases are
#   ignored); hold-out recall and precision are computed on the UNION of kept
#   rules' flags, so an error caught by several (redundant) rules counts once
#   and recall is never overstated. Nothing is selected on the test data. The
#   only difference between the two curves is whether elderly/disabled cases get
#   their own model or stay inside the household-size strata (where
#   elderly_disabled_i remains available as a predictor).
# - A stratified rule only flags cases in its own stratum (rule AND grp ==
#   stratum, where grp uses that scheme's group_fn), so the union mixes strata
#   into one portfolio directly comparable across schemes.
# - The carve-out means the ESAP scheme's "1"/"2-3"/"4+" strata contain only
#   non-elderly/disabled cases; its ESAP stratum spans all household sizes and
#   keeps cert_HH_size_FS_n as a predictor.
# - Read the Δ panel at the recall you would actually operate at, not just the
#   single mean-precision summary.
# - To run a different error type, set DATA_DF to one of the frames at the top
#   (unearned_income_df, income_overissuance_df, underissuance_df).
