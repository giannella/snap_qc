# ──────────────────────────────────────────────────────────────────────────────
# Comparing household-size STRATIFICATION SCHEMES: precision-recall comparison
#
# A generalization of compare_combined_vs_by_hh_size_model_performance.R. Instead
# of comparing one stratified scheme against the pooled model, this script compares
# SEVERAL ways of grouping household size against each other (and against pooled),
# so you can see whether collapsing strata costs or buys you anything relative to
# the standard 1 / 2 / 3 / 4 / 5+ scheme.
#
# Schemes compared (edit SCHEMES below to add/remove):
#   - "1/2/3/4/5+"  (baseline / reference)
#   - "1/2-3/4+"
#   - "1/2/3-4/5+"
#   - "Pooled (all HH)"  (no stratification; reference line)
#
# For each stratified scheme, rules are mined per stratum on TRAIN_YEARS with one
# shared RuleFit config, then OR-combined into a greedy net and scored on
# HOLDOUT_YEARS, pooling all states. The pooled model mines one rule set over all
# household sizes at once. The only thing that differs across lines is HOW
# household size is grouped, so the curves isolate the effect of the grouping.
#
# Produces:
#   1. overall precision-recall curves (one line per scheme), pooled across HH;
#   2. a Δ-precision panel: each stratified scheme MINUS the baseline 1/2/3/4/5+
#      scheme over a common recall grid (positive = the alternative beats baseline);
#   3. a mean-precision summary table across schemes.
# Plus the underlying curve data as CSVs.
#
# Set DATA_DF to the same labelled frame script 6 uses, and copy `features`, the
# RF list, OBJECTIVE, PENALTY and NET_EPS so the fits match what you have been
# running.
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
NET_EPS         <- 1

HH_SIZE_COL <- "HH_size_n"

# ── Stratification schemes ────────────────────────────────────────────────────
# Each scheme has: a display name, the ordered factor levels, and a group_fn that
# maps a raw household-size value to one of those levels. The BASELINE scheme is the
# reference everything is compared against in the Δ panel.
BASELINE_SCHEME <- "1/2/3/4/5+"

SCHEMES <- list(
  "1/2/3/4/5+" = list(
    levels   = c("1", "2", "3", "4", "5+"),
    group_fn = function(n) {
      n <- suppressWarnings(as.numeric(as.character(n)))
      g <- pmin(n, 5)
      ifelse(is.na(g), NA_character_, ifelse(g == 5, "5+", as.character(g)))
    }
  ),
  "1/2-3/4+" = list(
    levels   = c("1", "2-3", "4+"),
    group_fn = function(n) {
      n <- suppressWarnings(as.numeric(as.character(n)))
      ifelse(is.na(n), NA_character_,
             ifelse(n <= 1, "1",
                    ifelse(n <= 3, "2-3", "4+")))
    }
  ),
  "1/2/3-4/5+" = list(
    levels   = c("1", "2", "3-4", "5+"),
    group_fn = function(n) {
      n <- suppressWarnings(as.numeric(as.character(n)))
      ifelse(is.na(n), NA_character_,
             ifelse(n <= 1, "1",
                    ifelse(n == 2, "2",
                           ifelse(n <= 4, "3-4", "5+"))))
    }
  )
)

INCLUDE_POOLED <- F       # add the pooled (all-HH) model as a reference line
POOLED_NAME    <- "Pooled (all HH)"

MIN_STRATUM <- 30      # skip a hold-out stratum smaller than this
out_dir <- "compare_models_by_HHsize_vs_pooled"
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
RF <- list(ntrees = 5000,
           maxdepth = 4L,
           learnrate = 0.005,
           sampfrac = .2,
           randomForest=F,
           #mtry=2,
           use.grad          = TRUE,
           tree.unbiased     = FALSE,
           verbose=T)

#update plot info:
cap <- "RuleFit; ntrees=5,000 (rpart), depth=4, sampfrac=.20, learnrate=.005"


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

# Greedy net: OR rules to climb recall at the best value-per-clean trade, recording
# the cumulative operating point after each addition. Returns the (recall, precision,
# workload) path used to draw a curve.
greedy_path <- function(flag_list, ie, ed) {
  N <- length(ie); total_err <- sum(ie); total_dol <- sum(ed)
  if (length(flag_list) == 0 || total_err == 0) return(tibble())
  protect <- if (OBJECTIVE == "dollars") ed else as.numeric(ie)

  flagged <- rep(FALSE, N); remaining <- seq_along(flag_list); path <- list(); step <- 0L
  repeat {
    best <- NULL; best_score <- -Inf; best_new <- NULL
    for (k in remaining) {
      nf <- flagged | flag_list[[k]]; newc <- nf & !flagged
      if (!any(newc)) next
      sc <- sum(protect[newc]) / (sum(newc & !ie) + NET_EPS)
      if (sc > best_score) { best_score <- sc; best <- k; best_new <- nf }
    }
    if (is.null(best)) break
    flagged <- best_new; remaining <- setdiff(remaining, best); step <- step + 1L
    nfl <- sum(flagged); tp <- sum(flagged & ie)
    path[[step]] <- tibble(
      step = step, n_flagged = nfl, workload = nfl / N,
      recall = tp / total_err,
      dollar_recall = if (total_dol > 0) sum(ed[flagged]) / total_dol else NA_real_,
      precision = tp / nfl
    )
    if (length(remaining) == 0) break
  }
  out <- bind_rows(path)
  if (nrow(out) > 0) out$x <- if (OBJECTIVE == "dollars") out$dollar_recall else out$recall
  out
}

# Mine + score one stratified scheme; returns a greedy_path tibble tagged `approach`.
run_scheme <- function(scheme_name, scheme, train, hold) {
  lvls <- scheme$levels; gfn <- scheme$group_fn
  gtr  <- gfn(train[[HH_SIZE_COL]])

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

  grp_h <- gfn(hold[[HH_SIZE_COL]])
  tg_h  <- make_target(hold); ie_h <- tg_h$ie; ed_h <- tg_h$ed
  flags <- lapply(seq_len(nrow(strat_rules)), function(i)
    flag_rule(strat_rules$rule[i], hold) & (grp_h == strat_rules$hh[i]))

  overall <- greedy_path(flags, ie_h, ed_h) %>% mutate(approach = scheme_name)
  list(rules = strat_rules %>% mutate(approach = scheme_name), overall = overall)
}

## ── 2. Split train / hold-out ─────────────────────────────────────────────────

yr    <- as.character(DATA_DF[[YEAR_COL]])
train <- DATA_DF[yr %in% as.character(TRAIN_YEARS), , drop = FALSE]
hold  <- DATA_DF[yr %in% as.character(HOLDOUT_YEARS), , drop = FALSE]
cat(sprintf("Train (%s): %d rows | Hold-out (%s): %d rows | states pooled\n",
            paste(TRAIN_YEARS, collapse = "/"), nrow(train),
            paste(HOLDOUT_YEARS, collapse = "/"), nrow(hold)))

tg_h <- make_target(hold); ie_h <- tg_h$ie; ed_h <- tg_h$ed

## ── 3. Run every stratification scheme + pooled ───────────────────────────────

scheme_runs <- lapply(names(SCHEMES), function(nm) run_scheme(nm, SCHEMES[[nm]], train, hold))
names(scheme_runs) <- names(SCHEMES)

overall_list <- lapply(scheme_runs, function(r) r$overall)
all_rules    <- bind_rows(lapply(scheme_runs, function(r) r$rules))

if (INCLUDE_POOLED) {
  cat("Mining pooled (all-HH) rules ...\n")
  pooled_rules <- mine_rules(train, drop_hh = FALSE)
  cat(sprintf("  -> %d pooled rules\n", nrow(pooled_rules)))
  if (nrow(pooled_rules) > 0) {
    flags_pooled <- lapply(pooled_rules$rule, flag_rule, data = hold)
    overall_list[[POOLED_NAME]] <- greedy_path(flags_pooled, ie_h, ed_h) %>%
      mutate(approach = POOLED_NAME)
  } else {
    cat("  WARNING: no pooled rules mined (check the data filter and RF settings).\n")
  }
}

overall <- bind_rows(overall_list)
if (nrow(overall) == 0) stop("No rules mined for any scheme; cannot compare.")

# Lock a sensible plotting / legend order: schemes in config order, pooled last.
approach_levels <- c(names(SCHEMES), if (INCLUDE_POOLED) POOLED_NAME)
approach_levels <- approach_levels[approach_levels %in% unique(overall$approach)]
overall$approach <- factor(overall$approach, levels = approach_levels)

write.csv(overall, file.path(out_dir, "compare_strat_options_earn_inc_pr_overall_schemes.csv"), row.names = FALSE)

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
write.csv(delta, file.path(out_dir, "compare_strat_options_earn_inc_pr_delta_vs_baseline.csv"), row.names = FALSE)

## ── 5. Mean-precision summary across schemes ──────────────────────────────────

summary_tbl <- overall %>%
  group_by(approach) %>%
  summarise(
    mean_precision = mean(interp_prec(cur_data(), grid), na.rm = TRUE),
    max_recall     = max(x, na.rm = TRUE),
    n_steps        = dplyr::n(),
    .groups = "drop"
  ) %>%
  mutate(delta_vs_baseline = mean_precision -
           mean_precision[approach == BASELINE_SCHEME]) %>%
  arrange(match(approach, approach_levels))
write.csv(summary_tbl, file.path(out_dir, "compare_strat_options_earn_inc_scheme_summary.csv"), row.names = FALSE)

cat("\nMean precision over common recall range (baseline = ", BASELINE_SCHEME, "):\n", sep = "")
print(as.data.frame(summary_tbl), digits = 3)
cat("(Positive delta_vs_baseline favours that grouping over 1/2/3/4/5+.)\n")

## ── 6. Plots ──────────────────────────────────────────────────────────────────

xlab <- if (OBJECTIVE == "dollars") "Recall of error dollars" else "Recall of errors"
pal  <- c("#1b1b1b", "#d1495b", "#0073b7", "#2e8b57", "#8c8c8c", "#9467bd")
cols <- setNames(pal[seq_along(approach_levels)], approach_levels)

#if need to reset graphics device
graphics.off()   # close all
dev.list()       # should now be NULL


p1 <- ggplot(overall, aes(x, precision, color = approach)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.0) +
  scale_color_manual(values = cols) +
  labs(x = xlab, y = "Precision (flagged that are errors)", color = "HH-size grouping",
       title = "Household-size stratification schemes - earned income overissuance",
       subtitle = sprintf("Trained %s, scored on %s hold-out, all states pooled",
                          paste(TRAIN_YEARS, collapse = "/"), paste(HOLDOUT_YEARS, collapse = "/")),
       caption = cap) +
  theme_minimal(base_size = 12) + theme(legend.position = "top")

p2 <- ggplot(delta, aes(x, delta, color = approach)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = cols[levels(delta$approach)]) +
  labs(x = xlab, y = sprintf("Δ precision (scheme − %s)", BASELINE_SCHEME),
       color = "HH-size grouping",
       title = sprintf("Precision gap vs the %s scheme - earned income overissuance", BASELINE_SCHEME),
       caption = cap) +
  theme_minimal(base_size = 12) + theme(legend.position = "top")
graphics.off()

save_png <- function(plot, file, w, h, dpi = 300) {
  png(file, width = w, height = h, units = "in", res = dpi, type = "cairo")
  on.exit(dev.off()); print(plot)
}

save_png(p1, file.path(out_dir, "compare_strat_options_earn_inc_pr_overall_schemes.png"), 8, 5)
save_png(p2, file.path(out_dir, "compare_strat_options_earn_inc_pr_delta_vs_baseline.png"), 8, 3.6)

if (requireNamespace("patchwork", quietly = TRUE)) {
  combined <- patchwork::wrap_plots(p1, p2, ncol = 1, heights = c(2, 1.2))
  save_png(combined, file.path(out_dir, "compare_strat_options_earn_inc_pr_schemes_with_delta.png"), 8, 8)
}

cat(sprintf("\nWrote plots and curve CSVs to %s/\n", out_dir))

## ── 7. Notes ──────────────────────────────────────────────────────────────────
# - Every stratified scheme mines rules fresh on TRAIN per stratum with identical
#   RuleFit settings, then ORs them into a greedy net scored on the hold-out. The
#   pooled model mines one rule set over all HH sizes. So differences between lines
#   isolate the effect of HOW you group household size.
# - A stratified rule only flags cases in its own stratum (rule AND grp == hh, where
#   grp uses that scheme's group_fn), so the greedy mixes strata into one portfolio
#   directly comparable across schemes and to pooled.
# - The Δ panel compares each alternative grouping against BASELINE_SCHEME. Positive
#   means the alternative is more precise at that recall; read it at the recall you
#   would actually operate at, not just the single mean-precision summary.
# - To add another grouping (e.g. 1-2 / 3-4 / 5+), append an entry to SCHEMES with
#   its `levels` and a `group_fn` that maps raw HH size to those levels.
# - To run a different error type, set DATA_DF to one of the frames at the top
#   (unearned_income_df, income_overissuance_df, underissuance_df).
