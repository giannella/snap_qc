# ──────────────────────────────────────────────────────────────────────────────
# By-household-size rules vs pooled (all-HH) rules: precision-recall comparison
#
# Trains both approaches on TRAIN_YEARS, scores both on HOLDOUT_YEARS, pooling all
# states. For a fair, apples-to-apples comparison it re-mines BOTH rule sets here
# with one shared RuleFit config (set it to match your script 6), so the only thing
# that differs is whether rules are mined per household size or on all sizes at once.
#
# Produces:
#   1. overall precision-recall curves (two lines), pooled across households;
#   2. a Δ-precision panel (by-HH minus pooled) over a common recall grid;
#   3. per-household-size small multiples (both approaches in each facet).
# Plus the underlying curve data as CSVs.
#
# Nothing extra is required from your other scripts. Set DATA_DF to the same
# labelled frame script 6 uses, and copy `features`, the RF list, OBJECTIVE,
# PENALTY and NET_EPS so the fits match what you have been running.
# ──────────────────────────────────────────────────────────────────────────────

library(pre)
library(dplyr)
library(ggplot2)
set.seed(111)

## ── 0. Config ─────────────────────────────────────────────────────────────────

income_overissuance_df <- reg_model_data %>%
  filter(error_status %in% c("earned_overissuance","unearned_overissuance", "no_error")) %>%
  filter(fiscal_year %in% c("2018","2019","2022","2023","2024"))
table(income_overissuance_df$element, income_overissuance_df$error_status)
table(income_overissuance_df$over_threshold, income_overissuance_df$error_status)

earned_income_df <- reg_model_data %>%
  filter(error_status %in% c("earned_overissuance", "no_error")) %>%
  filter(fiscal_year %in% c("2018","2019","2022","2023","2024"))
table(earned_income_df$element, earned_income_df$error_status)
table(earned_income_df$over_threshold, earned_income_df$error_status)

unearned_income_df <- reg_model_data %>%
  filter(error_status %in% c("unearned_overissuance", "no_error")) %>%
  filter(fiscal_year %in% c("2018","2019","2022","2023","2024"))
table(unearned_income_df$element, unearned_income_df$error_status)
table(unearned_income_df$over_threshold, unearned_income_df$error_status)

underissuance_df <- reg_model_data %>%
  filter(error_status %in% c("underissuance", "no_error")) %>%
  filter(fiscal_year %in% c("2018","2019","2022","2023","2024"))
table(underissuance_df$element, underissuance_df$error_status)
table(underissuance_df$over_threshold, underissuance_df$error_status)


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

HH_SIZE_COL <- "cert_HH_size_FS_n"
HH_LEVELS   <- c("1", "2", "3", "4", "5+")
hh_group_of <- function(n) {
  n <- suppressWarnings(as.numeric(as.character(n)))
  g <- pmin(n, 5)
  ifelse(is.na(g), NA_character_, ifelse(g == 5, "5+", as.character(g)))
}

# MUST match script 6 (paste your features vector and pre() settings here).
features <- c(
  "cert_HH_size_FS_n", "children_i", "elderly_disabled_i",
  "expedited_i", "cat_elig", "rawben_rel_max", "medical_deductions",
  "shelter_expenses_by_hh_size", "utilities", "married", "homeless",
  "earned_by_hh_size", "unearned_by_hh_size", "gross_by_hh_size",
  "percent_abawd", "unc_rawben_rel_max",
  "months_since_cert_n", "count_divisible_by_100"
)
RF <- list(ntrees = 10000, 
           maxdepth = 4L, 
           learnrate = 0.005, 
           sampfrac = .25, 
           randomForest=F, 
           #mtry=2,
           use.grad          = TRUE,
           tree.unbiased     = FALSE,        
           verbose=T)

MIN_STRATUM <- 10      # skip a hold-out stratum smaller than this in the small multiples
out_dir <- "compare_models_by_HHsize_vs_pooled"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
stopifnot(OBJECTIVE %in% c("dollars", "counts"))

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

## ── 2. Mine both rule sets on the training years ──────────────────────────────

yr    <- as.character(DATA_DF[[YEAR_COL]])
train <- DATA_DF[yr %in% as.character(TRAIN_YEARS), , drop = FALSE]
hold  <- DATA_DF[yr %in% as.character(HOLDOUT_YEARS), , drop = FALSE]
cat(sprintf("Train (%s): %d rows | Hold-out (%s): %d rows | states pooled\n",
            paste(TRAIN_YEARS, collapse = "/"), nrow(train),
            paste(HOLDOUT_YEARS, collapse = "/"), nrow(hold)))

gtr <- hh_group_of(train[[HH_SIZE_COL]])

cat("Mining pooled (all-HH) rules ...\n")
pooled_rules <- mine_rules(train, drop_hh = FALSE) %>% mutate(hh = "ALL")

cat("Mining by-household-size rules ...\n")
strat_rules <- bind_rows(lapply(HH_LEVELS, function(h) {
  sub <- train[!is.na(gtr) & gtr == h, , drop = FALSE]
  mr  <- mine_rules(sub, drop_hh = TRUE)
  if (nrow(mr) > 0) mr$hh <- h
  mr
}))
cat(sprintf("  pooled rules: %d | stratified rules: %d (across %d strata)\n",
            nrow(pooled_rules), nrow(strat_rules), dplyr::n_distinct(strat_rules$hh)))
if (nrow(pooled_rules) == 0) cat("  WARNING: no pooled rules mined (check the data filter and RF settings).\n")
if (nrow(strat_rules) == 0)  stop("No stratified rules mined; cannot compare.")

## ── 3. Overall hold-out curves (pooled across households) ─────────────────────

tg_h <- make_target(hold); ie_h <- tg_h$ie; ed_h <- tg_h$ed
grp_h <- hh_group_of(hold[[HH_SIZE_COL]])

flags_pooled <- lapply(pooled_rules$rule, flag_rule, data = hold)
flags_strat  <- lapply(seq_len(nrow(strat_rules)), function(i)
  flag_rule(strat_rules$rule[i], hold) & (grp_h == strat_rules$hh[i]))

overall <- bind_rows(
  greedy_path(flags_pooled, ie_h, ed_h) %>% mutate(approach = "Pooled (all HH)"),
  greedy_path(flags_strat,  ie_h, ed_h) %>% mutate(approach = "By household size")
)
write.csv(overall, file.path(out_dir, "earn_inc_pr_overall.csv"), row.names = FALSE)

## ── 4. Per-household-size curves (small multiples) ────────────────────────────

per_hh <- bind_rows(lapply(HH_LEVELS, function(h) {
  sub <- hold[!is.na(grp_h) & grp_h == h, , drop = FALSE]
  if (nrow(sub) < MIN_STRATUM) return(NULL)
  tg <- make_target(sub); ieh <- tg$ie; edh <- tg$ed
  rr <- strat_rules$rule[strat_rules$hh == h]
  bind_rows(
    greedy_path(lapply(pooled_rules$rule, flag_rule, data = sub), ieh, edh) %>%
      mutate(approach = "Pooled (all HH)"),
    greedy_path(lapply(rr, flag_rule, data = sub), ieh, edh) %>%
      mutate(approach = "By household size")
  ) %>% mutate(hh_size = h)
}))
if (!is.null(per_hh) && nrow(per_hh) > 0)
  write.csv(per_hh, file.path(out_dir, "earn_inc_pr_by_hh.csv"), row.names = FALSE)

## ── 5. Δ-precision over a common recall grid ──────────────────────────────────

interp_prec <- function(df, grid) {
  df <- df[!is.na(df$x) & !is.na(df$precision), ]
  if (nrow(df) < 2) return(rep(NA_real_, length(grid)))
  approx(df$x, df$precision, xout = grid, ties = mean, rule = 1)$y
}
ps <- overall %>% filter(approach == "By household size")
pp <- overall %>% filter(approach == "Pooled (all HH)")
xmax  <- suppressWarnings(min(max(ps$x, na.rm = TRUE), max(pp$x, na.rm = TRUE)))
grid  <- seq(0.02, ifelse(is.finite(xmax), xmax, 0.5), by = 0.02)
delta <- tibble(x = grid,
                precision_byhh = interp_prec(ps, grid),
                precision_pooled = interp_prec(pp, grid)) %>%
  mutate(delta = precision_byhh - precision_pooled)
write.csv(delta, file.path(out_dir, "earn_inc_pr_delta.csv"), row.names = FALSE)

avg_byhh   <- mean(delta$precision_byhh, na.rm = TRUE)
avg_pooled <- mean(delta$precision_pooled, na.rm = TRUE)
cat(sprintf("\nMean precision over common recall range:  by-HH %.3f vs pooled %.3f  (Δ = %+.3f)\n",
            avg_byhh, avg_pooled, avg_byhh - avg_pooled))
cat("(Positive Δ favours stratifying; inspect the curves where they cross.)\n")

## ── 6. Plots ──────────────────────────────────────────────────────────────────

xlab  <- if (OBJECTIVE == "dollars") "Recall of error dollars" else "Recall of errors"
cols  <- c("By household size" = "#1b1b1b", "Pooled (all HH)" = "#8c8c8c")

#if need to reset graphics device
graphics.off()   # close all
dev.list()       # should now be NULL

p1 <- ggplot(overall, aes(x, precision, color = approach)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.1) +
  scale_color_manual(values = cols) +
  labs(x = xlab, y = "Precision (flagged that are errors)", color = NULL,
       title = "Stratified vs pooled rules - predicting earned income overissuance errors",
       subtitle = sprintf("Trained %s, scored on %s hold-out, all states pooled",
                          paste(TRAIN_YEARS, collapse = "/"), paste(HOLDOUT_YEARS, collapse = "/")),
       caption="RuleFit algorithm predicting error dollars; ntrees=10,000 (rpart), depth=4, sampfrac=.25, learnrate=.005") +
  theme_minimal(base_size = 12) + theme(legend.position = "top")

p2 <- ggplot(delta, aes(x, delta)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 0.8) +
  labs(x = xlab, y = "Δ precision (by-HH − pooled)",
       title = "Precision gap from stratifying - predicting earned income overissuance errors",
       caption="RuleFit algorithm predicting error dollars; ntrees=10,000 (rpart), depth=4, sampfrac=.25, learnrate=.005") +
  theme_minimal(base_size = 12)
graphics.off()

save_png <- function(plot, file, w, h, dpi = 300) {
  png(file, width = w, height = h, units = "in", res = dpi, type = "cairo")
  on.exit(dev.off()); print(plot)
}

save_png(p1, file.path(out_dir, "earn_inc_pr_overall.png"), 8, 5)
save_png(p2, file.path(out_dir, "earn_inc_pr_delta.png"),   8, 3.2)

if (!is.null(per_hh) && nrow(per_hh) > 0) {
  p3 <- ggplot(per_hh, aes(x, precision, color = approach)) +
    geom_line(linewidth = 0.7) +
    facet_wrap(~ factor(hh_size, levels = HH_LEVELS), scales = "free_x") +
    scale_color_manual(values = cols) +
    labs(x = xlab, y = "Precision", color = NULL, title = "By household size (hold-out) - predicting earned income overissuance errors", 
         caption="RuleFit algorithm predicting error dollars; ntrees=10,000 (rpart), depth=4, sampfrac=.25, learnrate=.005") +
    theme_minimal(base_size = 11) + theme(legend.position = "top")
  save_png(p3, file.path(out_dir, "earn_inc_pr_by_hh.png"), 9, 6)
}

if (requireNamespace("patchwork", quietly = TRUE)) {
  combined <- patchwork::wrap_plots(p1, p2, ncol = 1, heights = c(2, 1))
  save_png(combined, file.path(out_dir, "earn_inc_pr_overall_with_delta.png"), 8, 7.5)
}

file.exists(file.path(out_dir, "earn_inc_pr_overall.png"))
cat(sprintf("\nWrote plots and curve CSVs to %s/\n", out_dir))

## ── 7. Notes ──────────────────────────────────────────────────────────────────
# - Both rule sets are mined fresh on TRAIN with identical settings, so the curves
#   isolate the effect of stratifying. To compare the EXACT rules script 6 already
#   selected instead, replace the strat_rules line with your saved table, e.g.
#     strat_rules <- rule_table_all %>% filter(role == "INCLUDE") %>%
#                      transmute(rule, hh = hh_size)
#   (the pooled side still has to be mined here, since you have no pooled output).
# - The overall curves pool all households: a stratified rule only flags cases in
#   its own stratum (rule AND grp == hh), so the greedy mixes strata into one
#   portfolio directly comparable to the pooled model.
# - Curves are discrete operating points; the higher curve wins. Where they cross,
#   read the Δ panel at the recall you would actually operate at rather than the
#   single mean-precision summary.
# - For an operational view, swap `precision` for `workload` on the y-axis to see how
#   many cases each approach must review to reach a given recall.