# ──────────────────────────────────────────────────────────────────────────────
# Household-size stratification on the v2 stack: no split vs 1/2-3/4+ vs
# 1/2/3/4/5+ — confirmation of the pre-era finding (split, but coarsely).
#
# Three schemes, identical engines (production settings) and identical
# downstream pipeline; the only difference is how household size partitions
# the data. In the pooled scheme household size remains available as a FEATURE
# (HH_size_n, cert-size), so the comparison isolates stratification itself.
#
# Frame: any_error; z = 2.326. Expects `reg_model_data`.
# Outputs -> methods/compare_hh_strata_v2/.
# ──────────────────────────────────────────────────────────────────────────────

library(dplyr)
library(ggplot2)
library(ranger)
library(xgboost)
source("rule_mining_helpers.R")
set.seed(117)

## ── 0. Config ─────────────────────────────────────────────────────────────────

YEAR_COL      <- "fiscal_year"
TRAIN_YEARS   <- c("2022", "2024")
HOLDOUT_YEARS <- c("2023")
TARGET_IS_ERROR <- quote(!is.na(over_threshold) & over_threshold != 0)
ERR_AMT_COL     <- "total_error_amount"
HH_SIZE_COL <- "cert_HH_size_FS_n"

SCHEMES <- list(
  "Pooled (no split)" = function(n) rep("all", length(n)),
  "1 / 2-3 / 4+" = function(n) {
    n <- suppressWarnings(as.numeric(as.character(n)))
    ifelse(is.na(n), NA_character_, ifelse(n <= 1, "1", ifelse(n <= 3, "2-3", "4+")))
  },
  "1 / 2 / 3 / 4 / 5+" = function(n) {
    n <- suppressWarnings(as.numeric(as.character(n)))
    g <- pmin(n, 5)
    ifelse(is.na(g), NA_character_, ifelse(g == 5, "5+", as.character(g)))
  }
)

features <- c(
  "HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size",
  "expedited_i", "cat_elig", "rawben_rel_max", "medical_deductions",
  "shelter_expenses_by_hh_size", "utilities", "married", "homeless",
  "rawearn_by_hh_size", "rawunearn_by_hh_size", "rawgross_by_hh_size",
  "percent_abawd", "unc_rawben_rel_max",
  "months_since_cert_n", "count_divisible_by_100"
)

XGB <- list(nrounds = 1000, max_depth = 4, eta = 0.02, subsample = 0.20)
RF  <- list(num_trees = 1000, max_depth = 4, mtry = 2, min_node_size = 20)

SIGNIF_DIGITS <- 3
OBJECTIVE     <- "dollars"
LCB_Z         <- 2.326
THRESHOLD_GRID    <- seq(0.05, 0.95, by = 0.05)
MIN_TRAIN_FLAGGED <- 10
PRUNE_MIN_PRECISION <- min(THRESHOLD_GRID)
MIN_PRECISION <- 0.20

out_dir <- "methods/compare_hh_strata_v2"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

## ── 1. Data ───────────────────────────────────────────────────────────────────

frame_df <- reg_model_data %>%
  filter(.data[[YEAR_COL]] %in% c(TRAIN_YEARS, HOLDOUT_YEARS))
pf  <- prep_features(frame_df, features)
fdf <- pf$data; pv <- pf$features
yr    <- as.character(fdf[[YEAR_COL]])
train <- fdf[yr %in% TRAIN_YEARS, , drop = FALSE]
hold  <- fdf[yr %in% HOLDOUT_YEARS, , drop = FALSE]
targets_of <- function(df) {
  ie <- eval(TARGET_IS_ERROR, envir = df); ie[is.na(ie)] <- FALSE
  amt <- df[[ERR_AMT_COL]]; amt[is.na(amt)] <- 0
  list(ie = ie, ed = ifelse(ie, abs(amt), 0))
}
tg_tr <- targets_of(train); tg_h <- targets_of(hold)

## ── 2. One scheme end-to-end ──────────────────────────────────────────────────

run_scheme <- function(nm) {
  gfn <- SCHEMES[[nm]]
  g_tr <- gfn(train[[HH_SIZE_COL]]); g_h <- gfn(hold[[HH_SIZE_COL]])
  lvls <- sort(unique(g_tr[!is.na(g_tr)]))
  strata_tr <- lapply(setNames(nm = lvls), function(h) which(g_tr %in% h))
  strata_h  <- lapply(setNames(nm = lvls), function(h) which(g_h %in% h))
  cat(sprintf("\n==== scheme: %s (%d strata) ====\n", nm, length(lvls)))
  t0 <- Sys.time()

  rules <- bind_rows(lapply(lvls, function(h) {
    ix <- strata_tr[[h]]; sub <- train[ix, , drop = FALSE]; ie_s <- tg_tr$ie[ix]
    cat(sprintf("-- stratum %s: %d rows, %d errors\n", h, length(ix), sum(ie_s)))
    rx <- canonicalize_rules(
      generate_rules_xgboost(sub, ie_s, pv, nrounds = XGB$nrounds,
                             max_depth = XGB$max_depth, eta = XGB$eta,
                             subsample = XGB$subsample, seed = 117), SIGNIF_DIGITS)
    rr <- canonicalize_rules(
      generate_rules_ranger(sub, ie_s, pv, num_trees = RF$num_trees,
                            max_depth = RF$max_depth, mtry = RF$mtry,
                            min_node_size = RF$min_node_size, seed = 117), SIGNIF_DIGITS)
    data.frame(rule = c(rx, rr), hh = h, stringsAsFactors = FALSE)
  })) %>% distinct(rule, hh)
  cat(sprintf("mined %d canonical rules in %.0fs\n", nrow(rules),
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))

  idx_tr <- flags_for_rules(rules, train, strata_tr)
  n_tr <- lengths(idx_tr)
  k_tr <- vapply(idx_tr, function(ix) sum(tg_tr$ie[ix]), numeric(1))
  raw  <- ifelse(n_tr > 0, k_tr / n_tr, NA_real_)
  base <- vapply(rules$hh, function(h) mean(tg_tr$ie[strata_tr[[h]]]), numeric(1))
  keep <- !is.na(raw) & n_tr >= MIN_TRAIN_FLAGGED &
          raw >= PRUNE_MIN_PRECISION & raw > base
  rules <- rules[keep, , drop = FALSE]; idx_tr <- idx_tr[keep]
  rules$stat <- wilson_lcb(k_tr[keep], n_tr[keep], LCB_Z)
  d1 <- dedup_exact_coverage(rules, idx_tr)
  rules <- rules[!d1, , drop = FALSE]; idx_tr <- idx_tr[!d1]
  d2 <- dedup_dominated(rules, rules$stat)
  rules <- rules[!d2, , drop = FALSE]
  cat(sprintf("screen+dedup -> %d rules | pass filter >= %.2f: %d\n",
              nrow(rules), MIN_PRECISION,
              sum(rules$stat >= MIN_PRECISION, na.rm = TRUE)))

  idx_h <- flags_for_rules(rules, hold, strata_h)
  sw <- precision_sweep(rules$stat, !is.na(rules$stat), idx_h, tg_h$ie, tg_h$ed,
                        THRESHOLD_GRID[THRESHOLD_GRID >= PRUNE_MIN_PRECISION])
  sw$x <- if (OBJECTIVE == "dollars") sw$dollar_recall else sw$recall
  list(sweep = sw %>% mutate(scheme = nm),
       info = data.frame(scheme = nm, n_rules = nrow(rules),
                         n_pass_filter = sum(rules$stat >= MIN_PRECISION, na.rm = TRUE),
                         total_secs = round(as.numeric(difftime(Sys.time(), t0, units = "secs")))))
}

runs <- lapply(names(SCHEMES), run_scheme)
sweeps <- bind_rows(lapply(runs, `[[`, "sweep"))
info   <- bind_rows(lapply(runs, `[[`, "info"))
sweeps$scheme <- factor(sweeps$scheme, levels = names(SCHEMES))
write.csv(sweeps, file.path(out_dir, "strata_sweeps.csv"), row.names = FALSE)

## ── 3. Summary + plot ─────────────────────────────────────────────────────────

interp_prec <- function(df, grid) {
  df <- df[!is.na(df$x) & !is.na(df$precision), ]
  if (nrow(df) < 2) return(rep(NA_real_, length(grid)))
  approx(df$x, df$precision, xout = grid, ties = mean, rule = 1)$y
}
grid <- seq(0.02, 0.5, by = 0.02)
summary_tbl <- sweeps %>% group_by(scheme) %>%
  summarise(mean_precision = round(mean(interp_prec(pick(everything()), grid), na.rm = TRUE), 4),
            recall_at_020 = round(x[threshold == 0.2][1], 3),
            precision_at_020 = round(precision[threshold == 0.2][1], 3),
            max_recall = round(max(x, na.rm = TRUE), 3), .groups = "drop") %>%
  left_join(info, by = "scheme")
write.csv(summary_tbl, file.path(out_dir, "strata_summary.csv"), row.names = FALSE)
cat("\nScheme summary (matched dollar recall 0.02-0.50):\n")
print(as.data.frame(summary_tbl), row.names = FALSE)

cols <- c("Pooled (no split)" = "#8c8c8c", "1 / 2-3 / 4+" = "#d1495b",
          "1 / 2 / 3 / 4 / 5+" = "#0073b7")
p <- ggplot(sweeps, aes(x, precision, color = scheme)) +
  geom_line(linewidth = 0.9) + geom_point(size = 1.1) +
  geom_text(aes(label = sprintf("%.2f", threshold)), size = 2.4, vjust = -0.75,
            show.legend = FALSE, check_overlap = TRUE) +
  scale_color_manual(values = cols) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Hold-out dollar recall of the union (all 2023 errors)",
       y = "Hold-out precision of the union", color = NULL,
       title = "Household-size stratification on the v2 stack",
       subtitle = "Same engines and filtering; only the partition differs. Pooled keeps HH size as a feature.\nany-error frame, trained 2022/2024, scored on 2023; point labels = LCB floor") +
  theme_minimal(base_size = 12) + theme(legend.position = "top")
save_png(p, file.path(out_dir, "strata_sweeps.png"), 9, 5.5)
cat(sprintf("\nWrote strata comparison to %s/\n", out_dir))
