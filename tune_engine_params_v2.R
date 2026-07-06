# ──────────────────────────────────────────────────────────────────────────────
# Engine parameter tuning for the v2 rule-mining stack (successor to
# optimize_rulefit_params.R, which tuned pre()).
#
# One-at-a-time sweeps around a BASE config for each engine (xgboost, ranger):
# every setting mines rules on TRAIN, runs the standard screen -> dedup ->
# Wilson-LCB threshold sweep, and is scored by its HOLD-OUT union
# precision-recall frontier — the metric the production drivers optimize —
# plus its mining wall time, so cost/benefit is visible (the 2026-07-05
# head-to-head showed 5x trees buys recall extension, not matched-recall
# precision; this script quantifies that per parameter).
#
# Expects `reg_model_data`. Outputs land in parameter_tuning_v2/.
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

# Tuning frame: "any_error" (all cases, target = any over-threshold error;
# most stable base rate) or one of the typed statuses.
FRAME <- "any_error"   # or "earned_overissuance", "unearned_overissuance",
                       #    "underissuance", "other_error"

TARGET_IS_ERROR <- quote(!is.na(over_threshold) & over_threshold != 0)
ERR_AMT_COL     <- "total_error_amount"

HH_SIZE_COL <- "cert_HH_size_FS_n"
HH_LEVELS   <- c("1", "2-3", "4+")
hh_group_of <- function(n) {
  n <- suppressWarnings(as.numeric(as.character(n)))
  ifelse(is.na(n), NA_character_, ifelse(n <= 1, "1", ifelse(n <= 3, "2-3", "4+")))
}

features <- c(
  "HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size",
  "expedited_i", "cat_elig", "rawben_rel_max", "medical_deductions",
  "shelter_expenses_by_hh_size", "utilities", "married", "homeless",
  "rawearn_by_hh_size", "rawunearn_by_hh_size", "rawgross_by_hh_size",
  "percent_abawd", "unc_rawben_rel_max",
  "months_since_cert_n", "count_divisible_by_100"
)

# Baselines (the point each one-at-a-time sweep moves away from) and the swept
# values. Edit freely; identical configs (the base re-appearing in its own
# sweep) are deduplicated automatically.
BASE_XGB  <- list(nrounds = 300, max_depth = 4, eta = 0.05, subsample = 0.5)
XGB_SWEEP <- list(nrounds   = c(100, 300, 1000),
                  max_depth = c(3, 4, 5),
                  eta       = c(0.02, 0.05, 0.10),
                  subsample = c(0.25, 0.50, 0.75))

BASE_RF  <- list(num_trees = 500, max_depth = 4, mtry = 1, min_node_size = 20)
RF_SWEEP <- list(num_trees     = c(250, 500, 1000, 2500),
                 max_depth     = c(3, 4, 5),
                 mtry          = c(1, 2, 4),
                 min_node_size = c(10, 20, 50))

SIGNIF_DIGITS <- 3
OBJECTIVE     <- "dollars"    # recall basis for plots / matched-recall summary
LCB_Z         <- 1.2816       # one-sided 90%
THRESHOLD_GRID    <- seq(0.05, 0.95, by = 0.05)
MIN_TRAIN_FLAGGED <- 10
PRUNE_MIN_PRECISION <- min(THRESHOLD_GRID)
MIN_PRECISION <- 0.20         # shortlist floor (reported per setting)

out_dir <- "parameter_tuning_v2"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
stopifnot(OBJECTIVE %in% c("dollars", "counts"))

## ── 1. Data ───────────────────────────────────────────────────────────────────

frame_df <- reg_model_data %>%
  filter(.data[[YEAR_COL]] %in% c(TRAIN_YEARS, HOLDOUT_YEARS))
if (FRAME != "any_error")
  frame_df <- frame_df %>% filter(error_status %in% c(FRAME, "no_error"))

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

strata_tr <- lapply(setNames(nm = HH_LEVELS), function(h)
  which(hh_group_of(train[[HH_SIZE_COL]]) %in% h))
strata_h  <- lapply(setNames(nm = HH_LEVELS), function(h)
  which(hh_group_of(hold[[HH_SIZE_COL]]) %in% h))

cat(sprintf("Frame %s | Train: %d rows, %d errors (%.2f%%) | Holdout: %d rows, %d errors\n",
            FRAME, nrow(train), sum(tg_tr$ie), 100 * mean(tg_tr$ie),
            nrow(hold), sum(tg_h$ie)))

## ── 2. One config end-to-end ──────────────────────────────────────────────────

run_config <- function(engine, cfg, param, label) {
  t0 <- Sys.time()
  rules <- bind_rows(lapply(HH_LEVELS, function(h) {
    ix <- strata_tr[[h]]; sub <- train[ix, , drop = FALSE]; ie_s <- tg_tr$ie[ix]
    rs <- if (engine == "xgboost")
      generate_rules_xgboost(sub, ie_s, pv, nrounds = cfg$nrounds,
                             max_depth = cfg$max_depth, eta = cfg$eta,
                             subsample = cfg$subsample, seed = 117)
    else
      generate_rules_ranger(sub, ie_s, pv, num_trees = cfg$num_trees,
                            max_depth = cfg$max_depth, mtry = cfg$mtry,
                            min_node_size = cfg$min_node_size, seed = 117)
    data.frame(rule = canonicalize_rules(rs, SIGNIF_DIGITS), hh = h,
               stringsAsFactors = FALSE)
  })) %>% distinct(rule, hh)
  mining_secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

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

  idx_h <- flags_for_rules(rules, hold, strata_h)
  sw <- precision_sweep(rules$stat, !is.na(rules$stat), idx_h, tg_h$ie, tg_h$ed,
                        THRESHOLD_GRID[THRESHOLD_GRID >= PRUNE_MIN_PRECISION])
  sw$x <- if (OBJECTIVE == "dollars") sw$dollar_recall else sw$recall
  total_secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cat(sprintf("  %-8s %-22s rules %6d | shortlist %5d | mine %4.0fs | total %4.0fs\n",
              engine, label, nrow(rules),
              sum(rules$stat >= MIN_PRECISION, na.rm = TRUE),
              mining_secs, total_secs))
  list(sweep = sw %>% mutate(engine = engine, param = param, setting = label),
       info = data.frame(engine = engine, param = param, setting = label,
                         n_rules = nrow(rules),
                         n_shortlist = sum(rules$stat >= MIN_PRECISION, na.rm = TRUE),
                         mining_secs = round(mining_secs),
                         total_secs = round(total_secs)))
}

## ── 3. Build and run the one-at-a-time grid ───────────────────────────────────

configs <- list()
add_configs <- function(engine, base, sweep) {
  for (param in names(sweep)) for (v in sweep[[param]]) {
    cfg <- base; cfg[[param]] <- v
    key <- paste(engine, paste(unlist(cfg), collapse = "_"))
    label <- sprintf("%s=%s%s", param, v,
                     ifelse(identical(v, base[[param]]), " (base)", ""))
    configs[[key]] <<- list(engine = engine, cfg = cfg, param = param, label = label)
  }
}
add_configs("xgboost", BASE_XGB, XGB_SWEEP)
add_configs("ranger",  BASE_RF,  RF_SWEEP)
cat(sprintf("\n%d distinct configs to run\n\n", length(configs)))

runs <- lapply(configs, function(x) run_config(x$engine, x$cfg, x$param, x$label))

sweeps <- bind_rows(lapply(runs, `[[`, "sweep"))
info   <- bind_rows(lapply(runs, `[[`, "info"))

# The base config is ONE run but is the reference point of EVERY parameter's
# sweep; deduplication left it under a single param label. Replicate its curve
# into each facet it belongs to, labeled with that facet's base value.
replicate_base <- function(sweeps, eng, base, sweep) {
  brow <- sweeps[sweeps$engine == eng & grepl("\\(base\\)$", sweeps$setting), , drop = FALSE]
  extra <- bind_rows(lapply(names(sweep), function(pn) {
    lbl <- sprintf("%s=%s (base)", pn, base[[pn]])
    if (lbl %in% sweeps$setting[sweeps$engine == eng]) return(NULL)
    out <- brow
    out$param <- pn        # base-R assignment: no data-masking capture of `pn`
    out$setting <- lbl
    out
  }))
  bind_rows(sweeps, extra)
}
sweeps <- replicate_base(sweeps, "xgboost", BASE_XGB, XGB_SWEEP)
sweeps <- replicate_base(sweeps, "ranger",  BASE_RF,  RF_SWEEP)

write.csv(sweeps, file.path(out_dir, "v2_tuning_sweeps.csv"), row.names = FALSE)
write.csv(info,   file.path(out_dir, "v2_tuning_settings_summary.csv"), row.names = FALSE)

## ── 4. Summary at matched recall + reference floors ───────────────────────────

interp_prec <- function(df, grid) {
  df <- df[!is.na(df$x) & !is.na(df$precision), ]
  if (nrow(df) < 2) return(rep(NA_real_, length(grid)))
  approx(df$x, df$precision, xout = grid, ties = mean, rule = 1)$y
}
grid <- seq(0.02, 0.5, by = 0.02)
floor_rows <- sweeps %>% filter(threshold %in% c(0.20, 0.30)) %>%
  transmute(engine, param, setting, threshold,
            precision = round(precision, 3), x = round(x, 3))
summary_tbl <- sweeps %>%
  group_by(engine, param, setting) %>%
  summarise(mean_precision = round(mean(interp_prec(pick(everything()), grid),
                                        na.rm = TRUE), 4),
            .groups = "drop") %>%
  left_join(info, by = c("engine", "param", "setting"))
write.csv(summary_tbl, file.path(out_dir, "v2_tuning_matched_recall_summary.csv"),
          row.names = FALSE)
cat("\nMean precision at matched recall (0.02-0.50 grid):\n")
print(as.data.frame(summary_tbl), digits = 3)
cat("\nOperating points at LCB floors 0.20 / 0.30:\n")
print(as.data.frame(floor_rows), row.names = FALSE)

## ── 5. Plots: one page per engine, one facet per parameter ───────────────────

xlab <- sprintf("Hold-out %s recall of the union",
                if (OBJECTIVE == "dollars") "dollar" else "case")
for (eng in unique(sweeps$engine)) {
  d <- sweeps %>% filter(engine == eng)
  p <- ggplot(d, aes(x, precision, color = setting)) +
    geom_line(linewidth = 0.7) + geom_point(size = 0.9) +
    facet_wrap(~param, ncol = 2, scales = "free_x") +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = xlab, y = "Hold-out precision of the union", color = NULL,
         title = sprintf("%s parameter sweep - %s frame", eng, FRAME),
         subtitle = sprintf("One-at-a-time around base; 90%% Wilson-LCB selection; trained %s, scored on %s",
                            paste(TRAIN_YEARS, collapse = "/"),
                            paste(HOLDOUT_YEARS, collapse = "/"))) +
    theme_minimal(base_size = 11) + theme(legend.position = "top")
  save_png(p, file.path(out_dir, sprintf("v2_tuning_%s.png", eng)), 9, 7)
}

cat(sprintf("\nWrote tuning outputs to %s/\n", out_dir))
