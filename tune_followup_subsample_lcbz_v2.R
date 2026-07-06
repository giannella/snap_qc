# ──────────────────────────────────────────────────────────────────────────────
# Tuning follow-ups (v2 stack), motivated by the 2026-07-05 grid:
#
# PART A — fine subsample search for xgboost. Under pre()/rpart, sampfrac
#   optimized around 0.15-0.20 and degraded by 0.10 / 0.25; the coarse v2 grid
#   (0.25 / 0.50 / 0.75) also favored the low end. Here: subsample in
#   {.15,.20,.25,.30,.60,.65,.70,.75,.80} at nrounds = 300, eta = 0.02, depth 4.
#
# PART B — LCB_Z sweep ("flexible LCB"). 100 vs 1000 rounds trades too much
#   recall reach for states (55% vs 68% dollar recall at the 0.20 floor), but
#   more mining dilutes matched-recall precision via selection multiplicity.
#   Hypothesis: a HIGHER z on a BIGGER pool recovers the precision while
#   keeping the reach. z is applied post-mining, so we mine the 100-round and
#   1000-round pools once each (at the ORIGINAL eta=.05/subsample=.5 settings,
#   isolating the exact trade observed) and sweep z in {0.8416 (80%),
#   1.2816 (90%), 1.645 (95%), 2.326 (99%)} over both.
#
# Frame: any_error. Expects `reg_model_data`. Outputs -> parameter_tuning_v2/.
# ──────────────────────────────────────────────────────────────────────────────

library(dplyr)
library(ggplot2)
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

SUBSAMPLE_GRID <- c(0.15, 0.20, 0.25, 0.30, 0.60, 0.65, 0.70, 0.75, 0.80)
SUB_BASE       <- list(nrounds = 300, max_depth = 4, eta = 0.02)

LCBZ_GRID   <- c(0.8416, 1.2816, 1.645, 2.326)     # 80 / 90 / 95 / 99 %
LCBZ_LABELS <- c("z=0.84 (80%)", "z=1.28 (90%)", "z=1.64 (95%)", "z=2.33 (99%)")
LCBZ_CONFIGS <- list(
  "nrounds=100"  = list(nrounds = 100,  max_depth = 4, eta = 0.05, subsample = 0.5),
  "nrounds=1000" = list(nrounds = 1000, max_depth = 4, eta = 0.05, subsample = 0.5)
)

SIGNIF_DIGITS <- 3
OBJECTIVE     <- "dollars"
DEFAULT_Z     <- 1.2816       # used for Part A
THRESHOLD_GRID    <- seq(0.05, 0.95, by = 0.05)
MIN_TRAIN_FLAGGED <- 10
PRUNE_MIN_PRECISION <- min(THRESHOLD_GRID)

out_dir <- "parameter_tuning_v2"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

## ── 1. Data (any_error frame) ─────────────────────────────────────────────────

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
strata_tr <- lapply(setNames(nm = HH_LEVELS), function(h)
  which(hh_group_of(train[[HH_SIZE_COL]]) %in% h))
strata_h  <- lapply(setNames(nm = HH_LEVELS), function(h)
  which(hh_group_of(hold[[HH_SIZE_COL]]) %in% h))

## ── 2. Shared pipeline pieces ─────────────────────────────────────────────────

mine_xgb <- function(cfg) {
  bind_rows(lapply(HH_LEVELS, function(h) {
    ix <- strata_tr[[h]]
    data.frame(rule = canonicalize_rules(
      generate_rules_xgboost(train[ix, , drop = FALSE], tg_tr$ie[ix], pv,
                             nrounds = cfg$nrounds, max_depth = cfg$max_depth,
                             eta = cfg$eta, subsample = cfg$subsample, seed = 117),
      SIGNIF_DIGITS), hh = h, stringsAsFactors = FALSE)
  })) %>% distinct(rule, hh)
}

# screen + coverage-dedup once per mined pool (z-independent); returns the
# pieces the per-z stage needs
prepare_pool <- function(rules) {
  idx_tr <- flags_for_rules(rules, train, strata_tr)
  n_tr <- lengths(idx_tr)
  k_tr <- vapply(idx_tr, function(ix) sum(tg_tr$ie[ix]), numeric(1))
  raw  <- ifelse(n_tr > 0, k_tr / n_tr, NA_real_)
  base <- vapply(rules$hh, function(h) mean(tg_tr$ie[strata_tr[[h]]]), numeric(1))
  keep <- !is.na(raw) & n_tr >= MIN_TRAIN_FLAGGED &
          raw >= PRUNE_MIN_PRECISION & raw > base
  rules <- rules[keep, , drop = FALSE]
  idx_tr <- idx_tr[keep]; n_tr <- n_tr[keep]; k_tr <- k_tr[keep]
  d1 <- dedup_exact_coverage(rules, idx_tr)
  rules <- rules[!d1, , drop = FALSE]
  n_tr <- n_tr[!d1]; k_tr <- k_tr[!d1]
  idx_h <- flags_for_rules(rules, hold, strata_h)
  list(rules = rules, n_tr = n_tr, k_tr = k_tr, idx_h = idx_h)
}

# dominance + sweep at one z (cheap; reuses the prepared pool)
sweep_at_z <- function(pool, z) {
  stat <- wilson_lcb(pool$k_tr, pool$n_tr, z)
  d2 <- dedup_dominated(pool$rules, stat)
  sw <- precision_sweep(stat[!d2], !is.na(stat[!d2]), pool$idx_h[!d2],
                        tg_h$ie, tg_h$ed,
                        THRESHOLD_GRID[THRESHOLD_GRID >= PRUNE_MIN_PRECISION])
  sw$x <- if (OBJECTIVE == "dollars") sw$dollar_recall else sw$recall
  sw$n_pool <- sum(!d2)
  sw
}

## ── 3. Part A: fine subsample grid ────────────────────────────────────────────

cat("\n#### PART A: xgboost subsample fine grid (nrounds=300, eta=0.02) ####\n")
sub_sweeps <- bind_rows(lapply(SUBSAMPLE_GRID, function(s) {
  t0 <- Sys.time()
  cfg <- c(SUB_BASE, list(subsample = s))
  pool <- prepare_pool(mine_xgb(cfg))
  sw <- sweep_at_z(pool, DEFAULT_Z) %>%
    mutate(subsample = s, setting = sprintf("subsample=%.2f", s))
  cat(sprintf("  subsample=%.2f  pool %6d | %4.0fs\n", s, sw$n_pool[1],
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  sw
}))
write.csv(sub_sweeps, file.path(out_dir, "v2_subsample_fine_sweeps.csv"), row.names = FALSE)

## ── 4. Part B: LCB_Z sweep on small vs large pools ────────────────────────────

cat("\n#### PART B: LCB_Z sweep (xgboost 100 vs 1000 rounds, eta=0.05) ####\n")
lcbz_sweeps <- bind_rows(lapply(names(LCBZ_CONFIGS), function(nm) {
  t0 <- Sys.time()
  pool <- prepare_pool(mine_xgb(LCBZ_CONFIGS[[nm]]))
  cat(sprintf("  %s: pool prepared in %.0fs\n", nm,
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  bind_rows(lapply(seq_along(LCBZ_GRID), function(i) {
    sweep_at_z(pool, LCBZ_GRID[i]) %>%
      mutate(config = nm, z = LCBZ_GRID[i], z_label = LCBZ_LABELS[i])
  }))
}))
write.csv(lcbz_sweeps, file.path(out_dir, "v2_lcbz_sweeps.csv"), row.names = FALSE)

## ── 5. Summaries ──────────────────────────────────────────────────────────────

interp_prec <- function(df, grid) {
  df <- df[!is.na(df$x) & !is.na(df$precision), ]
  if (nrow(df) < 2) return(rep(NA_real_, length(grid)))
  approx(df$x, df$precision, xout = grid, ties = mean, rule = 1)$y
}
grid <- seq(0.02, 0.5, by = 0.02)

sub_summary <- sub_sweeps %>% group_by(setting) %>%
  summarise(mean_precision = round(mean(interp_prec(pick(everything()), grid), na.rm = TRUE), 4),
            recall_at_020 = round(x[threshold == 0.2][1], 3),
            precision_at_020 = round(precision[threshold == 0.2][1], 3),
            n_pool = max(n_pool), .groups = "drop") %>% arrange(desc(mean_precision))
write.csv(sub_summary, file.path(out_dir, "v2_subsample_fine_summary.csv"), row.names = FALSE)
cat("\nPart A summary (matched dollar recall 0.02-0.50):\n")
print(as.data.frame(sub_summary), row.names = FALSE)

lcbz_summary <- lcbz_sweeps %>% group_by(config, z_label) %>%
  summarise(mean_precision = round(mean(interp_prec(pick(everything()), grid), na.rm = TRUE), 4),
            recall_at_020 = round(x[threshold == 0.2][1], 3),
            precision_at_020 = round(precision[threshold == 0.2][1], 3),
            max_recall = round(max(x, na.rm = TRUE), 3),
            n_pool = max(n_pool), .groups = "drop")
write.csv(lcbz_summary, file.path(out_dir, "v2_lcbz_summary.csv"), row.names = FALSE)
cat("\nPart B summary:\n")
print(as.data.frame(lcbz_summary), row.names = FALSE)

## ── 6. Plots ──────────────────────────────────────────────────────────────────

p1 <- ggplot(sub_sweeps, aes(x, precision, color = setting)) +
  geom_line(linewidth = 0.7) + geom_point(size = 0.9) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Hold-out dollar recall of the union", y = "Hold-out precision of the union",
       color = NULL, title = "xgboost subsample fine grid (nrounds=300, eta=0.02)",
       subtitle = "any_error frame; 90% Wilson-LCB selection; trained 2022/2024, scored on 2023") +
  theme_minimal(base_size = 12) + theme(legend.position = "top")
save_png(p1, file.path(out_dir, "v2_subsample_fine.png"), 9, 5.5)

p2 <- ggplot(lcbz_sweeps, aes(x, precision, color = z_label)) +
  geom_line(linewidth = 0.7) + geom_point(size = 0.9) +
  facet_wrap(~config) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Hold-out dollar recall of the union", y = "Hold-out precision of the union",
       color = "LCB confidence",
       title = "Flexible LCB: does a stiffer bound tame a bigger pool?",
       subtitle = "Same rules, different filter stringency; any_error frame, trained 2022/2024, scored on 2023") +
  theme_minimal(base_size = 12) + theme(legend.position = "top")
save_png(p2, file.path(out_dir, "v2_lcbz_sweep.png"), 9, 5.5)

cat(sprintf("\nWrote follow-up tuning outputs to %s/\n", out_dir))
