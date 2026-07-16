# ──────────────────────────────────────────────────────────────────────────────
# INCLUSION rules v2 — no {pre}, no lasso, no nets.
#
# For each error type and each household-size stratum (1 / 2-3 / 4+), mine
# candidate rules with two engines (xgboost boosted trees + ranger forest with
# mtry = 1 for diversity), then:
#   canonicalize (3-signif thresholds, canonical ordering)
#   -> screen on TRAIN (support, minimum raw precision, above stratum base rate)
#   -> dedup (exact coverage, then same-structure dominance on the Wilson LCB)
#   -> evaluate every surviving rule on TRAIN, HOLDOUT, and the ANY-ERROR
#      holdout universe (all error types)
#   -> sweep Wilson-LCB thresholds: union recall/precision at each floor
#   -> shortlist: rules whose train-precision LCB clears MIN_PRECISION.
#
# The selection statistic is the one-sided Wilson LOWER BOUND of train
# precision: it removes the winner's curse that inflates raw train precision
# when thresholding a large pool (validated 2026-07-04; at matched deployed
# precision it beats raw-precision selection ~12.8% vs 8.2% any-error recall).
# Redundant rules ACROSS different variable combinations are deliberately kept
# (states drop rules on expert knowledge); only same-structure threshold
# variants that can never add recall are pruned.
#
# Expects `reg_model_data` in the environment (script 1 output). All logic
# lives in rule_mining_helpers.R; this driver is config + orchestration.
# Outputs land in inclusion_rules_by_hh_size_v2/.
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

# Household-size stratification (cert_HH_size_FS_n collapsed to 1 / 2-3 / 4+).
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

# Engines, at the 2026-07-05 tuned settings: "mine big, filter stringently".
# xgboost: 1000 slow rounds, low subsample (0.15-0.30 indistinguishable; 0.20
# per prior rpart experience) — extends recall reach; the stiff LCB below
# removes the multiplicity dilution big pools otherwise suffer.
# ranger: mtry = 2 beat mtry = 1 across the frontier (guided splits > pure
# randomness); 1000 trees for reach.
XGB <- list(nrounds = 1000, max_depth = 4, eta = 0.02, subsample = 0.20)
RF  <- list(num_trees = 1000, max_depth = 4, mtry = 2, min_node_size = 20)

SIGNIF_DIGITS <- 3          # threshold rounding (state-presentable rules)

OBJECTIVE <- "dollars"      # "dollars" or "counts": recall basis for sweep plots
                            # and the x column (both bases are always in the CSVs)

# Selection / evaluation.
LCB_Z             <- 2.326  # one-sided 99% Wilson lower bound. "Mine big,
                            # filter stringently": with 1000-round/1000-tree pools,
                            # the z-sweep (2026-07-05) showed a stiff bound
                            # recovers the precision that pool-size multiplicity
                            # otherwise dilutes, while keeping the extended
                            # recall reach. Use 1.2816 (90%) for exploration.
THRESHOLD_GRID    <- seq(0.05, 0.95, by = 0.05)
MIN_TRAIN_FLAGGED <- 10     # hard support backstop (the LCB does the real work)
PRUNE_MIN_PRECISION <- min(THRESHOLD_GRID)  # raw-precision floor at screen time
MIN_PRECISION     <- 0.20   # shortlist floor, applied to the train LCB

out_dir <- "inclusion_rules_by_hh_size_v2"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
stopifnot(OBJECTIVE %in% c("dollars", "counts"))

## ── 1. Frames ─────────────────────────────────────────────────────────────────

frame_of <- function(statuses) reg_model_data %>%
  filter(error_status %in% c(statuses, "no_error")) %>%
  filter(.data[[YEAR_COL]] %in% c(TRAIN_YEARS, HOLDOUT_YEARS))

df_list <- list(
  earned_income   = frame_of("earned_overissuance"),
  unearned_income = frame_of("unearned_overissuance"),
  underissuance   = frame_of("underissuance"),
  other_error     = frame_of("other_error"),    # kept for completeness (heterogeneous)
  # pooled-target frame: all error types vs clean. The head-to-head showed the
  # typed and pooled vocabularies complement — their union catches 3-6pp more
  # recall at any filter floor than typed alone (see modeling_findings.md §3)
  any_error       = frame_of(c("earned_overissuance", "unearned_overissuance",
                               "underissuance", "other_error"))
)

# Any-error holdout universe: every case in HOLDOUT_YEARS, all error types.
universe <- reg_model_data %>% filter(.data[[YEAR_COL]] %in% HOLDOUT_YEARS)

targets_of <- function(df) {
  ie <- eval(TARGET_IS_ERROR, envir = df); ie[is.na(ie)] <- FALSE
  amt <- df[[ERR_AMT_COL]]; amt[is.na(amt)] <- 0
  list(ie = ie, ed = ifelse(ie, abs(amt), 0))
}

## ── 2. One frame end-to-end ───────────────────────────────────────────────────

run_frame <- function(frame_df, frame_name, universe) {
  cat(sprintf("\n============ FRAME: %s ============\n", frame_name))
  pf   <- prep_features(frame_df, features)
  pfu  <- prep_features(universe, features)
  fdf  <- pf$data; pv <- pf$features; univ <- pfu$data

  yr    <- as.character(fdf[[YEAR_COL]])
  train <- fdf[yr %in% TRAIN_YEARS, , drop = FALSE]
  hold  <- fdf[yr %in% HOLDOUT_YEARS, , drop = FALSE]
  tg_tr <- targets_of(train); tg_h <- targets_of(hold); tg_u <- targets_of(univ)

  strata_tr <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(train[[HH_SIZE_COL]]) %in% h))
  strata_h  <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(hold[[HH_SIZE_COL]]) %in% h))
  strata_u  <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(univ[[HH_SIZE_COL]]) %in% h))

  ## generate per stratum x engine (shared with the delivery builder; the
  ## single-frame call keeps this script's outputs unchanged — `source` is the
  ## engine tag, the frame is already in the output filename)
  rules_df <- mine_rule_vocabulary(
    train, setNames(list(list(rows = seq_len(nrow(train)), ie = tg_tr$ie)),
                    frame_name),
    strata_tr, pv, xgb = XGB, rf = RF,
    signif_digits = SIGNIF_DIGITS, seed = 117)
  if (is.null(rules_df) || nrow(rules_df) == 0) return(invisible(NULL))
  rules_df$source <- rules_df$engines
  rules_df$engines <- NULL
  rules_df$mined_frames <- NULL

  ## train flags -> screen
  idx_tr <- flags_for_rules(rules_df, train, strata_tr, label = "train")
  n_tr   <- lengths(idx_tr)
  k_tr   <- vapply(idx_tr, function(ix) sum(tg_tr$ie[ix]), numeric(1))
  raw_tr <- ifelse(n_tr > 0, k_tr / n_tr, NA_real_)
  base_rate <- vapply(rules_df$hh, function(h) mean(tg_tr$ie[strata_tr[[h]]]), numeric(1))

  keep <- !is.na(raw_tr) & n_tr >= MIN_TRAIN_FLAGGED &
          raw_tr >= PRUNE_MIN_PRECISION & raw_tr > base_rate
  cat(sprintf("\nscreen: %d of %d rules kept (support >= %d, raw >= %.2f, above base rate)\n",
              sum(keep), nrow(rules_df), MIN_TRAIN_FLAGGED, PRUNE_MIN_PRECISION))
  rules_df <- rules_df[keep, , drop = FALSE]
  idx_tr <- idx_tr[keep]; n_tr <- n_tr[keep]; k_tr <- k_tr[keep]

  rules_df$n_flagged_train <- n_tr
  rules_df$errors_train    <- k_tr
  rules_df$precision_train <- round(k_tr / n_tr, 4)
  rules_df$precision_train_lcb <- round(wilson_lcb(k_tr, n_tr, LCB_Z), 4)

  ## dedup: exact coverage, then same-structure dominance on the LCB
  drop_cov <- dedup_exact_coverage(rules_df, idx_tr)
  rules_df <- rules_df[!drop_cov, , drop = FALSE]; idx_tr <- idx_tr[!drop_cov]
  drop_dom <- dedup_dominated(rules_df, rules_df$precision_train_lcb)
  rules_df <- rules_df[!drop_dom, , drop = FALSE]; idx_tr <- idx_tr[!drop_dom]
  cat(sprintf("dedup: -%d exact-coverage, -%d dominated -> %d rules\n",
              sum(drop_cov), sum(drop_dom), nrow(rules_df)))
  if (nrow(rules_df) == 0) return(invisible(NULL))

  ## evaluate on holdout + any-error universe
  idx_h <- flags_for_rules(rules_df, hold, strata_h, label = "holdout")
  idx_u <- flags_for_rules(rules_df, univ, strata_u, label = "any-error universe")

  rule_eval <- bind_cols(
    rules_df,
    eval_rules_on(rules_df, idx_h, tg_h$ie, tg_h$ed, strata_h, "holdout"),
    eval_rules_on(rules_df, idx_u, tg_u$ie, tg_u$ed, strata_u, "any")
  ) %>% arrange(hh, desc(precision_train_lcb))
  write.csv(rule_eval, file.path(out_dir, sprintf("%s_rules_all.csv", frame_name)),
            row.names = FALSE)

  # deliverable-time ladder collapse: the state-facing shortlist keeps one rung
  # per same-structure family (the max-LCB one); the full ladder stays in
  # *_rules_all.csv and the sweep below still uses every rung for reach
  shortlist <- rule_eval %>% filter(precision_train_lcb >= MIN_PRECISION)
  shortlist <- shortlist[collapse_ladders(shortlist, shortlist$precision_train_lcb), ,
                         drop = FALSE]
  write.csv(shortlist, file.path(out_dir, sprintf("%s_rules_highprecision.csv", frame_name)),
            row.names = FALSE)
  cat(sprintf("shortlist (train LCB >= %.2f): %d rules | median holdout precision %.3f | median any-error %.3f\n",
              MIN_PRECISION, nrow(shortlist),
              median(shortlist$precision_holdout, na.rm = TRUE),
              median(shortlist$precision_any, na.rm = TRUE)))

  ## LCB threshold sweeps (frame-relative and any-error)
  stat   <- rules_df$precision_train_lcb
  usable <- !is.na(stat)
  grid   <- THRESHOLD_GRID[THRESHOLD_GRID >= PRUNE_MIN_PRECISION]
  sweep_frame <- precision_sweep(stat, usable, idx_h, tg_h$ie, tg_h$ed, grid)
  sweep_any   <- precision_sweep(stat, usable, idx_u, tg_u$ie, tg_u$ed, grid)
  sweeps <- bind_rows(
    sweep_frame %>% mutate(scoring = "frame only (mined error type)"),
    sweep_any   %>% mutate(scoring = "any error type"))
  sweeps$x <- if (OBJECTIVE == "dollars") sweeps$dollar_recall else sweeps$recall
  write.csv(sweeps, file.path(out_dir, sprintf("%s_lcb_sweep.csv", frame_name)),
            row.names = FALSE)

  recall_lab <- if (OBJECTIVE == "dollars") "hold-out share of error $ caught" else
    "hold-out share of errors caught"
  sweep_long <- bind_rows(
    sweeps %>% mutate(metric = "hold-out precision", value = precision),
    sweeps %>% mutate(metric = recall_lab, value = x))
  p <- ggplot(sweep_long, aes(threshold, value, linetype = scoring)) +
    geom_line(linewidth = 0.8) + geom_point(size = 1.0) +
    facet_wrap(~metric, nrow = 1) +
    labs(x = "99% lower bound precision",
         y = NULL, linetype = "Scored against",
         title = sprintf("What the kept rules achieve together, by precision floor - %s", frame_name),
         subtitle = sprintf("xgboost + ranger, trained %s, scored on %s; compare lines vertically at any floor",
                            paste(TRAIN_YEARS, collapse = "/"), paste(HOLDOUT_YEARS, collapse = "/"))) +
    coord_cartesian(xlim = c(0.05, max(0.7, max(sweeps$threshold) + 0.05)),
                    ylim = c(0, 1)) +
    theme_minimal(base_size = 12) + theme(legend.position = "top")
  save_png(p, file.path(out_dir, sprintf("%s_lcb_sweep.png", frame_name)), 9, 4.5)

  invisible(list(rule_eval = rule_eval, shortlist = shortlist, sweeps = sweeps))
}

## ── 3. Run every frame ────────────────────────────────────────────────────────

results <- lapply(names(df_list), function(nm) run_frame(df_list[[nm]], nm, universe))
names(results) <- names(df_list)

combined_shortlist <- bind_rows(lapply(names(results), function(nm) {
  s <- results[[nm]]$shortlist
  if (!is.null(s) && nrow(s) > 0) mutate(s, error_frame = nm)
}))
write.csv(combined_shortlist, file.path(out_dir, "final_rules_highprecision_all_frames.csv"),
          row.names = FALSE)

cat(sprintf("\nCombined high-precision shortlist: %d rules across %d frames -> %s\n",
            nrow(combined_shortlist), dplyr::n_distinct(combined_shortlist$error_frame),
            file.path(out_dir, "final_rules_highprecision_all_frames.csv")))

## ── Notes ─────────────────────────────────────────────────────────────────────
# - No rebalancing, no lasso, no nets. Selection = train-LCB thresholding only.
# - other_error is mined like any frame but is known to be heterogeneous;
#   expect structurally lower precision there.
# - Class imbalance knobs if a frame mines poorly: XGB$params_extra
#   scale_pos_weight (clean/error ratio) or RF min_node_size / num_trees.
# - Per-rule any-error columns show deployment-level performance: a flagged
#   case with a DIFFERENT error type is an operational win, not a false
#   positive (frame precision understates deployed precision ~2x).
