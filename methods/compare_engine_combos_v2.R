# ──────────────────────────────────────────────────────────────────────────────
# ENGINE COMBOS: which PAIR powers the production drivers?
#   xgboost + ranger   vs   rpart + ranger   (singles shown for context)
#
# All three vocabularies (bagged rpart / xgboost / ranger, production settings:
# 1000 trees or rounds, depth 4) are mined once and CHECKPOINTED; the union is
# screened once (screening is pool-independent); five pools are then swept with
# identical filtering (z = 2.326):
#   rpart, xgboost, ranger, xgboost+ranger, rpart+ranger
# Combos are unions of vocabularies with cross-vocabulary coverage + dominance
# dedup, exactly as the production drivers pool their two engines.
#
# Frame: any_error; strata 1 / 2-3 / 4+; OBJECTIVE dollars.
# Expects `reg_model_data`. Outputs -> methods/compare_engines_v2/ (combo-prefixed).
# ──────────────────────────────────────────────────────────────────────────────

library(dplyr)
library(ggplot2)
library(rpart)
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

SIGNIF_DIGITS <- 3
OBJECTIVE     <- "dollars"
LCB_Z         <- 2.326
THRESHOLD_GRID    <- seq(0.05, 0.95, by = 0.05)
MIN_TRAIN_FLAGGED <- 10
PRUNE_MIN_PRECISION <- min(THRESHOLD_GRID)
MIN_PRECISION <- 0.20
if (!exists("RESUME_FROM_CHECKPOINT")) RESUME_FROM_CHECKPOINT <- FALSE

out_dir <- "methods/compare_engines_v2"
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
strata_tr <- lapply(setNames(nm = HH_LEVELS), function(h)
  which(hh_group_of(train[[HH_SIZE_COL]]) %in% h))
strata_h  <- lapply(setNames(nm = HH_LEVELS), function(h)
  which(hh_group_of(hold[[HH_SIZE_COL]]) %in% h))

## ── 2. Mine three vocabularies (checkpointed) ─────────────────────────────────

ckpt <- file.path(out_dir, "engine_vocabularies_checkpoint.rds")
if (RESUME_FROM_CHECKPOINT && file.exists(ckpt)) {
  vocab <- readRDS(ckpt)
  cat("Resumed engine vocabularies from checkpoint\n")
} else {
  gen <- list(
    rpart = function(sub, ie_s) generate_rules_rpart(sub, ie_s, pv,
      num_trees = 1000, max_depth = 4, sample_frac = 0.20, min_bucket = 20, seed = 117),
    xgboost = function(sub, ie_s) generate_rules_xgboost(sub, ie_s, pv,
      nrounds = 1000, max_depth = 4, eta = 0.02, subsample = 0.20, seed = 117),
    ranger = function(sub, ie_s) generate_rules_ranger(sub, ie_s, pv,
      num_trees = 1000, max_depth = 4, mtry = 2, min_node_size = 20, seed = 117)
  )
  vocab <- lapply(names(gen), function(eng) {
    t0 <- Sys.time()
    v <- bind_rows(lapply(HH_LEVELS, function(h) {
      ix <- strata_tr[[h]]
      data.frame(rule = canonicalize_rules(
        gen[[eng]](train[ix, , drop = FALSE], tg_tr$ie[ix]), SIGNIF_DIGITS),
        hh = h, stringsAsFactors = FALSE)
    })) %>% distinct(rule, hh)
    cat(sprintf("[%s] %d canonical rules in %.0fs\n", eng, nrow(v),
                as.numeric(difftime(Sys.time(), t0, units = "secs"))))
    v
  })
  names(vocab) <- names(gen)
  saveRDS(vocab, ckpt)
}

## ── 3. Screen ONCE on the union; dedup + sweep per pool ──────────────────────

u <- bind_rows(lapply(names(vocab), function(e) mutate(vocab[[e]], eng = e))) %>%
  group_by(rule, hh) %>%
  summarise(in_rpart = any(eng == "rpart"), in_xgb = any(eng == "xgboost"),
            in_ranger = any(eng == "ranger"), .groups = "drop")
cat(sprintf("\nUnion of vocabularies: %d rules\n", nrow(u)))

idx_tr <- flags_for_rules(u, train, strata_tr, label = "train (union, once)")
n_tr <- lengths(idx_tr)
k_tr <- vapply(idx_tr, function(ix) sum(tg_tr$ie[ix]), numeric(1))
raw  <- ifelse(n_tr > 0, k_tr / n_tr, NA_real_)
base <- vapply(u$hh, function(h) mean(tg_tr$ie[strata_tr[[h]]]), numeric(1))
keep <- !is.na(raw) & n_tr >= MIN_TRAIN_FLAGGED &
        raw >= PRUNE_MIN_PRECISION & raw > base
u <- u[keep, , drop = FALSE]; idx_tr <- idx_tr[keep]
u$stat <- wilson_lcb(k_tr[keep], n_tr[keep], LCB_Z)
cat(sprintf("screen (identical for every pool): %d rules kept\n", nrow(u)))

dedup_pool <- function(rows, label) {
  sdf <- u[rows, , drop = FALSE]; sidx <- idx_tr[rows]
  d1 <- dedup_exact_coverage(sdf, sidx)
  rows <- rows[!d1]; sdf <- sdf[!d1, , drop = FALSE]
  d2 <- dedup_dominated(sdf, sdf$stat)
  cat(sprintf("%-18s -%d coverage, -%d dominated -> %d rules (pass filter: %d)\n",
              label, sum(d1), sum(d2), sum(!d2),
              sum(sdf$stat[!d2] >= MIN_PRECISION, na.rm = TRUE)))
  rows[!d2]
}
pools <- list(
  "rpart"           = dedup_pool(which(u$in_rpart), "rpart"),
  "xgboost"         = dedup_pool(which(u$in_xgb), "xgboost"),
  "ranger"          = dedup_pool(which(u$in_ranger), "ranger"),
  "xgboost + ranger" = dedup_pool(which(u$in_xgb | u$in_ranger), "xgboost + ranger"),
  "rpart + ranger"  = dedup_pool(which(u$in_rpart | u$in_ranger), "rpart + ranger")
)

needed <- sort(unique(unlist(pools)))
remap  <- match(seq_len(nrow(u)), needed)
idx_h_needed <- flags_for_rules(u[needed, , drop = FALSE], hold, strata_h,
                                label = "holdout (survivor union, once)")

sweeps <- bind_rows(lapply(names(pools), function(nm) {
  rows <- pools[[nm]]
  sw <- precision_sweep(u$stat[rows], !is.na(u$stat[rows]),
                        idx_h_needed[remap[rows]], tg_h$ie, tg_h$ed,
                        THRESHOLD_GRID[THRESHOLD_GRID >= PRUNE_MIN_PRECISION])
  sw$x <- if (OBJECTIVE == "dollars") sw$dollar_recall else sw$recall
  sw %>% mutate(pool = nm)
}))
sweeps$pool <- factor(sweeps$pool, levels = names(pools))
write.csv(sweeps, file.path(out_dir, "combo_sweeps.csv"), row.names = FALSE)

## ── 4. Summary + plot ─────────────────────────────────────────────────────────

interp_prec <- function(df, grid) {
  df <- df[!is.na(df$x) & !is.na(df$precision), ]
  if (nrow(df) < 2) return(rep(NA_real_, length(grid)))
  approx(df$x, df$precision, xout = grid, ties = mean, rule = 1)$y
}
grid <- seq(0.02, 0.5, by = 0.02)
summary_tbl <- sweeps %>% group_by(pool) %>%
  summarise(mean_precision = round(mean(interp_prec(pick(everything()), grid), na.rm = TRUE), 4),
            recall_at_020 = round(x[threshold == 0.2][1], 3),
            precision_at_020 = round(precision[threshold == 0.2][1], 3),
            n_pass_filter = round(n_rules[threshold == 0.2][1]),
            max_recall = round(max(x, na.rm = TRUE), 3), .groups = "drop")
write.csv(summary_tbl, file.path(out_dir, "combo_summary.csv"), row.names = FALSE)
cat("\nPool summary (matched dollar recall 0.02-0.50):\n")
print(as.data.frame(summary_tbl), row.names = FALSE)

cols <- c("rpart" = "#f2a3ab", "xgboost" = "#9ecbe8", "ranger" = "#a8d5b9",
          "xgboost + ranger" = "#0073b7", "rpart + ranger" = "#d1495b")
p <- ggplot(sweeps, aes(x, precision, color = pool)) +
  geom_line(linewidth = 0.9) + geom_point(size = 1.1) +
  scale_color_manual(values = cols) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Hold-out dollar recall of the union (all 2023 errors)",
       y = "Hold-out precision of the union", color = NULL,
       title = "Engine pairs for production: xgboost + ranger vs rpart + ranger",
       subtitle = "Singles muted for context. 1,000 trees/rounds each, identical screens and 99% LCB filter;\nany-error frame, trained 2022/2024, scored on 2023") +
  theme_minimal(base_size = 12) + theme(legend.position = "top")
save_png(p, file.path(out_dir, "combo_sweeps.png"), 9, 5.5)
cat(sprintf("\nWrote combo comparison to %s/\n", out_dir))
