# ──────────────────────────────────────────────────────────────────────────────
# HEAD-TO-HEAD: one ANY-ERROR model vs the FOUR typed error frames (v2 stack)
#
# Question: is it worth mining rules separately by error type (earned /
# unearned / underissuance / other), or does one model trained on "any
# over-threshold error" do as well for total error capture?
#
# Three rule pools, identical machinery (xgboost + ranger mtry=1 per
# household-size stratum):
#   "Typed (4 frames)"  — rules mined inside each typed frame, pooled
#   "Any-error (1 model)" — rules mined once on all cases, target = any error
#   "Combined"          — union of the two pools' SURVIVORS, re-deduped
#
# FAIRNESS: every rule, wherever it was mined, is screened with the SAME
# selection statistic — the Wilson LCB (one-sided 90%) of its precision against
# ANY error on the SAME full training universe — and every pool is scored on
# the SAME any-error holdout universe. The only thing that differs between the
# lines is what data the trees learned from.
#
# EFFICIENCY (v2 of this script): train flags are evaluated ONCE for the union
# of both vocabularies and pools index into them; hold-out flags are evaluated
# once for the union of pool survivors; Combined reuses the parents' survivors
# instead of reprocessing raw; the mined vocabularies are checkpointed so a
# rerun with different screens/floors skips mining (set RESUME_FROM_CHECKPOINT
# <- TRUE before source()ing, e.g. in a runner).
#
# Expects `reg_model_data`. Outputs (CSVs + presentation plot) land in
# methods/compare_anyerror_vs_typed_v2/.
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
HH_LEVELS   <- c("1", "2-3", "4+")
hh_group_of <- function(n) {
  n <- suppressWarnings(as.numeric(as.character(n)))
  ifelse(is.na(n), NA_character_, ifelse(n <= 1, "1", ifelse(n <= 3, "2-3", "4+")))
}

TYPED_STATUSES <- list(
  earned_income   = "earned_overissuance",
  unearned_income = "unearned_overissuance",
  underissuance   = "underissuance",
  other_error     = "other_error"
)

features <- c(
  "HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size",
  "expedited_i", "cat_elig", "rawben_rel_max", "medical_deductions",
  "shelter_expenses_by_hh_size", "utilities", "married", "homeless",
  "rawearn_by_hh_size", "rawunearn_by_hh_size", "rawgross_by_hh_size",
  "percent_abawd", "unc_rawben_rel_max",
  "months_since_cert_n", "count_divisible_by_100"
)

XGB <- list(nrounds = 1000, max_depth = 4, eta = 0.05, subsample = 0.5)
RF  <- list(num_trees = 2500, max_depth = 4, mtry = 1, min_node_size = 20)

SIGNIF_DIGITS     <- 3
OBJECTIVE         <- "dollars"  # "dollars" or "counts": recall basis for the
                                # x-axis, deltas and summary (both are always
                                # written to the sweep CSV)
LCB_Z             <- 1.2816   # one-sided 90%
THRESHOLD_GRID    <- seq(0.05, 0.95, by = 0.05)
MIN_TRAIN_FLAGGED <- 10
PRUNE_MIN_PRECISION <- min(THRESHOLD_GRID)
MIN_PRECISION     <- 0.20     # shortlist floor on the any-error train LCB

if (!exists("RESUME_FROM_CHECKPOINT")) RESUME_FROM_CHECKPOINT <- FALSE
stopifnot(OBJECTIVE %in% c("dollars", "counts"))

out_dir <- "methods/compare_anyerror_vs_typed_v2"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

cap <- sprintf("xgboost (%s rounds) + ranger (%s trees, mtry=1), depth 4, per HH stratum 1 / 2-3 / 4+",
               format(XGB$nrounds, big.mark = ","), format(RF$num_trees, big.mark = ","))

## ── 1. Data: the full universe, plus typed training subsets ──────────────────

all_df <- reg_model_data %>%
  filter(.data[[YEAR_COL]] %in% c(TRAIN_YEARS, HOLDOUT_YEARS))
pf  <- prep_features(all_df, features)
adf <- pf$data; pv <- pf$features

yr    <- as.character(adf[[YEAR_COL]])
train <- adf[yr %in% TRAIN_YEARS, , drop = FALSE]
hold  <- adf[yr %in% HOLDOUT_YEARS, , drop = FALSE]

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

cat(sprintf("Train: %d rows, %d any-type errors (%.2f%%) | Holdout: %d rows, %d errors (%.2f%%)\n",
            nrow(train), sum(tg_tr$ie), 100 * mean(tg_tr$ie),
            nrow(hold), sum(tg_h$ie), 100 * mean(tg_h$ie)))

## ── 2. Mine the two vocabularies (checkpointed) ───────────────────────────────

ckpt <- file.path(out_dir, "mined_vocabularies_checkpoint.rds")
if (RESUME_FROM_CHECKPOINT && file.exists(ckpt)) {
  vocab <- readRDS(ckpt)
  cat(sprintf("Resumed vocabularies from %s (any: %d, typed: %d rules)\n",
              ckpt, nrow(vocab$any), nrow(vocab$typed)))
} else {
  mine_stratum <- function(sub, ie_s, tag) {
    rx <- canonicalize_rules(
      generate_rules_xgboost(sub, ie_s, pv, nrounds = XGB$nrounds,
                             max_depth = XGB$max_depth, eta = XGB$eta,
                             subsample = XGB$subsample, seed = 117), SIGNIF_DIGITS)
    rr <- canonicalize_rules(
      generate_rules_ranger(sub, ie_s, pv, num_trees = RF$num_trees,
                            max_depth = RF$max_depth, mtry = RF$mtry,
                            min_node_size = RF$min_node_size, seed = 117), SIGNIF_DIGITS)
    cat(sprintf("   [%s] xgboost: %d | ranger(mtry=1): %d\n", tag, length(rx), length(rr)))
    unique(c(rx, rr))
  }

  cat("\n#### mining: any-error model ####\n")
  rules_any <- bind_rows(lapply(HH_LEVELS, function(h) {
    ix <- strata_tr[[h]]
    cat(sprintf("-- HH %s: %d rows, %d errors\n", h, length(ix), sum(tg_tr$ie[ix])))
    data.frame(rule = mine_stratum(train[ix, , drop = FALSE], tg_tr$ie[ix], "any"),
               hh = h, stringsAsFactors = FALSE)
  }))

  cat("\n#### mining: four typed frames ####\n")
  rules_typed <- bind_rows(lapply(names(TYPED_STATUSES), function(nm) {
    fdf <- train[train$error_status %in% c(TYPED_STATUSES[[nm]], "no_error"), , drop = FALSE]
    ie_f <- eval(TARGET_IS_ERROR, envir = fdf); ie_f[is.na(ie_f)] <- FALSE
    grp  <- hh_group_of(fdf[[HH_SIZE_COL]])
    bind_rows(lapply(HH_LEVELS, function(h) {
      sel <- which(grp %in% h)
      cat(sprintf("-- %s HH %s: %d rows, %d errors\n", nm, h, length(sel), sum(ie_f[sel])))
      data.frame(rule = mine_stratum(fdf[sel, , drop = FALSE], ie_f[sel], nm),
                 hh = h, stringsAsFactors = FALSE)
    }))
  }))

  vocab <- list(any = distinct(rules_any, rule, hh),
                typed = distinct(rules_typed, rule, hh),
                params = list(XGB = XGB, RF = RF))
  saveRDS(vocab, ckpt)
  cat(sprintf("Vocabularies checkpointed to %s\n", ckpt))
}

## ── 3. Screen ONCE on the union, then dedup per pool ─────────────────────────

u <- bind_rows(mutate(vocab$typed, pool = "typed"),
               mutate(vocab$any,   pool = "any")) %>%
  group_by(rule, hh) %>%
  summarise(in_typed = any(pool == "typed"), in_any = any(pool == "any"),
            .groups = "drop")
cat(sprintf("\nUnion of vocabularies: %d rules (typed %d, any %d, shared %d)\n",
            nrow(u), sum(u$in_typed), sum(u$in_any), sum(u$in_typed & u$in_any)))

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

# per-pool dedup on subsets of the screened union; returns surviving row ids
dedup_pool <- function(rows, label) {
  sdf <- u[rows, , drop = FALSE]; sidx <- idx_tr[rows]
  d1 <- dedup_exact_coverage(sdf, sidx)
  rows <- rows[!d1]; sdf <- sdf[!d1, , drop = FALSE]
  d2 <- dedup_dominated(sdf, sdf$stat)
  cat(sprintf("%s: -%d coverage, -%d dominated -> %d rules\n",
              label, sum(d1), sum(d2), sum(!d2)))
  rows[!d2]
}
rows_typed <- dedup_pool(which(u$in_typed), "Typed (4 frames)")
rows_any   <- dedup_pool(which(u$in_any),   "Any-error (1 model)")
# Combined reuses the parents' survivors and re-dedups only across pools
rows_comb  <- dedup_pool(sort(unique(c(rows_typed, rows_any))), "Combined")

pools <- list("Typed (4 frames)" = rows_typed,
              "Any-error (1 model)" = rows_any,
              "Combined" = rows_comb)

## ── 4. Hold-out flags ONCE for all survivors, sweep per pool ─────────────────

needed <- sort(unique(unlist(pools)))
remap  <- match(seq_len(nrow(u)), needed)   # u-row -> position in `needed`
idx_h_needed <- flags_for_rules(u[needed, , drop = FALSE], hold, strata_h,
                                label = "holdout (survivor union, once)")

overall <- bind_rows(lapply(names(pools), function(nm) {
  rows <- pools[[nm]]
  sw <- precision_sweep(u$stat[rows], !is.na(u$stat[rows]),
                        idx_h_needed[remap[rows]], tg_h$ie, tg_h$ed,
                        THRESHOLD_GRID[THRESHOLD_GRID >= PRUNE_MIN_PRECISION])
  sw$approach <- nm
  sw$x <- if (OBJECTIVE == "dollars") sw$dollar_recall else sw$recall
  sw
}))
overall$approach <- factor(overall$approach, levels = names(pools))
write.csv(overall, file.path(out_dir, "anyerror_vs_typed_sweep.csv"), row.names = FALSE)

## ── 5. Delta at matched recall + headline summary ─────────────────────────────

interp_prec <- function(df, grid) {
  df <- df[!is.na(df$x) & !is.na(df$precision), ]
  if (nrow(df) < 2) return(rep(NA_real_, length(grid)))
  approx(df$x, df$precision, xout = grid, ties = mean, rule = 1)$y
}
xmax <- suppressWarnings(min(tapply(overall$x, overall$approach, max, na.rm = TRUE)))
grid <- seq(0.02, ifelse(is.finite(xmax), xmax, 0.5), by = 0.02)

summary_tbl <- overall %>%
  group_by(approach) %>%
  summarise(mean_precision = mean(interp_prec(pick(everything()), grid), na.rm = TRUE),
            max_recall = max(x, na.rm = TRUE), .groups = "drop") %>%
  mutate(n_rules     = vapply(as.character(approach), function(a) length(pools[[a]]), 0),
         n_shortlist = vapply(as.character(approach), function(a)
           sum(u$stat[pools[[a]]] >= MIN_PRECISION, na.rm = TRUE), 0),
         delta_vs_typed = mean_precision - mean_precision[approach == "Typed (4 frames)"])
write.csv(summary_tbl, file.path(out_dir, "anyerror_vs_typed_summary.csv"), row.names = FALSE)
cat("\nMean precision at matched recall (common grid):\n")
print(as.data.frame(summary_tbl), digits = 3)

## ── 6. Presentation plot ──────────────────────────────────────────────────────

cols <- c("Typed (4 frames)" = "#d1495b", "Any-error (1 model)" = "#0073b7",
          "Combined" = "#1b1b1b")
p <- ggplot(overall, aes(x, precision, color = approach)) +
  geom_line(linewidth = 0.9) + geom_point(size = 1.2) +
  geom_text(aes(label = sprintf("%.2f", threshold)), size = 2.4, vjust = -0.75,
            show.legend = FALSE, check_overlap = TRUE) +
  scale_color_manual(values = cols) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = if (OBJECTIVE == "dollars") "Hold-out recall of ALL payment error DOLLARS (2023)"
           else "Hold-out recall of ALL payment error CASES (2023)",
       y = "Hold-out precision of the union of kept rules",
       color = NULL,
       title = "Does mining by error type beat one all-errors model?",
       subtitle = sprintf("Same engines, screens and 90%% Wilson-LCB selection on any-error train precision;\ntrained %s, scored on %s; point labels = LCB floor",
                          paste(TRAIN_YEARS, collapse = "/"), paste(HOLDOUT_YEARS, collapse = "/")),
       caption = cap) +
  theme_minimal(base_size = 13) + theme(legend.position = "top")
save_png(p, file.path(out_dir, "anyerror_vs_typed_sweep.png"), 9, 5.5)

cat(sprintf("\nWrote head-to-head outputs to %s/\n", out_dir))
