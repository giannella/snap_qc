# ──────────────────────────────────────────────────────────────────────────────
# EXCLUSION rules v2 — no {pre}, no lasso, no nets.
#
# Mirror image of INCL v2: rules flag SAFE cases to EXCLUDE from an existing
# review pile. One frame (all cases, any error type), stratified by household
# size 1 / 2-3 / 4+. The pipeline is identical — the target is inverted:
#
#   "precision"  -> CLEAN RATE  = share of flagged (excluded) cases with no
#                   over-threshold error of ANY type
#   "recall"     -> CLEAN CUT   = share of the stratum's clean cases excluded
#   "dollar_recall" -> ERROR DOLLARS LOST = share of the stratum's error
#                   dollars sitting in the excluded pile (want this LOW;
#                   dollar retention = 1 - this)
#
# Selection statistic: one-sided Wilson LOWER BOUND of the train clean rate,
# for the same winner's-curse reason as INCL. Same-structure dominance and
# coverage dedup apply unchanged.
#
# Expects `reg_model_data` in the environment. Logic lives in
# rule_mining_helpers.R. Outputs land in exclusion_rules_by_hh_size_v2/.
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

features <- c(
  "HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size",
  "expedited_i", "cat_elig", "rawben_rel_max", "medical_deductions",
  "shelter_expenses_by_hh_size", "utilities", "married", "homeless",
  "rawearn_by_hh_size", "rawunearn_by_hh_size", "rawgross_by_hh_size",
  "percent_abawd", "unc_rawben_rel_max",
  "months_since_cert_n", "count_divisible_by_100"
)

XGB <- list(nrounds = 300, max_depth = 4, eta = 0.05, subsample = 0.5)
RF  <- list(num_trees = 500, max_depth = 4, mtry = 1, min_node_size = 20)

SIGNIF_DIGITS <- 3

# Clean-rate floors. Base clean rate is ~92%, so the grid lives near 1:
# excluding at a 0.995 floor means <= ~0.5% of excluded cases carry an error.
LCB_Z             <- 1.645  # kept at one-sided 95% (INCL uses 90%): declaring a
                            # case as safe to SKIP warrants the stricter bound
THRESHOLD_GRID    <- c(0.90, 0.925, 0.95, 0.96, 0.97, 0.98, 0.985, 0.99, 0.995, 0.999)
MIN_TRAIN_FLAGGED <- 25     # exclusions warrant more support than inclusions
PRUNE_MIN_CLEAN   <- min(THRESHOLD_GRID)
MIN_CLEAN_RATE    <- 0.99   # shortlist floor, applied to the train clean-rate LCB
RETENTION_MARKS   <- c(0.97, 0.99)  # console: best workload cut at these dollar retentions

out_dir <- "exclusion_rules_by_hh_size_v2"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

## ── 1. Data: one frame, all error types ───────────────────────────────────────

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
  list(is_clean = !ie, err_dollars = ifelse(ie, abs(amt), 0))
}
tg_tr <- targets_of(train); tg_h <- targets_of(hold)

strata_tr <- lapply(setNames(nm = HH_LEVELS), function(h)
  which(hh_group_of(train[[HH_SIZE_COL]]) %in% h))
strata_h  <- lapply(setNames(nm = HH_LEVELS), function(h)
  which(hh_group_of(hold[[HH_SIZE_COL]]) %in% h))

cat(sprintf("Train %s: %d rows (%.1f%% clean) | Holdout %s: %d rows (%.1f%% clean)\n",
            paste(TRAIN_YEARS, collapse = "/"), nrow(train), 100 * mean(tg_tr$is_clean),
            paste(HOLDOUT_YEARS, collapse = "/"), nrow(hold), 100 * mean(tg_h$is_clean)))

## ── 2. Generate per stratum ───────────────────────────────────────────────────
# Trees are fit on the SAME error-vs-clean target as INCL (splits are symmetric
# for a binary outcome); the clean-rate screen below keeps the safe-direction
# rules instead of the error-direction ones.

rules_df <- bind_rows(lapply(HH_LEVELS, function(h) {
  ix  <- strata_tr[[h]]
  sub <- train[ix, , drop = FALSE]; ie_s <- !tg_tr$is_clean[ix]
  cat(sprintf("\n-- HH %s: %d train rows, %d errors (%.2f%%)\n",
              h, nrow(sub), sum(ie_s), 100 * mean(ie_s)))
  rx <- canonicalize_rules(
    generate_rules_xgboost(sub, ie_s, pv, nrounds = XGB$nrounds,
                           max_depth = XGB$max_depth, eta = XGB$eta,
                           subsample = XGB$subsample, seed = 117),
    SIGNIF_DIGITS)
  rr <- canonicalize_rules(
    generate_rules_ranger(sub, ie_s, pv, num_trees = RF$num_trees,
                          max_depth = RF$max_depth, mtry = RF$mtry,
                          min_node_size = RF$min_node_size, seed = 117),
    SIGNIF_DIGITS)
  cat(sprintf("   xgboost rules: %d | ranger(mtry=1) rules: %d\n", length(rx), length(rr)))
  bind_rows(data.frame(rule = rx, hh = h, source = "xgboost", stringsAsFactors = FALSE),
            data.frame(rule = rr, hh = h, source = "ranger_mtry1", stringsAsFactors = FALSE))
})) %>%
  group_by(hh, rule) %>%
  summarise(source = paste(sort(unique(source)), collapse = "+"), .groups = "drop")

## ── 3. Screen on train clean rate, dedup ──────────────────────────────────────

idx_tr <- flags_for_rules(rules_df, train, strata_tr, label = "train")
n_tr   <- lengths(idx_tr)
k_tr   <- vapply(idx_tr, function(ix) sum(tg_tr$is_clean[ix]), numeric(1))
raw_tr <- ifelse(n_tr > 0, k_tr / n_tr, NA_real_)
base_clean <- vapply(rules_df$hh, function(h) mean(tg_tr$is_clean[strata_tr[[h]]]), numeric(1))

keep <- !is.na(raw_tr) & n_tr >= MIN_TRAIN_FLAGGED &
        raw_tr >= PRUNE_MIN_CLEAN & raw_tr > base_clean
cat(sprintf("\nscreen: %d of %d rules kept (support >= %d, clean rate >= %.3f, above base)\n",
            sum(keep), nrow(rules_df), MIN_TRAIN_FLAGGED, PRUNE_MIN_CLEAN))
rules_df <- rules_df[keep, , drop = FALSE]
idx_tr <- idx_tr[keep]; n_tr <- n_tr[keep]; k_tr <- k_tr[keep]

rules_df$n_flagged_train    <- n_tr
rules_df$clean_rate_train   <- round(k_tr / n_tr, 5)
rules_df$clean_rate_train_lcb <- round(wilson_lcb(k_tr, n_tr, LCB_Z), 5)

drop_cov <- dedup_exact_coverage(rules_df, idx_tr)
rules_df <- rules_df[!drop_cov, , drop = FALSE]; idx_tr <- idx_tr[!drop_cov]
drop_dom <- dedup_dominated(rules_df, rules_df$clean_rate_train_lcb)
rules_df <- rules_df[!drop_dom, , drop = FALSE]; idx_tr <- idx_tr[!drop_dom]
cat(sprintf("dedup: -%d exact-coverage, -%d dominated -> %d rules\n",
            sum(drop_cov), sum(drop_dom), nrow(rules_df)))

## ── 4. Hold-out evaluation ────────────────────────────────────────────────────

idx_h <- flags_for_rules(rules_df, hold, strata_h, label = "holdout")

ev <- eval_rules_on(rules_df, idx_h, tg_h$is_clean, tg_h$err_dollars, strata_h, "holdout")
# translate INCL column names into exclusion semantics
names(ev) <- sub("^precision_",     "clean_rate_",       names(ev))
names(ev) <- sub("^recall_",        "clean_cut_",        names(ev))
names(ev) <- sub("^dollar_recall_", "err_dollars_lost_", names(ev))
names(ev) <- sub("^errors_caught_", "clean_flagged_",    names(ev))

rule_eval <- bind_cols(rules_df, ev) %>% arrange(hh, desc(clean_rate_train_lcb))
write.csv(rule_eval, file.path(out_dir, "exclusion_rules_all.csv"), row.names = FALSE)

shortlist <- rule_eval %>% filter(clean_rate_train_lcb >= MIN_CLEAN_RATE)
write.csv(shortlist, file.path(out_dir, "exclusion_rules_highclean.csv"), row.names = FALSE)
cat(sprintf("shortlist (train clean-rate LCB >= %.3f): %d rules | median holdout clean rate %.4f\n",
            MIN_CLEAN_RATE, nrow(shortlist),
            median(shortlist$clean_rate_holdout, na.rm = TRUE)))

## ── 5. Clean-rate LCB sweep: workload cut vs dollars retained ─────────────────

stat   <- rules_df$clean_rate_train_lcb
usable <- !is.na(stat)
sw <- precision_sweep(stat, usable, idx_h, tg_h$is_clean, tg_h$err_dollars,
                      THRESHOLD_GRID)
names(sw) <- c("clean_rate_floor", "n_rules", "n_excluded", "workload_cut",
               "clean_rate", "clean_cut", "err_dollars_lost")
sw$dollar_retention <- 1 - sw$err_dollars_lost
write.csv(sw, file.path(out_dir, "exclusion_lcb_sweep.csv"), row.names = FALSE)
print(sw[, c("clean_rate_floor", "n_rules", "workload_cut", "clean_rate",
             "err_dollars_lost", "dollar_retention")], row.names = FALSE)

for (rt in RETENTION_MARKS) {
  ok <- sw[!is.na(sw$dollar_retention) & sw$dollar_retention >= rt, , drop = FALSE]
  if (nrow(ok) > 0) {
    best <- ok[which.max(ok$workload_cut), ]
    cat(sprintf("retention >= %.2f: cut %.1f%% of the pile (floor %.3f, %d rules, %.4f clean)\n",
                rt, 100 * best$workload_cut, best$clean_rate_floor,
                best$n_rules, best$clean_rate))
  } else cat(sprintf("retention >= %.2f: no floor achieves it\n", rt))
}

p <- ggplot(sw, aes(workload_cut, dollar_retention)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.0) +
  geom_text(aes(label = sprintf("%.3f", clean_rate_floor)), size = 2.4, vjust = -0.7,
            check_overlap = TRUE) +
  labs(x = "Workload cut (share of hold-out pile excluded)",
       y = "Error-dollar retention (1 - dollars excluded)",
       title = "Exclusion frontier across train clean-rate LCB floors",
       subtitle = sprintf("xgboost + ranger(mtry=1), trained %s, scored on %s; labels = LCB floor",
                          paste(TRAIN_YEARS, collapse = "/"), paste(HOLDOUT_YEARS, collapse = "/"))) +
  theme_minimal(base_size = 12)
save_png(p, file.path(out_dir, "exclusion_lcb_sweep.png"), 8, 5)

cat(sprintf("\nWrote exclusion outputs to %s/\n", out_dir))

## ── Notes ─────────────────────────────────────────────────────────────────────
# - Errors here are ANY over-threshold error (all types), so an excluded case
#   is only "safe" if it carries no error at all — no frame-relative blind spot.
# - The sweep's union is the set of cases excluded by AT LEAST ONE qualifying
#   rule; a clean case excluded by several rules counts once, so workload cut
#   is never overstated, mirroring the INCL recall logic.
# - MIN_TRAIN_FLAGGED is higher than INCL's because exclusion mistakes are
#   costlier: a small lucky rule that excludes error dollars hurts directly.
