# driver.R: the shared harness for the three expert-practitioner skills
# (principal-data-scientist, senior-statistician, content-designer).
#
# It runs the REAL v2 five-stage rule-mining pipeline
# (generate -> canonicalize -> dedup -> evaluate -> sweep) from
# rule_mining_helpers.R on the REAL modelling frame (reg_model_data.rds), on
# ONE frame x the coarse HH-size strata, with SMALL ensembles so it finishes in
# ~1 min instead of the ~tens of minutes a full 5-frame / 1000-tree run takes.
# It is a faithful slice of INCL_find_inclusion_rules_by_hh_size_v2.R, not a
# re-implementation: same helpers, same screen/dedup/shortlist/sweep, same
# winner's-curse-corrected Wilson-LCB selection statistic.
#
# What it proves, and what each skill reads off it:
#   - principal-data-scientist: the pipeline runs end-to-end on real data and
#     emits a shortlist + a filter-floor sweep (the deliverable shape).
#   - senior-statistician: raw train precision vs its 99% Wilson LCB vs the
#     held-out number (the winner's curse, measured), and frame-relative vs
#     any-error precision side by side (deployed precision ~2x frame-relative).
#   - content-designer: the sweep PNG is the exact chart shape states see; the
#     printed table is the numbers a slide must carry.
#
# Usage (from the repo root, or anywhere, since it locates its own inputs):
#   Rscript .claude/skills/principal-data-scientist/driver.R
#   FRAME=earned_income XGB_NROUNDS=120 RF_TREES=120 Rscript .../driver.R
#
# Env knobs (all optional; fast defaults):
#   FRAME        one of earned_income unearned_income underissuance
#                other_error any_error   (default any_error)
#   XGB_NROUNDS  xgboost boosting rounds (default 80; prod is 1000)
#   RF_TREES     ranger trees            (default 80; prod is 1000)
#   TRAIN_YEARS  comma list              (default 2022,2024)
#   HOLDOUT_YEARS comma list             (default 2023)
#   OUT_DIR      where the CSV + PNG land (default $CLAUDE_JOB_DIR/tmp or ./driver_out)
#   DATA         path to reg_model_data.rds (auto-located otherwise)
#   HELPERS      path to rule_mining_helpers.R (auto-located otherwise)

suppressWarnings(suppressMessages({
  library(dplyr); library(ggplot2); library(ranger); library(xgboost)
}))

`%||%` <- function(a, b) if (is.null(a) || is.na(a) || !nzchar(a)) b else a
envc  <- function(k) { v <- Sys.getenv(k); if (nzchar(v)) v else NULL }

## ── Locate inputs (robust to running from a git worktree) ─────────────────────
# rule_mining_helpers.R is tracked, so it sits next to us; reg_model_data.rds is
# gitignored, so in a worktree it lives back in the primary checkout (/workspace).
find_first <- function(cands) { for (p in cands) if (nzchar(p) && file.exists(p)) return(p); NA_character_ }
helpers <- envc("HELPERS") %||% find_first(c(
  "rule_mining_helpers.R", "/workspace/rule_mining_helpers.R",
  file.path(dirname(dirname(dirname(getwd()))), "rule_mining_helpers.R")))
data_rds <- envc("DATA") %||% find_first(c(
  "reg_model_data.rds", "/workspace/reg_model_data.rds"))
if (is.na(helpers))  stop("cannot locate rule_mining_helpers.R (set HELPERS=)")
if (is.na(data_rds)) stop("cannot locate reg_model_data.rds (set DATA=); ",
                          "almost all work reads this file (see .devcontainer/README.md)")
source(helpers)

FRAME     <- envc("FRAME") %||% "any_error"
XGB_N     <- as.integer(envc("XGB_NROUNDS") %||% "80")
RF_N      <- as.integer(envc("RF_TREES") %||% "80")
TRAIN_YEARS   <- strsplit(envc("TRAIN_YEARS")   %||% "2022,2024", ",")[[1]]
HOLDOUT_YEARS <- strsplit(envc("HOLDOUT_YEARS") %||% "2023", ",")[[1]]
OUT_DIR   <- envc("OUT_DIR") %||% (
  { j <- Sys.getenv("CLAUDE_JOB_DIR"); if (nzchar(j)) file.path(j, "tmp") else "driver_out" })
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

cat(sprintf("driver: frame=%s xgb_nrounds=%d rf_trees=%d train=%s holdout=%s\n",
            FRAME, XGB_N, RF_N, paste(TRAIN_YEARS, collapse="/"),
            paste(HOLDOUT_YEARS, collapse="/")))
cat(sprintf("driver: helpers=%s data=%s out=%s\n", helpers, data_rds, OUT_DIR))

reg_model_data <- readRDS(data_rds)
cat(sprintf("driver: reg_model_data loaded: %d rows, %d cols\n",
            nrow(reg_model_data), ncol(reg_model_data)))

## ── Config mirrored from INCL_find_inclusion_rules_by_hh_size_v2.R ─────────────
set.seed(117)
YEAR_COL        <- "fiscal_year"
TARGET_IS_ERROR <- quote(!is.na(over_threshold) & over_threshold != 0)
ERR_AMT_COL     <- "total_error_amount"
HH_SIZE_COL     <- "cert_HH_size_FS_n"
HH_LEVELS       <- c("1", "2-3", "4+")
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
  "months_since_cert_n", "count_divisible_by_100")
XGB <- list(nrounds = XGB_N, max_depth = 4, eta = 0.02, subsample = 0.20)
RF  <- list(num_trees = RF_N, max_depth = 4, mtry = 2, min_node_size = 20)
SIGNIF_DIGITS <- 3
LCB_Z         <- 2.326                     # 99% one-sided Wilson lower bound
THRESHOLD_GRID    <- seq(0.05, 0.95, by = 0.05)
MIN_TRAIN_FLAGGED <- 10
PRUNE_MIN_PRECISION <- min(THRESHOLD_GRID)
MIN_PRECISION     <- 0.20

statuses <- switch(FRAME,
  earned_income   = "earned_overissuance",
  unearned_income = "unearned_overissuance",
  underissuance   = "underissuance",
  other_error     = "other_error",
  any_error       = c("earned_overissuance", "unearned_overissuance",
                      "underissuance", "other_error"),
  stop("unknown FRAME: ", FRAME))

frame_df <- reg_model_data %>%
  filter(error_status %in% c(statuses, "no_error")) %>%
  filter(.data[[YEAR_COL]] %in% c(TRAIN_YEARS, HOLDOUT_YEARS))
universe <- reg_model_data %>% filter(.data[[YEAR_COL]] %in% HOLDOUT_YEARS)

targets_of <- function(df) {
  ie <- eval(TARGET_IS_ERROR, envir = df); ie[is.na(ie)] <- FALSE
  amt <- df[[ERR_AMT_COL]]; amt[is.na(amt)] <- 0
  list(ie = ie, ed = ifelse(ie, abs(amt), 0))
}

## ── The real five stages ──────────────────────────────────────────────────────
pf  <- prep_features(frame_df, features)
pfu <- prep_features(universe, features)
fdf <- pf$data; pv <- pf$features; univ <- pfu$data
yr    <- as.character(fdf[[YEAR_COL]])
train <- fdf[yr %in% TRAIN_YEARS, , drop = FALSE]
hold  <- fdf[yr %in% HOLDOUT_YEARS, , drop = FALSE]
tg_tr <- targets_of(train); tg_h <- targets_of(hold); tg_u <- targets_of(univ)
strata <- function(d) lapply(setNames(nm = HH_LEVELS), function(h)
  which(hh_group_of(d[[HH_SIZE_COL]]) %in% h))
strata_tr <- strata(train); strata_h <- strata(hold); strata_u <- strata(univ)

cat(sprintf("\n[generate] mining %s: %d train rows (%d errors), %d holdout rows\n",
            FRAME, nrow(train), sum(tg_tr$ie), nrow(hold)))
rules_df <- mine_rule_vocabulary(
  train, setNames(list(list(rows = seq_len(nrow(train)), ie = tg_tr$ie)), FRAME),
  strata_tr, pv, xgb = XGB, rf = RF, signif_digits = SIGNIF_DIGITS, seed = 117)
stopifnot(!is.null(rules_df), nrow(rules_df) > 0)
rules_df$source <- rules_df$engines; rules_df$engines <- NULL; rules_df$mined_frames <- NULL
cat(sprintf("[canonicalize/dedup] %d canonical candidate rules\n", nrow(rules_df)))

idx_tr <- flags_for_rules(rules_df, train, strata_tr, label = "train")
n_tr <- lengths(idx_tr); k_tr <- vapply(idx_tr, function(ix) sum(tg_tr$ie[ix]), numeric(1))
raw_tr <- ifelse(n_tr > 0, k_tr / n_tr, NA_real_)
base_rate <- vapply(rules_df$hh, function(h) mean(tg_tr$ie[strata_tr[[h]]]), numeric(1))
keep <- !is.na(raw_tr) & n_tr >= MIN_TRAIN_FLAGGED & raw_tr >= PRUNE_MIN_PRECISION & raw_tr > base_rate
rules_df <- rules_df[keep, , drop = FALSE]; idx_tr <- idx_tr[keep]; n_tr <- n_tr[keep]; k_tr <- k_tr[keep]
cat(sprintf("[screen] %d rules kept (support>=%d, raw>=%.2f, above base rate)\n",
            nrow(rules_df), MIN_TRAIN_FLAGGED, PRUNE_MIN_PRECISION))
rules_df$n_flagged_train <- n_tr; rules_df$errors_train <- k_tr
rules_df$precision_train     <- round(k_tr / n_tr, 4)
rules_df$precision_train_lcb <- round(wilson_lcb(k_tr, n_tr, LCB_Z), 4)

drop_cov <- dedup_exact_coverage(rules_df, idx_tr)
rules_df <- rules_df[!drop_cov, , drop = FALSE]; idx_tr <- idx_tr[!drop_cov]
drop_dom <- dedup_dominated(rules_df, rules_df$precision_train_lcb)
rules_df <- rules_df[!drop_dom, , drop = FALSE]; idx_tr <- idx_tr[!drop_dom]
cat(sprintf("[dedup] -%d exact-coverage, -%d dominated -> %d rules\n",
            sum(drop_cov), sum(drop_dom), nrow(rules_df)))

idx_h <- flags_for_rules(rules_df, hold, strata_h, label = "holdout")
idx_u <- flags_for_rules(rules_df, univ, strata_u, label = "any-error universe")
rule_eval <- bind_cols(rules_df,
  eval_rules_on(rules_df, idx_h, tg_h$ie, tg_h$ed, strata_h, "holdout"),
  eval_rules_on(rules_df, idx_u, tg_u$ie, tg_u$ed, strata_u, "any")) %>%
  arrange(hh, desc(precision_train_lcb))
shortlist <- rule_eval %>% filter(precision_train_lcb >= MIN_PRECISION)
shortlist <- shortlist[collapse_ladders(shortlist, shortlist$precision_train_lcb), , drop = FALSE]

## ── What the three skills read off ────────────────────────────────────────────
cat("\n================= WINNER'S CURSE (senior-statistician) =================\n")
wc <- rule_eval %>%
  summarise(
    n_rules            = n(),
    median_raw_train   = median(precision_train, na.rm = TRUE),
    median_train_lcb   = median(precision_train_lcb, na.rm = TRUE),
    median_holdout     = median(precision_holdout, na.rm = TRUE),
    median_frame_prec  = median(precision_holdout, na.rm = TRUE),
    median_anyerror    = median(precision_any, na.rm = TRUE))
print(as.data.frame(round(wc, 4)))
cat("  ^ raw train precision is optimistic; the LCB pulls it toward holdout.\n")
cat("  ^ any-error precision > frame precision: deployed precision understated by frame-relative.\n")

cat("\n================= SHORTLIST (principal-data-scientist) =================\n")
cat(sprintf("shortlist (train LCB >= %.2f): %d rules | median holdout precision %.3f | median any-error %.3f\n",
            MIN_PRECISION, nrow(shortlist),
            median(shortlist$precision_holdout, na.rm = TRUE),
            median(shortlist$precision_any, na.rm = TRUE)))
if (nrow(shortlist) > 0) {
  show <- head(shortlist, 8)[, c("hh", "rule", "precision_train", "precision_train_lcb",
                                 "precision_holdout", "precision_any", "n_flagged_train")]
  print(as.data.frame(show), right = FALSE)
}

cat("\n================= FILTER-FLOOR SWEEP (content-designer chart) =================\n")
stat <- rules_df$precision_train_lcb; usable <- !is.na(stat)
grid <- THRESHOLD_GRID[THRESHOLD_GRID >= PRUNE_MIN_PRECISION]
sweep_frame <- precision_sweep(stat, usable, idx_h, tg_h$ie, tg_h$ed, grid)
sweep_any   <- precision_sweep(stat, usable, idx_u, tg_u$ie, tg_u$ed, grid)
sweeps <- bind_rows(
  sweep_frame %>% mutate(scoring = "frame only (mined error type)"),
  sweep_any   %>% mutate(scoring = "any error type"))
print(as.data.frame(sweeps[, c("scoring","threshold","n_rules","n_flagged","precision","recall","dollar_recall")]),
      row.names = FALSE)

sweeps$x <- sweeps$dollar_recall
sweep_long <- bind_rows(
  sweeps %>% mutate(metric = "hold-out precision", value = precision),
  sweeps %>% mutate(metric = "hold-out share of error $ caught", value = x))
p <- ggplot(sweep_long, aes(threshold, value, linetype = scoring)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.0) +
  facet_wrap(~metric, nrow = 1) +
  labs(x = "99% lower bound precision floor", y = NULL, linetype = "Scored against",
       title = sprintf("What the kept rules achieve together, by precision floor (%s)", FRAME),
       subtitle = sprintf("xgboost(%d)+ranger(%d) [quick-run ensembles], trained %s, scored on %s",
                          XGB_N, RF_N, paste(TRAIN_YEARS, collapse="/"),
                          paste(HOLDOUT_YEARS, collapse="/"))) +
  coord_cartesian(xlim = c(0.05, max(0.7, max(sweeps$threshold) + 0.05)), ylim = c(0, 1)) +
  theme_minimal(base_size = 12) + theme(legend.position = "top")
png_path <- file.path(OUT_DIR, sprintf("%s_lcb_sweep_quick.png", FRAME))
csv_path <- file.path(OUT_DIR, sprintf("%s_rules_quick.csv", FRAME))
save_png(p, png_path, 9, 4.5)
write.csv(rule_eval, csv_path, row.names = FALSE)

cat(sprintf("\ndriver: wrote %s\ndriver: wrote %s\n", png_path, csv_path))
cat("driver: OK. Real pipeline ran end-to-end on real data.\n")
