# ──────────────────────────────────────────────────────────────────────────────
# THREE-WAY state comparison: what should a state actually deploy?
#   (a) national rules used unchanged
#   (b) national rules with thresholds tuned to the state
#   (c) rules mined directly on the state's own data
#
# (a) and (b) come from the state grid search's saved summary (train 2022+2024,
# test 2023, hybrid criterion). This script adds (c) on the SAME split: mine on
# the state's 2022+2024 with the validated state criterion (raw precision >=
# 0.30 at >= 30 flagged cases), and score the shortlist union on the state's
# 2023 against any error. Outputs a combined summary + figure.
#
# Expects `reg_model_data`. Outputs -> methods/compare_state_options_v2/.
# ──────────────────────────────────────────────────────────────────────────────

suppressMessages({library(dplyr); library(ggplot2)})
source("rule_mining_helpers.R")
set.seed(117)

STATES <- c("Washington", "Connecticut", "North Carolina", "Louisiana",
            "Michigan", "Virginia", "Arizona")
TRAIN_YEARS <- c("2022", "2024")
TEST_YEARS  <- c("2023")
GRIDSEARCH_SUMMARY <- "archive/state_rules_v2/state_union_summary.csv"

TARGET_IS_ERROR <- quote(!is.na(over_threshold) & over_threshold != 0)
ERR_AMT_COL <- "total_error_amount"
HH_SIZE_COL <- "cert_HH_size_FS_n"
HH_LEVELS <- c("1", "2-3", "4+")
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
XGB <- list(nrounds = 1000, max_depth = 4, eta = 0.02, subsample = 0.20)
RF  <- list(num_trees = 1000, max_depth = 4, mtry = 2, min_node_size = 20)
SIGNIF_DIGITS <- 3; MIN_TRAIN_FLAGGED <- 30; PRUNE_MIN <- 0.05
MIN_PRECISION <- 0.30

FRAMES <- list(
  earned_income = "earned_overissuance", unearned_income = "unearned_overissuance",
  underissuance = "underissuance", other_error = "other_error",
  any_error = c("earned_overissuance", "unearned_overissuance",
                "underissuance", "other_error"))

out_dir <- "methods/compare_state_options_v2"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

adf <- prep_features(reg_model_data %>%
  filter(fiscal_year %in% c(TRAIN_YEARS, TEST_YEARS)), features)$data
pv <- prep_features(adf, features)$features

targets_of <- function(df) {
  ie <- eval(TARGET_IS_ERROR, envir = df); ie[is.na(ie)] <- FALSE
  amt <- df[[ERR_AMT_COL]]; amt[is.na(amt)] <- 0
  list(ie = ie, ed = ifelse(ie, abs(amt), 0))
}

mine_state <- function(st) {
  sdf <- adf[as.character(adf$state) == st, , drop = FALSE]
  yr <- as.character(sdf$fiscal_year)
  tr <- sdf[yr %in% TRAIN_YEARS, , drop = FALSE]
  te <- sdf[yr %in% TEST_YEARS, , drop = FALSE]
  tg_te <- targets_of(te)
  cat(sprintf("\n===== %s: mine on %d rows (%d errors), test on %d rows (%d errors) =====\n",
              st, nrow(tr), sum(targets_of(tr)$ie), nrow(te), sum(tg_te$ie)))

  short_all <- list()
  for (fr in names(FRAMES)) {
    ftr <- tr %>% filter(error_status %in% c(FRAMES[[fr]], "no_error"))
    tg <- targets_of(ftr)
    strata <- lapply(setNames(nm = HH_LEVELS), function(h)
      which(hh_group_of(ftr[[HH_SIZE_COL]]) %in% h))
    rules <- bind_rows(lapply(HH_LEVELS, function(h) {
      ix <- strata[[h]]; sub <- ftr[ix, , drop = FALSE]; ie_s <- tg$ie[ix]
      if (length(ix) < 100 || sum(ie_s) < 10) return(NULL)
      rx <- canonicalize_rules(generate_rules_xgboost(sub, ie_s, pv,
        nrounds = XGB$nrounds, max_depth = XGB$max_depth, eta = XGB$eta,
        subsample = XGB$subsample, seed = 117), SIGNIF_DIGITS)
      rr <- canonicalize_rules(generate_rules_ranger(sub, ie_s, pv,
        num_trees = RF$num_trees, max_depth = RF$max_depth, mtry = RF$mtry,
        min_node_size = RF$min_node_size, seed = 117), SIGNIF_DIGITS)
      data.frame(rule = c(rx, rr), hh = h, stringsAsFactors = FALSE)
    }))
    if (is.null(rules) || nrow(rules) == 0) next
    rules <- distinct(rules, rule, hh)
    idx <- flags_for_rules(rules, ftr, strata)
    n <- lengths(idx); k <- vapply(idx, function(ix) sum(tg$ie[ix]), numeric(1))
    raw <- ifelse(n > 0, k / n, NA_real_)
    base <- vapply(rules$hh, function(h) mean(tg$ie[strata[[h]]]), numeric(1))
    keep <- !is.na(raw) & n >= MIN_TRAIN_FLAGGED & raw >= PRUNE_MIN & raw > base
    rules <- rules[keep, , drop = FALSE]; idx <- idx[keep]
    rules$prec_tr <- ifelse(lengths(idx) > 0,
      vapply(idx, function(ix) sum(tg$ie[ix]), numeric(1)) / lengths(idx), NA_real_)
    d1 <- dedup_exact_coverage(rules, idx); rules <- rules[!d1, , drop = FALSE]; idx <- idx[!d1]
    d2 <- dedup_dominated(rules, rules$prec_tr); rules <- rules[!d2, , drop = FALSE]
    sl <- rules[rules$prec_tr >= MIN_PRECISION, , drop = FALSE]
    sl <- sl[collapse_ladders(sl, sl$prec_tr), , drop = FALSE]
    if (nrow(sl) > 0) short_all[[fr]] <- sl
  }
  comb <- bind_rows(short_all)
  if (nrow(comb) == 0) {
    cat("  no mined rules qualified\n")
    return(data.frame(state = st, n_rules_mined = 0, flagged = 0, precision = NA,
                      recall = 0, dollar_recall = 0))
  }
  strata_te <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(te[[HH_SIZE_COL]]) %in% h))
  idx_te <- flags_for_rules(distinct(comb, rule, hh), te, strata_te)
  un <- rep(FALSE, nrow(te)); for (ix in idx_te) un[ix] <- TRUE
  out <- data.frame(state = st, n_rules_mined = nrow(comb), flagged = sum(un),
    precision = round(sum(tg_te$ie[un]) / max(sum(un), 1), 3),
    recall = round(sum(tg_te$ie[un]) / sum(tg_te$ie), 3),
    dollar_recall = round(sum(tg_te$ed[un]) / sum(tg_te$ed), 3))
  cat(sprintf("  mined %d rules | 2023 union: flag %d | precision %.3f | recall %.3f | $recall %.3f\n",
              out$n_rules_mined, out$flagged, out$precision, out$recall, out$dollar_recall))
  out
}

mined <- bind_rows(lapply(STATES, mine_state))

gs <- read.csv(GRIDSEARCH_SUMMARY, stringsAsFactors = FALSE)
three <- bind_rows(
  gs %>% transmute(state, option = "national rules as-is",
                   precision = precision_natl_all, recall = recall_natl_all,
                   dollar_recall = dollar_recall_natl_all, n_rules = NA),
  gs %>% transmute(state, option = "national rules, tuned to state",
                   precision = precision_tuned, recall = recall_tuned,
                   dollar_recall = dollar_recall_tuned, n_rules = n_rules_qualified),
  mined %>% transmute(state, option = "mined on the state's own data",
                      precision, recall, dollar_recall, n_rules = n_rules_mined))
write.csv(three, file.path(out_dir, "three_way_summary.csv"), row.names = FALSE)
cat("\n===== three-way summary =====\n")
print(three %>% arrange(state, option) %>% as.data.frame(), row.names = FALSE)

ord <- gs$state[order(-gs$n_rules_qualified)]
three$state <- factor(three$state, levels = ord)
three$option <- factor(three$option, levels = c(
  "national rules as-is", "national rules, tuned to state",
  "mined on the state's own data"))
pal <- c("national rules as-is" = "#8c8c8c",
         "national rules, tuned to state" = "#d1495b",
         "mined on the state's own data" = "#0073b7")
pC <- ggplot(three, aes(state, dollar_recall, fill = option)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(aes(label = ifelse(is.na(precision), "", sprintf("accuracy\n%.2f", precision))),
            position = position_dodge(width = 0.75), vjust = -0.12, size = 2.4,
            lineheight = 0.9) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.25))) +
  scale_fill_manual(values = pal) +
  labs(x = NULL, y = "Share of error dollars caught (2023 test year)", fill = NULL,
       title = "Three options for a state, tested on a year none of them saw",
       subtitle = "States ordered by how much of their own data supports rule work (left = most). Mining your own\nrules wins where data is deep; with thin data, the national rules used unchanged are the safe choice.") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", axis.text.x = element_text(angle = 20, hjust = 1))
save_png(pC, file.path(out_dir, "three_way_states.png"), 9.5, 5.4)
cat(sprintf("\nWrote summary + figure to %s/\n", out_dir))
