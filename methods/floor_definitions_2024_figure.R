# Year-swapped version of the educational floor-definitions figure (deck
# comment 2026-07-12: "is the chart really scored on 2023? can we get a
# version scored on 2024?"). The original reuses the production vocabulary,
# which was TRAINED on 2022+2024 -- so a 2024-scored version must re-mine.
# This script mines the unearned frame on 2022+2023 with the exact INCL
# recipe (screen: support >= 10, raw >= 0.05, above base rate; coverage +
# dominance dedup), checkpoints the vocabulary, and reproduces the
# three-column figure scored on 2024.
# Output: presentation_figures/floor_definitions_educational_2024.png
#         methods/yearswap_vocab/unearned_income_rules_2223.csv

suppressMessages({library(dplyr); library(ggplot2)})
source("rule_mining_helpers.R")
set.seed(117)

FRAME <- "unearned_income"
TRAIN_YEARS   <- c("2022", "2023")
HOLDOUT_YEARS <- c("2024")
THRESHOLD_GRID <- seq(0.05, 0.95, by = 0.05)
LCB_Z <- 2.326
MIN_TRAIN_FLAGGED <- 10
SIGNIF_DIGITS <- 3
XGB <- list(nrounds = 1000, max_depth = 4, eta = 0.02, subsample = 0.20)
RF  <- list(num_trees = 1000, max_depth = 4, mtry = 2, min_node_size = 20)
VOCAB_DIR <- "methods/yearswap_vocab"
dir.create(VOCAB_DIR, showWarnings = FALSE, recursive = TRUE)

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
targets_of <- function(df) {
  ie <- !is.na(df$over_threshold) & df$over_threshold != 0
  amt <- df$total_error_amount; amt[is.na(amt)] <- 0
  list(ie = ie, ed = ifelse(ie, abs(amt), 0))
}

frame_all <- reg_model_data %>%
  filter(error_status %in% c("unearned_overissuance", "no_error"),
         fiscal_year %in% c(TRAIN_YEARS, HOLDOUT_YEARS))
universe  <- reg_model_data %>% filter(fiscal_year %in% HOLDOUT_YEARS)
pf   <- prep_features(frame_all, features)
fdf  <- pf$data; pv <- pf$features
univ <- prep_features(universe, features)$data
yr    <- as.character(fdf$fiscal_year)
train <- fdf[yr %in% TRAIN_YEARS, , drop = FALSE]
hold  <- fdf[yr %in% HOLDOUT_YEARS, , drop = FALSE]
tg_tr <- targets_of(train); tg_h <- targets_of(hold); tg_u <- targets_of(univ)
strata_tr <- lapply(setNames(nm = HH_LEVELS), function(h)
  which(hh_group_of(train[[HH_SIZE_COL]]) %in% h))
strata_h <- lapply(setNames(nm = HH_LEVELS), function(h)
  which(hh_group_of(hold[[HH_SIZE_COL]]) %in% h))
strata_u <- lapply(setNames(nm = HH_LEVELS), function(h)
  which(hh_group_of(univ[[HH_SIZE_COL]]) %in% h))

vocab_csv <- file.path(VOCAB_DIR, "unearned_income_rules_2223.csv")
if (file.exists(vocab_csv)) {
  rules_df <- read.csv(vocab_csv, stringsAsFactors = FALSE)
  cat(sprintf("vocabulary checkpoint: %d rules\n", nrow(rules_df)))
} else {
  rules_df <- bind_rows(lapply(HH_LEVELS, function(h) {
    ix <- strata_tr[[h]]
    sub <- train[ix, , drop = FALSE]; ie_s <- tg_tr$ie[ix]
    cat(sprintf("-- HH %s: %d train rows, %d errors\n", h, nrow(sub), sum(ie_s)))
    if (nrow(sub) < 100 || sum(ie_s) < 10) return(NULL)
    rx <- canonicalize_rules(
      generate_rules_xgboost(sub, ie_s, pv, nrounds = XGB$nrounds,
                             max_depth = XGB$max_depth, eta = XGB$eta,
                             subsample = XGB$subsample, seed = 117), SIGNIF_DIGITS)
    rr <- canonicalize_rules(
      generate_rules_ranger(sub, ie_s, pv, num_trees = RF$num_trees,
                            max_depth = RF$max_depth, mtry = RF$mtry,
                            min_node_size = RF$min_node_size, seed = 117), SIGNIF_DIGITS)
    bind_rows(data.frame(rule = rx, hh = h, stringsAsFactors = FALSE),
              data.frame(rule = rr, hh = h, stringsAsFactors = FALSE))
  })) %>% distinct(hh, rule)
  idx_tr <- flags_for_rules(rules_df, train, strata_tr, label = "train")
  n_tr <- lengths(idx_tr)
  k_tr <- vapply(idx_tr, function(ix) sum(tg_tr$ie[ix]), numeric(1))
  raw  <- ifelse(n_tr > 0, k_tr / n_tr, NA_real_)
  base <- vapply(rules_df$hh, function(h) mean(tg_tr$ie[strata_tr[[h]]]), numeric(1))
  keep <- !is.na(raw) & n_tr >= MIN_TRAIN_FLAGGED &
          raw >= min(THRESHOLD_GRID) & raw > base
  rules_df <- rules_df[keep, , drop = FALSE]; idx_tr <- idx_tr[keep]
  rules_df$n_flagged_train <- n_tr[keep]
  rules_df$precision_train <- round(raw[keep], 4)
  rules_df$precision_train_lcb <- round(wilson_lcb(k_tr[keep], n_tr[keep], LCB_Z), 4)
  drop_cov <- dedup_exact_coverage(rules_df, idx_tr)
  rules_df <- rules_df[!drop_cov, , drop = FALSE]; idx_tr <- idx_tr[!drop_cov]
  drop_dom <- dedup_dominated(rules_df, rules_df$precision_train_lcb)
  rules_df <- rules_df[!drop_dom, , drop = FALSE]
  write.csv(rules_df, vocab_csv, row.names = FALSE)
  cat(sprintf("mined + screened vocabulary: %d rules -> %s\n", nrow(rules_df), vocab_csv))
}

idx_h <- flags_for_rules(rules_df, hold, strata_h, label = "holdout")
idx_u <- flags_for_rules(rules_df, univ, strata_u, label = "universe")

sweep_both <- function(stat, floor_label) {
  usable <- !is.na(stat)
  bind_rows(
    precision_sweep(stat, usable, idx_h, tg_h$ie, tg_h$ed, THRESHOLD_GRID) %>%
      mutate(scoring = "frame only (mined error type)"),
    precision_sweep(stat, usable, idx_u, tg_u$ie, tg_u$ed, THRESHOLD_GRID) %>%
      mutate(scoring = "any error type")) %>%
    mutate(floor_type = floor_label)
}
raw_within_lcb <- ifelse(!is.na(rules_df$precision_train_lcb) &
                           rules_df$precision_train_lcb >= 0.20,
                         rules_df$precision_train, NA_real_)
sw <- bind_rows(
  sweep_both(rules_df$precision_train,     "raw trained precision, all rules"),
  sweep_both(raw_within_lcb,               "raw trained precision, only rules passing 99% bound at 0.20"),
  sweep_both(rules_df$precision_train_lcb, "99% lower bound precision"))
sw$floor_type <- factor(sw$floor_type, levels = c(
  "raw trained precision, all rules",
  "raw trained precision, only rules passing 99% bound at 0.20",
  "99% lower bound precision"))

long <- bind_rows(
  sw %>% mutate(metric = "hold-out precision", value = precision),
  sw %>% mutate(metric = "hold-out share of error $ caught", value = dollar_recall))
ref <- expand.grid(metric = "hold-out precision",
                   floor_type = levels(sw$floor_type))
ref$x <- 0.05; ref$y <- 0.05; ref$xend <- 0.95; ref$yend <- 0.95

p <- ggplot(long, aes(threshold, value, linetype = scoring)) +
  geom_segment(data = ref, aes(x = x, y = y, xend = xend, yend = yend),
               inherit.aes = FALSE, linewidth = 0.3, colour = "grey60") +
  geom_line(linewidth = 0.8) + geom_point(size = 1.0) +
  facet_grid(metric ~ floor_type, switch = "both", axes = "all",
             labeller = labeller(floor_type = label_wrap_gen(32),
                                 metric = label_wrap_gen(22))) +
  scale_x_continuous(limits = c(0.05, 0.95)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = NULL, y = NULL, linetype = "Scored against",
       title = sprintf("Three ways to define the precision floor - %s rules", FRAME),
       subtitle = "rules trained on 2022-23, scored on 2024; grey line = floor delivered exactly") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", aspect.ratio = 1,
        strip.placement = "outside",
        strip.text.x.bottom = element_text(size = 11, face = "bold",
                                           margin = margin(t = 6)),
        strip.text.y.left = element_text(size = 11, face = "bold", angle = 90,
                                         margin = margin(r = 6)),
        panel.spacing.x = unit(18, "pt"))

g <- ggplot2::ggplotGrob(p)
panel_cols <- sort(unique(g$layout$l[grepl("^panel", g$layout$name)]))
panel_rows <- range(c(g$layout$t[grepl("^panel|strip-b", g$layout$name)],
                      g$layout$b[grepl("^panel|strip-b", g$layout$name)]))
for (i in seq_len(length(panel_cols) - 1)) {
  gap_col <- panel_cols[i] + 2
  g <- gtable::gtable_add_grob(
    g, grid::segmentsGrob(x0 = 0.5, x1 = 0.5, y0 = 0, y1 = 1,
                          gp = grid::gpar(col = "grey55", lwd = 1.2)),
    t = panel_rows[1], b = panel_rows[2], l = gap_col, r = gap_col,
    name = sprintf("divider-%d", i))
}
png("presentation_figures/floor_definitions_educational_2024.png",
    width = 13, height = 9.5, units = "in", res = 300)
grid::grid.draw(g)
dev.off()
cat("wrote presentation_figures/floor_definitions_educational_2024.png\n")

cat("\ndelivered union precision vs floor (any-error scoring, 2024):\n")
print(sw %>% filter(scoring == "any error type",
                    threshold %in% c(0.2, 0.3, 0.4, 0.5, 0.6)) %>%
        transmute(floor_type, floor = threshold, n_rules,
                  delivered_precision = round(precision, 3),
                  dollar_recall = round(dollar_recall, 3)) %>%
        as.data.frame(), row.names = FALSE)
