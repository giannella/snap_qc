# Educational figure: three ways to define the precision floor, side by side.
# Columns (separated by vertical rules): floors on raw trained precision over
# all rules (full winner's curse); floors on raw trained precision among rules
# passing the 99% lower bound at 0.20 (the production chart basis); floors on
# the 99% lower bound itself (the guarantee - never overpromises).
# Rows: hold-out precision (with a 45-degree "floor delivered exactly" line)
# and hold-out share of error $ caught. Uses the saved rule vocabulary
# (unearned_income_rules_all.csv) - no re-mining.
# Output: presentation_figures/floor_definitions_educational.png

suppressMessages({library(dplyr); library(ggplot2)})
source("rule_mining_helpers.R")
reg_model_data <- readRDS("reg_model_data.rds")

FRAME <- "unearned_income"
HOLDOUT_YEARS <- c("2023")
THRESHOLD_GRID <- seq(0.05, 0.95, by = 0.05)
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

frame_df <- reg_model_data %>%
  filter(error_status %in% c("unearned_overissuance", "no_error"),
         fiscal_year %in% HOLDOUT_YEARS)
universe <- reg_model_data %>% filter(fiscal_year %in% HOLDOUT_YEARS)
hold <- prep_features(frame_df, features)$data
univ <- prep_features(universe, features)$data

targets_of <- function(df) {
  ie <- !is.na(df$over_threshold) & df$over_threshold != 0
  amt <- df$total_error_amount; amt[is.na(amt)] <- 0
  list(ie = ie, ed = ifelse(ie, abs(amt), 0))
}
tg_h <- targets_of(hold); tg_u <- targets_of(univ)
strata_h <- lapply(setNames(nm = HH_LEVELS), function(h)
  which(hh_group_of(hold[[HH_SIZE_COL]]) %in% h))
strata_u <- lapply(setNames(nm = HH_LEVELS), function(h)
  which(hh_group_of(univ[[HH_SIZE_COL]]) %in% h))

rules_df <- read.csv(sprintf("inclusion_rules_by_hh_size_v2/%s_rules_all.csv", FRAME),
                     stringsAsFactors = FALSE)
cat(sprintf("%d rules in vocabulary\n", nrow(rules_df)))
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

# columns = floor definition (name printed under each x axis),
# rows = metric (name printed vertically next to each y axis)
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
       subtitle = "same rule pool, scored on 2023; grey line = floor delivered exactly") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", aspect.ratio = 1,
        strip.placement = "outside",
        strip.text.x.bottom = element_text(size = 11, face = "bold",
                                           margin = margin(t = 6)),
        strip.text.y.left = element_text(size = 11, face = "bold", angle = 90,
                                         margin = margin(r = 6)),
        panel.spacing.x = unit(18, "pt"))

# vertical rules dividing the three floor-definition columns; the divider
# lives in the panel-spacing column (panel left col + 2: [+1] is a 0-width
# gutter, [+2] is the actual spacing column)
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
dir.create("presentation_figures", showWarnings = FALSE)
png("presentation_figures/floor_definitions_educational.png",
    width = 13, height = 9.5, units = "in", res = 300)
grid::grid.draw(g)
dev.off()
cat("wrote presentation_figures/floor_definitions_educational.png\n")

## calibration table at key floors
cat("\ndelivered union precision vs floor (any-error scoring):\n")
print(sw %>% filter(scoring == "any error type",
                    threshold %in% c(0.2, 0.3, 0.4, 0.5, 0.6)) %>%
        transmute(floor_type, floor = threshold, n_rules,
                  delivered_precision = round(precision, 3),
                  dollar_recall = round(dollar_recall, 3)) %>%
        as.data.frame(), row.names = FALSE)
