# Dot-plot views of the SAME-ERA budgeted transfer benchmark (2022-24 both
# sides; methods/budgeted_transfer_menu_v2.R). In this benchmark the full
# national pool's training rows include the scored cases, so the honest
# baseline is the leave-one-state-out (LOO) pool and honest-pool charts
# exclude national_asis. (The time-shifted 2024-test charts, where including
# the target state IS honest, live in visualize_deployment_own_vs_national_v2.R.)
#
# Charts (Healy-style: ordered dot plots, pointrange uncertainty, direct labels):
#   dotplot_best_pool_budget05/10.png  best honest pool per state, labeled
#   dotplot_loo_budget05/10.png        LOO national only (no labels needed)
#   budget_slopes.png                  5% -> 10% movement, precision + $ panels
#   budget_move_diagnostic.csv         what changes between budgets, per state
#
# Reads methods/state_similarity_v2/transfer_benchmark/budgeted_menu_results.csv.

suppressMessages({library(dplyr); library(ggplot2); library(tidyr)})
source("rule_mining_helpers.R")
out <- "methods/state_similarity_v2/transfer_benchmark"

bud <- read.csv(file.path(out, "budgeted_menu_results.csv"), stringsAsFactors = FALSE)
base <- read.csv(file.path(out, "transfer_benchmark_results.csv"), stringsAsFactors = FALSE) %>%
  distinct(target, target_base_rate)
bud <- bud %>% left_join(base, by = "target")

wilson_ci <- function(k, n, z = 1.96) {
  p <- k / n; d <- 1 + z^2 / n
  c_ <- (p + z^2 / (2 * n)) / d
  h  <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / d
  list(lo = c_ - h, hi = c_ + h)
}

## ── best honest pool per state, one dot plot per budget ──────────────────────
make_dotplot <- function(budget_val, fname, title_pct) {
  d <- bud %>%
    filter(budget == budget_val, approach != "national_asis") %>%
    group_by(target) %>%
    slice_max(precision, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(k = round(precision * n_flagged))
  ci <- wilson_ci(d$k, d$n_flagged)
  d$lo <- ci$lo; d$hi <- ci$hi

  p <- ggplot(d, aes(x = precision, y = reorder(target, precision))) +
    geom_point(aes(x = target_base_rate), shape = 1, size = 2.4, colour = "grey45") +
    geom_pointrange(aes(xmin = lo, xmax = hi), size = 0.35) +
    geom_text(aes(x = hi, label = sub("national_", "", sub("transfer_", "", approach))),
              hjust = -0.25, size = 2.7, colour = "grey40") +
    labs(x = sprintf("precision at a %s review budget (95%% interval)\nopen circle = state base error rate", title_pct),
         y = NULL,
         title = sprintf("What a %s review budget delivers, state by state", title_pct),
         subtitle = "filled point = best out-of-state rule pool (labeled); everything right\nof its open circle is lift over random review") +
    expand_limits(x = c(0, max(d$hi) * 1.12)) +
    theme_minimal(base_size = 12.5) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank())
  save_png(p, file.path(out, fname), 6.6, 4.4)
}
make_dotplot(0.10, "dotplot_best_pool_budget10.png", "10%")
make_dotplot(0.05, "dotplot_best_pool_budget05.png", "5%")

## ── LOO-only versions: single approach, so no per-interval labels ────────────
make_dotplot_loo <- function(budget_val, fname, title_pct) {
  d <- bud %>%
    filter(budget == budget_val, approach == "national_loo") %>%
    mutate(k = round(precision * n_flagged))
  ci <- wilson_ci(d$k, d$n_flagged)
  d$lo <- ci$lo; d$hi <- ci$hi

  p <- ggplot(d, aes(x = precision, y = reorder(target, precision))) +
    geom_point(aes(x = target_base_rate), shape = 1, size = 2.4, colour = "grey45") +
    geom_pointrange(aes(xmin = lo, xmax = hi), size = 0.35) +
    labs(x = sprintf("precision at a %s review budget (95%% interval)\nopen circle = state base error rate", title_pct),
         y = NULL,
         title = sprintf("National rules, %s review budget, state by state", title_pct),
         subtitle = "national rule pool mined with the target state held out; everything\nright of the open circle is lift over random review") +
    expand_limits(x = 0) +
    theme_minimal(base_size = 12.5) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank())
  save_png(p, file.path(out, fname), 6.6, 4.4)
}
make_dotplot_loo(0.10, "dotplot_loo_budget10.png", "10%")
make_dotplot_loo(0.05, "dotplot_loo_budget05.png", "5%")

## ── 5% -> 10% movement as paired slope panels ────────────────────────────────
d3 <- bud %>%
  filter(approach != "national_asis") %>%
  group_by(target, budget) %>%
  slice_max(precision, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(approach_short = sub("national_", "", sub("transfer_", "", approach)))

# diagnostic table: what actually moves between 5% and 10% per state
diag3 <- d3 %>%
  select(target, budget, approach_short, precision, dollar_recall, workload, n_rules_used) %>%
  pivot_wider(names_from = budget,
              values_from = c(approach_short, precision, dollar_recall, workload, n_rules_used),
              names_sep = "_b") %>%
  mutate(precision_change = precision_b0.1 - precision_b0.05,
         recall_change = dollar_recall_b0.1 - dollar_recall_b0.05,
         pool_switched = approach_short_b0.05 != approach_short_b0.1) %>%
  arrange(precision_change)
write.csv(diag3, file.path(out, "budget_move_diagnostic.csv"), row.names = FALSE)

d3l <- d3 %>%
  select(target, budget, precision, dollar_recall) %>%
  pivot_longer(c(precision, dollar_recall), names_to = "metric") %>%
  mutate(metric = factor(metric, levels = c("precision", "dollar_recall"),
                         labels = c("precision", "share of error $ caught"))) %>%
  pivot_wider(names_from = budget, values_from = value, names_prefix = "b")
ord <- d3 %>% filter(budget == 0.1) %>% arrange(precision) %>% pull(target)
d3l$target <- factor(d3l$target, levels = ord)

p3 <- ggplot(d3l, aes(y = target)) +
  geom_segment(aes(x = b0.05, xend = b0.1, yend = target),
               colour = "grey60",
               arrow = arrow(length = unit(0.14, "cm"), type = "closed")) +
  geom_point(aes(x = b0.05), shape = 1, size = 2.2) +
  geom_point(aes(x = b0.1), size = 2.2) +
  facet_wrap(~metric, nrow = 1, scales = "free_x") +
  labs(x = "5% budget (open) → 10% budget (filled), best honest pool at each",
       y = NULL,
       title = "Doubling the review budget: what each state trades",
       subtitle = "dollars caught rise for every state; precision moves both ways\nbecause the added rules dilute some states' pools") +
  theme_minimal(base_size = 12.5) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())
save_png(p3, file.path(out, "budget_slopes.png"), 6.6, 4.6)

cat("wrote dot plots + budget_move_diagnostic.csv to", out, "\n")
