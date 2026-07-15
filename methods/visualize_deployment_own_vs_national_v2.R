# Charts from the TIME-SHIFTED deployment benchmark (train 2022+2023, test
# the target state's 2024; methods/deployment_benchmark_train2223_test24.R).
# Here the honest national pool INCLUDES the target state: the test year was
# never seen, and the all-state list is what a deploying state actually gets.
#
# Charts:
#   deploy_national_dotplot_budget05/10.png  national_all per state (deck)
#   deploy_own_vs_national_budget05/10.png   own-state vs national, side-by-side
#                                            precision + $ panels per budget
#
# Reads methods/state_similarity_v2/transfer_benchmark_train2223_test24/
#   deployment_menu_train2223_test24.csv

suppressMessages({library(dplyr); library(ggplot2); library(tidyr)})
source("rule_mining_helpers.R")
out <- "methods/state_similarity_v2/transfer_benchmark_train2223_test24"

d <- read.csv(file.path(out, "deployment_menu_train2223_test24.csv"),
              stringsAsFactors = FALSE)

wilson_ci <- function(k, n, z = 1.96) {
  p <- k / n; dd <- 1 + z^2 / n
  c_ <- (p + z^2 / (2 * n)) / dd
  h  <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / dd
  list(lo = c_ - h, hi = c_ + h)
}

## ── national rules (all states incl. target), tested on 2024 ─────────────────
make_dotplot_national <- function(budget_val, fname, title_pct) {
  dn <- d %>%
    filter(budget == budget_val, approach == "national_all") %>%
    mutate(k = round(precision * n_flagged))
  ci <- wilson_ci(dn$k, dn$n_flagged)
  dn$lo <- ci$lo; dn$hi <- ci$hi

  p <- ggplot(dn, aes(x = precision, y = factor(target, levels = rev(sort(unique(target)))))) +
    geom_point(aes(x = target_base_rate), shape = 1, size = 2.4, colour = "grey45") +
    geom_pointrange(aes(xmin = lo, xmax = hi), size = 0.35) +
    labs(x = sprintf("precision at a %s review budget (95%% interval)\nopen circle = state 2024 base error rate", title_pct),
         y = NULL,
         title = sprintf("National rules at a %s review budget, tested on 2024", title_pct),
         subtitle = "rules mined on all states' 2022-23 data, scored on the state's 2024\ncases; everything right of the open circle is lift over random review") +
    expand_limits(x = 0) +
    theme_minimal(base_size = 12.5) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank())
  save_png(p, file.path(out, fname), 6.6, 4.4)
}
make_dotplot_national(0.10, "deploy_national_dotplot_budget10.png", "10%")
make_dotplot_national(0.05, "deploy_national_dotplot_budget05.png", "5%")

## ── own-state vs national, side-by-side panels per budget ────────────────────
make_own_vs_national <- function(budget_val, fname, title_pct) {
  dd <- d %>%
    filter(budget == budget_val, approach %in% c("own_state", "national_all")) %>%
    select(target, approach, precision, dollar_recall, target_base_rate)
  ord <- rev(sort(unique(dd$target)))
  ddl <- dd %>%
    pivot_longer(c(precision, dollar_recall), names_to = "metric") %>%
    mutate(metric = factor(metric, levels = c("precision", "dollar_recall"),
                           labels = c("precision", "share of error $ caught")),
           target = factor(target, levels = ord))
  seg <- ddl %>%
    select(target, metric, approach, value) %>%
    pivot_wider(names_from = approach, values_from = value)

  p <- ggplot(ddl, aes(x = value, y = target)) +
    geom_segment(data = seg, aes(x = own_state, xend = national_all,
                                 y = target, yend = target),
                 colour = "grey70", inherit.aes = FALSE) +
    geom_point(aes(shape = approach), size = 2.3, stroke = 0.9) +
    scale_shape_manual(values = c(own_state = 1, national_all = 16),
                       labels = c(own_state = "state's own rules",
                                  national_all = "national rules"),
                       breaks = c("national_all", "own_state"), name = NULL) +
    facet_wrap(~metric, nrow = 1, scales = "free_x") +
    labs(x = sprintf("at a %s review budget, on the state's 2024 cases", title_pct),
         y = NULL,
         title = sprintf("A state's own rules vs the national rules, %s budget", title_pct),
         subtitle = "both mined on 2022-23 only and tested on 2024; own-state mining\nswings from best-in-class to worse than random review") +
    theme_minimal(base_size = 12.5) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position = "top")
  save_png(p, file.path(out, fname), 6.6, 4.8)
}
make_own_vs_national(0.10, "deploy_own_vs_national_budget10.png", "10%")
make_own_vs_national(0.05, "deploy_own_vs_national_budget05.png", "5%")

cat("wrote deployment dot plots + own-vs-national comparisons to", out, "\n")
