# Workshop versions of the deployment charts: the main 12 benchmark states
# PLUS the workshop extension (Maine, Maryland, Missouri, Massachusetts,
# District of Columbia, Tennessee), 18 states total. Same construction as
# visualize_deployment_own_vs_national_v2.R: train 2022+2023, test the
# state's 2024; the national pool includes the target (honest here because
# the test year is unseen).
#
# Charts (workshop_ prefix):
#   workshop_national_dotplot_budget05/10.png
#   workshop_own_vs_national_budget05/10.png
#
# Reads methods/state_similarity_v2/transfer_benchmark_train2223_test24/
#   deployment_menu_train2223_test24.csv + deployment_menu_workshop_extension.csv

suppressMessages({library(dplyr); library(ggplot2); library(tidyr)})
source("rule_mining_helpers.R")
out <- "methods/state_similarity_v2/transfer_benchmark_train2223_test24"

d <- bind_rows(
  read.csv(file.path(out, "deployment_menu_train2223_test24.csv"),
           stringsAsFactors = FALSE),
  read.csv(file.path(out, "deployment_menu_workshop_extension.csv"),
           stringsAsFactors = FALSE)) %>%
  filter(approach %in% c("own_state", "national_all")) %>%
  distinct(target, approach, budget, .keep_all = TRUE)

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

  p <- ggplot(dn, aes(x = precision, y = reorder(target, precision))) +
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
  save_png(p, file.path(out, fname), 6.6, 5.6)
}
make_dotplot_national(0.10, "workshop_national_dotplot_budget10.png", "10%")
make_dotplot_national(0.05, "workshop_national_dotplot_budget05.png", "5%")

## ── own-state vs national, side-by-side panels per budget ────────────────────
make_own_vs_national <- function(budget_val, fname, title_pct) {
  dd <- d %>%
    filter(budget == budget_val) %>%
    select(target, approach, precision, dollar_recall)
  ord <- dd %>% filter(approach == "national_all") %>%
    arrange(precision) %>% pull(target)
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
                 colour = "grey70", inherit.aes = FALSE, na.rm = TRUE) +
    geom_point(aes(shape = approach), size = 2.3, stroke = 0.9, na.rm = TRUE) +
    scale_shape_manual(values = c(own_state = 1, national_all = 16),
                       labels = c(own_state = "state's own rules",
                                  national_all = "national rules"),
                       breaks = c("national_all", "own_state"), name = NULL) +
    facet_wrap(~metric, nrow = 1, scales = "free_x") +
    labs(x = sprintf("at a %s review budget, on the state's 2024 cases", title_pct),
         y = NULL,
         title = sprintf("A state's own rules vs the national rules, %s budget", title_pct),
         subtitle = "both mined on 2022-23 only and tested on 2024; states without an\nown-rules point had too little data for any rule to survive the filter") +
    theme_minimal(base_size = 12.5) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position = "top")
  save_png(p, file.path(out, fname), 6.6, 5.8)
}
make_own_vs_national(0.10, "workshop_own_vs_national_budget10.png", "10%")
make_own_vs_national(0.05, "workshop_own_vs_national_budget05.png", "5%")

cat("wrote workshop deployment charts to", out, "\n")
