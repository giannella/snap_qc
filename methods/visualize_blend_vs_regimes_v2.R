# Blend vs state-only vs national-only, per state on the 2024 holdout:
# all three are frozen lists built on the state's 2022-23 caseload and
# walked to capacity on 2024 (sections 15-16), so the comparison is at
# identical review volume. Panels: precision | share of error dollars.
#
# Output: blend_vs_state_vs_national_budget05/10.png

suppressMessages({library(dplyr); library(ggplot2); library(tidyr)})
source("rule_mining_helpers.R")
out <- "methods/state_similarity_v2/transfer_benchmark_train2223_test24"

natl <- read.csv(file.path(out, "frozen_list_results.csv")) %>%
  transmute(target, budget, list = "national pool only",
            precision = precision_deployed,
            `share of error $ caught` = dollar_recall_deployed)
own <- read.csv(file.path(out, "frozen_own_list_results.csv")) %>%
  transmute(target, budget, list = "state's own pool only",
            precision = precision_deployed,
            `share of error $ caught` = dollar_recall_deployed)
bl <- read.csv(file.path(out, "blended_frozen_results.csv")) %>%
  filter(variant == "lcb99") %>%
  transmute(target, budget, list = "blended (99% bound)",
            precision, `share of error $ caught` = dollar_recall)
d <- bind_rows(bl, natl, own)
LEVELS <- c("blended (99% bound)", "national pool only", "state's own pool only")

make_chart <- function(budget_val, fname, title_pct) {
  dd <- d %>% filter(budget == budget_val)
  ord <- dd %>% filter(list == LEVELS[1]) %>% arrange(precision) %>% pull(target)
  ddl <- dd %>%
    pivot_longer(c(precision, `share of error $ caught`), names_to = "metric") %>%
    mutate(metric = factor(metric, levels = c("precision", "share of error $ caught")),
           target = factor(target, levels = ord),
           list = factor(list, levels = LEVELS))

  p <- ggplot(ddl, aes(x = value, y = target)) +
    geom_line(aes(group = target), colour = "grey80", linewidth = 0.4) +
    geom_point(aes(shape = list, colour = list), size = 2.1, stroke = 0.9) +
    scale_shape_manual(values = setNames(c(16, 1, 2), LEVELS), name = NULL) +
    scale_colour_manual(values = setNames(c("black", "grey35", "grey35"), LEVELS),
                        name = NULL) +
    facet_wrap(~metric, nrow = 1, scales = "free_x") +
    labs(x = sprintf("frozen lists at a %s review budget, on the state's 2024 cases", title_pct),
         y = NULL,
         title = sprintf("Blend vs national vs own rules, %s budget", title_pct),
         subtitle = "all three frozen on the state's 2022-23 caseload,\nwalked to capacity: identical review volume") +
    guides(shape = guide_legend(nrow = 1)) +
    theme_minimal(base_size = 12.5) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position = "top")
  save_png(p, file.path(out, fname), 6.6, 5.8)
}
make_chart(0.10, "blend_vs_state_vs_national_budget10.png", "10%")
make_chart(0.05, "blend_vs_state_vs_national_budget05.png", "5%")
cat("wrote blend-vs-regimes charts to", out, "\n")
