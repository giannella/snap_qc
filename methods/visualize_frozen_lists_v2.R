# Frozen-list results across the 18 workshop states (frozen_list_experiment_
# v2.R): each state's core list + ranked buffer is frozen against its own
# 2022-23 caseload covariates and deployed on 2024, walked until capacity
# fills (so workload always lands on the sizing -- no workload panel).
# Two aligned panels per budget: precision and share of error dollars --
# the frozen deliverable (filled) vs the batch fill of the full national
# pool against the year's realized caseload (open).
#
# Output: frozen_lists_panels_budget05/10.png (same folder as inputs)

suppressMessages({library(dplyr); library(ggplot2); library(tidyr)})
source("rule_mining_helpers.R")
out <- "methods/state_similarity_v2/transfer_benchmark_train2223_test24"

fz <- read.csv(file.path(out, "frozen_list_results.csv"), stringsAsFactors = FALSE)
sy <- bind_rows(
  read.csv(file.path(out, "deployment_menu_train2223_test24.csv"), stringsAsFactors = FALSE),
  read.csv(file.path(out, "deployment_menu_workshop_extension.csv"), stringsAsFactors = FALSE)) %>%
  filter(approach == "national_all") %>%
  distinct(target, budget, .keep_all = TRUE)

make_panels <- function(budget_val, fname, title_pct) {
  f <- fz %>% filter(budget == budget_val) %>%
    transmute(target, list = "state-specific frozen list from national pool",
              precision = precision_deployed,
              `share of error $ caught` = dollar_recall_deployed)
  s <- sy %>% filter(budget == budget_val) %>%
    transmute(target, list = "full national pool applied at once",
              precision, `share of error $ caught` = dollar_recall)
  ord <- f %>% arrange(precision) %>% pull(target)
  dd <- bind_rows(f, s) %>%
    pivot_longer(-c(target, list), names_to = "metric") %>%
    mutate(metric = factor(metric, levels = c("precision",
                                              "share of error $ caught")),
           target = factor(target, levels = ord))

  p <- ggplot(dd, aes(x = value, y = target)) +
    geom_point(aes(shape = list), size = 2.1, stroke = 0.9) +
    scale_shape_manual(values = c("state-specific frozen list from national pool" = 16,
                                  "full national pool applied at once" = 1),
                       breaks = c("state-specific frozen list from national pool",
                                  "full national pool applied at once"),
                       name = NULL) +
    facet_wrap(~metric, nrow = 1, scales = "free_x") +
    labs(x = sprintf("lists sized to a %s review budget; scored on the state's 2024 cases", title_pct),
         y = NULL,
         title = sprintf("Frozen state lists a year ahead, %s sizing", title_pct),
         subtitle = "both evaluated at the same review volume") +
    guides(shape = guide_legend(nrow = 2)) +
    theme_minimal(base_size = 12.5) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position = "top")
  save_png(p, file.path(out, fname), 6.6, 5.8)
}
make_panels(0.10, "frozen_lists_panels_budget10.png", "10%")
make_panels(0.05, "frozen_lists_panels_budget05.png", "5%")
cat("wrote frozen-list panel charts to", out, "\n")
