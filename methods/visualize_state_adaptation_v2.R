# Charts for the state-adaptation benchmark (deployment_state_adaptation_
# v2.R): national as-is vs filtered vs tuned vs hybrid, per state on the
# 2024 holdout, at each budget. Previously table-only; charted so every
# results CSV has a figure. States alphabetical (2026-07-12 convention).
#
# Output: adaptation_arms_budget05/10.png (same folder as the CSV)

suppressMessages({library(dplyr); library(ggplot2); library(tidyr)})
source("rule_mining_helpers.R")
out <- "methods/state_similarity_v2/transfer_benchmark_train2223_test24"

d <- read.csv(file.path(out, "deployment_state_adaptation.csv"),
              stringsAsFactors = FALSE)
LEVELS <- c("national as-is", "filtered", "tuned", "hybrid")
d$arm <- factor(d$approach,
                levels = c("national_asis", "filtered", "tuned", "hybrid"),
                labels = LEVELS)

make_chart <- function(budget_val, fname, title_pct) {
  dd <- d %>% filter(budget == budget_val)
  ord <- rev(sort(unique(dd$target)))
  ddl <- dd %>%
    select(target, arm, precision, `share of error $ caught` = dollar_recall) %>%
    pivot_longer(c(precision, `share of error $ caught`), names_to = "metric") %>%
    mutate(metric = factor(metric, levels = c("precision", "share of error $ caught")),
           target = factor(target, levels = ord))

  p <- ggplot(ddl, aes(x = value, y = target)) +
    geom_line(aes(group = target), colour = "grey85", linewidth = 0.4) +
    geom_point(aes(shape = arm, colour = arm), size = 2.0, stroke = 0.8, na.rm = TRUE) +
    scale_shape_manual(values = setNames(c(16, 1, 2, 0), LEVELS), name = NULL) +
    scale_colour_manual(values = setNames(c("black", "grey35", "grey35", "grey35"), LEVELS),
                        name = NULL) +
    facet_wrap(~metric, nrow = 1, scales = "free_x") +
    labs(x = sprintf("at a %s review budget, on the state's 2024 cases", title_pct),
         y = NULL,
         title = sprintf("Adapting national rules to a state: four schemes, %s budget", title_pct),
         subtitle = "filled = national list applied as-is; open shapes = state-adapted variants\n(filtered / threshold-tuned / hybrid); identical review volume") +
    guides(shape = guide_legend(nrow = 1)) +
    theme_minimal(base_size = 12.5) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position = "top")
  save_png(p, file.path(out, fname), 6.6, 5.8)
}
make_chart(0.10, "adaptation_arms_budget10.png", "10%")
make_chart(0.05, "adaptation_arms_budget05.png", "5%")
cat("wrote adaptation-arms charts to", out, "\n")
