# Focused figure for the constrained-random-forest point (deck comment
# 2026-07-12): the ranger mtry sweep alone, from the saved one-at-a-time
# tuning sweeps. mtry = 2 (constrained but not fully random) traces the best
# frontier; mtry = 1 (fully random splits) and mtry = 4 (less constrained)
# sit below. No mining - reads v2_tuning_sweeps.csv.
# Output: methods/parameter_tuning_v2/mtry_frontier.png

suppressMessages({library(dplyr); library(ggplot2)})
source("rule_mining_helpers.R")
sw <- read.csv("methods/parameter_tuning_v2/v2_tuning_sweeps.csv",
               stringsAsFactors = FALSE) %>%
  filter(engine == "ranger", param == "mtry")

p <- ggplot(sw, aes(x, precision, colour = setting)) +
  geom_line(linewidth = 0.9) + geom_point(size = 1.1) +
  scale_x_continuous(labels = scales::percent, limits = c(0, 0.6)) +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_manual(values = c("mtry=1 (base)" = "#8c8c8c", "mtry=2" = "#d1495b",
                                 "mtry=4" = "#0073b7"),
                      labels = c("mtry=1 (base)" = "mtry=1")) +
  labs(x = "Hold-out dollar recall of the union",
       y = "Hold-out precision of the union", colour = NULL,
       title = "How constrained should the random forest be?",
       subtitle = "mtry = variables the forest may consider per split; 2 beats\nfully random (1) and looser (4). Trained 2022/2024, scored 2023.") +
  theme_minimal(base_size = 12) + theme(legend.position = "top")
save_png(p, "methods/parameter_tuning_v2/mtry_frontier.png", 6.6, 4.6)
cat("wrote methods/parameter_tuning_v2/mtry_frontier.png\n")
