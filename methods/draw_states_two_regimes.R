# Generating script for presentation_figures/states_two_regimes.png.
# Three frozen lists per state - the state's own pool, the national pool,
# and the blend (99% bound) - all built on the state's 2022-23 caseload and
# walked to identical review volume on its 2024 cases (findings sections
# 15-16). Neutral presentation: no verdict coloring, alphabetical states,
# one condensed caption.
#
# Data: methods/state_similarity_v2/transfer_benchmark_train2223_test24/
#       frozen_list_results.csv, frozen_own_list_results.csv,
#       blended_frozen_results.csv  (committed)
#
#   Rscript methods/draw_states_two_regimes.R
suppressMessages({library(ggplot2); library(dplyr)})
source("rule_mining_helpers.R")

BUDGET <- 0.10
src <- "methods/state_similarity_v2/transfer_benchmark_train2223_test24"

natl <- read.csv(file.path(src, "frozen_list_results.csv")) %>%
  transmute(target, budget, list = "national", precision = precision_deployed,
            target_base_rate)
own <- read.csv(file.path(src, "frozen_own_list_results.csv")) %>%
  transmute(target, budget, list = "state's own", precision = precision_deployed)
bl <- read.csv(file.path(src, "blended_frozen_results.csv")) %>%
  filter(variant == "lcb99") %>%
  transmute(target, budget, list = "blended", precision)

d <- bind_rows(bl, natl %>% select(-target_base_rate), own) %>%
  filter(budget == BUDGET) %>%
  mutate(target = factor(target, levels = rev(sort(unique(target)))),
         list = factor(list, levels = c("blended", "national", "state's own")))
base <- natl %>% filter(budget == BUDGET) %>%
  mutate(target = factor(target, levels = levels(d$target)))

p <- ggplot(d, aes(x = precision, y = target)) +
  geom_line(aes(group = target), colour = "grey80", linewidth = 0.5) +
  geom_point(data = base, aes(x = target_base_rate), shape = 124, size = 4.5,
             colour = "grey55") +
  geom_point(aes(shape = list, colour = list), size = 3.2, stroke = 1.0) +
  scale_shape_manual(values = c("blended" = 16, "national" = 17, "state's own" = 1),
                     name = NULL) +
  scale_colour_manual(values = c("blended" = "black", "national" = "#34568B",
                                 "state's own" = "grey35"), name = NULL) +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "State, national, and blended rule lists, tested on 2024",
       x = NULL, y = NULL,
       caption = paste0("Precision at a ", scales::percent(BUDGET), " review budget on the state's 2024 cases; ",
                        "all three lists frozen on the state's 2022-23\ncaseload and walked to identical review volume. ",
                        "Grey tick = the state's 2024 base error rate.")) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top", panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.caption = element_text(hjust = 0, size = 10.5, colour = "grey30"))

save_png(p, "presentation_figures/states_two_regimes.png", 8.6, 6.4)
cat(sprintf("wrote states_two_regimes.png (%d states, %s budget)\n",
            length(unique(d$target)), scales::percent(BUDGET)))
