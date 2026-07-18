# Reconstructed generating script for presentation_figures/states_two_regimes.png.
# The original was never committed AND its only committed data source is a superseded
# per-state tuning run whose story contradicts the figure. This rebuilds the same
# message ("using your own state's data is boom-or-bust") from the CURRENT, non-
# superseded deployment benchmark (findings section 14): rules mined on 2022-23,
# scored on each state's 2024 cases, filled to a review budget.
#
# Data: methods/state_similarity_v2/transfer_benchmark_train2223_test24/
#       deployment_menu_train2223_test24.csv  (committed)
#
#   Rscript methods/draw_states_two_regimes.R
suppressMessages({library(ggplot2); library(dplyr)})
source("rule_mining_helpers.R")

BUDGET <- 0.10
d <- read.csv("methods/state_similarity_v2/transfer_benchmark_train2223_test24/deployment_menu_train2223_test24.csv",
              check.names = FALSE) %>%
  filter(budget == BUDGET, approach %in% c("own_state", "national_all"))

own  <- d %>% filter(approach == "own_state") %>%
  select(target, own_state = precision, target_base_rate)
natl <- d %>% filter(approach == "national_all") %>%
  select(target, national_all = precision)
wide <- merge(own, natl, by = "target") %>%
  mutate(state = target,
         verdict = case_when(
           own_state < target_base_rate ~ "own-state FAILS (below random review)",
           own_state > national_all     ~ "own-state beats national",
           TRUE                         ~ "own-state trails national")) %>%
  arrange(own_state - national_all) %>%
  mutate(state = factor(state, levels = state))

cols <- c("own-state beats national" = "#2E7D32",
          "own-state trails national" = "#8A8A8A",
          "own-state FAILS (below random review)" = "#C0392B")

p <- ggplot(wide) +
  geom_segment(aes(y = state, yend = state, x = national_all, xend = own_state),
               colour = "grey70", linewidth = 0.9) +
  geom_point(aes(y = state, x = target_base_rate), shape = 124, size = 5,
             colour = "grey45") +
  geom_point(aes(y = state, x = national_all), size = 3.4, colour = "#34568B") +
  geom_point(aes(y = state, x = own_state, colour = verdict), size = 3.8) +
  scale_colour_manual(values = cols, name = NULL) +
  scale_x_continuous(labels = scales::percent) +
  labs(
    title = "Mining on your own state's data is boom-or-bust; the national list is the safe default",
    subtitle = paste0("Precision on the state's 2024 cases at a ", scales::percent(BUDGET),
                      " review budget (rules mined on 2022-23).\n",
                      "Blue = national list; coloured dot = own-state rules; grey tick = the state's ",
                      "base error rate.\nOwn-state has the biggest wins and the worst failures ",
                      "(below the base rate = worse than random review)."),
    x = "Share of flagged cases that have an error (2024)", y = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top", panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())

save_png(p, "presentation_figures/states_two_regimes.png", 11.0, 6.6)
cat(sprintf("wrote states_two_regimes.png (%d states, %s budget)\n", nrow(wide), scales::percent(BUDGET)))
