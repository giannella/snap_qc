# Reconstructed generating script for presentation_figures/mine_big_filter_stringently.png
# (the original was never committed; this rebuilds it from committed data so the figure
# is reproducible and drift-checkable). Data: methods/parameter_tuning_v2/v2_lcbz_sweeps.csv.
#
# The two lines are the same any-error frame mined two ways, each filtered at its own
# appropriate quality bar, then swept across precision floors:
#   - "grow fewer rules, lighter bar"  = 100 rounds, 90% lower-bound (z = 1.28)
#   - "grow many rules, stricter bar"  = 1000 rounds, 99% lower-bound (z = 2.33)
# They trace the same precision / dollar-recall frontier; the big pool just keeps far
# more rules (2,026 vs 789 at the 0.20 floor), which is the state's freedom to veto.
#
#   Rscript methods/draw_mine_big_filter_stringently.R
suppressMessages({library(ggplot2); library(dplyr)})
source("rule_mining_helpers.R")

sw <- read.csv("methods/parameter_tuning_v2/v2_lcbz_sweeps.csv", check.names = FALSE)

lines <- bind_rows(
  sw %>% filter(config == "nrounds=100",  z_label == "z=1.28 (90%)") %>%
    mutate(arm = "grow fewer rules, lighter quality bar"),
  sw %>% filter(config == "nrounds=1000", z_label == "z=2.33 (99%)") %>%
    mutate(arm = "grow many rules, stricter quality bar")
)

# annotation counts at the 0.20 floor, read straight from the data (no magic numbers)
at020 <- lines %>% filter(abs(threshold - 0.20) < 1e-9)
n_small <- at020$n_rules[at020$arm == "grow fewer rules, lighter quality bar"]
n_big   <- at020$n_rules[at020$arm == "grow many rules, stricter quality bar"]
cross_x <- mean(at020$dollar_recall)
cross_y <- mean(at020$precision)

cols <- c("grow fewer rules, lighter quality bar" = "#C0392B",
          "grow many rules, stricter quality bar" = "#2E75B6")

p <- ggplot(lines, aes(dollar_recall, precision, colour = arm)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  annotate("point", x = cross_x, y = cross_y, shape = 21, size = 9,
           stroke = 1.4, colour = "black", fill = NA) +
  annotate("text", x = cross_x + 0.02, y = cross_y + 0.02, hjust = 0, size = 4.4,
           label = sprintf("same performance here, but\n%s rules to choose from vs %s",
                           format(n_big, big.mark = ","), format(n_small, big.mark = ","))) +
  scale_colour_manual(values = cols, name = NULL) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Growing many rules costs nothing in accuracy, and gives states more choices",
    subtitle = paste("The two approaches perform the same on new data. The advantage of growing many",
                     "rules is the menu:\nseveral times more rules meet any quality bar, so a state can",
                     "drop rules its experts distrust\nand still cover the same errors with alternatives."),
    x = "Share of error dollars caught (on a year of new data)",
    y = "Share of flagged cases that have an error") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top", panel.grid.minor = element_blank())

save_png(p, "presentation_figures/mine_big_filter_stringently.png", 10.8, 6.2)
cat(sprintf("wrote mine_big_filter_stringently.png (big=%s vs small=%s rules)\n", n_big, n_small))
