# Visualize the budget-constrained benchmark: honest national (leave-one-
# state-out pool) vs NB-similarity 5-neighbor pool, 12 states, 5% and 10%
# review budgets. Two variants: diagonal scatter (45-degree line = no
# difference) and dumbbell (per-state gaps, sorted).
# Reads methods/state_similarity_v2/transfer_benchmark/budgeted_menu_results.csv.

suppressMessages({library(dplyr); library(ggplot2); library(tidyr)})
source("rule_mining_helpers.R")

dat <- read.csv("methods/state_similarity_v2/transfer_benchmark/budgeted_menu_results.csv",
                stringsAsFactors = FALSE) %>%
  filter(approach %in% c("national_loo", "transfer_nb")) %>%
  mutate(budget_lab = factor(sprintf("review budget: %.0f%% of caseload", 100 * budget),
                             levels = sprintf("review budget: %.0f%% of caseload", c(5, 10))),
         approach_lab = ifelse(approach == "national_loo",
                               "all 48 other states", "5 most similar states (NB)"))

long <- dat %>%
  select(target, budget_lab, approach_lab, precision, dollar_recall) %>%
  pivot_longer(c(precision, dollar_recall), names_to = "metric") %>%
  mutate(metric = ifelse(metric == "precision", "precision",
                         "share of error $ caught"))

## ── variant 1: diagonal scatter ───────────────────────────────────────────────
wide <- long %>% pivot_wider(names_from = approach_lab, values_from = value)
names(wide)[names(wide) == "all 48 other states"] <- "loo"
names(wide)[names(wide) == "5 most similar states (NB)"] <- "nb"

p1 <- ggplot(wide, aes(loo, nb)) +
  geom_abline(slope = 1, intercept = 0, linewidth = 0.3, colour = "grey60") +
  geom_point(size = 1.6) +
  geom_text(aes(label = target), size = 2.6, vjust = -0.8, check_overlap = TRUE) +
  geom_blank(aes(x = nb, y = loo)) +   # per-panel: force equal x/y ranges so
  facet_grid(metric ~ budget_lab, scales = "free") +  # the 45-degree line is honest
  labs(x = "rules mined on ALL 48 other states",
       y = "rules mined on the 5 MOST SIMILAR states",
       title = "Five similar states vs forty-eight others - neither pool ever saw the target state",
       subtitle = "each point is one target state, scored on its own 2022-24 cases; above the grey line = similarity pool wins") +
  theme_minimal(base_size = 12) + theme(aspect.ratio = 1)
save_png(p1, "methods/state_similarity_v2/transfer_benchmark/loo_vs_nb_scatter.png", 9, 8.5)

## ── variant 2: dumbbell, sorted by precision gap at each budget ───────────────
prec <- dat %>%
  select(target, budget_lab, approach_lab, precision) %>%
  pivot_wider(names_from = approach_lab, values_from = precision)
names(prec)[names(prec) == "all 48 other states"] <- "loo"
names(prec)[names(prec) == "5 most similar states (NB)"] <- "nb"
prec <- prec %>%
  mutate(winner = ifelse(nb >= loo, "similarity pool wins", "48-state pool wins"))

pd <- prec %>%
  group_by(budget_lab) %>%
  mutate(target_o = reorder(paste(target, budget_lab), nb - loo)) %>%
  ungroup()
p2 <- ggplot(pd, aes(y = target_o)) +
  geom_segment(aes(x = loo, xend = nb, yend = target_o, colour = winner),
               linewidth = 1.1,
               arrow = arrow(length = unit(4, "pt"), type = "closed")) +
  geom_point(aes(x = loo), size = 1.8) +
  facet_wrap(~budget_lab, scales = "free_y") +
  scale_y_discrete(labels = function(x) sub(" review budget.*$", "", x)) +
  scale_colour_manual(values = c("similarity pool wins" = "black",
                                 "48-state pool wins" = "grey55")) +
  labs(x = "delivered precision on the target state (dot = 48-state pool, arrow tip = 5-state similarity pool)",
       y = NULL, colour = NULL,
       title = "Where five similar states beat forty-eight - delivered precision under a review budget",
       subtitle = "neither pool ever saw the target state; states sorted by the gap") +
  theme_minimal(base_size = 12) + theme(legend.position = "top")
save_png(p2, "methods/state_similarity_v2/transfer_benchmark/loo_vs_nb_dumbbell.png", 10, 5.5)

## ── variant 3: metric space (precision vs $ recall), pair per state ───────────
ms <- dat %>% select(target, budget_lab, approach_lab, precision, dollar_recall)
seg <- ms %>%
  pivot_wider(names_from = approach_lab,
              values_from = c(precision, dollar_recall))
names(seg) <- c("target", "budget_lab", "prec_loo", "prec_nb", "dr_loo", "dr_nb")

p3 <- ggplot() +
  geom_segment(data = seg, aes(x = dr_loo, y = prec_loo,
                               xend = dr_nb, yend = prec_nb),
               linewidth = 0.4, colour = "grey65") +
  geom_point(data = ms, aes(dollar_recall, precision, shape = approach_lab),
             size = 2.0) +
  geom_text(data = seg, aes((dr_loo + dr_nb) / 2, (prec_loo + prec_nb) / 2,
                            label = target),
            size = 2.6, vjust = -0.9, check_overlap = TRUE) +
  scale_shape_manual(values = c("all 48 other states" = 1,
                                "5 most similar states (NB)" = 16)) +
  facet_wrap(~budget_lab) +
  labs(x = "share of the state's error $ caught",
       y = "precision (share of flagged cases with an error)",
       shape = NULL,
       title = "What out-of-state rules deliver under a review budget - 12 states, two donor pools",
       subtitle = "neither pool ever saw the target state; state base error rates run 9-15%, so precision 0.25-0.35 is a 2-3x lift") +
  coord_cartesian(ylim = c(0, NA), xlim = c(0, NA)) +
  theme_minimal(base_size = 12) + theme(legend.position = "top", aspect.ratio = 1)
save_png(p3, "methods/state_similarity_v2/transfer_benchmark/loo_vs_nb_metricspace.png", 10, 5.8)

## ── variant 4: best approach per state, one PR panel per budget ───────────────
best <- dat %>%
  group_by(target, budget_lab) %>%
  slice_max(precision, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(approach_lab = paste("best here:", approach_lab))

p4 <- ggplot(best, aes(dollar_recall, precision, shape = approach_lab)) +
  geom_point(size = 2.4) +
  geom_text(aes(label = target), size = 2.7, vjust = -0.9, check_overlap = TRUE) +
  scale_shape_manual(values = c("best here: all 48 other states" = 1,
                                "best here: 5 most similar states (NB)" = 16)) +
  facet_wrap(~budget_lab) +
  labs(x = "share of the state's error $ caught",
       y = "precision (share of flagged cases with an error)",
       shape = NULL,
       title = "Out-of-state rules under a review budget - the better donor pool for each state",
       subtitle = "best = higher precision between the two pools; neither pool ever saw the target state; base error rates 9-15%") +
  coord_cartesian(ylim = c(0, max(best$precision) * 1.12),
                  xlim = c(0, max(best$dollar_recall) * 1.12)) +
  theme_minimal(base_size = 12) + theme(legend.position = "top", aspect.ratio = 1)
save_png(p4, "methods/state_similarity_v2/transfer_benchmark/best_pool_pr_by_budget.png", 10, 5.8)

cat("wrote loo_vs_nb_scatter.png, loo_vs_nb_dumbbell.png, loo_vs_nb_metricspace.png, best_pool_pr_by_budget.png\n")
