# Generating script for presentation_figures/states_two_regimes.png.
# Three frozen lists per state, all 49 states - the state's own pool, the
# national pool, and the blend - from the three-arm evaluation of the
# v2.5.0 recipe (methods/threearm_lists_2024_v2.R): pools mined FY2022-23
# (seed 117, corrected frame, 19-feature vocabulary, artifact gates),
# lists frozen on the state's FY2022-23 caseload with the shipped
# fresh-share walk, walked on the state's FY2024 cases. Design: option B
# containment ring (2026-08-14 review) - components are small filled
# dots, the blended list is a larger open ring, so when the blend adopts
# a component the ring encircles that dot instead of stacking on it.
# Ticks are the published FNS FY2024 payment error rates
# (additional_data/fns_payment_error_rates_fy2024.csv), not the QC-frame
# base rates. South Dakota and Wyoming have no own-pool list (their
# FY2022-23 state pools admit no rules); they show national/blended only.
# Supersedes the 18-state version drawn from the July (pre-v2.5.0) study
# artifacts in methods/state_similarity_v2/.
#
#   Rscript methods/draw_states_two_regimes.R
suppressMessages({library(ggplot2); library(dplyr)})
source("rule_mining_helpers.R")

BUDGET <- 0.05
ta <- read.csv("methods/threearm_2024/threearm_results_2024.csv") %>%
  filter(budget == BUDGET) %>%
  select(state, arm, precision) %>%
  tidyr::pivot_wider(names_from = arm, values_from = precision,
                     names_prefix = "p_")
per <- read.csv("additional_data/fns_payment_error_rates_fy2024.csv") %>%
  transmute(state, fy24_rate = payment_error_rate / 100)

w <- ta %>% inner_join(per, by = "state") %>%
  arrange(state) %>%
  mutate(y = n():1)   # alphabetical, A at top

LEV <- c("blended", "national", "state", "FY24 error rate")

seg <- w %>%
  mutate(lo = pmin(p_state, pmin(p_national, p_blended), na.rm = TRUE),
         hi = pmax(p_state, pmax(p_national, p_blended), na.rm = TRUE))

p <- ggplot(w, aes(y = y)) +
  geom_segment(data = seg, aes(x = lo, xend = hi, yend = y),
               colour = "grey88", linewidth = 0.5) +
  geom_point(aes(x = fy24_rate, shape = LEV[4], colour = LEV[4], size = LEV[4])) +
  geom_point(data = w %>% filter(!is.na(p_state)),
             aes(x = p_state, shape = LEV[3], colour = LEV[3], size = LEV[3])) +
  geom_point(aes(x = p_national, shape = LEV[2], colour = LEV[2], size = LEV[2])) +
  geom_point(aes(x = p_blended, shape = LEV[1], colour = LEV[1], size = LEV[1]),
             fill = NA, stroke = 1.3) +
  scale_shape_manual(NULL, values = setNames(c(21, 16, 16, 124), LEV), breaks = LEV) +
  scale_colour_manual(NULL, values = setNames(c("black", "#34568B", "grey55",
                                                "grey60"), LEV), breaks = LEV) +
  scale_size_manual(NULL, values = setNames(c(4.2, 2.4, 2.4, 3.4), LEV), breaks = LEV) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(breaks = w$y, labels = w$state) +
  labs(title = "Including national rules generally improves performance",
       x = NULL, y = NULL,
       caption = paste0(
         "Precision at a ", scales::percent(BUDGET), " review budget on the state's 2024 cases; all lists frozen on the state's\n",
         "2022-23 caseload at equal review volume. FY24 error rate = the published FNS payment error\n",
         "rate (dollar-weighted, includes cases the public QC file omits).")) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.caption = element_text(hjust = 0, size = 9.5, colour = "grey30"))

save_png(p, "presentation_figures/states_two_regimes.png", 8.0, 12.5)
cat(sprintf("wrote states_two_regimes.png (%d states, %s budget)\n",
            nrow(w), scales::percent(BUDGET)))
