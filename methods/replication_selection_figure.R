# Replication figure for the model-selection studies: the same three
# comparisons judged by the year that CHOSE the configuration (test 2023,
# train 2022+2024) and by a year that never judged anything (test 2024,
# train 2022+2023). Orderings that persist are real; orderings that vanish
# were selection luck (the subsample band). Feeds the lessons deck.
# Output: presentation_figures/replication_selection_studies.png

suppressMessages({library(dplyr); library(ggplot2)})
source("rule_mining_helpers.R")

lab23 <- "judged on 2023 (chose the config)"
lab24 <- "verdict on 2024 (never judged anything)"

co23 <- read.csv("methods/compare_engines_v2/combo_summary.csv")
co24 <- read.csv("methods/compare_engines_v2/yearswap_train2223_test24/combo_summary.csv")
eng <- bind_rows(co23 %>% mutate(judge = lab23), co24 %>% mutate(judge = lab24)) %>%
  filter(pool %in% c("rpart", "xgboost", "ranger", "xgboost + ranger")) %>%
  transmute(panel = "engines: recall at the 0.20 floor",
            x = factor(pool, levels = c("rpart", "ranger", "xgboost", "xgboost + ranger"),
                       labels = c("bagged\nCART", "ranger", "xgboost", "xgboost\n+ ranger")),
            value = recall_at_020, judge)

z23 <- read.csv("methods/parameter_tuning_v2/v2_lcbz_summary.csv")
z24 <- read.csv("methods/parameter_tuning_v2/yearswap_train2223_test24/v2_lcbz_summary.csv")
zz <- bind_rows(z23 %>% mutate(judge = lab23), z24 %>% mutate(judge = lab24)) %>%
  filter(config == "nrounds=1000") %>%
  transmute(panel = "filter stringency: precision at the floor",
            x = factor(sub(" .*$", "", z_label),
                       levels = c("z=0.84", "z=1.28", "z=1.64", "z=2.33"),
                       labels = c("80%", "90%", "95%", "99%")),
            value = precision_at_020, judge)

s23 <- read.csv("methods/parameter_tuning_v2/v2_subsample_fine_summary.csv")
s24 <- read.csv("methods/parameter_tuning_v2/yearswap_train2223_test24/v2_subsample_fine_summary.csv")
ss <- bind_rows(s23 %>% mutate(judge = lab23), s24 %>% mutate(judge = lab24)) %>%
  mutate(sub = as.numeric(sub("subsample=", "", setting))) %>%
  transmute(panel = "subsample: precision at the floor (retired)",
            x = factor(sprintf("%.2f", sub)),
            value = precision_at_020, judge)

dd <- bind_rows(eng, zz, ss) %>%
  mutate(panel = factor(panel, levels = unique(panel)))

p <- ggplot(dd, aes(x, value, group = judge, linetype = judge)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.6) +
  facet_wrap(~panel, nrow = 1, scales = "free") +
  labs(x = NULL, y = NULL, linetype = NULL,
       title = "Re-testing the modeling choices on a year that never judged them",
       subtitle = "solid = the replication (train 2022+2023, test 2024); orderings that persist are real - the subsample ordering did not") +
  scale_linetype_manual(values = setNames(c("dashed", "solid"), c(lab23, lab24))) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", aspect.ratio = 1)
save_png(p, "presentation_figures/replication_selection_studies.png", 12, 4.8)
cat("wrote presentation_figures/replication_selection_studies.png\n")
