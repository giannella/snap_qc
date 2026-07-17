# Plots for the pre-registered era validation (train 2017-18, test 2019).
# Reads era_validation_results.csv (long format) and writes one figure per
# comparison plus a trade-off scatter. Style follows the repo's dot-plot
# convention: one dot per state, medians as heavy marks, Okabe-Ito accents.
# Output: methods/state_similarity_v2/era_validation_train1718_test19/*.png

suppressMessages({library(dplyr); library(ggplot2)})

ERA_DIR <- "methods/state_similarity_v2/era_validation_train1718_test19"
d <- read.csv(file.path(ERA_DIR, "era_validation_results.csv"),
              stringsAsFactors = FALSE)
d$budget_lbl <- sprintf("%.0f%% review budget", 100 * d$budget)

BLUE <- "#0072B2"; ORANGE <- "#E69F00"; MUTED <- "#6b7280"

dotplot <- function(dd, x, xlab, title, file, order_by_median = TRUE) {
  dd$val <- dd[[x]]
  med <- dd %>% group_by(ordering, budget_lbl) %>%
    summarise(m = median(val, na.rm = TRUE), .groups = "drop")
  if (order_by_median) {
    lv <- med %>% filter(grepl("10", budget_lbl)) %>% arrange(m) %>% pull(ordering)
    dd$ordering <- factor(dd$ordering, levels = lv)
    med$ordering <- factor(med$ordering, levels = lv)
  }
  p <- ggplot(dd, aes(val, ordering)) +
    geom_point(colour = BLUE, alpha = 0.35, size = 1.8) +
    geom_point(data = med, aes(m, ordering), colour = ORANGE, size = 3.4) +
    facet_wrap(~budget_lbl, nrow = 1, scales = "free_x") +
    labs(x = xlab, y = NULL, title = title,
         subtitle = "one dot per state (18); orange = median") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
  ggsave(file.path(ERA_DIR, file), p, width = 10, height = 4.2, dpi = 300,
         bg = "white")
  cat("wrote", file, "\n")
}

## ordering comparison: precision
dotplot(d %>% filter(comparison == "ordering"),
        "precision", "any-error precision on 2019",
        "Ordering statistics, tested on an era they never saw (train 2017-18, test 2019)",
        "era_ordering_precision.png")

## admission comparison
adm <- d %>% filter(comparison == "admission") %>%
  mutate(ordering = admission)
dotplot(adm, "precision", "any-error precision on 2019",
        "Admission gates: production filter vs FDR + support floor",
        "era_admission_precision.png", order_by_median = FALSE)

## dollar comparison: dollar recall
dotplot(d %>% filter(comparison == "dollar"),
        "dollar_recall", "share of 2019 error dollars caught",
        "Dollar-goal orderings, judged on error dollars at capacity",
        "era_dollar_recall.png")

## xfit validity check
if (any(d$comparison == "xfit")) {
  dotplot(d %>% filter(comparison == "xfit"),
          "precision", "any-error precision on 2019 (national-only lists)",
          "Cross-fit (selection-free) ordering vs full-mine ordering",
          "era_xfit_precision.png", order_by_median = FALSE)
}

## trade-off scatter: median precision vs median dollars per arm
sm <- d %>% filter(comparison %in% c("ordering", "dollar")) %>%
  group_by(comparison, ordering, budget_lbl) %>%
  summarise(prec = median(precision, na.rm = TRUE),
            dollars = median(dollar_recall, na.rm = TRUE), .groups = "drop")
p <- ggplot(sm, aes(dollars, prec)) +
  geom_point(aes(shape = comparison), colour = BLUE, size = 2.6) +
  geom_text(aes(label = ordering), size = 2.7, vjust = -0.9, colour = MUTED) +
  facet_wrap(~budget_lbl, nrow = 1, scales = "free") +
  labs(x = "median share of 2019 error dollars caught",
       y = "median any-error precision on 2019",
       title = "The precision-dollars trade-off by ordering statistic",
       shape = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), legend.position = "top")
ggsave(file.path(ERA_DIR, "era_tradeoff_scatter.png"), p,
       width = 10, height = 4.6, dpi = 300, bg = "white")
cat("wrote era_tradeoff_scatter.png\n")
