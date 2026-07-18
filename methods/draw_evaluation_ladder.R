# RevC (senior-statistician pass): the evaluation ladder - every number in
# the talk sits on one of these rungs, and each rung is harder to fool than
# the one below. Annotated with the measured example from our own runs.
# Output: presentation_figures/evaluation_ladder.png

suppressMessages(library(ggplot2))
source("rule_mining_helpers.R")

rungs <- data.frame(
  y = 1:5,
  label = c(
    "Training fit",
    "Sandwiched hold-out (2023 sits between train years)",
    "Swapped-year replication (2024, pre-registered)",
    "Time-shifted deployment (train 22-23 → score 24)",
    "The state's own FY25/26 validation"),
  example = c(
    "a '0.20 precision' shortlist... that delivers 0.10",
    "tests interpolation, not the forward leap a state\nfaces, and the judge never changes",
    "3 of 4 selection claims survived; 1 retired",
    "the number a state should actually expect:\nmedian 0.27-0.30 at review budgets",
    "the only judge that sees the full error population\n(public files show 43-91% of error cases)")
)

p <- ggplot(rungs) +
  geom_rect(aes(xmin = 0.6, xmax = 5.4, ymin = y - 0.33, ymax = y + 0.33),
            fill = c("#e8e8e8", "#dcdcdc", "#cfcfcf", "#c2c2c2", "#b5b5b5"),
            colour = "grey40") +
  geom_text(aes(x = 0.78, y = y, label = label), hjust = 0, size = 4.1,
            fontface = "bold") +
  geom_text(aes(x = 5.7, y = y, label = example), hjust = 0, size = 3.3,
            colour = "grey25") +
  annotate("segment", x = 0.25, xend = 0.25, y = 0.75, yend = 5.25,
           arrow = arrow(length = unit(0.28, "cm"), type = "closed"),
           linewidth = 0.8) +
  annotate("text", x = 0.08, y = 3, label = "harder to fool",
           angle = 90, size = 3.8, fontface = "italic") +
  coord_cartesian(xlim = c(0, 10.4), ylim = c(0.5, 5.6)) +
  theme_void()
save_png(p, "presentation_figures/evaluation_ladder.png", 9.6, 4.2)
cat("wrote presentation_figures/evaluation_ladder.png\n")
