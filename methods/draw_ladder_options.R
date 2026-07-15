# Three alternative renderings of the evaluation ladder (RevC slide 17),
# same content as evaluation_ladder.png:
#   B  staircase - literal ascending steps, left to right
#   C  pyramid  - evidence-hierarchy tiers, narrowing upward
#   D  two-column rows - minimal, text-forward ("what can fool it" column)
# Outputs: presentation_figures/evaluation_ladder_steps.png, _pyramid.png,
#          _rows.png

suppressMessages(library(ggplot2))
source("rule_mining_helpers.R")

lv <- data.frame(
  n = 1:5,
  name = c("Training fit",
           "Same-era hold-out\n(judged on 2023)",
           "Swapped-year replication\n(2024, pre-registered)",
           "Time-shifted deployment\n(train 22-23 → score 24)",
           "State's own FY25/26\nvalidation"),
  short = c("Training fit",
            "Sandwiched hold-out (2023)",
            "Swapped-year replication (2024)",
            "Time-shifted deployment (22-23 → 24)",
            "State's own FY25/26 validation"),
  note = c("'0.20 precision' delivers 0.10",
            "interpolation, not the forward leap; judge never changes",
            "3 of 4 claims survived; 1 retired",
            "median 0.27-0.30 at review budgets",
            "the only judge seeing the full error population"))
greys <- c("#e9e9e9", "#d9d9d9", "#c9c9c9", "#b9b9b9", "#a9a9a9")

## B: staircase ---------------------------------------------------------------
step_w <- 2.0; step_h <- 0.85
sb <- do.call(rbind, lapply(1:5, function(i) {
  data.frame(xmin = (i - 1) * step_w, xmax = 11,
             ymin = (i - 1) * step_h, ymax = i * step_h, i = i)
}))
p_b <- ggplot() +
  geom_rect(data = sb, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = greys[sb$i], colour = "grey35") +
  geom_text(data = lv, aes(x = (n - 1) * step_w + 0.25,
                           y = (n - 0.5) * step_h, label = short),
            hjust = 0, size = 3.4, fontface = "bold") +
  geom_text(data = lv, aes(x = 10.85, y = (n - 0.5) * step_h, label = note),
            hjust = 1, size = 2.9, colour = "grey25", fontface = "italic") +
  annotate("segment", x = 0.7, xend = 3.4, y = 1.35, yend = 3.75,
           arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
           linewidth = 0.7) +
  annotate("text", x = 1.55, y = 2.95, label = "harder to fool", angle = 42,
           size = 3.6, fontface = "italic") +
  coord_cartesian(xlim = c(0, 11), ylim = c(0, 4.4)) +
  theme_void()
save_png(p_b, "presentation_figures/evaluation_ladder_steps.png", 9.6, 4.2)

## C: pyramid -----------------------------------------------------------------
half <- c(5.0, 4.1, 3.2, 2.3, 1.4)
pc <- do.call(rbind, lapply(1:5, function(i) {
  data.frame(xmin = 5.5 - half[i], xmax = 5.5 + half[i],
             ymin = i - 1 + 0.06, ymax = i - 0.06, i = i)
}))
p_c <- ggplot() +
  geom_rect(data = pc, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = greys[pc$i], colour = "grey35") +
  geom_text(data = lv, aes(x = 5.5, y = n - 0.5, label = short),
            size = 3.4, fontface = "bold") +
  geom_text(data = lv, aes(x = 5.5 + half[n] + 0.25, y = n - 0.5, label = note),
            hjust = 0, size = 2.9, colour = "grey25", fontface = "italic") +
  annotate("segment", x = 0.15, xend = 0.15, y = 0.2, yend = 4.8,
           arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
           linewidth = 0.7) +
  annotate("text", x = -0.12, y = 2.5, label = "harder to fool", angle = 90,
           size = 3.6, fontface = "italic") +
  coord_cartesian(xlim = c(-0.4, 15.4), ylim = c(0, 5)) +
  theme_void()
save_png(p_c, "presentation_figures/evaluation_ladder_pyramid.png", 9.6, 4.2)

## D: two-column rows ---------------------------------------------------------
fool <- c("everything - fit is what the model wants you to believe",
          "2023 sits BETWEEN the train years - interpolation flatters; the judge never changes",
          "very little - expectations were written down before the run",
          "nothing in-sample - the test year influenced no decision",
          "sees what public data cannot: the full error population")
rows <- data.frame(y = 1:5, name = lv$short, fool = fool, note = lv$note)
p_d <- ggplot(rows) +
  geom_text(aes(x = 0, y = y, label = paste0(y, ".  ", name)), hjust = 0,
            size = 3.8, fontface = "bold") +
  geom_text(aes(x = 5.6, y = y, label = fool), hjust = 0, size = 3.1,
            colour = "grey25") +
  geom_segment(aes(x = 0, xend = 13.4, y = y - 0.5, yend = y - 0.5),
               colour = "grey85", linewidth = 0.35) +
  annotate("text", x = 0, y = 5.75, label = "RUNG", size = 3.1,
           fontface = "bold", hjust = 0, colour = "grey40") +
  annotate("text", x = 5.6, y = 5.75, label = "WHAT CAN STILL FOOL IT",
           size = 3.1, fontface = "bold", hjust = 0, colour = "grey40") +
  coord_cartesian(xlim = c(-0.2, 13.6), ylim = c(0.4, 6)) +
  scale_y_reverse() +
  theme_void()
save_png(p_d, "presentation_figures/evaluation_ladder_rows.png", 9.6, 3.9)
cat("wrote three ladder options\n")
