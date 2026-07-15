# Teaching diagram (deck comment 2026-07-12): how the two engines complement
# each other - xgboost's sequential boosting sharpens the core error
# patterns; a constrained random forest (few variables per split) spreads
# many independent trees across the periphery. Schematic, not data.
# Output: presentation_figures/engines_concept_diagram.png

suppressMessages(library(ggplot2))
source("rule_mining_helpers.R")

tree <- function(cx, cy, h = 1.6, w = 1.15, fill = "#88a8c8") {
  data.frame(x = c(cx - w / 2, cx + w / 2, cx), y = c(cy, cy, cy + h),
             g = paste(cx, cy), fill = fill)
}
trees <- rbind(
  # left panel: boosting chain
  tree(2, 5.4), tree(5, 5.4), tree(8, 5.4),
  # right panel: forest, parallel
  tree(12.4, 5.4, fill = "#7fbf8e"), tree(14.6, 5.4, fill = "#7fbf8e"),
  tree(16.8, 5.4, fill = "#7fbf8e"), tree(19.0, 5.4, fill = "#7fbf8e"))
trunks <- data.frame(x = c(2, 5, 8, 12.4, 14.6, 16.8, 19.0))

p <- ggplot() +
  # panel titles
  annotate("text", x = 5, y = 9.4, size = 5.2, fontface = "bold",
           label = "xgboost: boosting") +
  annotate("text", x = 15.7, y = 9.4, size = 5.2, fontface = "bold",
           label = "constrained random forest") +
  annotate("text", x = 5, y = 8.6, size = 3.6, colour = "grey30",
           label = "trees built in sequence,\neach one fits what the last missed") +
  annotate("text", x = 15.7, y = 8.6, size = 3.6, colour = "grey30",
           label = "many independent trees; each split may\nuse only 2 randomly chosen variables") +
  # trees
  geom_polygon(data = trees, aes(x, y, group = g, fill = fill), colour = "grey25") +
  scale_fill_identity() +
  geom_rect(data = trunks, aes(xmin = x - 0.12, xmax = x + 0.12,
                               ymin = 4.7, ymax = 5.4),
            fill = "#8a6d4a", colour = NA) +
  # boosting arrows + labels
  annotate("segment", x = 2.8, xend = 4.2, y = 6.2, yend = 6.2,
           arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  annotate("segment", x = 5.8, xend = 7.2, y = 6.2, yend = 6.2,
           arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  annotate("text", x = 3.5, y = 6.65, size = 3.0, colour = "grey35",
           label = "remaining\nerrors") +
  annotate("text", x = 6.5, y = 6.65, size = 3.0, colour = "grey35",
           label = "remaining\nerrors") +
  annotate("text", x = c(2, 5, 8), y = 4.3, size = 3.1, colour = "grey35",
           label = c("tree 1", "tree 2", "tree 3")) +
  # forest randomness labels
  annotate("text", x = c(12.4, 14.6, 16.8, 19.0), y = 4.3, size = 3.1,
           colour = "grey35",
           label = c("cases 1st draw\nvars {A,F}", "2nd draw\nvars {C,D}",
                     "3rd draw\nvars {B,E}", "4th draw\nvars {A,D}")) +
  # takeaway band
  annotate("text", x = 5, y = 2.6, size = 3.7,
           label = "sharpens rules for the CORE,\nmost common error patterns") +
  annotate("text", x = 15.7, y = 2.6, size = 3.7,
           label = "forced variety reaches the PERIPHERY -\nless common circumstances") +
  annotate("segment", x = 10.35, xend = 10.35, y = 1.6, yend = 9.8,
           colour = "grey70", linetype = "dashed") +
  annotate("text", x = 10.35, y = 0.9, size = 3.9, fontface = "italic",
           label = "complementary: mine rules from both, pool them, filter on the lower bound") +
  coord_equal(xlim = c(0, 21), ylim = c(0.3, 10)) +
  theme_void()
save_png(p, "presentation_figures/engines_concept_diagram.png", 9.2, 4.6)
cat("wrote presentation_figures/engines_concept_diagram.png\n")
