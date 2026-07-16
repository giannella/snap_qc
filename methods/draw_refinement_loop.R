# Companion figure to the pipeline diagram (draw_pipeline_options.R, option B):
# how a proposed change qualifies for the recommended workflow. Same visual
# language. Examples on the two exits are real cases from modeling_findings.md.
# Output: presentation_figures/refinement_loop.png

suppressMessages(library(ggplot2))
dir.create("presentation_figures", showWarnings = FALSE)

INK    <- "#1a1a2e"
MUTED  <- "#6b7280"
BOX    <- "#e5e7eb"
EDGE   <- "#9ca3af"
BLUE   <- "#0072B2"

arrowspec <- arrow(length = unit(7, "pt"), type = "closed")
box <- function(x, y, w, h, fill, colour = NA)
  annotate("rect", xmin = x - w/2, xmax = x + w/2, ymin = y - h/2, ymax = y + h/2,
           fill = fill, colour = colour, linewidth = 0.4)
lbl <- function(x, y, text, size = 3.2, col = INK, face = "plain", lh = 0.95)
  annotate("text", x = x, y = y, label = text, size = size, colour = col,
           fontface = face, lineheight = lh)
seg <- function(x1, y1, x2, y2, col = EDGE)
  annotate("segment", x = x1, y = y1, xend = x2, yend = y2,
           colour = col, linewidth = 0.6, arrow = arrowspec)

p <- ggplot() +
  # 1 proposal
  box(1.2, 2.0, 1.9, 1.4, fill = scales::alpha(BLUE, 0.14), colour = BLUE) +
  lbl(1.2, 2.35, "proposed improvement", 3.4, INK, "bold") +
  lbl(1.2, 1.90, "a new engine, rule\nvocabulary, or selection\nstatistic", 2.7, MUTED) +
  seg(2.2, 2.0, 2.62, 2.0) +
  # 2 exploratory comparison
  box(3.85, 2.0, 2.2, 1.4, fill = BOX, colour = EDGE) +
  lbl(3.85, 2.35, "exploratory comparison", 3.4, INK, "bold") +
  lbl(3.85, 1.86, "head-to-head against the\ncurrent recipe: same states,\nbudgets, and protocol\n(train 2022-23, test 2024)", 2.7, MUTED) +
  seg(5.0, 2.0, 5.42, 2.0) +
  # 3 pre-registered validation
  box(6.7, 2.0, 2.3, 1.4, fill = BOX, colour = EDGE) +
  lbl(6.7, 2.35, "pre-registered validation", 3.4, INK, "bold") +
  lbl(6.7, 1.86, "expected outcome written down\nfirst; tested on data that never\njudged the idea (held-out year,\nseparate era)", 2.7, MUTED) +
  # exits
  seg(7.9, 2.35, 8.42, 2.85) +
  seg(7.9, 1.65, 8.42, 1.15) +
  box(9.7, 3.0, 2.4, 1.15, fill = scales::alpha(BLUE, 0.14), colour = BLUE) +
  lbl(9.7, 3.28, "adopted", 3.4, INK, "bold") +
  lbl(9.7, 2.88, "enters the recommended workflow;\nchangelog entry, release tag,\ndelivery lists rebuilt", 2.7, MUTED) +
  lbl(9.7, 2.28, "e.g., confidence-bound selection (v2);\nthe blended state + national list", 2.5, BLUE) +
  box(9.7, 1.0, 2.4, 1.15, fill = "white", colour = INK) +
  lbl(9.7, 1.28, "retired, in writing", 3.4, INK, "bold") +
  lbl(9.7, 0.88, "the claim and its numbers recorded\nin methods/modeling_findings.md", 2.7, MUTED) +
  lbl(9.7, 0.28, "e.g., \"low subsampling beats high\" (2026-07);\nper-state threshold tuning as the default (2026-07)", 2.5, MUTED) +
  lbl(6.0, 3.85, "How a change earns its way into the recommended workflow", 5, INK, "bold") +
  coord_cartesian(xlim = c(0.1, 11.1), ylim = c(-0.1, 4.1)) +
  theme_void()
ggsave("presentation_figures/refinement_loop.png", p,
       width = 12, height = 4.6, dpi = 300, bg = "white")
cat("wrote presentation_figures/refinement_loop.png\n")
