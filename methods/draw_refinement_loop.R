# Companion figure to the pipeline diagram: how a proposed change qualifies
# for the recommended workflow, closed as a loop (the outcome, adopted or
# retired, is recorded under the versioning policy and informs the next
# proposal). Grid-aligned boxes, orthogonal connectors.
# Output: presentation_figures/refinement_loop.png

suppressMessages(library(ggplot2))
dir.create("presentation_figures", showWarnings = FALSE)

INK    <- "#1a1a2e"
MUTED  <- "#6b7280"
BOX    <- "#e5e7eb"
EDGE   <- "#9ca3af"
BLUE   <- "#0072B2"

arrowspec <- arrow(length = unit(5.5, "pt"), type = "closed")
box <- function(x, y, w, h, fill, colour = NA)
  annotate("rect", xmin = x - w/2, xmax = x + w/2, ymin = y - h/2, ymax = y + h/2,
           fill = fill, colour = colour, linewidth = 0.4)
lbl <- function(x, y, text, size = 3.2, col = INK, face = "plain", lh = 0.95)
  annotate("text", x = x, y = y, label = text, size = size, colour = col,
           fontface = face, lineheight = lh)
seg <- function(x1, y1, x2, y2, col = EDGE, lt = "solid", ah = TRUE, lw = 0.6) {
  # passing arrow = NULL into annotate() silently drops the layer; only
  # include the argument when an arrowhead is wanted
  if (ah)
    annotate("segment", x = x1, y = y1, xend = x2, yend = y2,
             colour = col, linewidth = lw, linetype = lt, arrow = arrowspec)
  else
    annotate("segment", x = x1, y = y1, xend = x2, yend = y2,
             colour = col, linewidth = lw, linetype = lt)
}

p <- ggplot() +
  ## spine row (centers y = 2.6; heights 1.1)
  box(1.3, 2.6, 1.9, 1.1, fill = scales::alpha(BLUE, 0.14), colour = BLUE) +
  lbl(1.3, 2.82, "proposed improvement", 3.2, INK, "bold") +
  lbl(1.3, 2.46, "a new engine, vocabulary,\nor selection statistic", 2.5, MUTED) +
  seg(2.25, 2.6, 2.53, 2.6) +
  box(3.55, 2.6, 2.0, 1.1, fill = BOX, colour = EDGE) +
  lbl(3.55, 2.82, "exploratory comparison", 3.2, INK, "bold") +
  lbl(3.55, 2.46, "head-to-head vs the current\nrecipe on the 2024 benchmark", 2.5, MUTED) +
  seg(4.55, 2.6, 4.88, 2.6, lw = 0.45) +
  box(5.9, 2.6, 2.0, 1.1, fill = BOX, colour = EDGE) +
  lbl(5.9, 2.82, "pre-registered validation", 3.2, INK, "bold") +
  lbl(5.9, 2.46, "expectation written first;\ntested on unjudged data", 2.5, MUTED) +
  ## split to the two outcomes (orthogonal)
  seg(6.9, 2.6, 7.15, 2.6, ah = FALSE, lw = 0.45) +
  seg(7.15, 2.6, 7.15, 3.4, ah = FALSE, lw = 0.45) +
  seg(7.15, 3.4, 7.33, 3.4, lw = 0.45) +
  seg(7.15, 2.6, 7.15, 1.4, ah = FALSE, lw = 0.45) +
  seg(7.15, 1.4, 7.33, 1.4, lw = 1.8) +
  ## exploration kill-path joins the falls-short rail
  seg(3.55, 2.05, 3.55, 1.4, ah = FALSE, lw = 1.8) +
  seg(3.55, 1.4, 7.14, 1.4, ah = FALSE, lw = 1.8) +
  lbl(5.2, 1.56, "most ideas stop in exploration", 2.3, MUTED, "bold") +
  ## outcomes column (centers x = 8.3)
  box(8.3, 3.4, 1.9, 1.1, fill = BOX, colour = EDGE) +
  lbl(8.3, 3.68, "adopted", 3.2, INK, "bold") +
  lbl(8.3, 3.40, "joins the recommended workflow", 2.4, MUTED) +
  lbl(8.3, 3.12, "e.g., confidence-bound selection;\nthe blended state + national list", 2.2, MUTED) +
  box(8.3, 1.4, 1.9, 1.1, fill = BOX, colour = EDGE) +
  lbl(8.3, 1.68, "retired, in writing", 3.2, INK, "bold") +
  lbl(8.3, 1.40, "claim + numbers recorded in\nmethods/modeling_findings.md", 2.4, MUTED) +
  lbl(8.3, 1.08, "e.g., \"low subsampling beats high\";\nper-state tuning as the default", 2.2, MUTED) +
  ## both outcomes converge on the versioning terminal (orthogonal)
  seg(9.25, 3.4, 9.55, 3.4, ah = FALSE) +
  seg(9.55, 3.4, 9.55, 2.95, ah = FALSE) +
  seg(9.55, 2.95, 9.83, 2.95) +
  seg(9.25, 1.4, 9.55, 1.4, ah = FALSE) +
  seg(9.55, 1.4, 9.55, 2.25, ah = FALSE) +
  seg(9.55, 2.25, 9.83, 2.25) +
  box(10.95, 2.6, 2.2, 1.3, fill = scales::alpha(BLUE, 0.14), colour = BLUE) +
  lbl(10.95, 2.98, "recorded under the\nversioning policy", 3.1, INK, "bold") +
  lbl(10.95, 2.40, "CHANGELOG.md entry; release tag;\nsuperseded pieces deprecated or\narchived, never deleted\n(VERSIONING.md)", 2.3, MUTED) +
  ## the loop closes: terminal -> next proposal
  seg(10.95, 1.95, 10.95, 0.45, col = MUTED, lt = "22", ah = FALSE, lw = 0.7) +
  seg(10.95, 0.45, 1.3, 0.45, col = MUTED, lt = "22", ah = FALSE, lw = 0.7) +
  seg(1.3, 0.45, 1.3, 2.03, col = MUTED, lt = "22", lw = 0.7) +
  lbl(6.1, 0.60, "what was learned - adopted or retired - informs the next proposal", 2.2, MUTED) +
  lbl(6.1, 4.25, "How a change earns its way into the recommended workflow", 4.6, INK, "bold") +
  coord_cartesian(xlim = c(0.25, 12.15), ylim = c(0.28, 4.45)) +
  theme_void()
ggsave("presentation_figures/refinement_loop.png", p,
       width = 12.2, height = 4.3, dpi = 300, bg = "white")
cat("wrote presentation_figures/refinement_loop.png\n")
