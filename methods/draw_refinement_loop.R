# Companion figure to the pipeline diagram (draw_pipeline_options.R, option B):
# how a proposed change qualifies for the recommended workflow, and what
# release means under VERSIONING.md. Same visual language. Examples on the
# exits are real, completed cases from modeling_findings.md.
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
  # 1 proposal
  box(1.15, 2.0, 1.9, 1.4, fill = scales::alpha(BLUE, 0.14), colour = BLUE) +
  lbl(1.15, 2.35, "proposed improvement", 3.3, INK, "bold") +
  lbl(1.15, 1.90, "a new engine, rule\nvocabulary, or selection\nstatistic", 2.7, MUTED) +
  seg(2.15, 2.0, 2.42, 2.0) +
  # 2 exploratory comparison
  box(3.6, 2.0, 2.1, 1.4, fill = BOX, colour = EDGE) +
  lbl(3.6, 2.35, "exploratory comparison", 3.3, INK, "bold") +
  lbl(3.6, 1.86, "head-to-head against the\ncurrent recipe: same states,\nbudgets, and protocol\n(train 2022-23, test 2024)", 2.6, MUTED) +
  seg(4.7, 2.0, 5.15, 2.0) +
  lbl(4.92, 2.14, "promising", 2.2, MUTED) +
  # 3 pre-registered validation
  box(6.35, 2.0, 2.3, 1.4, fill = BOX, colour = EDGE) +
  lbl(6.35, 2.35, "pre-registered validation", 3.3, INK, "bold") +
  lbl(6.35, 1.86, "expected outcome written down\nfirst; tested on data that never\njudged the idea (held-out year,\nseparate era)", 2.6, MUTED) +
  # exits from validation
  seg(7.55, 2.35, 7.86, 2.80) +
  lbl(7.45, 2.95, "meets\nexpectation", 2.2, MUTED) +
  seg(7.55, 1.65, 7.86, 1.35) +
  lbl(7.45, 1.18, "falls\nshort", 2.2, MUTED) +
  # exploration kill-path (most ideas stop here)
  seg(4.15, 1.28, 7.86, 1.05, lt = "solid") +
  lbl(5.5, 0.98, "falls short in exploration (most ideas stop here)", 2.2, MUTED) +
  # adopted lane
  box(8.9, 3.0, 2.0, 1.15, fill = BOX, colour = EDGE) +
  lbl(8.9, 3.28, "adopted", 3.3, INK, "bold") +
  lbl(8.9, 2.90, "becomes part of the\nrecommended workflow", 2.6, MUTED) +
  lbl(8.9, 2.27, "e.g., confidence-bound selection (v2);\nthe blended state + national list", 2.4, MUTED) +
  seg(9.92, 3.0, 10.38, 2.62) +
  box(11.7, 2.05, 2.55, 1.5, fill = scales::alpha(BLUE, 0.14), colour = BLUE) +
  lbl(11.7, 2.48, "recorded under the\nversioning policy, either way", 3.1, INK, "bold") +
  lbl(11.7, 1.88, "CHANGELOG.md entry; release tag\nwhen the workflow changes;\nsuperseded or retired pieces are\ndeprecated with pointers or archived\n- never deleted (VERSIONING.md)", 2.5, MUTED) +
  # retired lane
  box(8.9, 1.1, 2.0, 1.15, fill = BOX, colour = EDGE) +
  lbl(8.9, 1.38, "retired, in writing", 3.3, INK, "bold") +
  lbl(8.9, 0.98, "the claim and its numbers recorded\nin methods/modeling_findings.md", 2.5, MUTED) +
  lbl(8.9, 0.38, "e.g., \"low subsampling beats high\" (2026-07);\nper-state threshold tuning as the default (2026-07)", 2.4, MUTED) +
  seg(9.92, 1.1, 10.38, 1.5) +
  # return loop: failures inform the next proposal
  seg(7.86, 0.75, 7.5, 0.5, col = MUTED, lt = "22", ah = FALSE, lw = 0.9) +
  seg(7.5, 0.5, 1.15, 0.5, col = MUTED, lt = "22", ah = FALSE, lw = 0.9) +
  seg(1.15, 0.5, 1.15, 1.26, col = MUTED, lt = "22", lw = 0.9) +
  lbl(4.3, 0.36, "what failed, and why, informs the next proposal", 2.2, MUTED) +
  lbl(6.5, 3.85, "How a change earns its way into the recommended workflow", 5, INK, "bold") +
  coord_cartesian(xlim = c(0.1, 13.0), ylim = c(-0.02, 4.1)) +
  theme_void()
ggsave("presentation_figures/refinement_loop.png", p,
       width = 13, height = 4.6, dpi = 300, bg = "white")
cat("wrote presentation_figures/refinement_loop.png\n")
