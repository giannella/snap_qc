# Three candidate figures explaining the delivery-list pipeline, for the deck.
# Audience: state program analysts. All counts/precision figures are from the
# published any-error recipe (national 2022-24 pool; blended benchmark,
# findings 14-16).
#   A  funnel: mine everything, trust little (stage counts)
#   B  two streams: national + state pools merge on one confidence scale
#   C  recipe card: numbered steps, what you run and what you get
# Output: presentation_figures/pipeline_option_{A,B,C}.png

suppressMessages(library(ggplot2))
dir.create("presentation_figures", showWarnings = FALSE)

INK    <- "#1a1a2e"   # primary text
MUTED  <- "#6b7280"   # secondary text
BOX    <- "#e5e7eb"   # neutral structure fill
EDGE   <- "#9ca3af"
BLUE   <- "#0072B2"   # national / pipeline accent (Okabe-Ito)
ORANGE <- "#E69F00"   # the state's own data accent (Okabe-Ito)

arrowspec <- arrow(length = unit(7, "pt"), type = "closed")

## ── Option A: the funnel ──────────────────────────────────────────────────────
stages <- data.frame(
  x = 1:5,
  half = c(2.0, 1.35, 0.85, 0.45, 0.18),
  title = c("mine candidates", "clean & de-duplicate", "filter on evidence",
            "rank on one scale", "fill to your budget"),
  sub = c("two tree engines x 3\nhousehold-size groups",
          "round thresholds, drop\nredundant renderings",
          "kept only if precision is\nstatistically above the\nbase error rate",
          "99% lower confidence\nbound of precision;\nstate + national together",
          "add rules in rank order\nuntil review capacity fits"),
  n = c("146,787 candidate rules", "~80,000 distinct rules",
        "48,429 rules with evidence", "one ranked pool",
        "23-42 rules deployed")
)
polys <- do.call(rbind, lapply(1:4, function(i) data.frame(
  id = i,
  x = c(stages$x[i] + 0.28, stages$x[i + 1] - 0.28,
        stages$x[i + 1] - 0.28, stages$x[i] + 0.28),
  y = c(stages$half[i], stages$half[i + 1],
        -stages$half[i + 1], -stages$half[i])
)))
pA <- ggplot() +
  geom_polygon(data = polys, aes(x, y, group = id), fill = BLUE, alpha = 0.12) +
  geom_rect(data = stages,
            aes(xmin = x - 0.28, xmax = x + 0.28, ymin = -half, ymax = half),
            fill = BLUE, alpha = 0.25, colour = BLUE, linewidth = 0.4) +
  geom_text(data = stages, aes(x, half + 0.95, label = title),
            fontface = "bold", size = 4.1, colour = INK, vjust = 1) +
  geom_text(data = stages, aes(x, half + 0.73, label = sub),
            size = 2.9, colour = MUTED, vjust = 1, lineheight = 0.95) +
  geom_text(data = stages, aes(x, -half - 0.22, label = n),
            size = 3.4, colour = BLUE, fontface = "bold", vjust = 1) +
  annotate("text", x = 3, y = 3.25,
           label = "146,787 mined candidates become 23-42 deployed rules",
           size = 5, fontface = "bold", colour = INK) +
  annotate("text", x = 3, y = -2.55,
           label = "Deployed lists, tested on a year they never saw: median precision 0.32 at a 5% review budget, 0.26 at 10% - every state above its base error rate.",
           size = 3.1, colour = MUTED) +
  coord_cartesian(xlim = c(0.5, 5.5), ylim = c(-2.8, 3.45)) +
  theme_void()
ggsave("presentation_figures/pipeline_option_A.png", pA,
       width = 11, height = 5.4, dpi = 300, bg = "white")

## ── Option B: two streams merge ───────────────────────────────────────────────
box <- function(x, y, w, h, fill, colour = NA) {
  annotate("rect", xmin = x - w/2, xmax = x + w/2, ymin = y - h/2, ymax = y + h/2,
           fill = fill, colour = colour, linewidth = 0.4)
}
lbl <- function(x, y, text, size = 3.4, col = INK, face = "plain", lh = 0.95)
  annotate("text", x = x, y = y, label = text, size = size, colour = col,
           fontface = face, lineheight = lh)
seg <- function(x1, y1, x2, y2, col = EDGE)
  annotate("segment", x = x1, y = y1, xend = x2, yend = y2,
           colour = col, linewidth = 0.6, arrow = arrowspec)

pB <- ggplot() +
  # national lane
  box(1.1, 3.0, 1.7, 1.0, fill = scales::alpha(BLUE, 0.18), colour = BLUE) +
  lbl(1.1, 3.17, "all states' public\nQC data 2022-24", 3.2, INK) +
  lbl(1.1, 2.72, "118,263 cases", 2.9, BLUE, "bold") +
  box(3.3, 3.0, 1.8, 1.0, fill = scales::alpha(BLUE, 0.18), colour = BLUE) +
  lbl(3.3, 3.17, "national rule pool\n(mined + filtered)", 3.2, INK) +
  lbl(3.3, 2.72, "48,429 rules", 2.9, BLUE, "bold") +
  seg(1.95, 3.0, 2.4, 3.0, BLUE) +
  # state lane
  box(1.1, 1.0, 1.7, 1.0, fill = scales::alpha(ORANGE, 0.22), colour = ORANGE) +
  lbl(1.1, 1.17, "your state's cases\n(or internal files)", 3.2, INK) +
  lbl(1.1, 0.72, "~2,000-3,000 / year", 2.9, "#9a6b00", "bold") +
  box(3.3, 1.0, 1.8, 1.0, fill = scales::alpha(ORANGE, 0.22), colour = ORANGE) +
  lbl(3.3, 1.17, "your state's rule pool\n(same mining, your data)", 3.2, INK) +
  lbl(3.3, 0.72, "~15,000 rules", 2.9, "#9a6b00", "bold") +
  seg(1.95, 1.0, 2.4, 1.0, ORANGE) +
  # merge
  box(5.6, 2.0, 2.0, 1.3, fill = BOX, colour = EDGE) +
  lbl(5.6, 2.28, "one confidence scale", 3.5, INK, "bold") +
  lbl(5.6, 1.86, "every rule ranked by the 99%\nlower confidence bound of\nits own precision", 2.9, MUTED) +
  seg(4.2, 3.0, 4.75, 2.35, BLUE) +
  seg(4.2, 1.0, 4.75, 1.65, ORANGE) +
  # frozen list
  box(7.9, 2.0, 1.9, 1.3, fill = BOX, colour = EDGE) +
  lbl(7.9, 2.32, "your ranked list", 3.5, INK, "bold") +
  lbl(7.9, 1.88, "filled to your 5% or 10%\nreview budget, plus\nsubstitutes to 3x depth", 2.9, MUTED) +
  seg(6.6, 2.0, 6.95, 2.0) +
  # deployment
  box(9.9, 2.0, 1.6, 1.3, fill = "white", colour = INK) +
  lbl(9.9, 2.32, "reviewers", 3.5, INK, "bold") +
  lbl(9.9, 1.88, "work rules in order\nuntil capacity fits;\nno outcome data\nneeded", 2.9, MUTED) +
  seg(8.85, 2.0, 9.1, 2.0) +
  lbl(5.5, 3.85, "A state's rules compete with national rules on equal, evidence-priced terms", 5, INK, "bold") +
  lbl(5.5, 0.15, "Small-sample state rules are automatically discounted by the confidence bound; they earn slots only where their evidence overcomes the penalty (e.g., DC: 16 of its 18 deployed rules are its own).", 3.0, MUTED) +
  coord_cartesian(xlim = c(0.1, 10.8), ylim = c(-0.1, 4.1)) +
  theme_void()
ggsave("presentation_figures/pipeline_option_B.png", pB,
       width = 11.5, height = 4.6, dpi = 300, bg = "white")

## ── Option C: the recipe card ─────────────────────────────────────────────────
steps <- data.frame(
  y = 7:1,
  n = 1:7,
  title = c("Build the case table",
            "Mine candidate rules",
            "Clean and de-duplicate",
            "Score against ALL error types",
            "Filter on statistical evidence",
            "Blend and rank",
            "Fill to budget and freeze"),
  what = c("one row per case: features + error yes/no",
           "two tree engines x 3 household-size groups; every branch becomes a candidate",
           "round thresholds; collapse rules that flag identical cases",
           "a rule mined for one error type gets credit for any error it finds",
           "keep a rule only if its precision is statistically above the base rate (99% lower bound, support >= 30)",
           "state + national rules on one confidence scale",
           "add rules in rank order to your 5%/10% review capacity, plus labeled substitutes"),
  runs = c("1_data_munging_..._public_qc_data.R",
           "INCL_build_blended_delivery_list_v2.R  (steps 2-7, one script)",
           "", "", "", "",
           "-> state_delivery_lists/blended_delivery_<State>_budget05/10.csv")
)
pC <- ggplot(steps) +
  annotate("segment", x = 0.32, xend = 0.32, y = 0.65, yend = 7.35,
           colour = EDGE, linewidth = 0.7) +
  geom_point(aes(x = 0.32, y = y), size = 9, colour = BLUE) +
  geom_text(aes(x = 0.32, y = y, label = n), colour = "white",
            size = 4.2, fontface = "bold") +
  geom_text(aes(x = 0.55, y = y + 0.18, label = title), hjust = 0,
            size = 4.2, fontface = "bold", colour = INK) +
  geom_text(aes(x = 0.55, y = y - 0.14, label = what), hjust = 0,
            size = 3.1, colour = MUTED) +
  geom_text(aes(x = 0.55, y = y - 0.34, label = runs), hjust = 0,
            size = 2.8, colour = BLUE, family = "mono") +
  annotate("text", x = 0.32, y = 7.85, hjust = 0,
           label = "From public data to a ranked review list",
           size = 5.4, fontface = "bold", colour = INK) +
  annotate("text", x = 0.32, y = 0.28, hjust = 0,
           label = "Tested one year ahead: median precision 0.32 at a 5% budget (0.26 at 10%); 23-42 rules deployed per state.",
           size = 3.0, colour = MUTED) +
  coord_cartesian(xlim = c(0.2, 4.4), ylim = c(0.1, 8.1)) +
  theme_void()
ggsave("presentation_figures/pipeline_option_C.png", pC,
       width = 9, height = 7, dpi = 300, bg = "white")

cat("wrote pipeline_option_A/B/C.png\n")
