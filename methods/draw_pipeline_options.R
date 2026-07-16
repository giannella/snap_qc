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

chip <- function(x, y, w, h, fill, colour, text, tsize = 2.6) {
  list(box(x, y, w, h, fill = fill, colour = colour),
       lbl(x, y, text, tsize, INK, lh = 0.9))
}
pB <- ggplot() +
  # ── national lane ──
  box(1.0, 3.0, 1.6, 1.0, fill = scales::alpha(BLUE, 0.18), colour = BLUE) +
  lbl(1.0, 3.17, "all states' public
QC data 2022-24", 3.1, INK) +
  lbl(1.0, 2.72, "118,263 cases", 2.8, BLUE, "bold") +
  chip(3.0, 3.30, 1.55, 0.48, scales::alpha(BLUE, 0.10), BLUE,
       "xgboost (boosted trees)") +
  chip(3.0, 2.70, 1.55, 0.48, scales::alpha(BLUE, 0.10), BLUE,
       "ranger (random forest)") +
  lbl(3.0, 2.30, "mined separately for 3 household sizes;
every tree branch becomes a candidate rule", 2.4, MUTED) +
  seg(1.82, 3.12, 2.18, 3.28, BLUE) + seg(1.82, 2.88, 2.18, 2.72, BLUE) +
  seg(3.82, 3.28, 4.18, 3.10, BLUE) + seg(3.82, 2.72, 4.18, 2.90, BLUE) +
  box(5.0, 3.0, 1.6, 1.0, fill = scales::alpha(BLUE, 0.18), colour = BLUE) +
  lbl(5.0, 3.17, "national rule pool
(passed the filter)", 3.1, INK) +
  lbl(5.0, 2.72, "146,787 -> 48,429", 2.8, BLUE, "bold") +
  lbl(5.0, 2.30, "mined once, cached,
reused for every state", 2.4, MUTED) +
  # ── state lane ──
  box(1.0, 1.0, 1.6, 1.0, fill = scales::alpha(ORANGE, 0.22), colour = ORANGE) +
  lbl(1.0, 1.17, "your state's cases
(or internal files)", 3.1, INK) +
  lbl(1.0, 0.72, "~2,000-3,000 / year", 2.8, "#9a6b00", "bold") +
  chip(3.0, 1.30, 1.55, 0.48, scales::alpha(ORANGE, 0.12), ORANGE,
       "xgboost (boosted trees)") +
  chip(3.0, 0.70, 1.55, 0.48, scales::alpha(ORANGE, 0.12), ORANGE,
       "ranger (random forest)") +
  lbl(3.0, 0.30, "the same mining machinery,
run on your data", 2.4, MUTED) +
  seg(1.82, 1.12, 2.18, 1.28, ORANGE) + seg(1.82, 0.88, 2.18, 0.72, ORANGE) +
  seg(3.82, 1.28, 4.18, 1.10, ORANGE) + seg(3.82, 0.72, 4.18, 0.90, ORANGE) +
  box(5.0, 1.0, 1.6, 1.0, fill = scales::alpha(ORANGE, 0.22), colour = ORANGE) +
  lbl(5.0, 1.17, "your state's rule pool
(same filter)", 3.1, INK) +
  lbl(5.0, 0.72, "~15,000 rules", 2.8, "#9a6b00", "bold") +
  # ── shared evidence bar note between lanes ──
  lbl(4.4, 2.00, "filter, both pools: keep a rule only if precision
is statistically above the base error rate
(99% lower bound, support >= 30)", 2.4, MUTED) +
  # ── merge on one scale ──
  box(7.2, 2.0, 1.8, 1.3, fill = BOX, colour = EDGE) +
  lbl(7.2, 2.30, "one confidence scale", 3.3, INK, "bold") +
  lbl(7.2, 1.86, "every rule ranked by the 99%
lower confidence bound of its
precision on its own training data", 2.6, MUTED) +
  seg(5.82, 2.85, 6.28, 2.30, BLUE) +
  seg(5.82, 1.15, 6.28, 1.70, ORANGE) +
  # ── frozen list ──
  box(9.4, 2.0, 1.7, 1.3, fill = BOX, colour = EDGE) +
  lbl(9.4, 2.32, "your ranked list", 3.3, INK, "bold") +
  lbl(9.4, 1.86, "filled to your 5% or 10%
review budget, plus
substitutes to 3x depth", 2.6, MUTED) +
  seg(8.12, 2.0, 8.52, 2.0) +
  # ── deployment ──
  box(11.5, 2.0, 1.5, 1.3, fill = "white", colour = INK) +
  lbl(11.5, 2.32, "reviewers", 3.3, INK, "bold") +
  lbl(11.5, 1.86, "work rules in order
until capacity fits;
no outcome data
needed", 2.6, MUTED) +
  seg(10.27, 2.0, 10.72, 2.0) +
  annotate("text", x = 10.45, y = 1.14, hjust = 0.5, label = "blended_delivery_<State>_budget05/10.csv",
           size = 2.2, colour = BLUE, family = "mono") +
  lbl(6.3, 3.95, "Two engines, two data sources, one evidence-ranked list", 5, INK, "bold") +
  lbl(6.3, 0.02, "Small-sample state rules are automatically discounted by the confidence bound; they earn slots only where their evidence overcomes the penalty (e.g., DC: 16 of its 18 deployed rules are its own).", 2.7, MUTED) +
  lbl(6.3, -0.22, "Refresh cadence: re-run mining when a new QC year is released; during the year, reviewers work the frozen list and never need outcome data.", 2.7, MUTED) +
  coord_cartesian(xlim = c(0.1, 12.4), ylim = c(-0.35, 4.2)) +
  theme_void()
ggsave("presentation_figures/pipeline_option_B.png", pB,
       width = 13.2, height = 4.9, dpi = 300, bg = "white")

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
