# Deck figure critique (Kieran Healy pass, 2026-07)

Simplify-first review of the deck figures. Bias: less ink, one idea per figure, direct
labels over legends, no dual-encoding, and every number traceable to the findings docs.
Ordered by priority. "Script committed?" matters because a figure with no committed
generating script cannot be regenerated or drift-checked (that is exactly how a stale
number survived in the ladder, below).

## Fixed in this pass

- **`evaluation_ladder.png`** (script: `draw_evaluation_ladder.R`, committed). The top
  rung showed the stale "public files show 43-81% of error cases"; the source CSV puts
  the range at 43% (New Jersey) to 91% (Georgia). Corrected to 43-91% and the one ` - `
  dash to a comma; regenerated. Good conceptual figure otherwise: clear "harder to fool"
  axis, direct annotations, keep as is.

## High priority

- **`winners_curse_raw_vs_lcb.png`** — the **title is cut off** at the right edge ("...
  disappoint on new d[ata]"); the canvas is too narrow for the title, so a viewer never
  sees the headline. Fix: shorten the title (e.g. "Rules that look good on their own data
  disappoint on new data") or widen the plot. The subtitle also uses a ` - ` dash. The
  chart itself is strong (two lines vs a "what was promised" diagonal, direct line labels)
  and should be kept once the title fits. **No committed script** — needs the generating
  script to fix (see recommendation below).

- **`states_two_regimes.png`** — the weakest figure. It **dual-encodes**: bar height is
  "share of error dollars caught" while a text label on every bar reads "accuracy 0.XX"
  (precision). Two metrics per bar, fourteen "accuracy" labels, and the two regimes
  (states with enough data vs not) are told only in the subtitle, not shown. Healy rebuild:
  a single-metric dot plot of **tuned minus national** per state, ordered by that gap and
  faceted or colored by "enough data / too little," so the regime split is the visual. Data
  is committed (`archive/state_rules_v2/state_union_summary.csv`) but the generating script
  is not, and the archived numbers differ slightly from the current PNG (a superseded run),
  so a rebuild would shift the numbers — confirm the intended source before rebuilding. Note
  also that findings §9 is partly superseded, so this appendix figure could simply be
  demoted.

## Medium priority (keep the design; small fixes, all need the missing scripts)

- **`esap_error_mix.png`** — clear 100% stacked comparison with direct % labels; keep. Fix
  the ` - ` dash in the title ("separate model - their errors"). Only caution: a 4-segment
  stack makes the middle bands hard to compare across the two bars; if that comparison ever
  matters, a small-multiple of four side-by-side bars would be cleaner. For "the mix
  differs," the stack is fine.

- **`mine_big_filter_stringently.png`** — strong: the two lines nearly coincide and the
  direct annotation ("same performance here, but 2,026 rules to choose from vs 789") makes
  the point without a legend lookup. Keep. Fix the ` - ` dash in the title.

- **`pipeline_option_B.png`** ("How lists are built") — clean flow diagram; keep. Light
  tightening: the two engine boxes per lane (xgboost / ranger) could collapse to "two tree
  methods" if the engines are not the point of this slide, and the "your ranked list"
  caption is wordy. Verify the "146,787 -> 37,795" counts still match the current pipeline.

## Lower priority

- **Tuning-sweep appendix charts** (xgboost / ranger / stringency / subsampling sweeps,
  single-engines-vs-pairs, mtry frontier). Appropriate as an appendix. Healy notes: keep
  axes identical across the small multiples so panels are comparable, thin the gridlines,
  and prefer direct labels to legends where a panel has 2-3 series. Not urgent.

## Systemic

1. **` - ` dashes in figure titles/subtitles** are the same compression problem as the deck
   text. Sweep the committed figure scripts and convert them to spare punctuation.
2. **Commit the missing figure scripts.** These four were committed as PNG binaries only,
   so they cannot be regenerated or checked when a number changes — which is precisely how
   the ladder kept a stale "43-81%". Progress on reconstructing them:
   - **`mine_big_filter_stringently`** — reconstructed as `draw_mine_big_filter_stringently.R`
     from committed data (`parameter_tuning_v2/v2_lcbz_sweeps.csv`); reproduces the original
     exactly (789 vs 2,026 rules), with the ` - ` title dash fixed. Done.
   - **`esap_error_mix`** — reconstructed as `draw_esap_error_mix.R` and regenerated from the
     current frame (it COMPUTES the mix rather than hardcoding it). Current-frame result is
     63/20/11/6 (elderly) and 29/10/44/17 (other), essentially the original 64/18/11/6 &
     29/10/45/16. Done.
   - **`states_two_regimes`** — NOT reconstructed: the only committed data
     (`archive/state_rules_v2/state_union_summary.csv`) is a different, superseded run whose
     story contradicts the original figure (it shows tuning helping 6/7 states; the original
     shows it hurting 4). Rebuilding from it would misrepresent. Needs the original script,
     or a fresh figure built on the current §14 deployment data.
   - **`winners_curse_raw_vs_lcb`** — NOT reconstructed: the raw-vs-lower-bound decay is not
     committed as a single sweep CSV. Needs the original script or the frame.
