# Vocabulary attribution: 16 vs 26 features (run 2026-08-08 -> 09)

Design: `methods/design_note_vocab_attribution_2026-08-08.md`. Review:
`methods/vocab_attribution_review_2026-08-08.md` (approve with flags).
Ran 21:21-01:20, all 120 walk cells valid (slack 0; findings 27), no
errors, seeds 117 / 20260805 / 31415 paired across arms, rebuilt frame of
2026-08-08, FY2024 held out, ten-state panel, legacy findings-31 walk.

## Result: the 26-feature package is NOT recommended for adoption as-is

Pre-registered readout (within-state median paired delta, candidate minus
baseline, seed-mean, with the mandatory companions):

| budget | median | mean | harmed tail (< -0.05) | median d dollar-recall | seed-noise ref (base / cand) |
|---|---|---|---|---|---|
| 5% | +0.0000 | -0.0231 | 2 of 10 | -0.0082 | 0.0553 / 0.0741 |
| 10% | -0.0046 | -0.0050 | 1 of 10 | -0.0068 | 0.0415 / 0.0353 |

Every delta sits inside the within-arm across-seed spread, so nothing here
is attributable to the vocabulary in either direction; the tilt is mildly
negative and the harmed tail is real (Massachusetts -0.2017 at 5% /
-0.1272 at 10%, Michigan -0.0635 at 5% - see
`vocab_attribution_paired_deltas.csv`). Under the mandatory-companions
rule a flat median with both companions negative does not ship.

## What the candidate vocabulary did do

- The miner uses it heavily: median 85.8% of deployed candidate-arm rules
  reference at least one new feature (60 of 60 state x seed x budget
  cells deploy some); `rawrent_p` and `rawmedded_p` reached admitted
  rank 1 in individual seeds (`vocab_attribution_feature_usage.csv`).
- Heavy usage with no performance change is the findings 30-31 pattern
  again: different vocabularies re-describe largely the same errors
  (text churn, not work churn). The added features substitute for
  incumbent rules without extending what the lists catch.
- Absolute FY2024 levels on the rebuilt frame (median across panel and
  seeds): baseline 0.325 at 5% / 0.286 at 10% - consistent with the
  shipped benchmark levels. The findings-28 artifact share of flagged
  cases is low in both arms (median 0.023-0.032): the recreation fix in
  the 2026-08-08 frame already thinned the band.

## Caveats and open ends

- Ten states x three seeds supports a recommendation, not a ledger
  promotion; the panel deltas are within seed noise by construction of
  the readout.
- The candidate arm bundles two feature families (3 name-fixed per-size
  income features + 7 frozen percentiles); this run attributes the
  PACKAGE, not the families (review flag 3). Family-level arms or
  cache-based ablation re-walks are the follow-up if wanted - all six
  scored pools are cached under `cache/` (gitignored), so no re-mining
  is needed.
- The Massachusetts collapse (one state, both budgets) is unexplained;
  its deployed-list composition is recoverable from cache.
- The frozen train-year percentile construction worked as designed and
  is the right template if any percentile feature is revisited.

Files: `vocab_attribution_run_info.csv` (pools), 
`vocab_attribution_feature_usage.csv` (admitted-pool usage),
`vocab_attribution_budget_readout.csv` (per state x arm x seed x budget),
`vocab_attribution_paired_deltas.csv`, `vocab_attribution_seed_noise.csv`,
`vocab_attribution_window_usage_all_features.csv` (per-feature share of the
top-20k window, both arms, incumbents and new).

## Additional considerations from the results review (2026-08-09)

**1. The artifact fix did not cost performance — a win (the headline
reading of this study).** The baseline is
artificially high because of the artifact: its top-ranked rule is a
near-boundary rel_max shape in all three seeds, so the bar the candidate
arm was measured against still carries residual artifact-correlate
inflation. That the new variables and the fixed data frame produce the
SAME overall performance against that inflated bar — while drawing 85.8%
of their deployed rules from legitimate features instead — is a win, not
a null: benchmark-level delivered precision no longer depends on the
reconstruction artifact. The details:

Findings 28
measured that 88 delivered rules took 76.7% of their flags from the
reconstruction band just below max benefit, and two of three seeds put a
band rule at national rank 1 (findings 31). The 2026-08-08 frame rebuild
(the $0-tolerance recreation) removed most of that band (in-band errors
537 -> 227; the artifact share of flagged cases in this run is 2-3%
against the findings-28 median of 6.3%). The reasonable expectation was
that losing that rule class would drop delivered precision. It did not:
baseline medians on the rebuilt frame are 0.325 at 5% / 0.286 at 10%,
at the shipped benchmark levels (49-state medians 0.314 / 0.275 on the
old frame; cross-panel and cross-frame, so indicative rather than
paired). The pipeline replaced the artifact rules with legitimate rules
of equal delivered performance.

**2. Results review (PDS analysis + fresh senior-statistician).**

- **Correction to the null reading above.** "Nothing attributable in
  either direction" understates the structure. The paired cells carry
  sign information: Massachusetts is negative in 6 of 6 cells on BOTH
  precision and dollars (10%: -0.145, -0.092, -0.145 across seeds);
  Michigan 6 of 6 negative on both; Mississippi 5 of 6 positive with
  +0.10 precision in all three seeds at 10%. The result is a
  sign-consistent REDISTRIBUTION netting to a flat median - within-state
  moves of 0.10-0.20 in both directions. For a state-facing deliverable
  that is a worse property than a true null, and it makes the
  do-not-adopt verdict firmer.
- **Rarity-based variable exclusion (rules rarely in the top 20k) is
  rejected.** In this run's own data, usage predicts nothing in either
  direction: the new features hold 85.8% of deployed rules with zero
  gain, while the rarest window features are the INCUMBENTS homeless
  (1.5%), children_i (1.6%), and married (2.5%) - a rarity rule would
  trim those validated features before it touched rawcsded_p (3.9%). A
  feature's window share also is not stable to which other features are
  present (children_i halves when the new features enter), and policing
  chance is the admission system's job at the rule level (BH FDR 10% +
  n >= 30, findings 19). Membership belongs to validity/deployability
  grounds plus measured family-level delivered effect (ablation arms) -
  the grounds actually used to exclude unc_fsben_rel_max, at_max_ben,
  and second_element_i.
- **Massachusetts is attributable displacement, with magnitude caution.**
  The base arm there is the panel's most stable cell (0.553-0.579 at 5%,
  spread 0.026); the candidate arm collapses in two of three seeds
  (0.263/0.289, catching 10-11 of 38 flags vs 21-22) and is negative in
  all six cells including dollars - sign-consistent, ~3.5 SE, not
  sampling noise. Trust the sign, not the -0.20 (an extreme order
  statistic across ten states). Cache-based re-walks (drop new-feature
  rules from the candidate pool; then per family) attribute the
  mechanism in minutes and should run before the five-arm design note is
  finalized. Guard: a fix chosen because it rescues MA must be judged on
  data that did not choose it.
- **Five-arm implication: the factorial baseline is the 16-feature
  vocabulary, everywhere, including the state-typed arm.** The package
  is excluded whole or trimmed (failed its pre-registered bar; harm is
  sign-consistent; features needing state-side lookup tables must clear
  a POSITIVE bar, not do-no-harm, because per-state frozen cutoff tables
  are a real cost). One correction to `methods/remine_proposal_2026-08.md`:
  its baseline arm says "current 19 features" - the true shipped
  vocabulary is 16 (the three raw*_by_hh_size names never existed in the
  frame). This result does NOT pre-judge the issue-7 binary outlier-indicator
  arm: a different construction with its own pre-screen, which stands.
  The frozen train-only percentile plumbing built here is validated and
  is the template for the outlier arm's cutoffs.
- **New standing companions for attribution readouts**: per-state
  same-sign paired-cell counts (would have caught the MA/MI structure
  the median/mean/harmed-tail trio missed), and per-arm seed spread
  (the candidate arm's 5% spread, 0.074 vs 0.055, is suggestively wider
  - a vocabulary that widens findings-31 instability is a cost even at
  equal precision; three seeds estimate it coarsely).

**3. The five-arm mining has NOT run.** This study was a two-arm
contrast. The remine proposal's four national factorial arms and the
state-typed arm remain pending; their results are unknown, and their
design note should incorporate the recommendations above.

**4. The Massachusetts-baseline ruling (2026-08-09).**
Precision at the 0.55-0.58 level is what artifact-driven mining
produced: the pre-fix era's results were inflated by the reconstruction
artifact (findings 28), and with the artifact diminished by the
2026-08-08 recreation fix, lower results at MA are the expected
deflation toward truth, not harm. A drop from that level is therefore
not a concern unless the resulting level falls below 0.30. Applied to
the cells: candidate MA at 5% is 0.263 / 0.289 / 0.526 (two seeds
marginally below 0.30) and at 10% is 0.250 / 0.276 / 0.329; Michigan
never falls below 0.298. Under this ruling the MA/MI redistribution
concern is marginal, and the do-not-adopt verdict rests on the primary
readout (no gain, mandatory companions negative, deployment cost), not
on the harmed tail. Context recorded with it: the strict findings-28
band accounts for 2.6-5.3% of MA's 38 baseline flags, so any remaining
inflation runs through correlates of the artifact (e.g. the
near-boundary rel_max rule shapes that still top the baseline ranking)
rather than the strict band itself; MA's baseline lift is 3.7x over its
0.1545 FY2024 base rate.
