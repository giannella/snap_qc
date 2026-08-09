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
`vocab_attribution_paired_deltas.csv`, `vocab_attribution_seed_noise.csv`.
