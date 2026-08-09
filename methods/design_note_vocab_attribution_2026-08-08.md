# Design note: vocabulary attribution re-mine (2026-08-08, overnight)

Eric delegated tonight's cycle (2026-08-08): "have the senior-statistician and
PDS sort things out and at least get it running in the next hour or two."
Decisions normally his are marked below; anything that clears its bar is a
recommendation, not a ship — promotion stays his call (VERSIONING.md).

## 1. The question (one sentence)

Does replacing the shipped 16-feature national any-error mining vocabulary
with the Eric-confirmed 26-feature vocabulary (the 3 name-fixed per-size
income features plus the 7 frozen train-year percentile features) change
delivered budget-list performance on a true future year (FY2024) beyond seed
noise?

## 2. What varies, what is held fixed (exactly one varying)

**Varies: the mining feature vocabulary only.**

- baseline arm: the 16 features every v2 mine has actually used (the three
  `raw*_by_hh_size` names in the finder vectors never existed in the frame;
  `prep_features()` dropped them silently — established 2026-08-08, Gate-1
  session).
- candidate arm: those 16 plus `gross_by_hh_size`, `earned_by_hh_size`,
  `unearned_by_hh_size`, `rawgrinc_p`, `rawearn_p`, `rawunearn_p`,
  `rawrent_p`, `rawmedded_p`, `rawcsded_p`, `rawdepded_p`. Confirmed by Eric
  2026-08-08. Excluded by Eric's sign-off the same day: `unc_fsben_rel_max`
  (leakage pair with `unc_rawben_rel_max`: "ratios differ" has precision
  0.244 vs 0.010 and captures 94.7% of errors — the review outcome, not a
  household trait), `at_max_ben` (re-expression of `unc_rawben_rel_max > 1`),
  `second_element_i` (hazard row), `smd_amt`/`medicare_part_b_premium`
  lookups.

**Held fixed, both arms, all seeds:**

- Frame: `reg_model_data.rds` rebuilt 2026-08-08 on the merged munging script
  (231,619 rows; Ben's MFIP/SSI-CAP exclusions, rawusize!=0 drop, $1-step /
  $0-tolerance recreation). Both arms mine the SAME frame, so the frame
  change is not in the contrast.
- Engines and params as shipped: xgboost nrounds 1000 / eta 0.02 / subsample
  0.20 / depth 4 + ranger 1000 trees / mtry 2 / depth 4 (findings 4-5).
- Strata 1 / 2-3 / 4+ (findings 11); admission BH FDR 10% vs stratum base
  rate AND n >= 30 (findings 19); ordering one-sided 99% Wilson LCB, pooled
  national scale, seed-independent tie-break (findings 20; §31 machinery).
- Walk and readout: the §31 budget-readout machinery verbatim — top-20,000
  window (findings 27 slack-zero certificate; slack asserted), fill on the
  state's FY2022-23 caseload to core + 3x buffer, freeze, walk against
  FY2024 to that year's cap, LEGACY walk (see deviations).
- Budgets 5% and 10%; target panel = the §30 run-2 / §31 ten states
  (California, Texas, Michigan, Massachusetts, Arizona, Washington,
  Louisiana, Maine, New Jersey, Mississippi).
- Seeds paired across arms: 117, 20260805, 31415 (§31's seeds). Six mines.
- Percentile features: re-fit on FY2022-23 only and FROZEN (per-cell
  state × reported-HH-size empirical CDFs of CPI-deflated values, zeros
  pinned to 0, ranked among non-zero; Ben's construction with the train-only
  fit), applied unchanged to FY2024. The frame's as-built `_p` columns rank
  across all years and are overwritten for this study; the leakage fix was
  flagged to Eric 2026-08-08 ("got it").

## 3. Support after the split (rows AND events per unit), computed

Computed on the rebuilt frame 2026-08-08 (asserted at runtime):

| unit | rows | any-error events |
|---|---|---|
| train FY2022-23, total | 76,031 | 8,397 |
| train stratum 1 | 45,165 | 3,423 |
| train stratum 2-3 | 20,162 | 2,898 |
| train stratum 4+ | 10,704 | 2,076 |
| test FY2024 | 39,528 | 4,764 |

No split beyond the shipped strata; the n >= 30 admission floor is intact.
National scale throughout — the state-scale collapse hazard (CLAUDE.md,
Virginia 2026-07-06) does not apply.

## 4. What the record already says (cited)

- Findings 19 (settled): BH + n >= 30 admission — used as shipped.
- Findings 20 (settled, two eras): 99% LCB ordering — used as shipped.
- Findings 31 (yardstick): seed variation alone gives median pairwise
  errors-caught Jaccard 0.531 at the 5% budget / 0.666 at 10% on this exact
  walk and panel; deltas smaller than seed noise cannot be credited to the
  vocabulary. Three-seed discipline adopted from it.
- Findings 27: the 20k window carried slack zero in all 252 prior pools —
  asserted, not judged (engineering-artifacts rule).
- Findings 28: artifact-band exposure (share of delivered flags with
  `rawben_rel_max` in [0.987, 1)) reported as a companion; the rebuilt
  frame already thinned the band (errors in band 537 -> 227).
- Findings 33-34: the SHIPPED walk carries the fresh-share floor; see
  deviation (a).
- Hazard rows respected: `second_element_i` excluded; FY2020/21 excluded;
  frame written by the munging script only (rebuilt today, backup at
  `archive_data/reg_model_data_pre_benmerge_2026-08-08.rds`).
- No determinism anchor is available: the §26 anchor artifacts are old-frame.
  Substitute: the frame invariants above are hard assertions, and the
  baseline arm becomes the new-frame reference.

## Deviations decided without Eric tonight (delegated)

(a) **Legacy §31 walk, not the v2.4.0 fresh-share walk.** The seed-noise
yardstick (§31) was measured on this walk; using it keeps the noise
reference like-for-like. The walk is identical across arms, so the
vocabulary contrast is unaffected. The winning vocabulary's full 49-state
scorecard under the shipped fresh-share walk is the follow-up, not tonight.

(b) **Ten-state panel, not 49.** Walk evaluation on 49 states across 6
pools adds ~5h; the ten-state panel is the pre-established §30/§31 panel
and keeps the run inside the overnight window.

(c) **Percentile source NAs are a hard stop**, not an imputation decision
made silently at 2am.

(d) **Pre-registered readout.** Decision statistic: within-state MEDIAN
paired delta (candidate − baseline, same seed) in FY2024 precision at the
5% budget, averaged over seeds, across the panel. Companions (mandatory):
within-state MEAN and HARMED-TAIL count (paired delta < −0.05), plus dollar
recall at both budgets, plus the within-arm across-seed spread as the noise
reference. The candidate vocabulary is recommended for the v2.5.0 re-mine
if the median delta is >= 0 at the 5% budget, not contradicted by both
companions, and new-vocabulary rules actually appear in the deployed lists
(usage table). Anything else -> report as measured; Eric arbitrates
tomorrow.

## Mechanics

Script `methods/vocab_attribution_v2.R` (adapted from
`methods/seed_stability_v2.R`, the §31 machinery), runner
`runners/run_vocab_attribution.R`. Mines checkpoint per arm × seed to
`methods/vocab_attribution_v2/cache/` (gitignored .rds);
`RESUME_FROM_CHECKPOINT` supported. Heavy scoring through
`reduce_flags_for_rules` (OOM hazard row). Outputs (CSVs) ->
`methods/vocab_attribution_v2/`. No writes to `state_delivery_lists/`, no
CHANGELOG entry, no version bump. Estimated 6 mines x ~45-60 min + panel
walks ≈ 7-8 h.
