# Design note: EXPLORATORY state-scale percentile runoff (2026-08-11)

The new S37. Every design decision below was made interactively on
2026-08-11 (this note records them; it is not a proposal). EXPLORATORY per
the formality-proportionality rule: no pre-registered bars, no winner rule,
no ledger rows; the readout goes to the project lead with the standard companions and
any vocabulary decision is his. Replaces the deleted 2026-08-10 state
re-mine, whose percentile arm used the frozen train-only construction and
therefore did not test the intended design.

## 1. The question (one sentence)

Are within-state percentiles - the as-built `_p` features (features.R:
CPI-deflated dollars ranked within state x household-size cells, all six
frame years pooled: 2017-19 and 2022-24, zeros pinned to 0) - better than
the per-size variables for identifying outliers, measured as delivered
within-state FY2024 budget-list performance?

## 2. What varies (exactly one component: the added feature family)

- **persize**: the 16 shipped-in-practice features + the 3 per-size income
  features (19). (The 16 already carries 2 per-size fields.)
- **benp**: persize + the 7 as-built percentile features (26). ADDITIVE
  by decision; only the as-built variables; NO percentile override
  anywhere - the frame's columns are used as built.

Held fixed: rebuilt 2026-08-08 frame (231,619 rows, asserted); any_error
frame x coarse HH strata (1 / 2-3 / 4+) mined PER STATE on FY2022-23;
shipped engines (xgboost 1000/0.02/0.20/depth4 + ranger 1000/mtry2);
admission ONE joint BH FDR 10% across the state's candidates with
per-stratum base rates in the p-values (the 2026-08-09 review catch) AND
n >= 30; 99%-LCB ordering; the SHIPPED v2.4.0 fresh-share walk (f = 0.50,
walk2 semantics with the capacity assertion), fill FY2022-23 to 5%/10%
core + 3x buffer, freeze, walk FY2024 in delivered order to that year's
cap, outcome-free; seed 117, single seed (by decision; the paired
49-state design averages state noise, NOT mining noise - a standing
reading limit).

## 3. Support after the split (computed; per-state table prints at runtime)

Any_error x stratum at state scale: roughly 500-800 rows and 50-100
any-error events per state x stratum on the public FY2022-23 file (the
prior run's printed tables). The n >= 30 floor and joint BH are expected
to leave thin states admitting little; Wyoming admitted nothing under
either vocabulary last time. The readout reports the paired-state count
and per-arm states-admitting counts.

## 4. What the record says (cited)

- Findings 19 (settled): BH + n >= 30, as shipped; joint BH per the
  2026-08-09 review catch (ledger Admission hazard row).
- Findings 20 (settled, two eras): 99%-LCB ordering, as shipped.
- Findings 33-34 (settled, two eras): the fresh-share walk at f = 0.50 is
  the shipped walk; the capacity assertion is a construction property
  (engineering-artifacts rule).
- Virginia hazard: single-state mining collapses without the n >= 30
  floor - the floor is on.
- §§35-36 (EXPLORATORY): tested the FROZEN percentile construction, not
  the pooled-years construction; nothing there
  pre-judges this run.
- Findings 31 / §36: state-level readings sit inside mining seed noise;
  aggregates are the readable layer. Single seed is accepted as an
  exploratory limit by decision.

## Pre-stated reading limits (encoded in the script header and readout)

1. **Information asymmetry**: the pooled fit includes FY2024, so benp
   train features carry (weak) test-year information persize's do not.
   Faithful to deployment-on-current-caseload; a small benp win could be
   the pooling rather than the representation. Standing limit, not a
   discovery.
2. **"Outliers" vs what is measured**: the readout is budget precision;
   the flag-profile layer (where benp's flagged cases sit on the
   percentile scales vs the caseload and vs error cases, at >p90 / >p99)
   reports what percentile rules actually catch. The senior-statistician
   review is asked to shape this layer (by instruction).
3. **Small-cell degeneracy is a property of the construction at public
   state scale**, quantified in
   `methods/state_percentile_runoff_v2/percentile_value_map_fy2024.csv`
   (n_distinct per state x size x variable, requested 2026-08-11). A null is "the
   construction did not help HERE", not "the idea is wrong".
4. **Redistribution pre-named**: flat median + large two-sided tails is
   described as redistribution, not equivalence (the §35 lesson).
   Companions: median, mean, harmed tail (< -0.05), helped (> +0.05),
   paired-state count, dollar recall, absolute levels, base rates,
   deployed `_p`-rule counts.
5. **No winner automation**: the script selects nothing; the project lead decides on
   the readout (the deleted run's winner rule is how a wrong construction
   reached production).

## Addendum (2026-08-11, post-review, pre-launch)

Fresh senior-statistician review returned REVISE; all blocking items fixed
before launch:

- **B1 (bug, caught in review):** the evaluation walk passed
  `list(all = ...)` strata while rules are mined and admitted per HH
  stratum, so `flags_for_rules()` silently applied stratum rules
  UNRESTRICTED. Fixed: evaluation strata now mirror the mining strata on
  both train and test (the shipped builder's semantics). Mining caches
  unaffected.
- **B2 (walk deviation, found by the smoke run):** the shipped walk's
  exact-refill capacity assertion is an empirical property of large
  blended pools (98/98), NOT a guarantee; at state-pool scale the
  completion pass stranded a 1-case remainder on the first smoke state.
  Reviewed treatment: tolerate and REPORT the shortfall, never modify the
  walk (an overshooting rule would break the findings 33-34
  equal-workload construction) and never crash (engineering-artifacts
  rule). Section 2's "capacity assertion" language is superseded by this
  addendum.
- **B3:** the gap is reported PER PHASE (`fill_gap_core`,
  `fill_gap_total`, with `fill_cases`) because a phase-1 gap moves the
  core/buffer boundary and hence the delivered order; per-arm gap
  summaries print in the log.
- **Gap reading rule (exploratory, pre-stated):** per-arm max gaps of a
  few cases (< 1% of the fill target) carry no reading; a systematic
  per-arm difference is itself a legitimate exploratory observation about
  the walk at state-pool scale - reported, not judged.
- **Flag-profile layer:** the reviewer's proposed design is adopted in
  full (condition inventory, p-rule catch, per-variable profile with
  pinned-zero mass and direction, incremental catch vs the persize arm).
  Interpretation hazard carried in the readout notes: `_p` conditions are
  conjoined with other conditions, so "flagged by a rule using `_p`" is
  rule-level, not causal, attribution.

## Mechanics

Driver `methods/state_percentile_runoff_v2.R` + runner
`runners/run_state_percentile_runoff.R`; cache per state x arm
(`ae_<state>_<arm>_117.rds`) under `methods/state_percentile_runoff_v2/cache/`;
`RESUME_FROM_CHECKPOINT` honored. SMOKE=1: 3 states, tiny ensembles, own
subdir. Estimated 2-4h mining (2 arms x 49 states x any-error strata) +
~30-60 min walks/readout. Outputs (CSVs) ->
`methods/state_percentile_runoff_v2/`. No writes to
`state_delivery_lists/`, no CHANGELOG entry, no version bump.
