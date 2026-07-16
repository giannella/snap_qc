# Design: the v3 selection architecture (decided 2026-07-15)

## Why

Two 2026-07 results forced this. (1) The five-frame (typed + pooled)
vocabulary — a 3.3x larger candidate pool — LOST budget-filled precision at
fixed stringency on the train-2022-23/test-2024 benchmark (median 0.306 vs
0.324 at the 5% budget; `blended_frozen_results_5frames.csv`), even though the
same vocabulary WINS at filter floors (findings #3). The ranking statistic,
not the vocabulary, mis-prices candidates at the top of the list. (2) The
stringency sweep (`stringency_vocabulary_sweep.csv`) showed the fixed
`LCB_Z = 2.326` is itself a hidden calibration to ~48k candidates: raising z
improved even the production vocabulary, and no fixed z rescued the larger
pool. Hand-tuned constants do not survive changes in pool size — and states
running this code on internal data change the pool size every time.

Product framing (2026-07-15 discussion): the deliverable is not a rule list
but calibrated, selection-corrected per-rule statistics with honest
uncertainty. Budget-filled precision/dollar-recall is ONE consumption mode;
threshold-and-staff, exclusion, and audit triage are others. The machinery
must be data-agnostic (states swap `features` and `TARGET_IS_ERROR`),
self-calibrating (no magic constants), and fail-conservative in unfamiliar
hands.

## The four layers

1. **Admission — FDR, not a fixed z.** Benjamini-Hochberg (optionally
   IHW-weighted by support or mining frame) against each stratum's base rate.
   Self-calibrates to candidate count and signal density; the parameter
   ("at most X% of admitted rules are flukes") is meaningful to a state
   analyst. The Wilson LCB remains a REPORTED column.
2. **Evidence structure — near-duplicate families.** Rules whose train
   coverage is nearly identical are one pattern rendered many ways; treating
   renderings as independent evidence is wrong in every use mode. One
   COMPETITOR per family in the ranking — the highest-support member, an
   outcome-free selection that adds no curse — but members are NOT deleted
   from the deliverable: they ship as labeled substitutes (`family_id`
   column) attached to their representative, preserving the
   experts-drop-rules-and-need-alternates design principle (decided
   2026-07-15). Cross-engine / cross-frame provenance (`engines`,
   `mined_frames`) and family size serve as zero-compute stability signals
   (Meinshausen-Buhlmann by construction); family size feeds layer 3's prior.
   Refinements on the roadmap, not blocking: union-coverage estimation for
   representatives; hierarchical partial pooling for looser families
   (J ~ 0.6-0.8), where collapse would be wrong.
3. **Estimation — empirical-Bayes shrinkage.** Beta-binomial prior fit to the
   pool per stratum; rank and report posterior means/quantiles. Adaptive to
   any state's data; degrades conservatively (over-shrinks) rather than
   invalidly. A posterior for error DOLLARS is a planned alternative ranking
   objective. Conditional (selective) MLE is kept as a diagnostic
   cross-check, not the default (our compound selection event makes analytic
   conditioning fragile).
4. **Certification — assumption-free checks around whatever ships.**
   Case-level cross-fitting at national scale per release (honest standard
   errors); bootstrap optimism of the full select-rank-fill pipeline reported
   next to every deliverable (replaces the folklore "~1/3 deflation");
   era-swap validation (2017-19 as the independent era) for methodology
   changes.

## Adoption discipline

Layers audition ONE AT A TIME in the 2024 exploration harness
(`stringency_vocabulary_sweep_v2.R` lineage), each with a pre-registered
expectation; anything adopted must then pass the honest designs (national
2022/2023 tuning split; 2017-19 era check). Failures are retired in writing
in `modeling_findings.md`, like the subsampling claim.

If adoption replaces the 99% Wilson LCB as the ranking statistic, update
the README pipeline figure (`methods/draw_pipeline_options.R` ->
`presentation_figures/pipeline_option_B.png`) — its confidence-scale box
names the bound explicitly.

## Audition outcomes (2026-07-15/16, all exploratory on 2024)

- **Layer 2 (families/collapse)**: mechanism CONFIRMED — the deployed-rule
  autopsy showed the five-frame walk deploying median-support-59 rules with
  39pp train->2024 deflation vs support-77 / 22pp for the any-error pool;
  collapse (J=0.95) normalized both (support 81, deflation 23pp) and lifted
  the five-frame 5%-budget median 0.306 -> 0.319. But it did NOT beat the
  production baseline (0.324) and HURT the production pool at 5%. Not adopted
  as a ranking step; the `family_id` substitutes column remains a planned
  deliverable feature. (`neardup_collapse_sweep.csv`)
- **Layer 3 (EB shrinkage ranking)**: REFUTED in the simple per-stratum
  beta-binomial form — posterior-mean ranking degraded the production pool's
  5% median to 0.259-0.293 vs 0.324 for the Wilson LCB; the posterior 5%
  quantile tracked the LCB slightly worse everywhere.
  (`estimation_admission_sweep.csv`)
- **Layer 1 (FDR admission)**: audition INVALID as designed — the cached
  pools were already support/base-rate filtered, so BH admission was a no-op
  (identical medians across admission arms). A true test must replace the
  build-time filter, which requires caching the raw unfiltered vocabulary.
  Roadmap, together with the z(N) era validation (2017-19).
- **Net decision**: the production recipe (any-error vocabulary, Wilson LCB
  ranking, no collapse) stays; the provenance schema is adopted; the typed
  vocabulary is retired at delivery scale after three failed rescue attempts
  (stringency, collapse, shrinkage). Delivery lists rebuilt on this settled
  recipe 2026-07-16.

## Revision (2026-07-16): the goal-agnostic core

The EB audit's deeper lesson: ranking by an average-calibrated estimate lost
because budget-fill is a TAIL decision — there is no single best estimate,
only a best statistic for a given decision. The architecture therefore
separates:

- **Evidence core (invariant)**: per-rule counts, coverage, families,
  provenance, calibrated uncertainty; capacity-matched evaluation on unjudged
  years; assumption-free certification. Goal-independent, column-agnostic.
- **Statistic-goal module (user-chosen)**: a ranking statistic paired with
  the goal metric it is judged by. Delivery filenames carry the pairing label
  (`lcb99_workloadfill` today); the exclusion pipeline's clean-rate LCB is a
  second, pre-existing pairing. New pairings audition in the harness against
  their own goal metric and pass the honest designs before shipping.

Opening measurement for the dollar goal (dollar_persistence_check_v2.R,
train 2022-23 pools scored per state on 2024, 169k rule-state pairs with
n_train >= 30, n_test >= 10): per-rule dollars-per-flag persists train->test
MORE strongly than precision in every support band — Spearman 0.560 / 0.699 /
0.789 / 0.677 (bands 30-60 / 61-120 / 121-300 / 300+) vs 0.497 / 0.634 /
0.708 / 0.673 for precision. Error magnitude is anchored to observable case
characteristics (benefit levels, household size), so a dollar-yield statistic
needs less shrinkage than feared. Next: audition a dollar-LCB ranking against
dollars-at-capacity in the goal-parametric harness.
