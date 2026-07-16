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

Status at write time:
- Layer 2 audition running: `neardup_collapse_sweep_v2.R` (does
  support-preferring collapse close the five-frame gap? does the deployed-rule
  autopsy confirm displacement?).
- Layer 1 + 3 audition written: `estimation_admission_sweep_v2.R` (EB ranking
  x FDR admission, composed with layer 2's winning collapse setting).
- Published delivery lists stay as-is until the audited architecture settles;
  no rebuild in between, to avoid shipping the folder twice.
