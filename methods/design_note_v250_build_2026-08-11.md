# Design note: v2.5.0 CANDIDATE build, staged (2026-08-11, 22:00 launch)

Not a study: a PRODUCTION-CANDIDATE build, staged for Eric's review. All
decisions below are Eric's, made interactively 2026-08-11 (train years,
state-side scope, and staging destination confirmed by explicit ruling).
No pre-registered bars; no findings entry until Eric reviews the lists.

## 1. What is built (one sentence)

For each of the 49 states, a blended frozen delivery list (5% and 10%
budgets, core + 3x buffer) from the per-size 19-variable vocabulary:
the state's own any-error rules blended with the national any-error rules
on one 99%-LCB scale, with the section-29 characterization columns joined
onto every list.

## 2. The recipe, component by component (nothing varies; this is a build)

- Train years: ALL public years FY2022-24, fresh mines, no holdout - the
  shipped delivery recipe (findings 15-16; expected performance is the
  one-year-ahead benchmark). 115,559 rows / 13,161 any-error events,
  asserted.
- Vocabulary: the per-size 19 (16 shipped-in-practice + gross/earned/
  unearned per size). Chosen by Eric with the exploratory record §§35-37
  in view (percentile additions a wash; frozen percentiles retired
  unadopted; the silent-drop hazard guarded by an explicit
  features-after-prep assertion).
- Mining: any_error frame x coarse HH strata (1 / 2-3 / 4+), one national
  mine + one mine per state; shipped engines (xgboost 1000/0.02/0.20/d4 +
  ranger 1000/mtry2/d4), seed 117.
- Admission: per mining unit, ONE joint BH at FDR 10% with per-stratum
  base rates inside the p-values (findings 19; 2026-08-09 review catch)
  AND n >= 30 (Virginia hazard). National pool scored through the chunked
  reducer (OOM constraint).
- Blend: national + own pool on the one LCB scale, sorted (-lcb, -n, hh,
  rule), dedup by (hh, rule) keeping the higher-LCB copy (shipped builder
  semantics). Known hazard acknowledged: one LCB scale spans a 115k-row
  and a ~2k-row search (rule-pool incomparability); this is the SHIPPED
  blend recipe (findings 16), unchanged here.
- Fill: the shipped fresh-share walk (f = 0.50), pass zero fixing C0/CT,
  two-phase re-walk, with the §37-machinery tolerated-gap treatment at
  state scale (gaps reported per phase in build_summary.csv, never a
  crash; the shipped builder's assertion is an empirical property of
  large pools). Outcome-free.
- Characterization: step 2 reuses the validated §29 machinery
  (methods/rule_error_profiles.py maps, load_variances, characterize) on
  the new rules via methods/v250_characterize_lists.py, reading a fresh
  full-precision frame export (the top-level reg_model_data.csv is STALE:
  it predates the 2026-08-08 rebuild and lacks the per-size features;
  reg_model_data.rds is the source of truth). Era: all_2022_24 pooled,
  as the shipped characterization sheet reports. Step 3 joins the full
  characterization row onto every list CSV by (hh, rule).
- Destination: methods/v250_candidate_lists/ - STAGED. No writes to
  state_delivery_lists/, no CHANGELOG entry, no version bump; promotion
  is Eric's decision after reviewing the lists (ask-before-user-facing
  rule).

## 2b. Recorded deviations from the shipped builder (review 2026-08-11)

Fresh senior-statistician review: REVISE -> fixed (the hand lists now carry
per-rule `engines` / `mined_frames` provenance; the smoke had shown the
national pool is ~50/50 ranger-only and xgboost-only rules, so constants
would have falsified the column). Two accepted, validity-neutral deviations
recorded for the promotion decision: (i) blend ordering/dedup runs on the
FULL-PRECISION LCB rather than the shipped builder's 4-decimal-rounded
column (more exact, deterministic; §20 validated the statistic, not the
rounding); (ii) pool-level `dedup_exact_coverage` / `dedup_dominated` are
not re-run on the blend - delivered membership is invariant (zero-add rules
are skipped by both walks); on exact-coverage ties this build delivers the
alphabetically-first rule text rather than the fewest-conditions variant.
Column order also differs (rank/role first) - a schema change to state
explicitly at promotion. Characterization on the lists is the CURATED
block (Eric's selection, 2026-08-11 evening): n_error_cases,
element_groups_to_75, nature_groups_to_75, found_in_case_record,
share_overissuance, timing_at_certification, cause_agency - 20 columns
per list in all (cause_client dropped as a near-complement of
cause_agency: median sum 0.968, correlation -0.982 on the smoke rules). The full ~150-column sheet with every share's
Wilson interval is written separately
(rule_characterization_v250.csv) for states that want to join the rest.
Step 3 merges the full sheet for its integrity guards (row-count, NA, and
the cross-language flag-count identity on national-pool rows - the
ULP-drift guard) before delivering the curated subset.

## 3. Support (computed)

National: 115,559 rows / 13,161 events across three strata (per-stratum
counts print at mine time; the 2022-24 strata are half again today's
2022-23 counts, all far above every floor). States: roughly 800-2,800
rows and 100-400 events per state across strata (today's per-state
prints, scaled by the added year); admission floors n >= 30 per rule.
Wyoming may admit nothing (consistent across every state-scale run);
its lists would then carry national rules only.

## 4. What the record says (cited)

Findings 15-16 (blend + frozen list, the deliverable), 19 (admission),
20 (LCB ordering), 33-34 (fresh-share walk, two-era validated), 27
(evaluation machinery; no evaluation walk runs tonight - no holdout
exists), §29 (characterization fields and their reliability), §§35-37
(exploratory vocabulary record), rule-pool incomparability (memory,
acknowledged above), OOM constraint (chunked reducer for the national
pool). Estimated 4-5h: national mine + admit ~1-1.5h, 49 state mines
~2h, blending/fill ~30min, characterization ~45-60min. Runner:
runners/run_v250_build.R, launched 22:00 via Task Scheduler; mines
checkpoint per unit and RESUME_FROM_CHECKPOINT is on.
