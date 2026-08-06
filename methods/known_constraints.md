# Known constraints for the pipeline files

Operational rules derived from measured findings. Each line carries its source:
`§N` is a section of `methods/modeling_findings.md` (and its detailed
companion); other sources are named. The findings docs own the evidence; this
file is the distillate that must be in front of anyone editing these files.
A hook in `.claude/settings.json` injects the matching section whenever one of
these files is edited in Claude Code; the header of each protected file points
here. Update this file through the `log-finding` skill when a new finding adds
or changes a constraint.

## rule_mining_helpers.R {#rule-mining-helpers}

- Never rank or shortlist rules on raw training precision or on holdout
  performance. Ordering is the one-sided 99% Wilson LCB of train precision
  (`wilson_lcb()`, `LCB_Z = 2.326`), settled on two eras (§1, §20, §22).
- Admission is Benjamini-Hochberg at FDR 10% vs the stratum base rate AND
  n >= 30 flagged training cases. The two guards do different jobs; neither
  replaces the other, and floorless admission is refuted (§19, §26).
- Dedup deliberately keeps overlapping rules of different structure (states
  want substitutes). Never re-prune a pooled rule set with a joint lasso
  (CLAUDE.md, design decision).
- `flags_for_rules()` OOMs this 29 GB host on national-scale pools. Heavy
  callers must go through `reduce_flags_for_rules()`, the chunked reducer
  (RESUME.md, 2026-07-22 builder fix).
- After ANY change to this file, run
  `Rscript methods/test_rule_mining_helpers.R` and get 27 of 27 PASS.

## INCL_find_inclusion_rules_by_hh_size_v2.R {#incl-finder}

- Strata are coarse household size 1 / 2-3 / 4+ from `cert_HH_size_FS_n`;
  finer splits add compute, not signal (§11). elderly/disabled is a feature,
  not a stratum (§8).
- `second_element_i` must never enter the feature set; state reporting of it
  is inconsistent (CLAUDE.md, 2026-07-07 frame rebuild).
- This script calls the flag evaluator 3x per frame x 5 frames x 3 strata.
  Before any heavy regeneration it must use the chunked reducer or it OOMs
  (RESUME.md A1-F1 caveat).
- The delivery vocabulary is the any-error frame; typed frames are mined for
  research completeness, not delivery (§17).
- Mines checkpoint to `.rds`; honor `RESUME_FROM_CHECKPOINT` rather than
  re-mining from scratch.

## EXCL_find_exclusion_rules_by_hh_size_v2.R {#excl-finder}

- Exclusion settings: 95% Wilson LCB on the clean rate, support floor 25,
  and an excluded pocket may carry at most 1/5 of its stratum's base error
  rate (§23).
- Validation depth is a single holdout year (2023). Do not describe or treat
  the exclusion list as validated like the inclusion deliverable (§23).
- Same OOM constraint as the inclusion finder: chunked reducer before any
  heavy regeneration (RESUME.md A1-F1 caveat).

## INCL_build_blended_delivery_list_v2.R {#delivery-builder}

- The deliverable is the blended frozen list: state and national rules on one
  99%-LCB scale, core filled to the review budget, buffer to 3x depth, walked
  in rank order outcome-free (§15, §16).
- Window pruning must keep the slack-zero check. Never cap the pool at a
  fixed rank as policy: the median state's core alone reaches rank 969 at the
  10% budget (§27).
- The national pool must be scored through `reduce_flags_for_rules()`
  (RESUME.md, 2026-07-22; the unpatched path OOM'd this host).
- `state_delivery_lists/` is a public, user-consumed artifact. Any schema,
  filename, or content change is a MINOR version bump and Eric's decision;
  present it, do not push it (VERSIONING.md; CLAUDE.md).

## 1_data_munging_and_raw_variable_reconstruction_for_using_public_qc_data.R {#munging}

- `reg_model_data.rds` is saved by this script and only this script. Never
  hand-build or hand-save the frame: a stale hand-built frame silently
  excluded ~31% of error cases for weeks (§10; CLAUDE.md).
- FY2020 and FY2021 stay excluded (decision, not measurement), and the
  benefit-reconciliation filter stays on; the exclusions are validity guards,
  additive-only on the six kept years (§24).
- Multi-element error cases are KEPT; deduction-field NAs are zero-filled
  (`ded_fields_imputed`), not dropped (§10).
- Paths resolve through `here()`; no hardcoded machine paths
  (RESUME.md, 2026-07-27 merge).

## methods/add_refill_metrics_v2.R {#refill-metrics}

- The stratum comes from `cert_HH_size_FS_n` via `hh_group_of` (<=1, <=3,
  else 4+). Using `HH_size_n` is wrong (handoff 2026-08-04; Michigan
  reproduction).
- Rule strings evaluate with NA comparisons as FALSE, matching the R flag
  evaluator.
- The refill walk is core then buffer in rank order, capacity
  `floor(budget * n_rows)`. Assert results against
  `methods/anyerror_blended_holdout_2024/holdout_metrics.json`; Michigan at
  the 10% budget must reproduce 19 rules, 86 cases, 24 errors, precision
  0.2791 (handoff 2026-08-04).

## New study scripts under methods/ or runners/ {#new-study}

- Read `methods/findings_ledger.md` before designing; check the retired and
  hazard rows for every component the study touches.
- The four-item design note goes to Eric before any run that costs a night:
  the question in one sentence; what varies with exactly one component
  varying; support after the split (rows AND events per unit), computed; what
  the ledger and findings already say, cited.
- A 50/50 split is not required for out-of-fold scoring: K-fold mines on
  (K-1)/K of the data. Halving state pools to 48-140 errors is what
  invalidated the 2026-08-04 cross-fit study (RESUME.md; §30).
- Evaluate at review budgets (5% / 10% of caseload) as well as filter floors
  (§12, §14), and quote any-error precision beside frame-relative (§6).
- Study outputs stay in `methods/`: no writes to `state_delivery_lists/`, no
  CHANGELOG entry, no version bump. Promotion is Eric's decision
  (VERSIONING.md).
