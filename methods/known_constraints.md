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
- The BH call is ONE joint pass across all of a frame's candidate rules,
  with each rule's stratum base rate inside its p-value. Running BH per
  stratum is a different multiplicity correction and admits a different
  set (smoke: 393 vs 436 rules on one state's any-error frame); it was
  caught in review before the state re-mine (§37).
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
- Verify every name in the `features` vector against the frame's columns
  before mining: `prep_features()` drops unknown names silently. The
  vector's three `raw*_by_hh_size` names never existed in the frame, so
  every v2 mine has used 16 features, not the listed 19 (§35).
- Vocabulary results (2026-08-09, one era, EXPLORATORY): replacing the
  per-size representation with FROZEN train-year percentiles (cutoffs fit
  on FY2022-23 only, applied unchanged to FY2024; defined in §35) cost a
  moderate, sign-consistent amount (mean negative in every seed, both
  budgets, ten evaluation states); `shelter_expenses_p` added nothing.
  Ben's pooled-years within-state percentile construction (features.R,
  the as-built `_p` columns) was NOT what those arms tested; §37 tested
  it 2026-08-11 (EXPLORATORY, 48 states, single seed): precision a wash
  at 5%, and of 495 deployed `_p` conditions only 2 are high-tail - the
  miner does not use the construction as an outlier detector. The
  per-size vocabulary (16 + gross/earned/unearned per size) is the
  v2.5.0 re-mine candidate, Eric's call at regen time (§35-§37).
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
  hand-build or hand-save the frame; one code path writes it (§10;
  CLAUDE.md).
- FY2020 and FY2021 stay excluded (decision, not measurement), and the
  benefit-reconciliation filter stays on; the exclusions are validity guards,
  additive-only on the six kept years (§24).
- Multi-element error cases are KEPT; deduction-field NAs are zero-filled
  (`ded_fields_imputed`), not dropped (§10).
- Paths resolve through `here()`; no hardcoded machine paths
  (RESUME.md, 2026-07-27 merge).
- **`reg_model_data.rds` is the source of truth; the CSV export is lossy**
  (15 significant digits, does not round-trip; Eric's ruling 2026-08-06).
  Threshold comparisons against `reg_model_data.csv` flipped rule flags on
  cases sitting 1-2 ULP from a rule literal, and pandas' default float
  parser lands 1 ULP low on many 17-digit decimals. Any consumer comparing
  thresholds must read the rds, or parse the CSV with
  `float_precision="round_trip"`, or take a `%.17g` export.

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

- Invoke the `principal-data-scientist` skill before writing the script;
  study scripts are written under that framing (routing rule below).
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
- **Mandatory shipping companions** (Eric, 2026-08-07): the pre-registered
  decision statistic stays the within-state MEDIAN, and every shipping
  readout must carry two companions: the within-state MEAN and the
  HARMED-TAIL count (states whose paired change is worse than -0.05). A
  median win contradicted by both companions does not ship. Origin: the
  0.60-vs-0.50 threshold adjudication (findings 34 addendum), where the
  median lens alone would have shipped a threshold that doubled the harmed
  tail on all six harness-budget readouts.
- **Engineering artifacts are not failure modes** (Eric, 2026-08-06): a
  pre-stated bar or guard must test the analytical question. Any outcome
  preventable by a design change or an incremental engineering parameter
  that does not overturn the approach under test is a design requirement
  (engineer it away, or assert it by construction), never a judged failure
  mode. A pre-registration that could close a line of work over such an
  artifact is an invalid design.
- Before the run launches, the script and its design note go to a fresh
  senior-statistician review (routing rule below).

## Routing: who writes, who reviews {#routing}

Adopted 2026-08-05. Two treatments, applied to three classes of work: new
study scripts under `methods/` or `runners/`, edits to the six protected
pipeline files above, and any script that precedes an overnight run.
Everything else (figure regens, doc tooling, one-off reshapes) runs without
this ceremony.

- **Written under the principal-data-scientist skill.** Invoke it before
  writing; it loads the ledger and the pipeline discipline into the session.
- **Reviewed by a fresh senior-statistician.** Before the run launches (for
  studies) or before the change is committed (for pipeline edits), spawn a
  SEPARATE agent with no shared conversation context. Give it the script or
  diff and the four-item design note, and instruct it to load the
  senior-statistician skill and `methods/findings_ledger.md`. It verifies:
  exactly one component varies; support after any split is computed (rows AND
  events per unit); no retired or hazard ledger row is re-opened; results
  will be read at review budgets with any-error metrics beside
  frame-relative. The reviewer is fresh-context on purpose: an author
  reviewing its own script inherits the assumptions that produced the
  mistake. The review verdict (approve, or revise with reasons and ledger
  citations) goes to Eric with the design note.
