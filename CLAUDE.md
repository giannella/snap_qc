# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Purpose

R codebase for modeling SNAP (food stamp) payment errors. It mines interpretable decision rules from public USDA SNAP Quality Control (QC) data to help state agencies either:
- **Include / flag** high-risk cases for review when no prior flagging system exists, or
- **Exclude** low-risk cases from an existing review pile.

The repo contains two generations. **v2 (current, recommended)** mines rules with xgboost + ranger and filters them on a Wilson lower confidence bound; work on it by default. **v1 (legacy, {pre}/RuleFit-based)** is preserved unchanged for state agencies already using it — do not rename, move, or alter v1 scripts or their output folders without explicit instruction. v1 scripts carry a two-line breadcrumb header pointing to their v2 successor.

## Running the Code

Scripts are run interactively in R (RStudio) or non-interactively via the `run_*.R` runner pattern:

```
"C:\Program Files\R\R-4.5.1\bin\Rscript.exe" runners/run_incl_v2.R > incl_v2_run.log 2>&1
```

Runners load `reg_model_data.rds` (a saved copy of the main modelling frame, expected in the repo root) then source the driver. There is no package or build system. **After changing `rule_mining_helpers.R`, always run the regression test**: `Rscript methods/test_rule_mining_helpers.R` (29 checks, all must PASS).

Key v2 packages: `dplyr`, `ggplot2`, `ranger`, `xgboost` (plus `rpart` for the optional bagged-CART engine). v1 additionally needs `pre`. Parse-check scripts with `Rscript -e "invisible(parse('file.R'))"` before running.

## Data

Public SNAP QC `.sav` files are expected at `qc_data/` under the repo root; auxiliary lookups live in `additional_data/`. The munging script resolves both through `here()`, so it needs the `here` package and no hardcoded paths (it read from a separate `C:/Users/ericg/qc/` checkout until 2026-07-26; that copy is byte-identical and is no longer used by this repo). The main modelling frame `reg_model_data` is built by `1_data_munging_and_raw_variable_reconstruction_for_using_public_qc_data.R` (shared by v1 and v2) and cached as `reg_model_data.rds`.

`error_status` values: `earned_overissuance`, `unearned_overissuance`, `underissuance`, `other_error`, `no_error`. A case is an error when `over_threshold != 0` (aligned 1:1 with error_status). Base rates are low: ~11% of cases have any over-threshold error (post-2026-07-07 rebuild; was ~8.4% on the earlier single-element frame); typed frames run 0.4-6% by stratum. `other_error` is the LARGEST error category and is mined in v2 (it never was in v1).

**Frame provenance (2026-07-07)**: `reg_model_data.rds` is saved by the munging script itself (never hand-built; one code path writes the frame). Multi-element cases are KEPT since the 2026-07-07 rebuild — earlier frames excluded them, ~31% of errors (`second_element_i` flags them; do NOT use as a mining feature — state reporting is inconsistent). Deduction-field NAs are zero-filled (`ded_fields_imputed`), not dropped. Results predating the rebuild live in `run*_singleelement_frame/` archive subfolders; rule-content changes across data revisions are diffed with `methods/compare_rule_sets_v2.R`. Per-state error visibility (public data misses all ineligible cases): `methods/state_error_accounting/`.

## v2 Architecture

All logic lives in **`rule_mining_helpers.R`** as a five-stage pipeline; every driver is a thin config + orchestration script:

```
generate -> canonicalize -> dedup -> evaluate -> sweep / shortlist
```

- **generate**: `generate_rules_xgboost()` / `generate_rules_ranger()` / `generate_rules_rpart()` — every tree node's root-to-node path becomes a candidate rule. Features must be numeric/logical/2-level (see `prep_features()`).
- **canonicalize**: 3-signif-digit thresholds, collapsed bounds, canonical ordering.
- **dedup**: exact text -> exact coverage -> same-structure dominance (drop a rule only when a looser same-shape rule has an equal-or-better statistic). Overlapping rules with different structure are DELIBERATELY kept — states drop rules on expert judgment and want substitutes. Never re-prune with a joint lasso.
- **evaluate**: sparse flag index vectors built from a unique-conditions table (memory stays flat at 100k+ rules); every rule scored on train, holdout, and the any-error universe.
- **filter/sweep**: Wilson LCB of train precision (`wilson_lcb()`), then `precision_sweep()` reports the union's holdout precision/recall/dollar-recall per filter floor. An error caught by several rules counts once. There are NO greedy nets in v2.

### Validated settings and principles (2026-07 studies; see methods/modeling_findings.md)

- Engines: xgboost (nrounds 1000, eta 0.02, subsample 0.20) + ranger (1000 trees, mtry 2), depth 4. The PAIR beats either alone and beats rpart+ranger.
- **"Mine big, filter stringently"**: big ensembles extend recall reach; `LCB_Z = 2.326` (99%) removes their selection-multiplicity noise. 90% (1.2816) for exploration only.
- Delivery-builder admission (since v2.3.0, findings 19): Benjamini-Hochberg at FDR 10% vs the stratum base rate AND `n >= 30`. Floorless BH is refuted (the floor is an estimation-quality guard, not a discovery guard); `ADMISSION <- "legacy"` restores the old raw-precision filter. Ordering stays the 99% Wilson LCB — vindicated on two eras (findings 20); shrinkage ordering and the typed delivery vocabulary are retired (findings 17-18).
- Raw train precision suffers a strong winner's curse (nominal 0.20 -> ~0.10 holdout); the LCB fixes calibration. Never shortlist on raw precision or on holdout performance.
- Strata: household size 1 / 2-3 / 4+ (`cert_HH_size_FS_n` collapsed). Year-dependent margins (2023: pooling matched precision, split won reach; 2024: split won precision +4pp, pooling won floor-reach) but the coarse split never loses - it stays the default. 5-way adds nothing over 3-way at ~1.6x compute (2024 re-test; findings 11).
- elderly/disabled: a FEATURE, not a stratum (settled empirically — the ensembles carve the caseload themselves).
- Frame-relative precision understates deployed precision ~2x; always compute and quote any-error metrics.
- States (deployment recipe as of 2026-07-10, findings 14-16): the default deliverable is the BLENDED FROZEN LIST — the state's own mined pool merged into the national pool on the 99%-LCB scale, filled against the state's own caseload to the review budget (core) plus buffer rules to 3x depth, walked in rank order until capacity fits (outcome-free). Built by `INCL_build_blended_delivery_list_v2.R`; the lists go in tracked, public `state_delivery_lists/` (batch-built from public data — the custom_one_off rule below does not apply to them). The ranking statistic + goal metric are a user-chosen module on the shared evidence core (README "Statistics and goal metrics"); the default pairing (`lcb99_workloadfill`) keeps plain filenames, non-default pairings carry their label in the filename. Per-state re-filtering/tuning did NOT beat the national ordering for the median state on a true future-year test; the state's own-pool list is a FALLBACK judged by the state's internal validation (public files show only 43-81% of each state's error cases). Small-sample tuning remains winner's-curse territory — the n >= 30 support floor stands. Evaluate at review budgets (5%/10% of caseload), not just filter floors.

### v2 scripts

| script | role |
|---|---|
| `rule_mining_helpers.R` + `methods/test_rule_mining_helpers.R` | shared pipeline + regression test |
| `INCL_find_inclusion_rules_by_hh_size_v2.R` | inclusion rules per mining frame (4 typed + pooled any_error) x stratum -> `inclusion_rules_by_hh_size_v2/` |
| `EXCL_find_exclusion_rules_by_hh_size_v2.R` | exclusion rules (clean-rate LCB; workload cut vs dollar retention) -> `exclusion_rules_by_hh_size_v2/` |
| `INCL_build_blended_delivery_list_v2.R` (+ `runners/run_blended_delivery_batch.R`) | the deployment deliverable: blended frozen list per state -> `state_delivery_lists/` (NB its `POOL_CACHE` dir holds STALE pre-v2.5 pools; the current pool artifact is `state_delivery_lists/national_rule_pool_2022_2024_v250.rds`) |
| `INCL_mine_internal_and_blend_with_national_v2.R` | issue-#14 mechanics (added 2026-08-18): mines a state's INTERNAL case file with the shipped recipe and blends with the published national pool artifact; no public frame needed; validity judged only by the state's held-out internal period |
| `state_threshold_gridsearch_v2.R` | per-state threshold tuning + holdout test + national-as-is benchmark (superseded fallback; committed outputs archived in `archive/state_rules_v2/`) |
| `methods/tune_engine_params_v2.R`, `methods/tune_followup_subsample_lcbz_v2.R` | hyperparameter + LCB_Z sweeps -> `methods/parameter_tuning_v2/` |
| `methods/compare_engines_v2.R`, `methods/compare_engine_combos_v2.R` | engine studies -> `methods/compare_engines_v2/` |
| `methods/compare_anyerror_vs_typed_frames_v2.R` | typed vs pooled-target mining -> `methods/compare_anyerror_vs_typed_v2/` |
| `methods/compare_hh_strata_v2.R` | stratification schemes -> `methods/compare_hh_strata_v2/` |
| `methods/check_esap_coverage_v2.R` | elderly/disabled coverage parity check |
| `visualize_state_regression_trees_predicting_dollar_amounts.R` | exploration, NOT rule mining: one `rpart` tree on `total_error_amount` per state -> `state_income_error_trees_any_timeper/` |
| `visualize_national_regression_trees_by_type_of_error.R` | exploration, NOT rule mining: one `rpart` tree on `total_error_amount` per error type -> `income_error_trees/` |
| `tree_visualization_helpers/` | the two scripts above: fitting + split/error-capture metrics (`regression_tree_functions.R`) and the `rpart.plot` wrappers (`regression_tree_plots_simplified.R`) |

Long-running scripts checkpoint mined vocabularies to `.rds` and support `RESUME_FROM_CHECKPOINT` (pre-set it in a runner before `source()`). Comparison outputs from superseded configurations are kept in suffixed/archived copies (e.g., `run1_*` subfolders) — don't delete them.

The two tree-visualization scripts are for looking at where error dollars concentrate; they feed no rule list and share nothing with `rule_mining_helpers.R`. They need `rpart.plot` on top of the pipeline packages, expect `reg_model_data` in the environment, and write PNG plus PDF into the two output folders. The state script's `states` vector is overridden near the loop to a single state (Washington); set it to `unique(reg_model_data$state)` for the full 49-state set. Their committed PNG/PDF outputs were built on the pre-2026-07-07 frame, so a rerun today will not reproduce them byte-for-byte.

**State-specific custom work stays out of GitHub**: scripts and outputs tailored to a single state's engagement (custom floors, no-holdout tuning, state-mined rules) go in `custom_one_off/<state>/`, which is gitignored. The public repo carries generic tooling and comparative studies only. Lesson from the Virginia work (2026-07-06, artifacts in `custom_one_off/virginia/`): single-state mining needs a hard support floor (n >= 30) — at state scale the LCB alone does not prevent collapse (median holdout precision 0 at n >= 5); with the floor, rules deflate gently (~1/3) instead.

### Key v2 config knobs

| knob | default | meaning |
|---|---|---|
| `LCB_Z` | 2.326 | filter stringency (one-sided Wilson) |
| `THRESHOLD_GRID` | .05-.95 | filter floors for the sweep |
| `MIN_TRAIN_FLAGGED` | 10 | support backstop |
| `MIN_PRECISION` | 0.20 | shortlist floor, applied to the LCB |
| `OBJECTIVE` | "dollars" | recall basis for plots/x (counts always also written) |
| `SIGNIF_DIGITS` | 3 | rule threshold rounding |
| `SORT_WALK_USE_FRESH_SHARE` | TRUE | delivery builder: fresh-share floor on the fill walk (findings 33-34); FALSE restores the legacy walk |
| `SORT_WALK_MIN_FRESH_SHARE` | 0.50 | the floor on f = n_new/n_flagged at a rule's turn; skipped slots refilled from deeper ranks at unchanged consumed capacity (findings 33-34 and the section 34 adjudication) |

## v1 (legacy) — handle with care

v1 = the {pre}-based scripts documented in the README's legacy section: `INCL_find_inclusion_rules_multi_model_by_hh_size.R` (+ `_c50`, `_xrf`), `EXCL_find_exclusion_rules_by_hh_size.R`, the `INCL/EXCL_optimize_*_for_a_state.R` grid searches, `archive/optimize_rulefit_params.R` / `methods/single_model_optimize_params.R`, and `code_for_single_model_combined_HH_sizes/`. Their outputs are consumed by external users — treat the CONTENT as frozen; the folders were relocated 2026-07-14: `archive/inclusion_rules_by_hh_size/`, `archive/exclusion_rules/`, `archive/inclusion_rules_combined_hh_sizes/`, `archive/code_for_single_model_combined_HH_sizes/`, `methods/parameter_tuning/`, `methods/compare_models_by_HHsize_vs_pooled/`. The 14:1 rebalancing blocks in v1 INCL scripts are commented out by design (original intent); greedy "nets" exist only in v1.

## Presentations and write-ups (decks, docs, README prose)

The audience is SNAP program experts and state analysts — they know the DATA
and the PROGRAM better than we do; our edge is modeling. Persuade with numbers
and charts from our own runs, never with authority or intuition. Rules:

- **Modeling conclusions only.** Report what moved held-out performance in
  experiments we actually ran. No claims about real-world phenomena, program
  behavior, or data semantics — even reasonable assumptions (e.g., what a
  blank field "means") stay out; state the measured effect instead (rows
  dropped, rules gained).
- **Every claim carries its measurement.** A slide without a number or chart
  from our runs is a slide to cut. Head-to-head + held-out-year framing up
  front is the credibility signal.
- **No clever slogans or over-generalization.** Say the concrete thing:
  "split by coarse HH size strata (1 / 2-3 / 4+)", not "split by structure —
  but split coarsely". Plain English, fewer acronyms, define terms inline once.
- **Don't play up deduction/'other' errors.** Most states treat them as
  low-value distractions (small dollars, high volume); finding them is not a
  headline win, even though we mine that frame for completeness.
- Keep frame-relative AND any-error numbers honest side by side; never quote
  only the flattering one.

## Task tracking

**GitHub issues are the task board** (2026-08-06): operational and code
follow-ups are filed as issues; research claims and their statuses stay in
`methods/findings_ledger.md`; RESUME.md points at both. Claude-drafted issues
and comments post from the repo owner's account and must say so in the
text ("Drafted by Claude; posted from the repo owner's account").

## Reference documents

- `methods/findings_ledger.md` — one row per claim: status (settled / retired / option / hazard / open), the scope it was tested at, and its section citation. **The first stop when planning any experiment or pipeline change**; quote its rows in the pre-run design note. Maintained by the log-finding skill.
- `methods/known_constraints.md` — the measured do/don'ts for each pipeline file, one section per file, every line cited. A hook (`.claude/settings.json` + `.claude/hooks/findings_guard.sh`) injects the matching section whenever one of those files is edited in Claude Code, and injects the study-design gate when a new script is written under `methods/` or `runners/`.
- `methods/modeling_findings.md` — plain-language summary of every finding: a tagged "Takeaway" per section (data-vs-pipeline), a glossary, and links into the detailed record. Read this first.
- `methods/modeling_findings_detailed.md` — the complete evidence log: full numbers, tables, caveats, and artifact pointers for every section (winner's curse, engine studies, strata, ESAP, states).
- `methods/design_drop_pre_architecture.md` — the v2 design rationale and decisions.
- `DATA_DICTIONARY.md` (rendered) / `Definitions for variables used.txt` (original) — feature dictionary.
- `methods/check_doc_consistency.sh` (with `methods/retired_claims.txt`): guards the docs against drift. It checks that the reader docs carry no em-dashes and that no retired claim reappears in README/GUIDANCE. Run it after logging a finding and before a release. Rule of thumb: reader docs (README, GUIDANCE, decks) cite the findings by section number and never originate a number.
