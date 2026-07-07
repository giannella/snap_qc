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
"C:\Program Files\R\R-4.5.1\bin\Rscript.exe" run_incl_v2.R > incl_v2_run.log 2>&1
```

Runners load `reg_model_data.rds` (a saved copy of the main modelling frame, expected in the repo root) then source the driver. There is no package or build system. **After changing `rule_mining_helpers.R`, always run the regression test**: `Rscript test_rule_mining_helpers.R` (18 checks, all must PASS).

Key v2 packages: `dplyr`, `ggplot2`, `ranger`, `xgboost` (plus `rpart` for the optional bagged-CART engine). v1 additionally needs `pre`. Parse-check scripts with `Rscript -e "invisible(parse('file.R'))"` before running.

## Data

Public SNAP QC `.sav` files are expected at `C:/Users/ericg/qc/qc_data/`. Auxiliary lookups live in `additional_data/`. The main modelling frame `reg_model_data` is built by `1_data_munging_and_raw_variable_reconstruction_for_using_public_qc_data.R` (shared by v1 and v2) and cached as `reg_model_data.rds`.

`error_status` values: `earned_overissuance`, `unearned_overissuance`, `underissuance`, `other_error`, `no_error`. A case is an error when `over_threshold != 0` (aligned 1:1 with error_status). Base rates are low: ~11% of cases have any over-threshold error (post-2026-07-07 rebuild; was ~8.4% on the stale single-element frame); typed frames run 0.4-6% by stratum. `other_error` is the LARGEST error category and is mined in v2 (it never was in v1).

**Frame provenance (2026-07-07)**: `reg_model_data.rds` is now saved by the munging script itself (never hand-built — a stale hand-built frame silently excluded all multi-element error cases, ~31% of errors, for weeks). Multi-element cases are KEPT (`second_element_i` flags them; do NOT use as a mining feature — state reporting is inconsistent). Deduction-field NAs are zero-filled (`ded_fields_imputed`), not dropped. Results predating the rebuild live in `run*_singleelement_frame/` archive subfolders; rule-content changes across data revisions are diffed with `compare_rule_sets_v2.R`. Per-state error visibility (public data misses all ineligible cases): `state_error_accounting/`.

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

### Validated settings and principles (2026-07 studies; see modeling_findings.md)

- Engines: xgboost (nrounds 1000, eta 0.02, subsample 0.20) + ranger (1000 trees, mtry 2), depth 4. The PAIR beats either alone and beats rpart+ranger.
- **"Mine big, filter stringently"**: big ensembles extend recall reach; `LCB_Z = 2.326` (99%) removes their selection-multiplicity noise. 90% (1.2816) for exploration only.
- Raw train precision suffers a strong winner's curse (nominal 0.20 -> ~0.10 holdout); the LCB fixes calibration. Never shortlist on raw precision or on holdout performance.
- Strata: household size 1 / 2-3 / 4+ (`cert_HH_size_FS_n` collapsed). On v2 engines pooling is nearly as precise, but the coarse split buys recall reach and ~5x filtered inventory; 5-way splits are worse.
- elderly/disabled: a FEATURE, not a stratum (settled empirically — the ensembles carve the caseload themselves).
- Frame-relative precision understates deployed precision ~2x; always compute and quote any-error metrics.
- States: tune thresholds locally only when ~30+ rules qualify on state train data; small states deploy national rules unchanged (small-sample tuning is winner's-curse territory).

### v2 scripts

| script | role |
|---|---|
| `rule_mining_helpers.R` + `test_rule_mining_helpers.R` | shared pipeline + regression test |
| `INCL_find_inclusion_rules_by_hh_size_v2.R` | inclusion rules per mining frame (4 typed + pooled any_error) x stratum -> `inclusion_rules_by_hh_size_v2/` |
| `EXCL_find_exclusion_rules_by_hh_size_v2.R` | exclusion rules (clean-rate LCB; workload cut vs dollar retention) -> `exclusion_rules_by_hh_size_v2/` |
| `state_threshold_gridsearch_v2.R` | per-state threshold tuning + holdout test + national-as-is benchmark -> `state_rules_v2/` |
| `tune_engine_params_v2.R`, `tune_followup_subsample_lcbz_v2.R` | hyperparameter + LCB_Z sweeps -> `parameter_tuning_v2/` |
| `compare_engines_v2.R`, `compare_engine_combos_v2.R` | engine studies -> `compare_engines_v2/` |
| `compare_anyerror_vs_typed_frames_v2.R` | typed vs pooled-target mining -> `compare_anyerror_vs_typed_v2/` |
| `compare_hh_strata_v2.R` | stratification schemes -> `compare_hh_strata_v2/` |
| `check_esap_coverage_v2.R` | elderly/disabled coverage parity check |

Long-running scripts checkpoint mined vocabularies to `.rds` and support `RESUME_FROM_CHECKPOINT` (pre-set it in a runner before `source()`). Comparison outputs from superseded configurations are kept in suffixed/archived copies (e.g., `run1_*` subfolders) — don't delete them.

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

## v1 (legacy) — handle with care

v1 = the {pre}-based scripts documented in the README's legacy section: `INCL_find_inclusion_rules_multi_model_by_hh_size.R` (+ `_c50`, `_xrf`), `EXCL_find_exclusion_rules_by_hh_size.R`, the `INCL/EXCL_optimize_*_for_a_state.R` grid searches, `optimize_rulefit_params.R` / `single_model_optimize_params.R`, and `code_for_single_model_combined_HH_sizes/`. Their outputs (`inclusion_rules_by_hh_size/`, `exclusion_rules/`, `parameter_tuning/`, `compare_models_by_HHsize_vs_pooled/`) are consumed by external users — treat as frozen. The 14:1 rebalancing blocks in v1 INCL scripts are commented out by design (original intent); greedy "nets" exist only in v1.

## Reference documents

- `modeling_findings.md` — all empirical results with artifact pointers (winner's curse, engine studies, strata, ESAP, states).
- `design_drop_pre_architecture.md` — the v2 design rationale and decisions.
- `Definitions for variables used.txt` — feature dictionary.
