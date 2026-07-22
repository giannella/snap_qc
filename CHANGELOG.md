# Changelog

All notable changes to this repository are recorded here, newest first.
The format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/);
version numbers follow the policy in [VERSIONING.md](VERSIONING.md).

If you only read one thing before updating: the **Changed** and **Removed**
sections of each release tell you whether anything you currently run or read
moved or behaves differently.

## [Unreleased]

### Changed
- State delivery lists rebuilt with a uniform 13-column schema. Every list in
  `state_delivery_lists/` now carries `dollars_per_flag_train` (mean error
  dollars per flagged training case), so all 49 states share one format. 19
  states previously used a 12-column format without that column.
- 14 states still built under the pre-v2.3.0 raw-precision admission (Arizona,
  California, Colorado, and 11 others) now use the Benjamini-Hochberg FDR
  admission adopted in v2.3.0 (findings 19), matching every other state.

### Notes
- The lists are one fresh mine of the current recipe. The ranger forest is not
  thread-pinned, so re-mining on a different machine gives a different draw: the
  rebuilt lists differ from the prior committed ones by roughly 10 to 15 percent
  of rows, in both directions, including states whose recipe did not change.
  xgboost and the filters are deterministic; the variation is ranger's default
  multi-threading. Neither draw is more correct than the other.

Other research results, including several retired approaches, are recorded in
`methods/modeling_findings.md` (sections 17-22).

## [2.3.0] - 2026-07-17

No action needed: filenames, columns, and script interfaces are unchanged.

### Changed
- **The delivery builder's admission test.** A candidate rule was
  previously admitted if it flagged at least 30 training cases, had raw
  precision of at least 0.05, and was above its stratum's base error
  rate. It is now admitted if a Benjamini-Hochberg test (false-discovery
  rate 10%) rejects "precision at or below the stratum base rate" and it
  flags at least 30 training cases. The new test sets its own bar from
  the number of candidates and the strength of their evidence, which
  matters when the code runs on data of a different size than ours. On
  two held-out years it matched the previous filter at the 5% review
  budget and was slightly better at 10% (`methods/modeling_findings.md`
  section 19). The previous filter remains available (`ADMISSION <-
  "legacy"` in the builder).
- **All 19 states' delivery lists rebuilt with the new admission test**,
  under the existing filenames. Differences from the previous lists are
  small; the evidence for the change is in the findings file, not in
  large deliverable movements.

### Added
- Findings 17-22: the selection-method studies behind this release,
  including the approaches that were tested and not adopted.

## [2.2.0] - 2026-07-16

No action needed: filenames, columns, and script interfaces are unchanged.

### Changed
- **All 19 states' delivery lists refreshed onto one recipe and one
  schema**: the any-error vocabulary with the 12-column provenance schema,
  under the existing filenames. This replaces the 10 lists briefly
  published from five-frame mining, a vocabulary change that a
  train-2022-23/test-2024 benchmark showed was worse at the 5% review
  budget (median precision 0.306 vs 0.324) and that three rescue attempts
  (higher stringency, near-duplicate collapse, shrinkage estimates) did
  not repair. Details in `methods/design_selection_layers_v3.md`.

### Added
- The machinery/goal separation made explicit: a "Statistics and goal
  metrics" section in the README and breadcrumbs in the driver scripts.
  The mining and evidence machinery is general purpose; the ranking
  statistic + goal metric are a module the user chooses. The default,
  validated pairing keeps the plain filenames; any future non-default
  pairing will carry its label in the filename, so an unlabeled file
  always means "the recommended list."
- Pipeline and process figures in the README
  (`presentation_figures/pipeline_option_B.png`, `refinement_loop.png`).
- Measured: per-rule error-dollar size persists from training to a future
  year more strongly than precision does (`dollar_persistence_check_v2.R`),
  which is groundwork for a dollar-prioritizing pairing.

Work in progress on a revised rule-selection method (see
`methods/design_selection_layers_v3.md`): admission by false-discovery-rate
control instead of a fixed confidence cutoff, grouping of near-duplicate
rules into families with labeled substitutes, and shrinkage-based precision
estimates. Nothing ships until it passes validation on held-out years;
current scripts and lists are unaffected.

## [2.1.0] - 2026-07-14

### Added
- `state_delivery_lists/`: ready-built, ranked rule lists per state (5% and
  10% review budgets), with a README defining every column. This folder is
  the recommended starting point for states.
- Rule provenance columns in newly built delivery lists: which engine(s) and
  which mining pass(es) produced each rule, plus per-state flag counts
  (`pool`, `engines`, `mined_frames`, `n_flagged_state`, `n_new_at_rank`).
- `INCL_build_blended_delivery_list_v2.R` promoted to a top-level script:
  it is self-contained (mines its own rule pools) and is the recommended
  v2 entry point.

### Changed
- Repository layout: v1 output folders moved to `archive/`
  (`inclusion_rules_by_hh_size`, `exclusion_rules`,
  `inclusion_rules_combined_hh_sizes`, `code_for_single_model_combined_HH_sizes`,
  `state_rules_v2`); two v1-era study folders moved into `methods/`
  (`compare_models_by_HHsize_vs_pooled`, `parameter_tuning`). File contents
  are unchanged; only locations moved, and all in-repo references were
  updated.
- `state_delivery_lists/` currently contains two column layouts (10 states
  on the newer provenance schema, 9 on the original 8-column schema); the
  folder README documents both. A single-schema rebuild will follow the
  selection-method validation.

### Deprecated
- Per-state threshold tuning (`state_threshold_gridsearch_v2.R`) as a default
  workflow: an 18-state test on a future year showed it does not beat
  deploying the national ranking as-is for the median state. The script
  remains available; treat tuning as a fallback and validate on your own
  held-out year.

## [2.0.0] - 2026-07-06

### Added
- The v2 rule-mining pipeline: `rule_mining_helpers.R` (shared five-stage
  machinery), `INCL_find_inclusion_rules_by_hh_size_v2.R`,
  `EXCL_find_exclusion_rules_by_hh_size_v2.R`, regression test, and the
  validation studies under `methods/` with results in
  `methods/modeling_findings.md`.
- Selection on the Wilson lower confidence bound of training precision,
  which fixed the overstatement in raw-precision shortlists (a nominal
  "20% precision" list had delivered ~10% out of sample).

### Changed
- v1 remains fully intact and documented; nothing v1 was removed or renamed
  in this release. New projects should start with v2.

## [1.x] - 2026-03 through 2026-06

The original RuleFit/{pre}-based pipeline (initial commit 2026-03-16),
preserved as documented in the README's legacy section. Changes in this
period predate this changelog and are visible in the git history.
