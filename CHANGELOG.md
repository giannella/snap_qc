# Changelog

All notable changes to this repository are recorded here, newest first.
The format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/);
version numbers follow the policy in [VERSIONING.md](VERSIONING.md).

If you only read one thing before updating: the **Changed** and **Removed**
sections of each release tell you whether anything you currently run or read
moved or behaves differently.

## [Unreleased]

Dollar-prioritizing ranking (a `*_dollaryield_*` pairing) is in audition;
the FDR admission test and the 2017-19 era validation of pool-size-scaled
stringency are on the research roadmap.

## [3.0.0] - 2026-07-16

### Changed (breaking)
- **Delivery list filenames now name the statistic-goal pairing that built
  them.** `blended_delivery_<State>_2022_2024_budget05.csv` is now
  `blended_delivery_<State>_2022_2024_lcb99_workloadfill_budget05.csv`
  (same for budget10). Update any saved links or scripts that read these
  files; contents are the rebuilt lists below.
- **All 19 states' delivery lists rebuilt on one recipe and one schema**:
  the any-error vocabulary with the 12-column provenance schema. This
  replaces the 10 lists briefly published from five-frame mining — a
  vocabulary change that a train-2022-23/test-2024 benchmark showed was
  WORSE at the 5% review budget (median precision 0.306 vs 0.324) and that
  three rescue attempts (higher stringency, near-duplicate collapse,
  shrinkage estimates) did not repair. Details in
  `methods/design_selection_layers_v3.md` and the findings file.

### Added
- The machinery/goal separation made explicit: a "Statistics and goal
  metrics" section in the README, pairing labels in filenames, and
  breadcrumbs in the driver scripts. The mining and evidence machinery is
  general purpose; the ranking statistic + goal metric are a module the
  user chooses.
- Pipeline and process figures in the README
  (`presentation_figures/pipeline_option_B.png`, `refinement_loop.png`).
- Measured: per-rule error-dollar size persists from training to a future
  year more strongly than precision does (`dollar_persistence_check_v2.R`) —
  groundwork for the dollar-prioritizing pairing.

Work in progress on a revised rule-selection method (see
`methods/design_selection_layers_v3.md`): admission by false-discovery-rate
control instead of a fixed confidence cutoff, grouping of near-duplicate
rules into families with labeled substitutes, and shrinkage-based precision
estimates. Nothing ships until it passes validation on held-out years;
current scripts and lists are unaffected.

## [2.1.0] - 2026-07-14

### Added
- `state_delivery_lists/` — ready-built, ranked rule lists per state (5% and
  10% review budgets), with a README defining every column. This folder is
  the recommended starting point for states.
- Rule provenance columns in newly built delivery lists: which engine(s) and
  which mining pass(es) produced each rule, plus per-state flag counts
  (`pool`, `engines`, `mined_frames`, `n_flagged_state`, `n_new_at_rank`).
- `INCL_build_blended_delivery_list_v2.R` promoted to a top-level script —
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
