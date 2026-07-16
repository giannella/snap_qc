# Changelog

All notable changes to this repository are recorded here, newest first.
The format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/);
version numbers follow the policy in [VERSIONING.md](VERSIONING.md).

If you only read one thing before updating: the **Changed** and **Removed**
sections of each release tell you whether anything you currently run or read
moved or behaves differently.

## [Unreleased]

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
