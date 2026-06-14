# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Purpose

R codebase for modeling SNAP (food stamp) payment errors. It mines interpretable decision rules from public USDA SNAP Quality Control (QC) data to help state agencies either:
- **Exclude** low-risk cases from an existing review pile (improve targeting precision), or
- **Include / flag** high-risk cases for review when no prior flagging system exists.

## Running the Code

All scripts are run interactively in R (RStudio or similar). There is no build system, package, or test suite. Scripts are designed to be sourced top-to-bottom after setting the config section at the top. Key R packages:

```r
library(pre)       # RuleFit (Prediction Rule Ensembles) — the core modeling package
library(ranger)    # Random forests (used in data munging / variable reconstruction)
library(dplyr)
library(ggplot2)
library(haven)     # read .sav SPSS files (public QC data format)
library(yardstick)
```

## Data

Public SNAP QC `.sav` files are expected at `C:/Users/ericg/qc/qc_data/` (e.g., `qc_pub_fy2022.sav`). Auxiliary lookup tables (state FIPS, max allotments, standard deductions) live in `additional_data/`. The main modelling frame (`reg_model_data`) is built by script `1_data_munging_and_raw_variable_reconstruction_for_using_public_qc_data.R` and expected in the R environment for all downstream scripts.

## Script Workflow

### Step 1 — Data preparation
`1_data_munging_and_raw_variable_reconstruction_for_using_public_qc_data.R`
Loads public QC `.sav` files for FY2017–2024, reconstructs income/deduction variables, and produces `reg_model_data` with columns such as `error_status`, `over_threshold`, `total_error_amount`, `fiscal_year`, `state`, `cert_HH_size_FS_n`.

`error_status` values: `"earned_overissuance"`, `"unearned_overissuance"`, `"underissuance"`, `"no_error"`.

### Steps 2–3 — Regression tree visualization (EDA)
`visualize_national_regression_trees_by_type_of_error.R`, `visualize_state_regression_trees_predicting_dollar_amounts.R`
Plot rpart trees to explore error patterns before rule mining.

### Steps 4–5 / EXCL\_\* — Exclusion rules
Find rules that safely exclude low-risk cases from an existing review pile:
1. `EXCL_find_exclusion_rules_by_hh_size.R` — runs RuleFit, outputs `exclusion_rules/exclusion_rules_by_hh_size_*.csv`
2. `EXCL_optimize_single_exclusion_rule_by_hh_size_for_a_state.R` — grid-searches one rule's thresholds for a state
3. `EXCL_optimize_set_of_exclusion_rules_by_hh_size_for_a_state.R` — grid-searches the full shortlist for a state

Objective: maximize workload cut (cases excluded) while retaining ≥ `RETAIN_FLOOR` of error dollars.

### Steps 6–9 / INCL\_\* — Inclusion rules
Find rules that flag high-risk cases for review:
1. `INCL_find_inclusion_rules_multi_model_by_hh_size.R` — runs RuleFit per error type and household size, writes `inclusion_rules_by_hh_size/`
2. `INCL_optimize_single_inclusion_rule_by_hh_size_for_a_state.R` — grid-searches one rule's thresholds for a state
3. `by_HHsize_8_grid_search_for_high_precision_rules.R` — optimizes all high-precision rules for a state
4. `by_HHsize_9_state_by_state_grid_search_and_hold_out_testing.R` — train/test split across every state; assesses rule stability

Objective: maximize precision (share of flagged cases that are true errors) subject to ≥ `RECALL_FLOOR` of error dollars (or counts).

The **recommended final output** is `inclusion_rules_by_hh_size/final_by_HHsize_inclusion_rules_highprecision.csv`.

## Key Architecture Decisions

### Two modeling paradigms
- **By-household-size (stratified)**: `INCL_*` / `EXCL_*` scripts. Models are fit separately for HH sizes 1, 2, 3, 4, 5+. The stratifier column (`cert_HH_size_FS_n`, collapsed to `"1"–"4"` and `"5+"`) is dropped from predictors. This approach consistently outperforms the pooled model on precision-recall.
- **Single pooled model**: `code_for_single_model_combined_HH_sizes/`. Kept for comparison; the stratified approach is preferred.

### RuleFit rule mining (`{pre}` package)
`pre()` fits an ensemble of rpart trees (controlled by `maxdepth`, `ntrees`, `learnrate`, `sampfrac`) and extracts conjunctive rules (2–5 conditions each). Rules are then greedily OR-combined into a **"net"** that climbs recall while keeping precision high. The net construction is distinct from the `{pre}` prediction itself.

### Precision-recall terminology
- **Inclusion net**: greedy OR of INCLUDE-direction rules; scored by precision (errors / flagged) and recall (errors caught / all errors).
- **Exclusion net**: greedy OR of EXCLUDE-direction rules; scored by workload cut (cases dropped) and dollar retention (error $ kept).
- `OBJECTIVE = "dollars"` or `"counts"` controls whether recall is measured in error dollars or error case counts throughout.

### Parameter tuning
`optimize_rulefit_params.R` and `single_model_optimize_params.R` sweep `pre()` hyperparameters one-at-a-time and plot precision-recall curves, helping choose `maxdepth`, `ntrees`, etc. before committing to a full rule-mining run.

## Output Files

| Folder | Contents |
|---|---|
| `inclusion_rules_by_hh_size/` | Rule CSVs from INCL scripts; `final_*` files are the curated combined set |
| `exclusion_rules/` | Rule CSVs from EXCL scripts, including state-optimized results |
| `state_holdout_rulecheck/` | Cross-state holdout performance (script 9 output) |
| `state_train_test_rulecheck/` | State-level train/test stability checks |
| `compare_models_by_HHsize_vs_pooled/` | PR curve comparison CSVs |
| `pre_param_sweep/` | Parameter sweep results |
| `inclusion_rules_combined_hh_sizes/` | Pooled-model rule outputs |

## Common Config Parameters

These appear at the top of every script and should be set before running:

| Parameter | Typical values | Meaning |
|---|---|---|
| `OBJECTIVE` | `"dollars"` / `"counts"` | Recall basis for precision-recall optimization |
| `RECALL_FLOOR` | `0.02` – `0.50` | Minimum recall the net must achieve |
| `NET_FLOORS` | `c(0.20, 0.30, ...)` | Multiple recall floors to report nets at |
| `RETAIN_FLOOR` | `0.97` | Exclusion: minimum share of error dollars to keep |
| `TRAIN_YEARS` / `TEST_YEARS` | `c("2022","2023","2024")` / `c("2018","2019")` | Year splits for train/holdout |
| `MIN_PRECISION` | `0.20` | Threshold to call a rule "high precision" |
