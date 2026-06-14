# snap_qc
code and data for modeling SNAP payment errors using regression trees

Putting this out on my github with an Apache 2.0 license as an assurance that anyone can freely use and build upon the code, ideas, or results.

---

## Where to start

There are two main use cases, and you can jump directly into either one with your own internal data — no need to run the data munging script or the visualization scripts first.

**I have a pile of cases already flagged for review and want to cut it down** → go to the [EXCL_ scripts](#exclusion-rules-excl_-scripts).

**I don't have a flagging system and want to identify which cases are most likely to have an error** → go to the [INCL_ scripts](#inclusion-rules-incl_-scripts).

In both cases, the scripts expect a data frame in your R environment with one row per case, a column indicating whether the case is a true error, and whatever features you want to use as predictors. You can swap in your own internal data and your own feature list — just update the config section at the top of the script. The `features` vector and the `TARGET_IS_ERROR` expression are the main things to change. See [`Definitions for variables used.txt`](Definitions%20for%20variables%20used.txt) for documentation of the features used in the default setup.

If you want to use national public QC data rather than internal data, see [Getting data](#getting-data) below.

---

## Exclusion rules (EXCL_ scripts)

You have a list of cases flagged for review. These scripts find simple rules that let you safely drop low-risk cases from that pile, leaving a more targeted set for reviewers.

1. **`EXCL_find_exclusion_rules_by_hh_size.R`** — runs RuleFit on your flagged cases, stratified by household size (1, 2, 3, 4, 5+), to find candidate exclusion rules. Outputs rule CSVs to `exclusion_rules/`. If your internal data is rich enough, you may be able to stop here and apply the rules directly.

2. **`EXCL_optimize_single_exclusion_rule_by_hh_size_for_a_state.R`** — takes one rule from step 1 and grid-searches its numeric thresholds on a specific state's data, maximizing workload cut while retaining a floor of error dollars.

3. **`EXCL_optimize_set_of_exclusion_rules_by_hh_size_for_a_state.R`** — same as step 2 but tunes the full exclusion shortlist at once.

The `code_for_single_model_combined_HH_sizes/` folder has pooled-model counterparts (not stratified by household size) if you prefer a single model across all sizes.

---

## Inclusion rules (INCL_ scripts)

You don't have a flagging system. These scripts find rules that identify which cases are most likely to have an error, so you can prioritize your review workload.

1. **`INCL_find_inclusion_rules_multi_model_by_hh_size.R`** — runs RuleFit separately for each error type (earned overissuance, unearned overissuance, underissuance) and household size. Outputs rule CSVs to `inclusion_rules_by_hh_size/`. If you have a lot of internal labeled data, you may be done after this step.

2. **`INCL_optimize_single_inclusion_rule_by_hh_size_for_a_state.R`** — takes one rule from step 1 and grid-searches its numeric thresholds for a specific state to maximize precision at a recall floor.

3. **`INCL_optimize_set_of_inclusion_rules_by_hh_size_for_a_state.R`** — same as step 2 but tunes all high-precision rules for a state at once.

The end goal is a state-specific rule list like:
`inclusion_rules_by_hh_size/final_by_HHsize_inclusion_rules_highprecision.csv`

The `code_for_single_model_combined_HH_sizes/` folder has pooled-model counterparts. The stratified approach consistently outperforms the pooled model on precision-recall.

---

## Getting data

**If you have internal state data**, you can use it directly — just load it into R and point the scripts at it. You don't need the public QC data.

**If you want to use national public data**, download the SNAP QC public-use files from [snapqcdata.net](https://snapqcdata.net) and run `1_data_munging_and_raw_variable_reconstruction_for_using_public_qc_data.R` to build the `reg_model_data` frame. This is useful for identifying general national patterns or for training rules before tuning them on state-specific data.

**If you want to explore patterns visually before rule mining**, `visualize_national_regression_trees_by_type_of_error.R` and `visualize_state_regression_trees_predicting_dollar_amounts.R` plot regression trees by error type and state. See the `state_income_error_trees_any_timeper` folder for examples.

---

## Data dictionary

Variable definitions and sourcing notes are in [`Definitions for variables used.txt`](Definitions%20for%20variables%20used.txt). Covers all model features — income variables, deductions, household composition indicators, benefit ratios — and maps them back to the raw QC data elements. Many thanks to Jesse Shaw for putting this together.

---

## Converting rules to SQL

Once you have rules you want to implement, **`parse_tree.R`** provides a `parse_tree_to_sql()` function that converts an rpart tree into a SQL `CASE` statement, useful for deploying rules in a production case management system.

---

The main goal of putting up this repo is to make it unambiguous that anyone can freely use ideas / materials they've seen me present regarding SNAP QC. I'll continue adding to and cleaning up the code based on what's useful so please reach out!