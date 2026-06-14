# snap_qc
code and data for modeling SNAP payment errors using regression trees

Putting this out on my github with an Apache 2.0 license as an assurance that anyone can freely use and build upon the code, ideas, or results.

---

## Where to start

There are two main use cases. If you have your own internal data, you can go directly to the corresponding sets of scripts (i.e., no need to run the data munging script or the visualization scripts first).

**I have a pile of cases already flagged for review and want to cut it down** → go to the [EXCL_ scripts](#exclusion-rules-excl_-scripts).

**I don't have a flagging system and want to identify which cases are most likely to have an error** → go to the [INCL_ scripts](#inclusion-rules-incl_-scripts).

In both cases, the scripts expect a data frame in your R environment with one row per case, a column indicating whether the case is a true error, and whatever features you want to use as predictors. You can swap in your own internal data and your own feature list — just update the config section at the top of the script. The `features` vector and the `TARGET_IS_ERROR` expression are the main things to change. See [`Definitions for variables used.txt`](Definitions%20for%20variables%20used.txt) for documentation of the features used in the default setup.

If you want to use national public QC data rather than internal data, see [What data is required?](#What data is required?) below. The national data can be useful for finding more specific patterns since the number of errors in any one state is limited. 

Note that I recommend running all the models stratified by household size (e.g., one model for each household size: 1, 2, 3, 4, 5+), which is what I have in the main directory. The precision-recall curves are better across the board (see results in `compare_models_by_HHsize_vs_pooled/` folder and explore yourself using the `compare_combined_vs_by_hh_size_model_performance.R` script). If this is not an option for you or you have found better results in your state with pooling, see the `code_for_single_model_combined_HH_sizes folder`. 

---

## Exclusion rules (EXCL_ scripts)

You have a list of cases flagged for review. These scripts help you find simple rules that let you drop low-risk cases from that pile, leaving a more targeted set for reviewers.


1. **`EXCL_find_exclusion_rules_by_hh_size.R`** — runs RuleFit on your flagged cases, stratified by household size (1, 2, 3, 4, 5+), to find candidate exclusion rules. Outputs rule CSVs to `exclusion_rules/`. If your internal data is rich enough, you may be able to stop here and apply the rules directly.

2. **`EXCL_optimize_single_exclusion_rule_by_hh_size_for_a_state.R`** — takes one rule from step 1 and grid-searches its numeric thresholds on a specific state's data, maximizing workload cut while retaining a floor of error dollars. This is useful if you have idenfied a small number of rules of interest that you really want to get right. 

3. **`EXCL_optimize_set_of_exclusion_rules_by_hh_size_for_a_state.R`** — same as step 2 but optimizes the full exclusion shortlist at once. This is useful if you're taking rules from national data and adjusting them to your state. 

Examples of exclusion rule outputs are in the `exclusion_rules/` folder.


---

## Inclusion rules (INCL_ scripts)

These scripts find candidate prioritization rules (i.e., rules that flag cases more likely to have an error). The goal is to increase the yield of review time and you can set the particular strategy for that in the script (e.g., dollar recall with a minimum floor of precision). 

1. **`INCL_find_inclusion_rules_multi_model_by_hh_size.R`** — runs RuleFit separately by error type (configurable, but in the script, it's just based on error element and status from the QC data: earned overissuance, unearned overissuance, underissuance) and household size. Outputs rule CSVs to `inclusion_rules_by_hh_size/`. If you have a lot of internal labeled data, you may be done after this step.

2. **`INCL_optimize_single_inclusion_rule_by_hh_size_for_a_state.R`** — takes one rule from step 1 (or a rule from any source) and grid-searches its numeric thresholds for a specific state to maximize precision at a recall floor.

3. **`INCL_optimize_set_of_inclusion_rules_by_hh_size_for_a_state.R`** — same as step 2 but tunes all high-precision rules for a state at once. The results will change quite a bit as you modify the OBJECTIVE (dollars vs. counts), OPTIMIZE_FOR (precision vs recall), MIN_FLAGGED (cases flagged by each rule), PRECISION_TARGET (floor for precision). 

The end goal is a state-specific rule list like:
`inclusion_rules_by_hh_size/final_by_HHsize_inclusion_rules_highprecision.csv`

Examples of state rule outputs are in the `inclusion_rules_by_hh_size/` folder. 

---

## What data is required?

You don't have to use the national QC data to use these scripts — it's easy to adjust the `features` list and `TARGET_IS_ERROR` expression to work with your own internal data and the variables you'd like to use.

If you want to mine patterns from the national data, it is included in this repo in the `qc_data/` folder, so you don't need to download it separately. The source is [snapqcdata.net](https://snapqcdata.net). The main reason to use it to get started is that there is a lot more error signal. You can train on a much larger number of error cases and identify more detailed patterns than a single state's data might support. The examples in this repo reflect me mining rules from national data then tuning them to states using the `INCL_optimize_*` or `EXCL_optimize_*` scripts.

If you want to use the national data, you'll see the `reg_model_data` data frame in the scripts. Recreate is using `1_data_munging_and_raw_variable_reconstruction_for_using_public_qc_data.R`. 

**If you want to explore patterns visually before rule mining**, `visualize_national_regression_trees_by_type_of_error.R` and `visualize_state_regression_trees_predicting_dollar_amounts.R` plot regression trees by error type and state. See the `state_income_error_trees_any_timeper` folder for examples. This is optional.

---

## Data dictionary

Variable definitions and sourcing notes are in [`Definitions for variables used.txt`](Definitions%20for%20variables%20used.txt). Covers all model features — income variables, deductions, household composition indicators, benefit ratios — and maps them back to the raw QC data elements. Many thanks to Jesse Shaw for putting this together.

---

## Converting rules to SQL

Once you have rules you want to implement, **`parse_tree.R`** provides a `parse_tree_to_sql()` function that converts an rpart tree into a SQL `CASE` statement, useful for deploying rules in a production case management system.

---

The main goal of putting up this repo is to make it unambiguous that anyone can freely use ideas / materials they've seen me present regarding SNAP QC. I'll continue adding to and cleaning up the code based on what's useful so please reach out!