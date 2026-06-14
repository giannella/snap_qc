# snap_qc
code and data for modeling SNAP payment errors using regression trees

Putting this out on my github with an Apache 2.0 license as an assurance that anyone can freely use and build upon the code, ideas, or results.

---

## Scripts

**`1_data_munging_and_raw_variable_reconstruction_for_using_public_qc_data.R`**
Loads the public SNAP QC `.sav` files (available at [snapqcdata.net](https://snapqcdata.net)) and reconstructs the income and deduction variables used in modeling. Run this first to build the `reg_model_data` frame that all downstream scripts expect. Useful if you want to identify patterns using national data rather than data from a single state.

**`visualize_national_regression_trees_by_type_of_error.R`** and **`visualize_state_regression_trees_predicting_dollar_amounts.R`**
Plot regression trees to explore error patterns before rule mining — a form of exploratory data analysis. See the `state_income_error_trees_any_timeper` folder for examples.

### Exclusion rules (EXCL_ scripts)
Use these if you already have a list of cases flagged for review and want to make it more targeted by removing low-risk cases.

- **`EXCL_find_exclusion_rules_by_hh_size.R`** — runs RuleFit separately within each household-size stratum (1, 2, 3, 4, 5+) to find rules that safely exclude cases. Outputs rule CSVs to `exclusion_rules/`.
- **`EXCL_optimize_single_exclusion_rule_by_hh_size_for_a_state.R`** — takes one rule from the output above and grid-searches its numeric thresholds on a specific state's data to maximize workload cut while retaining a floor of error dollars.
- **`EXCL_optimize_set_of_exclusion_rules_by_hh_size_for_a_state.R`** — same as above but optimizes the full exclusion shortlist at once.

The `code_for_single_model_combined_HH_sizes/` folder contains pooled-model counterparts of these scripts (not stratified by household size).

### Inclusion rules (INCL_ scripts)
Use these if you don't have an existing flagging system and want to identify which cases are most likely to have an error.

- **`INCL_find_inclusion_rules_multi_model_by_hh_size.R`** — runs RuleFit separately for each error type (earned overissuance, unearned overissuance, underissuance) and household size. Outputs rule CSVs to `inclusion_rules_by_hh_size/`. Start here, and if you have internal data already labeled as errors vs. clean, you may be able to stop here.
- **`INCL_optimize_single_inclusion_rule_by_hh_size_for_a_state.R`** — grid-searches one rule's thresholds for a specific state to maximize precision at a recall floor.
- **`INCL_optimize_set_of_inclusion_rules_by_hh_size_for_a_state.R`** — grid-searches all high-precision rules for a specific state at once.

The end goal is a state-specific version of:
`inclusion_rules_by_hh_size/final_by_HHsize_inclusion_rules_highprecision.csv`

The `code_for_single_model_combined_HH_sizes/` folder contains pooled-model counterparts. The stratified (by household size) approach consistently outperforms the pooled model on precision-recall.

---

## Data dictionary

Variable definitions and sourcing notes are in [`Definitions for variables used.txt`](Definitions%20for%20variables%20used.txt). This covers all model features (income variables, deductions, household composition indicators, benefit ratios, etc.) and maps them back to the raw QC data elements. Many thanks to Jesse Shaw for putting this together.

---

## Converting rules to SQL

**`parse_tree.R`** contains a `parse_tree_to_sql()` function that converts an rpart tree into a SQL `CASE` statement. Useful for implementing identified rules in a production case management system.

---

The main goal of putting up this repo is to make it unambiguous that anyone can freely use ideas / materials they've seen me present regarding SNAP QC. I'll continue adding to and cleaning up the code based on what's useful so please reach out!