# snap_qc
code and data for modeling SNAP payment errors using regression trees

Putting this out on my github with an Apache 2.0 license as an assurance that anyone can freely use and build upon it, the approaches used, and the results. Most of these scripts are a mess - but if all you want to do is plot regression trees, just use the R scripts that begin with "1_", "2_", and "3_" and they will pull in the associated scripts in the helper_functions folder. That will help you to generate the example figures in the two income_error_trees folders. 

If you're trying to draw conclusions from the trees you see here, it's very important to note that the regression trees only cover income-related errors (where element1 is one of the earned or unearned income errors). This is because the vast majority of the public QC data fields related to calculating benefit amounts reflect corrected data (including most, but not all, of the variables with the prefix "raw"). In order to make these analyses useful to states, we have to reconstruct uncorrected values (i.e., what would be there before a case went through the QC process). We do this for earned and unearned income errors in the first script (starts with "1_"). We're working on expanding this to cover other kinds of errors, but for now, the analyses here are limited to earned and unearned income errors. If you are using internal data, you can ignore the first script and just change the feature list in the second script to match the variables you have. 

1_data_munging_and_income_var_recovery.R is about feature engineering from the public data available at snapqcdata.net, including reconstructing the values just mentioned. 

2_state_reg_trees.R generates a png and pdf regression tree for each state. You can easily modify to just do one state. Modify the features list as appropriate.

3_regression_trees_income_errors.R generates very large pngs for three types of errors using data from all states (2022-2023, but easy to change): overissuance errors regarding earned income, overissuance errors regarding unearned income, and underissuance errors. The idea is to find patterns across all states that can be tested and optimized with internal state data using a grid search. I'll revise the grid search scripts if there's demand.  

If you're interested in using the public QC data exactly as it is in the first script, it can be found here (2017-2023): 
https://github.com/AdamXiaoRose/SNAP_error_rate_support/blob/main/model/model_variables_new.csv

The main goal of putting up this repo is to make it unambiguous that anyone can freely use ideas / materials they've seen me present regarding SNAP QC. I'll continue adding to and cleaning up the code based on what's useful so please reach out!
