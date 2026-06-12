# snap_qc
code and data for modeling SNAP payment errors using regression trees

Putting this out on my github with an Apache 2.0 license as an assurance that anyone can freely use and build upon the code, ideas, or results. 

Script 1_ is for recreating the dataset that I'm using in modeling. This is useful if you want to identify patterns using national data rather than data from a single state. 
Scripts 2 and 3 are for plotting regression trees in order to identify potential rules. This can be viewed as a form of exploratory data analysis. See the state_income_error_trees_any_timeper folder for examples. 
Scripts 4 and 5 are for identifying criteria for excluding cases from review. In other words, which cases are less likely to have an error. This is useful if you already have a list of cases to review and you want to make it more targeted. Start with your list of cases and these scripts will tell you what the risks are of various exclusion criteria based on historical data. 
Scripts 6, 7, and 8 are for identifying criteria for flagging cases for review. If you don't have an existing system for prioritizing cases for review, I would start here. I recommend using the by_HHsize_ versions of these scripts as the results in terms of precision-recall are across the board better.  
If you have a lot of internal data that is flagged as having an error or not, then you could use script 6 with your internal data and basically be done in one step. If you do not, you could use script 6 with national data or several states data to identify general rules, which you then tune one at a time (script 7) or all at once (script 8). 

If you're trying to draw conclusions from the trees you see here, it's very important to note that the regression trees only cover income-related errors (where element1 is one of the earned or unearned income errors). This is because the vast majority of the public QC data fields related to calculating benefit amounts reflect corrected data (including most, but not all, of the variables with the prefix "raw"). In order to make these analyses useful to states, we have to reconstruct uncorrected values (i.e., what would be there before a case went through the QC process). We do this for earned and unearned income errors in the first script (starts with "1_"). We're working on expanding this to cover other kinds of errors, but for now, the analyses here are limited to earned and unearned income errors. If you are using internal data, you can ignore the first script and just change the feature list in the second script to match the variables you have. 

1_data_munging_and_income_var_recovery.R is about feature engineering from the public data available at snapqcdata.net, including reconstructing the values just mentioned. 

2_state_reg_trees.R generates a png and pdf regression tree for each state. You can easily modify to just do one state. Modify the features list as appropriate.

3_regression_trees_income_errors.R generates very large pngs for three types of errors using data from all states (2022-2023, but easy to change): overissuance errors regarding earned income, overissuance errors regarding unearned income, and underissuance errors. The idea is to find patterns across all states that can be tested and optimized with internal state data using a grid search. I'll revise the grid search scripts if there's demand.  

The main goal of putting up this repo is to make it unambiguous that anyone can freely use ideas / materials they've seen me present regarding SNAP QC. I'll continue adding to and cleaning up the code based on what's useful so please reach out!