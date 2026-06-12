# snap_qc
code and data for modeling SNAP payment errors using regression trees

Putting this out on my github with an Apache 2.0 license as an assurance that anyone can freely use and build upon the code, ideas, or results. 

Script 1_ is for recreating the dataset that I'm using in modeling. This is useful if you want to identify patterns using national data rather than data from a single state. 

Scripts 2 and 3 are for plotting regression trees in order to identify potential rules. This can be viewed as a form of exploratory data analysis. See the state_income_error_trees_any_timeper folder for examples. 

Scripts 4 and 5 are for identifying criteria for excluding cases from review. In other words, which cases are less likely to have an error. This is useful if you already have a list of cases to review and you want to make it more targeted. Start with your list of cases and these scripts will tell you what the risks are of various exclusion criteria based on historical data. 

Scripts 6, 7, and 8 are for identifying criteria for flagging cases for review. If you don't have an existing system for prioritizing cases for review, I would start here. I recommend using the by_HHsize_ versions of these scripts as the results in terms of precision-recall are across the board better. If you have a lot of internal data that is flagged as having an error or not, then you could use script 6 with your internal data and basically be done in one step. If you do not, you could use script 6 with national data or several states data to identify general rules, which you then tune one at a time (script 7) or all at once (script 8). 

The main goal of putting up this repo is to make it unambiguous that anyone can freely use ideas / materials they've seen me present regarding SNAP QC. I'll continue adding to and cleaning up the code based on what's useful so please reach out!