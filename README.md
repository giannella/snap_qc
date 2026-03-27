# snap_qc
code and data for modeling SNAP payment errors using regression trees

Putting this out on my github with an Apache 2.0 license as an assurance that anyone can freely use and build upon the code, the approaches it uses, and the results. Most of these scripts are a mess - so if all you want to do is plot regression trees, just used the R scripts that begin with 1_, 2_, and 3_ and they will pull in the associated scripts in the helper_functions folder. That will help you to generate the example figures in the two income_error_trees folders. 

Note that because so much of the public QC data reflects corrected amounts (including most, but not all, of the variables with the prefix "raw"), we have to reconstruct uncorrected values in order to have something that's similar to what a state agency would see prior to finding an error. We do this for earned and unearned income errors in the first script and so the resulting dataset is only focused on earned and unearned income errors. If you are using internal data, you can ignore the first script and just change the feature list in the second script to match the variables you have. 

If you're interested in using the public QC data referenced in the first script, it can be found here: 
https://github.com/AdamXiaoRose/SNAP_error_rate_support/blob/main/model/model_variables_new.csv

The main goal of putting up this repo is to make it unambiguous that anyone can freely use ideas / materials they've seen me present regarding SNAP QC. I'll continue adding to and cleaning up the code based on what's useful so please reach out. 

