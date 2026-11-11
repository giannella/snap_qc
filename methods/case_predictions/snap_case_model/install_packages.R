# One-time setup: installs the one R package the model needs, then runs the self-check.
#   Rscript install_packages.R
# The model was built with R 4.5.1 and xgboost 3.1.2.1 (see model/model_info.txt).
if (getRversion() < "4.3.0") stop("R 4.3 or newer is needed (xgboost 3.1 requires it); this is R ", getRversion())
if (!requireNamespace("xgboost", quietly = TRUE) || packageVersion("xgboost") < "3.1")
  install.packages("xgboost", repos = "https://cloud.r-project.org")
cat(sprintf("R %s, xgboost %s\n", getRversion(), packageVersion("xgboost")))
f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
here <- if (length(f)) dirname(normalizePath(f[1])) else getwd()   # run from this folder when sourced in RStudio
status <- system2(file.path(R.home("bin"), "Rscript"), c(shQuote(file.path(here, "predict.R")), "--check"))
if (status != 0) quit(status = 1)
