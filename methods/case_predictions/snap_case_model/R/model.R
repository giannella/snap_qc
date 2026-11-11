# Load the pre-trained model and score cases with it.
#
# The model has two parts, each an average of ten xgboost fits with different seeds:
#   P     the probability that the case has a payment error over the federal tolerance;
#   size  the error amount if there is one (a gamma model whose starting value is set by
#         the case's recomputed benefit).
# Expected error dollars = P x size. Both parts include the state, so a case is scored
# with its own state's term.

load_model <- function(path) {
  if (!requireNamespace("xgboost", quietly = TRUE))
    stop("the xgboost package is not installed; run install_packages.R first")
  m <- readRDS(path)
  stopifnot(length(m$models) == 10, !m$pct)
  have <- as.character(utils::packageVersion("xgboost"))
  if (numeric_version(have) < numeric_version("3.1"))
    warning(sprintf("the model was built with xgboost %s; this computer has %s. Run predict.R --check to confirm the scores reproduce.",
                    m$xgboost_version, have), call. = FALSE)
  m$boosters <- lapply(m$models, function(s) list(p = xgboost::xgb.load.raw(s$p), size = xgboost::xgb.load.raw(s$size)))
  m
}

# the model's input matrix: the 18 features, then one indicator column per state
design_matrix <- function(feat, state, m) {
  M <- as.matrix(feat[m$base_inputs]); storage.mode(M) <- "double"
  S <- matrix(0, nrow(M), length(m$states), dimnames = list(NULL, paste0("st_", make.names(m$states))))
  S[, paste0("st_", make.names(state))] <- 1
  M <- cbind(M, S); stopifnot(identical(colnames(M), m$inputs)); M
}

# P and size for each case. `benefit` is the benefit in the case's own year's dollars;
# sizes come back in FY2024 dollars.
score_cases <- function(feat, state, benefit, m) {
  M <- design_matrix(feat, state, m)
  dm <- xgboost::xgb.DMatrix(M)
  off <- m$size_offset_glm
  dms <- xgboost::xgb.DMatrix(M, base_margin = as.numeric(off[1] + off[2] * log1p(pmax(benefit, 0))))
  bag <- function(f) rowMeans(do.call(cbind, lapply(m$boosters, f)))     # a matrix even for a one-case file
  p <- bag(function(b) predict(b$p, dm))
  size <- bag(function(b) predict(b$size, dms))
  cpi_last <- unname(m$cpi_by_year[length(m$cpi_by_year)])   # training dollars -> FY2024 dollars
  data.frame(p_error = p, size = size / cpi_last)
}
