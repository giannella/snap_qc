# Step 3 of plan.md (2026-10-31): feature swaps between f1027 and f1030.
#   SR_ROOT=C:/Users/ericg/snap_qc_v27 Rscript methods/munging_drop_diagnosis/feature_swaps.R
# "restore": f1030 with one feature (or group) put back to its f1027 values.
# "remove":  f1027 with one feature (or group) taken from f1030.
# Rows are matched on fiscal year x state x household number; the label stays
# the base frame's unless the swap is "label". Probe: FY2022-23 -> FY2024 and
# three xgboost seeds (117-119), averaged.
source("methods/munging_drop_diagnosis/probe_lib.R")
OUT <- "methods/munging_drop_diagnosis"
A <- probe_frame(FRAMES[["f1027"]]); B <- probe_frame(FRAMES[["f1030"]])
A <- A[A$fiscal_year %in% 2022:2024, ]; B <- B[B$fiscal_year %in% 2022:2024, ]
stopifnot(all(B$key %in% A$key)); stamp("f1027 rows not in f1030: %d", sum(!A$key %in% B$key))
A <- A[match(B$key, A$key), ]

changed <- VOCAB19[vapply(VOCAB19, function(v) {
  x <- as.numeric(A[[v]]); y <- as.numeric(B[[v]]); any(xor(is.na(x), is.na(y)) | abs(x - y) > 1e-6, na.rm = TRUE)
}, logical(1))]
stamp("changed features: %s", paste(changed, collapse = ", "))
SWAPS <- c(setNames(as.list(changed), changed), list(
  income = intersect(c("earned_by_hh_size", "unearned_by_hh_size", "gross_by_hh_size"), changed),
  deductions = intersect(c("total_deductions_by_hh_size", "shelter_expenses_by_hh_size", "medical_deductions", "utilities_sua"), changed),
  benefit_ratios = intersect(c("rawben_rel_max", "unc_rawben_rel_max"), changed),
  all_features = changed, label = "LABEL"))

swap <- function(base, donor, vars) {
  if (identical(vars, "LABEL")) { base$ie <- donor$ie; base$ed <- donor$ed; return(base) }
  for (v in vars) base[[v]] <- donor[[v]]
  base
}
run <- function(d) do.call(rbind, lapply(117:119, function(s) probe(d, seed = s))) |> colMeans()

res <- list(base_f1027 = run(A), base_f1030 = run(B))
for (nm in names(SWAPS)) {
  res[[paste0("restore_", nm)]] <- run(swap(B, A, SWAPS[[nm]]))
  res[[paste0("remove_", nm)]] <- run(swap(A, B, SWAPS[[nm]]))
  stamp("%s done", nm)
}
res <- data.frame(arm = names(res), do.call(rbind, res), row.names = NULL)
write.csv(res, file.path(OUT, "feature_swaps.csv"), row.names = FALSE)
print(res, digits = 3)
