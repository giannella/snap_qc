# Step 3b of plan.md (2026-10-31): the swaps in feature_swaps.R splice one frame's
# values into the other, and the mismatch itself marks error cases (recreation
# touched mostly errors), so single-feature swaps score above both frames. Here
# each changed feature (or group) is instead dropped from both frames, which
# keeps each frame internally consistent. If the f1027-f1030 gap closes when a
# feature is dropped, that feature carried what f1027 had.
#   SR_ROOT=C:/Users/ericg/snap_qc_v27 Rscript methods/munging_drop_diagnosis/drop_features.R
source("methods/munging_drop_diagnosis/probe_lib.R")
OUT <- "methods/munging_drop_diagnosis"
A <- probe_frame(FRAMES[["f1027"]]); B <- probe_frame(FRAMES[["f1030"]])
A <- A[A$fiscal_year %in% 2022:2024, ]; B <- B[B$fiscal_year %in% 2022:2024, ]
A <- A[match(B$key, A$key), ]
FULL <- VOCAB19
DROPS <- list(none = character(0), HH_size_n = "HH_size_n",
  total_deductions_by_hh_size = "total_deductions_by_hh_size", rawben_rel_max = "rawben_rel_max",
  unc_rawben_rel_max = "unc_rawben_rel_max", gross_by_hh_size = "gross_by_hh_size",
  earned_by_hh_size = "earned_by_hh_size", unearned_by_hh_size = "unearned_by_hh_size",
  shelter_expenses_by_hh_size = "shelter_expenses_by_hh_size", utilities_sua = "utilities_sua",
  income = c("earned_by_hh_size", "unearned_by_hh_size", "gross_by_hh_size"),
  deductions = c("total_deductions_by_hh_size", "shelter_expenses_by_hh_size", "medical_deductions", "utilities_sua"),
  benefit_ratios = c("rawben_rel_max", "unc_rawben_rel_max"),
  income_and_ratios = c("earned_by_hh_size", "unearned_by_hh_size", "gross_by_hh_size", "rawben_rel_max", "unc_rawben_rel_max"))
run <- function(d) do.call(rbind, lapply(117:119, function(s) probe(d, seed = s))) |> colMeans()
res <- list()
for (nm in names(DROPS)) {
  VOCAB19 <- setdiff(FULL, DROPS[[nm]])
  a <- run(A); b <- run(B)
  res[[nm]] <- data.frame(dropped = nm, n_features = length(VOCAB19),
    auc_f1027 = a[["auc"]], auc_f1030 = b[["auc"]], prec05_f1027 = a[["prec05"]], prec05_f1030 = b[["prec05"]],
    gap_prec05 = a[["prec05"]] - b[["prec05"]], dpf05_f1027 = a[["dpf05"]], dpf05_f1030 = b[["dpf05"]],
    gap_dpf05 = a[["dpf05"]] - b[["dpf05"]])
  stamp("%s done", nm)
}
VOCAB19 <- FULL
res <- do.call(rbind, res)
write.csv(res, file.path(OUT, "drop_features.csv"), row.names = FALSE)
print(res, digits = 3)
