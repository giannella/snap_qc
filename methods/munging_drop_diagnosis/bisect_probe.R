# Step 4 of plan.md (2026-10-31): the probe on frames rebuilt after each group
# of Ben's recreation commits (built by the munging script in the detached
# worktree C:/Users/ericg/snap_qc_bisect into C:/Users/ericg/snap_qc_bisect_frames).
#   SR_ROOT=C:/Users/ericg/snap_qc_v27 Rscript methods/munging_drop_diagnosis/bisect_probe.R
source("methods/munging_drop_diagnosis/probe_lib.R")
OUT <- "methods/munging_drop_diagnosis"
DIR <- "C:/Users/ericg/snap_qc_bisect_frames"
CHECKPOINTS <- c(
  ec4d190 = "start (the f1027 build)",
  `0f9b028` = "+ nature 97/26, direction codes",
  `0aad422` = "+ elements 2-9 household size and utilities, utility allowances, refactor",
  `7e2e22d` = "+ homeless deduction, utilities data fix, more household-size elements",
  `0dff35a` = "+ element 150 nature 13, multi-element method",
  `95ef77a` = "+ 97 post-recreation, SMD, excluded members, direction tie-break, 520/361",
  `02ca411` = "+ no leftover amounts",
  `419044a` = "+ smoothing at the max/min benefit",
  `5f6690a` = "+ no small raises from zero",
  `2570391` = "+ rent at the shelter cap (= f1030)")
res <- list()
for (c in names(CHECKPOINTS)) {
  f <- file.path(DIR, sprintf("frame_%s.rds", c))
  if (!file.exists(f)) { stamp("missing %s", c); next }
  d <- probe_frame(f); d <- d[d$fiscal_year %in% 2022:2024, ]
  r <- colMeans(do.call(rbind, lapply(117:119, function(s) probe(d, seed = s))))
  res[[c]] <- data.frame(commit = c, step = CHECKPOINTS[[c]], errors_fy2024 = sum(d$ie[d$fiscal_year == 2024]),
                         t(r), sha8 = substr(tools::md5sum(f), 1, 8))
  stamp("%s done", c)
}
res <- do.call(rbind, res)
write.csv(res, file.path(OUT, "bisect_probe.csv"), row.names = FALSE)
print(res[, -2], digits = 3)
