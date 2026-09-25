# Readout for the v2.7 frame measurement (runners/run_v270_cpi_bench.R):
# one-year-ahead precision, recall and dollar recall of the blended lists,
# nominal frame vs the v2.7 frame, per window (FY2024: mined FY2022-23;
# FY2019: mined FY2017-18), at the 5% and 10% review budgets. A measurement
# for the record of how the pipeline has evolved; no bars. Every readout
# carries the within-state median with its two companions (mean, harmed-tail
# count); for the recall rows the tail cut is also given on a relative basis,
# because the absolute 0.05 cut is calibrated for precision.
#
# WHAT VARIES (statistician review 2026-09-24): the "frame" is a BUNDLE of
# Ben's three commits, not CPI alone. Six mined features moved: five by the
# CPI inflation to 2026 dollars (median new/old 1.06-1.36 by year), and
# total_deductions_by_hh_size by a REDEFINITION as well (standard and homeless
# deductions added; median new/old 1.43-1.84). Label it "CPI + total-deductions
# redefinition". Rows, error flags, error dollars and utilities_sua are
# identical across arms (archive_data/munging_rebuild_run_2026-09-24.log), so
# any-error and frame-relative readings coincide. The fy2019 nominal arm is
# also the pending era-2 replication of ledger row 87 (utilities_sua under
# the max_sua anchor); era2_variant/ and era2_baseline/ hold only smokes.
#   Rscript methods/v270_cpi_benchmark/readout.R
# Writes readout_paired.csv (one row per window x budget x metric),
# readout_arms.csv (each arm's own medians beside the earlier benchmarks) and
# readout.md (the same, as text) into methods/v270_cpi_benchmark/.
suppressMessages(library(dplyr))
setwd("C:/Users/ericg/snap_qc")
D <- "methods/v270_cpi_benchmark"
LABEL <- "CPI + total-deductions redefinition (staging-v2.7 frame, rebuilt 2026-09-24) vs nominal (main, 2026-09-21 frame)"

load_arm <- function(fn) {
  if (!file.exists(fn)) return(NULL)
  b <- read.csv(fn, check.names = FALSE)
  b$n_errors_te <- round(b$base_rate_te * b$n_te)   # base_rate_te is 4-dp, n_te <= ~1,100: exact
  b$recall <- ifelse(b$n_errors_te > 0, b$n_errors_caught / b$n_errors_te, NA)
  stopifnot(all(b$fill_gap_core == 0), all(b$fill_gap_total == 0))
  b
}
summ <- function(x) c(median = median(x, na.rm = TRUE), mean = mean(x, na.rm = TRUE),
                      harmed = sum(x < -0.05, na.rm = TRUE), helped = sum(x > 0.05, na.rm = TRUE),
                      n = sum(!is.na(x)))
pooled <- function(b, m) switch(m,
  precision = sum(b$n_errors_caught) / sum(b$n_flagged),
  recall = sum(b$n_errors_caught) / sum(b$n_errors_te),
  dollar_recall = NA_real_)   # the benchmark CSV carries no dollar totals

paired <- list(); arms <- list()
md <- c("# v2.7 frame measurement: readout", "",
        sprintf("Generated %s. %s.", format(Sys.time(), "%Y-%m-%d %H:%M"), LABEL),
        "Blended lists (national + state pools), shipped recipe, seed 117; per window the two arms differ only in the frame file.",
        "Rows, error flags, error dollars and utilities_sua are identical across arms, so any-error and frame-relative readings coincide.",
        "Harmed / helped count states whose paired change is below -0.05 / above +0.05 (absolute); for recall rows a relative cut of -20% / +20% is also given, since 0.05 absolute is about 40% of a 5%-budget recall.", "")
for (w in c("fy2024", "fy2019")) {
  A <- load_arm(file.path(D, paste0(w, "_nominal"), "v250_benchmark_2024.csv"))
  B <- load_arm(file.path(D, paste0(w, "_cpi"), "v250_benchmark_2024.csv"))
  if (is.null(A) || is.null(B)) { md <- c(md, sprintf("## %s: arm(s) not finished", w), ""); next }
  md <- c(md, sprintf("## %s window (%s)", toupper(w),
                      if (w == "fy2024") "mined FY2022-23, walked FY2024" else "mined FY2017-18, walked FY2019; the nominal arm doubles as the era-2 replication of the utilities_sua tier under the max_sua anchor"), "",
          "| budget | metric | nominal median | v2.7 median | paired d median | d mean | harmed (< -0.05) | helped (> +0.05) | harmed rel (< -20%) | helped rel (> +20%) | states |",
          "|---|---|---|---|---|---|---|---|---|---|---|")
  for (b in c(0.05, 0.10)) {
    a <- A[A$budget == b, ]; z <- B[B$budget == b, ]
    j <- merge(a, z, by = "state", suffixes = c("_nom", "_cpi"))
    for (m in c("precision", "recall", "dollar_recall")) {
      d <- j[[paste0(m, "_cpi")]] - j[[paste0(m, "_nom")]]
      rel <- d / j[[paste0(m, "_nom")]]
      s <- summ(d)
      harmed_rel <- sum(rel < -0.20, na.rm = TRUE); helped_rel <- sum(rel > 0.20, na.rm = TRUE)
      paired[[length(paired) + 1]] <- data.frame(window = w, budget = b, metric = m,
        nominal_median = median(j[[paste0(m, "_nom")]], na.rm = TRUE),
        cpi_median = median(j[[paste0(m, "_cpi")]], na.rm = TRUE),
        d_median = s[["median"]], d_mean = s[["mean"]], harmed = s[["harmed"]],
        helped = s[["helped"]], harmed_rel20 = harmed_rel, helped_rel20 = helped_rel,
        n_states = s[["n"]], nominal_pooled = pooled(a, m), cpi_pooled = pooled(z, m),
        # per-state confounds the median hides: a state pool held or gated in one arm only
        states_pool_held_nom = sum(a$state_pool_held), states_pool_held_cpi = sum(z$state_pool_held),
        artifact_dropped_nom = sum(a$n_artifact_dropped), artifact_dropped_cpi = sum(z$n_artifact_dropped),
        state_rules_core_nom = sum(a$n_state_rules_core), state_rules_core_cpi = sum(z$n_state_rules_core))
      md <- c(md, sprintf("| %d%% | %s | %.4f | %.4f | %+.4f | %+.4f | %d | %d | %d | %d | %d |",
                          round(100 * b), m, median(j[[paste0(m, "_nom")]], na.rm = TRUE),
                          median(j[[paste0(m, "_cpi")]], na.rm = TRUE), s[["median"]], s[["mean"]],
                          s[["harmed"]], s[["helped"]], harmed_rel, helped_rel, s[["n"]]))
    }
    for (arm in c("nominal", "cpi")) {
      x <- if (arm == "nominal") a else z
      arms[[length(arms) + 1]] <- data.frame(window = w, arm = if (arm == "cpi") "v2.7 frame (CPI + total-deductions redefinition)" else "nominal frame", budget = b,
        median_precision = median(x$precision), mean_precision = mean(x$precision),
        median_recall = median(x$recall, na.rm = TRUE), median_dollar_recall = median(x$dollar_recall),
        pooled_precision = pooled(x, "precision"), pooled_recall = pooled(x, "recall"),
        below_base_rate = sum(x$precision < x$base_rate_te), n_states = nrow(x),
        state_pools_held = sum(x$state_pool_held), artifact_dropped_total = sum(x$n_artifact_dropped),
        state_rules_core_total = sum(x$n_state_rules_core))
    }
  }
  md <- c(md, "", "Pooled (flag-weighted) precision and recall, and the per-arm pool-held / artifact-dropped / state-rules-in-core totals, are in readout_arms.csv and readout_paired.csv.", "")
}

## the earlier benchmarks on the same window, for the evolution record
prior <- list(
  c("fy2024", "v2.5.0 recipe, raw utilities (2026-08-12 frame)", "methods/v250_benchmark_2024/v250_benchmark_2024.csv"),
  c("fy2024", "v2.6.0 vocabulary, utilities_sua modal anchor (2026-08-22 frame)", "methods/v250_benchmark_2024_utilrel/v250_benchmark_2024.csv"))
for (p in prior) {
  x <- load_arm(p[3]); if (is.null(x)) next
  for (b in c(0.05, 0.10)) {
    y <- x[x$budget == b, ]
    arms[[length(arms) + 1]] <- data.frame(window = p[1], arm = p[2], budget = b,
      median_precision = median(y$precision), mean_precision = mean(y$precision),
      median_recall = median(y$recall, na.rm = TRUE), median_dollar_recall = median(y$dollar_recall),
      pooled_precision = pooled(y, "precision"), pooled_recall = pooled(y, "recall"),
      below_base_rate = sum(y$precision < y$base_rate_te), n_states = nrow(y),
      state_pools_held = sum(y$state_pool_held), artifact_dropped_total = sum(y$n_artifact_dropped),
      state_rules_core_total = sum(y$n_state_rules_core))
  }
}
arms_df <- bind_rows(arms)
if (length(paired)) write.csv(bind_rows(paired), file.path(D, "readout_paired.csv"), row.names = FALSE)
write.csv(arms_df, file.path(D, "readout_arms.csv"), row.names = FALSE)
md <- c(md, "## Each arm beside the earlier benchmarks on the same window", "",
        "| window | arm | budget | median precision | mean precision | median recall | median $ recall | pooled precision | pooled recall | states below base rate |",
        "|---|---|---|---|---|---|---|---|---|---|")
for (i in seq_len(nrow(arms_df))) with(arms_df[i, ], md <<- c(md, sprintf(
  "| %s | %s | %d%% | %.4f | %.4f | %.4f | %.4f | %.4f | %.4f | %d / %d |", window, arm, round(100 * budget),
  median_precision, mean_precision, median_recall, median_dollar_recall, pooled_precision, pooled_recall,
  below_base_rate, n_states)))
md <- c(md, "", "Caveat: the national-only state x budget selection (methods/threearm_2024/) was measured on the v2.5.0 pool and nominal frame; the v2.7 national-only lists inherit it unmeasured.")
writeLines(md, file.path(D, "readout.md"))
cat(paste(md, collapse = "\n"), "\n")
