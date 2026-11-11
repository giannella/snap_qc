# Readout: the 2026-10-16 frame (f1016) against the v2.7 final frame, FY2022-23
# mined -> FY2024 scored, national pool only, seeds 117 / 118 / 119.
# See design_note_f1016.md. The final frame's results are the ones on record
# from the 2026-10-02 run (same scripts; mining code unchanged since); the
# chain re-scores the final seed-117 pool with today's scripts and section 0
# checks it reproduces the record. Pairs are same-seed. Writes
# readout_f1016.md and readout_f1016_*.csv.
#   SR_ROOT=C:/Users/ericg/snap_qc_v27 Rscript methods/v270_seed_replicates/readout_f1016.R
source("methods/v270_seed_replicates/common.R")
D <- "methods/v270_seed_replicates"
FINAL_RULES_DIR <- "C:/Users/ericg/snap_qc/methods/v270_seed_replicates"   # the final frame's rule-level files (gitignored .rds)
out <- character(0); say <- function(...) { l <- sprintf(...); cat(l, "\n"); out <<- c(out, l) }
sfile <- function(fr, s) file.path(D, sprintf("states_%s_seed%d.csv", fr, s))
st <- function(fr, s) read.csv(sfile(fr, s))
HAVE <- SEEDS[file.exists(sfile("f1016", SEEDS)) & file.exists(sfile("final", SEEDS))]

say("# f1016 frame vs v2.7 final frame: FY2022-23 mined, FY2024 scored")
say("")
say("National pool only, 49 national-only state lists, same seeds paired. Generated %s.", format(Sys.time(), "%Y-%m-%d %H:%M %Z"))
say("Seeds with both arms: %s%s.", paste(HAVE, collapse = ", "),
    if (length(setdiff(SEEDS, HAVE))) sprintf(" (MISSING: %s)", paste(setdiff(SEEDS, HAVE), collapse = ", ")) else "")
say("")
say("Caveat: this is the one-year-ahead window, the secondary one. The primary window is three years ahead (FY2017-19 -> FY2022; process rule of 2026-10-06), and a one-year-ahead dip alone does not re-open a correctness fix.")
stopifnot(length(HAVE) > 0)

## 0. reproduction check: the final seed-117 pool re-scored with today's scripts
say("")
say("## 0. Reproduction check")
rf <- file.path(D, "states_final_rescore117.csv")
if (file.exists(rf)) {
  a <- read.csv(rf); b <- st("final", 117L)
  key <- c("state", "budget", "n_core", "n_buffer", "deepest_rank", "n_flagged", "n_errors_caught", "precision", "dollar_recall")
  m <- merge(a[, key], b[, key], by = c("state", "budget"), suffixes = c("_now", "_rec"))
  same <- with(m, n_core_now == n_core_rec & n_buffer_now == n_buffer_rec & deepest_rank_now == deepest_rank_rec &
                 n_flagged_now == n_flagged_rec & n_errors_caught_now == n_errors_caught_rec &
                 abs(precision_now - precision_rec) < 1e-4 & abs(dollar_recall_now - dollar_recall_rec) < 1e-4)
  say("Final frame, seed-117 pool re-scored today vs the 2026-10-02 record: %d of %d state-budget cells identical%s.",
      sum(same), nrow(m), if (all(same) && nrow(m) == 98) " (REPRODUCED)" else " (NOT REPRODUCED: the comparison below is not like-for-like)")
} else say("Re-score file missing (%s): reproduction NOT checked.", rf)

## 1. pooled over the 49 national-only state lists, per seed and budget
pooled <- do.call(rbind, lapply(HAVE, function(s) do.call(rbind, lapply(c("final", "f1016"), function(fr) {
  x <- st(fr, s)
  do.call(rbind, lapply(BUDGETS, function(b) { y <- x[x$budget == b, ]
    data.frame(frame = fr, seed = s, budget = b, states = nrow(y), flagged = sum(y$n_flagged),
               errors = sum(y$n_errors_caught), precision = round(sum(y$n_errors_caught) / sum(y$n_flagged), 4),
               median_state_precision = round(median(y$precision), 4),
               mean_dollar_recall = round(mean(y$dollar_recall), 4),
               states_below_base = sum(y$precision < y$base_rate_te),
               fill_gap_cells = sum(y$fill_gap_core > 0 | y$fill_gap_total > 0)) }))
}))))
write.csv(pooled, file.path(D, "readout_f1016_pooled.csv"), row.names = FALSE)
say("")
say("## 1. Pooled over 49 states (any-error precision; the target is any error, so frame-relative = any-error)")
say("")
say("| budget | seed | final errors / flagged | final precision | new errors / flagged | new precision | change in errors | mean dollar recall, final / new |")
say("|---|---|---|---|---|---|---|---|")
for (b in BUDGETS) for (s in HAVE) {
  f <- pooled[pooled$frame == "final" & pooled$seed == s & pooled$budget == b, ]
  n <- pooled[pooled$frame == "f1016" & pooled$seed == s & pooled$budget == b, ]
  say("| %.0f%% | %d | %d / %d | %.4f | %d / %d | %.4f | %+d | %.4f / %.4f |", 100 * b, s, f$errors, f$flagged, f$precision,
      n$errors, n$flagged, n$precision, n$errors - f$errors, f$mean_dollar_recall, n$mean_dollar_recall)
}
say("")
for (b in BUDGETS) {
  f <- pooled[pooled$frame == "final" & pooled$budget == b, ]; n <- pooled[pooled$frame == "f1016" & pooled$budget == b, ]
  say("- %.0f%%: final-frame seed range %.4f-%.4f (errors %d-%d); new frame %.4f-%.4f (errors %d-%d). New frame above every final seed: %s; below every final seed: %s.",
      100 * b, min(f$precision), max(f$precision), min(f$errors), max(f$errors), min(n$precision), max(n$precision),
      min(n$errors), max(n$errors), all(n$precision > max(f$precision)), all(n$precision < min(f$precision)))
}

## 2. paired by state (shipping companions: median, mean, harmed / helped tails at 0.05)
say("")
say("## 2. Paired by state (new minus final, same seed)")
say("")
say("| budget | seed | median change | mean change | states up / down | harmed (< -0.05) / helped (> +0.05) | dollar recall: median / mean change, harmed (< -0.05) | states below base rate, final / new |")
say("|---|---|---|---|---|---|---|---|")
pair_rows <- list()
for (b in BUDGETS) for (s in HAVE) {
  m <- merge(st("final", s)[st("final", s)$budget == b, ], st("f1016", s)[st("f1016", s)$budget == b, ],
             by = c("state", "budget"), suffixes = c("_final", "_new"))
  stopifnot(nrow(m) == 49)
  d <- m$precision_new - m$precision_final; dd <- m$dollar_recall_new - m$dollar_recall_final
  pair_rows[[length(pair_rows) + 1]] <- data.frame(state = m$state, budget = b, seed = s, precision_final = m$precision_final,
    precision_new = m$precision_new, change = round(d, 4), errors_final = m$n_errors_caught_final, errors_new = m$n_errors_caught_new,
    dollar_recall_change = round(dd, 4))
  say("| %.0f%% | %d | %+.4f | %+.4f | %d / %d | %d / %d | %+.4f / %+.4f, %d | %d / %d |", 100 * b, s, median(d), mean(d), sum(d > 0), sum(d < 0),
      sum(d < -0.05), sum(d > 0.05), median(dd), mean(dd), sum(dd < -0.05),
      sum(m$precision_final < m$base_rate_te_final), sum(m$precision_new < m$base_rate_te_new))
}
pairs <- do.call(rbind, pair_rows)
write.csv(pairs, file.path(D, "readout_f1016_states.csv"), row.names = FALSE)

say("")
say("Per-state change averaged over the %d seeds:", length(HAVE))
say("")
say("| budget | median | mean | harmed (< -0.05) / helped (> +0.05) | states down at every seed / up at every seed |")
say("|---|---|---|---|---|")
for (b in BUDGETS) {
  p <- pairs[pairs$budget == b, ]
  avg <- tapply(p$change, p$state, mean)
  dn <- tapply(p$change, p$state, function(x) all(x < 0)); up <- tapply(p$change, p$state, function(x) all(x > 0))
  say("| %.0f%% | %+.4f | %+.4f | %d / %d | %d / %d |", 100 * b, median(avg), mean(avg), sum(avg < -0.05), sum(avg > 0.05), sum(dn), sum(up))
}

## 3. seed movement on the final frame alone, for scale (different seeds; same-seed pairs share subsamples, so this scale is conservative)
say("")
say("## 3. For scale: the same paired statistics between two final-frame seeds (no frame change)")
say("")
say("| budget | seeds | median change | mean change | harmed / helped |")
say("|---|---|---|---|---|")
for (b in BUDGETS) for (pr in list(c(117L, 118L), c(117L, 119L), c(118L, 119L))) {
  m <- merge(st("final", pr[1])[st("final", pr[1])$budget == b, ], st("final", pr[2])[st("final", pr[2])$budget == b, ],
             by = c("state", "budget"), suffixes = c("_a", "_b"))
  d <- m$precision_b - m$precision_a
  say("| %.0f%% | %d -> %d | %+.4f | %+.4f | %d / %d |", 100 * b, pr[1], pr[2], median(d), mean(d), sum(d < -0.05), sum(d > 0.05))
}

## 4. rule level: the top of the national ranking on all FY2024 rows
say("")
say("## 4. Rule level: top of the national ranking, all FY2024 rows")
say("")
say("| frame | seed | admitted visible rules | top 200 precision | top 1,000 | top 3,000 | top 1,000 below their 99%% bound |")
say("|---|---|---|---|---|---|---|")
for (s in HAVE) for (fr in c("final", "f1016")) {
  fn <- file.path(if (fr == "final") FINAL_RULES_DIR else D, sprintf("rules_%s_seed%d.rds", fr, s))
  if (!file.exists(fn)) { say("| %s | %d | (missing %s) | | | | |", fr, s, fn); next }
  r <- readRDS(fn)$rules
  pk <- function(k) { y <- r[seq_len(min(k, nrow(r))), ]; sum(y$k_te) / sum(y$n_te) }
  y <- r[seq_len(min(1000, nrow(r))), ]
  nv <- st(fr, s)$n_visible[1]
  say("| %s | %d | %d | %.4f | %.4f | %.4f | %.3f |", fr, s, nv, pk(200), pk(1000), pk(3000),
      mean(ifelse(y$n_te > 0, y$k_te / y$n_te, NA) < y$lcb, na.rm = TRUE))
}
writeLines(out, file.path(D, "readout_f1016.md"))
cat("written", file.path(D, "readout_f1016.md"), "\n")
