# Readout for one evaluation window (night 3, 2026-10-04): the CPI step alone
# (final minus CPI-off, same munging code) and the three ordered steps from the
# 09-25 frame to the final frame, for whatever frames and seeds finished.
# Reads states_<frame>_seed<seed>.csv and rules_<frame>_seed<seed>.rds from
# the window's folder; writes readout.md there. Descriptive; no bar.
#   SR_WINDOW=fy2022 Rscript methods/v270_seed_replicates/readout_window.R
suppressMessages(library(dplyr))
options(width = 250)
setwd("C:/Users/ericg/snap_qc")
WINDOW <- Sys.getenv("SR_WINDOW", "fy2022"); SMOKE <- identical(Sys.getenv("SMOKE"), "1")
D <- if (WINDOW == "fy2022") "methods/v270_seed_replicates/fy2022" else "methods/v270_seed_replicates"
if (SMOKE) D <- file.path(D, "smoke")
WLAB <- c(fy2022 = "mined FY2017-19, scored FY2022 (three years ahead across the excluded FY2020-21)", fy2024 = "mined FY2022-23, scored FY2024")
FR <- c(cpioff = "CPI-off", f0925 = "09-25", f0928 = "09-28", f0929 = "09-29", final = "final"); SEEDS <- c(117, 118, 119)
out <- character(0); say <- function(...) { s <- sprintf(...); cat(s, "\n", sep = ""); out <<- c(out, s) }
tab <- function(df) { x <- capture.output(print(as.data.frame(df), row.names = FALSE)); cat(x, sep = "\n"); out <<- c(out, "```", x, "```") }
f4 <- function(x) paste(sprintf("%.4f", x), collapse = " / ")
st_fn <- function(f, s) file.path(D, sprintf("states_%s_seed%d.csv", f, s))
avail <- expand.grid(frame = names(FR), seed = SEEDS, stringsAsFactors = FALSE)
avail <- avail[file.exists(st_fn(avail$frame, avail$seed)), ]
say("# v2.7 frames, %s: the CPI step and the three ordered steps%s", WINDOW, if (SMOKE) " [SMOKE]" else "")
say("Generated %s. Window: %s. National pool only, 49 state lists walked with the shipped fill, 5%% and 10%% budgets. The CPI comparison (final, CPI-off) uses seeds 117 / 118 / 119; the step frames start at seed 117. Seed 117 pools for the final and 09-28 frames are the cached FY2022-window benchmark pools (same recipe).",
    format(Sys.time(), "%Y-%m-%d %H:%M"), WLAB[[WINDOW]])
say("Finished: %s.", paste(sprintf("%s seed %d", avail$frame, avail$seed), collapse = ", "))
if (!nrow(avail)) { writeLines(out, file.path(D, "readout.md")); quit(save = "no") }
st <- bind_rows(lapply(seq_len(nrow(avail)), function(i) read.csv(st_fn(avail$frame[i], avail$seed[i]))))
per <- st %>% group_by(budget, frame, seed) %>% summarise(errors = sum(n_errors_caught), flagged = sum(n_flagged),
         pooled_precision = round(sum(n_errors_caught) / sum(n_flagged), 4), mean_dollar_recall = round(mean(dollar_recall), 4),
         below_base = sum(precision < base_rate_te), .groups = "drop") %>% mutate(frame = factor(frame, levels = names(FR))) %>% arrange(budget, frame, seed)
say(""); say("## Pooled precision of the 49 state lists, by frame and seed (errors caught / cases flagged)")
tab(per %>% mutate(cell = sprintf("%.4f (%d/%d)", pooled_precision, errors, flagged)) %>% select(budget, frame, seed, cell) %>%
      tidyr::pivot_wider(names_from = seed, values_from = cell, names_prefix = "seed_"))
top <- list(); rl <- list()
for (i in seq_len(nrow(avail))) { f <- avail$frame[i]; s <- avail$seed[i]; fn <- file.path(D, sprintf("rules_%s_seed%d.rds", f, s))
  if (!file.exists(fn)) next
  p <- readRDS(fn)$rules; top[[paste(f, s)]] <- paste(p$hh, p$rule)[seq_len(min(1000, nrow(p)))]
  q <- p[seq_len(min(1000, nrow(p))), ]; ok <- q$n_te >= 10
  rl[[length(rl) + 1]] <- data.frame(frame = f, seed = s, top1000_below_bound = round(mean((q$k_te / q$n_te)[ok] < q$lcb[ok]), 4),
                                     top1000_flagwt_test_precision = round(sum(q$k_te) / sum(q$n_te), 4)) }
contrast <- function(A, B, label, overlap = TRUE) {
  rows <- list()
  for (b in sort(unique(st$budget))) {
    seeds <- SEEDS[vapply(SEEDS, function(s) file.exists(st_fn(A, s)) && file.exists(st_fn(B, s)), logical(1))]
    if (!length(seeds)) next
    cs <- lapply(seeds, function(s) { x <- merge(st[st$frame == A & st$seed == s & st$budget == b, ], st[st$frame == B & st$seed == s & st$budget == b, ],
                                                 by = "state", suffixes = c("_a", "_b")); d <- x$precision_b - x$precision_a
      c(dprec = sum(x$n_errors_caught_b) / sum(x$n_flagged_b) - sum(x$n_errors_caught_a) / sum(x$n_flagged_a), derr = sum(x$n_errors_caught_b) - sum(x$n_errors_caught_a),
        med = median(d), mean = mean(d), pos = sum(d > 0), neg = sum(d < 0), harmed = sum(d < -0.05), helped = sum(d > 0.05),
        ddr = mean(x$dollar_recall_b - x$dollar_recall_a),
        shared = if (!is.null(top[[paste(A, s)]]) && !is.null(top[[paste(B, s)]])) length(intersect(top[[paste(A, s)]], top[[paste(B, s)]])) / 1000 else NA) })
    cs <- do.call(rbind, cs)
    a <- per$pooled_precision[per$budget == b & per$frame == A]; z <- per$pooled_precision[per$budget == b & per$frame == B]
    reading <- if (length(a) == 3 && length(z) == 3) { if (min(z) > max(a)) sprintf("all three %s above all three %s", FR[[B]], FR[[A]])
      else if (max(z) < min(a)) sprintf("all three %s below all three %s", FR[[B]], FR[[A]]) else "ranges overlap" } else sprintf("only seed(s) %s: no reading", paste(seeds, collapse = ", "))
    rows[[length(rows) + 1]] <- data.frame(comparison = label, budget = b, seeds = paste(seeds, collapse = "/"),
      d_pooled_precision_by_seed = paste(sprintf("%+.4f", cs[, "dprec"]), collapse = " / "), d_mean = round(mean(cs[, "dprec"]), 4),
      d_errors_by_seed = paste(sprintf("%+d", as.integer(cs[, "derr"])), collapse = " / "),
      d_state_mean_by_seed = paste(sprintf("%+.4f", cs[, "mean"]), collapse = " / "),
      d_state_median_by_seed = paste(sprintf("%+.4f", cs[, "med"]), collapse = " / "), states_up_down = paste(sprintf("%d/%d", cs[, "pos"], cs[, "neg"]), collapse = " , "),
      harmed_helped = paste(sprintf("%d/%d", cs[, "harmed"], cs[, "helped"]), collapse = " , "), d_mean_dollar_recall = round(mean(cs[, "ddr"]), 4),
      top1000_shared_same_seed = if (overlap) paste(sprintf("%.2f", cs[, "shared"]), collapse = " / ") else "n/a (cutoffs rescaled)", reading = reading)
  }
  bind_rows(rows)
}
say(""); say("## The CPI step alone, and the three steps (after minus before; values by seed in the order listed)")
# rule-text overlap only means something where the dollar cutoffs are on the same scale (step 3)
cmp <- bind_rows(contrast("cpioff", "final", "CPI step alone: final minus CPI-off (same code, CPI switched off)", overlap = FALSE),
                 contrast("f0925", "f0928", "step 1: fiscal-year CPI keying + SUA tier on review-year amounts (09-28 minus 09-25)", overlap = FALSE),
                 contrast("f0928", "f0929", "step 2: October CPI values (09-29 minus 09-28)", overlap = FALSE),
                 contrast("f0929", "final", "step 3: $10 SUA-tier tolerance (final minus 09-29)"),
                 contrast("f0925", "final", "steps 1-3 together: final minus 09-25", overlap = FALSE))
if (nrow(cmp)) tab(cmp) else say("No comparison has both frames finished at the same seed.")
if (length(rl)) { say(""); say("## Top 1,000 rules: share below their own confidence bound on the test year"); tab(bind_rows(rl) %>% arrange(factor(frame, levels = names(FR)), seed)) }
say(""); spread <- per %>% group_by(budget, frame) %>% filter(n() == 3) %>% summarise(range_errors = max(errors) - min(errors), .groups = "drop")
say("Reading: per-seed differences are paired over the same 49 states; a step measured at one seed only cannot be judged against seed variation.")
if (nrow(spread)) say("Seed yardstick in this window (range of errors caught across the three seeds of one frame): %s.",
                      paste(sprintf("%s %.0f%%: %d", FR[as.character(spread$frame)], 100 * spread$budget, spread$range_errors), collapse = "; "))
say("In the FY2024 window the same range at 10%% was 5 to 54 errors depending on the frame.")
write.csv(per, file.path(D, "readout_frames.csv"), row.names = FALSE)
if (nrow(cmp)) write.csv(cmp, file.path(D, "readout_comparisons.csv"), row.names = FALSE)
writeLines(out, file.path(D, "readout.md"))
cat("\nwritten:", file.path(D, "readout.md"), "\n")
