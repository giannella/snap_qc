# v2.7 seed-replicate study, step 4: the readout. Reads the per-pool outputs of
# score_pool.R and score_pooled.R and writes readout.md plus readout_*.csv.
#   Rscript methods/v270_seed_replicates/readout.R
# Reading rule stated before the run (design_note.md). PRIMARY statistic: the
# pooled precision of the 49 national-only state lists at the 10% budget,
# direction final BELOW 09-25. It is called "beyond seed movement" only if all
# three final-frame values lie below all three 09-25-frame values. Every other
# statistic is descriptive. Three values against three is a coarse test: a
# result short of full separation is weak evidence of no frame effect.
suppressMessages(library(dplyr))
options(width = 250)
setwd("C:/Users/ericg/snap_qc")
SMOKE <- identical(Sys.getenv("SMOKE"), "1")
D <- "methods/v270_seed_replicates"; if (SMOKE) D <- file.path(D, "smoke")
FRAMES <- c("f0925", "final"); SEEDS <- c(117, 118, 119)
LAB <- c(f0925 = "09-25 frame", final = "final frame")
out <- character(0); say <- function(...) { s <- sprintf(...); cat(s, "\n", sep = ""); out <<- c(out, s) }
tab <- function(df) { x <- capture.output(print(as.data.frame(df), row.names = FALSE)); cat(x, sep = "\n"); out <<- c(out, "```", x, "```") }
# a = the three values on frame la, b = the three on frame lb (unpaired: every value of one against every value of the other)
sep_note <- function(a, b, la = "09-25", lb = "final") {
  stopifnot(length(a) == 3, length(b) == 3, !anyNA(a), !anyNA(b))
  if (min(b) > max(a)) sprintf("all three %s values above all three %s values", lb, la)
  else if (max(b) < min(a)) sprintf("all three %s values below all three %s values", lb, la)
  else "ranges overlap"
}
f4 <- function(x) paste(sprintf("%.4f", x), collapse = " / ")

say("# v2.7 seed replicates: frame difference against seed movement%s", if (SMOKE) " [SMOKE]" else "")
say("Generated %s. National FY2022-23 mines on the 09-25 frame and the final frame at seeds 117, 118, 119; scored on FY2024. National pool only: no state pools, so these numbers do not carry to the blended benchmark.", format(Sys.time(), "%Y-%m-%d %H:%M"))
say("The 09-25 and final frames differ in four ways at once: the fiscal-year keying of the CPI, the SUA tier computed on review-year amounts (439 FY2022-24 rows), October CPI values, and the $10 SUA-tier tolerance (593 rows). Sections 1-4 read them together; section 5 splits them into three steps, the first of which still carries the first two changes.")
say("")
say("## Anchors and bookkeeping")
anchor_same <- c()
for (f in FRAMES) { fn <- file.path(D, sprintf("anchor_pool_%s.csv", f))
  if (file.exists(fn)) { a <- strsplit(readLines(fn)[1], ",")[[1]]; anchor_same[f] <- a[2] == "identical"
    say("- %s, seed 117 re-mined vs the benchmark's cached pool: %s (%s vs %s rules; %.1f%% of rules shared).", LAB[[f]], a[2], a[3], a[4], 100 * as.numeric(a[5])) }
  else say("- %s: no anchor file.", LAB[[f]]) }
if (length(anchor_same) == 2 && all(anchor_same))
  say("- Both seed-117 pools reproduce the benchmark's, so seed 117 is the draw that raised the question; seeds 118 and 119 are the new draws.")

st <- bind_rows(lapply(FRAMES, function(f) bind_rows(lapply(SEEDS, function(s) read.csv(file.path(D, sprintf("states_%s_seed%d.csv", f, s)))))))
tab(st %>% group_by(frame, seed) %>% summarise(visible_rules = first(n_visible), tagged_and_dropped = first(n_tagged), states = n_distinct(state),
      cells_with_fill_gap = sum(fill_gap_core > 0 | fill_gap_total > 0), deepest_rank_used = max(deepest_rank), .groups = "drop"))

## ---- 1. national union curve, single seeds ----------------------------------
cur <- bind_rows(lapply(FRAMES, function(f) read.csv(file.path(D, sprintf("pooled_curve_%s.csv", f)))))
one <- cur %>% filter(seeds == 1)
say(""); say("## 1. The national pool's rules in rank order: precision when the flagged share of FY2024 cases first reaches each line")
say("A rule can carry the union past a line, so the flagged counts differ a little by pool; precision is the like-for-like figure, and errors and flagged counts are in pooled_curve_*.csv.")
tab(one %>% mutate(col = paste(frame, arm), precision = round(precision, 4)) %>% select(share, col, precision) %>% tidyr::pivot_wider(names_from = col, values_from = precision))
rd <- bind_rows(lapply(sort(unique(one$share)), function(s) {
  a <- one[one$share == s & one$frame == "f0925", ]; b <- one[one$share == s & one$frame == "final", ]
  data.frame(share = s, f0925_precision = f4(a$precision), final_precision = f4(b$precision),
             final_minus_f0925_mean = round(mean(b$precision) - mean(a$precision), 4),
             seed_sd_precision = round(sqrt((var(a$precision) + var(b$precision)) / 2), 4),
             f0925_errors = paste(a$errors, collapse = " / "), final_errors = paste(b$errors, collapse = " / "),
             flagged_range = paste(range(c(a$flagged, b$flagged)), collapse = "-"), reading = sep_note(a$precision, b$precision)) }))
tab(rd); write.csv(rd, file.path(D, "readout_union_curve.csv"), row.names = FALSE)

## ---- 2. 49-state walk, national pool only -----------------------------------
say(""); say("## 2. State lists from the national pool alone (shipped fill walk, 5%% and 10%% budgets)")
per <- st %>% group_by(budget, frame, seed) %>% summarise(states = n(), errors = sum(n_errors_caught), flagged = sum(n_flagged),
  pooled_precision = round(sum(n_errors_caught) / sum(n_flagged), 4), median_precision = round(median(precision), 4), mean_precision = round(mean(precision), 4),
  mean_dollar_recall = round(mean(dollar_recall), 4), below_base = sum(precision < base_rate_te), core_rules_median = median(n_core), .groups = "drop")
tab(per)
for (b in sort(unique(st$budget), decreasing = TRUE)) { a <- per$pooled_precision[per$budget == b & per$frame == "f0925"]; z <- per$pooled_precision[per$budget == b & per$frame == "final"]
  say("- %sBudget %.0f%%, pooled precision: 09-25 frame %s; final frame %s; %s.", if (b == 0.10) "PRIMARY. " else "", 100 * b, f4(a), f4(z), sep_note(a, z)) }
say("- A median state's 5%% list is about 40 FY2024 cases (binomial SE about 0.07), so the 0.05 harmed / helped cut sits inside one standard error; the pooled figures rest on about 1,950 flags at 5%% (SE about 0.011) and 3,930 at 10%% (SE about 0.007).")
pairs <- list()
key <- unique(st[, c("frame", "seed")]); key <- key[order(key$frame, key$seed), ]
for (i in seq_len(nrow(key))) for (j in seq_len(nrow(key))) {
  if (j <= i) next
  A <- key[i, ]; B <- key[j, ]
  for (b in sort(unique(st$budget))) {
    x <- merge(st[st$frame == A$frame & st$seed == A$seed & st$budget == b, ], st[st$frame == B$frame & st$seed == B$seed & st$budget == b, ], by = "state", suffixes = c("_a", "_b"))
    d <- x$precision_b - x$precision_a; dd <- x$dollar_recall_b - x$dollar_recall_a
    pairs[[length(pairs) + 1]] <- data.frame(kind = if (A$frame == B$frame) "seed only" else if (A$seed == B$seed) "frame, same seed" else "frame, different seeds",
      a = paste(A$frame, A$seed), b = paste(B$frame, B$seed), budget = b, states = nrow(x), d_precision_median = round(median(d), 4), d_precision_mean = round(mean(d), 4),
      n_pos = sum(d > 0), n_neg = sum(d < 0), harmed = sum(d < -0.05), helped = sum(d > 0.05), d_dollar_recall_mean = round(mean(dd), 4),
      d_errors = sum(x$n_errors_caught_b) - sum(x$n_errors_caught_a))
  }
}
pairs <- bind_rows(pairs)   # f0925 sorts before final, so every frame contrast is final minus 09-25
stopifnot(all(grepl("^f0925", pairs$a[pairs$kind != "seed only"])), all(grepl("^final", pairs$b[pairs$kind != "seed only"])))
write.csv(pairs, file.path(D, "readout_state_contrasts.csv"), row.names = FALSE)
say(""); say("Paired state changes (pool b minus pool a; frame contrasts are final minus 09-25; n_pos / n_neg = states up / down; harmed / helped = states beyond -0.05 / +0.05). For seed-only rows the sign is arbitrary.")
tab(pairs %>% arrange(budget, kind))
say(""); say("How large is a frame contrast next to a seed contrast (absolute size of the mean paired change, and the tails):")
tab(pairs %>% mutate(kind2 = ifelse(kind == "seed only", "seed only (6 contrasts)", "frame, final minus 09-25 (9 contrasts)")) %>% group_by(budget, kind2) %>%
  summarise(mean_abs_d_precision_mean = round(mean(abs(d_precision_mean)), 4), range_d_precision_mean = paste(sprintf("%+.4f", range(d_precision_mean)), collapse = " to "),
            harmed_range = paste(range(harmed), collapse = "-"), helped_range = paste(range(helped), collapse = "-"), abs_d_errors_mean = round(mean(abs(d_errors)), 1), .groups = "drop"))
say("The mean over the nine frame contrasts equals the difference of the two frames' three-seed means, so it is one number, not nine confirmations:")
tab(pairs %>% filter(kind != "seed only") %>% group_by(budget) %>% summarise(final_minus_f0925_mean_of_means = round(mean(d_precision_mean), 4), d_errors_mean = round(mean(d_errors), 1), .groups = "drop"))

## ---- 3. rule level -----------------------------------------------------------
say(""); say("## 3. Rule level: the top of each pool on FY2024")
rl <- list()
for (f in FRAMES) for (s in SEEDS) { r <- readRDS(file.path(D, sprintf("rules_%s_seed%d.rds", f, s))); p <- r$rules
  reach <- (p$n_te / r$te_strata_n[p$hh]) / (p$n / r$tr_strata_n[p$hh])
  for (K in c(200, 1000, 3000)) { i <- seq_len(min(K, nrow(p))); q <- p[i, ]; ok <- q$n_te >= 10
    rl[[length(rl) + 1]] <- data.frame(frame = f, seed = s, topK = K, median_train_n = median(q$n), flagwt_test_precision = round(sum(q$k_te) / sum(q$n_te), 4),
      share_under_bound = round(mean((q$k_te / q$n_te)[ok] < q$lcb[ok]), 4), median_margin = round(median((q$k_te / q$n_te)[ok] - q$lcb[ok]), 4),
      median_abs_log_reach = round(median(abs(log(reach[i][q$n_te > 0]))), 4), share_lt10_test_flags = round(mean(!ok), 4)) } }
rl <- bind_rows(rl); tab(rl); write.csv(rl, file.path(D, "readout_rule_level.csv"), row.names = FALSE)
for (K in c(200, 1000)) { a <- rl$share_under_bound[rl$topK == K & rl$frame == "f0925"]; z <- rl$share_under_bound[rl$topK == K & rl$frame == "final"]
  say("- Top %d, share of rules below their bound: 09-25 frame %s; final frame %s; %s.", K, f4(a), f4(z), sep_note(a, z)) }

## ---- 4. pooled seeds ---------------------------------------------------------
say(""); say("## 4. Pooling seeds: one seed's candidates, two seeds', all three (one BH pass per arm, same LCB ordering)")
say("Like-for-like: precision when the union first reaches a share of FY2024 cases.")
tab(cur %>% filter(share %in% c(0.01, 0.02, 0.05, 0.10)) %>% group_by(frame, share, seeds) %>%
      summarise(arms = n(), candidates = round(mean(candidates)), admitted = round(mean(admitted)), rules_used = round(mean(rules)), flagged = round(mean(flagged)),
                precision_mean = round(mean(precision), 4), precision_range = paste(sprintf("%.4f", range(precision)), collapse = "-"),
                errors_mean = round(mean(errors), 1), errors_range = paste(range(errors), collapse = "-"), .groups = "drop"))
cl <- bind_rows(lapply(FRAMES, function(f) read.csv(file.path(D, sprintf("pooled_calibration_%s.csv", f)))))
say(""); say("NOT like-for-like across arms: the top K rules reach less deep into a pooled ranking, so read this table for what the top is made of (support, bound, share below bound), not as a performance comparison.")
tab(cl %>% group_by(frame, topK, seeds) %>% summarise(median_train_n = round(mean(median_train_n), 1), share_train_n_lt50 = round(mean(share_train_n_lt50), 3),
      median_lcb = round(mean(median_lcb), 4), flagwt_test_precision = round(mean(flagwt_test_precision), 4), share_under_bound = round(mean(share_under_bound), 4),
      median_margin = round(mean(median_margin), 4), .groups = "drop"))
jc <- bind_rows(lapply(FRAMES, function(f) read.csv(file.path(D, sprintf("pooled_jaccard_%s.csv", f)))))
ns <- function(a) lengths(strsplit(ifelse(a == "pooled3", "a+b+c", a), "+", fixed = TRUE))
jc$kind <- paste0(ns(jc$arm_a), " vs ", ns(jc$arm_b), " seeds, ", jc$shared_seeds, " shared")
say(""); say("Overlap of the errors caught (Jaccard) between lists. Lists that share a seed overlap partly by construction:")
tab(jc %>% group_by(frame, share, kind) %>% summarise(pairs = n(), jaccard_mean = round(mean(jaccard_errors_caught), 3),
      jaccard_range = paste(sprintf("%.3f", range(jaccard_errors_caught)), collapse = "-"), .groups = "drop"))
p3 <- bind_rows(lapply(FRAMES, function(f) { fn <- file.path(D, sprintf("states_%s_pooled3.csv", f)); if (file.exists(fn)) read.csv(fn) else NULL }))
if (nrow(p3)) {
  say(""); say("State lists from the three-seed pooled pool against each single-seed pool (pooled minus single):")
  pr <- list()
  for (f in FRAMES) for (s in SEEDS) for (b in sort(unique(p3$budget))) {
    x <- merge(st[st$frame == f & st$seed == s & st$budget == b, ], p3[p3$frame == f & p3$budget == b, ], by = "state", suffixes = c("_single", "_pooled"))
    d <- x$precision_pooled - x$precision_single
    pr[[length(pr) + 1]] <- data.frame(frame = f, single_seed = s, budget = b, pooled_precision_single = round(sum(x$n_errors_caught_single) / sum(x$n_flagged_single), 4),
      pooled_precision_pooled3 = round(sum(x$n_errors_caught_pooled) / sum(x$n_flagged_pooled), 4), d_median = round(median(d), 4), d_mean = round(mean(d), 4),
      n_pos = sum(d > 0), n_neg = sum(d < 0), harmed = sum(d < -0.05), helped = sum(d > 0.05), d_dollar_recall_mean = round(mean(x$dollar_recall_pooled - x$dollar_recall_single), 4),
      core_rules_median_single = median(x$n_core_single), core_rules_median_pooled = median(x$n_core_pooled))
  }
  pr <- bind_rows(pr); tab(pr); write.csv(pr, file.path(D, "readout_pooled3_vs_single.csv"), row.names = FALSE)
}
## ---- 5. decomposition: the changes between the 09-25 and final frames, in three ordered steps ----
CORE <- c("f0925", "f0928", "f0929", "final")
SHORT <- c(nominal = "nominal", cpioff = "CPI-off", f0925 = "09-25", f0928 = "09-28", f0929 = "09-29", final = "final")
DLAB <- c(f0925 = "step 0: the CPI step introduced, with the 09-25 bundle (09-25 minus nominal)",
          f0928 = "step 1: fiscal-year CPI keying + SUA tier on review-year amounts (09-28 minus 09-25)",
          f0929 = "step 2: October CPI values (09-29 minus 09-28)",
          final = "step 3: $10 SUA-tier tolerance (final minus 09-29)")
have <- names(SHORT)[vapply(names(SHORT), function(f) all(file.exists(file.path(D, sprintf("states_%s_seed%d.csv", f, SEEDS)))), logical(1))]
DEC <- c(if ("nominal" %in% have) "nominal", CORE)
PAIRS <- lapply(2:length(DEC), function(i) list(A = DEC[i - 1], B = DEC[i], lab = DLAB[[DEC[i]]]))
if ("cpioff" %in% have) {
  PAIRS <- c(PAIRS, list(list(A = "cpioff", B = "final", lab = "clean: the CPI step alone (final minus CPI-off; same munging code, CPI switched off)")))
  if ("nominal" %in% have) PAIRS <- c(PAIRS, list(list(A = "nominal", B = "cpioff", lab = "clean: every non-CPI change since 09-21 (CPI-off minus nominal)")))
}
say(""); say("## 5. Decomposition: ordered steps from no CPI step (nominal) to the final frame (descriptive; no step has a primary statistic)")
say("Step 1 carries two changes: the fiscal-year keying of the CPI (October-December rows' dollar features) and the SUA tier computed on review-year amounts (439 FY2022-24 rows move from tier 2 to tier 1, 229 of them in the training years, 207 of them among the 593 rows step 3 moves back). Step 2 changes only the six CPI-adjusted dollar features. Step 3 changes only the tier on 593 rows (1 to 2) and the utilities, shelter and total-deduction amounts on those rows. The steps are measured in this order, each with the earlier changes in place, so they add up to the 09-25-to-final total.")
say("Step 0, when the no-CPI (nominal) frame is present, is itself a bundle: dollar amounts inflated to 2026 by the review month's calendar year with annual-average CPI and 2026 deduction tables, total deductions redefined to include the standard and homeless deductions, the ABAWD share over the reconstructed unit size (81 rows), and the SUA tier computed on inflated amounts (439 rows, reverted in step 1).")
say("The clean comparison, when the CPI-off frame is present: the CPI-off frame is the final frame's own munging code run with its CPI switch (cpi_inflate_vars) off, so 'final minus CPI-off' is the CPI step alone, and 'CPI-off minus nominal' is every other change since the 09-21 frame. The two add up to the nominal-to-final total.")
say("Frames with all three seeds scored: %s.", paste(have, collapse = ", "))
if (all(CORE %in% have)) {
  sd4 <- bind_rows(lapply(have, function(f) bind_rows(lapply(SEEDS, function(s) read.csv(file.path(D, sprintf("states_%s_seed%d.csv", f, s)))))))
  pd <- sd4 %>% group_by(budget, frame, seed) %>% summarise(errors = sum(n_errors_caught), flagged = sum(n_flagged),
          pooled_precision = round(sum(n_errors_caught) / sum(n_flagged), 4), mean_dollar_recall = round(mean(dollar_recall), 4), .groups = "drop") %>%
    mutate(frame = factor(frame, levels = unique(c(DEC, intersect("cpioff", have))))) %>% arrange(budget, frame, seed)
  say("Pooled precision of the 49 national-only state lists, by frame and seed:")
  tab(pd %>% select(budget, frame, seed, pooled_precision) %>% tidyr::pivot_wider(names_from = seed, values_from = pooled_precision, names_prefix = "seed_"))
  rl4 <- list(); top <- list()
  for (f in have) for (s in SEEDS) { r <- readRDS(file.path(D, sprintf("rules_%s_seed%d.rds", f, s))); p <- r$rules
    top[[paste(f, s)]] <- paste(p$hh, p$rule)[seq_len(min(1000, nrow(p)))]
    for (K in c(200, 1000)) { q <- p[seq_len(min(K, nrow(p))), ]; ok <- q$n_te >= 10
      rl4[[length(rl4) + 1]] <- data.frame(frame = f, seed = s, topK = K, flagwt_test_precision = round(sum(q$k_te) / sum(q$n_te), 4),
                                           share_under_bound = round(mean((q$k_te / q$n_te)[ok] < q$lcb[ok]), 4)) } }
  rl4 <- bind_rows(rl4)
  ovl <- function(x, y) length(intersect(x, y)) / 1000
  steps <- list(); rsteps <- list()
  for (pr in PAIRS) {
    A <- pr$A; B <- pr$B; LABEL <- pr$lab
    same_seed_shared <- vapply(SEEDS, function(s) ovl(top[[paste(A, s)]], top[[paste(B, s)]]), numeric(1))
    cross_seed_shared <- mean(c(ovl(top[[paste(B, 117)]], top[[paste(B, 118)]]), ovl(top[[paste(B, 117)]], top[[paste(B, 119)]]),
                                ovl(top[[paste(B, 118)]], top[[paste(B, 119)]])))
    for (b in sort(unique(sd4$budget))) {
      a <- pd$pooled_precision[pd$budget == b & pd$frame == A]; z <- pd$pooled_precision[pd$budget == b & pd$frame == B]
      dra <- pd$mean_dollar_recall[pd$budget == b & pd$frame == A]; drz <- pd$mean_dollar_recall[pd$budget == b & pd$frame == B]
      cs <- lapply(SEEDS, function(s) { x <- merge(sd4[sd4$frame == A & sd4$seed == s & sd4$budget == b, ], sd4[sd4$frame == B & sd4$seed == s & sd4$budget == b, ],
                                                   by = "state", suffixes = c("_a", "_b")); d <- x$precision_b - x$precision_a
        c(med = median(d), mean = mean(d), pos = sum(d > 0), neg = sum(d < 0), harmed = sum(d < -0.05), helped = sum(d > 0.05),
          derr = sum(x$n_errors_caught_b) - sum(x$n_errors_caught_a)) })
      cs <- do.call(rbind, cs); dz <- z - a
      steps[[length(steps) + 1]] <- data.frame(step = LABEL, budget = b, pooled_precision_before = f4(a), pooled_precision_after = f4(z),
        d_pooled_by_seed = paste(sprintf("%+.4f", dz), collapse = " / "), d_pooled_mean = round(mean(dz), 4),
        same_sign_of_3 = max(sum(dz > 0), sum(dz < 0)), d_errors_by_seed = paste(sprintf("%+d", cs[, "derr"]), collapse = " / "),
        d_state_mean_by_seed = paste(sprintf("%+.4f", cs[, "mean"]), collapse = " / "), d_state_median_by_seed = paste(sprintf("%+.4f", cs[, "med"]), collapse = " / "),
        states_up_down = paste(sprintf("%d/%d", cs[, "pos"], cs[, "neg"]), collapse = " , "), harmed_helped = paste(sprintf("%d/%d", cs[, "harmed"], cs[, "helped"]), collapse = " , "),
        d_mean_dollar_recall = round(mean(drz) - mean(dra), 4),
        top1000_shared_same_seed = paste(sprintf("%.2f", same_seed_shared), collapse = " / "), top1000_shared_across_seeds = round(cross_seed_shared, 2),
        reading = sep_note(a, z, SHORT[[A]], SHORT[[B]]))
    }
    a <- rl4$share_under_bound[rl4$topK == 1000 & rl4$frame == A]; z <- rl4$share_under_bound[rl4$topK == 1000 & rl4$frame == B]
    rsteps[[length(rsteps) + 1]] <- data.frame(step = LABEL, top1000_below_bound_before = f4(a), top1000_below_bound_after = f4(z),
      d_by_seed = paste(sprintf("%+.4f", z - a), collapse = " / "), d_mean = round(mean(z - a), 4), reading = sep_note(a, z, SHORT[[A]], SHORT[[B]]))
  }
  steps <- bind_rows(steps); rsteps <- bind_rows(rsteps)
  say("Each step, after minus before. Columns with three values give seeds 117 / 118 / 119 in order. same_sign_of_3 counts how many of the three same-seed pooled differences share a sign. top1000_shared gives the share of the top 1,000 rules the two frames have in common at the same seed, beside the share two different seeds have in common on the later frame: where the same-seed share is high the two pools are nearly the same pool and the same-seed differences are close to paired; where it is near the cross-seed share they are nearly independent draws.")
  tab(steps)
  say("Top 1,000 rules: share below their own bound on FY2024, by step:")
  tab(rsteps)
  tot <- pd %>% filter(frame %in% c("f0925", "final")) %>% mutate(frame = as.character(frame)) %>% group_by(budget, frame) %>%
    summarise(m = mean(pooled_precision), .groups = "drop") %>% tidyr::pivot_wider(names_from = frame, values_from = m) %>% mutate(total = round(final - f0925, 4))
  for (b in tot$budget) say("- Budget %.0f%%: steps 1-3 sum to %+.4f; the 09-25-to-final difference of means is %+.4f.", 100 * b,
                            sum(steps$d_pooled_mean[steps$budget == b & grepl("^step [1-3]", steps$step)]), tot$total[tot$budget == b])
  if ("nominal" %in% DEC) {
    totn <- pd %>% filter(frame %in% c("nominal", "final")) %>% mutate(frame = as.character(frame)) %>% group_by(budget, frame) %>%
      summarise(m = mean(pooled_precision), .groups = "drop") %>% tidyr::pivot_wider(names_from = frame, values_from = m) %>% mutate(total = round(final - nominal, 4))
    for (b in totn$budget) say("- Budget %.0f%%: steps 0-3 sum to %+.4f; the nominal-to-final difference of means (the whole v2.7 frame against no CPI step) is %+.4f.", 100 * b,
                               sum(steps$d_pooled_mean[steps$budget == b & grepl("^step [0-3]", steps$step)]), totn$total[totn$budget == b])
    if ("cpioff" %in% have) for (b in totn$budget)
      say("- Budget %.0f%%: the clean pair (non-CPI changes + the CPI step alone) sums to %+.4f, against the same nominal-to-final total %+.4f.", 100 * b,
          sum(steps$d_pooled_mean[steps$budget == b & grepl("^clean", steps$step)]), totn$total[totn$budget == b])
  }
  say("Reading with care: this section prints three separation readings per step (two budgets and the top-1,000 bound). With no real effect each shows full separation about 1 time in 10, so across all steps one or more will often do so by chance. Seed 117's column also adds up to the 09-25-to-final gap at seed 117, which was already known.")
  write.csv(steps, file.path(D, "readout_decomposition.csv"), row.names = FALSE)
  write.csv(rsteps, file.path(D, "readout_decomposition_bound.csv"), row.names = FALSE)
  write.csv(rl4, file.path(D, "readout_decomposition_rule_level.csv"), row.names = FALSE)
}
say(""); say("Limits: one test year (FY2024); three seeds per frame (for the 09-25 and final frames seed 117 is the draw already seen); national pool only; three against three cannot establish a small effect or rule one out; step 1 bundles two changes.")
writeLines(out, file.path(D, "readout.md"))
cat("\nreadout written:", file.path(D, "readout.md"), "\n")
