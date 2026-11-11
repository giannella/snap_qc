# Readout of the case-scoring-by-rule-votes confirmation (design_note.md,
# "Readout and bars"). Reads methods/vote_scoring/confirm/<window>/*.csv written
# by confirm_window.R and applies the bars. VS_SMOKE=1 reads the smoke folders.
#   Rscript methods/vote_scoring/confirm_readout.R  > methods/vote_scoring/confirm/readout.txt
# Pooling rules, as declared in the note or fixed here before any reading:
# - primary gain: the seed mean over 117-119; its variance is the mean
#   within-seed bootstrap variance plus (1 + 1/3) times the between-seed
#   variance; 95% interval mean +/- 1.96 SD.
# - harm (bar 4): the seed-mean harm count against the 95th percentile of the
#   seed-mean null (permutation i averaged over the three seeds), at 5% and 10%.
# - companions (bar 5): seed means of the within-state mean change in dollar
#   recall at 5% (score minus simple ordering) and of states up / down.
# - secondaries: z pooled by seed mean. Trend: the curve must rise in every size
#   group in both windows, so its p-value is the largest of the six one-sided
#   p-values. Size: the vote's coefficient must be positive in both windows, so
#   its p-value is the larger of the two. Holm over the two, family alpha 0.05.
suppressMessages(library(dplyr))
setwd("C:/Users/ericg/snap_qc")
SM <- identical(Sys.getenv("VS_SMOKE"), "1")
dir_of <- function(w) file.path("methods/vote_scoring/confirm", w, if (SM) "smoke" else "")
rd <- function(w, stem) bind_rows(lapply(list.files(dir_of(w), sprintf("^%s_seed[0-9]+\\.csv$", stem), full.names = TRUE), read.csv))
W <- c(fy2022 = "three years ahead (FY2017-19 rules -> FY2022; decides)", fy2024 = "one year ahead (FY2022-23 rules -> FY2024; must agree)")
out <- character(0); say <- function(...) { x <- sprintf(...); out <<- c(out, x); cat(x, "\n") }
pool_gain <- function(g, v) {
  m <- mean(g); T <- mean(v) + (1 + 1 / length(g)) * var(g)
  c(mean = m, sd = sqrt(T), lo = m - 1.96 * sqrt(T), hi = m + 1.96 * sqrt(T))
}
R <- list()
for (w in names(W)) {
  arms <- rd(w, "arms"); boot <- rd(w, "boot"); harm <- rd(w, "harm"); hn <- rd(w, "harm_null"); stt <- rd(w, "states")
  if (!nrow(arms)) { say("%s: no output yet", w); next }
  pc <- function(a, rdg = "all rows", b = 0.05) arms %>% filter(arm == a, reading == rdg, budget == b) %>% arrange(seed) %>% pull(per_case)
  g <- pc("full") / pc("simple") - 1; g2 <- pc("full", "artifact rows dropped") / pc("simple", "artifact rows dropped") - 1
  bv <- boot %>% group_by(seed) %>% summarise(v = var(gain), v2 = var(gain_no_mm)) %>% arrange(seed)
  P1 <- pool_gain(g, bv$v); P2 <- pool_gain(g2, bv$v2)
  nullm <- hn %>% group_by(perm) %>% summarise(h05 = mean(harm_05), h10 = mean(harm_10))
  hobs <- harm %>% group_by(budget) %>% summarise(h = mean(harm_observed)) %>% arrange(budget)
  hp95 <- c(quantile(nullm$h05, 0.95), quantile(nullm$h10, 0.95))
  comp <- stt %>% filter(budget == 0.05) %>% group_by(seed) %>%
    summarise(mean_change = mean(recall_full - recall_simple, na.rm = TRUE), up = sum(recall_full > recall_simple, na.rm = TRUE),
              down = sum(recall_full < recall_simple, na.rm = TRUE))
  R[[w]] <- list(g = g, g2 = g2, P1 = P1, P2 = P2, hobs = hobs$h, hp95 = hp95, comp = comp)
  say("")
  say("== %s ==", W[[w]])
  say("Top 5%% per state, error $ per reviewed case: score %s vs simple ordering %s (seeds 117/118/119)",
      paste(sprintf("$%.2f", pc("full")), collapse = "/"), paste(sprintf("$%.2f", pc("simple")), collapse = "/"))
  say("Gain by seed: %s; seed mean %.1f%% (95%% %.1f%% to %.1f%%; SD %.1f pts)", paste(sprintf("%.1f%%", 100 * g), collapse = " / "),
      100 * P1[["mean"]], 100 * P1[["lo"]], 100 * P1[["hi"]], 100 * P1[["sd"]])
  say("Artifact rows dropped: %s; seed mean %.1f%% (95%% %.1f%% to %.1f%%)", paste(sprintf("%.1f%%", 100 * g2), collapse = " / "),
      100 * P2[["mean"]], 100 * P2[["lo"]], 100 * P2[["hi"]])
  say("Harm count (states >%.2f worse in dollar recall), seed mean: %.1f at 5%% (null p95 %.1f), %.1f at 10%% (null p95 %.1f)",
      0.05, hobs$h[1], hp95[1], hobs$h[2], hp95[2])
  say("Within-state mean change in dollar recall at 5%%: %s; states up/down: %s", paste(sprintf("%+.4f", comp$mean_change), collapse = " / "),
      paste(sprintf("%d/%d", comp$up, comp$down), collapse = " , "))
  tab <- arms %>% filter(reading == "all rows") %>% group_by(arm, budget) %>%
    summarise(precision = mean(precision), dollar_recall = mean(dollar_recall), per_case = mean(per_case), mm_share = mean(mm_dollar_share), .groups = "drop") %>%
    arrange(budget, -per_case)
  say("Arms (seed means):"); for (i in seq_len(nrow(tab))) with(tab[i, ], say("  %4.0f%%  %-10s precision %.3f  dollar recall %.3f  $%.2f per case  artifact share %.3f",
                                                                        100 * budget, arm, precision, dollar_recall, per_case, mm_share))
  write.csv(tab, file.path(dir_of(w), "arms_seedmean.csv"), row.names = FALSE)
}
if (length(R) == 2) {
  a <- R$fy2022; b <- R$fy2024
  bar1 <- a$P1[["mean"]] >= 0.20 && a$P1[["lo"]] > 0.20
  bar2 <- a$P2[["mean"]] >= 0.20 && a$P2[["lo"]] > 0.20
  bar3 <- b$P1[["mean"]] > 0
  bar4 <- all(a$hobs <= a$hp95)
  contra <- mean(a$comp$mean_change) < 0 && any(a$hobs > a$hp95)
  say("")
  say("== Bars (deciding window: three years ahead) ==")
  say("1. seed-mean gain >= 20%% with 95%% low end > 20%%: %s (%.1f%%, low %.1f%%)", if (bar1) "PASS" else "FAIL", 100 * a$P1[["mean"]], 100 * a$P1[["lo"]])
  say("2. same with artifact rows dropped: %s (%.1f%%, low %.1f%%)", if (bar2) "PASS" else "FAIL", 100 * a$P2[["mean"]], 100 * a$P2[["lo"]])
  say("3. one year ahead seed-mean gain > 0: %s (%.1f%%)", if (bar3) "PASS" else "FAIL", 100 * b$P1[["mean"]])
  say("4. harm count at or below the null's 95th percentile at 5%% and 10%%: %s", if (bar4) "PASS" else "FAIL")
  say("5. companions do not contradict (negative within-state mean AND harm above null): %s", if (!contra) "PASS" else "FAIL")
  ships <- bar1 && bar2 && bar3 && bar4 && !contra
  say("Ships as a general workbook feature: %s", if (ships) "YES" else "NO")
  if (!ships && a$P1[["lo"]] > 0) say("Recorded as a real gain under the bar (95%% low end above 0) if bar 1 is what failed.")
  if (xor(a$P1[["lo"]] > 0, b$P1[["lo"]] > 0)) say("Gain whose interval clears 0 in one window only: recorded as an era result.")
  # secondaries
  sec <- bind_rows(lapply(names(W), function(w) rd(w, "secondary") %>% mutate(window = w)))
  zt <- sec %>% filter(test == "trend") %>% group_by(window, hh) %>% summarise(z = mean(z), .groups = "drop")
  zs <- sec %>% filter(test == "size_vote") %>% group_by(window) %>% summarise(z = mean(z), coef = mean(coef), .groups = "drop")
  p_tr <- max(pnorm(-zt$z)); p_sz <- max(pnorm(-zs$z))
  holm <- p.adjust(c(trend = p_tr, size = p_sz), "holm")
  say("")
  say("== Secondaries (Holm, family alpha 0.05) ==")
  say("Trend z by window x size group: %s; p = %.2g, Holm %.2g", paste(sprintf("%s %s %.1f", zt$window, zt$hh, zt$z), collapse = "; "), p_tr, holm[["trend"]])
  say("Vote in the error-size model: %s; p = %.2g, Holm %.2g", paste(sprintf("%s z %.1f (coef %.3f)", zs$window, zs$z, zs$coef), collapse = "; "), p_sz, holm[["size"]])
}
writeLines(out, file.path("methods/vote_scoring/confirm", if (SM) "readout_smoke.txt" else "readout.txt"))
