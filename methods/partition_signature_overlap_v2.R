# How much does the TOP of the ranked pool depend on which half we mined on?
#
# Reads the five cached national mines (each one an independent random 50/50
# split of FY2022-23) and compares the top N rules across partitions by
# SIGNATURE: the set of (variable, direction) pairs, thresholds discarded. That
# is the level at which two partitions can agree, since the log already shows
# exact rule text almost never repeats (97.8% of texts appear in one split of
# five) while structure usually does (72.9% of signatures appear in all five).
#
# The question here is narrower than that whole-pool statistic: does the TOP of
# the list, which is the only part the review budget ever reads, agree across
# partitions?
#
# Reads cached mines only. Mines nothing. Expects `reg_model_data` not at all.

suppressMessages(library(dplyr))
source("rule_mining_helpers.R")

MINES  <- "methods/state_similarity_v2/crossfit_ranking_train2223_test24/mines"
OUT    <- "methods/national_only_xfit"
LCB_Z  <- 2.326
TOPN   <- c(100, 500, 1000)
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

stamp <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"),
                                   sprintf(...)))

sig_of <- function(rules) vapply(rules, function(r) .rule_struct(r)$sig, character(1))

reps <- list()
for (i in 1:5) {
  r <- readRDS(file.path(MINES, sprintf("national_rep%d.rds", i)))
  r$lcb_hon  <- wilson_lcb(r$k_hon,  r$n_hon,  LCB_Z)
  r$lcb_self <- wilson_lcb(r$k_self, r$n_self, LCB_Z)
  reps[[i]] <- r
  stamp("rep %d: %d rules", i, nrow(r))
}

# Signatures are computed only for the rules we actually need (the union of the
# top 1000 by either statistic), because parsing 138k rules five times is slow
# and the question is about the top.
for (stat in c("lcb_hon", "lcb_self")) {
  stamp("=== ranking statistic: %s ===", stat)
  tops <- lapply(reps, function(r) {
    o <- order(-r[[stat]], r$hh, r$rule)
    r[o[seq_len(max(TOPN))], c("hh", "rule")]
  })
  sigs <- lapply(tops, function(t) paste(t$hh, sig_of(t$rule), sep = "|"))

  for (N in TOPN) {
    sN <- lapply(sigs, function(s) unique(s[seq_len(N)]))
    # pairwise Jaccard across the 10 partition pairs
    j <- c()
    for (a in 1:4) for (b in (a + 1):5) {
      inter <- length(intersect(sN[[a]], sN[[b]]))
      uni   <- length(union(sN[[a]], sN[[b]]))
      j <- c(j, inter / uni)
    }
    tab <- table(table(unlist(sN)))
    all5 <- sum(unlist(sN) %in% names(which(table(unlist(sN)) == 5)))
    cat(sprintf("  top %4d: distinct signatures per partition %s | pairwise Jaccard median %.3f (range %.3f-%.3f)\n",
                N, paste(vapply(sN, length, integer(1)), collapse = "/"),
                median(j), min(j), max(j)))
    present <- table(unlist(sN))
    cat(sprintf("            signatures present in 5/4/3/2/1 partitions: %s\n",
                paste(rev(as.integer(table(factor(present, levels = 1:5)))), collapse = "/")))
  }
}

# also: how much of the top N by out-of-fold bound is the SAME EXACT rule text
stamp("=== exact rule text, for contrast ===")
for (N in TOPN) {
  tx <- lapply(reps, function(r) {
    o <- order(-r$lcb_hon, r$hh, r$rule)
    unique(paste(r$hh[o[seq_len(N)]], r$rule[o[seq_len(N)]]))
  })
  j <- c()
  for (a in 1:4) for (b in (a + 1):5)
    j <- c(j, length(intersect(tx[[a]], tx[[b]])) / length(union(tx[[a]], tx[[b]])))
  cat(sprintf("  top %4d by text: pairwise Jaccard median %.4f\n", N, median(j)))
}
stamp("done")
