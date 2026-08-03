# What happens to admission if the multiplicity correction counts the SEARCHED
# space rather than the rules the trees reported?
#
# The Benjamini-Hochberg step-up at stage 5 divides by m, the number of
# candidate rules the ensembles reported (144,533 nationally on 2022-2023). The
# trees searched a far larger space, so that denominator makes the bar too easy.
# This script substitutes larger denominators and asks the only question that
# matters for a delivered list: does the top of the ranking survive?
#
# Reads the cached raw vocabulary. No mining, runs in seconds. Prints the table
# recorded in modeling_findings_detailed.md section 25.
suppressMessages(library(dplyr))
source("rule_mining_helpers.R")

RAW <- "methods/state_similarity_v2/transfer_benchmark_train2223_test24/fdr_raw_vocab"
ALPHA <- 0.10
LCB_Z <- 2.326
MIN_TRAIN_FLAGGED <- 30

nat  <- readRDS(file.path(RAW, "raw_national.rds"))
base <- attr(nat, "base_rates")[nat$hh]
pv   <- pbinom(nat$k - 1, nat$n, base, lower.tail = FALSE)
lcb  <- wilson_lcb(nat$k, nat$n, LCB_Z)

# BH with the denominator forced to m_eff, composed with the support floor
admit <- function(m_eff, alpha = ALPHA) {
  o <- order(pv)
  thr <- max(c(0L, which(pv[o] <= alpha * seq_along(pv) / m_eff)))
  a <- rep(FALSE, length(pv))
  if (thr > 0) a[o[seq_len(thr)]] <- TRUE
  a & nat$n >= MIN_TRAIN_FLAGGED
}

reported <- admit(length(pv))
ord <- which(reported)[order(-lcb[which(reported)])]   # the delivered ordering
cat(sprintf("reported denominator m = %d -> admitted %d\n\n", length(pv), sum(reported)))
cat(sprintf("%-14s %10s %14s %14s %13s\n", "denominator", "admitted",
            "top 25 kept", "top 100 kept", "top 1000 kept"))
for (M in c(length(pv), 1e6, 5.7e6, 3.4e7, 1e8)) {
  a <- admit(M)
  cat(sprintf("%-14.3g %10d %14d %14d %13d\n", M, sum(a),
              sum(a[head(ord, 25)]), sum(a[head(ord, 100)]), sum(a[head(ord, 1000)])))
}

# A simultaneous bound over m candidates would use this z instead of 2.326.
# The point of the table is how little it moves: three orders of magnitude in
# the search size move z by about 1.2.
cat("\nz implied by a simultaneous bound over m candidates, sqrt(2 log m):\n")
for (M in c(1.4e5, 1e6, 5.7e6, 3.4e7, 1e8))
  cat(sprintf("  m = %8.1e -> z = %.2f\n", M, sqrt(2 * log(M))))
