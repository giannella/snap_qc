# Noise check for the case-scoring-by-rule-votes study (issues #16 / #21; plan doc of 2026-10-22, step 1).
# One-year-ahead window only, as the plan specifies: base rates from FY2022-23, ordering scored on FY2024.
# Simple ordering = issued benefit x household-size group's FY2022-23 over-threshold error rate.
# Each state sorts its own FY2024 cases and takes its top 1 / 2 / 5 / 10%; the flagged cases are pooled
# nationally and we compute error dollars per reviewed case and the share of error dollars found.
# Uncertainty: resample cases within each state 2,000 times (ties broken at random in each draw).
# No rule pool and no score is involved, so this opens nothing the study will test.
# Usage (repo root): Rscript methods/vote_scoring/noise_check.R reg_model_data.rds v27_1dbe8733
suppressPackageStartupMessages(library(dplyr))
args <- commandArgs(TRUE); FRAME <- args[1]; LABEL <- args[2]
d <- readRDS(FRAME)
g <- function(x) { x <- suppressWarnings(as.numeric(as.character(x))); x[is.na(x)] <- 0; x }
d <- d %>% mutate(fy = as.character(fiscal_year), st = as.character(state),
  ie = as.integer(!is.na(over_threshold) & over_threshold != 0), amt = ifelse(ie == 1, abs(g(total_error_amount)), 0),
  hh = ifelse(g(cert_HH_size_FS_n) <= 1, "1", ifelse(g(cert_HH_size_FS_n) <= 3, "2-3", "4+")), ben = g(rawben))
br <- d %>% filter(fy %in% c("2022", "2023")) %>% group_by(hh) %>% summarise(base = mean(ie))
te <- d %>% filter(fy == "2024") %>% left_join(br, by = "hh") %>% mutate(score = ben * base)
cat(sprintf("frame %s: FY2024 %d cases, %d errors, $%.0f error dollars, %d states\n", LABEL, nrow(te), sum(te$ie), sum(te$amt), n_distinct(te$st)))
print(as.data.frame(br))
BUD <- c(0.01, 0.02, 0.05, 0.10)
S <- split(seq_len(nrow(te)), te$st)
topk <- function(idx, score, b) { k <- max(1, round(b * length(idx))); idx[order(-score, runif(length(idx)))][seq_len(k)] }
one <- function(draw) {               # draw: list of resampled row indices per state
  sapply(BUD, function(b) {
    f <- unlist(lapply(draw, function(ix) topk(ix, te$score[ix], b)))
    tot <- sum(unlist(lapply(draw, function(ix) sum(te$amt[ix]))))
    c(per_case = sum(te$amt[f]) / length(f), share = sum(te$amt[f]) / tot, n = length(f))
  })
}
set.seed(20261027)
obs <- one(S)
perfect <- sapply(BUD, function(b) { f <- unlist(lapply(S, function(ix) topk(ix, te$amt[ix], b))); sum(te$amt[f]) / length(f) })
B <- 2000
boot <- replicate(B, one(lapply(S, function(ix) ix[sample.int(length(ix), replace = TRUE)])), simplify = "array")
res <- data.frame(frame = LABEL, budget = BUD, reviewed = obs["n", ], random_per_case = sum(te$amt) / nrow(te),
  simple_per_case = obs["per_case", ], se = apply(boot["per_case", , ], 1, sd),
  lo95 = apply(boot["per_case", , ], 1, quantile, 0.025), hi95 = apply(boot["per_case", , ], 1, quantile, 0.975),
  share_found = obs["share", ], perfect_per_case = perfect)
res$rel_se <- res$se / res$simple_per_case
# A 20% bar with its 95% low end above 20% needs a true gain of about 20% + 1.96 x SE(gain).
# SE(gain) on the same resampled cases depends on how correlated the two orderings are; bounds:
#   independent orderings: SE(ratio) ~ rel_se * sqrt(1 + 1.2^2)  (upper bound)
#   correlation 0.5:        SE(ratio) ~ rel_se * sqrt(1 + 1.44 - 1.2)
res$needed_gain_indep <- 0.20 + 1.96 * res$rel_se * sqrt(1 + 1.44)
res$needed_gain_r05 <- 0.20 + 1.96 * res$rel_se * sqrt(1 + 1.44 - 1.2)
print(res %>% mutate(across(where(is.numeric), ~ round(.x, 3))), row.names = FALSE)
write.csv(res, sprintf("methods/vote_scoring/noise_check_%s.csv", LABEL), row.names = FALSE)
cat("\nstate-level spread at the top 2% (median state reviews", median(sapply(S, function(ix) max(1, round(0.02 * length(ix))))), "cases):\n")
pc <- sapply(S, function(ix) { f <- topk(ix, te$score[ix], 0.02); sum(te$amt[f]) / length(f) })
print(round(quantile(pc, c(0, .1, .25, .5, .75, .9, 1)), 1)); cat("states with zero error dollars in their top 2%:", sum(pc == 0), "of", length(pc), "\n")
