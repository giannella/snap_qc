# Checks behind the exploration readout (2026-10-28): FY2019 random-order dollars per case, artifact rows in
# the curve's top bins (seed 117), and every confirmation pool's mining years (n and k re-derived for 300 rules).
#   Rscript methods/vote_scoring/explore_checks.R > methods/vote_scoring/fy2019/logs/explore_checks.log
Sys.setenv(SR_WINDOW = "fy2019")
source("methods/v270_seed_replicates/common.R"); source("methods/vote_scoring/vote_groups.R")
F <- load_frame("final"); te <- which(F$yr == "2019")
cat(sprintf("FY2019 random ordering: $%.2f per reviewed case\n", sum(F$ed[te]) / length(te)))
g <- readRDS("methods/vote_scoring/fy2019/explore_groups.rds")[["117"]]
al <- allot(g, 300L, "rep"); fl <- flags_for_rules(al, F$adf[te, ], strata_of(F$hh[te]))
v <- votes_of(al, fl, length(te))$vote; hh <- F$hh[te]; mm <- F$mm[te]; ie <- F$ie[te]
for (h in HH_LEVELS) { top <- hh == h & v >= c(`1` = 21, `2-3` = 6, `4+` = 3)[h]
  cat(sprintf("HH %s top bin: %d cases, %d errors, %d artifact rows (%d errors)\n", h, sum(top), sum(ie[top]), sum(mm[top]), sum(mm[top] & ie[top]))) }
# confirmation pools: training years by folder AND by recomputing n, k on the expected mining years for 300 rules each
chk <- function(fn, years) {
  p <- read_pool(fn, years)
  d <- readRDS(FRAMES[["final"]]); d <- d[as.character(d$fiscal_year) %in% years, ]
  d <- prep_features(d, VOCAB19)$data
  ie <- !is.na(d$over_threshold) & d$over_threshold != 0
  set.seed(1); s <- p[sort(sample(nrow(p), 300)), ]
  f <- flags_for_rules(s, d, strata_of(hh_group_of(d$cert_HH_size_FS_n)))
  ok <- mean(lengths(f) == s$n & vapply(f, function(ix) sum(ie[ix]), numeric(1)) == s$k)
  cat(sprintf("%-75s FY%s: %d rules; n and k reproduce on %.0f%% of 300 sampled\n", fn, paste(years, collapse = "+"), nrow(p), 100 * ok))
}
chk("methods/v270_cpi_benchmark/fy2022_cpi/cache/bench_national_117.rds", c("2017","2018","2019"))
for (s in 118:119) chk(sprintf("methods/v270_seed_replicates/fy2022/pools/admitted_final_seed%d.rds", s), c("2017","2018","2019"))
chk("methods/v270_cpi_benchmark/fy2024_cpi/cache/bench_national_117.rds", c("2022","2023"))
for (s in 118:119) chk(sprintf("methods/v270_seed_replicates/pools/admitted_final_seed%d.rds", s), c("2022","2023"))
