# The 300 national vote rules a workbook carries (2026-10-29): the leading
# rule of each of the top 100 rule groups per household-size group, from one
# national pool, grouped on that pool's own mining years. Same definition as
# the study's representative vote, with count_divisible_by_100 rules dropped
# first (the workbook cannot compute that field; deploy_table.R does the same).
#   SR_WINDOW=fy2024 Rscript methods/vote_scoring/export_vote_rules.R    (interim: FY2022-23 pool, seed 117)
# Writes methods/vote_scoring/deploy/vote_rules_<years>.csv in the delivery-list
# columns rule_selection.py reads, plus group (e.g. "1-007": size group 1, group 7).
WINDOW_ARG <- Sys.getenv("SR_WINDOW"); stopifnot(WINDOW_ARG %in% c("fy2024", "deploy"))
source("methods/v270_seed_replicates/common.R")
# SR_FRAME (2026-10-29): the frame the pool was mined on; f1027 = staging frame eb989fda
FR <- Sys.getenv("SR_FRAME", "final"); stopifnot(FR %in% names(FRAMES))
source("methods/vote_scoring/vote_groups.R")
POOL <- Sys.getenv("VOTE_POOL", c(fy2024 = "methods/v270_cpi_benchmark/fy2024_cpi/cache/bench_national_117.rds",
                                  deploy = pool_file(FR, 117L))[[WINDOW]])   # POOL_DIR (smoke-aware)
OUT <- "methods/vote_scoring/deploy"
if (SMOKE) OUT <- file.path(OUT, "smoke")
dir.create(OUT, showWarnings = FALSE)

F <- load_frame(FR)
tr <- which(F$is_tr); ie_tr <- F$ie[tr]
pool <- read_pool(POOL, TRAIN_YEARS)
pool <- pool[!grepl("count_divisible_by_100", pool$rule, fixed = TRUE), , drop = FALSE]
rules <- top_rules(pool)
fl_tr <- flags_for_rules(rules, F$adf[tr, , drop = FALSE], strata_of(F$hh[tr]))
stopifnot(all(lengths(fl_tr) == rules$n), all(vapply(fl_tr, function(ix) sum(ie_tr[ix]), numeric(1)) == rules$k))
g <- build_groups(rules, fl_tr)
al <- allot(g, 300L, "rep")
stopifnot(nrow(al) == 300L)
out <- data.frame(rank = seq_len(nrow(al)), role = "vote", rule = al$rule, hh = al$hh, pool = "national",
                  engines = al$engines, mined_frames = al$mined_frames, n_flagged_train = al$n,
                  precision_train = round(al$k / al$n, 4), precision_train_lcb = round(al$lcb, 4),
                  group = sprintf("%s-%03d", al$hh, al$group))
fn <- file.path(OUT, sprintf("vote_rules_fy%s.csv", paste(range(as.integer(TRAIN_YEARS)), collapse = "_")))
write.csv(out, fn, row.names = FALSE)
stamp("%d vote rules from %s (mined on FY%s) -> %s", nrow(out), POOL, paste(TRAIN_YEARS, collapse = "+"), fn)
