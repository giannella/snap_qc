# Year-to-year stability check, the rule lists (2026-11-11). See run_stability.cmd beside this file.
# National rule lists from the seed-117 national pool mined on FY2017-19 + FY2022 (pools/admitted_f1109_seed117.rds,
# mine_national_seed.R under SR_WINDOW=stab23), built and scored by the gate stage of the f1109 rule-list benchmark
# (methods/reconstruction_income_piles/gate_removal_test.R, read from disk; its window w1719 is re-pointed here):
# artifact gate, pile gate on and off, 99% LCB order, fresh-share walk, 3x buffer, 5% and 10% budgets, one list per
# budget walked on the training rows. No state pools: each state's "blended" list equals its national list.
# Each list is scored on FY2023 and on FY2024, each year walked at its own budget, three ways: all cases, non-pile
# cases, and the kept cases (the scoring of runners/run_f1109_list_bench_kept.R, whose KEPT_BLOCK and KEPT_SWAPS
# are read from that file): flags = max(1, round(budget x the state's cases that year)), filled from kept cases
# only; errors and error dollars over kept cases. The gate script's reproduction check against a benchmark CSV is
# skipped (there is no benchmark run on this window).
#   [SMOKE=1] [SEL_STATES=Illinois,Maine] "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" methods/stability_fy2324/rules_stab.R
# Writes methods/stability_fy2324[/smoke]/gate_scores_w1719_rules[_smoke].csv.
ROOT <- "C:/Users/ericg/snap_qc_v27"; setwd(ROOT)
SMOKE_RUN <- identical(Sys.getenv("SMOKE"), "1")
OUT <- if (SMOKE_RUN) "methods/stability_fy2324/smoke" else "methods/stability_fy2324"
POOL_DIR <- file.path(OUT, "pools")

swap <- function(src, pairs) {
  for (p in pairs) {
    n <- lengths(regmatches(src, gregexpr(p[1], src, fixed = TRUE)))
    if (n != 1L) stop(sprintf("expected exactly one match of [%s], found %d", p[1], n))
    src <- sub(p[1], p[2], src, fixed = TRUE)
  }
  src
}
for (e in parse("runners/run_f1109_list_bench_kept.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]]) %in% c("KEPT_BLOCK", "KEPT_SWAPS")) eval(e)
stopifnot(exists("KEPT_BLOCK"), exists("KEPT_SWAPS"))

SWAPS <- c(list(
  c("stopifnot(nrow(reg_model_data) == 231619L)", "stopifnot(nrow(reg_model_data) == 237557L)"),
  c('setwd("C:/Users/ericg/snap_qc")', sprintf('setwd("%s")', ROOT)),
  # window w1719 re-pointed: FY2017-19 + FY2022 training, FY2023 and FY2024 test years, this check's pool
  c('w1719 = list(train = c("2017", "2018", "2019"), test = c("2022", "2023", "2024"),',
    'w1719 = list(train = c("2017", "2018", "2019", "2022"), test = c("2023", "2024"),'),
  c('cache = "methods/v270_cpi_benchmark/fy2022_cpi/cache"', sprintf('cache = "%s"', POOL_DIR)),
  c("expect_train = c(116060L, 10920L)", "expect_train = c(156990L, 15070L)"),
  c('sprintf("bench_national_%d.rds", SEED)', 'sprintf("admitted_f1109_seed%d.rds", SEED)'),
  c('own <- readRDS(file.path(W$cache, sprintf("bench_state_%s_%d.rds", gsub(" ", "_", state), SEED)))',
    "own <- natl[0, ]   # stability check: national lists only, no state pools"),
  c('OUT_DIR <- "methods/reconstruction_income_piles"', sprintf('OUT_DIR <- "%s"', OUT))),
  KEPT_SWAPS,
  list(c("## ---- reproduction check against the benchmark CSV", "if (FALSE) {"),
       c('cat("GATE REMOVAL TEST DONE\\n")', "}")))

src <- swap(paste(readLines("methods/reconstruction_income_piles/gate_removal_test.R", warn = FALSE), collapse = "\n"), SWAPS)
if (identical(Sys.getenv("PARSE_ONLY"), "1")) {
  invisible(parse(text = src)); cat("all swaps match once and the swapped script parses\n"); quit(save = "no")
}
stopifnot(file.exists(file.path(POOL_DIR, "admitted_f1109_seed117.rds")),
          identical(attr(readRDS(file.path(POOL_DIR, "admitted_f1109_seed117.rds")), "train_years"),
                    c("2017", "2018", "2019", "2022")),
          startsWith(digest::digest(file = "reg_model_data.rds", algo = "sha256"), "08e0e825"))
Sys.setenv(SEL_WINDOW = "w1719", SEL_TAG = if (SMOKE_RUN) "rules_smoke" else "rules")
cat(sprintf("[%s] gate_removal_test.R: running with %d substitutions\n", format(Sys.time(), "%H:%M:%S"), length(SWAPS)))
eval(parse(text = src), envir = globalenv())
stopifnot(all(res$list[res$gate == "on"] %in% c("blended", "national")))
cat(sprintf("[%s] rules done: %d rows\n", format(Sys.time(), "%H:%M:%S"), nrow(res)))
