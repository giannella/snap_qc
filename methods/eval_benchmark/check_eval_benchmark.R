# Checks additional_data/eval_benchmark_f1109.rds against the scoring it replaces (2026-11-12). Stops unless:
#   1. on each s46 test year (FY2019, FY2022, FY2024), the benchmark's state, error, error dollars and kept flag equal
#      the s46 scoring rows (keep_of(filter = "gap")) case by case;
#   2. score_ranking() on model B's stored scores reproduces methods/case_predictions/s46/out_f1109_eval/
#      B_f1109_by_state_5_10pct.csv (findings §51) for all three years, every state, 5% and 10%;
#   3. score_ranking() on the workbook vote score reproduces out_f1109_wb/WB_f1109_by_state_5_10pct.csv (FY2024);
#   4. on FY2022-24, the kept flag, error and error dollars equal those of the rule-list runs (the gate script's
#      KEPT_BLOCK, as stored in methods/included_rules_vote/refit/lists_seed118.rds).
# Nothing is refit; check 1-3 read stored scores. Run from the repository root:
#   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" methods/eval_benchmark/check_eval_benchmark.R
REPO <- getwd(); stopifnot(file.exists(file.path(REPO, "methods/eval_benchmark/score_ranking.R")))
source(file.path(REPO, "methods/eval_benchmark/score_ranking.R"))
bench <- read_benchmark(file.path(REPO, "additional_data/eval_benchmark_f1109.rds"))
stopifnot(startsWith(attr(bench, "frame_sha256"), "08e0e825"), nrow(bench) == 237557L)

here <- file.path(REPO, "methods/case_predictions/s46")
Sys.setenv(S46_OUT = "out_f1109_eval")
source(file.path(here, "s46_lib.R")); source(file.path(here, "s46_models.R"))   # sets the working directory to ROOT
for (e in parse(file.path(here, "s46_run.R")))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))) eval(e)
src <- paste(deparse(load_s46), collapse = "\n")
src <- sub("function (years)", "function (years, frame = FRAME)", src, fixed = TRUE)
src <- sub('FRAMES[["f1030"]]', "FRAMES[[frame]]", src, fixed = TRUE); src <- sub("231602L", "FRAME_ROWS[[frame]]", src, fixed = TRUE)
load_s46 <- eval(parse(text = src)); FRAME <- "f1109"
meth_B <- readRDS(file.path(here, "out_f1109_eval", "method.rds"))$method
Bf <- read.csv(file.path(here, "out_f1109_eval", "B_f1109_by_state_5_10pct.csv"), stringsAsFactors = FALSE)
WBf <- read.csv(file.path(here, "out_f1109_wb", "WB_f1109_by_state_5_10pct.csv"), stringsAsFactors = FALSE)
cols_n <- c("cases", "kept", "flags", "errors_caught", "errors_all"); cols_d <- c("dollars_caught", "dollars_all")
same_tab <- function(tab, ref, what) {
  m <- merge(tab, ref, by = c("budget", "state"), suffixes = c("", ".ref"))
  stopifnot(nrow(m) == nrow(tab), nrow(m) == nrow(ref))
  for (v in cols_n) if (!all(m[[v]] == m[[paste0(v, ".ref")]])) stop(what, ": ", v, " differs")
  for (v in cols_d) if (!all(abs(m[[v]] - m[[paste0(v, ".ref")]]) < 1e-6)) stop(what, ": ", v, " differs")
  cat(sprintf("  %s: %d state-budget rows reproduced (cases, kept, flags, errors and error dollars caught and available)\n",
              what, nrow(m)))
}

for (wn in names(WINDOWS)) {
  D <- win_data(wn); app <- app_rows(D, wn); k <- keep_of(D, app); y <- WINDOWS[[wn]]$score
  b <- bench[bench$fiscal_year == y, ]; b <- b[order(b$frame_row), ]
  stopifnot(nrow(b) == length(app), all(b$state == D$st[app]), all(b$error == D$y[app]),
            all(abs(b$error_dollars - D$d[app]) < 1e-9), all(b$kept == k))
  cat(sprintf("%s (FY%s): %d cases, %d kept, equal to the s46 rows case by case\n", wn, y, nrow(b), sum(b$kept)))
  scB <- rowMeans(sapply(SEEDS, function(sd) readRDS(file.path(here, "out_f1109_eval", "cache", sprintf("%s_%s_ed_B_%d.rds", wn, meth_B, sd)))))
  same_tab(score_ranking(bench, scB, y), Bf[Bf$window == wn, ], sprintf("B, FY%s", y))
  if (wn == "W3") {
    w <- readRDS(file.path(here, "out_f1109_wb", "wb", "wb_ed_W3.rds"))
    stopifnot(length(w$score) == nrow(b), all(w$st == b$state), all(w$ie == b$error))
    same_tab(score_ranking(bench, w$score, y), WBf, "vote score, FY2024")
    # the same through the keyed form of score_ranking()
    set.seed(1); sk <- data.frame(fiscal_year = y, state = b$state, hhldno = b$hhldno, score = w$score)[sample(nrow(b)), ]
    same_tab(score_ranking(bench, sk, y), WBf, "vote score, FY2024, scores joined by key in shuffled order")
  }
  rm(D); invisible(gc())
}

L <- readRDS(file.path(REPO, "methods/included_rules_vote/refit/lists_seed118.rds"))$cases
b <- bench[bench$fiscal_year %in% c("2022", "2023", "2024"), ]; b <- b[order(b$frame_row), ]
stopifnot(nrow(L) == nrow(b), all(as.character(L$year) == b$fiscal_year), all(L$st == b$state), all(L$ie == b$error),
          all(abs(L$ed - b$error_dollars) < 1e-9), all(L$kept == b$kept))
cat(sprintf("rule-list runs (gate script KEPT_BLOCK), FY2022-24: %d cases, %d kept, equal case by case\n", nrow(b), sum(b$kept)))
cat("ALL CHECKS PASSED\n")
