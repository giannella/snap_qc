# Challenger benchmark, step 1: export model B's data, frozen settings and stored scores (2026-11-12).
# Reuses the frozen s46 code the way methods/eval_benchmark/check_eval_benchmark.R does, so every challenger
# trains and is scored on exactly the rows and columns B used. Reads the staging checkout; writes only to
# this folder's data/. say() is never called, so nothing is appended to the s46 logs.
#   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" 01_export.R
HERE <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE))))
OUTD <- file.path(HERE, "data"); dir.create(OUTD, showWarnings = FALSE)
REPO <- "C:/Users/ericg/snap_qc_v27"
here <- file.path(REPO, "methods/case_predictions/s46")
Sys.setenv(S46_OUT = "out_f1109_eval")
source(file.path(here, "s46_lib.R")); source(file.path(here, "s46_models.R"))   # sets the working directory to ROOT
stopifnot(!SMOKE, basename(OUT) == "out_f1109_eval")
for (e in parse(file.path(here, "s46_run.R")))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))) eval(e)
src <- paste(deparse(load_s46), collapse = "\n")
stopifnot(grepl('FRAMES[["f1030"]]', src, fixed = TRUE), grepl("231602L", src, fixed = TRUE))
src <- sub("function (years)", "function (years, frame = FRAME)", src, fixed = TRUE)
src <- sub('FRAMES[["f1030"]]', "FRAMES[[frame]]", src, fixed = TRUE); src <- sub("231602L", "FRAME_ROWS[[frame]]", src, fixed = TRUE)
load_s46 <- eval(parse(text = src)); FRAME <- "f1109"
stopifnot(startsWith(digest(file = FRAMES[["f1109"]], algo = "sha256"), "08e0e825"))

source(file.path(REPO, "methods/eval_benchmark/score_ranking.R"))
bench <- read_benchmark(file.path(REPO, "additional_data/eval_benchmark_f1109.rds"))
stopifnot(startsWith(attr(bench, "frame_sha256"), "08e0e825"), nrow(bench) == 237557L)
meth <- method_of(); stopifnot(meth == "holdout")
Bref <- read.csv(file.path(OUT, "B_f1109_summary_5_10pct.csv"), stringsAsFactors = FALSE)

# Doubles are written at 17 significant digits so a reader recovers them bit for bit (ledger hazard: CSV consumers
# must parse round-trip or read the rds). col_hashes() lets the reader prove it: sha256 of each numeric column as
# little-endian bytes, blanks set to -9999.5.
write_exact <- function(df, f) { for (cn in names(df)) if (is.numeric(df[[cn]])) df[[cn]] <- ifelse(is.na(df[[cn]]), "", sprintf("%.17g", as.double(df[[cn]])))
  data.table::fwrite(df, f) }
col_hashes <- function(df) { nm <- names(df)[sapply(df, is.numeric)]
  data.frame(column = nm, sha256 = vapply(nm, function(cn) { x <- as.double(df[[cn]]); x[is.na(x)] <- -9999.5
    digest(writeBin(x, raw(), endian = "little"), algo = "sha256", serialize = FALSE) }, ""), stringsAsFactors = FALSE) }
settings <- list(); support <- list()
for (wn in names(WINDOWS)) {
  W <- WINDOWS[[wn]]; D <- win_data(wn); ch <- choices_of(wn); tr <- train_rows(D, wn); app <- app_rows(D, wn)
  last <- tail(W$train, 1); stopifnot(last == max(D$yr[tr]))
  rows <- c(tr, app)
  out <- data.frame(role = rep(c("train", "test"), c(length(tr), length(app))), yr = D$yr[rows], st = D$st[rows],
                    y = as.integer(D$y[rows]), d = D$d[rows], dd = D$dd[rows], lben = D$lben[rows],
                    keep = as.integer(keep_of(D, rows)), dm(D, rows, ch$pct, FALSE), check.names = FALSE)
  # the test rows are the benchmark's rows for that year, in frame_row order
  b <- bench[bench$fiscal_year == W$score, ]; b <- b[order(b$frame_row), ]
  stopifnot(nrow(b) == length(app), all(b$state == D$st[app]), all(b$error == D$y[app]),
            all(abs(b$error_dollars - D$d[app]) < 1e-9), all(b$kept == keep_of(D, app)))
  write_exact(out, file.path(OUTD, paste0(wn, ".csv"))); saveRDS(out, file.path(OUTD, paste0(wn, ".rds")))
  write.csv(col_hashes(out), file.path(OUTD, paste0(wn, "_colhash.csv")), row.names = FALSE)
  # B's stored scores, one column per seed, and a check that they reproduce the published summary
  scB <- sapply(SEEDS, function(sd) readRDS(cpath(wn, meth, "ed", "B", sd))); colnames(scB) <- paste0("s", SEEDS)
  stopifnot(nrow(scB) == length(app))
  write_exact(as.data.frame(scB), file.path(OUTD, paste0(wn, "_B_scores.csv")))
  s <- summarise_ranking(score_ranking(bench, rowMeans(scB), W$score)); r <- Bref[Bref$window == wn, ]
  stopifnot(all(abs(s$dollar_recall - r$dollar_recall[match(s$budget, r$budget)]) < 1e-9))
  S1 <- readRDS(f_rds("S1", wn))$B
  settings[[wn]] <- data.frame(window = wn, score_year = W$score, last_train_year = last, old = ch$old, pct = ch$pct,
    P_max_depth = S1$P$cfg$max_depth, P_min_child_weight = S1$P$cfg$min_child_weight, P_colsample_bytree = S1$P$cfg$colsample_bytree,
    P_rounds = S1$P$nr, S_max_depth = S1$S$cfg$max_depth, S_min_child_weight = S1$S$cfg$min_child_weight,
    S_offset = isTRUE(S1$S$cfg$off), S_rounds = S1$S$nr)
  fit <- tr[D$yr[tr] != last]; val <- tr[D$yr[tr] == last]; e <- function(r) sum(D$y[r] & D$dd[r] > 0)
  es <- tapply(D$y[tr] & D$dd[tr] > 0, D$st[tr], sum); ef <- tapply(D$y[fit] & D$dd[fit] > 0, D$st[fit], sum); ev <- tapply(D$y[val] & D$dd[val] > 0, D$st[val], sum)
  support[[wn]] <- data.frame(window = wn, train_years = paste(sort(unique(D$yr[tr])), collapse = "/"), train_rows = length(tr), train_errors = e(tr),
    tune_fit_rows = length(fit), tune_fit_errors = e(fit), tune_val_rows = length(val), tune_val_errors = e(val),
    test_rows = length(app), test_errors = sum(D$y[app]), test_kept = sum(keep_of(D, app)),
    state_train_errors_min = min(es), state_train_errors_median = median(es), state_fit_errors_min = min(ef), state_fit_errors_median = median(ef),
    state_val_errors_min = min(ev), state_val_errors_median = median(ev), inputs = ncol(out) - 8,
    na_cells_inputs = sum(is.na(out[, -(1:8)])))
  cat(sprintf("%s: %d train rows (%s), %d test rows (FY%s), %d inputs; B dollar recall at 5%%/10%% %.4f/%.4f reproduced\n", wn, length(tr),
              paste(sort(unique(D$yr[tr])), collapse = "/"), length(app), W$score, ncol(out) - 8, s$dollar_recall[1], s$dollar_recall[2]))
  rm(D); invisible(gc())
}
write.csv(do.call(rbind, settings), file.path(OUTD, "settings_B.csv"), row.names = FALSE)
write.csv(do.call(rbind, support), file.path(OUTD, "support.csv"), row.names = FALSE)
print(do.call(rbind, support))
cat("EXPORT DONE\n")
