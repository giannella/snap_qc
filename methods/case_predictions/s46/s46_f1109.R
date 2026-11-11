# B on frame f1109 (Ben's AMTERR labels, 2026-11-09): s46_f1104.R (§48) rerun with only the frame changed (Eric, 2026-11-10).
# The f1109 labels are AMTERR over tolerance; f1104's are |RAWBEN - FSBEN| over tolerance, so the comparison mixes both changes.
# prorated_i is not an input. Usage: S46_OUT=out_f1109_eval Rscript s46_f1109.R <setup | fit | score>
#   setup: copies B's stage-1 settings, the training-year and percentile choices and the tuning method from out_a2;
#          hashes the code, the ledger and both frames into manifest_f1104.txt
#   fit:   B on f1104, seeds 117-126, the three tests (the frozen fit code, via s46_run.R's function definitions)
#   score: B on f1109 and B on f1104 (out_f1104's fits), scored with the frozen code, plus the 5% workload figures
# The frozen files are not edited: load_s46 is re-read here with the frame swapped.
here <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE))))
source(file.path(here, "s46_lib.R")); source(file.path(here, "s46_models.R"))
PHASE <- commandArgs(TRUE)[1]; stopifnot(PHASE %in% c("setup", "fit", "score"), basename(OUT) == "out_f1109_eval", !SMOKE)
WN <- names(WINDOWS); A2 <- file.path(dirname(OUT), "out_a2")
for (e in parse(file.path(here, "s46_run.R")))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))) eval(e)

# load_s46 with the frame as an argument; everything else is the frozen text
src <- paste(deparse(load_s46), collapse = "\n")
stopifnot(grepl('FRAMES[["f1030"]]', src, fixed = TRUE), grepl("231602L", src, fixed = TRUE))
src <- sub("function (years)", "function (years, frame = FRAME)", src, fixed = TRUE)
src <- sub('FRAMES[["f1030"]]', "FRAMES[[frame]]", src, fixed = TRUE)
src <- sub("231602L", "FRAME_ROWS[[frame]]", src, fixed = TRUE)
load_s46 <- eval(parse(text = src)); FRAME <- "f1109"
F48 <- file.path(dirname(A2), "out_f1109_eval", "manifest_f1109.txt")
assert_frozen <- function(f) if (!file.exists(F48)) stop("run setup first")
say("=== f1109 phase %s (out %s)", PHASE, basename(OUT))

if (PHASE == "setup") {
  for (f in c(paste0("choices_", WN, ".rds"), "method.rds")) stopifnot(file.copy(file.path(A2, f), file.path(OUT, f), overwrite = FALSE))
  for (wn in WN) saveRDS(list(B = readRDS(file.path(A2, sprintf("S1_%s.rds", wn)))$B), f_rds("S1", wn))   # B's settings only
  stopifnot(method_of() == "holdout")
  files <- c(file.path(here, c("s46_lib.R", "s46_models.R", "s46_run.R", "s46_f1109.R")), file.path(ROOT, "methods/modeling_findings_detailed.md"),
             file.path(OUT, c(paste0("choices_", WN, ".rds"), paste0("S1_", WN, ".rds"), "method.rds")), FRAMES[["f1109"]], FRAMES[["f1104"]])
  h <- vapply(files, function(f) digest(file = f, algo = "sha256"), "")
  writeLines(c(sprintf("manifest_f1109 written %s", format(Sys.time())), sprintf("%s  %s", h, sub(paste0(ROOT, "/"), "", files))),
             file.path(OUT, "manifest_f1109.txt"))
  for (wn in WN) { s <- readRDS(f_rds("S1", wn))$B
    say("%s B settings: P depth %d mcw %d colsample %.1f, %d rounds; size depth %d mcw %d offset %s, %d rounds; old years %s, percentiles %s",
        wn, s$P$cfg$max_depth, s$P$cfg$min_child_weight, s$P$cfg$colsample_bytree, s$P$nr, s$S$cfg$max_depth, s$S$cfg$min_child_weight,
        s$S$cfg$off, s$S$nr, choices_of(wn)$old, choices_of(wn)$pct) }
}

if (PHASE == "fit") { assert_frozen(); run_fits("B", method_of(), function(wn) readRDS(f_rds("S1", wn))) }

if (PHASE == "score") { assert_frozen(); meth <- method_of()
  # the 5% workload: each state flags round(5% of its scored cases), taken from the top of the kept rows
  at5 <- function(sc, D, app) { k <- keep_of(D, app); dv <- D$d[app]; yv <- D$y[app]; SS <- split(seq_along(app), D$st[app])
    r <- t(vapply(SS, function(ix) { kk <- max(1L, round(0.05 * length(ix))); ix2 <- ix[k[ix]]
      sel <- ix2[order(-sc[ix2], ix2)[seq_len(min(kk, length(ix2)))]]
      c(n = length(ix), kept = length(ix2), flags = kk, err = sum(yv[sel]), dol = sum(dv[sel]), err_all = sum(yv[ix2]), dol_all = sum(dv[ix2])) }, numeric(7)))
    data.frame(states = nrow(r), cases = sum(r[, "n"]), kept = sum(r[, "kept"]), flags = sum(r[, "flags"]), errors_caught = sum(r[, "err"]),
      dollars_caught = sum(r[, "dol"]), precision = sum(r[, "err"]) / sum(r[, "flags"]), dollars_per_flag = sum(r[, "dol"]) / sum(r[, "flags"]),
      dollars_per_flag_state_mean = mean(r[, "dol"] / r[, "flags"]), error_rate_kept = sum(r[, "err_all"]) / sum(r[, "kept"]),
      dollars_per_case_kept = sum(r[, "dol_all"]) / sum(r[, "kept"]), dollar_recall = sum(r[, "dol"]) / sum(r[, "dol_all"]),
      error_recall = sum(r[, "err"]) / sum(r[, "err_all"])) }
  res <- list()
  F04 <- file.path(dirname(A2), "out_f1104")
  for (fr in c("f1109", "f1104")) { FRAME <- fr; OUT <- if (fr == "f1109") OUT else F04
    for (wn in WN) { s <- score_window(wn, "B", meth); D <- win_data(wn); app <- app_rows(D, wn)
      sc <- rowMeans(sapply(SEEDS, function(sd) readRDS(cpath(wn, meth, "ed", "B", sd))))
      res[[paste(fr, wn)]] <- cbind(frame = fr, window = wn, score_year = WINDOWS[[wn]]$score, s$tab[, c("L", "L_seed_var", "L_5pct", "prec5", "slope", "shape")],
                                    L_se_boot = sd(s$boot[, "B"]), at5(sc, D, app))
      say("%s %s: L %.3f, at 5%%: precision %.3f, $/flag %.0f, dollar recall %.3f", fr, wn, s$tab$L, res[[paste(fr, wn)]]$precision,
          res[[paste(fr, wn)]]$dollars_per_flag, res[[paste(fr, wn)]]$dollar_recall) } }
  OUT <- file.path(dirname(A2), "out_f1109_eval"); tab <- do.call(rbind, res); rownames(tab) <- NULL
  write.csv(tab, file.path(OUT, "B_f1109_vs_f1104.csv"), row.names = FALSE); print(tab, digits = 4)
}
