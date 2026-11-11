# §46 Amendment 2 driver (modeling_findings_detailed.md, §46 "Amendment 2", approved 2026-11-03).
# Usage: S46_OUT=out_a2 Rscript s46_a2.R <setup | tuneB | extra>
#   setup: copies Amendment 1's training-year, percentile and tuning-method choices (none uses lben) into the
#          new folder and hashes the code and ledger into manifest_a2.txt
#   tuneB: B's stage-1 tuning on the held-back year only (the method Amendment 1 chose)
#   extra: the offset-off check (B0) on all three tests and the voted-workbook reference (WB, from wb_ref.R) on
#          tests 2 and 3, scored with the frozen scoring code beside A and B. Neither can be chosen.
# The frozen driver s46_run.R runs the other phases unchanged; only its function definitions are read here.
here <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE))))
source(file.path(here, "s46_lib.R")); source(file.path(here, "s46_models.R"))
PHASE <- commandArgs(TRUE)[1]; stopifnot(PHASE %in% c("setup", "tuneB", "extra"), basename(OUT) != "out")
WN <- names(WINDOWS); if (SMOKE) WEIGHTS <- c(S0 = 1)
for (e in parse(file.path(here, "s46_run.R")))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))) eval(e)
say("=== a2 phase %s (smoke %s, out %s)", PHASE, SMOKE, basename(OUT))

if (PHASE == "setup") {
  src <- file.path(dirname(OUT), if (SMOKE) "out_smoke" else "out")
  for (f in c(paste0("choices_", WN, ".rds"), "method.rds")) stopifnot(file.copy(file.path(src, f), file.path(OUT, f), overwrite = FALSE))
  if (SMOKE) saveRDS(list(method = "holdout", Q = NA), f_rds("method"))   # the smoke folder's method; the real one is holdout
  stopifnot(method_of() == "holdout")
  files <- c(file.path(here, c("s46_lib.R", "s46_models.R", "s46_run.R", "s46_a2.R", "wb_ref.R", "s46_post.R", "run_a2.sh")),
             file.path(ROOT, "methods/modeling_findings_detailed.md"), file.path(OUT, paste0("choices_", WN, ".rds")), file.path(OUT, "method.rds"))
  h <- vapply(files, function(f) digest(file = f, algo = "sha256"), "")
  writeLines(c(sprintf("manifest_a2 written %s", format(Sys.time())), sprintf("%s  %s", h, sub(paste0(ROOT, "/"), "", files))),
             file.path(OUT, "manifest_a2.txt"))
  say("setup: choices and method copied from %s; %d files hashed", basename(src), length(files))
}

if (PHASE == "tuneB") for (wn in WN) {
  fn <- f_rds("S1B", wn, "holdout"); if (file.exists(fn)) next
  D <- win_data(wn); ch <- choices_of(wn); tr <- train_rows(D, wn); sp <- make_splits(D, tr, "holdout", tail(WINDOWS[[wn]]$train, 1))
  say("%s tuneB holdout: %d training rows", wn, length(tr))
  saveRDS(list(P = tune_P(D, sp, tr, ch$pct, TRUE, label = "B P holdout"), S = tune_size(D, sp, tr, ch$pct, TRUE, label = "B size holdout")), fn)
}

if (PHASE == "extra") { assert_frozen("manifest_freeze2.txt"); meth <- method_of()
  G_SIZE <- G_SIZE[!G_SIZE$off, ]   # B0: the six offset-off rows of B's size grid (tune_size reads the global grid)
  for (wn in WN) { D <- NULL; S1 <- readRDS(f_rds("S1", wn))
    for (seed in SEEDS) { fn <- cpath(wn, meth, "ed", "B0", seed); if (file.exists(fn)) next
      if (is.null(D)) { D <- win_data(wn); tr <- train_rows(D, wn); app <- app_rows(D, wn); pct <- choices_of(wn)$pct
        S0 <- cached(f_rds("S1B0size", wn), tune_size(D, make_splits(D, tr, meth, tail(WINDOWS[[wn]]$train, 1)), tr, pct, TRUE,
                                                      label = "B0 size (offset off)"))
        say("%s B0 size: depth %d, mcw %d, %d rounds (B: offset %s)", wn, S0$cfg$max_depth, S0$cfg$min_child_weight, S0$nr, S1$B$S$cfg$off) }
      t0 <- Sys.time(); saveRDS(fit_P(D, tr, app, S1$B$P, pct, TRUE, seed)$p * fit_size(D, tr, app, S0, pct, TRUE, seed), fn)
      say("%s %s B0 seed %d: %.1f min", wn, meth, seed, as.numeric(difftime(Sys.time(), t0, units = "mins"))) } }
  # WB: one fit per test (wb_ref.R); the same vector stands for every seed, so its seed variance is 0
  wbw <- intersect(WN, c("W2", "W3")); wbw <- wbw[file.exists(file.path(OUT, "wb", sprintf("wb_ed_%s.rds", wbw)))]
  for (wn in wbw) { w <- readRDS(file.path(OUT, "wb", sprintf("wb_ed_%s.rds", wn))); D <- win_data(wn); app <- app_rows(D, wn)
    stopifnot(length(w$score) == length(app), all(w$st == D$st[app]), all(w$ie == D$y[app]), all(is.finite(w$score)))
    for (s in SEEDS) saveRDS(w$score, cpath(wn, meth, "ed", "WB", s)) }
  say("WB scored on: %s", if (length(wbw)) paste(wbw, collapse = ",") else "none (wb_ref.R outputs missing)")
  S <- lapply(setNames(nm = WN), function(wn) score_window(wn, c("A", "B", "B0", if (wn %in% wbw) "WB"), meth))
  tab <- do.call(rbind, lapply(S, `[[`, "tab")); write.csv(tab, file.path(OUT, "s46_a2_extra_window_scores.csv"), row.names = FALSE)
  sv <- function(wn, cn) tab$L_seed_var[tab$window == wn & tab$cand == cn]
  Lw <- function(wn, cn) tab$L[tab$window == wn & tab$cand == cn]
  Qof <- function(cn, wins, w) sum(w * sapply(wins, Lw, cn = cn))
  se_diff <- function(a, b, wins, w) sqrt(sum(w^2 * sapply(wins, function(wn) var(S[[wn]]$boot[, a] - S[[wn]]$boot[, b]) + (sv(wn, a) + sv(wn, b)) / length(SEEDS))))
  w3 <- WEIGHTS[WN] / sum(WEIGHTS[WN])
  out <- data.frame(comparison = "B - B0, three tests (0.2/0.4/0.4)", Q_B = Qof("B", WN, w3), Q_other = Qof("B0", WN, w3),
                    diff = Qof("B", WN, w3) - Qof("B0", WN, w3), se = se_diff("B", "B0", WN, w3))
  if (length(wbw)) { we <- rep(1 / length(wbw), length(wbw))
    out <- rbind(out, data.frame(comparison = sprintf("B - WB, tests %s (equal weights)", paste(sub("W", "", wbw), collapse = "+")),
      Q_B = Qof("B", wbw, we), Q_other = Qof("WB", wbw, we), diff = Qof("B", wbw, we) - Qof("WB", wbw, we), se = se_diff("B", "WB", wbw, we)),
      data.frame(comparison = sprintf("A - WB, tests %s (equal weights)", paste(sub("W", "", wbw), collapse = "+")),
      Q_B = Qof("A", wbw, we), Q_other = Qof("WB", wbw, we), diff = Qof("A", wbw, we) - Qof("WB", wbw, we), se = se_diff("A", "WB", wbw, we))) }
  write.csv(out, file.path(OUT, "s46_a2_extra.csv"), row.names = FALSE)
  print(tab[, c("window", "cand", "L", "L_5pct", "prec5", "L_full", "slope", "shape")], digits = 3); print(out, digits = 4)
}
