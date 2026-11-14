# Follow-up run, BART run properly (2026-11-13). See design_note_followup.md.
#   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" 06_bart_proper.R hb|test [--smoke]
# hb    held-back years of W1 and W3 (FY2018, FY2023): the P model at k = 1, 2, 3, every sweep kept so the log loss
#       can be read block by block; the size model at last night's offset choice. No test-year row is scored.
# test  the P model at the k chosen in promoted.json (bart_choice.csv), 8 chains x (4,000 burn-in + 500 kept sweeps),
#       and the size model, 8 chains x (1,500 + 500), on all training years; one score file per chain.
# The loader, criteria and offset are 03_fit_bart.R's, taken verbatim (everything above its main loop), so the test
# year's labels are blanked at load here too.
suppressMessages(library(dbarts))
args0 <- commandArgs(TRUE); STAGE <- args0[1]; SMOKE <- "--smoke" %in% args0
HERE <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE))))
src <- readLines(file.path(HERE, "03_fit_bart.R")); cut <- grep('^say\\("=== bart on', src)[1]; stopifnot(!is.na(cut))
commandArgs <- function(trailingOnly = FALSE) if (trailingOnly) c("W1", if (SMOKE) "--smoke") else base::commandArgs(FALSE)   # 03's own argument parsing
eval(parse(text = src[seq_len(cut - 1)])); rm(commandArgs)
ROOT <- file.path(HERE, if (SMOKE) "smoke" else "", "followup"); HB <- file.path(ROOT, "heldback"); SC <- file.path(ROOT, "scores")
for (p in c(HB, SC, file.path(ROOT, "logs"))) dir.create(p, showWarnings = FALSE, recursive = TRUE)
logf <- file.path(ROOT, "logs", sprintf("06_bart_%s.log", STAGE))
NTREE <- if (SMOKE) 50L else 200L
# numbers written at 17 significant digits, so a score read back is the score computed (the ledger's CSV hazard)
w17 <- function(df, f) { for (cc in names(df)) if (is.numeric(df[[cc]])) df[[cc]] <- sprintf("%.17g", df[[cc]]); write.csv(df, f, row.names = FALSE, quote = FALSE) }
last_off <- function(w) { f <- file.path(HERE, "tuning", sprintf("bart_%s.csv", w)); if (file.exists(f)) isTRUE(as.logical(read.csv(f)$S_off[1])) else FALSE }   # last night's choice

# P model: probit BART with prior scale k. Returns the per-chain mean probability over the kept sweeps (cases x chains)
# and, when every sweep is kept, the chain-pooled probability by block of sweeps (cases x blocks).
# dbarts::bart runs its burn-in as one stored run and holds about 2 x training rows x burn-in sweeps x chains x 8 bytes
# while it does (38 GB for the first test at 8 chains x 4,000 sweeps, 94 GB for the third). bart_chunked is bart's own
# code path with the burn-in made in pieces of CHUNK sweeps, each dropped: the same sampler, the same sweeps, the same
# draws (equal to 1e-14 on a check with synthetic data), about a fifth of the memory or less. Added 2026-11-13 after
# the test stage was stopped for memory; see design_note_followup.md section 11.
CHUNK <- if (SMOKE) 25L else 250L
bart_chunked <- function(..., nskip, combinechains = FALSE) {
  sampler <- bart(..., nskip = nskip, combinechains = combinechains, sampleronly = TRUE); control <- sampler$control
  xt <- sampler$data@x.test; ot <- sampler$data@offset.test; keep <- control@keepTrainingFits; nsamp <- control@n.samples
  sampler$setTestPredictorAndOffset(NULL, NULL); control@keepTrainingFits <- FALSE; control@verbose <- FALSE; sampler$setControl(control)
  sampler$startThreads(); left <- as.integer(nskip)
  while (left > 0L) { n <- min(CHUNK, left); s <- sampler$run(0L, n, updateState = FALSE); rm(s); left <- left - n }
  sampler$setTestPredictorAndOffset(xt, ot); control@keepTrainingFits <- keep; sampler$setControl(control)
  samples <- sampler$run(0L, nsamp, updateState = FALSE); sampler$stopThreads()
  dbarts:::packageBartResults(sampler, samples, NULL, NULL, combinechains, FALSE) }
peak_gb <- function() tryCatch(as.numeric(system(sprintf("powershell.exe -NoProfile -Command \"(Get-Process -Id %d).PeakWorkingSet64/1GB\"", Sys.getpid()), intern = TRUE)), error = function(e) NA_real_)
bartP <- function(W, rows, app, k, nchain, nskip, ndpost, block = NULL) {
  base <- mean(W$y[rows])
  m <- (if (nskip > 0L) bart_chunked else bart)(W$X[rows, ], as.integer(W$y[rows]), W$X[app, ], ntree = NTREE, ndpost = ndpost, nskip = nskip, nchain = nchain, nthread = nchain, k = k,
            usequants = TRUE, keeptrainfits = FALSE, combinechains = FALSE, verbose = FALSE, binaryOffset = qnorm(base), seed = SEED)
  stopifnot(identical(dim(m$yhat.test), c(as.integer(nchain), as.integer(ndpost), length(app))))
  last <- if (is.null(block)) seq_len(ndpost) else (ndpost - block + 1L):ndpost
  p <- sapply(seq_len(nchain), function(ch) colMeans(pnorm(m$yhat.test[ch, last, , drop = TRUE])))
  if (mean(p) < base / 3 || mean(p) > base * 3) stop(sprintf("BART P mean %.4f is far from the training rate %.4f", mean(p), base))
  blocks <- if (is.null(block)) NULL else sapply(seq_len(ndpost %/% block), function(b) { ix <- ((b - 1L) * block + 1L):(b * block)
    rowMeans(sapply(seq_len(nchain), function(ch) colMeans(pnorm(m$yhat.test[ch, ix, , drop = TRUE])))) })
  list(p = p, blocks = blocks) }
bartS <- function(W, rows, app, off, nchain, nskip, ndpost) {
  e <- err_rows(W, rows); o <- if (is.null(off)) numeric(nrow(W$X)) else off
  m <- bart(W$X[e, ], log(W$dd[e]) - o[e], W$X[app, ], ntree = NTREE, ndpost = ndpost, nskip = nskip, nchain = nchain, nthread = nchain,
            usequants = TRUE, keeptrainfits = FALSE, combinechains = FALSE, verbose = FALSE, seed = SEED)
  stopifnot(identical(dim(m$yhat.test), c(as.integer(nchain), as.integer(ndpost), length(app))))
  sapply(seq_len(nchain), function(ch) colMeans(exp(m$yhat.test[ch, , ] + m$sigma[ch, ]^2 / 2)) * exp(o[app])) }   # lognormal mean, draw by draw

say("=== 06_bart_proper %s%s", STAGE, if (SMOKE) " (smoke)" else "")
if (STAGE == "hb") {
  KS <- c(1, 2, 3); NCH <- if (SMOKE) 2L else 4L; TOT <- if (SMOKE) 120L else 2000L; BLK <- if (SMOKE) 40L else 500L
  for (w in c("W1", "W3")) {
    W <- NULL
    for (k in KS) { f <- file.path(HB, sprintf("bart_hb_%s_k%d.csv", w, k)); if (file.exists(f)) next
      if (is.null(W)) W <- load_win(w)
      t0 <- Sys.time(); v <- W$val; r <- bartP(W, W$fit, v, k, NCH, 0L, TOT, BLK)
      out <- data.frame(st = W$st[v], r$p); names(out) <- c("st", paste0("p_c", seq_len(NCH))); w17(out, f)
      say("%s held-back P, k %g: log loss of the last %d sweeps %.5f; by block of %d sweeps: %s; %.1f min", w, k, BLK, wll(rowMeans(r$p), W$y[v], W$grp[v]), BLK,
          paste(sprintf("%.5f", apply(r$blocks, 2, function(p) wll(p, W$y[v], W$grp[v]))), collapse = " "), as.numeric(difftime(Sys.time(), t0, units = "mins"))) }
    f <- file.path(HB, sprintf("bart_hb_%s_size.csv", w))
    if (!file.exists(f)) { if (is.null(W)) W <- load_win(w)
      v <- W$val; ev <- err_rows(W, v); use <- last_off(w); off <- if (use) gamma_off(W, W$fit) else NULL
      s <- bartS(W, W$fit, v, off, NCH, if (SMOKE) 60L else 1500L, if (SMOKE) 40L else 500L)
      out <- data.frame(st = W$st[v], s); names(out) <- c("st", paste0("s_c", seq_len(NCH))); w17(out, f)
      say("%s held-back size (offset %s): gamma deviance %.5f", w, use, gamma_dev(rowMeans(s)[match(ev, v)], W$dd[ev])) } }
} else if (STAGE == "test") {
  ch <- read.csv(file.path(ROOT, "bart_choice.csv")); K <- as.numeric(ch$k[1]); B <- as.numeric(ch$b[1])
  NCH <- if (SMOKE) 2L else 8L; NSK <- if (SMOKE) 60L else 4000L; NDP <- if (SMOKE) 40L else 500L; NSKS <- if (SMOKE) 60L else 1500L
  say("k %g, size exponent %.3f, %d chains x (%d burn-in + %d kept sweeps)", K, B, NCH, NSK, NDP)
  for (w in c("W1", "W2", "W3")) {
    seeds <- SEED + seq_len(NCH) - 1L; sf <- file.path(SC, sprintf("bart_long_%s_%d.csv", w, seeds)); if (all(file.exists(sf))) next
    W <- load_win(w); t0 <- Sys.time(); use <- last_off(w); off <- if (use) gamma_off(W, W$tr) else NULL
    p <- bartP(W, W$tr, W$te, K, NCH, NSK, NDP)$p; say("%s P model done: %.1f min, peak memory so far %.1f GB", w, as.numeric(difftime(Sys.time(), t0, units = "mins")), peak_gb()); gc()
    s <- bartS(W, W$tr, W$te, off, NCH, NSKS, NDP)
    stopifnot(all(is.finite(p)), all(is.finite(s)), all(s > 0), all(p >= 0 & p <= 1))
    for (i in seq_len(NCH)) { w17(data.frame(st = W$st[W$te], p = p[, i], size = s[, i], ed = p[, i] * s[, i]), sf[i])
      if (B != 1) w17(data.frame(st = W$st[W$te], p = p[, i], size = s[, i], ed = p[, i] * s[, i]^B), file.path(SC, sprintf("bart_rev_%s_%d.csv", w, seeds[i]))) }
    cc <- cor(p * s); say("%s bart_long: %.1f min, mean p %.4f, median size %.0f, chain-to-chain score correlation %.4f, spread of chain mean p %.5f", w,
        as.numeric(difftime(Sys.time(), t0, units = "mins")), mean(p), median(s), mean(cc[upper.tri(cc)]), sd(colMeans(p))) }
} else stop("stage must be hb or test")
say("=== %s done", STAGE)
