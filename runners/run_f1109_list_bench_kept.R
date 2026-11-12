# The f1109 rule-list benchmark (runners/run_f1109_list_bench.R) scored on the kept cases, the cases the vote score
# (WB) and model B are scored on in methods/case_predictions/s46/out_f1109_wb/WB_f1109_by_state_5_10pct.csv
# (2026-11-11; Eric: "score the rules on the votes' and model's cases"). Run after both stages of that runner: it
# reads the stage-1 pools and benchmark CSV from methods/f1109_list_bench/bench/.
# It is the runner's gate stage (gate_removal_test.R, window w2224, read from disk with the same f1109 swaps) plus
# a third scoring, "kept", beside "all" and "nonpile":
#   kept cases   s46 keep_of(filter = "gap"): not a mass-change case, not a pile case, not rebuilt from prior-year
#                tables, and a recreation gap of at most $5;
#   flags        WB's count, max(1, round(budget x all the state's FY2024 cases)), filled from kept cases only by
#                the benchmark's rule-by-rule walk (a rule is taken whole or skipped);
#   denominators errors and error dollars over kept cases.
# The run stops unless every state's kept cases, kept errors, kept error dollars and flag count equal WB's, and,
# as in the gate stage, unless the ungated blended arm on all cases reproduces the stage-1 CSV.
# Writes methods/f1109_list_bench/gate_scores_w2224_kept[_smoke].csv.
#   [SMOKE=1 SEL_STATES=Illinois,Maine] [PARSE_ONLY=1] "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" runners/run_f1109_list_bench_kept.R
ROOT <- "C:/Users/ericg/snap_qc_v27"; setwd(ROOT)
SMOKE_RUN <- identical(Sys.getenv("SMOKE"), "1")
OUT <- "methods/f1109_list_bench"; BENCH_DIR <- file.path(OUT, "bench")
BENCH_RUN_DIR <- if (SMOKE_RUN) file.path(BENCH_DIR, "smoke") else BENCH_DIR
stopifnot(unname(tools::md5sum("reg_model_data.rds")) == unname(tools::md5sum("additional_data/reg_model_data.rds")),
          startsWith(digest::digest(file = "reg_model_data.rds", algo = "sha256"), "08e0e825"))

swap <- function(src, pairs) {
  for (p in pairs) {
    n <- lengths(regmatches(src, gregexpr(p[1], src, fixed = TRUE)))
    if (n != 1L) stop(sprintf("expected exactly one match of [%s], found %d", p[1], n))
    src <- sub(p[1], p[2], src, fixed = TRUE)
  }
  src
}
GATE <- "methods/reconstruction_income_piles/gate_removal_test.R"

# the gate stage's f1109 swaps, as in run_f1109_list_bench.R
GATE_SWAPS <- list(
  c("stopifnot(nrow(reg_model_data) == 231619L)", "stopifnot(nrow(reg_model_data) == 237557L)"),
  c('setwd("C:/Users/ericg/snap_qc")', sprintf('setwd("%s")', ROOT)),
  c('cache = "methods/v270_cpi_benchmark/fy2024_cpi/cache"', sprintf('cache = "%s/cache"', BENCH_RUN_DIR)),
  c('bench = "methods/v270_cpi_benchmark/fy2024_cpi/v250_benchmark_2024.csv"',
    sprintf('bench = "%s/v250_benchmark_2024.csv"', BENCH_RUN_DIR)),
  c("expect_train = c(76031L, 8397L)", "expect_train = c(77750L, 8477L)"),
  c('OUT_DIR <- "methods/reconstruction_income_piles"', sprintf('OUT_DIR <- "%s"', OUT)))

# the kept scoring
KEPT_BLOCK <- paste(
  'pile_all <- income_pile_rows(adf0)',
  'kgap <- abs(adf$rawben - adf$raw_benefit_amount); kgap[is.na(kgap)] <- 0',
  'kmm <- adf$rawben >= adf$benmax & adf$rawben_uncapped < adf$benmax; kmm[is.na(kmm)] <- FALSE',
  'knotes <- as.character(adf$correctednotes); knotes[is.na(knotes) | knotes == ""] <- "no_change"',
  'kpile <- pile_all; kpile[is.na(kpile)] <- FALSE',
  'kept_all <- !kmm & !kpile & !grepl("prior_year_tables", knotes) & kgap <= 5',
  'stamp("FY%s kept cases (s46 keep rule): %d of %d", W$test[1], sum(kept_all[yr == W$test[1]]), sum(yr == W$test[1]))',
  sep = "\n")
KEPT_SWAPS <- list(
  c("pile_all <- income_pile_rows(adf0)", KEPT_BLOCK),
  c("score_year <- function(sel, idx_te, n_te, b, err_te, doll_te) {",
    "score_year <- function(sel, idx_te, n_te, b, err_te, doll_te, cap = floor(b * n_te)) {"),
  c("cap <- floor(b * n_te); un <- rep(FALSE, n_te)", "un <- rep(FALSE, n_te)"),
  c("r <- rs[[y]]; ix <- idx_of(y); np <- !pile_all[r]; pos <- cumsum(np)",
    "r <- rs[[y]]; ix <- idx_of(y); np <- !pile_all[r]; pos <- cumsum(np); kp <- kept_all[r]; posk <- cumsum(kp)"),
  c("err = ie_all[r][np], doll = ed_all[r][np]))",
    "err = ie_all[r][np], doll = ed_all[r][np]),\n         kept = list(idx = lapply(ix, function(v) posk[v[kp[v]]]), err = ie_all[r][kp], doll = ed_all[r][kp]))"),
  c('for (sc in c("all", "nonpile")) {', 'for (sc in c("all", "nonpile", "kept")) {'),
  c("s <- score_year(sel, TS$idx, length(TS$err), b, TS$err, TS$doll)",
    paste0('cap_k <- if (sc == "kept") max(1L, round(b * length(rs[[y]]))) else floor(b * length(TS$err))\n',
           "        s <- score_year(sel, TS$idx, length(TS$err), b, TS$err, TS$doll, cap_k)")),
  c("n_te = length(TS$err), n_err_te = sum(TS$err),", "n_te = length(TS$err), n_err_te = sum(TS$err), n_cap = cap_k,"))

src <- swap(paste(readLines(GATE, warn = FALSE), collapse = "\n"), c(GATE_SWAPS, KEPT_SWAPS))
if (identical(Sys.getenv("PARSE_ONLY"), "1")) {   # syntax check of the swapped script; nothing is run
  invisible(parse(text = src)); cat("all swaps match once and the swapped script parses\n"); quit(save = "no")
}
Sys.setenv(SEL_WINDOW = "w2224", SEL_TAG = if (SMOKE_RUN) "kept_smoke" else "kept")
reg_model_data <- NULL   # the gate script reads the frame itself
cat(sprintf("[%s] %s: running with %d substitutions\n", format(Sys.time(), "%H:%M:%S"), GATE,
            length(GATE_SWAPS) + length(KEPT_SWAPS)))
eval(parse(text = src), envir = globalenv())

# the kept scoring must sit on WB's cases
wb <- read.csv("methods/case_predictions/s46/out_f1109_wb/WB_f1109_by_state_5_10pct.csv")
kk <- res[res$scoring == "kept", ]
m <- merge(kk, wb, by = c("state", "budget"))
stopifnot(nrow(m) == nrow(kk), nrow(kk) == 4L * length(STATES) * length(BUDGETS))   # 4 arms
bad <- m[m$n_te != m$kept | m$n_err_te != m$errors_all | abs(m$dollars_total - m$dollars_all) > 0.005 |
         m$n_cap != m$flags | m$n_flagged > m$n_cap, ]
if (nrow(bad)) {
  print(bad[, c("state", "budget", "list", "gate", "n_te", "kept", "n_err_te", "errors_all",
                "dollars_total", "dollars_all", "n_cap", "flags", "n_flagged")])
  stop("kept scoring does not sit on WB's cases")
}
cat(sprintf("[%s] kept scoring matches WB's cases, errors, error dollars and flag counts in all %d state-budget cells\n",
            format(Sys.time(), "%H:%M:%S"), nrow(kk) / 4L))
