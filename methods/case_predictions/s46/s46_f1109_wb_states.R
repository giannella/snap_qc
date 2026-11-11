# WB, the workbook vote score (wb_ref_f1109.R), by state on f1109: test 3 (W3: FY2022-23 rules, scored on FY2024) at 5%
# and 10% workloads. Rows, keep rule and selection are those of s46_f1109_states.R, so the table sits beside
# out_f1109_eval/B_f1109_by_state_5_10pct.csv; the run stops unless every state's cases, kept rows, flags, errors and
# error dollars match B's file. Nothing is refit. (2026-11-11, for the results map.)
# Usage: S46_OUT=out_f1109_wb Rscript s46_f1109_wb_states.R
# Writes out_f1109_wb/: WB_f1109_by_state_5_10pct.csv (votes capped at 7, as shipped), WB_f1109_uncapped_by_state_5_10pct.csv,
# and WB_f1109_national.csv (L, L at 5% and pooled precision at 5%, as the ledger reports them, for WB, WB uncapped and B).
here <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE))))
source(file.path(here, "s46_lib.R")); source(file.path(here, "s46_models.R"))
stopifnot(basename(OUT) == "out_f1109_wb", !SMOKE)
for (e in parse(file.path(here, "s46_run.R")))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))) eval(e)
src <- paste(deparse(load_s46), collapse = "\n")
src <- sub("function (years)", "function (years, frame = FRAME)", src, fixed = TRUE)
src <- sub('FRAMES[["f1030"]]', "FRAMES[[frame]]", src, fixed = TRUE); src <- sub("231602L", "FRAME_ROWS[[frame]]", src, fixed = TRUE)
load_s46 <- eval(parse(text = src)); FRAME <- "f1109"

wn <- "W3"; D <- win_data(wn); app <- app_rows(D, wn); k <- keep_of(D, app); dv <- D$d[app]; yv <- D$y[app]
SS <- split(seq_along(app), D$st[app])
w <- readRDS(file.path(OUT, "wb", "wb_ed_W3.rds"))
stopifnot(length(w$score) == length(app), all(w$st == D$st[app]), all(w$ie == D$y[app]),
          all(is.finite(w$score)), all(is.finite(w$score_uncapped)))

by_state <- function(sc) {
  res <- lapply(c(0.05, 0.10), function(b) do.call(rbind, lapply(names(SS), function(s) { ix <- SS[[s]]
    kk <- max(1L, round(b * length(ix))); ix2 <- ix[k[ix]]; sel <- ix2[order(-sc[ix2], ix2)[seq_len(min(kk, length(ix2)))]]
    data.frame(window = wn, score_year = WINDOWS[[wn]]$score, budget = b, state = s, cases = length(ix), kept = length(ix2), flags = kk,
               errors_caught = sum(yv[sel]), dollars_caught = sum(dv[sel]), errors_all = sum(yv[ix2]), dollars_all = sum(dv[ix2])) })))
  tab <- do.call(rbind, res); rownames(tab) <- NULL
  tab$precision <- tab$errors_caught / tab$flags; tab$dollars_per_flag <- tab$dollars_caught / tab$flags
  tab$dollar_recall <- tab$dollars_caught / tab$dollars_all; tab$times_random <- tab$dollar_recall / tab$budget
  tab$error_rate <- tab$errors_all / tab$kept
  tab }
tab <- by_state(w$score); tab0 <- by_state(w$score_uncapped)

# the same rows as B's file, state by state
Bf <- read.csv(file.path(dirname(OUT), "out_f1109_eval", "B_f1109_by_state_5_10pct.csv"), stringsAsFactors = FALSE)
m <- merge(tab, Bf[Bf$window == wn, ], by = c("budget", "state"), suffixes = c("", ".B"))
stopifnot(nrow(m) == nrow(tab), nrow(tab) == 2 * length(SS),
          all(m$cases == m$cases.B), all(m$kept == m$kept.B), all(m$flags == m$flags.B),
          all(m$errors_all == m$errors_all.B), all(abs(m$dollars_all - m$dollars_all.B) < 1e-6))
write.csv(tab, file.path(OUT, "WB_f1109_by_state_5_10pct.csv"), row.names = FALSE)
write.csv(tab0, file.path(OUT, "WB_f1109_uncapped_by_state_5_10pct.csv"), row.names = FALSE)

# national figures in the ledger's terms, with B's from out_f1109_eval on the same rows
meth_B <- readRDS(file.path(dirname(OUT), "out_f1109_eval", "method.rds"))$method
scB <- rowMeans(sapply(SEEDS, function(sd)
  readRDS(file.path(dirname(OUT), "out_f1109_eval", "cache", sprintf("%s_%s_ed_B_%d.rds", wn, meth_B, sd)))))
stopifnot(length(scB) == length(app))
m0 <- mean(dv[k])
nat <- do.call(rbind, lapply(list(WB = w$score, WB_uncapped = w$score_uncapped, B = scB), function(sc)
  data.frame(L = mean(fastL(sc, dv, k, SS, BUD)) / m0, L_5pct = mean(fastL(sc, dv, k, SS, 0.05)) / m0, prec5 = prec5(sc, yv, k, SS))))
nat <- cbind(score = rownames(nat), nat); rownames(nat) <- NULL
write.csv(nat, file.path(OUT, "WB_f1109_national.csv"), row.names = FALSE)
print(nat, digits = 4)
for (t in list(list("WB", tab), list("WB uncapped", tab0))) for (b in c(0.05, 0.10)) { x <- t[[2]][t[[2]]$budget == b, ]
  cat(sprintf("%s at %.0f%%: pooled precision %.3f, pooled dollar recall %.3f (%.2fx random), states below 1x: %d\n", t[[1]], 100 * b,
              sum(x$errors_caught) / sum(x$flags), sum(x$dollars_caught) / sum(x$dollars_all),
              sum(x$dollars_caught) / sum(x$dollars_all) / b, sum(x$times_random < 1))) }
