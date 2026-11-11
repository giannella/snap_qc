# Section 46 Amendment 2's voted-workbook reference (WB) by state, on frame f1030. Test 3 (window W3): rules mined on
# FY2022-23 at seed 117, scored on FY2024. The run itself reported WB nationally (modeling_findings_detailed.md, section 46);
# this breaks the cached expected dollars in out_a2/cache out by state, with the rows, keep rule and selection of
# s46_f1109_states.R. Nothing is refit; only <output.csv> is written. It reproduces the ledger's L at 5% (3.240) and pooled
# precision at 5% (0.300). The results map used it on 2026-11-11 until WB was re-run on f1109 (wb_ref_f1109.R).
# Usage (R 4.5.1, from a staging-v2.7 checkout of the QC repo at ROOT in s46_lib.R):
#   Rscript s46_a2_wb_states.R <output.csv>
out_csv <- commandArgs(TRUE)[1]; stopifnot(!is.na(out_csv))
Sys.setenv(S46_OUT = "out_a2")
here <- "C:/Users/ericg/snap_qc_v27/methods/case_predictions/s46"
source(file.path(here, "s46_lib.R")); source(file.path(here, "s46_models.R"))
stopifnot(basename(OUT) == "out_a2", !SMOKE)
for (e in parse(file.path(here, "s46_run.R")))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))) eval(e)
meth <- method_of(); wn <- "W3"
D <- win_data(wn); app <- app_rows(D, wn); k <- keep_of(D, app); dv <- D$d[app]; yv <- D$y[app]; SS <- split(seq_along(app), D$st[app])
sc <- rowMeans(sapply(SEEDS, function(sd) readRDS(cpath(wn, meth, "ed", "WB", sd))))   # one mine: every seed's vector is the same
# check against the ledger (section 46, Amendment 2 results): WB on test 3 has L at 5% 3.240 and prec5 0.300
stopifnot(abs(mean(fastL(sc, dv, k, SS, 0.05)) / mean(dv[k]) - 3.240) < 5e-4, abs(prec5(sc, yv, k, SS) - 0.300) < 5e-4)
res <- list()
for (b in c(0.05, 0.10)) res[[as.character(b)]] <- do.call(rbind, lapply(names(SS), function(s) { ix <- SS[[s]]
  kk <- max(1L, round(b * length(ix))); ix2 <- ix[k[ix]]; sel <- ix2[order(-sc[ix2], ix2)[seq_len(min(kk, length(ix2)))]]
  data.frame(window = wn, score_year = WINDOWS[[wn]]$score, budget = b, state = s, cases = length(ix), kept = length(ix2), flags = kk,
             errors_caught = sum(yv[sel]), dollars_caught = sum(dv[sel]), errors_all = sum(yv[ix2]), dollars_all = sum(dv[ix2])) }))
tab <- do.call(rbind, res); rownames(tab) <- NULL
tab$precision <- tab$errors_caught / tab$flags; tab$dollars_per_flag <- tab$dollars_caught / tab$flags
tab$dollar_recall <- tab$dollars_caught / tab$dollars_all; tab$times_random <- tab$dollar_recall / tab$budget
tab$error_rate <- tab$errors_all / tab$kept
write.csv(tab, out_csv, row.names = FALSE)
