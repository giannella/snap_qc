# §48 addendum: B on f1104 by state at a 10% workload (and 5% for reference), from the fits in out_f1104; nothing is refit.
# Usage: S46_OUT=out_f1104 Rscript s46_f1104_states.R
here <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE))))
source(file.path(here, "s46_lib.R")); source(file.path(here, "s46_models.R"))
stopifnot(basename(OUT) == "out_f1104", !SMOKE)
for (e in parse(file.path(here, "s46_run.R")))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))) eval(e)
src <- paste(deparse(load_s46), collapse = "\n")
src <- sub("function (years)", "function (years, frame = FRAME)", src, fixed = TRUE)
src <- sub('FRAMES[["f1030"]]', "FRAMES[[frame]]", src, fixed = TRUE); src <- sub("231602L", "FRAME_ROWS[[frame]]", src, fixed = TRUE)
load_s46 <- eval(parse(text = src)); FRAME <- "f1104"; meth <- method_of()
res <- list()
for (wn in names(WINDOWS)) { D <- win_data(wn); app <- app_rows(D, wn); k <- keep_of(D, app); dv <- D$d[app]; yv <- D$y[app]
  sc <- rowMeans(sapply(SEEDS, function(sd) readRDS(cpath(wn, meth, "ed", "B", sd)))); SS <- split(seq_along(app), D$st[app])
  for (b in c(0.05, 0.10)) res[[paste(wn, b)]] <- do.call(rbind, lapply(names(SS), function(s) { ix <- SS[[s]]
    kk <- max(1L, round(b * length(ix))); ix2 <- ix[k[ix]]; sel <- ix2[order(-sc[ix2], ix2)[seq_len(min(kk, length(ix2)))]]
    data.frame(window = wn, score_year = WINDOWS[[wn]]$score, budget = b, state = s, cases = length(ix), kept = length(ix2), flags = kk,
               errors_caught = sum(yv[sel]), dollars_caught = sum(dv[sel]), errors_all = sum(yv[ix2]), dollars_all = sum(dv[ix2])) })) }
tab <- do.call(rbind, res); rownames(tab) <- NULL
tab$precision <- tab$errors_caught / tab$flags; tab$dollars_per_flag <- tab$dollars_caught / tab$flags
tab$dollar_recall <- tab$dollars_caught / tab$dollars_all; tab$times_random <- tab$dollar_recall / tab$budget
tab$error_rate <- tab$errors_all / tab$kept
write.csv(tab, file.path(OUT, "s48_B_f1104_by_state_10pct.csv"), row.names = FALSE)
agg <- do.call(rbind, lapply(split(tab, paste(tab$window, tab$budget)), function(x) data.frame(window = x$window[1], budget = x$budget[1],
  flags = sum(x$flags), precision = sum(x$errors_caught) / sum(x$flags), dollars_per_flag = sum(x$dollars_caught) / sum(x$flags),
  dollar_recall = sum(x$dollars_caught) / sum(x$dollars_all), times_random = sum(x$dollars_caught) / sum(x$dollars_all) / x$budget[1],
  base_rate = sum(x$errors_all) / sum(x$kept), med_precision = median(x$precision), min_precision = min(x$precision), max_precision = max(x$precision),
  med_times = median(x$times_random), min_times = min(x$times_random), max_times = max(x$times_random),
  states_below_base = sum(x$precision < x$error_rate), states_below_1x = sum(x$times_random < 1))))
write.csv(agg, file.path(OUT, "s48_B_f1104_summary_5_10pct.csv"), row.names = FALSE); print(agg, digits = 3)
