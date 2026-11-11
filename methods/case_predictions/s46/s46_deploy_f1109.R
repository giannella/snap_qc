# B's deploy bundle refit on frame f1109 (Ben's 2026-11-09 munging: AMTERR labels, nature 97 / agency 26 flagged
# not removed; Eric, 2026-11-09). Copied from s46_deploy_f1104.R: s46_post.R's deploy step unchanged except for the
# frame. The frozen Amendment-2 choices (percentiles, old years, method) are copied from out_a2, as for out_f1104;
# the deploy settings are re-tuned on f1109 with FY2024 held back. prorated_i is not an input (Eric, 2026-11-09).
# Usage: S46_OUT=out_f1109 Rscript s46_deploy_f1109.R   -> out_f1109/deploy_B_bundle.rds, deploy_B_levels.csv
here <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE))))
source(file.path(here, "s46_lib.R")); source(file.path(here, "s46_models.R"))
stopifnot(basename(OUT) == "out_f1109", !SMOKE)
WN <- names(WINDOWS); A2 <- file.path(dirname(OUT), "out_a2")
for (f in c(paste0("choices_", WN, ".rds"), "method.rds"))
  if (!file.exists(file.path(OUT, f))) stopifnot(file.copy(file.path(A2, f), file.path(OUT, f)))
for (e in parse(file.path(here, "s46_run.R")))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))) eval(e)
src <- paste(deparse(load_s46), collapse = "\n")
src <- sub("function (years)", "function (years, frame = FRAME)", src, fixed = TRUE)
src <- sub('FRAMES[["f1030"]]', "FRAMES[[frame]]", src, fixed = TRUE); src <- sub("231602L", "FRAME_ROWS[[frame]]", src, fixed = TRUE)
load_s46 <- eval(parse(text = src)); FRAME <- "f1109"; meth <- method_of(); PHASE <- "deploy"
post <- readLines(file.path(here, "s46_post.R")); i0 <- which(startsWith(post, 'if (PHASE == "deploy")')); stopifnot(length(i0) == 1)
blk <- paste(post[i0:length(post)], collapse = "\n")
subs <- list(c('FRAMES[["f1030"]]', "FRAMES[[FRAME]]"),
             c('if (basename(OUT) == "out_a2") "Amendment 2 (lben from the recreated benefit)" else "Amendment 1"',
               '"Amendment 2 (lben from the recreated benefit), refit on frame f1109"'),
             c("frame_sha256 = ", "frame = FRAME, frame_sha256 = "))
for (s in subs) { stopifnot(grepl(s[1], blk, fixed = TRUE)); blk <- sub(s[1], s[2], blk, fixed = TRUE) }
say("=== deploy on %s", FRAME)
eval(parse(text = blk))
