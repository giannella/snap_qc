# §52 part 3, counting correction (2026-11-12), step 3: score_variants.R, read from disk and run with swaps that add
# the corrected set core10nv (lv_core10_nv from list_flags_dedup.R) beside the original sets, read the corrected list
# files, write to this folder, and put each_core10nv (the shipped workbook's counting) in the map's file. All of
# score_variants.R's checks against the rule-list and vote files run unchanged.
#   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" methods/included_rules_vote/dedup/score_dedup.R
# Writes methods/included_rules_vote/dedup/variants_by_state_dedup.csv and IR_f1109_by_state_5_10pct_dedup.csv.
# (score_variants.R's closing line still names its original output files; the files written are these.)
ROOT <- "C:/Users/ericg/snap_qc_v27"; setwd(ROOT)
swap_ <- function(src, pairs) {
  for (p in pairs) {
    n <- lengths(regmatches(src, gregexpr(p[1], src, fixed = TRUE)))
    if (n != 1L) stop(sprintf("expected exactly one match of [%s], found %d", p[1], n))
    src <- sub(p[1], p[2], src, fixed = TRUE)
  }
  src
}
SW <- list(
  c('SETS <- c(all = "list_votes", all10 = "lv_all10", core10 = "lv_core10")',
    'SETS <- c(all = "list_votes", all10 = "lv_all10", core10 = "lv_core10", core10nv = "lv_core10_nv")'),
  c('readRDS(file.path(A, "list_votes_f1109_fy2024.rds"))', 'readRDS(file.path(A, "dedup", "list_votes_f1109_fy2024_dedup.rds"))'),
  c('readRDS(file.path(A, "list_votes_y2y.rds"))', 'readRDS(file.path(A, "dedup", "list_votes_y2y_dedup.rds"))'),
  c('write.csv(M, file.path(A, "variants_by_state.csv")', 'write.csv(M, file.path(A, "dedup", "variants_by_state_dedup.csv")'),
  c("errors_caught = x$each_core10_e, dollars_caught = x$each_core10_d", "errors_caught = x$each_core10nv_e, dollars_caught = x$each_core10nv_d"),
  c('write.csv(ir, file.path(A, "IR_f1109_by_state_5_10pct.csv")', 'write.csv(ir, file.path(A, "dedup", "IR_f1109_by_state_5_10pct_dedup.csv")'),
  c('each = 3), names(SETS)', 'each = length(SETS)), names(SETS)'))
src <- swap_(paste(readLines("methods/included_rules_vote/score_variants.R", warn = FALSE), collapse = "\n"), SW)
if (identical(Sys.getenv("PARSE_ONLY"), "1")) { invisible(parse(text = src)); cat("all swaps match once and the script parses\n"); quit(save = "no") }
eval(parse(text = src), envir = globalenv())
