# §52 part 3, counting correction (2026-11-12), step 2: list_flags_f1109.R (map) or list_flags_y2y.R (y2y), read
# from disk and run with a few swaps that add lv_core10_nv: the count of 10% core rules flagging a case, leaving out
# the rules that are also among the setting's 300 vote rules (votekeys_<set>.rds from vote_keys_s52.R). Everything
# else, including each script's reproduction check against the kept benchmark CSVs, runs unchanged.
#   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" methods/included_rules_vote/dedup/list_flags_dedup.R <map | y2y>
# Writes methods/included_rules_vote/dedup/list_votes_f1109_fy2024_dedup.rds or list_votes_y2y_dedup.rds.
ROOT <- "C:/Users/ericg/snap_qc_v27"; setwd(ROOT)
SET <- commandArgs(TRUE)[1]; stopifnot(SET %in% c("map", "y2y"))
if (!identical(Sys.getenv("PARSE_ONLY"), "1")) {
  VK <- readRDS(sprintf("methods/included_rules_vote/dedup/votekeys_%s.rds", SET))$key; stopifnot(length(VK) == 300L) }
swap_ <- function(src, pairs) {
  for (p in pairs) {
    n <- lengths(regmatches(src, gregexpr(p[1], src, fixed = TRUE)))
    if (n != 1L) stop(sprintf("expected exactly one match of [%s], found %d", p[1], n))
    src <- sub(p[1], p[2], src, fixed = TRUE)
  }
  src
}
nv <- "core10[!(paste(P$hh[core10], P$rule[core10]) %in% VK)]"
SW <- if (SET == "map") list(
  c('OUTF <- "methods/included_rules_vote/list_votes_f1109_fy2024.rds"',
    'OUTF <- "methods/included_rules_vote/dedup/list_votes_f1109_fy2024_dedup.rds"'),
  c("lv_core10 <- lv_all10 <- integer(length(adf$state));", "lv_core10 <- lv_all10 <- lv_core10_nv <- integer(length(adf$state)); n_ov <- c();"),
  c("lv_core10[r] <- tabulate(unlist(idx_te[core10]), length(r));",
    sprintf("lv_core10[r] <- tabulate(unlist(idx_te[core10]), length(r)); lv_core10_nv[r] <- tabulate(unlist(idx_te[%s]), length(r)); n_ov[state] <- length(core10) - length(%s);", nv, nv)),
  c("lv_all10 = lv_all10[te_rows], n_list_rules", "lv_all10 = lv_all10[te_rows], lv_core10_nv = lv_core10_nv[te_rows], n_list_rules")) else list(
  c('OUTF <- "methods/included_rules_vote/list_votes_y2y.rds"', 'OUTF <- "methods/included_rules_vote/dedup/list_votes_y2y_dedup.rds"'),
  c("lv_core10 <- lv_all10 <- integer(nrow(adf));", "lv_core10 <- lv_all10 <- lv_core10_nv <- integer(nrow(adf)); n_ov <- c();"),
  c("    lv_core10[rte[[y]]] <- tabulate(unlist(idx_y[[y]][core10]), length(rte[[y]]))",
    sprintf("    lv_core10[rte[[y]]] <- tabulate(unlist(idx_y[[y]][core10]), length(rte[[y]]))\n    lv_core10_nv[rte[[y]]] <- tabulate(unlist(idx_y[[y]][%s]), length(rte[[y]])); n_ov[state] <- length(core10) - length(%s)", nv, nv)),
  c("lv_all10 = lv_all10[te_rows], n_list_rules", "lv_all10 = lv_all10[te_rows], lv_core10_nv = lv_core10_nv[te_rows], n_list_rules"))
src <- swap_(paste(readLines(sprintf("methods/included_rules_vote/%s", if (SET == "map") "list_flags_f1109.R" else "list_flags_y2y.R"), warn = FALSE), collapse = "\n"), SW)
if (identical(Sys.getenv("PARSE_ONLY"), "1")) { invisible(parse(text = src)); cat("all swaps match once and the script parses\n"); quit(save = "no") }
eval(parse(text = src), envir = globalenv())
stamp("core rules that are also vote rules, by state: median %g, range %d-%d", median(n_ov), min(n_ov), max(n_ov))
