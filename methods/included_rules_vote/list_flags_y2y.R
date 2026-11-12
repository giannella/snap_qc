# Included rules as voting rules, step 1 of the year-to-year setting (2026-11-12). The seed-117 national pool mined
# on FY2017-19 + FY2022 (methods/stability_fy2324/pools/admitted_f1109_seed117.rds), national lists at 5% and 10%
# (core + buffer), artifact gate, pile gate on (it tags no rule here: every state's line in
# methods/stability_fy2324/3_rules.log reads "pile-tagged national 0, state 0", so tagging is skipped). Counts, for
# each FY2023 and FY2024 case, how many list rules flag it (list_votes, lv_all10, lv_core10, as in
# list_flags_f1109.R).
# Code: the gate script read from disk with rules_stab.R's swaps, run through the national pool; the per-state loop
# is the script's own, cut down to the national list.
# Check: each list, walked on FY2023 and FY2024 kept cases, must reproduce
# methods/stability_fy2324/gate_scores_w1719_rules.csv (national, gate on, kept) in every state, budget and year.
#   [SMOKE_STATES=Maine] "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" methods/included_rules_vote/list_flags_y2y.R
# Writes methods/included_rules_vote/list_votes_y2y.rds: one row per FY2023 and FY2024 case, frame order.
ROOT <- "C:/Users/ericg/snap_qc_v27"; setwd(ROOT)
OUTF <- "methods/included_rules_vote/list_votes_y2y.rds"
OUT <- "methods/stability_fy2324"; POOL_DIR <- file.path(OUT, "pools"); SMOKE_RUN <- FALSE
swap <- function(src, pairs) {
  for (p in pairs) {
    n <- lengths(regmatches(src, gregexpr(p[1], src, fixed = TRUE)))
    if (n != 1L) stop(sprintf("expected exactly one match of [%s], found %d", p[1], n))
    src <- sub(p[1], p[2], src, fixed = TRUE)
  }
  src
}
for (e in parse("runners/run_f1109_list_bench_kept.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]]) %in% c("KEPT_BLOCK", "KEPT_SWAPS")) eval(e)
for (e in parse(file.path(OUT, "rules_stab.R")))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), "SWAPS")) eval(e)
SW <- c(SWAPS, list(
  c("natl <- pile_tag(natl, adf[tr_rows, , drop = FALSE], strata_tr_nat, pile_all[tr_rows], ie_all[tr_rows])",
    "natl$pile_i <- rep(FALSE, nrow(natl))   # no rule pile-tagged (3_rules.log)")))
src <- swap(paste(readLines("methods/reconstruction_income_piles/gate_removal_test.R", warn = FALSE), collapse = "\n"), SW)
head_src <- substr(src, 1, regexpr("## ---- per state", src, fixed = TRUE) - 1)
Sys.setenv(SEL_WINDOW = "w1719")
eval(parse(text = head_src), envir = globalenv())
stopifnot(identical(W$test, c("2023", "2024")), identical(W$train, c("2017", "2018", "2019", "2022")), exists("kept_all"))

ref <- read.csv(file.path(OUT, "gate_scores_w1719_rules.csv"))
ref <- ref[ref$scoring == "kept" & ref$gate == "on" & ref$list == "national", ]
P <- natl[!natl$artifact_i & !natl$pile_i, cols, drop = FALSE]   # the gate script's national_on list pool
te_rows <- which(yr %in% W$test)
list_votes <- integer(nrow(adf)); lv_core10 <- lv_all10 <- integer(nrow(adf)); n_list <- setNames(integer(0), character(0)); nchk <- 0L
RUN_STATES <- if (nzchar(Sys.getenv("SMOKE_STATES"))) strsplit(Sys.getenv("SMOKE_STATES"), ",")[[1]] else sort(unique(st))
if (nzchar(Sys.getenv("SMOKE_STATES"))) OUTF <- sub("\\.rds$", "_smoke.rds", OUTF)
for (state in RUN_STATES) {
  rs_tr <- which(st == state & is_tr); n_tr <- length(rs_tr)
  rte <- lapply(setNames(nm = W$test), function(y) which(st == state & yr == y))
  all_rows <- c(rs_tr, unlist(rte, use.names = FALSE)); off <- cumsum(c(n_tr, lengths(rte)))
  strata_s <- lapply(setNames(nm = HH_LEVELS), function(h) which(hh_all[all_rows] %in% h))
  idx <- flags_for_rules(P, adf[all_rows, , drop = FALSE], strata_s)
  idx_tr <- lapply(idx, function(ix) ix[ix <= n_tr])
  idx_y <- lapply(seq_along(rte), function(j) { lo <- off[j]; hi <- off[j + 1L]
    lapply(idx, function(ix) ix[ix > lo & ix <= hi] - lo) })
  names(idx_y) <- W$test
  sel_u <- integer(0); core10 <- all10 <- integer(0)
  for (b in BUDGETS) {
    wf <- walk_fill(idx_tr, n_tr, b); sel <- c(wf$frozen, wf$buffer); sel_u <- union(sel_u, sel)
    if (b == 0.10) { core10 <- wf$frozen; all10 <- sel }
    for (y in W$test) {
      r <- rte[[y]]; kp <- kept_all[r]; posk <- cumsum(kp); ik <- lapply(idx_y[[y]], function(v) posk[v[kp[v]]])
      s <- score_year(sel, ik, sum(kp), b, ie_all[r][kp], ed_all[r][kp], max(1L, round(b * length(r))))
      g <- ref[ref$state == state & abs(ref$budget - b) < 1e-9 & ref$test_year == as.integer(y), ]
      ok <- nrow(g) == 1 && g$n_rules == length(sel) && g$n_flagged == s[["n_flagged"]] && g$n_errors == s[["n_errors"]] &&
        abs(g$dollars_caught - round(s[["dollars_caught"]], 2)) < 0.01
      if (!ok) stop(sprintf("%s %g%% FY%s: list does not reproduce gate_scores_w1719_rules.csv", state, 100 * b, y))
      nchk <- nchk + 1L
    }
  }
  for (y in W$test) {
    list_votes[rte[[y]]] <- tabulate(unlist(idx_y[[y]][sel_u]), length(rte[[y]]))
    lv_core10[rte[[y]]] <- tabulate(unlist(idx_y[[y]][core10]), length(rte[[y]]))
    lv_all10[rte[[y]]] <- tabulate(unlist(idx_y[[y]][all10]), length(rte[[y]]))
  }
  n_list[state] <- length(sel_u)
  stamp("  %s: %d list rules; flagged by one: FY2023 %.1f%%, FY2024 %.1f%%", state, length(sel_u),
        100 * mean(list_votes[rte[["2023"]]] > 0), 100 * mean(list_votes[rte[["2024"]]] > 0))
  rm(idx, idx_tr, idx_y); invisible(gc())
}
stamp("all %d state-budget-year lists reproduce gate_scores_w1719_rules.csv", nchk)
out <- data.frame(year = yr[te_rows], st = st[te_rows], ie = ie_all[te_rows], ed = ed_all[te_rows], kept = kept_all[te_rows],
                  ben = suppressWarnings(as.numeric(as.character(adf$raw_benefit_amount[te_rows]))),
                  list_votes = list_votes[te_rows], lv_core10 = lv_core10[te_rows], lv_all10 = lv_all10[te_rows], n_list_rules = unname(n_list[st[te_rows]]))
saveRDS(out, OUTF)
stamp("wrote %s (%d cases)", OUTF, nrow(out))
