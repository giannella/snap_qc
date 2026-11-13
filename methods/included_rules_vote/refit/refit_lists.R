# Vote-table refit, step 2 (2026-11-12; design note ../design_note_table_refit_2026-11-12.md). For one seed: each
# state's national 5% and 10% lists (core + buffer) walked on FY2017-19 from that seed's FY2017-19 pool
# (methods/v270_seed_replicates/fy2022/pools/admitted_f1109_seed<SEED>.rds), artifact gate and pile gate on, and,
# for each FY2022, FY2023 and FY2024 case, how many of the 10% list's core rules flag it (lv_core10, the included
# rules), with lv_all10 and list_votes as in list_flags_y2y.R. Also the rule-list arm: each list walked on each
# test year's kept cases (the gate script's score_year with the kept runner's flag count).
# Code: the gate script (methods/reconstruction_income_piles/gate_removal_test.R), window w1719 (FY2017-19 train;
# FY2022, FY2023, FY2024 test), read from disk with the f1109 swaps and the kept runner's KEPT_SWAPS; national
# lists only (no FY2017-19 state pools on f1109); the per-state loop is the script's own, cut to the national list.
#   [SMOKE_STATES=Maine] "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" methods/included_rules_vote/refit/refit_lists.R 117
# lv_core10_nv leaves out the core rules that are also among the seed's 300 vote rules (votekeys_seed<SEED>.rds from
# refit_votes.R): the workbook merges such a rule with its vote rule and counts it once. lv_core10 keeps them.
# Run refit_votes.R for the seed first.
# Writes methods/included_rules_vote/refit/lists_seed<SEED>[_smoke].rds: list(cases, rulelist, walk).
ROOT <- "C:/Users/ericg/snap_qc_v27"; setwd(ROOT)
SEED <- as.integer(commandArgs(TRUE)[1]); stopifnot(SEED %in% 117:119)
OUTD <- "methods/included_rules_vote/refit"; dir.create(OUTD, showWarnings = FALSE)
OUTF <- file.path(OUTD, sprintf("lists_seed%d.rds", SEED))
POOLS <- "methods/v270_seed_replicates/fy2022/pools"
VK <- readRDS(file.path(OUTD, sprintf("votekeys_seed%d.rds", SEED)))$key; stopifnot(length(VK) == 300L)
stopifnot(file.exists(file.path(POOLS, sprintf("admitted_f1109_seed%d.rds", SEED))),
          identical(attr(readRDS(file.path(POOLS, sprintf("admitted_f1109_seed%d.rds", SEED))), "train_years"),
                    c("2017", "2018", "2019")),
          startsWith(digest::digest(file = "reg_model_data.rds", algo = "sha256"), "08e0e825"))
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
SW <- c(list(
  c("stopifnot(nrow(reg_model_data) == 231619L)", "stopifnot(nrow(reg_model_data) == 237557L)"),
  c('setwd("C:/Users/ericg/snap_qc")', sprintf('setwd("%s")', ROOT)),
  c('cache = "methods/v270_cpi_benchmark/fy2022_cpi/cache"', sprintf('cache = "%s"', POOLS)),
  c("expect_train = c(116060L, 10920L)", "expect_train = c(119310L, 11048L)"),   # f1109 FY2017-19 (EXPECT_F1109 fy2022)
  c('sprintf("bench_national_%d.rds", SEED)', 'sprintf("admitted_f1109_seed%d.rds", SEED)'),
  c("SEED <- 117; BUDGETS", sprintf("SEED <- %dL; BUDGETS", SEED)),
  c('OUT_DIR <- "methods/reconstruction_income_piles"', sprintf('OUT_DIR <- "%s"', OUTD))),
  KEPT_SWAPS,
  # SMOKE_FAST=1 (code test only): skip the national pool's pile tagging (about 25 minutes), as list_flags_y2y.R does
  if (identical(Sys.getenv("SMOKE_FAST"), "1")) list(c("natl <- pile_tag(natl, adf[tr_rows, , drop = FALSE], strata_tr_nat, pile_all[tr_rows], ie_all[tr_rows])",
    "natl$pile_i <- rep(FALSE, nrow(natl))   # SMOKE_FAST")) else list())
if (identical(Sys.getenv("SMOKE_FAST"), "1")) stopifnot(nzchar(Sys.getenv("SMOKE_STATES")))
src <- swap(paste(readLines("methods/reconstruction_income_piles/gate_removal_test.R", warn = FALSE), collapse = "\n"), SW)
head_src <- substr(src, 1, regexpr("## ---- per state", src, fixed = TRUE) - 1)
Sys.setenv(SEL_WINDOW = "w1719")
eval(parse(text = head_src), envir = globalenv())
stopifnot(identical(W$test, c("2022", "2023", "2024")), identical(W$train, c("2017", "2018", "2019")), exists("kept_all"),
          SEED == as.integer(commandArgs(TRUE)[1]))

P <- natl[!natl$artifact_i & !natl$pile_i, cols, drop = FALSE]   # the gate script's national_on list pool
te_rows <- which(yr %in% W$test)
list_votes <- integer(nrow(adf)); lv_core10 <- lv_all10 <- lv_core10_nv <- integer(nrow(adf))
RL <- list(); WK <- list()
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
    if (b == 0.10) { core10 <- wf$frozen; all10 <- sel; core10_nv <- core10[!(paste(P$hh[core10], P$rule[core10]) %in% VK)] }
    WK[[length(WK) + 1L]] <- data.frame(seed = SEED, state = state, budget = b, n_train = n_tr, n_core = length(wf$frozen),
                                        n_core_vote = sum(paste(P$hh[wf$frozen], P$rule[wf$frozen]) %in% VK),
                                        n_buffer = length(wf$buffer), gap_core = wf$gap_core, gap_total = wf$gap_total)
    for (y in W$test) {
      r <- rte[[y]]; kp <- kept_all[r]; posk <- cumsum(kp); ik <- lapply(idx_y[[y]], function(v) posk[v[kp[v]]])
      cap <- max(1L, round(b * length(r)))
      s <- score_year(sel, ik, sum(kp), b, ie_all[r][kp], ed_all[r][kp], cap)
      RL[[length(RL) + 1L]] <- data.frame(seed = SEED, state = state, budget = b, year = y, cases = length(r), kept = sum(kp),
        flags = cap, n_flagged = s[["n_flagged"]], errors_caught = s[["n_errors"]], dollars_caught = s[["dollars_caught"]],
        errors_all = sum(ie_all[r][kp]), dollars_all = sum(ed_all[r][kp]))
    }
  }
  for (y in W$test) {
    list_votes[rte[[y]]] <- tabulate(unlist(idx_y[[y]][sel_u]), length(rte[[y]]))
    lv_core10[rte[[y]]] <- tabulate(unlist(idx_y[[y]][core10]), length(rte[[y]]))
    lv_core10_nv[rte[[y]]] <- tabulate(unlist(idx_y[[y]][core10_nv]), length(rte[[y]]))
    lv_all10[rte[[y]]] <- tabulate(unlist(idx_y[[y]][all10]), length(rte[[y]]))
  }
  stamp("  seed %d %s: %d core rules at 10%% (%d also vote rules); flagged by a core rule: FY2022 %.1f%%, FY2023 %.1f%%, FY2024 %.1f%%", SEED, state,
        length(core10), length(core10) - length(core10_nv), 100 * mean(lv_core10[rte[["2022"]]] > 0), 100 * mean(lv_core10[rte[["2023"]]] > 0),
        100 * mean(lv_core10[rte[["2024"]]] > 0))
  rm(idx, idx_tr, idx_y); invisible(gc())
}
cases <- data.frame(year = yr[te_rows], st = st[te_rows], ie = ie_all[te_rows], ed = ed_all[te_rows], kept = kept_all[te_rows],
                    list_votes = list_votes[te_rows], lv_core10 = lv_core10[te_rows], lv_all10 = lv_all10[te_rows],
                    lv_core10_nv = lv_core10_nv[te_rows])
saveRDS(list(cases = cases, rulelist = do.call(rbind, RL), walk = do.call(rbind, WK)), OUTF)
stamp("wrote %s (%d cases)", OUTF, nrow(cases))
