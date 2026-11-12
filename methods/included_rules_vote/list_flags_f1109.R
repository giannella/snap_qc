# Included rules as voting rules, step 1 of the map setting (2026-11-12; Eric: "included rules are voting rules").
# For each state, the rules a workbook built on FY2022-23 would carry as its 5% and 10% lists (core + buffer),
# from the f1109 rule-list benchmark: blended national + state pools, artifact gate, pile gate on (it tags no rule
# on f1109: every state's gate log line reads "pile-tagged national 0, state 0", so tagging is skipped here), the
# gate script's walk. Counts, for each FY2024 case, how many list rules flag it: list_votes (both lists, core +
# buffer), lv_all10 (the 10% list, core + buffer), lv_core10 (the 10% list's core rules, the held-out stand-in for
# the rules a workbook ships with Include? = TRUE; the workbook's build-time filters in rule_selection.py are not
# applied).
# Code: the gate script (methods/reconstruction_income_piles/gate_removal_test.R), read from disk with the kept
# runner's f1109 and kept swaps, run through the national pool; the per-state loop below is the script's own loop
# cut down to the blended list.
# Check: each budget's list, walked on FY2024 kept cases, must reproduce the kept CSV
# (methods/f1109_list_bench/gate_scores_w2224_kept.csv, blended, gate on, kept) in every state and budget: rules on
# the list, flags used, errors and error dollars.
#   [SMOKE_STATES=Maine] "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" methods/included_rules_vote/list_flags_f1109.R
# Writes methods/included_rules_vote/list_votes_f1109_fy2024.rds: one row per FY2024 case, in the frame's order.
ROOT <- "C:/Users/ericg/snap_qc_v27"; setwd(ROOT)
OUTF <- "methods/included_rules_vote/list_votes_f1109_fy2024.rds"
swap <- function(src, pairs) {
  for (p in pairs) {
    n <- lengths(regmatches(src, gregexpr(p[1], src, fixed = TRUE)))
    if (n != 1L) stop(sprintf("expected exactly one match of [%s], found %d", p[1], n))
    src <- sub(p[1], p[2], src, fixed = TRUE)
  }
  src
}
OUT <- "methods/f1109_list_bench"; BENCH_RUN_DIR <- file.path(OUT, "bench")   # as in the kept runner
for (e in parse("runners/run_f1109_list_bench_kept.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]]) %in% c("GATE_SWAPS", "KEPT_BLOCK", "KEPT_SWAPS")) eval(e)
SW <- c(GATE_SWAPS, KEPT_SWAPS, list(
  c("natl <- pile_tag(natl, adf[tr_rows, , drop = FALSE], strata_tr_nat, pile_all[tr_rows], ie_all[tr_rows])",
    "natl$pile_i <- rep(FALSE, nrow(natl))   # f1109: no rule pile-tagged (gate stage log)")))
src <- swap(paste(readLines("methods/reconstruction_income_piles/gate_removal_test.R", warn = FALSE), collapse = "\n"), SW)
head_src <- substr(src, 1, regexpr("## ---- per state", src, fixed = TRUE) - 1)
Sys.setenv(SEL_WINDOW = "w2224")
eval(parse(text = head_src), envir = globalenv())
stopifnot(identical(W$test, "2024"), exists("kept_all"))

ref <- read.csv("methods/f1109_list_bench/gate_scores_w2224_kept.csv")
ref <- ref[ref$scoring == "kept" & ref$gate == "on" & ref$list == "blended", ]
te_rows <- which(yr == "2024")
list_votes <- integer(length(adf$state)); lv_core10 <- lv_all10 <- integer(length(adf$state)); n_list <- setNames(integer(0), character(0)); chk <- list()
RUN_STATES <- if (nzchar(Sys.getenv("SMOKE_STATES"))) strsplit(Sys.getenv("SMOKE_STATES"), ",")[[1]] else sort(unique(st))
if (nzchar(Sys.getenv("SMOKE_STATES"))) OUTF <- sub("\\.rds$", "_smoke.rds", OUTF)
for (state in RUN_STATES) {
  rs_tr <- which(st == state & is_tr); r <- which(st == state & yr == "2024"); n_tr <- length(rs_tr)
  own <- readRDS(file.path(W$cache, sprintf("bench_state_%s_%d.rds", gsub(" ", "_", state), SEED)))
  own <- mm_tag(own); own$pile_i <- rep(FALSE, nrow(own))
  own_in <- if (nrow(own)) { own$pool <- "state"; own[, cols] } else NULL
  kf <- function(x) if (is.null(x)) NULL else x[!x$artifact_i & !x$pile_i, , drop = FALSE]
  P <- blend_of(kf(natl[, cols]), kf(own_in))
  all_rows <- c(rs_tr, r)
  strata_s <- lapply(setNames(nm = HH_LEVELS), function(h) which(hh_all[all_rows] %in% h))
  idx <- flags_for_rules(P, adf[all_rows, , drop = FALSE], strata_s)
  idx_tr <- lapply(idx, function(ix) ix[ix <= n_tr]); idx_te <- lapply(idx, function(ix) ix[ix > n_tr] - n_tr)
  kp <- kept_all[r]; posk <- cumsum(kp); idx_k <- lapply(idx_te, function(v) posk[v[kp[v]]])
  sel_u <- integer(0); core10 <- all10 <- integer(0)
  for (b in BUDGETS) {
    wf <- walk_fill(idx_tr, n_tr, b); sel <- c(wf$frozen, wf$buffer); sel_u <- union(sel_u, sel)
    if (b == 0.10) { core10 <- wf$frozen; all10 <- sel }
    s <- score_year(sel, idx_k, sum(kp), b, ie_all[r][kp], ed_all[r][kp], max(1L, round(b * length(r))))
    g <- ref[ref$state == state & abs(ref$budget - b) < 1e-9, ]
    ok <- nrow(g) == 1 && g$n_rules == length(sel) && g$n_flagged == s[["n_flagged"]] && g$n_errors == s[["n_errors"]] &&
      abs(g$dollars_caught - round(s[["dollars_caught"]], 2)) < 0.01
    if (!ok) stop(sprintf("%s %g%%: list does not reproduce the kept CSV", state, 100 * b))
    chk[[length(chk) + 1L]] <- data.frame(state = state, budget = b, n_rules = length(sel))
  }
  list_votes[r] <- tabulate(unlist(idx_te[sel_u]), length(r)); n_list[state] <- length(sel_u)
  lv_core10[r] <- tabulate(unlist(idx_te[core10]), length(r)); lv_all10[r] <- tabulate(unlist(idx_te[all10]), length(r))
  stamp("  %s: %d list rules (5%% and 10%% lists); %.1f%% of FY2024 cases flagged by one", state, length(sel_u),
        100 * mean(list_votes[r] > 0))
  rm(idx, idx_tr, idx_te, idx_k); invisible(gc())
}
stamp("all %d state-budget lists reproduce the kept CSV", length(chk))
out <- data.frame(st = st[te_rows], ie = ie_all[te_rows], ed = ed_all[te_rows], kept = kept_all[te_rows],
                  ben = suppressWarnings(as.numeric(as.character(adf$raw_benefit_amount[te_rows]))),
                  list_votes = list_votes[te_rows], lv_core10 = lv_core10[te_rows], lv_all10 = lv_all10[te_rows], n_list_rules = unname(n_list[st[te_rows]]))
saveRDS(out, OUTF)
stamp("wrote %s (%d FY2024 cases; %.1f%% flagged by a list rule)", OUTF, nrow(out), 100 * mean(out$list_votes > 0))
