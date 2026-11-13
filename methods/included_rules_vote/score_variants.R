# Counting note (2026-11-12): core10 counts a core list rule that is also one of the 300 vote rules twice, once as
# a vote rule and once as a list rule; the workbook counts it once. dedup/score_dedup.R reruns this script with the
# corrected set core10nv; findings section 52 and the results map use its outputs (dedup/*_dedup.csv).
# Included rules as voting rules, the scoring (2026-11-12; Eric: "included rules are voting rules"). Ways of
# letting a workbook's list rules enter the vote score, held out, in three settings, each on kept cases with the
# map's flag counts (flags = max(1, round(budget x the state's cases)), filled from kept cases):
#   map FY2024   rules mined on FY2022-23: blended state lists (list_flags_f1109.R) and the map's 300 vote rules
#                (methods/case_predictions/s46/out_f1109_wb/wb/votes_W3.rds); scored on FY2024
#   y2y FY2023   rules mined on FY2017-19 + FY2022: national lists (list_flags_y2y.R) and 300 vote rules from the
#                same pool (votes_cases_y2y.R)
#   y2y FY2024   the same, scored on FY2024
# The vote score is the shipped scorer throughout (scorer_W3.rds: curve and size model fit on FY2022 votes from
# FY2017-19 rules), not refit. List rule sets: "all" = both lists' core and buffer rules; "all10" = the 10% list's
# core and buffer rules; "core10" = the 10% list's core rules (the held-out stand-in for the rules a workbook ships
# with Include? = TRUE). Rankings, within state:
#   first_<set>  cases flagged by a vote rule or a list rule first, then the vote score
#   each_<set>   votes = vote rules + each list rule that flags the case; ranked by the resulting score alone
#   one_<set>    votes = vote rules + 1 if any list rule flags the case; ranked by the resulting score alone
# each_core10 is the design the workbooks adopt (included rules vote; Vote? follows Include?).
# Compared with the rule lists and the vote score: the files the map and the year-to-year check use, each checked
# for the same kept cases, errors and flag counts, and the vote score re-ranked here must equal its file.
# Pooled as sums across the 49 states.
#   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" methods/included_rules_vote/score_variants.R
# Writes methods/included_rules_vote/variants_by_state.csv (every setting, state, budget and ranking) and
# IR_f1109_by_state_5_10pct.csv (map FY2024, each_core10, in the columns of the map's vote and model files).
ROOT <- "C:/Users/ericg/snap_qc_v27"; setwd(ROOT)
A <- "methods/included_rules_vote"
Sys.setenv(SR_ROOT = ROOT, SR_WINDOW = "stab24")
source("methods/v270_seed_replicates/common.R")
source("methods/vote_scoring/vote_groups.R"); source("methods/vote_scoring/vote_fit.R")
VOTE_CAP <- 7L; BUD <- c(0.05, 0.10); SETS <- c(all = "list_votes", all10 = "lv_all10", core10 = "lv_core10")
WB <- "methods/case_predictions/s46/out_f1109_wb"
s <- readRDS(file.path(WB, "wb", "scorer_W3.rds"))
score_of <- function(d, vote) {
  p <- pred_curve(s$curve, vote, d$hh) / s$national
  as.numeric(p * predict(s$size, size_data(d$ed, d$hh, d$ben, d$hsz, pmin(vote, VOTE_CAP)), type = "response"))
}
# d: one row per test case with st, ie, ed, hh, ben, hsz, vote (300 vote rules), score (shipped), kept, and the
# list counts. Returns errors and error dollars caught per state, budget and ranking.
run_setting <- function(d, label) {
  stopifnot(isTRUE(all.equal(score_of(d, d$vote), d$score, tolerance = 1e-8)))
  keys <- list()
  for (nm in names(SETS)) {
    lv <- d[[SETS[[nm]]]]
    keys[[paste0("first_", nm)]] <- list(as.numeric(d$vote > 0 | lv > 0), d$score)
    keys[[paste0("each_", nm)]] <- list(score_of(d, d$vote + lv))
    keys[[paste0("one_", nm)]] <- list(score_of(d, d$vote + (lv > 0)))
  }
  keys$votes_check <- list(d$score)
  out <- list()
  for (b in BUD) for (ix in split(seq_len(nrow(d)), d$st)) {
    kk <- max(1L, round(b * length(ix))); ix2 <- ix[d$kept[ix]]
    row <- data.frame(setting = label, state = d$st[ix[1]], budget = b, cases = length(ix), flags = kk,
                      kept = length(ix2), errors_all = sum(d$ie[ix2]), dollars_all = sum(d$ed[ix2]))
    for (k in names(keys)) {
      sel <- ix2[do.call(order, c(lapply(keys[[k]], function(z) -z[ix2]), list(ix2)))[seq_len(min(kk, length(ix2)))]]
      row[[paste0(k, "_e")]] <- sum(d$ie[sel]); row[[paste0(k, "_d")]] <- sum(d$ed[sel])
    }
    out[[length(out) + 1L]] <- row
  }
  do.call(rbind, out)
}
attach_refs <- function(x, rules, votes) {
  m <- merge(merge(x, rules, by = c("state", "budget")), votes, by = c("state", "budget"))
  stopifnot(nrow(m) == nrow(x), all(m$r_flags == m$flags), all(m$v_flags == m$flags), all(m$r_kept == m$kept),
            all(m$v_kept == m$kept), all(m$r_eall == m$errors_all), all(m$v_eall == m$errors_all),
            all(m$votes_check_e == m$votes_e), all(abs(m$votes_check_d - m$votes_d) < 0.01))
  m
}

# --- map FY2024
v <- readRDS(file.path(WB, "wb", "votes_W3.rds")); w <- readRDS(file.path(WB, "wb", "wb_ed_W3.rds"))
L <- readRDS(file.path(A, "list_votes_f1109_fy2024.rds")); b0 <- L$ben; b0[is.na(b0)] <- 0
stopifnot(nrow(L) == nrow(v), all(L$st == v$st), all(L$ie == v$ie), all(abs(b0 - v$ben) < 1e-6), all(w$st == v$st))
d <- cbind(v, score = w$score, kept = L$kept, L[, unname(SETS)])
x1 <- run_setting(d, "map FY2024")
R <- read.csv("methods/f1109_list_bench/gate_scores_w2224_kept.csv"); R <- R[R$scoring == "kept" & R$gate == "on" & R$list == "blended", ]
V <- read.csv(file.path(WB, "WB_f1109_by_state_5_10pct.csv"))
m1 <- attach_refs(x1, data.frame(state = R$state, budget = R$budget, rules_e = R$n_errors, rules_d = R$dollars_caught,
                                 r_flags = R$n_cap, r_kept = R$n_te, r_eall = R$n_err_te),
                  data.frame(state = V$state, budget = V$budget, votes_e = V$errors_caught, votes_d = V$dollars_caught,
                             v_flags = V$flags, v_kept = V$kept, v_eall = V$errors_all))
stopifnot(all(m1$cases == V$cases[match(paste(m1$state, m1$budget), paste(V$state, V$budget))]))

# --- year-to-year FY2023 and FY2024
C <- readRDS(file.path(A, "votes_cases_y2y.rds")); LS <- readRDS(file.path(A, "list_votes_y2y.rds"))
RS <- read.csv("methods/stability_fy2324/gate_scores_w1719_rules.csv"); RS <- RS[RS$scoring == "kept" & RS$gate == "on" & RS$list == "national", ]
VS <- read.csv("methods/stability_fy2324/votes_by_state.csv")
ms <- list()
for (y in c("2023", "2024")) {
  cc <- C[C$year == y, ]; l <- LS[LS$year == y, ]; b0 <- l$ben; b0[is.na(b0)] <- 0
  stopifnot(nrow(cc) == nrow(l), all(cc$st == l$st), all(cc$ie == l$ie), all(abs(b0 - cc$ben) < 1e-6), all(cc$keep == l$kept))
  d <- cbind(cc[, c("st", "ie", "ed", "hh", "ben", "hsz", "vote", "score")], kept = l$kept, l[, unname(SETS)])
  r <- RS[RS$test_year == as.integer(y), ]; vv <- VS[VS$year == as.integer(y), ]
  ms[[y]] <- attach_refs(run_setting(d, paste0("y2y FY", y)),
    data.frame(state = r$state, budget = r$budget, rules_e = r$n_errors, rules_d = r$dollars_caught,
               r_flags = r$n_cap, r_kept = r$n_te, r_eall = r$n_err_te),
    data.frame(state = vv$state, budget = vv$budget, votes_e = vv$errors_caught, votes_d = vv$dollars_caught,
               v_flags = vv$flags, v_kept = vv$kept, v_eall = vv$errors_all))
}
M <- do.call(rbind, c(list(m1), ms))
write.csv(M, file.path(A, "variants_by_state.csv"), row.names = FALSE)

# the map's file: map FY2024, each_core10
x <- m1[order(m1$budget, m1$state), ]
ir <- data.frame(window = "W3", score_year = "2024", budget = x$budget, state = x$state, cases = x$cases, kept = x$kept,
                 flags = x$flags, errors_caught = x$each_core10_e, dollars_caught = x$each_core10_d,
                 errors_all = x$errors_all, dollars_all = x$dollars_all)
ir$precision <- ir$errors_caught / ir$flags; ir$dollars_per_flag <- ir$dollars_caught / ir$flags
ir$dollar_recall <- ir$dollars_caught / ir$dollars_all; ir$times_random <- ir$dollar_recall / ir$budget
ir$error_rate <- ir$errors_all / ir$kept
write.csv(ir, file.path(A, "IR_f1109_by_state_5_10pct.csv"), row.names = FALSE)

ARMS <- c("rules", "votes", paste0(rep(c("first_", "each_", "one_"), each = 3), names(SETS)))
for (st_ in unique(M$setting)) for (b in BUD) {
  x <- M[M$setting == st_ & M$budget == b, ]; rate <- sum(x$errors_all) / sum(x$kept)
  cat(sprintf("\n== %s, %g%% (49 states) ==\n", st_, 100 * b))
  best_e <- pmax(x$rules_e, x$votes_e)
  for (a in ARMS) {
    e <- x[[paste0(a, "_e")]]; dd <- x[[paste0(a, "_d")]]; g <- e - best_e
    cat(sprintf("  %-12s cases %.2fx  dollars %.2fx", a, (sum(e) / sum(x$flags)) / rate, sum(dd) / sum(x$dollars_all) / b))
    if (!a %in% c("rules", "votes"))
      cat(sprintf(" | errors vs better of rules/votes: median %+g, within 2 in %d; >= rules %d, >= votes %d states",
                  median(g), sum(abs(g) <= 2), sum(e >= x$rules_e), sum(e >= x$votes_e)))
    cat("\n")
  }
}
cat("\nwrote", file.path(A, "variants_by_state.csv"), "and", file.path(A, "IR_f1109_by_state_5_10pct.csv"), "\n")
