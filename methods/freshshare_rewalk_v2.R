# Stage-2 fresh-share re-walk on the section 32 harness.
#
# BINDING SPEC: methods/stage2_freshshare_rewalk_plan_2026-08-06.md (approved).
# Where any comment here and the plan disagree, the plan wins. No mining; every
# input is cached (FY2022-23 pools, bench lists, FY2024 scoring artifacts).
#
# MECHANISM (the one component that varies): the per-rank skip test of the
# delivery walk tightens from n_new >= 1 to sequential fresh share
# f = n_new / n_flagged >= F_MIN (primary 0.50), as a two-pass walk applied
# identically to the core fill and the buffer fill:
#   priority pass    rank (99%-LCB) order, keep a rule iff it adds cases,
#                    f >= F_MIN at its turn, and it fits the fill target
#   completion pass  if the target is not filled when the priority pass
#                    exhausts the pool window, walk the remaining rules again
#                    in rank order under the shipped test (n_new >= 1) until
#                    the target fills exactly
#
# CAPACITY IS AN ASSERTION, NOT A GUARD (plan; Eric's engineering-artifacts
# rule): each re-walked list must consume EXACTLY the baseline bench list's
# capacity - core fill target C0 = the baseline core's consumed capacity on
# the FY2022-23 build caseload, total target CT = the baseline core+buffer
# consumed capacity. Any deviation halts the run as a bug, never a result.
# Filling to consumed-capacity targets (not the nominal caps) is what makes
# equality a construction property; at F_MIN = 0 this walk is provably
# identical to the shipped fill_list (the baseline's take prefix sums to C0
# and CT exactly, so the phase targets terminate the scans at precisely the
# shipped takes), which the anchor verifies list-for-list.
#
# ANCHORS, checked before any counterfactual number:
#   1. F_MIN = 0 rebuilds all bench lists rule-for-rule (rule, hh, role,
#      n_flagged_state, n_new_at_rank at every rank).
#   2. The FY2024 refill walk on the rebuilt list reproduces
#      methods/marginal_precision_diagnostic/per_rule_marginal.csv's holdout
#      n_new / k_new at every rank.
#   3. The rebuilt core's frozen FY2024 union reproduces the committed
#      scorecard (holdout_metrics.json cases/errors).
#
# RANKED-WINDOW EVALUATION with the section 27 slack-zero certificate,
# transposed to the two-pass walk: a windowed walk is certified only if every
# scan terminated by REACHING its fill target inside the window; any scan
# that exhausts the window while short means the window under-fills, and the
# state-budget-F_MIN combination is redone unpruned (full pool). A completion
# stall on the FULL pool is a capacity-assertion failure and halts.
#
# READOUTS (per plan): primary = per-state paired difference in FY2024
# any-error delivered precision of the re-walked CORE list vs the unmodified
# bench list, 5% budget; verdict = median across 49 states vs +0.010.
# Secondary: same at 10%, directional. Judged guard: within-state median
# change in FY2024 dollar recall at 5% >= -0.005 (unweighted dollar-recall
# fraction; weighted reported as companion). Companions, no bar: errors
# caught, dollar recall, fill rate, walk depth, both budgets. Sensitivity
# grid F_MIN in {0.25, 0.40, 0.50, 0.60, 0.75}, descriptive only, no verdict.
#
# SMOKE=1: 5 states, plumbing only (anchor results, capacity assertions,
# window redo counts, runtime); no paired difference printed; outputs go to a
# scratch dir deleted at the end. The full run is launched separately after
# the senior-statistician review (routing rule).
#
# Outputs (full run) -> methods/freshshare_rewalk/ :
#   per_state_paired.csv, sensitivity_grid.csv, summary.txt
# Read-only on state_delivery_lists/ and the bench directory; no CHANGELOG,
# no version bump.

suppressMessages({ library(dplyr); library(jsonlite) })
source("rule_mining_helpers.R")

t0 <- Sys.time()
SMOKE <- Sys.getenv("SMOKE", "0") == "1"

OUT_ROOT <- "methods/freshshare_rewalk"
OUT_DIR  <- if (SMOKE) file.path(OUT_ROOT, "SMOKE_SCRATCH") else OUT_ROOT
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

POOL_DIR   <- "methods/delivery_pools_2022_2023_anyerror"
BENCH_DIR  <- "methods/anyerror_blended_holdout_2024"
ANCHOR_CSV <- "methods/marginal_precision_diagnostic/per_rule_marginal.csv"

ALL_YEARS    <- c("2022", "2023", "2024")
BENCH_YEARS  <- c("2022", "2023")
HOLDOUT_YEAR <- "2024"
BUDGETS      <- c(0.05, 0.10)
RANK_STAT    <- "precision_train_lcb"
F_PRIMARY    <- 0.50
F_GRID       <- c(0.25, 0.40, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75)   # plan: descriptive
# only; extended 2026-08-07 per the pre-stated fine-grid step in
# methods/remine_proposal_2026-08.md (dose-finding on the validated mechanism;
# previously recorded cells must reproduce exactly on the rerun)
WINDOW_TOPK  <- 12000
BAR_PRIMARY  <- 0.010     # median paired precision diff, 5% budget
BAR_GUARD    <- -0.005    # median dollar-recall (fraction) change, 5% budget
SMOKE_STATES <- c("Alabama", "California", "Colorado", "Georgia", "Michigan")

HH_LEVELS <- c("1", "2-3", "4+")
hh_group_of <- function(n) {
  n <- suppressWarnings(as.numeric(as.character(n)))
  ifelse(is.na(n), NA_character_, ifelse(n <= 1, "1", ifelse(n <= 3, "2-3", "4+")))
}
features <- c(
  "HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size",
  "expedited_i", "cat_elig", "rawben_rel_max", "medical_deductions",
  "shelter_expenses_by_hh_size", "utilities", "married", "homeless",
  "rawearn_by_hh_size", "rawunearn_by_hh_size", "rawgross_by_hh_size",
  "percent_abawd", "unc_rawben_rel_max",
  "months_since_cert_n", "count_divisible_by_100"
)
stamp <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"),
                                   sprintf(...)))

## ── frame ─────────────────────────────────────────────────────────────────────
if (!exists("reg_model_data")) reg_model_data <- readRDS("reg_model_data.rds")
pf  <- prep_features(reg_model_data %>% filter(fiscal_year %in% ALL_YEARS), features)
adf <- pf$data
st  <- as.character(adf$state)
fy  <- as.character(adf$fiscal_year)
stamp("frame: %d rows FY2022-24", nrow(adf))

json_rec <- fromJSON(file.path(BENCH_DIR, "holdout_metrics.json"))$records
anchor   <- read.csv(ANCHOR_CSV, stringsAsFactors = FALSE) %>% filter(basis == "holdout")
states_all <- sort(unique(json_rec$state))
states <- if (SMOKE) intersect(states_all, SMOKE_STATES) else states_all
stamp("states: %d of %d%s | window %d | F_MIN grid {%s}, primary %.2f",
      length(states), length(states_all), if (SMOKE) " (SMOKE subset)" else "",
      WINDOW_TOPK, paste(F_GRID, collapse = ", "), F_PRIMARY)

natl <- readRDS(file.path(POOL_DIR, "pool_national_anyerror_fdr10.rds"))
natl$pool <- "national"
stamp("national FY2022-23 pool: %d rules", nrow(natl))

flags_from_index <- function(ci, hh, data, strata_idx) {
  idx <- eval_conditions(ci$conditions, data)
  out <- vector("list", length(hh))
  for (h in unique(hh)) {
    rows <- which(hh == h)
    out[rows] <- rule_flag_idx(ci$rule_conds[rows], idx, base_idx = strata_idx[[h]])
  }
  out
}

## ── the two-pass walk ─────────────────────────────────────────────────────────
# Phase 1 fills the core to exactly c_core, phase 2 the buffer to exactly
# c_tot; each phase is a priority scan (f >= fmin) then, only if short, a
# completion scan (shipped test). Scans run in rank order over untaken rules
# and terminate the moment the target is reached, which is what makes
# F_MIN = 0 reproduce the shipped fill exactly. Returns short = TRUE when a
# scan exhausted `idx` (the window, or the full pool) with its target unmet.
walk2 <- function(idx, nfl, n_rows, c_core, c_tot, fmin) {
  un <- rep(FALSE, n_rows); n_in <- 0L
  taken <- logical(length(idx))
  sel <- integer(0); role <- character(0); nnew <- integer(0)
  used_completion <- FALSE; short <- FALSE
  for (ph in 1:2) {
    tgt <- if (ph == 1) c_core else c_tot
    for (ps in 1:2) {
      if (n_in >= tgt) break
      if (ps == 2) used_completion <- TRUE
      for (i in seq_along(idx)) {
        if (taken[i]) next
        ix <- idx[[i]]
        add <- sum(!un[ix])
        if (add == 0L) next
        if (ps == 1 && add / nfl[i] < fmin) next
        if (n_in + add > tgt) next
        un[ix] <- TRUE; n_in <- n_in + add; taken[i] <- TRUE
        sel <- c(sel, i); role <- c(role, c("core", "buffer")[ph])
        nnew <- c(nnew, add)
        if (n_in == tgt) break
      }
    }
    if (n_in != tgt) { short <- TRUE; break }
  }
  list(sel = sel, role = role, n_new = nnew, n_in = n_in, short = short,
       used_completion = used_completion,
       depth = if (length(sel)) max(sel) else 0L)   # sel indexes rank order
}

refill_walk <- function(idx, err, n_rows, cap) {   # mirrors add_refill_metrics_v2.R
  un <- rep(FALSE, n_rows); n_in <- 0L
  deployed <- logical(length(idx))
  n_new <- integer(length(idx)); k_new <- integer(length(idx))
  for (i in seq_along(idx)) {
    ix <- idx[[i]]; newix <- ix[!un[ix]]; add <- length(newix)
    if (add > 0L && n_in + add <= cap) {
      un[ix] <- TRUE; n_in <- n_in + add
      deployed[i] <- TRUE; n_new[i] <- add; k_new[i] <- sum(err[newix])
    }
  }
  list(deployed = deployed, n_new = n_new, k_new = k_new)
}

## ── main loop ─────────────────────────────────────────────────────────────────
res <- list()
n_anchor_build <- 0L; n_anchor_refill <- 0L; n_anchor_score <- 0L
n_cap_assert <- 0L; n_window_redo <- 0L; n_completion_used <- 0L

for (S in states) {
  tS <- Sys.time()
  key <- gsub("[^A-Za-z]", "", S)
  pfl <- file.path(POOL_DIR, sprintf("pool_%s_anyerror_fdr10.rds", key))
  own <- if (file.exists(pfl)) { o <- readRDS(pfl); o$pool <- "state"; o } else NULL
  # pool assembly and walk order, verbatim from the bench builder
  pool <- bind_rows(natl, own) %>%
    arrange(desc(.data[[RANK_STAT]]), hh, rule) %>%
    distinct(hh, rule, .keep_all = TRUE)
  ord  <- order(-pool[[RANK_STAT]], pool$hh, pool$rule, method = "radix")
  pool <- pool[ord, , drop = FALSE]                 # row i = pool rank i

  tr <- adf[st == S & fy %in% BENCH_YEARS, , drop = FALSE]
  ho <- adf[st == S & fy == HOLDOUT_YEAR, , drop = FALSE]
  strata_tr <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(tr$cert_HH_size_FS_n) %in% h))
  strata_ho <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(ho$cert_HH_size_FS_n) %in% h))
  ie_ho <- !is.na(ho$over_threshold) & ho$over_threshold != 0
  ed_ho <- ho$total_error_amount; ed_ho[is.na(ed_ho)] <- 0
  ed_ho <- ifelse(ie_ho, abs(ed_ho), 0)
  w_ho  <- ho$fywgt; w_ho[is.na(w_ho)] <- 0
  tot_unw <- sum(ed_ho); tot_wtd <- sum(w_ho * ed_ho)

  # window flags (build + holdout) from one condition index; full pool lazily
  W <- min(WINDOW_TOPK, nrow(pool))
  win <- pool[seq_len(W), , drop = FALSE]
  ci_w <- build_condition_index(win$rule)
  idx_tr <- flags_from_index(ci_w, win$hh, tr, strata_tr)
  idx_ho <- flags_from_index(ci_w, win$hh, ho, strata_ho)
  nfl_w  <- lengths(idx_tr)
  full <- NULL   # list(idx_tr, idx_ho, nfl) once any walk needs the full pool
  get_full <- function() {
    if (is.null(full)) {
      ci_f <- build_condition_index(pool$rule)
      full <<- list(idx_tr = flags_from_index(ci_f, pool$hh, tr, strata_tr),
                    idx_ho = flags_from_index(ci_f, pool$hh, ho, strata_ho))
      full$nfl <<- lengths(full$idx_tr)
    }
    full
  }

  for (b in BUDGETS) {
    btag <- sprintf("budget%02.0f", 100 * b)
    bench <- read.csv(file.path(BENCH_DIR, sprintf("bench_list_%s_%s.csv",
                                                   gsub(" ", "_", S), btag)),
                      stringsAsFactors = FALSE)
    bench <- bench[order(bench$rank), , drop = FALSE]
    C0 <- sum(bench$n_new_at_rank[bench$role == "core"])
    CT <- sum(bench$n_new_at_rank)

    run_walk <- function(fmin) {
      # Windowed first. The certificate (header, senior-statistician review):
      # a windowed walk is provably equal to the unpruned walk only if EVERY
      # scan reached its target inside the window AND the completion pass was
      # never entered - a windowed completion is not certifiable, because the
      # full pool's priority pass would scan ranks beyond the window for
      # f >= F_MIN rules BEFORE any completion rule is admitted. So both
      # under-fill and any windowed completion use trigger the unpruned redo.
      wk <- walk2(idx_tr, nfl_w, nrow(tr), C0, CT, fmin)
      redo <- FALSE
      if (wk$short || wk$used_completion) {
        redo <- TRUE; n_window_redo <<- n_window_redo + 1L
        fu <- get_full()
        wk <- walk2(fu$idx_tr, fu$nfl, nrow(tr), C0, CT, fmin)
        if (wk$short)
          stop(sprintf("CAPACITY ASSERTION FAILED [%s %s f_min=%.2f]: the completion pass exhausted the FULL pool with consumed capacity %d against targets core %d / total %d. Per the plan this is a bug that halts the run, never a result.",
                       S, btag, fmin, wk$n_in, C0, CT))
      }
      if (wk$used_completion) n_completion_used <<- n_completion_used + 1L  # on the walk actually used
      # capacity assertion proper (construction property)
      core_in <- sum(wk$n_new[wk$role == "core"])
      if (core_in != C0 || wk$n_in != CT)
        stop(sprintf("CAPACITY ASSERTION FAILED [%s %s f_min=%.2f]: consumed core %d vs baseline %d, total %d vs %d.",
                     S, btag, fmin, core_in, C0, wk$n_in, CT))
      n_cap_assert <<- n_cap_assert + 1L
      wk$redo <- redo
      wk$idx_ho_used <- if (redo) get_full()$idx_ho else idx_ho
      wk$pool_used   <- if (redo) pool else win
      wk
    }

    ## ── anchors at F_MIN = 0, before any counterfactual number ───────────────
    w0 <- run_walk(0)
    got <- data.frame(rule = w0$pool_used$rule[w0$sel], hh = w0$pool_used$hh[w0$sel],
                      role = w0$role,
                      n_flagged_state = if (w0$redo) get_full()$nfl[w0$sel] else nfl_w[w0$sel],
                      n_new_at_rank = w0$n_new, stringsAsFactors = FALSE)
    ok <- nrow(got) == nrow(bench) &&
      identical(got$rule, bench$rule) && identical(got$hh, bench$hh) &&
      identical(got$role, bench$role) &&
      identical(as.integer(got$n_flagged_state), as.integer(bench$n_flagged_state)) &&
      identical(as.integer(got$n_new_at_rank), as.integer(bench$n_new_at_rank))
    if (!ok)
      stop(sprintf("ANCHOR FAILED [%s %s]: the F_MIN = 0 re-walk does not rebuild the bench list rule-for-rule. No counterfactual number can be trusted.",
                   S, btag))
    n_anchor_build <- n_anchor_build + 1L

    # FY2024 refill on the rebuilt list must reproduce per_rule_marginal.csv
    idx_ho_list <- w0$idx_ho_used[w0$sel]
    rf <- refill_walk(idx_ho_list, ie_ho, nrow(ho), floor(b * nrow(ho)))
    dep <- which(rf$deployed)
    exp <- anchor %>% filter(state == S, budget == 100 * b) %>% arrange(rank)
    ok <- length(dep) == nrow(exp) &&
      identical(as.integer(dep), as.integer(exp$rank)) &&
      identical(rf$n_new[dep], as.integer(exp$n_new)) &&
      identical(rf$k_new[dep], as.integer(exp$k_new))
    if (!ok)
      stop(sprintf("ANCHOR FAILED [%s %s]: the FY2024 refill walk on the rebuilt list does not reproduce per_rule_marginal.csv's holdout n_new/k_new.",
                   S, btag))
    n_anchor_refill <- n_anchor_refill + 1L

    # frozen-core FY2024 union must reproduce the committed scorecard
    score_core <- function(wk) {
      hit <- rep(FALSE, nrow(ho))
      for (j in which(wk$role == "core")) hit[wk$idx_ho_used[[wk$sel[j]]]] <- TRUE
      n <- sum(hit); k <- sum(ie_ho & hit)
      list(n = n, k = k, prec = if (n > 0) k / n else NA_real_,
           dr_unw = if (tot_unw > 0) sum(ed_ho[hit]) / tot_unw else NA_real_,
           dr_wtd = if (tot_wtd > 0) sum((w_ho * ed_ho)[hit]) / tot_wtd else NA_real_,
           share = n / nrow(ho))
    }
    base <- score_core(w0)
    rec <- json_rec[json_rec$state == S & json_rec$budget_pct == 100 * b, , drop = FALSE]
    if (nrow(rec) != 1 || base$n != rec$n_cases_flagged_holdout ||
        base$k != rec$n_errors_flagged_holdout)
      stop(sprintf("ANCHOR FAILED [%s %s]: rebuilt core union on FY2024 gives %d/%d; scorecard says %d/%d.",
                   S, btag, base$n, base$k,
                   rec$n_cases_flagged_holdout, rec$n_errors_flagged_holdout))
    n_anchor_score <- n_anchor_score + 1L

    ## ── the counterfactual grid ───────────────────────────────────────────────
    for (fmin in F_GRID) {
      wk <- run_walk(fmin)
      sc <- score_core(wk)
      res[[paste(S, b, fmin)]] <- data.frame(
        state = S, budget = 100 * b, f_min = fmin,
        n_rules_core = sum(wk$role == "core"),
        n_rules_total = length(wk$sel),
        used_completion = wk$used_completion, window_redone = wk$redo,
        walk_depth = wk$depth,
        base_precision = base$prec, rewalk_precision = sc$prec,
        precision_diff = sc$prec - base$prec,
        base_errors = base$k, rewalk_errors = sc$k,
        base_dollar_recall_unw = base$dr_unw, rewalk_dollar_recall_unw = sc$dr_unw,
        dollar_recall_unw_diff = sc$dr_unw - base$dr_unw,
        base_dollar_recall_wtd = base$dr_wtd, rewalk_dollar_recall_wtd = sc$dr_wtd,
        dollar_recall_wtd_diff = sc$dr_wtd - base$dr_wtd,
        base_flagged_share = base$share, rewalk_flagged_share = sc$share,
        fill_rate = sc$share / b,
        stringsAsFactors = FALSE)
    }
  }
  rm(idx_tr, idx_ho, full, ci_w); invisible(gc())
  stamp("%-22s done in %5.1fs", S, as.numeric(difftime(Sys.time(), tS, units = "secs")))
}

if (n_anchor_build != length(states) * length(BUDGETS))
  stop("ANCHOR FAILED: not every state-budget list was verified at F_MIN = 0.")

pr <- bind_rows(res); rownames(pr) <- NULL

# BLINDING BY CONSTRUCTION (senior-statistician review, change 2): in SMOKE
# mode no paired difference and no verdict is ever serialized - the writes and
# the whole verdict/summary block exist only on the !SMOKE path, so no crash
# can leave a primary statistic on disk.
if (!SMOKE) {

write.csv(pr, file.path(OUT_DIR, "per_state_paired.csv"), row.names = FALSE)

grid <- pr %>%
  group_by(f_min, budget) %>%
  summarise(n_states = n(),
            median_precision_diff = median(precision_diff),
            median_dollar_recall_unw_diff = median(dollar_recall_unw_diff),
            median_dollar_recall_wtd_diff = median(dollar_recall_wtd_diff),
            median_fill_rate = median(fill_rate),
            median_walk_depth = median(walk_depth),
            median_errors_diff = median(rewalk_errors - base_errors),
            n_used_completion = sum(used_completion),
            n_window_redone = sum(window_redone),
            .groups = "drop")
write.csv(grid, file.path(OUT_DIR, "sensitivity_grid.csv"), row.names = FALSE)

## ── verdict per the plan's pre-stated rules ───────────────────────────────────
p5  <- pr %>% filter(f_min == F_PRIMARY, budget == 5)
p10 <- pr %>% filter(f_min == F_PRIMARY, budget == 10)
med5   <- median(p5$precision_diff)
med10  <- median(p10$precision_diff)
guard5 <- median(p5$dollar_recall_unw_diff)
primary_pass <- med5 >= BAR_PRIMARY
guard_pass   <- guard5 >= BAR_GUARD
win <- primary_pass && guard_pass

sum_lines <- c(
  "STAGE-2 FRESH-SHARE RE-WALK: verdict",
  sprintf("Plan: methods/stage2_freshshare_rewalk_plan_2026-08-06.md | run %s%s",
          format(Sys.time(), "%Y-%m-%d %H:%M"), if (SMOKE) " [SMOKE - VOID]" else ""),
  sprintf("States %d | mechanism: two-pass walk, fresh-share floor f >= %.2f",
          length(states), F_PRIMARY),
  sprintf("Anchors: %d/%d F_MIN=0 rebuilds; %d/%d FY2024 refill matches; %d/%d scorecard matches",
          n_anchor_build, length(states) * length(BUDGETS),
          n_anchor_refill, length(states) * length(BUDGETS),
          n_anchor_score, length(states) * length(BUDGETS)),
  sprintf("Capacity assertions passed: %d walks (all exact) | window redos: %d | completion used: %d",
          n_cap_assert, n_window_redo, n_completion_used),
  "",
  sprintf("PRIMARY  (5%% budget): median paired precision diff = %+.4f vs bar %+.3f -> %s",
          med5, BAR_PRIMARY, ifelse(primary_pass, "CLEARED", "FAILED")),
  sprintf("SECONDARY (10%% budget, directional): median = %+.4f -> %s",
          med10, ifelse(med10 > 0, "positive", "not positive")),
  sprintf("GUARD    (5%% dollar recall, unweighted): median change = %+.4f vs %+.3f -> %s",
          guard5, BAR_GUARD, ifelse(guard_pass, "HELD", "FAILED")),
  "",
  if (win) paste(
    "CONSEQUENCE (plan, pre-stated): bar and guards cleared - the fresh-share",
    "floor becomes a validated selection-refinement CANDIDATE for",
    "INCL_build_blended_delivery_list_v2.R. Promotion into shipped lists is a",
    "MINOR bump and Eric's call, and per the section 20 discipline it",
    "additionally requires replication on the second era before shipping: the",
    "cached train FY2017-18 / test FY2019 harness, same mechanism, same 0.50",
    "threshold, bar pre-stated then.")
  else paste(
    "CONSEQUENCE (plan, pre-stated): bar failed, or the dollar-recall guard",
    "failed - the line closes at public-data scale and the ledger records it",
    "beside sections 18/20/30/32. The diagnostic's SIGNAL still stands as a",
    "logged property: marginal quality IS predictable from fresh share, the",
    "only instrument of six to clear its gates; its actionable value at fixed",
    "capacity was insufficient, and the question stays open at internal-data",
    "scale only, like the section 32 row it extends. One-shot rule: the other",
    "mechanisms (near-tie break, penalized score, the other five instruments)",
    "are NOT then tried on this outcome."),
  "",
  "Sensitivity grid (descriptive only, no verdict; medians across states):",
  capture.output(print(as.data.frame(grid %>% mutate(across(where(is.numeric), ~ round(.x, 4)))),
                       row.names = FALSE)))
writeLines(sum_lines, file.path(OUT_DIR, "summary.txt"))

}   # end if (!SMOKE): nothing above this line runs, and nothing is written, in SMOKE

el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
if (SMOKE) {
  cat("\n================ SMOKE PLUMBING SUMMARY (no paired difference computed to disk) ================\n")
  cat(sprintf("anchor 1 (F_MIN=0 rebuilds bench list rule-for-rule) : %d/%d\n",
              n_anchor_build, length(states) * length(BUDGETS)))
  cat(sprintf("anchor 2 (FY2024 refill == per_rule_marginal.csv)    : %d/%d\n",
              n_anchor_refill, length(states) * length(BUDGETS)))
  cat(sprintf("anchor 3 (frozen core == committed scorecard)        : %d/%d\n",
              n_anchor_score, length(states) * length(BUDGETS)))
  cat(sprintf("capacity assertions (exact core AND total)           : %d walks, all passed\n",
              n_cap_assert))
  cat(sprintf("window redos (under-fill OR windowed completion)     : %d\n", n_window_redo))
  cat(sprintf("completion pass used (on the walk actually used)     : %d walks\n", n_completion_used))
  cat(sprintf("result rows held in memory only, NOTHING serialized  : %d\n", nrow(pr)))
  cat(sprintf("wall time: %.1f minutes | grid {%s} x %d budgets\n",
              el, paste(F_GRID, collapse = ", "), length(BUDGETS)))
  unlink(OUT_DIR, recursive = TRUE)
  cat(sprintf("SMOKE scratch dir removed (was empty by construction): %s\n", OUT_DIR))
} else {
  stamp("done in %.1f minutes; outputs in %s/", el, OUT_DIR)
}
