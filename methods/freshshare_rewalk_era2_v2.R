# Bridge arm + second-era replication of the fresh-share floor.
#
# BINDING SPEC: methods/era2_freshshare_replication_plan_2026-08-06.md
# (approved). Where any comment here and the plan disagree, the plan wins.
# Follows methods/freshshare_rewalk_v2.R (stage 2, findings section 33); the
# walk machinery (walk2, the certificate rule, the capacity assertion) is
# copied VERBATIM from that reviewed script - the committed stage-2 script is
# not modified, so the shared functions are duplicated here with their review
# provenance noted inline. Do not let the copies drift.
#
# DESIGN CHAIN (one component varies at each link):
#   stage 2 (done)   blended lists, era 1 (build FY2022-23, score FY2024)
#   ARM A bridge     NATIONAL-ONLY lists, same era 1 - pool composition varies
#   ARM B/C era 2    national-only lists, era 2 (build FY2017-18, score
#                    FY2019) - the era varies
# The bridge pool is the UNDEDUPED admitted national cache
# (filtered_national_any_error_fdr10.rds, 50,697 rules), per the plan's
# explicit path: era 2's pool is likewise dedup-free (admission recomputed
# from cached n/k), so both arms walk undeduped national pools and the
# era-2-vs-bridge comparison carries the era as its only difference.
#
# ANCHORS (any failure aborts; checked before any counterfactual number):
#   support reconciliation   per-state FY2017-18 / FY2019 rows and error
#                            events must match the plan's table (national
#                            79,907/7,115 build; 39,221/3,931 test; Wyoming
#                            273/13, South Dakota 654/15; eligibility = 47)
#   era-2 cache determinism  n/k recomputed on FY2017-18 from
#                            reg_model_data.rds for the cached rules must
#                            match raw_national.rds exactly (all 145,313 in
#                            the full run; a seeded subsample in SMOKE)
#   bridge LCB match         every national rule on the committed bench lists
#                            carries the same precision_train_lcb in the
#                            bridge pool
#   baseline identity        per arm, state and budget: the F_MIN = 0
#                            two-pass walk must reproduce, rule for rule and
#                            role for role, the shipped fill logic
#                            (INCL_build_blended_delivery_list_v2.R
#                            semantics, the section 27 fill) run
#                            national-only on identical inputs
#   capacity by construction each re-walked list's consumed core and total
#                            capacity equals its baseline's exactly; any
#                            deviation halts as a bug, never a result
#
# BRANCH (mechanical, pre-registered): the bridge 5%-budget median at
# f = 0.50 is computed by the machine; >= +0.005 proceeds to era 2, below it
# the run STOPS after the bridge outputs and the re-scope branch is stated.
# Blinding applies to humans and to SMOKE, not to the branch logic.
#
# VERDICTS (full run only; plan sections quoted in summary.txt):
#   Eric's hypothesis   bridge 5% median vs the blended +0.0118, logged
#                       whichever way it goes
#   era-2 confirmatory  f = 0.50: median >= +0.005 across 47 states at the
#                       5% budget; dollar guard >= -0.005; 10% directional
#   era-2 challenger    f = 0.60, evaluated ONLY if confirmatory clears, four
#                       conditions exactly as written in the plan
#   one-shot rule       if the confirmatory arm fails, nothing else is tried
#                       on the FY2019 outcome
#
# SMOKE=1: 3 states, plumbing only. Nothing is serialized and no output
# directory is created (scratch-free by construction); no median, paired
# difference, branch or verdict is computed to disk or printed. The full run
# is launched separately after the senior-statistician review (routing rule).
#
# Outputs (full run) -> methods/freshshare_rewalk_era2/ :
#   support_reconciliation.csv (first output), bridge/per_state_paired.csv,
#   bridge/sensitivity_grid.csv, per_state_paired.csv, sensitivity_grid.csv,
#   summary.txt
# Read-only on state_delivery_lists/, the bench directory, the delivery pool
# caches and the era cache; no CHANGELOG entry, no version bump.

suppressMessages({ library(dplyr) })
source("rule_mining_helpers.R")

t0 <- Sys.time()
SMOKE <- Sys.getenv("SMOKE", "0") == "1"

OUT_ROOT   <- "methods/freshshare_rewalk_era2"
BRIDGE_DIR <- file.path(OUT_ROOT, "bridge")
if (!SMOKE) for (d in c(OUT_ROOT, BRIDGE_DIR))
  dir.create(d, showWarnings = FALSE, recursive = TRUE)

BENCH_DIR   <- "methods/anyerror_blended_holdout_2024"
BRIDGE_POOL <- "methods/delivery_pools_2022_2023_anyerror/filtered_national_any_error_fdr10.rds"
ERA2_POOL   <- "methods/state_similarity_v2/era_validation_train1718_test19/raw_vocab/raw_national.rds"
STAGE2_CSV  <- "methods/freshshare_rewalk/per_state_paired.csv"

E1_BUILD <- c("2022", "2023"); E1_SCORE <- "2024"
E2_BUILD <- c("2017", "2018"); E2_SCORE <- "2019"
ALL_YEARS <- c(E2_BUILD, E2_SCORE, E1_BUILD, E1_SCORE)
BUDGETS     <- c(0.05, 0.10)
BUFFER_MULT <- 3
RANK_STAT   <- "precision_train_lcb"
LCB_Z       <- 2.326
FDR_ALPHA   <- 0.10
MIN_TRAIN_FLAGGED <- 30
F_PRIMARY   <- 0.50
F_CHALLENGER<- 0.60
F_GRID      <- c(0.25, 0.40, 0.50, 0.60, 0.75)
WINDOW_TOPK <- 12000
BAR_BRIDGE  <- 0.005      # proceed/re-scope floor (plan)
ERA1_BLENDED<- 0.0118     # Eric's hypothesis reference (stage 2, section 33)
BAR_ERA2    <- 0.005      # confirmatory bar, 47 states
BAR_GUARD   <- -0.005     # dollar-recall guard, both arms' judged scale
CH_SIG_PREC <- 0          # challenger warning signature: 10% precision <= 0
CH_SIG_DR   <- -0.010     #   AND 10% dollar-recall change <= -0.010
DET_SAMPLE  <- 2000L      # SMOKE-only determinism subsample (seeded)
INELIGIBLE  <- c("Wyoming", "South Dakota")   # fixed by the plan's threshold
SMOKE_STATES<- c("Alabama", "California", "Michigan")

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
ie_all <- !is.na(adf$over_threshold) & adf$over_threshold != 0
stamp("frame: %d rows over FY%s", nrow(adf), paste(range(ALL_YEARS), collapse = "-"))

## ── support reconciliation (the run's first output) ───────────────────────────
states49 <- sort(unique(st))
supp <- do.call(rbind, lapply(states49, function(S) data.frame(
  state = S,
  build_rows_1718 = sum(st == S & fy %in% E2_BUILD),
  build_errors_1718 = sum(st == S & fy %in% E2_BUILD & ie_all),
  test_rows_19 = sum(st == S & fy == E2_SCORE),
  test_errors_19 = sum(st == S & fy == E2_SCORE & ie_all),
  stringsAsFactors = FALSE)))
supp$eligible <- supp$test_rows_19 >= 400 & supp$test_errors_19 >= 20
tot <- c(b = sum(supp$build_rows_1718), be = sum(supp$build_errors_1718),
         t = sum(supp$test_rows_19),  te = sum(supp$test_errors_19))
wy <- supp[supp$state == "Wyoming", ]; sd_ <- supp[supp$state == "South Dakota", ]
ok_supp <- tot[["b"]] == 79907 && tot[["be"]] == 7115 &&
  tot[["t"]] == 39221 && tot[["te"]] == 3931 &&
  wy$test_rows_19 == 273 && wy$test_errors_19 == 13 &&
  sd_$test_rows_19 == 654 && sd_$test_errors_19 == 15 &&
  identical(sort(supp$state[!supp$eligible]), sort(INELIGIBLE)) &&
  sum(supp$eligible) == 47
if (!ok_supp)
  stop("SUPPORT RECONCILIATION FAILED: recomputed FY2017-19 per-state support does not match the plan's table. No downstream number can be trusted.")
if (!SMOKE) write.csv(supp, file.path(OUT_ROOT, "support_reconciliation.csv"),
                      row.names = FALSE)
stamp("support reconciliation: PASS (build %d/%d, test %d/%d; WY 273/13, SD 654/15; eligible 47)",
      tot[["b"]], tot[["be"]], tot[["t"]], tot[["te"]])

elig_states <- supp$state[supp$eligible]
bridge_states <- if (SMOKE) intersect(states49, SMOKE_STATES) else states49
era2_states   <- if (SMOKE) intersect(elig_states, SMOKE_STATES) else elig_states

## ── era-2 cache determinism anchor (before any counterfactual number) ─────────
raw <- readRDS(ERA2_POOL)
stopifnot(nrow(raw) == 145313,
          all(c("hh", "rule", "n", "k") %in% names(raw)))
rows_e2b <- which(fy %in% E2_BUILD)
tr_nat <- adf[rows_e2b, , drop = FALSE]
ie_nat <- ie_all[rows_e2b]
strata_nat <- lapply(setNames(nm = HH_LEVELS), function(h)
  which(hh_group_of(tr_nat$cert_HH_size_FS_n) %in% h))
det_idx <- if (SMOKE) { set.seed(20260806); sort(sample.int(nrow(raw), DET_SAMPLE)) } else seq_len(nrow(raw))
t_det <- Sys.time()
nk <- reduce_flags_for_rules(raw[det_idx, c("hh", "rule")], tr_nat, strata_nat,
                             fun = function(ix) c(length(ix), sum(ie_nat[ix])),
                             label = sprintf("era-2 determinism (%d rules)",
                                             length(det_idx)))
det_secs <- as.numeric(difftime(Sys.time(), t_det, units = "secs"))
if (!identical(as.integer(nk[, 1]), as.integer(raw$n[det_idx])) ||
    !identical(as.integer(nk[, 2]), as.integer(raw$k[det_idx])))
  stop("ERA-2 CACHE DETERMINISM FAILED: recomputed FY2017-18 n/k does not match raw_national.rds. No downstream number can be trusted.")
stamp("era-2 cache determinism: PASS for %d/%d rules in %.1fs%s",
      length(det_idx), nrow(raw), det_secs,
      if (SMOKE) sprintf(" (SMOKE subsample; projected full recompute ~%.0f min)",
                         det_secs / length(det_idx) * nrow(raw) / 60) else "")

## ── shared walk machinery ─────────────────────────────────────────────────────
flags_from_index <- function(ci, hh, data, strata_idx) {
  idx <- eval_conditions(ci$conditions, data)
  out <- vector("list", length(hh))
  for (h in unique(hh)) {
    rows <- which(hh == h)
    out[rows] <- rule_flag_idx(ci$rule_conds[rows], idx, base_idx = strata_idx[[h]])
  }
  out
}

# the shipped fill (INCL_build_blended_delivery_list_v2.R semantics, section
# 27 windowed form with the slack certificate; verbatim from
# methods/certified_fill_check_v2.R)
fill_windowed <- function(idx, n_rows, budget) {
  cap <- floor(budget * n_rows); cap_buf <- floor(BUFFER_MULT * budget * n_rows)
  un <- rep(FALSE, n_rows); n_in <- 0L
  core <- integer(0); buffer <- integer(0)
  for (i in seq_along(idx)) {
    add <- sum(!un[idx[[i]]])
    if (add == 0) next
    if (n_in + add <= cap) { un[idx[[i]]] <- TRUE; n_in <- n_in + add; core <- c(core, i) }
    else if (n_in + add <= cap_buf) { un[idx[[i]]] <- TRUE; n_in <- n_in + add; buffer <- c(buffer, i) }
  }
  list(core = core, buffer = buffer, slack = cap_buf - n_in, n_in = n_in)
}

# two-pass walk, VERBATIM from methods/freshshare_rewalk_v2.R (reviewed):
# phase 1 fills the core to exactly c_core, phase 2 the buffer to exactly
# c_tot; priority scan (f >= fmin) then, only if short, a completion scan.
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
       depth = if (length(sel)) max(sel) else 0L)
}

n_f0_identity <- 0L; n_cap_assert <- 0L
n_window_redo <- 0L; n_base_redo <- 0L; n_completion_used <- 0L

# One state under one arm: baseline (shipped fill), F_MIN = 0 identity anchor,
# then the counterfactual grid. Returns per-fmin metric rows.
run_state <- function(S, arm, pool, build_years, score_year) {
  tr <- adf[st == S & fy %in% build_years, , drop = FALSE]
  te <- adf[st == S & fy == score_year, , drop = FALSE]
  strata_tr <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(tr$cert_HH_size_FS_n) %in% h))
  strata_te <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(te$cert_HH_size_FS_n) %in% h))
  ie_te <- !is.na(te$over_threshold) & te$over_threshold != 0
  ed_te <- te$total_error_amount; ed_te[is.na(ed_te)] <- 0
  ed_te <- ifelse(ie_te, abs(ed_te), 0)
  w_te  <- te$fywgt; w_te[is.na(w_te)] <- 0
  tot_unw <- sum(ed_te); tot_wtd <- sum(w_te * ed_te)

  W <- min(WINDOW_TOPK, nrow(pool))
  win <- pool[seq_len(W), , drop = FALSE]
  ci_w <- build_condition_index(win$rule)
  idx_tr <- flags_from_index(ci_w, win$hh, tr, strata_tr)
  idx_te <- flags_from_index(ci_w, win$hh, te, strata_te)
  nfl_w  <- lengths(idx_tr)
  full <- NULL
  get_full <- function() {
    if (is.null(full)) {
      ci_f <- build_condition_index(pool$rule)
      full <<- list(idx_tr = flags_from_index(ci_f, pool$hh, tr, strata_tr),
                    idx_te = flags_from_index(ci_f, pool$hh, te, strata_te))
      full$nfl <<- lengths(full$idx_tr)
    }
    full
  }
  score_core <- function(sel, role, idx_te_used) {
    hit <- rep(FALSE, nrow(te))
    for (j in which(role == "core")) hit[idx_te_used[[sel[j]]]] <- TRUE
    n <- sum(hit); k <- sum(ie_te & hit)
    list(n = n, k = k, prec = if (n > 0) k / n else NA_real_,
         dr_unw = if (tot_unw > 0) sum(ed_te[hit]) / tot_unw else NA_real_,
         dr_wtd = if (tot_wtd > 0) sum((w_te * ed_te)[hit]) / tot_wtd else NA_real_,
         share = n / nrow(te))
  }

  out <- list()
  for (b in BUDGETS) {
    # baseline: the shipped fill, windowed with the section 27 slack-zero
    # certificate; slack > 0 in the window -> redo unpruned (the unpruned
    # fill's own slack, if any, is the true baseline: pool exhausted)
    bl <- fill_windowed(idx_tr, nrow(tr), b)
    base_redo <- FALSE
    if (bl$slack > 0) {
      base_redo <- TRUE; n_base_redo <<- n_base_redo + 1L
      fu <- get_full()
      bl <- fill_windowed(fu$idx_tr, nrow(tr), b)
    }
    idx_tr_u <- if (base_redo) get_full()$idx_tr else idx_tr
    idx_te_u <- if (base_redo) get_full()$idx_te else idx_te
    nfl_u    <- if (base_redo) get_full()$nfl else nfl_w
    base_sel  <- c(bl$core, bl$buffer)
    base_role <- rep(c("core", "buffer"), c(length(bl$core), length(bl$buffer)))
    # consumed capacities from a re-walk of the baseline in take order
    un <- rep(FALSE, nrow(tr)); nn <- integer(length(base_sel))
    for (j in seq_along(base_sel)) {
      ix <- idx_tr_u[[base_sel[j]]]; nn[j] <- sum(!un[ix]); un[ix] <- TRUE
    }
    C0 <- sum(nn[base_role == "core"]); CT <- sum(nn)
    stopifnot(CT == bl$n_in)

    # F_MIN = 0 identity anchor: the two-pass walk with the baseline's
    # consumed capacities must reproduce the shipped fill rule-for-rule
    w0 <- walk2(idx_tr_u, nfl_u, nrow(tr), C0, CT, 0)
    if (w0$short || !identical(w0$sel, base_sel) || !identical(w0$role, base_role))
      stop(sprintf("BASELINE IDENTITY ANCHOR FAILED [%s %s %.0f%%]: the F_MIN = 0 two-pass walk does not reproduce the shipped fill logic on identical inputs.",
                   arm, S, 100 * b))
    n_f0_identity <<- n_f0_identity + 1L
    base <- score_core(base_sel, base_role, idx_te_u)

    for (fmin in F_GRID) {
      # windowed two-pass; certificate per the stage-2 review: under-fill OR
      # any windowed completion use triggers the unpruned redo
      wk <- walk2(idx_tr, nfl_w, nrow(tr), C0, CT, fmin)
      redo <- FALSE
      if (wk$short || wk$used_completion) {
        redo <- TRUE; n_window_redo <<- n_window_redo + 1L
        fu <- get_full()
        wk <- walk2(fu$idx_tr, fu$nfl, nrow(tr), C0, CT, fmin)
        if (wk$short)
          stop(sprintf("CAPACITY ASSERTION FAILED [%s %s %.0f%% f_min=%.2f]: the completion pass exhausted the FULL pool at %d against targets %d/%d. A deviation is a bug that halts the run, never a result.",
                       arm, S, 100 * b, fmin, wk$n_in, C0, CT))
      }
      if (wk$used_completion) n_completion_used <<- n_completion_used + 1L
      core_in <- sum(wk$n_new[wk$role == "core"])
      if (core_in != C0 || wk$n_in != CT)
        stop(sprintf("CAPACITY ASSERTION FAILED [%s %s %.0f%% f_min=%.2f]: consumed core %d vs baseline %d, total %d vs %d.",
                     arm, S, 100 * b, fmin, core_in, C0, wk$n_in, CT))
      n_cap_assert <<- n_cap_assert + 1L
      sc <- score_core(wk$sel, wk$role, if (redo) get_full()$idx_te else idx_te)
      out[[paste(b, fmin)]] <- data.frame(
        arm = arm, state = S, budget = 100 * b, f_min = fmin,
        n_rules_core = sum(wk$role == "core"), n_rules_total = length(wk$sel),
        used_completion = wk$used_completion, window_redone = redo,
        baseline_redone = base_redo, walk_depth = wk$depth,
        base_precision = base$prec, rewalk_precision = sc$prec,
        precision_diff = sc$prec - base$prec,
        base_errors = base$k, rewalk_errors = sc$k,
        base_dollar_recall_unw = base$dr_unw, rewalk_dollar_recall_unw = sc$dr_unw,
        dollar_recall_unw_diff = sc$dr_unw - base$dr_unw,
        base_dollar_recall_wtd = base$dr_wtd, rewalk_dollar_recall_wtd = sc$dr_wtd,
        dollar_recall_wtd_diff = sc$dr_wtd - base$dr_wtd,
        base_flagged_share = base$share, rewalk_flagged_share = sc$share,
        fill_rate = sc$share / b, stringsAsFactors = FALSE)
    }
  }
  bind_rows(out)
}

grid_of <- function(pr) pr %>%
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

## ── ARM A: the bridge (era 1, national-only) ─────────────────────────────────
bpool <- readRDS(BRIDGE_POOL)
bpool <- bpool[order(-bpool[[RANK_STAT]], bpool$hh, bpool$rule, method = "radix"),
               , drop = FALSE]
stamp("ARM A bridge pool: %d national rules (undeduped admitted cache, per the plan)",
      nrow(bpool))

# bridge LCB anchor: national rules on the committed bench lists carry the
# same precision_train_lcb in this pool
key_pool <- paste(bpool$hh, bpool$rule)
n_lcb_checked <- 0L
for (S in bridge_states) for (b in BUDGETS) {
  bench <- read.csv(file.path(BENCH_DIR, sprintf("bench_list_%s_budget%02.0f.csv",
                                                 gsub(" ", "_", S), 100 * b)),
                    stringsAsFactors = FALSE)
  nat <- bench[bench$pool == "national", , drop = FALSE]
  m <- match(paste(nat$hh, nat$rule), key_pool)
  if (any(is.na(m)) ||
      !isTRUE(all.equal(bpool[[RANK_STAT]][m], nat$precision_train_lcb, tolerance = 1e-9)))
    stop(sprintf("BRIDGE LCB ANCHOR FAILED [%s %.0f%%]: bench national rows do not match the bridge pool's precision_train_lcb.",
                 S, 100 * b))
  n_lcb_checked <- n_lcb_checked + nrow(nat)
}
stamp("bridge LCB anchor: PASS (%d bench national rows matched over %d lists)",
      n_lcb_checked, length(bridge_states) * length(BUDGETS))

rows_bridge <- list()
for (S in bridge_states) {
  tS <- Sys.time()
  rows_bridge[[S]] <- run_state(S, "bridge", bpool, E1_BUILD, E1_SCORE)
  stamp("bridge %-20s done in %5.1fs", S,
        as.numeric(difftime(Sys.time(), tS, units = "secs")))
}
pr_bridge <- bind_rows(rows_bridge)
if (!SMOKE) {
  write.csv(pr_bridge, file.path(BRIDGE_DIR, "per_state_paired.csv"), row.names = FALSE)
  write.csv(grid_of(pr_bridge), file.path(BRIDGE_DIR, "sensitivity_grid.csv"),
            row.names = FALSE)
}

## ── the pre-registered branch (machine-read; humans stay blind until summary) ─
proceed <- TRUE
if (!SMOKE) {
  med_bridge <- median(pr_bridge$precision_diff[pr_bridge$f_min == F_PRIMARY &
                                                pr_bridge$budget == 5])
  proceed <- med_bridge >= BAR_BRIDGE
} else {
  stamp("SMOKE: branch not evaluated (no median computed); era-2 plumbing exercised unconditionally")
}

summary_lines <- function(era2_ran, pr_e2 = NULL) {
  L <- c("BRIDGE + ERA-2 FRESH-SHARE REPLICATION: verdicts",
         sprintf("Plan: methods/era2_freshshare_replication_plan_2026-08-06.md | run %s",
                 format(Sys.time(), "%Y-%m-%d %H:%M")),
         sprintf("Anchors: support reconciliation PASS; era-2 cache determinism PASS (%d rules); bridge LCB anchor PASS (%d rows); baseline identity %d walks; capacity assertions %d, all exact; window redos %d (+%d baseline)",
                 length(det_idx), n_lcb_checked, n_f0_identity, n_cap_assert,
                 n_window_redo, n_base_redo), "")
  med10 <- median(pr_bridge$precision_diff[pr_bridge$f_min == F_PRIMARY &
                                           pr_bridge$budget == 10])
  L <- c(L,
    sprintf("ARM A BRIDGE (era 1, national-only, %d states, f = %.2f):",
            length(bridge_states), F_PRIMARY),
    sprintf("  5%% median paired precision diff = %+.4f (10%%: %+.4f)", med_bridge, med10),
    sprintf("  Eric's hypothesis (dilution: bridge >= blended %+.4f): %s",
            ERA1_BLENDED,
            ifelse(med_bridge >= ERA1_BLENDED, "CONFIRMED", "REFUTED (logged, per the plan, whichever way it goes)")),
    sprintf("  proceed/re-scope floor %+.3f: %s", BAR_BRIDGE,
            ifelse(proceed, "PROCEED - era 2 runs under this plan",
                   paste("RE-SCOPE BRANCH FIRED - era 2 is NOT run under this plan;",
                         "the finding logged is that the floor's benefit is concentrated",
                         "in the interaction with state-rule slots, Eric's dilution",
                         "hypothesis is refuted in writing, and any re-scoped era-2",
                         "design requires a fresh pre-registration."))),
    "  (The bridge carries no shipping verdict of its own.)",
    paste("  CAVEAT (required by the pre-launch review): the bridge pool is the",
          "undeduped admitted cache (50,697 rules) while the stage-2 bench walk",
          "consumed its deduped derivative (32,633), so the comparison against",
          "the blended +0.0118 carries dedup status as a second difference",
          "alongside pool composition; dedup is not walk-neutral under an",
          "f-floor (dominance-dropped tighter rules are high-fresh-share",
          "substitutes). The bridge-vs-era-2 link, which the shipping gate",
          "rides on, is undeduped on both sides and unaffected."), "",
    "Bridge sensitivity grid (descriptive only):",
    capture.output(print(as.data.frame(grid_of(pr_bridge) %>%
      mutate(across(where(is.numeric), ~ round(.x, 4)))), row.names = FALSE)), "")
  if (era2_ran) {
    p5  <- pr_e2 %>% filter(f_min == F_PRIMARY, budget == 5)
    p10 <- pr_e2 %>% filter(f_min == F_PRIMARY, budget == 10)
    m5 <- median(p5$precision_diff); m10 <- median(p10$precision_diff)
    g5 <- median(p5$dollar_recall_unw_diff)
    conf <- m5 >= BAR_ERA2 && g5 >= BAR_GUARD
    L <- c(L,
      sprintf("ARM B ERA-2 CONFIRMATORY (f = %.2f, %d states, build FY2017-18, score FY2019):",
              F_PRIMARY, nrow(p5)),
      sprintf("  5%% median = %+.4f vs bar %+.3f -> %s", m5, BAR_ERA2,
              ifelse(m5 >= BAR_ERA2, "CLEARED", "FAILED")),
      sprintf("  dollar guard (5%%, unweighted) = %+.4f vs %+.3f -> %s", g5, BAR_GUARD,
              ifelse(g5 >= BAR_GUARD, "HELD", "FAILED")),
      sprintf("  secondary (10%%, directional) = %+.4f -> %s", m10,
              ifelse(m10 > 0, "positive", "not positive")))
    s2 <- if (file.exists(STAGE2_CSV)) read.csv(STAGE2_CSV, stringsAsFactors = FALSE) else NULL
    if (!is.null(s2)) {
      d1 <- s2$precision_diff[s2$f_min == F_PRIMARY & s2$budget == 5]
      L <- c(L, sprintf("  two-era pooled median (stage-2 blended %d states + era-2 %d states) = %+.4f",
                        length(d1), nrow(p5), median(c(d1, p5$precision_diff))))
    }
    if (conf) {
      c5  <- pr_e2 %>% filter(f_min == F_CHALLENGER, budget == 5)
      c10 <- pr_e2 %>% filter(f_min == F_CHALLENGER, budget == 10)
      ch1 <- median(c5$precision_diff) >= BAR_ERA2
      ch2 <- median(c5$precision_diff) > m5
      ch3 <- median(c5$dollar_recall_unw_diff) >= BAR_GUARD
      sig <- median(c10$precision_diff) <= CH_SIG_PREC &&
             median(c10$dollar_recall_unw_diff) <= CH_SIG_DR
      ch4 <- !sig
      L <- c(L, "",
        sprintf("ARM C CHALLENGER (f = %.2f; evaluated because the confirmatory arm cleared):", F_CHALLENGER),
        sprintf("  1. clears the era-2 bar (median %+.4f >= %+.3f): %s",
                median(c5$precision_diff), BAR_ERA2, ch1),
        sprintf("  2. strictly beats 0.50's era-2 5%% median (%+.4f > %+.4f): %s",
                median(c5$precision_diff), m5, ch2),
        sprintf("  3. holds the 5%% dollar guard (%+.4f >= %+.3f): %s",
                median(c5$dollar_recall_unw_diff), BAR_GUARD, ch3),
        sprintf("  4. no era-1 warning signature at era 2 (10%%: prec %+.4f, dollar %+.4f; signature = prec <= %.0f AND dollar <= %+.3f): %s",
                median(c10$precision_diff), median(c10$dollar_recall_unw_diff),
                CH_SIG_PREC, CH_SIG_DR, ch4),
        sprintf("  challenger compound rule: %s",
                ifelse(ch1 && ch2 && ch3 && ch4,
                       "MET - 0.60 goes to Eric as the proposed threshold",
                       "NOT MET - the shipping threshold stays 0.50")))
    } else {
      L <- c(L, "",
        "ARM C CHALLENGER: NOT evaluated (the confirmatory arm did not clear;",
        "evaluating it would choose the threshold on the era-2 outcome).")
    }
    L <- c(L, "",
      if (conf) paste(
        "CONSEQUENCE (plan, pre-stated): bridge held and confirmatory cleared",
        "(guards held) - the fresh-share floor is validated on two eras and",
        "goes to Eric as a MINOR-bump promotion decision for",
        "INCL_build_blended_delivery_list_v2.R. Either way the decision, the",
        "CHANGELOG entry, and the version bump are Eric's, not this study's.")
      else paste(
        "CONSEQUENCE (plan, pre-stated): confirmatory failed (or its guard",
        "failed) - the section 20 pattern. The era-1 result stands as recorded",
        "in section 33; the floor does not ship; the ledger row closes the",
        "line at public-data scale in writing, open at internal-data scale",
        "only. ONE-SHOT RULE: no other threshold, mechanism, or instrument is",
        "then tried on the FY2019 outcome."), "",
      "Era-2 sensitivity grid (descriptive only):",
      capture.output(print(as.data.frame(grid_of(pr_e2) %>%
        mutate(across(where(is.numeric), ~ round(.x, 4)))), row.names = FALSE)))
  }
  L
}

if (!SMOKE && !proceed) {
  writeLines(summary_lines(era2_ran = FALSE), file.path(OUT_ROOT, "summary.txt"))
  stamp("re-scope branch fired: era 2 not run under this plan; bridge outputs and summary written to %s/", OUT_ROOT)
  quit(save = "no", status = 0)
}

## ── ARMS B/C: era 2 (national-only, 47 states) ────────────────────────────────
# admission recomputed from the cached n/k with the SHIPPED recipe: pooled BH
# at FDR 10% against the FY2017-18 stratum base rates AND n >= 30 (NOT the era
# scripts' within-stratum variant); ordering = 99% Wilson LCB from cached n/k
base_by_hh <- vapply(strata_nat, function(rr) mean(ie_nat[rr]), numeric(1))
pvals <- pbinom(raw$k - 1, raw$n, base_by_hh[raw$hh], lower.tail = FALSE)
m <- length(pvals); o <- order(pvals)
thr <- max(c(0L, which(pvals[o] <= FDR_ALPHA * seq_len(m) / m)))
bh <- rep(FALSE, m); if (thr > 0) bh[o[seq_len(thr)]] <- TRUE
keep <- bh & raw$n >= MIN_TRAIN_FLAGGED
e2 <- raw[keep, , drop = FALSE]
e2[[RANK_STAT]] <- wilson_lcb(e2$k, e2$n, LCB_Z)
e2 <- e2[order(-e2[[RANK_STAT]], e2$hh, e2$rule, method = "radix"), , drop = FALSE]
stamp("era-2 admitted pool: %d of %d rules (pooled BH FDR %.0f%% AND n >= %d)",
      nrow(e2), nrow(raw), 100 * FDR_ALPHA, MIN_TRAIN_FLAGGED)

rows_e2 <- list()
for (S in era2_states) {
  tS <- Sys.time()
  rows_e2[[S]] <- run_state(S, "era2", e2, E2_BUILD, E2_SCORE)
  stamp("era2   %-20s done in %5.1fs", S,
        as.numeric(difftime(Sys.time(), tS, units = "secs")))
}
pr_e2 <- bind_rows(rows_e2)
if (!SMOKE) {
  write.csv(pr_e2, file.path(OUT_ROOT, "per_state_paired.csv"), row.names = FALSE)
  write.csv(grid_of(pr_e2), file.path(OUT_ROOT, "sensitivity_grid.csv"),
            row.names = FALSE)
  writeLines(summary_lines(era2_ran = TRUE, pr_e2 = pr_e2),
             file.path(OUT_ROOT, "summary.txt"))
}

el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
if (SMOKE) {
  cat("\n================ SMOKE PLUMBING SUMMARY (no median, branch or verdict computed) ================\n")
  cat("support reconciliation (plan table, WY/SD, 47 eligible) : PASS\n")
  cat(sprintf("era-2 cache determinism (seeded subsample)              : PASS %d/%d rules in %.1fs -> projected full recompute ~%.0f min\n",
              length(det_idx), nrow(raw), det_secs,
              det_secs / length(det_idx) * nrow(raw) / 60))
  cat(sprintf("bridge LCB anchor (bench national rows == pool)         : PASS, %d rows over %d lists\n",
              n_lcb_checked, length(bridge_states) * length(BUDGETS)))
  cat(sprintf("baseline identity anchors (F_MIN=0 == shipped fill)     : %d/%d walks\n",
              n_f0_identity, (length(bridge_states) + length(era2_states)) * length(BUDGETS)))
  cat(sprintf("capacity assertions (exact core AND total)              : %d walks, all passed\n",
              n_cap_assert))
  cat(sprintf("window redos: %d walk (under-fill OR windowed completion) + %d baseline (slack > 0)\n",
              n_window_redo, n_base_redo))
  cat(sprintf("completion pass used (on the walk actually used)        : %d walks\n",
              n_completion_used))
  cat(sprintf("result rows in memory only, NOTHING serialized, no output dir created: bridge %d + era2 %d\n",
              nrow(pr_bridge), nrow(pr_e2)))
  cat(sprintf("wall time: %.1f minutes\n", el))
} else {
  stamp("done in %.1f minutes; outputs in %s/", el, OUT_ROOT)
}
