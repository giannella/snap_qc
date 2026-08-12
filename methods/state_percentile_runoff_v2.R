# EXPLORATORY state-scale comparison: Ben's as-built within-state percentiles
# (additive) vs the per-size vocabulary. The new S37.
#
# EXPLORATORY, by design (Eric 2026-08-11): no pre-registered bars, no winner
# rule, no ledger rows. The readout is presented to Eric with the standard
# companions; any vocabulary decision is his, made on the readout.
# Design note: methods/design_note_state_pctl_runoff_2026-08-11.md.
# Reviewed 2026-08-11 (fresh senior-statistician): REVISE -> B1 stratum
# evaluation fixed, B3 per-phase gap reporting added, B2 record amendments
# made; flag-profile layer implements the reviewer's proposed design.
#
# Question (framing per Eric): are within-state percentiles better for
# identifying outliers? Ben's percentile features (features.R, the frame's
# as-built `_p` columns: CPI-deflated dollars ranked by cume_dist within
# state x household-size cells, ALL SIX frame years pooled - 2017-19 and
# 2022-24 - zeros pinned to 0) are used AS BUILT, no override. Pre-stated
# reading limit: the pooled fit includes FY2024, so the benp arm's features
# carry information about the test year that the persize arm's do not;
# faithful to deployment-on-current-caseload, but a small benp win could be
# the pooling rather than the representation.
#
# Arms (Eric 2026-08-11, additive):
#   persize = the 16 shipped-in-practice features + the 3 per-size income
#             features (19)
#   benp    = persize + Ben's 7 as-built percentile features (26)
# Mining: any_error frame x 3 HH strata, PER STATE, train FY2022-23,
# seed 117 (single seed, per Eric). Admission: ONE joint BH FDR 10% across
# the state's candidates with per-stratum base rates in the p-values
# (review catch 2026-08-09) AND n >= 30. Ordering: 99% Wilson LCB.
# Walk: the shipped fresh-share walk semantics (v2.4.0, f = 0.50, walk2
# two-phase re-walk) with ONE study-scale deviation, reviewed and accepted
# 2026-08-11: exact refill is an empirical property of large blended pools,
# not a guarantee, and at state-pool scale the completion pass can strand a
# remainder (smoke: 190 vs 191). The shortfall is TOLERATED and REPORTED
# per phase (fill_gap_core / fill_gap_total in the readout), never a crash
# and never silently absorbed. Fill FY2022-23 to 5%/10% core + 3x buffer,
# freeze, walk FY2024 in delivered order to that year's cap, outcome-free.
#
# SMOKE=1: 3 states, tiny ensembles, own output dir.
# Expects `reg_model_data`. Outputs -> methods/state_percentile_runoff_v2/.

suppressMessages(library(dplyr))
source("rule_mining_helpers.R")

## ---- config -----------------------------------------------------------------
SMOKE <- identical(Sys.getenv("SMOKE"), "1")

SEED        <- 117
BUDGETS     <- c(0.05, 0.10)
BUFFER_MULT <- 3
FRESH_MIN   <- 0.50            # SORT_WALK_MIN_FRESH_SHARE, the shipped floor
LCB_Z       <- 2.326
FDR_ALPHA   <- 0.10
MIN_N       <- 30
TRAIN_YEARS <- c("2022", "2023")
TEST_YEAR   <- "2024"
XGB <- list(nrounds = 1000, max_depth = 4, eta = 0.02, subsample = 0.20)
RF  <- list(num_trees = 1000, max_depth = 4, mtry = 2, min_node_size = 20)
SIGNIF_DIGITS <- 3

EXPECT_ROWS_FRAME <- 231619L
EXPECT_TRAIN_ROWS <- 76031L; EXPECT_TRAIN_ERRS <- 8397L
EXPECT_TEST_ROWS  <- 39528L; EXPECT_TEST_ERRS  <- 4764L

BASE_FEATURES <- c(
  "HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size",
  "expedited_i", "cat_elig", "rawben_rel_max", "medical_deductions",
  "shelter_expenses_by_hh_size", "utilities", "married", "homeless",
  "percent_abawd", "unc_rawben_rel_max", "months_since_cert_n",
  "count_divisible_by_100")
PS_FEATURES <- c("gross_by_hh_size", "earned_by_hh_size", "unearned_by_hh_size")
P7 <- c("rawgrinc_p", "rawearn_p", "rawunearn_p", "rawmedded_p",
        "rawdepded_p", "rawcsded_p", "rawrent_p")
ARMS <- list(persize = c(BASE_FEATURES, PS_FEATURES),
             benp    = c(BASE_FEATURES, PS_FEATURES, P7))

OUT_DIR <- "methods/state_percentile_runoff_v2"
if (!exists("RESUME_FROM_CHECKPOINT")) RESUME_FROM_CHECKPOINT <- FALSE
if (SMOKE) {
  XGB$nrounds <- 40; RF$num_trees <- 40
  OUT_DIR <- file.path(OUT_DIR, "smoke")
}
CACHE_DIR <- file.path(OUT_DIR, "cache")
dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)

HH_LEVELS <- c("1", "2-3", "4+")
hh_group_of <- function(n) {
  n <- suppressWarnings(as.numeric(as.character(n)))
  ifelse(is.na(n), NA_character_, ifelse(n <= 1, "1", ifelse(n <= 3, "2-3", "4+")))
}
stamp <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"),
                                   sprintf(...)))

## ---- frame: AS BUILT, no percentile override --------------------------------
stopifnot(nrow(reg_model_data) == EXPECT_ROWS_FRAME)
for (pc in P7) {
  v <- reg_model_data[[pc]]
  if (is.null(v)) stop("as-built percentile column missing from frame: ", pc)
  if (any(is.na(v)) || min(v) < 0 || max(v) > 1)
    stop("as-built percentile column out of range or NA: ", pc)
}

adf0 <- reg_model_data %>% filter(fiscal_year %in% c(TRAIN_YEARS, TEST_YEAR))
pf  <- prep_features(adf0, unique(unlist(ARMS)))
adf <- pf$data
for (an in names(ARMS)) {
  miss <- setdiff(ARMS[[an]], pf$features)
  if (length(miss)) stop(sprintf("arm %s: features missing after prep: %s",
                                 an, paste(miss, collapse = ", ")))
}
st <- as.character(adf$state)
yr <- as.character(adf$fiscal_year)
ie_all <- !is.na(adf$over_threshold) & adf$over_threshold != 0
ed_all <- ifelse(ie_all, abs(ifelse(is.na(adf$total_error_amount), 0,
                                    adf$total_error_amount)), 0)
stopifnot(sum(yr %in% TRAIN_YEARS) == EXPECT_TRAIN_ROWS,
          sum(ie_all[yr %in% TRAIN_YEARS]) == EXPECT_TRAIN_ERRS,
          sum(yr == TEST_YEAR) == EXPECT_TEST_ROWS,
          sum(ie_all[yr == TEST_YEAR]) == EXPECT_TEST_ERRS)

STATES <- sort(unique(st))
if (SMOKE) STATES <- c("Washington", "Maine", "Mississippi")
stamp("frame ready: %d states, %d features prepped", length(STATES),
      length(pf$features))

## ---- shared helpers ---------------------------------------------------------
admit_rank <- function(rdf, n, k, base_rate) {
  # ONE BH at FDR 10% across the state's candidates, per-stratum base rates
  # inside the p-values (joint correction; findings 19 as shipped, review
  # catch 2026-08-09), AND n >= MIN_N; 99%-LCB order
  rdf$n <- as.integer(n); rdf$k <- k
  pvals <- pbinom(rdf$k - 1, rdf$n, base_rate, lower.tail = FALSE)
  m <- length(pvals); o <- order(pvals)
  thr <- max(c(0L, which(pvals[o] <= FDR_ALPHA * seq_len(m) / m)))
  bh <- rep(FALSE, m); if (thr > 0) bh[o[seq_len(thr)]] <- TRUE
  adm <- rdf[bh & rdf$n >= MIN_N, , drop = FALSE]
  if (!nrow(adm)) return(adm)
  adm$lcb <- wilson_lcb(adm$k, adm$n, LCB_Z)
  adm[order(-adm$lcb, -adm$n, adm$hh, adm$rule, method = "radix"), ,
      drop = FALSE]
}

walk_freshshare <- function(idx_tr, idx_te, n_tr, n_te, b) {
  # The shipped fresh-share walk semantics (INCL_build_blended_delivery_
  # list_v2.R, findings 33-34): pass zero = the legacy floor-0 scan fixing
  # consumed core (C0) and total (CT) capacity; then the two-phase
  # fresh-share re-walk (priority scan at f >= FRESH_MIN, completion scan
  # at n_new >= 1) refills toward C0/CT. STUDY-SCALE DEVIATION (smoke
  # finding 2026-08-11, reviewed and accepted): exact refill is an
  # EMPIRICAL property of large blended pools (98/98 on the release
  # regenerations), not a mathematical guarantee; at state-only pool scale
  # the completion pass can strand a remainder no untaken rule fits
  # (smoke: 190 vs 191). The shortfall is TOLERATED and REPORTED per phase
  # (fill_gap_core / fill_gap_total), never asserted and never silent: a
  # phase-1 gap moves the core/buffer boundary and hence the delivered
  # order, so both gaps are per-cell readout columns. Arm comparability
  # rides on the shared FY2024 test-year cap, not on exact train-fill.
  # The FY2024 score walk runs the frozen list in delivered order to that
  # year's cap, outcome-free.
  cap <- floor(b * n_tr); cap_buf <- floor(BUFFER_MULT * b * n_tr)
  nfl <- vapply(idx_tr, length, integer(1))
  un <- rep(FALSE, n_tr); n_in <- 0L; n_core <- 0L
  frozen <- integer(0); buffer <- integer(0)
  for (i in seq_along(idx_tr)) {
    add <- sum(!un[idx_tr[[i]]]); if (add == 0) next
    if (n_in + add <= cap) {
      un[idx_tr[[i]]] <- TRUE; n_in <- n_in + add; n_core <- n_core + add
      frozen <- c(frozen, i)
    } else if (n_in + add <= cap_buf) {
      un[idx_tr[[i]]] <- TRUE; n_in <- n_in + add; buffer <- c(buffer, i)
    }
  }
  fill_gap_core <- 0L; fill_gap_total <- 0L
  if (FRESH_MIN > 0) {
    C0 <- n_core; CT <- n_in
    un <- rep(FALSE, n_tr); n_in <- 0L
    taken <- logical(length(idx_tr))
    frozen <- integer(0); buffer <- integer(0)
    for (ph in 1:2) {
      tgt <- if (ph == 1) C0 else CT
      for (ps in 1:2) {
        if (n_in >= tgt) break
        for (i in seq_along(idx_tr)) {
          if (taken[i]) next
          ix <- idx_tr[[i]]
          add <- sum(!un[ix])
          if (add == 0L) next
          if (ps == 1 && add / nfl[i] < FRESH_MIN) next
          if (n_in + add > tgt) next
          un[ix] <- TRUE; n_in <- n_in + add; taken[i] <- TRUE
          if (ph == 1) frozen <- c(frozen, i) else buffer <- c(buffer, i)
          if (n_in == tgt) break
        }
      }
      if (ph == 1) fill_gap_core <- C0 - n_in
    }
    fill_gap_total <- CT - n_in
    stopifnot(fill_gap_core >= 0L, fill_gap_total >= 0L)
  }
  sel <- c(frozen, buffer)
  cap24 <- floor(b * n_te); un24 <- rep(FALSE, n_te)
  deployed <- integer(0); n_new_dep <- integer(0)
  for (i in sel) {
    add <- sum(!un24[idx_te[[i]]])
    if (add > 0 && sum(un24) + add <= cap24) {
      un24[idx_te[[i]]] <- TRUE
      deployed <- c(deployed, i); n_new_dep <- c(n_new_dep, add)
    }
  }
  list(hit = un24, fill_cases = n_in, fill_gap_core = fill_gap_core,
       fill_gap_total = fill_gap_total, cap = cap, cap_buf = cap_buf,
       n_core_rules = length(frozen), n_rules = length(sel),
       deployed = deployed, n_new_dep = n_new_dep)
}

mine_state_arm <- function(state, arm) {
  fn <- file.path(CACHE_DIR, sprintf("ae_%s_%s_%d.rds",
                                     gsub(" ", "_", state), arm, SEED))
  if (RESUME_FROM_CHECKPOINT && file.exists(fn)) return(readRDS(fn))
  tr_s <- which(st == state & yr %in% TRAIN_YEARS)
  trs <- adf[tr_s, , drop = FALSE]
  hh_s <- hh_group_of(trs$cert_HH_size_FS_n)
  strata_s <- lapply(setNames(nm = HH_LEVELS), function(h) which(hh_s %in% h))
  ie_s <- ie_all[tr_s]
  for (h in HH_LEVELS)   # support print (design-note item 3)
    cat(sprintf("  [%s | %s | stratum %s] %d rows, %d events\n",
                state, arm, h, length(strata_s[[h]]), sum(ie_s[strata_s[[h]]])))
  rdf <- mine_rule_vocabulary(
    trs, list(any_error = list(rows = seq_len(nrow(trs)), ie = ie_s)),
    strata_s, ARMS[[arm]],
    xgb = XGB, rf = RF, signif_digits = SIGNIF_DIGITS, seed = SEED,
    verbose = FALSE)
  adm <- NULL
  if (!is.null(rdf) && nrow(rdf)) {
    fl <- flags_for_rules(rdf, trs, strata_s, label = "")
    n <- vapply(fl, length, integer(1))
    k <- vapply(fl, function(ix) sum(ie_s[ix]), numeric(1))
    base_by_hh <- vapply(strata_s, function(rows) mean(ie_s[rows]), numeric(1))
    adm <- admit_rank(rdf, n, k, base_by_hh[rdf$hh])
  }
  if (is.null(adm)) adm <- data.frame(hh = character(0), rule = character(0))
  saveRDS(adm, fn)
  adm
}

uses_p_feature <- function(rules) {
  pat <- paste0("\\b(", paste(P7, collapse = "|"), ")\\b")
  grepl(pat, rules)
}

# parse `_p` conditions out of canonical rule text ("var op number" clauses)
p_conditions_of <- function(rule) {
  clauses <- strsplit(rule, " & ", fixed = TRUE)[[1]]
  m <- regmatches(clauses, regexec(
    paste0("^\\s*(", paste(P7, collapse = "|"),
           ")\\s*(<=|>=|<|>)\\s*([0-9.]+)\\s*$"), clauses))
  m <- m[lengths(m) == 4]
  if (!length(m)) return(NULL)
  data.frame(var = vapply(m, `[`, "", 2), op = vapply(m, `[`, "", 3),
             threshold = as.numeric(vapply(m, `[`, "", 4)))
}

## ---- the runoff -------------------------------------------------------------
stamp("=== EXPLORATORY percentile runoff (benp vs persize), %d states ===",
      length(STATES))
readout <- list(); p_inventory <- list(); p_catch <- list()
p_var_profile <- list(); incr_catch <- list()
for (state in STATES) {
  tr_s <- which(st == state & yr %in% TRAIN_YEARS)
  te_s <- which(st == state & yr == TEST_YEAR)
  trs <- adf[tr_s, , drop = FALSE]; tes <- adf[te_s, , drop = FALSE]
  err_te <- ie_all[te_s]; doll_te <- ed_all[te_s]
  # B1 fix (review 2026-08-11): evaluation strata mirror the mining strata,
  # so a rule admitted in stratum h flags only stratum-h cases in the walk
  # (the shipped builder's semantics; the "all" shortcut silently applied
  # stratum rules unrestricted)
  strata_tr <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(trs$cert_HH_size_FS_n) %in% h))
  strata_te <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(tes$cert_HH_size_FS_n) %in% h))
  ps_hit <- list()   # persize arm's FY2024 hit masks per budget (runs first)
  for (arm in names(ARMS)) {
    pool <- mine_state_arm(state, arm)
    n_adm <- nrow(pool)
    if (n_adm) {
      idx_tr <- flags_for_rules(pool, trs, strata_tr, label = "")
      idx_te <- flags_for_rules(pool, tes, strata_te, label = "")
    }
    for (b in BUDGETS) {
      bkey <- sprintf("%.2f", b)
      if (n_adm) {
        wm <- walk_freshshare(idx_tr, idx_te, nrow(trs), nrow(tes), b)
        fl <- wm$hit
        dep <- wm$deployed
        dep_p <- if (length(dep)) uses_p_feature(pool$rule[dep]) else logical(0)
        readout[[length(readout) + 1]] <- data.frame(
          state = state, arm = arm, budget = b, n_admitted = n_adm,
          n_te = nrow(tes), n_flagged = sum(fl),
          n_errors_caught = sum(fl & err_te),
          precision = round(sum(fl & err_te) / max(sum(fl), 1), 4),
          base_rate_te = round(mean(err_te), 4),
          dollar_recall = round(sum(doll_te[fl]) / max(sum(doll_te), 1), 4),
          n_rules_deployed = length(dep),
          n_rules_deployed_p = sum(dep_p),
          fill_cases = wm$fill_cases, fill_gap_core = wm$fill_gap_core,
          fill_gap_total = wm$fill_gap_total, cap_buf = wm$cap_buf)
        if (arm == "persize") ps_hit[[bkey]] <- fl
        if (arm == "benp") {
          # ---- flag-profile layer (reviewer's design, Eric's brief) ----
          # File 1: per deployed rule x `_p` condition
          if (any(dep_p)) for (j in which(dep_p)) {
            pc <- p_conditions_of(pool$rule[dep[j]])
            if (!is.null(pc)) {
              pc$state <- state; pc$budget <- b; pc$rule_rank <- j
              pc$n_new_at_rank <- wm$n_new_dep[j]
              p_inventory[[length(p_inventory) + 1]] <- pc
            }
          }
          # File 2: do the `_p`-using rules earn their slots
          fp <- rep(FALSE, nrow(tes)); fn_ <- rep(FALSE, nrow(tes))
          for (j in seq_along(dep)) {
            m <- idx_te[[dep[j]]]
            if (dep_p[j]) fp[m] <- TRUE else fn_[m] <- TRUE
          }
          fp <- fp & fl; fn_ <- fn_ & fl
          caught_doll <- sum(doll_te[fl & err_te])
          p_catch[[length(p_catch) + 1]] <- data.frame(
            state = state, budget = b,
            n_flagged_p_rules = sum(fp & !fn_),
            n_flagged_nonp_only = sum(fn_ & !fp),
            n_flagged_both = sum(fp & fn_),
            precision_p_rules = round(sum(fp & err_te) / max(sum(fp), 1), 4),
            precision_nonp_rules = round(sum(fn_ & err_te) / max(sum(fn_), 1), 4),
            dollar_share_p_rules = round(
              sum(doll_te[fp & err_te]) / max(caught_doll, 1), 4))
          # incremental catch vs the persize arm (same budget)
          ph <- ps_hit[[bkey]]
          if (!is.null(ph)) {
            incr_catch[[length(incr_catch) + 1]] <- data.frame(
              state = state, budget = b,
              n_err_benp_only = sum(fl & err_te & !ph),
              n_err_persize_only = sum(ph & err_te & !fl),
              n_err_both = sum(fl & ph & err_te))
          }
          # File 3: per-variable profile with direction-agnostic tails,
          # the pinned-zero mass, and the incremental-catch tail shares
          for (v in P7) {
            rules_v <- dep[grepl(paste0("\\b", v, "\\b"), pool$rule[dep])]
            vals <- tes[[v]]
            fv <- rep(FALSE, nrow(tes))
            for (i in rules_v) fv[idx_te[[i]]] <- TRUE
            fv <- fv & fl
            benp_only_err <- fl & err_te & (if (!is.null(ph)) !ph else TRUE)
            shared_err    <- if (!is.null(ph)) fl & ph & err_te else rep(FALSE, nrow(tes))
            sh <- function(mask, thr) if (sum(mask)) round(mean(vals[mask] > thr), 4) else NA_real_
            p_var_profile[[length(p_var_profile) + 1]] <- data.frame(
              state = state, budget = b, var = v,
              n_rules_using_var = length(rules_v),
              share_flagged_gt90 = sh(fv, 0.90),
              share_caseload_gt90 = round(mean(vals > 0.90), 4),
              share_errors_gt90 = sh(err_te, 0.90),
              share_flagged_gt99 = sh(fv, 0.99),
              share_caseload_gt99 = round(mean(vals > 0.99), 4),
              share_errors_gt99 = sh(err_te, 0.99),
              share_flagged_p0 = if (sum(fv)) round(mean(vals[fv] == 0), 4) else NA_real_,
              share_caseload_p0 = round(mean(vals == 0), 4),
              share_benponly_err_gt90 = sh(benp_only_err, 0.90),
              share_shared_err_gt90 = sh(shared_err, 0.90))
          }
        }
      } else {
        readout[[length(readout) + 1]] <- data.frame(
          state = state, arm = arm, budget = b, n_admitted = 0L,
          n_te = nrow(tes), n_flagged = 0L, n_errors_caught = 0L,
          precision = NA_real_, base_rate_te = round(mean(err_te), 4),
          dollar_recall = NA_real_, n_rules_deployed = 0L,
          n_rules_deployed_p = 0L, fill_cases = NA_integer_,
          fill_gap_core = NA_integer_, fill_gap_total = NA_integer_,
          cap_buf = NA_integer_)
      }
    }
    if (n_adm) rm(idx_tr, idx_te)
  }
  stamp("  %s done", state)
}
readout <- bind_rows(readout)
write.csv(readout, file.path(OUT_DIR, "pctl_runoff_readout.csv"),
          row.names = FALSE)
if (length(p_inventory))
  write.csv(bind_rows(p_inventory)[, c("state", "budget", "rule_rank", "var",
                                       "op", "threshold", "n_new_at_rank")],
            file.path(OUT_DIR, "pctl_runoff_p_condition_inventory.csv"),
            row.names = FALSE)
if (length(p_catch))
  write.csv(bind_rows(p_catch),
            file.path(OUT_DIR, "pctl_runoff_p_rule_catch.csv"),
            row.names = FALSE)
if (length(p_var_profile))
  write.csv(bind_rows(p_var_profile),
            file.path(OUT_DIR, "pctl_runoff_p_variable_profile.csv"),
            row.names = FALSE)
if (length(incr_catch))
  write.csv(bind_rows(incr_catch),
            file.path(OUT_DIR, "pctl_runoff_incremental_catch.csv"),
            row.names = FALSE)

## ---- exploratory summary (companions; NO winner rule) -----------------------
for (b in BUDGETS) {
  w <- readout %>% filter(budget == b) %>%
    select(state, arm, n_admitted, precision, dollar_recall) %>%
    tidyr::pivot_wider(names_from = arm,
                       values_from = c(n_admitted, precision, dollar_recall))
  paired <- w %>% filter(n_admitted_persize > 0, n_admitted_benp > 0)
  if (nrow(paired)) {
    d  <- paired$precision_benp - paired$precision_persize
    dd <- paired$dollar_recall_benp - paired$dollar_recall_persize
    stamp("EXPLORATORY %d%% budget (%d paired states): d precision (benp - persize) median %+.4f | mean %+.4f | harmed (< -0.05) %d | helped (> +0.05) %d",
          round(100 * b), nrow(paired), median(d), mean(d), sum(d < -0.05),
          sum(d > 0.05))
    stamp("  dollars: d median %+.4f | mean %+.4f | harmed %d | helped %d",
          median(dd), mean(dd), sum(dd < -0.05), sum(dd > 0.05))
    stamp("  absolute medians: precision persize %.4f / benp %.4f | dollars persize %.4f / benp %.4f",
          median(paired$precision_persize), median(paired$precision_benp),
          median(paired$dollar_recall_persize), median(paired$dollar_recall_benp))
  } else {
    stamp("EXPLORATORY %d%% budget: no paired states", round(100 * b))
  }
  for (a in names(ARMS)) {
    g <- readout %>% filter(budget == b, arm == a, !is.na(fill_gap_total))
    if (nrow(g)) {
      nz <- g$fill_gap_total[g$fill_gap_total > 0]
      stamp("  fill gaps [%s]: %d of %d cells > 0 | max %d (%.2f%% of its target) | median nonzero %s",
            a, length(nz), nrow(g), if (length(nz)) max(nz) else 0L,
            if (length(nz)) 100 * max(nz / (g$fill_cases + g$fill_gap_total)[g$fill_gap_total > 0]) else 0,
            if (length(nz)) sprintf("%.0f", median(nz)) else "-")
    }
  }
}
w5 <- readout %>% filter(budget == 0.05)
stamp("states admitting: persize %d | benp %d (non-admitting states named in the CSV)",
      sum(w5$n_admitted[w5$arm == "persize"] > 0),
      sum(w5$n_admitted[w5$arm == "benp"] > 0))
paired_all <- bind_rows(lapply(BUDGETS, function(b) {
  readout %>% filter(budget == b) %>%
    select(state, arm, n_admitted, precision, dollar_recall) %>%
    tidyr::pivot_wider(names_from = arm,
                       values_from = c(n_admitted, precision, dollar_recall)) %>%
    mutate(budget = b,
           d_prec = precision_benp - precision_persize,
           d_doll = dollar_recall_benp - dollar_recall_persize)
}))
write.csv(paired_all, file.path(OUT_DIR, "pctl_runoff_paired_table.csv"),
          row.names = FALSE)
stamp("outputs written to %s - EXPLORATORY: no winner is selected by this script", OUT_DIR)
