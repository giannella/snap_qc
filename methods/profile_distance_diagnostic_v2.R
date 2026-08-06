# Profile-distance diagnostic: which rule-dissimilarity measure predicts
# marginal quality at a review budget?
#
# BINDING SPEC: methods/profile_distance_diagnostic_plan_2026-08-06.md (approved).
# Where any comment here and the plan disagree, the plan wins. Descriptive
# diagnostic on existing artifacts; mines nothing, changes no delivered list.
#
# Six instruments, head to head, each through both analyses:
#   i1_fresh_overlap   deploy-time flag overlap (pairwise: 1 - |Fi&Fj|/min on the
#                      FY2022-23 caseload; Analysis-2 form: fresh share f on the
#                      state's FY2024 caseload, outcome-free)
#   i2_spectral        spectral embedding of the co-firing (Jaccard) matrix,
#                      eigengap dimension capped at 10, judged on DISTANCES
#   i3_nb_hellinger    naive-Bayes feature divergence (binned mining features of
#                      flagged training cases; mean per-feature Hellinger)
#   i4_profile_tv      TV distance on section 29 element-group share vectors,
#                      FY2022-23 only, support floor 20 train error cases
#                      (nature/timing/cause TVs computed as SECONDARY)
#   i5_signature       structural signature distance (variables + directions,
#                      thresholds discarded; written null expectation)
#   i6_consensus       rank-normalized average of instruments 1-4 (pre-stated)
#
# Eligibility gate: split-half stability certificate (Spearman >= 0.70, median
# over 5 splits) BEFORE any signal reading. Analysis 1: pairwise
# complementarity on the national FY2024 caseload, permutation null within
# stratum, support-controlled partial correlation quoted. Analysis 2
# (co-primary): min distance to same-stratum deployed predecessors, outcome
# pooled capacity-weighted marginal precision by within-(state,budget) tercile,
# top minus bottom, per budget, on all capacity AND f < 0.99 capacity,
# permutation null within (list, stratum), 200 draws: for the matrix
# instruments the null shuffles RULE IDENTITIES among the list-stratum's
# deployed rules and RE-DERIVES d_pred (senior-statistician review, required
# change 1 - a value-shuffle is anti-conservative because d_pred_min shrinks
# mechanically with rank depth); instrument 1 (per-instance f, no matrix)
# keeps the value shuffle. The stability certificate is an eligibility gate:
# an ineligible instrument's signal reading is NOT computed anywhere
# (required change 2).
#
# RECONCILIATION ANCHORS (stop on mismatch):
#   - recomputed FY2024 flag/catch accounting (refill walk) must equal
#     methods/marginal_precision_diagnostic/per_rule_marginal.csv (holdout basis)
#   - the python variance join (methods/profile_distance_variance_join.py) must
#     reproduce methods/rule_error_profiles/rule_profiles.csv train-era rows
#   - python flag counts must equal the R evaluator's flag counts per rule
#
# SMOKE MODE (SMOKE=1): full pipeline on 5 states with 20 permutation draws.
# Prints ONLY plumbing (row counts, assertions, certificate values, runtime);
# analysis outputs go to a scratch dir under the output folder that is DELETED
# at the end. Signal statistics are never printed in smoke mode.
#
# Outputs (full run) -> methods/profile_distance_diagnostic/ :
#   pair_distances.csv, analysis1_correlations.csv, analysis2_tercile_gaps.csv,
#   per_instance_dpred.csv, stability_certificate.csv (+ _by_split),
#   spectral_clusters.csv, coverage_report.txt, summary.txt
# Read-only everywhere else. No CHANGELOG, no version bump.

suppressMessages({ library(dplyr); library(Matrix) })
source("rule_mining_helpers.R")

t0 <- Sys.time()
SMOKE <- Sys.getenv("SMOKE", "0") == "1"

OUT_ROOT <- "methods/profile_distance_diagnostic"
OUT_DIR  <- if (SMOKE) file.path(OUT_ROOT, "SMOKE_SCRATCH") else OUT_ROOT
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

BENCH_DIR    <- "methods/anyerror_blended_holdout_2024"
ANCHOR_CSV   <- "methods/marginal_precision_diagnostic/per_rule_marginal.csv"
ALL_YEARS    <- c("2022", "2023", "2024")
BENCH_YEARS  <- c("2022", "2023")
HOLDOUT_YEAR <- "2024"
BUDGETS      <- c(5, 10)

N_PERM        <- if (SMOKE) 20L else 200L   # plan: 200 draws
N_SPLITS      <- 5L                          # plan: median over 5 splits
CERT_RHO_MIN  <- 0.70
SPECTRAL_CAP  <- 10L                         # plan: eigengap rule, cap 10
PROFILE_FLOOR <- 20L                         # plan: >= 20 FY2022-23 error cases
ATTEN_FLOOR   <- 50L                         # plan: attenuation check at >= 50
N_FEATURE_BINS<- 5L                          # quintile bins for instrument 3
SMOKE_STATES  <- c("Alabama", "California", "Colorado", "Georgia", "Michigan")
PYTHON_BIN    <- Sys.getenv("PYTHON_BIN", "python")

INSTRUMENTS <- c("i1_fresh_overlap", "i2_spectral", "i3_nb_hellinger",
                 "i4_profile_tv", "i5_signature", "i6_consensus")
SECONDARY   <- c("sec_profile_tv_nature", "sec_profile_tv_timing",
                 "sec_profile_tv_cause")
# pre-set tie-break order, cheapest first (plan: ties break to the cheaper
# instrument): fresh share falls out of the walk itself, signature needs rule
# text only, then flags+eigen, flags+features, variance join, everything.
COST_ORDER  <- c("i1_fresh_overlap", "i5_signature", "i2_spectral",
                 "i3_nb_hellinger", "i4_profile_tv", "i6_consensus")

stamp <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"),
                                   sprintf(...)))
say_full <- function(...) if (!SMOKE) cat(sprintf(...))  # signal-bearing prints

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

## ── 0. frame ──────────────────────────────────────────────────────────────────
if (!exists("reg_model_data")) reg_model_data <- readRDS("reg_model_data.rds")
pf  <- prep_features(reg_model_data %>% filter(fiscal_year %in% ALL_YEARS), features)
adf <- pf$data
st  <- as.character(adf$state)
fy  <- as.character(adf$fiscal_year)

rows_tr_nat <- which(fy %in% BENCH_YEARS)
rows_ho_nat <- which(fy == HOLDOUT_YEAR)
train_df <- adf[rows_tr_nat, , drop = FALSE]
ho_df    <- adf[rows_ho_nat, , drop = FALSE]
n_tr <- nrow(train_df); n_ho <- nrow(ho_df)
err_tr <- !is.na(train_df$over_threshold) & train_df$over_threshold != 0
err_ho <- !is.na(ho_df$over_threshold)    & ho_df$over_threshold != 0
strata_tr <- lapply(setNames(nm = HH_LEVELS), function(h)
  which(hh_group_of(train_df$cert_HH_size_FS_n) %in% h))
strata_ho_nat <- lapply(setNames(nm = HH_LEVELS), function(h)
  which(hh_group_of(ho_df$cert_HH_size_FS_n) %in% h))
stamp("frame: %d rows FY2022-24 | train %d (errors %d) | holdout %d (errors %d)",
      nrow(adf), n_tr, sum(err_tr), n_ho, sum(err_ho))

## ── 1. anchor: recompute FY2024 accounting, assert vs per_rule_marginal.csv ──
anchor <- read.csv(ANCHOR_CSV, stringsAsFactors = FALSE) %>%
  filter(basis == "holdout")
states_all <- sort(unique(anchor$state))
states <- if (SMOKE) intersect(states_all, SMOKE_STATES) else states_all
stamp("states: %d of %d%s", length(states), length(states_all),
      if (SMOKE) " (SMOKE subset)" else "")

flags_for_list <- function(lst, data, strata_idx) {
  ci  <- build_condition_index(lst$rule)
  cnd <- eval_conditions(ci$conditions, data)
  idx <- vector("list", nrow(lst))
  for (h in unique(lst$hh)) {
    rows <- which(lst$hh == h)
    idx[rows] <- rule_flag_idx(ci$rule_conds[rows], cnd, base_idx = strata_idx[[h]])
  }
  idx
}
# capacity-capped refill walk, mirrors methods/add_refill_metrics_v2.R exactly
refill_walk <- function(idx, err, n_rows, cap) {
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

instances <- list(); n_assert_anchor <- 0L
for (S in states) {
  s_rows <- which(st == S & fy == HOLDOUT_YEAR)
  sdf <- adf[s_rows, , drop = FALSE]
  s_err <- !is.na(sdf$over_threshold) & sdf$over_threshold != 0
  s_strat <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(sdf$cert_HH_size_FS_n) %in% h))
  for (b in BUDGETS) {
    f_bch <- file.path(BENCH_DIR, sprintf("bench_list_%s_budget%02d.csv",
                                          gsub(" ", "_", S), b))
    bl <- read.csv(f_bch, stringsAsFactors = FALSE)
    bl <- bl[order(bl$rank), , drop = FALSE]
    idx <- flags_for_list(bl, sdf, s_strat)
    rf  <- refill_walk(idx, s_err, nrow(sdf), floor(b / 100 * nrow(sdf)))
    dep <- which(rf$deployed)
    got <- data.frame(rank = bl$rank[dep], hh = bl$hh[dep], rule = bl$rule[dep],
                      role = bl$role[dep],
                      n_new = rf$n_new[dep], k_new = rf$k_new[dep],
                      own_n_flagged = as.integer(lengths(idx)[dep]),
                      stringsAsFactors = FALSE)
    exp <- anchor %>% filter(state == S, budget == b) %>% arrange(rank)
    ok <- nrow(got) == nrow(exp) &&
      identical(got$rank, as.integer(exp$rank)) &&
      identical(got$n_new, as.integer(exp$n_new)) &&
      identical(got$k_new, as.integer(exp$k_new)) &&
      identical(got$own_n_flagged, as.integer(exp$own_n_flagged)) &&
      identical(got$rule, exp$rule)
    if (!ok)
      stop(sprintf("ASSERTION FAILED [%s budget %d]: recomputed FY2024 refill accounting does not reproduce per_rule_marginal.csv (holdout basis). No downstream number can be trusted.",
                   S, b))
    n_assert_anchor <- n_assert_anchor + 1L
    got$state <- S; got$budget <- b
    got$f <- got$n_new / got$own_n_flagged
    instances[[sprintf("%s_%d", S, b)]] <- got
  }
}
inst <- bind_rows(instances)
stamp("anchor: %d/%d state-budget refill walks reproduce per_rule_marginal.csv exactly",
      n_assert_anchor, length(states) * length(BUDGETS))
if (n_assert_anchor != length(states) * length(BUDGETS))
  stop("ASSERTION FAILED: not every state-budget list was verified.")

## ── 2. rule universe + python variance join (instrument 4 inputs) ────────────
uni <- inst %>% distinct(hh, rule) %>% arrange(hh, rule)
uni$rule_id <- seq_len(nrow(uni))
inst <- inst %>% left_join(uni, by = c("hh", "rule"))
stamp("rule universe: %d distinct (hh, rule) across %d deployed instances",
      nrow(uni), nrow(inst))

f_uni   <- file.path(OUT_DIR, "rule_universe.csv")
f_cases <- file.path(OUT_DIR, "profile_cases_train.csv")
f_prul  <- file.path(OUT_DIR, "profile_rules_train.csv")
write.csv(uni[, c("rule_id", "hh", "rule")], f_uni, row.names = FALSE)
stamp("calling python variance-join helper (reconciles vs rule_profiles.csv) ...")
rc <- system2(PYTHON_BIN, c("methods/profile_distance_variance_join.py",
                            "--rules", f_uni, "--out-cases", f_cases,
                            "--out-rules", f_prul))
if (rc != 0) stop("python helper failed (exit ", rc, "); see its stderr above.")
prul  <- read.csv(f_prul, stringsAsFactors = FALSE)
cases <- read.csv(f_cases, stringsAsFactors = FALSE)
stopifnot(nrow(prul) == nrow(uni))
prul <- prul[order(prul$rule_id), , drop = FALSE]
stopifnot(identical(sort(prul$rule_id), uni$rule_id))   # review hardening

# map profile cases onto R's train frame rows by (state, yrmonth, hhldno)
keyize <- function(s, ym, hh_no)
  paste(as.character(s), as.character(as.numeric(as.character(ym))),
        as.character(as.numeric(as.character(hh_no))), sep = "|")
key_tr <- keyize(as.character(train_df$state), train_df$yrmonth, train_df$hhldno)
if (any(duplicated(key_tr)))
  stop("ASSERTION FAILED: (state, yrmonth, hhldno) is not unique on the train frame; the case-key join is unsafe.")
cases$train_row <- match(keyize(cases$state_name, cases$yrmonth, cases$hhldno), key_tr)
if (any(is.na(cases$train_row)))
  stop(sprintf("ASSERTION FAILED: %d helper case rows did not match a train frame row.",
               sum(is.na(cases$train_row))))
stamp("profile cases: %d (rule, case) rows, all matched to train frame rows", nrow(cases))

## ── 3. national flags: train (FY2022-23) and holdout (FY2024) ────────────────
idx_tr <- flags_for_rules(uni, train_df, strata_tr, label = "train flags")
idx_ho <- flags_for_rules(uni, ho_df, strata_ho_nat, label = "FY2024 flags")
n_flag_tr <- lengths(idx_tr)
n_errcase_tr <- vapply(idx_tr, function(ix) sum(err_tr[ix]), integer(1))
catch <- lapply(idx_ho, function(ix) ix[err_ho[ix]])
n_catch <- lengths(catch)

# cross-evaluator anchors: python (pandas eval) must equal R (condition index)
if (!identical(as.integer(n_flag_tr), as.integer(prul$n_flagged_train)))
  stop("ASSERTION FAILED: python flag counts differ from the R evaluator's on the train caseload.")
if (!identical(as.integer(n_errcase_tr), as.integer(prul$n_error_cases)))
  stop("ASSERTION FAILED: python train error-case counts differ from the R evaluator's.")
stamp("cross-evaluator: python == R on n_flagged_train and n_error_cases for all %d rules",
      nrow(uni))

## ── 4. distance machinery ─────────────────────────────────────────────────────
loc <- lapply(setNames(nm = HH_LEVELS), function(h) which(uni$hh == h))

sparse_flags <- function(rows_keep = NULL) {
  # per stratum: rules x train-rows sparse pattern matrix (optionally masked)
  lapply(setNames(nm = HH_LEVELS), function(h) {
    r <- loc[[h]]
    ix <- idx_tr[r]
    if (!is.null(rows_keep)) ix <- lapply(ix, function(v) v[rows_keep[v]])
    sparseMatrix(i = rep.int(seq_along(r), lengths(ix)),
                 j = unlist(ix), x = 1, dims = c(length(r), n_tr))
  })
}

overlap_and_jaccard <- function(M) {
  lapply(M, function(m) {
    inter <- as.matrix(tcrossprod(m))
    n <- diag(inter)
    mn <- outer(n, n, pmin)
    d_ov <- 1 - inter / mn
    d_ov[mn == 0] <- NA_real_
    uni_ <- outer(n, n, "+") - inter
    jac <- ifelse(uni_ > 0, inter / uni_, 0)
    diag(d_ov) <- 0; diag(jac) <- 1
    list(overlap = d_ov, jaccard = jac, n = n)
  })
}

spec_embed <- function(jac, d_fixed = NULL) {
  # symmetric normalized Laplacian of the Jaccard affinity (diag zeroed);
  # eigengap rule for the dimension, capped at SPECTRAL_CAP (pre-set, plan).
  W <- jac; diag(W) <- 0
  deg <- rowSums(W)
  iso <- deg <= 1e-12
  keep <- which(!iso)
  m <- length(keep)
  out <- list(D = matrix(NA_real_, nrow(W), ncol(W)), d = NA_integer_,
              n_isolated = sum(iso), evals = numeric(0),
              emb = NULL, keep = keep)
  if (m < 3) return(out)
  Wk <- W[keep, keep, drop = FALSE]
  s <- 1 / sqrt(rowSums(Wk))
  L <- diag(m) - (s * Wk) * rep(s, each = m)   # I - D^-1/2 W D^-1/2
  ev <- eigen((L + t(L)) / 2, symmetric = TRUE)
  lam <- rev(ev$values)                        # ascending
  U   <- ev$vectors[, rev(seq_len(m)), drop = FALSE]
  kmax <- min(SPECTRAL_CAP, m - 1L)
  d <- if (is.null(d_fixed)) which.max(diff(lam)[seq_len(kmax)]) else min(d_fixed, kmax)
  if (is.na(d) || d < 1) return(out)
  E <- U[, seq_len(d), drop = FALSE]
  Dk <- as.matrix(dist(E))
  out$D[keep, keep] <- Dk
  out$d <- d; out$evals <- lam[seq_len(min(m, SPECTRAL_CAP + 1L))]; out$emb <- E
  out
}

# instrument 3: binned mining features (quintiles on the FY2022-23 national
# caseload; NA its own bin; Laplace +0.5 as in state_nb_similarity_v2.R)
bin_train <- local({
  codes <- matrix(0L, n_tr, length(pf$features))
  nbins <- integer(length(pf$features))
  for (j in seq_along(pf$features)) {
    x <- as.numeric(train_df[[pf$features[j]]])
    br <- unique(quantile(x, probs = seq_len(N_FEATURE_BINS - 1) / N_FEATURE_BINS,
                          na.rm = TRUE, names = FALSE))
    cd <- findInterval(x, br) + 1L
    K <- length(br) + 1L
    cd[is.na(x)] <- K + 1L
    codes[, j] <- cd; nbins[j] <- K + 1L
  }
  list(codes = codes, nbins = nbins)
})

# one-hot bin indicators, built once; a row-masked flag matrix m zeroes the
# masked rows' contributions automatically, so these are reused for halves
OH_LIST <- lapply(seq_along(pf$features), function(j)
  sparseMatrix(i = seq_len(n_tr), j = bin_train$codes[, j], x = 1,
               dims = c(n_tr, bin_train$nbins[j])))

nb_hellinger <- function(M) {
  lapply(M, function(m) {
    nR <- nrow(m); nfl <- rowSums(m)
    H <- matrix(0, nR, nR)
    for (j in seq_along(pf$features)) {
      K1 <- bin_train$nbins[j]
      CNT <- as.matrix(m %*% OH_LIST[[j]])
      P <- (CNT + 0.5) / (rowSums(CNT) + 0.5 * K1)
      BC <- tcrossprod(sqrt(P))
      H <- H + sqrt(pmax(0, 1 - pmin(BC, 1)))
    }
    H <- H / length(pf$features)
    H[nfl == 0, ] <- NA_real_; H[, nfl == 0] <- NA_real_
    diag(H) <- 0
    H
  })
}

# instrument 4 (+ secondary): TV on share vectors aggregated from case rows
EG_COLS <- grep("^eg_", names(cases), value = TRUE)
NG_COLS <- grep("^ng_", names(cases), value = TRUE)
TM_COLS <- c("tm_before_action", "tm_at_action", "tm_after_action")
CZ_COLS <- c("cz_agency", "cz_client", "cz_third_party", "cz_no_fault", "cz_other")

tv_mats <- function(case_rows, err_counts, floor_n, cols = EG_COLS) {
  # case_rows: subset of `cases`; err_counts: per-rule error-case counts used
  # for the support floor; returns per-stratum TV matrices (NA below floor)
  cnts <- matrix(0, nrow(uni), length(cols))
  if (nrow(case_rows) > 0) {
    agg <- rowsum(as.matrix(case_rows[, cols, drop = FALSE]),
                  group = case_rows$rule_id)
    cnts[as.integer(rownames(agg)), ] <- agg
  }
  tot <- rowSums(cnts)
  sh <- cnts / ifelse(tot > 0, tot, 1)
  bad <- err_counts < floor_n | tot == 0
  lapply(setNames(nm = HH_LEVELS), function(h) {
    r <- loc[[h]]
    D <- as.matrix(dist(sh[r, , drop = FALSE], method = "manhattan")) / 2
    D[bad[r], ] <- NA_real_; D[, bad[r]] <- NA_real_
    diag(D) <- 0
    D
  })
}

# instrument 5: signature Jaccard distance (deterministic in the rule text)
sig_tokens <- lapply(uni$rule, function(r) {
  s <- .rule_struct(r)
  unique(strsplit(s$sig, ";", fixed = TRUE)[[1]])
})
sig_mats <- lapply(setNames(nm = HH_LEVELS), function(h) {
  r <- loc[[h]]; nh <- length(r)
  D <- matrix(0, nh, nh)
  for (a in seq_len(nh - 1)) for (b in (a + 1):nh) {
    ta <- sig_tokens[[r[a]]]; tb <- sig_tokens[[r[b]]]
    D[a, b] <- D[b, a] <- 1 - length(intersect(ta, tb)) / length(union(ta, tb))
  }
  D
})

pair_idx_all <- lapply(setNames(nm = HH_LEVELS), function(h) {
  nh <- length(loc[[h]])
  if (nh < 2) return(matrix(integer(0), 0, 2))
  ut <- which(upper.tri(matrix(0, nh, nh)), arr.ind = TRUE)
  ut
})
vec_of <- function(mats) unlist(lapply(HH_LEVELS, function(h) {
  p <- pair_idx_all[[h]]
  if (nrow(p) == 0) numeric(0) else mats[[h]][p]
}), use.names = FALSE)

consensus_of <- function(m1, m2, m3, m4) {
  # rank-normalized average of instruments 1-4 over all within-stratum pairs
  # (pooled across strata onto one common scale), NA-tolerant per pair
  vs <- lapply(list(m1, m2, m3, m4), vec_of)
  rn <- lapply(vs, function(v) rank(v, na.last = "keep") / sum(!is.na(v)))
  cm <- rowMeans(do.call(cbind, rn), na.rm = TRUE)
  cm[!is.finite(cm)] <- NA_real_
  out <- list(); k <- 0L
  for (h in HH_LEVELS) {
    p <- pair_idx_all[[h]]; nh <- length(loc[[h]])
    D <- matrix(NA_real_, nh, nh); diag(D) <- 0
    if (nrow(p) > 0) {
      v <- cm[k + seq_len(nrow(p))]; k <- k + nrow(p)
      D[p] <- v; D[p[, c(2, 1), drop = FALSE]] <- v
    }
    out[[h]] <- D
  }
  out
}

compute_instruments <- function(rows_keep = NULL, d_fixed = NULL,
                                prof_floor = PROFILE_FLOOR) {
  M <- sparse_flags(rows_keep)
  oj <- overlap_and_jaccard(M)
  ov  <- lapply(oj, `[[`, "overlap")
  spc <- lapply(setNames(nm = HH_LEVELS), function(h)
    spec_embed(oj[[h]]$jaccard,
               d_fixed = if (is.null(d_fixed)) NULL else d_fixed[[h]]))
  nb <- nb_hellinger(M)
  if (is.null(rows_keep)) {
    cs <- cases; errc <- n_errcase_tr
  } else {
    cs <- cases[rows_keep[cases$train_row], , drop = FALSE]
    errc <- vapply(idx_tr, function(ix) sum(err_tr[ix] & rows_keep[ix]), integer(1))
  }
  pv <- tv_mats(cs, errc, prof_floor)
  cons <- consensus_of(ov, lapply(spc, `[[`, "D"), nb, pv)
  list(overlap = ov, spectral = lapply(spc, `[[`, "D"), nb = nb, profile = pv,
       consensus = cons, spec_detail = spc)
}

stamp("computing full-sample distance matrices (6 instruments + 3 secondary) ...")
FULL <- compute_instruments()
SPEC_DIMS <- lapply(FULL$spec_detail, `[[`, "d")
MATS <- list(i1_fresh_overlap = FULL$overlap,
             i2_spectral      = FULL$spectral,
             i3_nb_hellinger  = FULL$nb,
             i4_profile_tv    = FULL$profile,
             i5_signature     = sig_mats,
             i6_consensus     = FULL$consensus,
             sec_profile_tv_nature = tv_mats(cases, n_errcase_tr, PROFILE_FLOOR, NG_COLS),
             sec_profile_tv_timing = tv_mats(cases, n_errcase_tr, PROFILE_FLOOR, TM_COLS),
             sec_profile_tv_cause  = tv_mats(cases, n_errcase_tr, PROFILE_FLOOR, CZ_COLS))
stamp("spectral dims (eigengap, cap %d): %s | isolated nodes: %s", SPECTRAL_CAP,
      paste(sprintf("%s=%s", HH_LEVELS, unlist(SPEC_DIMS)), collapse = " "),
      paste(vapply(FULL$spec_detail, function(s) s$n_isolated, numeric(1)),
            collapse = "/"))

# descriptive rule families from the full-sample embedding (plan: reported
# regardless of the signal verdict)
clus <- bind_rows(lapply(HH_LEVELS, function(h) {
  s <- FULL$spec_detail[[h]]; r <- loc[[h]]
  cl <- rep(NA_integer_, length(r))
  if (!is.null(s$emb) && s$d >= 1 && length(s$keep) > s$d) {
    set.seed(20260806)
    k <- max(2L, s$d)
    cl[s$keep] <- kmeans(s$emb, centers = min(k, length(s$keep) - 1L),
                         nstart = 25, iter.max = 100)$cluster
  }
  data.frame(hh = h, rule_id = uni$rule_id[r], rule = uni$rule[r],
             spectral_dim = s$d, family = cl, isolated = is.na(cl),
             stringsAsFactors = FALSE)
}))
write.csv(clus, file.path(OUT_DIR, "spectral_clusters.csv"), row.names = FALSE)

## ── 5. split-half stability certificate (eligibility gate) ───────────────────
stamp("stability certificate: %d splits x (caseload halves for i1-i3, i6; per-rule error-case halves for i4) ...", N_SPLITS)
spearman_safe <- function(a, b) {
  ok <- is.finite(a) & is.finite(b)
  if (sum(ok) < 10) return(list(rho = NA_real_, n = sum(ok)))
  list(rho = suppressWarnings(cor(a[ok], b[ok], method = "spearman")), n = sum(ok))
}
set.seed(20260806)
cert_rows <- list()
for (s in seq_len(N_SPLITS)) {
  halfA <- rep(FALSE, n_tr); halfA[sample.int(n_tr, n_tr %/% 2)] <- TRUE
  A <- compute_instruments(halfA,  d_fixed = SPEC_DIMS,
                           prof_floor = ceiling(PROFILE_FLOOR / 2))
  B <- compute_instruments(!halfA, d_fixed = SPEC_DIMS,
                           prof_floor = ceiling(PROFILE_FLOOR / 2))
  for (nm in c("overlap", "spectral", "nb", "consensus")) {
    instr <- c(overlap = "i1_fresh_overlap", spectral = "i2_spectral",
               nb = "i3_nb_hellinger", consensus = "i6_consensus")[[nm]]
    r <- spearman_safe(vec_of(A[[nm]]), vec_of(B[[nm]]))
    cert_rows[[paste(instr, s)]] <- data.frame(
      instrument = instr, split = s, n_pairs = r$n, rho = r$rho)
  }
  # instrument 4: two random halves of each rule's error cases (plan)
  gA <- rep(FALSE, nrow(cases))
  for (g in split(seq_len(nrow(cases)), cases$rule_id)) {
    take <- sample(g, length(g) %/% 2)
    gA[take] <- TRUE
  }
  ecA <- tabulate(cases$rule_id[gA],  nbins = nrow(uni))
  ecB <- tabulate(cases$rule_id[!gA], nbins = nrow(uni))
  pa <- tv_mats(cases[gA, , drop = FALSE],  ecA, ceiling(PROFILE_FLOOR / 2))
  pb <- tv_mats(cases[!gA, , drop = FALSE], ecB, ceiling(PROFILE_FLOOR / 2))
  r <- spearman_safe(vec_of(pa), vec_of(pb))
  cert_rows[[paste("i4", s)]] <- data.frame(
    instrument = "i4_profile_tv", split = s, n_pairs = r$n, rho = r$rho)
}
cert <- bind_rows(cert_rows)
cert <- bind_rows(cert, data.frame(instrument = "i5_signature",
                                   split = seq_len(N_SPLITS),
                                   n_pairs = length(vec_of(sig_mats)),
                                   rho = 1))  # deterministic in the rule text
cert_sum <- cert %>% group_by(instrument) %>%
  summarise(median_rho = median(rho), min_rho = min(rho), max_rho = max(rho),
            .groups = "drop") %>%
  mutate(eligible = median_rho >= CERT_RHO_MIN)
write.csv(cert, file.path(OUT_DIR, "stability_certificate_by_split.csv"),
          row.names = FALSE)
write.csv(cert_sum, file.path(OUT_DIR, "stability_certificate.csv"),
          row.names = FALSE)
stamp("certificate (plumbing; eligibility gate at median rho >= %.2f):", CERT_RHO_MIN)
for (i in seq_len(nrow(cert_sum)))
  stamp("  %-18s median rho %.3f (range %.3f-%.3f) -> %s",
        cert_sum$instrument[i], cert_sum$median_rho[i], cert_sum$min_rho[i],
        cert_sum$max_rho[i], ifelse(cert_sum$eligible[i], "ELIGIBLE", "INELIGIBLE"))

# ELIGIBILITY GATE (plan; senior-statistician review, required change 2): an
# ineligible instrument's signal reading is NOT COMPUTED - it is excluded from
# the A1/A2 loops, from every outcome-bearing output table, and its summary
# line carries only the certificate value. Distance matrices were still built
# for all instruments above (i6 needs i1-i4 as components; construction is not
# a signal reading). The secondary TVs are variants of instrument 4 and share
# its certificate.
ELIG <- cert_sum$instrument[cert_sum$eligible]
A_INSTR <- c(intersect(INSTRUMENTS, ELIG),
             if ("i4_profile_tv" %in% ELIG) SECONDARY else character(0))
stamp("signal computed for %d instruments (%s)%s",
      length(A_INSTR), paste(A_INSTR, collapse = ", "),
      if (length(setdiff(INSTRUMENTS, ELIG)) > 0)
        sprintf(" | INELIGIBLE, signal not computed: %s",
                paste(setdiff(INSTRUMENTS, ELIG), collapse = ", ")) else "")

## ── 6. Analysis 1: pairwise complementarity on the national FY2024 caseload ──
lpos <- integer(nrow(uni))                       # position within stratum
for (h in HH_LEVELS) lpos[loc[[h]]] <- seq_along(loc[[h]])

pairs <- inst %>%
  group_by(state, budget, hh) %>%
  filter(n() >= 2) %>%
  do({
    ids <- sort(.$rule_id)
    cmb <- t(combn(ids, 2))
    data.frame(id_i = cmb[, 1], id_j = cmb[, 2])
  }) %>% ungroup() %>%
  count(hh, id_i, id_j, name = "n_lists_codeployed")

# outcome: 1 - overlap coefficient of the national FY2024 error-catch sets
catch_inter <- lapply(setNames(nm = HH_LEVELS), function(h) {
  r <- loc[[h]]
  cix <- catch[r]
  m <- sparseMatrix(i = rep.int(seq_along(r), lengths(cix)), j = unlist(cix),
                    x = 1, dims = c(length(r), n_ho))
  as.matrix(tcrossprod(m))
})
pairs$li <- lpos[pairs$id_i]; pairs$lj <- lpos[pairs$id_j]
pair_rows_h <- split(seq_len(nrow(pairs)), factor(pairs$hh, levels = HH_LEVELS))
lookup_pairs <- function(mats, perm = NULL) {
  # vectorized per-stratum matrix lookup, optionally under a rule-label
  # permutation within stratum (the plan's Analysis 1 null)
  v <- rep(NA_real_, nrow(pairs))
  for (h in HH_LEVELS) {
    ii <- pair_rows_h[[h]]
    if (length(ii) == 0) next
    a <- pairs$li[ii]; b <- pairs$lj[ii]
    if (!is.null(perm)) { a <- perm[[h]][a]; b <- perm[[h]][b] }
    v[ii] <- mats[[h]][cbind(a, b)]
  }
  v
}
pairs$n_catch_i <- n_catch[pairs$id_i]; pairs$n_catch_j <- n_catch[pairs$id_j]
pairs$catch_inter <- lookup_pairs(catch_inter)
pairs$complementarity <- ifelse(
  pmin(pairs$n_catch_i, pairs$n_catch_j) > 0,
  1 - pairs$catch_inter / pmin(pairs$n_catch_i, pairs$n_catch_j), NA_real_)
pairs$n_flag_tr_i <- n_flag_tr[pairs$id_i]; pairs$n_flag_tr_j <- n_flag_tr[pairs$id_j]
pairs$n_errcase_tr_i <- n_errcase_tr[pairs$id_i]
pairs$n_errcase_tr_j <- n_errcase_tr[pairs$id_j]
for (instr in A_INSTR)     # i1's pairwise form is overlap; eligible-only: an
  # ineligible instrument's distances never enter an outcome-bearing table
  pairs[[paste0("d_", instr)]] <- lookup_pairs(MATS[[instr]])
stamp("Analysis 1 pair universe: %d distinct same-stratum co-deployed pairs; outcome defined for %d (both rules catch >= 1 FY2024 error)",
      nrow(pairs), sum(!is.na(pairs$complementarity)))

pcor_ranks <- function(d, y, C) {   # C is guaranteed finite (n_flag_tr >= 30)
  ok <- is.finite(d) & is.finite(y)
  if (sum(ok) < 10) return(c(raw = NA_real_, partial = NA_real_, n = sum(ok)))
  rd <- rank(d[ok]); ry <- rank(y[ok])
  RC <- cbind(1, apply(C[ok, , drop = FALSE], 2, rank))
  res_d <- .lm.fit(RC, rd)$residuals
  res_y <- .lm.fit(RC, ry)$residuals
  c(raw = suppressWarnings(cor(rd, ry, method = "spearman")),
    partial = suppressWarnings(cor(res_d, res_y)), n = sum(ok))
}
CTRL <- cbind(log(pmin(pairs$n_flag_tr_i, pairs$n_flag_tr_j)),
              log(pmax(pairs$n_flag_tr_i, pairs$n_flag_tr_j)))
stopifnot(all(is.finite(CTRL)))

set.seed(20260807)
perm_of_stratum <- replicate(N_PERM, lapply(setNames(nm = HH_LEVELS), function(h)
  sample(length(loc[[h]]))), simplify = FALSE)

a1_rows <- list()
scopes1 <- list(all_pairs = rep(TRUE, nrow(pairs)),
                both_ge50_errcases = pairs$n_errcase_tr_i >= ATTEN_FLOOR &
                                     pairs$n_errcase_tr_j >= ATTEN_FLOOR)
for (instr in A_INSTR) {   # eligibility gate: signal not computed otherwise
  dv <- pairs[[paste0("d_", instr)]]
  obs <- lapply(scopes1, function(m)
    pcor_ranks(dv[m], pairs$complementarity[m], CTRL[m, , drop = FALSE]))
  null_raw <- null_par <- matrix(NA_real_, N_PERM, length(scopes1),
                                 dimnames = list(NULL, names(scopes1)))
  for (p in seq_len(N_PERM)) {
    dperm <- lookup_pairs(MATS[[instr]], perm_of_stratum[[p]])
    for (sc in names(scopes1)) {
      m <- scopes1[[sc]]
      r <- pcor_ranks(dperm[m], pairs$complementarity[m], CTRL[m, , drop = FALSE])
      null_raw[p, sc] <- r["raw"]; null_par[p, sc] <- r["partial"]
    }
  }
  for (sc in names(scopes1)) {
    o <- obs[[sc]]
    a1_rows[[paste(instr, sc)]] <- data.frame(
      instrument = instr, scope = sc, n_pairs = unname(o["n"]),
      spearman_raw = unname(o["raw"]), spearman_partial = unname(o["partial"]),
      perm_raw_mean = mean(null_raw[, sc], na.rm = TRUE),
      perm_raw_p95 = quantile(null_raw[, sc], 0.95, na.rm = TRUE, names = FALSE),
      perm_partial_mean = mean(null_par[, sc], na.rm = TRUE),
      perm_partial_p95 = quantile(null_par[, sc], 0.95, na.rm = TRUE, names = FALSE),
      share_null_ge_obs_partial = mean(null_par[, sc] >= o["partial"], na.rm = TRUE))
  }
}
a1 <- bind_rows(a1_rows)
write.csv(a1, file.path(OUT_DIR, "analysis1_correlations.csv"), row.names = FALSE)
write.csv(pairs %>% select(-li, -lj) %>%
            left_join(uni %>% select(rule_id, rule_i = rule), by = c(id_i = "rule_id")) %>%
            left_join(uni %>% select(rule_id, rule_j = rule), by = c(id_j = "rule_id")),
          file.path(OUT_DIR, "pair_distances.csv"), row.names = FALSE)
stamp("Analysis 1 written (%d instrument x scope rows); statistics not printed%s",
      nrow(a1), if (SMOKE) " (SMOKE: never printed)" else " (see CSVs)")

## ── 7. Analysis 2: distance to predecessors (co-primary) ─────────────────────
inst <- inst %>% arrange(state, budget, hh, rank) %>%
  group_by(state, budget, hh) %>% mutate(pred_n = row_number() - 1L) %>% ungroup()
has_pred <- inst$pred_n >= 1L
grp <- interaction(inst$state, inst$budget, inst$hh, drop = TRUE)
grp_list <- split(seq_len(nrow(inst)), grp)
grp_hh   <- vapply(grp_list, function(g) inst$hh[g[1]], character(1))
obs_pos  <- lapply(grp_list, function(g) lpos[inst$rule_id[g]])
stamp("Analysis 2 population: %d of %d deployed instances have >= 1 same-stratum deployed predecessor",
      sum(has_pred), nrow(inst))

# d_pred derivation from an assignment of rule identities to list slots.
# Used identically for the observed data (obs_pos) and for the permutation
# null (identity shuffle within (list, stratum), then RE-DERIVE - required
# change 1 of the senior-statistician review: shuffling the derived d_pred
# values is anti-conservative because d_pred_min mechanically shrinks with
# rank depth while marginal precision is rank-linked).
derive_dpred <- function(instr, pos_list) {
  dmin <- dswm <- rep(NA_real_, nrow(inst))
  for (gi in seq_along(grp_list)) {
    g <- grp_list[[gi]]; ng <- length(g)
    if (ng < 2) next
    h <- grp_hh[gi]; p <- pos_list[[gi]]
    M <- MATS[[instr]][[h]][p, p, drop = FALSE]
    wv <- n_flag_tr[loc[[h]][p]]        # weights travel with the identities
    for (a in 2:ng) {
      dv <- M[a, seq_len(a - 1L)]
      ok <- is.finite(dv)
      if (any(ok)) {
        dmin[g[a]] <- min(dv[ok])
        wsub <- wv[seq_len(a - 1L)][ok]
        dswm[g[a]] <- sum(dv[ok] * wsub) / sum(wsub)
      }
    }
  }
  list(min = dmin, swm = dswm)
}

MATRIX_A2 <- setdiff(A_INSTR, "i1_fresh_overlap")
dpred_obs <- lapply(setNames(nm = MATRIX_A2), derive_dpred, pos_list = obs_pos)
d_i1 <- ifelse(has_pred, inst$f, NA_real_)   # instrument 1's A2 form (plan)

per_inst <- inst %>% select(state, budget, hh, rank, rule_id, rule,
                            n_new, k_new, own_n_flagged, f, pred_n)
if ("i1_fresh_overlap" %in% A_INSTR)
  per_inst$dpred_i1_fresh_overlap <- d_i1
for (instr in MATRIX_A2) {
  per_inst[[paste0("dpred_min_", instr)]] <- dpred_obs[[instr]]$min
  per_inst[[paste0("dpred_swm_", instr)]] <- dpred_obs[[instr]]$swm
}
write.csv(per_inst, file.path(OUT_DIR, "per_instance_dpred.csv"), row.names = FALSE)

gap_of <- function(d, k, n, terc_grp_list) {
  terc <- integer(length(d))
  for (g in terc_grp_list) {
    # ties.method = "first" kept per the review: with the identity-shuffle
    # null, observed and null draws break ties by the same fixed slot order,
    # so "first" is harmless.
    r <- rank(d[g], ties.method = "first")
    terc[g] <- ceiling(3 * r / length(g))
  }
  p <- vapply(1:3, function(t) {
    kk <- sum(k[terc == t]); nn <- sum(n[terc == t])
    if (nn > 0) kk / nn else NA_real_
  }, numeric(1))
  c(prec_t1 = p[1], prec_t2 = p[2], prec_t3 = p[3], gap = p[3] - p[1])
}

# pooled capacity-weighted tercile gap on a population; instances whose d is
# NA (below the i4 floor, isolated spectral nodes - possibly differing draw to
# draw under the identity shuffle) are dropped from that computation
gap_pop <- function(d, keep) {
  ki <- which(keep & is.finite(d))
  if (length(ki) < 30) return(NULL)
  terc_grps <- split(seq_along(ki), inst$state[ki])   # within (state, budget)
  c(gap_of(d[ki], inst$k_new[ki], inst$n_new[ki], terc_grps),
    n_instances = length(ki), capacity = sum(inst$n_new[ki]))
}
base_keep <- list()
for (b in BUDGETS) for (sc in c("all_capacity", "f_lt_099")) {
  keep <- has_pred & inst$budget == b & inst$n_new > 0
  if (sc == "f_lt_099") keep <- keep & inst$f < 0.99
  base_keep[[paste(b, sc)]] <- keep
}
a2_row <- function(instr, form, b, sc, obs, null) {
  if (is.null(obs)) data.frame(
    instrument = instr, form = form, budget = b, scope = sc,
    n_instances = sum(base_keep[[paste(b, sc)]]), capacity = NA,
    prec_t1 = NA, prec_t2 = NA, prec_t3 = NA, gap = NA,
    perm_mean = NA, perm_sd = NA, perm_p95 = NA, share_null_ge_obs = NA)
  else data.frame(
    instrument = instr, form = form, budget = b, scope = sc,
    n_instances = unname(obs["n_instances"]), capacity = unname(obs["capacity"]),
    prec_t1 = unname(obs["prec_t1"]), prec_t2 = unname(obs["prec_t2"]),
    prec_t3 = unname(obs["prec_t3"]), gap = unname(obs["gap"]),
    perm_mean = mean(null, na.rm = TRUE), perm_sd = sd(null, na.rm = TRUE),
    perm_p95 = quantile(null, 0.95, na.rm = TRUE, names = FALSE),
    share_null_ge_obs = mean(null >= obs["gap"], na.rm = TRUE))
}

set.seed(20260808)
# one identity shuffle per draw per (list, stratum) group, shared across
# instruments and forms (as the A1 null shares its per-draw permutations)
perm_pos <- replicate(N_PERM, lapply(grp_list, function(g) {
  p <- lpos[inst$rule_id[g]]
  if (length(p) > 1) p[sample.int(length(p))] else p
}), simplify = FALSE)

a2_rows <- list()
# instrument 1: per-instance f, no matrix; the plan's shuffle IS the value
# shuffle among the rules deployed on the (list, stratum) (review, change 1)
if ("i1_fresh_overlap" %in% A_INSTR) {
  perm_grps_i1 <- split(seq_len(nrow(inst)),
                        interaction(inst$state, inst$budget, inst$hh, drop = TRUE))
  for (b in BUDGETS) for (sc in c("all_capacity", "f_lt_099")) {
    keep <- base_keep[[paste(b, sc)]]
    obs <- gap_pop(d_i1, keep)
    null <- rep(NA_real_, N_PERM)
    if (!is.null(obs)) for (p in seq_len(N_PERM)) {
      dp <- d_i1
      for (g in perm_grps_i1) {
        gg <- g[is.finite(d_i1[g])]
        if (length(gg) > 1) dp[gg] <- d_i1[gg][sample.int(length(gg))]
      }
      r <- gap_pop(dp, keep)
      null[p] <- if (is.null(r)) NA_real_ else r["gap"]
    }
    for (form in c("min", "swm"))   # i1 has a single A2 form; rows kept
      a2_rows[[paste("i1", form, b, sc)]] <-
        a2_row("i1_fresh_overlap", form, b, sc, obs, null)
  }
}
# instruments 2-6 (+ secondary, gated on i4): identity shuffle, re-derive
for (instr in MATRIX_A2) {
  null_g <- array(NA_real_, dim = c(N_PERM, 2, length(BUDGETS), 2),
                  dimnames = list(NULL, c("min", "swm"), as.character(BUDGETS),
                                  c("all_capacity", "f_lt_099")))
  for (p in seq_len(N_PERM)) {
    der <- derive_dpred(instr, perm_pos[[p]])
    for (form in c("min", "swm")) for (b in BUDGETS)
      for (sc in c("all_capacity", "f_lt_099")) {
        r <- gap_pop(der[[form]], base_keep[[paste(b, sc)]])
        null_g[p, form, as.character(b), sc] <- if (is.null(r)) NA_real_ else r["gap"]
      }
  }
  for (form in c("min", "swm")) for (b in BUDGETS)
    for (sc in c("all_capacity", "f_lt_099")) {
      obs <- gap_pop(dpred_obs[[instr]][[form]], base_keep[[paste(b, sc)]])
      a2_rows[[paste(instr, form, b, sc)]] <-
        a2_row(instr, form, b, sc, obs, null_g[, form, as.character(b), sc])
    }
}
a2 <- bind_rows(a2_rows); rownames(a2) <- NULL
write.csv(a2, file.path(OUT_DIR, "analysis2_tercile_gaps.csv"), row.names = FALSE)
stamp("Analysis 2 written (%d instrument x form x budget x scope rows); statistics not printed%s",
      nrow(a2), if (SMOKE) " (SMOKE: never printed)" else " (see CSVs)")

## ── 8. verdicts per the plan's pre-stated reading rules ───────────────────────
g_of <- function(instr, b, sc) {
  r <- a2 %>% filter(instrument == instr, form == "min", budget == b, scope == sc)
  if (nrow(r) != 1) return(list(gap = NA_real_, p95 = NA_real_))
  list(gap = r$gap, p95 = r$perm_p95)
}
verdicts <- lapply(INSTRUMENTS, function(instr) {
  if (!(instr %in% ELIG))   # gate: the signal reading was never computed
    return(list(instrument = instr, eligible = FALSE, computed = FALSE,
                signal = FALSE))
  g5a <- g_of(instr, 5, "all_capacity"); g10a <- g_of(instr, 10, "all_capacity")
  g5f <- g_of(instr, 5, "f_lt_099")
  c1 <- isTRUE(g5a$gap > g5a$p95)                       # (i)
  c2 <- isTRUE(g10a$gap > 0)                            # (ii)
  c3 <- if (instr == "i1_fresh_overlap") TRUE           # (iii): i1 exempt
        else isTRUE(g5f$gap > g5f$p95)
  list(instrument = instr, eligible = TRUE, computed = TRUE,
       signal = c1 && c2 && c3,
       cond_i = c1, cond_ii = c2, cond_iii = c3,
       gap5_all = g5a$gap, gap5_all_p95 = g5a$p95,
       gap10_all = g10a$gap, gap5_f = g5f$gap, gap5_f_p95 = g5f$p95,
       winner_metric = if (instr == "i1_fresh_overlap") g5a$gap else g5f$gap)
})
names(verdicts) <- INSTRUMENTS
sig_set <- INSTRUMENTS[vapply(verdicts, function(v) isTRUE(v$signal), logical(1))]
winner <- if (length(sig_set) == 0) NA_character_ else {
  wm <- vapply(sig_set, function(i) verdicts[[i]]$winner_metric, numeric(1))
  cand <- sig_set[wm >= max(wm) - 1e-12]
  cand[order(match(cand, COST_ORDER))][1]
}

sum_lines <- c(
  "PROFILE-DISTANCE DIAGNOSTIC: one-screen summary",
  sprintf("Plan: methods/profile_distance_diagnostic_plan_2026-08-06.md | run %s%s",
          format(Sys.time(), "%Y-%m-%d %H:%M"), if (SMOKE) " [SMOKE - VOID]" else ""),
  sprintf("States %d | instances %d | distinct rules %d | A1 pairs %d | perm draws %d",
          length(states), nrow(inst), nrow(uni), nrow(pairs), N_PERM),
  "",
  sprintf("%-18s %-11s %-9s  %8s %8s %8s %8s  %s", "instrument", "certificate",
          "eligible", "gap5all", "p95null", "gap10", "gap5f<99", "verdict"))
for (instr in INSTRUMENTS) {
  v <- verdicts[[instr]]
  cr <- cert_sum$median_rho[cert_sum$instrument == instr]
  sum_lines <- c(sum_lines, if (!v$computed) sprintf(
    "%-18s %-11.3f %-9s  %8s %8s %8s %8s  %s",
    instr, cr, "NO", "-", "-", "-", "-",
    "INELIGIBLE - signal not computed")
  else sprintf(
    "%-18s %-11.3f %-9s  %8.4f %8.4f %8.4f %8.4f  %s",
    instr, cr, "yes",
    v$gap5_all, v$gap5_all_p95, v$gap10_all,
    ifelse(is.na(v$gap5_f), NA, v$gap5_f),
    if (v$signal) "SIGNAL" else "NO SIGNAL"))
}
sum_lines <- c(sum_lines, "",
  if (length(sig_set) == 0) paste(
    "Stage-2 recommendation (plan): NO instrument clears (i)-(iii).",
    "Characterization stays descriptive decision support; the preference-ordering",
    "study proceeds on state preference alone with the measured price reported and",
    "no recall or precision claim; the ledger row closes at public-data scale,",
    "open at internal-data scale only.")
  else sprintf(paste(
    "Stage-2 recommendation (plan's no-arm gate): winner = %s (largest f<0.99 gap;",
    "i1 judged on all capacity; ties to the cheaper instrument). SIGNAL does NOT",
    "create a study arm: next step is the retrospective re-walk on the section 32",
    "harness (same rules, same capacity, LCB order with %s as tie-break/diversity",
    "term, scored on FY2024), judged on the section 30 bar: +0.010 within-state",
    "median at the 5%% budget across 49 states."), winner, winner))
writeLines(sum_lines, file.path(OUT_DIR, "summary.txt"))
if (!SMOKE) say_full("%s\n", paste(sum_lines, collapse = "\n"))

## ── 9. coverage report ────────────────────────────────────────────────────────
cov <- c(
  sprintf("states: %d | lists verified vs per_rule_marginal.csv: %d/%d",
          length(states), n_assert_anchor, length(states) * length(BUDGETS)),
  sprintf("deployed instances (holdout basis): %d | distinct rules: %d",
          nrow(inst), nrow(uni)),
  sprintf("rules with >= %d FY2022-23 error cases (i4 eligible): %d/%d (%.1f%% of instances, %.1f%% of 5%%-budget capacity excluded)",
          PROFILE_FLOOR, sum(n_errcase_tr >= PROFILE_FLOOR), nrow(uni),
          100 * mean(n_errcase_tr[inst$rule_id] < PROFILE_FLOOR),
          100 * sum(inst$n_new[inst$budget == 5][n_errcase_tr[inst$rule_id[inst$budget == 5]] < PROFILE_FLOOR]) /
            max(1, sum(inst$n_new[inst$budget == 5]))),
  sprintf("rules with >= %d train error cases (attenuation scope): %d",
          ATTEN_FLOOR, sum(n_errcase_tr >= ATTEN_FLOOR)),
  sprintf("A1 pairs: %d distinct; outcome defined: %d; both>=50 scope: %d",
          nrow(pairs), sum(!is.na(pairs$complementarity)), sum(scopes1$both_ge50_errcases)),
  sprintf("A2 instances with a predecessor: %d/%d | f < 0.99 instances: %d",
          sum(has_pred), nrow(inst), sum(inst$f < 0.99)),
  sprintf("spectral: dims %s, isolated nodes %s",
          paste(unlist(SPEC_DIMS), collapse = "/"),
          paste(vapply(FULL$spec_detail, function(s) s$n_isolated, numeric(1)),
                collapse = "/")),
  sprintf("permutation draws: %d | certificate splits: %d", N_PERM, N_SPLITS))
writeLines(cov, file.path(OUT_DIR, "coverage_report.txt"))

el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
if (SMOKE) {
  cat("\n================ SMOKE PLUMBING SUMMARY (no signal statistics) ================\n")
  cat(sprintf("anchor assertions            : %d/%d state-budget refill walks reproduced per_rule_marginal.csv\n",
              n_assert_anchor, length(states) * length(BUDGETS)))
  cat("python profile reconciliation: PASS (helper exits non-zero on any mismatch)\n")
  cat(sprintf("cross-evaluator flag counts  : PASS for all %d rules (python == R)\n", nrow(uni)))
  cat(sprintf("rows: instances %d | rules %d | A1 pairs %d (outcome defined %d) | A2 instances w/ pred %d\n",
              nrow(inst), nrow(uni), nrow(pairs),
              sum(!is.na(pairs$complementarity)), sum(has_pred)))
  cat(sprintf("profile cases matched to frame rows: %d | i4 floor >= %d: %d/%d rules\n",
              nrow(cases), PROFILE_FLOOR, sum(n_errcase_tr >= PROFILE_FLOOR), nrow(uni)))
  cat("certificate values on the SMOKE subset (plumbing, not a signal reading):\n")
  for (i in seq_len(nrow(cert_sum)))
    cat(sprintf("  %-18s median rho %.3f (%.3f-%.3f) -> %s\n",
                cert_sum$instrument[i], cert_sum$median_rho[i], cert_sum$min_rho[i],
                cert_sum$max_rho[i], ifelse(cert_sum$eligible[i], "eligible", "INELIGIBLE")))
  cat(sprintf("analysis tables computed and written to the scratch dir only: analysis1 %d rows, analysis2 %d rows\n",
              nrow(a1), nrow(a2)))
  cat(sprintf("wall time: %.1f minutes | perm draws %d | splits %d\n", el, N_PERM, N_SPLITS))
  unlink(OUT_DIR, recursive = TRUE)
  cat(sprintf("SMOKE scratch dir deleted: %s\n", OUT_DIR))
} else {
  stamp("done in %.1f minutes; outputs in %s/", el, OUT_DIR)
}
