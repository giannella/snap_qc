# Seed stability of the shipped full-data recipe: with everything else fixed,
# does changing only the mining RNG seed change WHICH FY2024 error cases the
# top-K rules cover (a vocabulary property), or only the order they are reached
# in (an ordering property)? K in {100, 200, 1000, 20000, full admitted pool}.
#
# Pre-registered in methods/preregistration_seed_stability_2026-08-05.md,
# including the bars and the extension path; where this script and the prereg
# could disagree, the prereg wins.
#
# Exactly one component varies: the mining seed (mine_rule_vocabulary passes it
# to both engines - set.seed() before xgboost and ranger's own seed argument).
# Seed A is the cached full-data mine (fdr_raw_vocab/raw_national.rds, 144,533
# raw candidates, seed 117, mined on all 77,806 FY2022-23 rows); B and C are
# fresh mines with the explicit seeds in SEEDS below. Held fixed, as shipped:
# any-error frame, national pool, strata 1 / 2-3 / 4+, xgboost nrounds 1000
# eta 0.02 subsample 0.20 depth 4 + ranger 1000 trees mtry 2, admission BH at
# FDR 10% vs the stratum base rate AND n >= 30 (self-scored on train, pooled
# across strata with per-rule stratum base rates, exactly as the delivery
# builder runs it), ordering by the one-sided 99% Wilson LCB on one pooled
# national scale. No dedup is applied, matching the section 30 machinery this
# study is the direct contrast to (the walk skips zero-new-case rules, so
# redundancy cannot distort the fills).
#
# Depth cuts use a seed-independent tie-break: (LCB desc, train flagged count
# desc, hh + canonical rule text asc), never pool insertion order.
#
# Determinism anchor: seed A re-scored through this machinery must reproduce
# its cached era artifacts - per-rule n/k identical to the cache, 50,697
# admitted (findings detailed section 26) - or the study STOPS.
#
# Coverage-by-rank: each seed's ranked admitted pool is walked once over the
# FY2024 error cases (4,803, asserted at runtime), recording for each error
# case the FIRST rank that covers it; E_s(K) at any depth is then a prefix
# read, and full-pool coverage falls out of the same pass.
#
# Heavy train scoring goes through reduce_flags_for_rules (the unpatched
# evaluator OOMs this 29 GB host at national scale). Mines, scored pools,
# first-rank vectors, and signatures checkpoint to CACHE_DIR (*.rds is
# gitignored); set RESUME_FROM_CHECKPOINT <- TRUE in the runner so a killed
# run resumes at the last finished stage.
#
# SMOKE=1 (environment variable): plumbing check only - tiny ensembles, two
# smoke seeds, small K grid, anchor skipped (no cached smoke seed); outputs
# and cache land under methods/seed_stability_v2/smoke/.
#
# Expects `reg_model_data`. Outputs -> methods/seed_stability_v2/ (CSVs).
# No writes to state_delivery_lists/, no CHANGELOG entry, no version bump.

suppressMessages(library(dplyr))
source("rule_mining_helpers.R")

## ---- config -----------------------------------------------------------------
SMOKE <- identical(Sys.getenv("SMOKE"), "1")

# seed A = the cached mine; fresh seeds carry no `cache`. Seeds D and E extend
# this list by config only; every readout loops over names(SEEDS), the union
# accumulation walks them in list order.
SEEDS <- list(
  A = list(seed = 117,
           cache = "methods/state_similarity_v2/transfer_benchmark_train2223_test24/fdr_raw_vocab/raw_national.rds"),
  B = list(seed = 20260805),
  C = list(seed = 31415))
K_GRID      <- c(100, 200, 1000, 20000)   # "full" is added per seed at runtime
BUDGETS     <- c(0.05, 0.10)
TOPK_WINDOW <- 20000    # budget-readout window; slack-zero certified (findings 27)
BUFFER_MULT <- 3
LCB_Z       <- 2.326
FDR_ALPHA   <- 0.10
MIN_N       <- 30
TRAIN_YEARS <- c("2022", "2023")
TEST_YEAR   <- "2024"
XGB <- list(nrounds = 1000, max_depth = 4, eta = 0.02, subsample = 0.20)
RF  <- list(num_trees = 1000, max_depth = 4, mtry = 2, min_node_size = 20)
SIGNIF_DIGITS <- 3

# era artifacts for the seed A determinism anchor (findings detailed section 26)
ANCHOR_RAW_RULES <- 144533L
ANCHOR_ADMITTED  <- 50697L

# frame invariants the prereg pins; asserted at runtime
EXPECT_TRAIN_ROWS <- 77806L; EXPECT_TRAIN_ERRS <- 8485L; EXPECT_TEST_ERRS <- 4803L

# the same 10 states as section 30 run 2 (methods/partition_case_overlap_v2.R)
TARGETS <- c("California", "Texas", "Michigan", "Massachusetts", "Arizona",
             "Washington", "Louisiana", "Maine", "New Jersey", "Mississippi")

OUT_DIR <- "methods/seed_stability_v2"
if (!exists("RESUME_FROM_CHECKPOINT")) RESUME_FROM_CHECKPOINT <- FALSE

if (SMOKE) {
  XGB$nrounds <- 40; RF$num_trees <- 40
  K_GRID <- c(10, 50)
  TOPK_WINDOW <- 2000
  SEEDS <- list(B_smoke = list(seed = 901), C_smoke = list(seed = 902))
  OUT_DIR <- file.path(OUT_DIR, "smoke")
}
CACHE_DIR <- file.path(OUT_DIR, "cache")
dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)

HH_LEVELS <- c("1", "2-3", "4+")
hh_group_of <- function(n) {
  n <- suppressWarnings(as.numeric(as.character(n)))
  ifelse(is.na(n), NA_character_, ifelse(n <= 1, "1", ifelse(n <= 3, "2-3", "4+")))
}
strata_of <- function(df) lapply(setNames(nm = HH_LEVELS), function(h)
  which(hh_group_of(df$cert_HH_size_FS_n) %in% h))
features <- c(
  "HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size",
  "expedited_i", "cat_elig", "rawben_rel_max", "medical_deductions",
  "shelter_expenses_by_hh_size", "utilities", "married", "homeless",
  "rawearn_by_hh_size", "rawunearn_by_hh_size", "rawgross_by_hh_size",
  "percent_abawd", "unc_rawben_rel_max", "months_since_cert_n",
  "count_divisible_by_100")
stamp <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"),
                                   sprintf(...)))
jac <- function(a, b) if (length(union(a, b))) length(intersect(a, b)) / length(union(a, b)) else NA_real_

## ---- frame ------------------------------------------------------------------
pf  <- prep_features(reg_model_data %>%
                       filter(fiscal_year %in% c(TRAIN_YEARS, TEST_YEAR)), features)
adf <- pf$data; pv <- pf$features
st  <- as.character(adf$state)
yr  <- as.character(adf$fiscal_year)
ie_all <- !is.na(adf$over_threshold) & adf$over_threshold != 0
ed_all <- ifelse(ie_all, abs(ifelse(is.na(adf$total_error_amount), 0,
                                    adf$total_error_amount)), 0)

tr_rows <- which(yr %in% TRAIN_YEARS)
train   <- adf[tr_rows, , drop = FALSE]
ie_tr   <- ie_all[tr_rows]
strata_tr <- strata_of(train)
base_by_hh <- vapply(strata_tr, function(rows) mean(ie_tr[rows]), numeric(1))

# the coverage universe: FY2024 error cases only (case identity = position in
# this subset, identical for every seed)
te_rows  <- which(yr == TEST_YEAR)
err_rows <- te_rows[ie_all[te_rows]]
te_err   <- adf[err_rows, , drop = FALSE]
ed_err   <- ed_all[err_rows]
strata_err <- strata_of(te_err)
N_ERR    <- length(err_rows)

stopifnot(nrow(train) == EXPECT_TRAIN_ROWS, sum(ie_tr) == EXPECT_TRAIN_ERRS,
          N_ERR == EXPECT_TEST_ERRS)
stamp("train %d rows / %d errors | FY%s %d rows / %d errors (denominator) | %d states",
      nrow(train), sum(ie_tr), TEST_YEAR, length(te_rows), N_ERR,
      length(unique(st)))

## ---- per-seed pipeline: mine -> score -> anchor -> admit -> rank -------------

mine_seed <- function(name, spec) {
  if (!is.null(spec$cache)) {
    rdf <- readRDS(spec$cache)
    if (nrow(rdf) != ANCHOR_RAW_RULES)
      stop(sprintf("seed %s cache %s has %d rules, expected %d - wrong cache",
                   name, spec$cache, nrow(rdf), ANCHOR_RAW_RULES))
    stamp("seed %s: cached mine %s (%d raw rules, seed %d)",
          name, spec$cache, nrow(rdf), spec$seed)
    return(rdf)
  }
  # checkpoints key on the seed VALUE as well as the name, so editing a seed in
  # config can never silently resume another seed's artifacts (review 2026-08-05)
  fn <- file.path(CACHE_DIR, sprintf("mine_%s_%d.rds", name, spec$seed))
  if (RESUME_FROM_CHECKPOINT && file.exists(fn)) {
    rdf <- readRDS(fn)
    stamp("seed %s: resumed mine from %s (%d raw rules)", name, fn, nrow(rdf))
    return(rdf)
  }
  stamp("seed %s: mining (seed %d) ...", name, spec$seed)
  rdf <- mine_rule_vocabulary(
    train, list(any_error = list(rows = seq_len(nrow(train)), ie = ie_tr)),
    strata_tr, pv, xgb = XGB, rf = RF,
    signif_digits = SIGNIF_DIGITS, seed = spec$seed)
  saveRDS(rdf, fn)
  stamp("seed %s: mined %d raw rules -> %s", name, nrow(rdf), fn)
  rdf
}

score_seed <- function(name, spec, rdf) {
  fn <- file.path(CACHE_DIR, sprintf("scored_%s_%d.rds", name, spec$seed))
  if (RESUME_FROM_CHECKPOINT && file.exists(fn)) {
    stamp("seed %s: resumed scored pool from %s", name, fn)
    return(readRDS(fn))
  }
  is_anchor <- !is.null(spec$cache)
  n_cached <- if (is_anchor) rdf$n else NULL
  k_cached <- if (is_anchor) rdf$k else NULL
  stats <- reduce_flags_for_rules(
    rdf, train, strata_tr,
    fun = function(ix) c(length(ix), sum(ie_tr[ix])),
    label = sprintf("train-score seed %s", name))
  rdf$n <- as.integer(stats[, 1])
  rdf$k <- stats[, 2]
  if (is_anchor) {
    # determinism anchor, part 1: the era cache carries the n/k that fed the
    # era's admitted set and ranking; this machinery must reproduce them
    if (!(all(rdf$n == n_cached) && all(rdf$k == k_cached)))
      stop(sprintf(paste0(
        "DETERMINISM ANCHOR FAILED for seed %s: recomputed train n/k differ from ",
        "the cached era artifacts (%d rules with n mismatch, %d with k mismatch). ",
        "The study machinery does not reproduce the shipped scoring; STOPPING ",
        "before any new number is trusted."),
        name, sum(rdf$n != n_cached), sum(rdf$k != k_cached)))
    base_cached <- attr(rdf, "base_rates")
    if (!is.null(base_cached) &&
        max(abs(base_by_hh[names(base_cached)] - base_cached)) > 1e-12)
      stop(sprintf("DETERMINISM ANCHOR FAILED for seed %s: stratum base rates differ from the era cache (frame mismatch).", name))
    stamp("seed %s: anchor part 1 PASSED - recomputed n/k identical to the era cache for all %d rules",
          name, nrow(rdf))
  }
  saveRDS(rdf, fn)
  rdf
}

# admission (as shipped: BH at FDR 10% over the pool's candidates, one-sided vs
# the per-rule stratum base rate, AND n >= MIN_N) then ranking on the pooled
# national LCB scale, strata interleaved, with the seed-independent tie-break
admit_and_rank <- function(name, spec, rdf) {
  pvals <- pbinom(rdf$k - 1, rdf$n, base_by_hh[rdf$hh], lower.tail = FALSE)
  m <- length(pvals); o <- order(pvals)
  thr <- max(c(0L, which(pvals[o] <= FDR_ALPHA * seq_len(m) / m)))
  bh <- rep(FALSE, m); if (thr > 0) bh[o[seq_len(thr)]] <- TRUE
  keep <- bh & rdf$n >= MIN_N
  adm <- rdf[keep, , drop = FALSE]
  adm$lcb <- wilson_lcb(adm$k, adm$n, LCB_Z)
  # tie-break at depth cuts must not depend on pool insertion order:
  # LCB desc, train flagged count desc, then hh + canonical rule text asc
  adm <- adm[order(-adm$lcb, -adm$n, adm$hh, adm$rule, method = "radix"), ,
             drop = FALSE]
  rownames(adm) <- NULL
  if (!is.null(spec$cache)) {
    # determinism anchor, part 2: the admitted count is an era artifact
    if (nrow(adm) != ANCHOR_ADMITTED)
      stop(sprintf(paste0(
        "DETERMINISM ANCHOR FAILED for seed %s: %d rules admitted, the era ",
        "artifact is %d (findings detailed section 26). STOPPING."),
        name, nrow(adm), ANCHOR_ADMITTED))
    stamp("seed %s: anchor part 2 PASSED - %d admitted matches the era artifact; top of ranking: [hh %s, lcb %.4f, n %d] %s",
          name, nrow(adm), adm$hh[1], adm$lcb[1], adm$n[1], adm$rule[1])
  } else {
    stamp("seed %s: %d of %d admitted; top of ranking: [hh %s, lcb %.4f, n %d] %s",
          name, nrow(adm), nrow(rdf), adm$hh[1], adm$lcb[1], adm$n[1], adm$rule[1])
  }
  adm
}

# one pass down the ranked pool over the FY2024 error cases: first covering rank
coverage_first_rank <- function(name, seed, adm) {
  fn <- file.path(CACHE_DIR, sprintf("firstrank_%s_%d.rds", name, seed))
  if (RESUME_FROM_CHECKPOINT && file.exists(fn)) {
    fr <- readRDS(fn)
    if (length(fr) == N_ERR) {
      stamp("seed %s: resumed first-rank vector from %s", name, fn)
      return(fr)
    }
  }
  ci  <- build_condition_index(adm$rule)
  idx <- eval_conditions(ci$conditions, te_err)
  stamp("seed %s: coverage walk over %d ranked rules (%d unique conditions) x %d error cases",
        name, nrow(adm), length(ci$conditions), N_ERR)
  first_rank <- rep(NA_integer_, N_ERR)
  for (i in seq_len(nrow(adm))) {
    fl <- rule_flag_idx(ci$rule_conds[i], idx,
                        base_idx = strata_err[[adm$hh[i]]])[[1]]
    hit <- fl[is.na(first_rank[fl])]
    if (length(hit)) first_rank[hit] <- i
    if (i %% 10000 == 0)
      stamp("  seed %s: rank %d / %d, %d of %d errors covered so far",
            name, i, nrow(adm), sum(!is.na(first_rank)), N_ERR)
  }
  saveRDS(first_rank, fn)
  stamp("seed %s: full pool covers %d of %d FY%s errors", name,
        sum(!is.na(first_rank)), N_ERR, TEST_YEAR)
  first_rank
}

# structural signature per admitted rule (stratum + sorted variable/direction
# pairs, thresholds discarded), for the descriptive rule-level contrast
signatures_of <- function(name, seed, adm) {
  fn <- file.path(CACHE_DIR, sprintf("sigs_%s_%d.rds", name, seed))
  if (RESUME_FROM_CHECKPOINT && file.exists(fn)) {
    sg <- readRDS(fn)
    if (length(sg) == nrow(adm)) return(sg)
  }
  sg <- paste(adm$hh,
              vapply(adm$rule, function(r) .rule_struct(r)$sig, character(1)),
              sep = "|")
  saveRDS(sg, fn)
  sg
}

pools <- list(); firstrank <- list(); sigs <- list()
run_info <- list()
for (nm in names(SEEDS)) {
  spec <- SEEDS[[nm]]
  rdf  <- mine_seed(nm, spec)
  rdf  <- score_seed(nm, spec, rdf)
  adm  <- admit_and_rank(nm, spec, rdf)
  pools[[nm]]     <- adm
  firstrank[[nm]] <- coverage_first_rank(nm, spec$seed, adm)
  sigs[[nm]]      <- signatures_of(nm, spec$seed, adm)
  run_info[[nm]] <- data.frame(
    seed_name = nm, seed_value = spec$seed,
    source = if (is.null(spec$cache)) "fresh mine" else spec$cache,
    n_raw_rules = nrow(rdf), n_admitted = nrow(adm),
    top_rule_hh = adm$hh[1], top_rule_lcb = round(adm$lcb[1], 4),
    top_rule = adm$rule[1], stringsAsFactors = FALSE)
  rm(rdf); invisible(gc())
}
write.csv(bind_rows(run_info), file.path(OUT_DIR, "seed_stability_run_info.csv"),
          row.names = FALSE)

## ---- depth-K readouts (prefix reads of the first-rank vectors) ---------------
n_full  <- vapply(pools, nrow, integer(1))
k_labels <- c(as.character(K_GRID), "full")
k_of <- function(lbl, nm) if (lbl == "full") n_full[[nm]] else
  min(as.integer(lbl), n_full[[nm]])
covered_at <- function(nm, lbl) which(firstrank[[nm]] <= k_of(lbl, nm))
tot_dollars <- sum(ed_err)

cov <- list()
for (nm in names(SEEDS)) for (lbl in k_labels) {
  e <- covered_at(nm, lbl)
  cov[[length(cov) + 1]] <- data.frame(
    seed = nm, K = lbl, K_effective = k_of(lbl, nm), n_admitted = n_full[[nm]],
    n_errors_covered = length(e),
    reach = round(length(e) / N_ERR, 4),
    dollar_coverage = round(sum(ed_err[e]) / tot_dollars, 4))
}
cov <- bind_rows(cov)
write.csv(cov, file.path(OUT_DIR, "seed_stability_coverage_by_k.csv"),
          row.names = FALSE)

pairs <- utils::combn(names(SEEDS), 2, simplify = FALSE)
pw <- list()
for (p in pairs) for (lbl in k_labels) {
  a <- covered_at(p[1], lbl); b <- covered_at(p[2], lbl)
  ei <- intersect(a, b); eu <- union(a, b)
  # chance baseline at matched reach: two independent random subsets of the
  # observed sizes from the N_ERR error cases (section 30's baseline)
  exp_int <- length(a) * length(b) / N_ERR
  chance  <- exp_int / (length(a) + length(b) - exp_int)
  sa <- unique(sigs[[p[1]]][seq_len(k_of(lbl, p[1]))])
  sb <- unique(sigs[[p[2]]][seq_len(k_of(lbl, p[2]))])
  pw[[length(pw) + 1]] <- data.frame(
    pair = paste(p, collapse = "-"), K = lbl,
    n_covered_1 = length(a), n_covered_2 = length(b),
    jaccard_cases = round(jac(a, b), 4),
    chance_jaccard = round(chance, 4),
    jaccard_dollars = round(sum(ed_err[ei]) / sum(ed_err[eu]), 4),
    jaccard_signatures = round(jac(sa, sb), 4))
}
pw <- bind_rows(pw)
write.csv(pw, file.path(OUT_DIR, "seed_stability_pairwise_by_k.csv"),
          row.names = FALSE)

inter <- list()
for (lbl in k_labels) {
  sets <- lapply(names(SEEDS), covered_at, lbl = lbl)
  all_s <- Reduce(intersect, sets); any_s <- Reduce(union, sets)
  inter[[length(inter) + 1]] <- data.frame(
    K = lbl, n_seeds = length(sets),
    n_union = length(any_s), n_intersection = length(all_s),
    intersection_share_of_union = round(length(all_s) / max(length(any_s), 1), 4),
    intersection_share_of_errors = round(length(all_s) / N_ERR, 4),
    union_share_of_errors = round(length(any_s) / N_ERR, 4),
    median_pairwise_jaccard = round(median(pw$jaccard_cases[pw$K == lbl]), 4))
}
inter <- bind_rows(inter)
write.csv(inter, file.path(OUT_DIR, "seed_stability_intersection_by_k.csv"),
          row.names = FALSE)

# union-accumulation curve across FULL pools, in SEEDS list order (A, B, C;
# seeds D and E append by config)
acc <- list(); u <- integer(0)
for (i in seq_along(SEEDS)) {
  nm <- names(SEEDS)[i]
  u <- union(u, covered_at(nm, "full"))
  acc[[i]] <- data.frame(
    n_seeds = i, seeds = paste(names(SEEDS)[seq_len(i)], collapse = "+"),
    n_errors_union = length(u),
    union_share = round(length(u) / N_ERR, 4),
    union_dollar_share = round(sum(ed_err[u]) / tot_dollars, 4),
    marginal_gain_share = round((length(u) -
      if (i == 1) 0L else acc[[i - 1]]$n_errors_union) / N_ERR, 4))
}
acc <- bind_rows(acc)
write.csv(acc, file.path(OUT_DIR, "seed_stability_union_accumulation.csv"),
          row.names = FALSE)

# dense Jaccard-vs-depth curve for the descriptive K* (first depth where the
# median pairwise Jaccard crosses 0.70)
k_max <- max(n_full)
ks <- unique(pmin(round(exp(seq(log(10), log(k_max), length.out = 200))), k_max))
curve <- lapply(ks, function(k) {
  jj <- vapply(pairs, function(p) {
    a <- firstrank[[p[1]]] <= min(k, n_full[[p[1]]])
    b <- firstrank[[p[2]]] <= min(k, n_full[[p[2]]])
    a[is.na(a)] <- FALSE; b[is.na(b)] <- FALSE
    if (any(a | b)) sum(a & b) / sum(a | b) else NA_real_
  }, numeric(1))
  data.frame(K = k, median_jaccard = round(median(jj), 4),
             min_jaccard = round(min(jj), 4), max_jaccard = round(max(jj), 4))
})
curve <- bind_rows(curve)
write.csv(curve, file.path(OUT_DIR, "seed_stability_jaccard_curve.csv"),
          row.names = FALSE)
kstar <- curve$K[which(curve$median_jaccard >= 0.70)[1]]
stamp("K* (first depth with median pairwise Jaccard >= 0.70): %s",
      ifelse(is.na(kstar), "not reached", as.character(kstar)))

## ---- budget readout: the same 10 states as section 30 run 2 ------------------
# per seed, the national-only frozen list at the 5% and 10% budgets via the
# refill walk (fill on the state's FY2022-23 caseload to core + 3x buffer,
# freeze, walk against FY2024 to that year's cap), then pairwise Jaccard on the
# errors caught. Window = top TOPK_WINDOW ranked rules; slack 0 certifies the
# truncation did not change the fill (findings 27).
walk_mask <- function(idx_tr, idx_te, n_tr, n_te, b) {
  cap <- floor(b * n_tr); cap_buf <- floor(BUFFER_MULT * b * n_tr)
  un <- rep(FALSE, n_tr); n_in <- 0L; frozen <- integer(0); buffer <- integer(0)
  for (i in seq_along(idx_tr)) {   # already in rank order
    add <- sum(!un[idx_tr[[i]]]); if (add == 0) next
    if (n_in + add <= cap) { un[idx_tr[[i]]] <- TRUE; n_in <- n_in + add; frozen <- c(frozen, i) }
    else if (n_in + add <= cap_buf) { un[idx_tr[[i]]] <- TRUE; n_in <- n_in + add; buffer <- c(buffer, i) }
  }
  cap24 <- floor(b * n_te); un24 <- rep(FALSE, n_te)
  for (i in c(frozen, buffer)) {
    add <- sum(!un24[idx_te[[i]]])
    if (add > 0 && sum(un24) + add <= cap24) un24[idx_te[[i]]] <- TRUE
  }
  list(hit = un24, slack = cap_buf - n_in)
}

windows <- lapply(pools, function(adm)
  adm[seq_len(min(TOPK_WINDOW, nrow(adm))), , drop = FALSE])

budget_res <- list()
for (target in TARGETS) {
  tr_s <- which(st == target & yr %in% TRAIN_YEARS)
  te_s <- which(st == target & yr == TEST_YEAR)
  trs <- adf[tr_s, , drop = FALSE]; tes <- adf[te_s, , drop = FALSE]
  strata_trs <- strata_of(trs); strata_tes <- strata_of(tes)
  err_te <- which(ie_all[te_s])
  masks <- list()
  for (nm in names(SEEDS)) {
    w <- windows[[nm]]
    idx_tr <- flags_for_rules(w, trs, strata_trs, label = "")
    idx_te <- flags_for_rules(w, tes, strata_tes, label = "")
    for (b in BUDGETS) {
      wm <- walk_mask(idx_tr, idx_te, nrow(trs), nrow(tes), b)
      if (wm$slack > 0)
        stamp("  WARNING %s seed %s b=%.2f: slack %d (window may bind; findings 27)",
              target, nm, b, wm$slack)
      masks[[paste(nm, b)]] <- which(wm$hit)
    }
    rm(idx_tr, idx_te); invisible(gc())
  }
  for (b in BUDGETS) {
    fl <- lapply(names(SEEDS), function(nm) masks[[paste(nm, b)]])
    er <- lapply(fl, function(f) intersect(f, err_te))
    jf <- c(); je <- c()
    for (a in seq_len(length(fl) - 1)) for (bb in (a + 1):length(fl)) {
      jf <- c(jf, jac(fl[[a]], fl[[bb]])); je <- c(je, jac(er[[a]], er[[bb]]))
    }
    # section 30's chance baseline: two random subsets of the median caught
    # size from this state's error cases -> Jaccard r / (2 - r)
    r <- median(vapply(er, length, integer(1))) / max(length(err_te), 1)
    budget_res[[length(budget_res) + 1]] <- data.frame(
      target = target, budget = b, n_te = nrow(tes), n_err = length(err_te),
      flagged_med = median(vapply(fl, length, integer(1))),
      errors_med  = median(vapply(er, length, integer(1))),
      jaccard_flagged = round(median(jf), 4),
      jaccard_errors  = round(median(je), 4),
      jaccard_errors_min = round(min(je), 4),
      jaccard_errors_max = round(max(je), 4),
      chance_errors = round(r / (2 - r), 4),
      errors_in_all = length(Reduce(intersect, er)),
      errors_in_any = length(Reduce(union, er)))
  }
  stamp("  %s done", target)
}
budget_res <- bind_rows(budget_res)
budget_res$share_errors_in_all <- round(
  budget_res$errors_in_all / pmax(budget_res$errors_in_any, 1), 4)
write.csv(budget_res, file.path(OUT_DIR, "seed_stability_budget_readout.csv"),
          row.names = FALSE)

## ---- summary ----------------------------------------------------------------
stamp("=== seed stability summary (%d seeds: %s) ===",
      length(SEEDS), paste(names(SEEDS), collapse = ", "))
for (lbl in k_labels)
  stamp("K=%-6s median pairwise case Jaccard %.3f (chance %.3f) | signature Jaccard %.3f | all-seed intersection %.3f of union",
        lbl, median(pw$jaccard_cases[pw$K == lbl]),
        median(pw$chance_jaccard[pw$K == lbl]),
        median(pw$jaccard_signatures[pw$K == lbl]),
        inter$intersection_share_of_union[inter$K == lbl])
primary <- median(pw$jaccard_cases[pw$K == "20000"])
if (!SMOKE && "20000" %in% pw$K)
  stamp("PRIMARY BAR (median pairwise Jaccard at K=20000): %.3f | >= 0.80 ordering fork; < 0.60 vocabulary fork; between -> extend to seeds D, E (prereg)",
        primary)
for (b in BUDGETS)
  stamp("budget %.0f%%: median Jaccard flagged %.3f | errors caught %.3f | chance %.3f | vs section 30 partitions: %s",
        100 * b, median(budget_res$jaccard_flagged[budget_res$budget == b]),
        median(budget_res$jaccard_errors[budget_res$budget == b]),
        median(budget_res$chance_errors[budget_res$budget == b]),
        ifelse(b == 0.05, "0.325 flagged / 0.382 errors", "0.435 / 0.497"))
stamp("outputs written to %s", OUT_DIR)
