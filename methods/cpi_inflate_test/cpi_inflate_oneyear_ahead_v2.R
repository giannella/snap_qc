# CPI-inflation test, one year ahead (staging/cpi-inflate, 2026-09-20).
# Design note: methods/cpi_inflate_test/design_note.md. A technical
# exploration (no bars, no verdicts): it measures what cpi_inflate()
# (features.R, commit ccdd682) does to held-out rule performance.
#
# One arm of one era per invocation, selected by environment variables:
#   CPI_ERA = "1718_19"  (mine FY2017-18, score FY2019; CPI frame target 2019)
#           | "2223_24"  (mine FY2022-23, score FY2024; CPI frame target 2024)
#           | "1719_22"  (added 2026-09-21: mine FY2017-19, score FY2022, a
#                         three-year projection across the excluded FY2020-21;
#                         CPI frame target 2022, training values raised 14-22%)
#   CPI_ARM = "nominal"        nominal-dollar frame, seed 117
#           | "cpi"            cpi_inflate() frame (target = test year), seed 117
#           | "nominal_seed2"  nominal frame, seed 118
#           | "cpi_seed2"      cpi_inflate() frame, seed 118
# The frame contrast is read at both seeds (cpi - nominal, cpi_seed2 -
# nominal_seed2); the seed contrasts (same frame, new seed) show what a
# re-mine with no design change moves.
# Exactly one component varies between "nominal" and "cpi": the frame's six
# CPI-inflated raw fields and the features computed from them. Rows, error
# flags, vocabulary (the shipped 19 features, `utilities_sua` encoding, as
# in methods/v250_build_staged_lists_utilsua_v2.R), engines, seed,
# admission (joint BH FDR 10% + n >= 30), ordering (99% Wilson LCB), the
# artifact tag, the fresh-share walk and the cap-walk scoring are the
# benchmark recipe (methods/v250_benchmark_era2_baseline_v2.R) verbatim.
# NATIONAL pool only: no state mines, no blend. The list-level readout is
# therefore the national-only list per state, not the blended deliverable.
#
# SMOKE=1: tiny ensembles, 4 states, own output dir.
# Run from the worktree root. Outputs -> methods/cpi_inflate_test/out/.

suppressMessages(library(dplyr))
source("rule_mining_helpers.R")

ERA   <- Sys.getenv("CPI_ERA"); ARM <- Sys.getenv("CPI_ARM")
SMOKE <- identical(Sys.getenv("SMOKE"), "1")
RESUME <- identical(Sys.getenv("CPI_RESUME"), "1")   # reuse a saved pool (frame md5 must match)
stopifnot(ERA %in% c("1718_19", "2223_24", "1719_22"),
          ARM %in% c("nominal", "cpi", "nominal_seed2", "cpi_seed2"))
IS_CPI <- grepl("^cpi", ARM)

ERA_CFG <- list(
  "1718_19" = list(train = c("2017", "2018"), test = "2019", target = 2019L),
  "2223_24" = list(train = c("2022", "2023"), test = "2024", target = 2024L),
  "1719_22" = list(train = c("2017", "2018", "2019"), test = "2022", target = 2022L))[[ERA]]
TRAIN_YEARS <- ERA_CFG$train; TEST_YEAR <- ERA_CFG$test
SEED        <- if (grepl("_seed2$", ARM)) 118 else 117
FRAME_DIR   <- "methods/cpi_inflate_test/frames"
NOMINAL_FILE <- file.path(FRAME_DIR, "reg_model_data_nominal.rds")
FRAME_FILE  <- if (IS_CPI) file.path(FRAME_DIR, sprintf("reg_model_data_%d.rds", ERA_CFG$target)) else NOMINAL_FILE
FRAME_MD5   <- unname(tools::md5sum(FRAME_FILE))

BUDGETS     <- c(0.05, 0.10)
BUFFER_MULT <- 3
FRESH_MIN   <- 0.50
LCB_Z       <- 2.326
FDR_ALPHA   <- 0.10
MIN_N       <- 30
XGB <- list(nrounds = 1000, max_depth = 4, eta = 0.02, subsample = 0.20)
RF  <- list(num_trees = 1000, max_depth = 4, mtry = 2, min_node_size = 20)
SIGNIF_DIGITS <- 3
SWEEP_GRID <- c(0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.50)

BASE_FEATURES <- c(
  "HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size",
  "expedited_i", "bbce_state_i", "rawben_rel_max", "medical_deductions",
  "shelter_expenses_by_hh_size", "utilities_sua", "married", "homeless",
  "percent_abawd", "unc_rawben_rel_max", "months_since_cert_n",
  "count_divisible_by_100")
VOCAB19 <- c(BASE_FEATURES,
             "gross_by_hh_size", "earned_by_hh_size", "unearned_by_hh_size")
BINARY_FEATURES <- c("children_i", "elderly_disabled_i", "expedited_i",
                     "married", "homeless", "bbce_state_i")
MM_TAG_SHARE <- 0.25

OUT_DIR <- "methods/cpi_inflate_test/out"
if (SMOKE) {
  XGB$nrounds <- 40; RF$num_trees <- 40
  OUT_DIR <- file.path(OUT_DIR, "smoke")
}
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
TAG <- sprintf("%s_%s", ERA, ARM)

HH_LEVELS <- c("1", "2-3", "4+")
hh_group_of <- function(n) {
  n <- suppressWarnings(as.numeric(as.character(n)))
  ifelse(is.na(n), NA_character_, ifelse(n <= 1, "1", ifelse(n <= 3, "2-3", "4+")))
}
stamp <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"),
                                   sprintf(...)))

## ---- frame ------------------------------------------------------------------
stamp("arm %s | frame %s | seed %d%s", TAG, FRAME_FILE, SEED,
      if (SMOKE) " | SMOKE" else "")
reg_model_data <- readRDS(FRAME_FILE)
stopifnot("utilities_sua" %in% names(reg_model_data))
adf0 <- reg_model_data %>% filter(fiscal_year %in% c(TRAIN_YEARS, TEST_YEAR))
rm(reg_model_data); invisible(gc())
KEEP <- c(VOCAB19, "state", "fiscal_year", "over_threshold", "total_error_amount",
          "cert_HH_size_FS_n", "rawben", "benmax", "rawben_uncapped")
pf  <- prep_features(adf0[, KEEP], VOCAB19)   # the frame carries 1,286 columns; keep the 27 used
adf <- pf$data; rm(adf0); invisible(gc())
stopifnot(length(setdiff(VOCAB19, pf$features)) == 0)
st <- as.character(adf$state)
yr <- as.character(adf$fiscal_year)
ie_all <- !is.na(adf$over_threshold) & adf$over_threshold != 0
ed_all <- ifelse(ie_all, abs(ifelse(is.na(adf$total_error_amount), 0,
                                    adf$total_error_amount)), 0)
hh_all <- hh_group_of(adf$cert_HH_size_FS_n)
is_tr  <- yr %in% TRAIN_YEARS
is_te  <- yr == TEST_YEAR
mm_all <- adf$rawben >= adf$benmax & adf$rawben_uncapped < adf$benmax
stopifnot(!anyNA(mm_all), sum(mm_all) < 1000)   # baseline driver's post-fix-frame assert
stamp("train %d rows / %d errors | test %d rows / %d errors | mismatch rows %d",
      sum(is_tr), sum(ie_all[is_tr]), sum(is_te), sum(ie_all[is_te]), sum(mm_all))
support <- data.frame(
  era = ERA, arm = ARM,
  split = rep(c("train", "test"), each = length(HH_LEVELS)),
  hh = rep(HH_LEVELS, 2),
  rows = c(vapply(HH_LEVELS, function(h) sum(is_tr & hh_all %in% h), numeric(1)),
           vapply(HH_LEVELS, function(h) sum(is_te & hh_all %in% h), numeric(1))),
  errors = c(vapply(HH_LEVELS, function(h) sum(ie_all[is_tr & hh_all %in% h]), numeric(1)),
             vapply(HH_LEVELS, function(h) sum(ie_all[is_te & hh_all %in% h]), numeric(1))))
write.csv(support, file.path(OUT_DIR, sprintf("support_%s.csv", TAG)), row.names = FALSE)

STATES <- sort(unique(st))
if (SMOKE) STATES <- c("Washington", "Maine", "Mississippi", "Illinois")

## ---- admission (benchmark recipe) ------------------------------------------
admit_rank <- function(rdf, n, k, base_rate) {
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

walk_fill <- function(idx_tr, n_tr, b) {
  # pass zero + shipped fresh-share re-walk with tolerated per-phase gaps
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
  gap_core <- 0L; gap_total <- 0L
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
      if (ph == 1) gap_core <- C0 - n_in
    }
    gap_total <- CT - n_in
    stopifnot(gap_core >= 0L, gap_total >= 0L)
  }
  list(frozen = frozen, buffer = buffer, fill_cases = n_in,
       gap_core = gap_core, gap_total = gap_total)
}

score_test <- function(sel, idx_te, n_te, b, err_te, doll_te) {
  cap <- floor(b * n_te); un <- rep(FALSE, n_te); used <- logical(length(sel))
  for (j in seq_along(sel)) {
    i <- sel[j]
    add <- sum(!un[idx_te[[i]]])
    if (add > 0 && sum(un) + add <= cap) { un[idx_te[[i]]] <- TRUE; used[j] <- TRUE }
  }
  prec <- sum(un & err_te) / max(sum(un), 1)
  list(stats = c(n_flagged = sum(un), n_errors = sum(un & err_te), precision = prec,
                 dollar_recall = sum(doll_te[un]) / max(sum(doll_te), 1)),
       used = sel[used])
}

## ---- per-stratum scoring ----------------------------------------------------
# reduce_flags_for_rules() evaluates every condition on every row it is given,
# and a worker held ~13 GB doing that for ~150k rules on 78k training rows
# (2026-09-20). A rule only ever flags rows of its own household-size stratum,
# so scoring each stratum's rules on that stratum's rows alone gives the same
# numbers from a fraction of the memory. `cols` is a named list of per-row
# vectors (aligned to `data`); each rule returns c(n flagged, sum of each col).
score_by_stratum <- function(rules_df, data, strata_idx, cols, label = "") {
  out <- matrix(NA_real_, nrow = nrow(rules_df), ncol = 1L + length(cols))
  for (h in names(strata_idx)) {
    r <- which(rules_df$hh == h); if (!length(r)) next
    rows <- strata_idx[[h]]
    sub  <- data[rows, , drop = FALSE]
    cs   <- lapply(cols, function(v) v[rows])
    out[r, ] <- reduce_flags_for_rules(
      rules_df[r, , drop = FALSE], sub, setNames(list(seq_along(rows)), h),
      function(ix) c(length(ix), vapply(cs, function(v) sum(v[ix]), numeric(1))),
      label = if (nzchar(label)) paste(label, h) else "")
    rm(sub, cs); invisible(gc())
  }
  stopifnot(!anyNA(out))   # a rule whose stratum is not in strata_idx would stay NA
  out
}

## ---- national pool: mine on train, score on train and on the test year ------
tr_rows <- which(is_tr); te_rows <- which(is_te)
trn <- adf[tr_rows, , drop = FALSE]; tes <- adf[te_rows, , drop = FALSE]
ie_tr <- ie_all[tr_rows]; ed_tr <- ed_all[tr_rows]; mm_tr <- mm_all[tr_rows]
ie_te <- ie_all[te_rows]; ed_te <- ed_all[te_rows]
strata_tr <- lapply(setNames(nm = HH_LEVELS), function(h) which(hh_all[tr_rows] %in% h))
strata_te <- lapply(setNames(nm = HH_LEVELS), function(h) which(hh_all[te_rows] %in% h))

# RECORDED-DOLLAR test rows for the CPI arms (review 2026-09-21). The step
# keys on the calendar year of the review month, so the CPI frame also
# inflates the test year's October-December cases (x1.080 for FY2022, a
# quarter of the year). Two deployment readings follow:
#   test rows of the arm's own frame = a state runs its live cases through the
#     feature code with the CPI step on;
#   recorded (nominal-frame) test rows = the as-shipped situation: rules mined
#     in target-year dollars, applied to the state's cases as recorded.
# Every held-out readout is produced on both; for nominal arms they coincide.
tes_nom <- NULL
if (IS_CPI) {
  nom <- readRDS(NOMINAL_FILE) %>% filter(fiscal_year %in% TEST_YEAR)
  tes_nom <- prep_features(nom[, KEEP], VOCAB19)$data; rm(nom); invisible(gc())
  stopifnot(nrow(tes_nom) == nrow(tes),
            identical(as.character(tes_nom$state), as.character(tes$state)),
            identical(tes_nom$over_threshold, tes$over_threshold),
            identical(hh_group_of(tes_nom$cert_HH_size_FS_n), hh_all[te_rows]))
}

pool_fn <- file.path(OUT_DIR, sprintf("pool_%s.rds", TAG))
if (RESUME && file.exists(pool_fn)) {
  natl <- readRDS(pool_fn)
  stopifnot(identical(natl$frame_md5[1], FRAME_MD5), natl$seed[1] == SEED)
  stamp("pool resumed from %s: %d admitted rules", pool_fn, nrow(natl))
} else {
  stamp("mining the national pool (FY%s, %d rows) ...",
        paste(TRAIN_YEARS, collapse = "-"), nrow(trn))
  rdf <- mine_rule_vocabulary(
    trn, list(any_error = list(rows = seq_len(nrow(trn)), ie = ie_tr)),
    strata_tr, VOCAB19, xgb = XGB, rf = RF,
    signif_digits = SIGNIF_DIGITS, seed = SEED, verbose = TRUE,
    binary_features = BINARY_FEATURES)
  n_raw <- nrow(rdf)
  stamp("raw rules: %d; scoring on train ...", n_raw)
  sc <- score_by_stratum(rdf, trn, strata_tr,
                         list(k = ie_tr, mm_n = mm_tr, mm_k = mm_tr & ie_tr),
                         label = paste(TAG, "train"))
  if (SMOKE) {   # identity check against the all-rows path, smoke only
    sc0 <- reduce_flags_for_rules(
      rdf, trn, strata_tr,
      function(ix) c(length(ix), sum(ie_tr[ix]), sum(mm_tr[ix]), sum(mm_tr[ix] & ie_tr[ix])))
    stopifnot(isTRUE(all.equal(sc, sc0, check.attributes = FALSE, tolerance = 0)))
    stamp("per-stratum scoring identical to the all-rows path (%d rules)", nrow(rdf))
  }
  base_by_hh <- vapply(strata_tr, function(rows) mean(ie_tr[rows]), numeric(1))
  natl <- admit_rank(rdf, sc[, 1], sc[, 2], base_by_hh[rdf$hh])
  ix_adm <- match(paste(natl$hh, natl$rule), paste(rdf$hh, rdf$rule))
  natl$mm_n <- sc[ix_adm, 3]; natl$mm_k <- sc[ix_adm, 4]
  natl$mm_share_flags  <- round(natl$mm_n / natl$n, 4)
  natl$mm_share_errors <- round(ifelse(natl$k > 0, natl$mm_k / natl$k, 0), 4)
  natl$artifact_i <- natl$mm_share_flags >= MM_TAG_SHARE |
    natl$mm_share_errors >= MM_TAG_SHARE
  natl$base_tr <- unname(base_by_hh[natl$hh])
  natl$n_stratum_tr <- unname(vapply(strata_tr, length, integer(1))[natl$hh])
  stamp("admitted %d of %d raw rules (artifact-tagged %d, top-40 tagged %d); scoring on FY%s ...",
        nrow(natl), n_raw, sum(natl$artifact_i),
        sum(natl$artifact_i[seq_len(min(40L, nrow(natl)))]), TEST_YEAR)
  sct <- score_by_stratum(natl, tes, strata_te, list(k = ie_te, d = ed_te),
                          label = paste(TAG, "test"))
  if (SMOKE) {
    sct0 <- reduce_flags_for_rules(
      natl, tes, strata_te, function(ix) c(length(ix), sum(ie_te[ix]), sum(ed_te[ix])))
    stopifnot(isTRUE(all.equal(sct, sct0, check.attributes = FALSE, tolerance = 0)))
    stamp("per-stratum TEST scoring identical to the all-rows path (%d rules)", nrow(natl))
  }
  natl$n_te <- as.integer(sct[, 1]); natl$k_te <- sct[, 2]; natl$dollars_te <- sct[, 3]
  base_te_by_hh <- vapply(strata_te, function(rows) mean(ie_te[rows]), numeric(1))
  natl$base_te <- unname(base_te_by_hh[natl$hh])
  natl$n_stratum_te <- unname(vapply(strata_te, length, integer(1))[natl$hh])
  if (IS_CPI) {
    scn <- score_by_stratum(natl, tes_nom, strata_te, list(k = ie_te, d = ed_te),
                            label = paste(TAG, "recorded-dollar test rows"))
    natl$n_te_nomtest <- as.integer(scn[, 1]); natl$k_te_nomtest <- scn[, 2]
    natl$dollars_te_nomtest <- scn[, 3]
    rm(scn); invisible(gc())
  } else {
    natl$n_te_nomtest <- natl$n_te; natl$k_te_nomtest <- natl$k_te
    natl$dollars_te_nomtest <- natl$dollars_te
  }
  natl$rank_lcb <- seq_len(nrow(natl))
  natl$era <- ERA; natl$arm <- ARM; natl$n_raw_rules <- n_raw
  natl$seed <- SEED; natl$frame_md5 <- FRAME_MD5
  rownames(natl) <- NULL
  saveRDS(natl, pool_fn)
  stamp("pool saved: %s", pool_fn)
}

## ---- pool-level sweep on the test year (an error caught by several rules counts once)
use <- natl[!natl$artifact_i, , drop = FALSE]
sweep_rows <- list(); un <- rep(FALSE, nrow(tes)); un_nom <- rep(FALSE, nrow(tes)); done <- 0L
for (t in sort(SWEEP_GRID, decreasing = TRUE)) {
  new <- which(use$lcb >= t); new <- new[new > done]
  if (length(new)) {
    for (start in seq.int(1L, length(new), by = 4096L)) {
      blk <- new[start:min(start + 4095L, length(new))]
      fl <- flags_for_rules(use[blk, , drop = FALSE], tes, strata_te)
      for (ix in fl) un[ix] <- TRUE
      if (IS_CPI) {
        fl <- flags_for_rules(use[blk, , drop = FALSE], tes_nom, strata_te)
        for (ix in fl) un_nom[ix] <- TRUE
      } else un_nom <- un
      rm(fl)
    }
    done <- max(new)
  }
  if (done == 0L) next
  sweep_rows[[length(sweep_rows) + 1L]] <- data.frame(
    era = ERA, arm = ARM, lcb_floor = t, n_rules = done, n_flagged = sum(un),
    workload = mean(un), precision = sum(un & ie_te) / max(sum(un), 1),
    recall = sum(un & ie_te) / sum(ie_te),
    dollar_recall = sum(ed_te[un]) / sum(ed_te),
    workload_nomrows = mean(un_nom),
    precision_nomrows = sum(un_nom & ie_te) / max(sum(un_nom), 1),
    recall_nomrows = sum(un_nom & ie_te) / sum(ie_te),
    dollar_recall_nomrows = sum(ed_te[un_nom]) / sum(ed_te))
}
sweep <- bind_rows(sweep_rows)
write.csv(sweep, file.path(OUT_DIR, sprintf("sweep_%s.csv", TAG)), row.names = FALSE)
stamp("sweep written (%d floors)", nrow(sweep))

## ---- list level: national-only list per state, walked on train, scored on test
cols <- c("hh", "rule", "n", "k", "lcb")
rows <- list(); used_rows <- list()
for (state in STATES) {
  tr_s <- which(st[tr_rows] == state); te_s <- which(st[te_rows] == state)
  trs <- trn[tr_s, , drop = FALSE]; tss <- tes[te_s, , drop = FALSE]
  err_te <- ie_te[te_s]; doll_te <- ed_te[te_s]
  s_tr <- lapply(setNames(nm = HH_LEVELS), function(h) which(hh_all[tr_rows][tr_s] %in% h))
  s_te <- lapply(setNames(nm = HH_LEVELS), function(h) which(hh_all[te_rows][te_s] %in% h))
  idx_tr <- flags_for_rules(use[, cols], trs, s_tr)
  idx_te <- flags_for_rules(use[, cols], tss, s_te)
  idx_te_nom <- if (IS_CPI) flags_for_rules(use[, cols], tes_nom[te_s, , drop = FALSE], s_te) else idx_te
  for (b in BUDGETS) {
    wf  <- walk_fill(idx_tr, nrow(trs), b)
    sel <- c(wf$frozen, wf$buffer)
    sc  <- score_test(sel, idx_te, nrow(tss), b, err_te, doll_te)
    s24 <- sc$stats; base <- mean(err_te)
    s24n <- score_test(sel, idx_te_nom, nrow(tss), b, err_te, doll_te)$stats
    rows[[length(rows) + 1L]] <- data.frame(
      era = ERA, arm = ARM, state = state, budget = b,
      n_core = length(wf$frozen), n_buffer = length(wf$buffer),
      n_te = nrow(tss), n_flagged = s24[["n_flagged"]],
      n_errors_caught = s24[["n_errors"]],
      precision = round(s24[["precision"]], 4), base_rate_te = round(base, 4),
      dollar_recall = round(s24[["dollar_recall"]], 4),
      n_flagged_nomrows = s24n[["n_flagged"]],
      precision_nomrows = round(s24n[["precision"]], 4),
      dollar_recall_nomrows = round(s24n[["dollar_recall"]], 4),
      fill_gap_core = wf$gap_core, fill_gap_total = wf$gap_total)
    if (length(sc$used))
      used_rows[[length(used_rows) + 1L]] <- data.frame(
        era = ERA, arm = ARM, state = state, budget = b,
        hh = use$hh[sc$used], rule = use$rule[sc$used],
        lcb = use$lcb[sc$used], stringsAsFactors = FALSE)
  }
  stamp("  %s done", state)
  rm(trs, tss, idx_tr, idx_te, idx_te_nom); invisible(gc())
}
lists <- bind_rows(rows)
write.csv(lists, file.path(OUT_DIR, sprintf("lists_%s.csv", TAG)), row.names = FALSE)
saveRDS(bind_rows(used_rows), file.path(OUT_DIR, sprintf("list_rules_%s.rds", TAG)))
for (b in BUDGETS) {
  s <- lists %>% filter(budget == b)
  stamp("LISTS %d%%: median precision %.4f (mean %.4f) | median dollar recall %.4f",
        round(100 * b), median(s$precision), mean(s$precision), median(s$dollar_recall))
}
stamp("arm %s complete", TAG)
