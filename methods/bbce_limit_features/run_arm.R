# BBCE-limit feature exploration, one arm: mine FY2017-19 with ranger alone on
# the arm's features (seed 117; ARM_SEED overrides), admit with the shipped
# recipe, drop the mismatch-tagged and income-pile-tagged rules as the shipped
# national-only builder does, then score
#   (a) the visible pool's rules on all FY2022 rows (top RULE_TOP), and the
#       regime check: every visible rule with a condition on a state-level
#       feature, its bound against the training error rate of its own regime
#       (the same stratum, limited by the same state-level conditions);
#   (b) the 49 national-only state lists, walked on each state's FY2017-19
#       rows with the shipped fill and scored on its FY2022 rows at 5% and
#       10%, twice: on all cases, and on the cases that are not income-pile
#       cases (pile rows removed from the test caseload, the budget a share of
#       the rest; the reading that stands in for a state's own file, which has
#       no piles; as methods/reconstruction_income_piles/gate_removal_test.R).
# Same blocks as methods/v270_seed_replicates/mine_national_seed.R and
# score_pool.R; the engine call, the feature list and the pile handling differ.
# SR_WINDOW=fy2024 (night 3) mines FY2022-23 and scores FY2024 instead, the
# same steps with those years; its outputs go to fy2024/.
#   ARM=B Rscript methods/bbce_limit_features/run_arm.R
source(file.path(Sys.getenv("SR_ROOT", "C:/Users/ericg/snap_qc_v27"), "methods/bbce_limit_features/common.R"))
ARM <- Sys.getenv("ARM"); stopifnot(ARM %in% names(ARMS))
TAG <- arm_tag(ARM)
FEATS <- ARMS[[ARM]]
RULE_TOP <- 10000L
F <- load_frame_arm()
stopifnot(all(FEATS %in% names(F$adf)))
# income-pile rows (piles are defined within each fiscal year, so the window's
# rows suffice); review counts: FY2017-19 1,232 rows / 320 errors, FY2022 427 / 135
pile_all <- income_pile_rows(F$adf)
for (y in c(TRAIN_YEARS, TEST_YEAR))
  stamp("FY%s: pile rows %d (errors %d)", y, sum(pile_all[F$yr == y]), sum(pile_all[F$yr == y] & F$ie[F$yr == y]))

## mine and admit (a pool on disk is kept, so a relaunch resumes)
fn_adm <- pool_file_arm(ARM, "admitted"); fn_raw <- pool_file_arm(ARM, "raw")
tr_rows <- which(F$is_tr)
trn <- F$adf[tr_rows, , drop = FALSE]; ie_tr <- F$ie[tr_rows]; mm_tr <- F$mm[tr_rows]; pile_tr <- pile_all[tr_rows]
strata_tr <- strata_of(F$hh[tr_rows])
base_by_hh <- vapply(strata_tr, function(rows) mean(ie_tr[rows]), numeric(1))
if (!file.exists(fn_adm)) {
  stamp("arm %s%s: mining FY%s with ranger alone at seed %d, mtry %d, %d features (%s added), %d rows, %d errors",
        TAG, if (SMOKE) " [SMOKE]" else "", paste(TRAIN_YEARS, collapse = "+"), SEED_ARM, MTRY_ARM, length(FEATS),
        paste(setdiff(FEATS, VOCAB19), collapse = ", "), nrow(trn), sum(ie_tr))
  rdf <- mine_ranger(trn, ie_tr, strata_tr, FEATS, SEED_ARM)
  stamp("raw rules: %d; scoring on the training rows ...", nrow(rdf))
  sc <- reduce_flags_for_rules(rdf, trn, strata_tr,
    function(ix) c(length(ix), sum(ie_tr[ix]), sum(mm_tr[ix]), sum(mm_tr[ix] & ie_tr[ix]),
                   sum(pile_tr[ix]), sum(pile_tr[ix] & ie_tr[ix])), label = sprintf("arm %s", TAG))
  natl <- admit_rank(rdf, sc[, 1], sc[, 2], base_by_hh[rdf$hh])
  ix_adm <- match(paste(natl$hh, natl$rule), paste(rdf$hh, rdf$rule))
  natl$mm_n <- sc[ix_adm, 3]; natl$mm_k <- sc[ix_adm, 4]
  natl$pile_n <- sc[ix_adm, 5]; natl$pile_k <- sc[ix_adm, 6]
  rdf$n <- as.integer(sc[, 1]); rdf$k <- sc[, 2]
  save_atomic(rdf, fn_raw); save_atomic(natl, fn_adm)
  stamp("arm %s: %d raw, %d admitted -> %s", TAG, nrow(rdf), nrow(natl), fn_adm)
  rm(rdf, sc); invisible(gc())
}
pool <- tag_artifacts(readRDS(fn_adm))
# the shipped pile gate: >= 0.25 of the rule's training flags or errors on pile rows
pool$pile_i <- round(pool$pile_n / pool$n, 4) >= PILE_TAG_SHARE |
  round(ifelse(pool$k > 0, pool$pile_k / pool$k, 0), 4) >= PILE_TAG_SHARE
vis <- pool[!pool$artifact_i & !pool$pile_i, , drop = FALSE]
stopifnot(!is.unsorted(-vis$lcb))
N_TAGGED <- sum(pool$artifact_i); N_PILE <- sum(pool$pile_i & !pool$artifact_i)
stamp("arm %s: %d admitted, %d mismatch-tagged and %d more pile-tagged (best pile-tagged rank %s), all dropped; %d visible",
      TAG, nrow(pool), N_TAGGED, N_PILE, if (any(pool$pile_i)) min(which(pool$pile_i)) else "-", nrow(vis))

## (a) rule level: top rules on FY2022, feature use, and the regime check
te <- which(F$yr == TEST_YEAR); tes <- F$adf[te, , drop = FALSE]
ie_te <- F$ie[te]; ed_te <- F$ed[te]
rl <- vis[seq_len(min(RULE_TOP, nrow(vis))), c("hh", "rule", "n", "k", "lcb")]
sc <- reduce_flags_for_rules(rl, tes, strata_of(F$hh[te]),
                             function(ix) c(length(ix), sum(ie_te[ix]), sum(ed_te[ix])), label = TAG)
rl$n_te <- sc[, 1]; rl$k_te <- sc[, 2]; rl$d_te <- sc[, 3]
rl$rank <- seq_len(nrow(rl))
for (f in unname(c(NEW, LIGHT, "bbce_state_i"))) rl[[paste0("uses_", f)]] <- uses(rl$rule, f)
# regime check over the whole visible pool: a rule limited to a regime must
# beat that regime's own training error rate, not only its stratum's
vis$regime <- regime_part(vis$rule)
key0 <- paste(vis$hh, vis$rule)
rg <- unique(vis[nzchar(vis$regime), c("hh", "regime")])
if (nrow(rg)) {
  rs <- reduce_flags_for_rules(data.frame(hh = rg$hh, rule = rg$regime), trn, strata_tr,
                               function(ix) c(length(ix), sum(ie_tr[ix])), label = "regimes")
  rg$regime_rate <- rs[, 2] / rs[, 1]
  vis <- merge(vis, rg, by = c("hh", "regime"), all.x = TRUE, sort = FALSE)
  vis <- vis[order(-vis$lcb, -vis$n, vis$hh, vis$rule, method = "radix"), ]
} else vis$regime_rate <- NA_real_
stopifnot(identical(paste(vis$hh, vis$rule), key0))   # the walk order is the admitted order
vis$rank <- seq_len(nrow(vis))
vis$lcb_below_regime_rate <- nzchar(vis$regime) & vis$lcb <= vis$regime_rate
save_atomic(list(arm = ARM, seed = SEED_ARM, mtry = MTRY_ARM, tag = TAG, features = FEATS, rules = rl,
                 regime = vis[nzchar(vis$regime), c("hh", "rule", "rank", "n", "k", "lcb", "regime", "regime_rate", "lcb_below_regime_rate")],
                 n_visible = nrow(vis), n_tagged = N_TAGGED, n_pile = N_PILE,
                 feature_use = vapply(unname(c(NEW, LIGHT, "bbce_state_i")), function(f) sum(uses(vis$rule, f)), integer(1))),
            file.path(OUT_DIR, sprintf("rules_%s.rds", TAG)))
stamp("arm %s: rule-level scores saved (top %d); regime-limited rules %d, of which %d have a bound at or below their regime's rate",
      TAG, nrow(rl), sum(nzchar(vis$regime)), sum(vis$lcb_below_regime_rate))
rm(tes, sc); invisible(gc())

## (b) the 49 national-only state lists, scored on all cases and on non-pile cases
STATES <- sort(unique(F$st))
if (SMOKE) STATES <- c("Washington", "Maine", "Mississippi", "Delaware")
new_feats <- setdiff(FEATS, VOCAB19)
rows <- list()
for (state in STATES) {
  tr_s <- which(F$st == state & F$is_tr); te_s <- which(F$st == state & F$yr == TEST_YEAR)
  trs <- F$adf[tr_s, , drop = FALSE]; tess <- F$adf[te_s, , drop = FALSE]
  err_te <- F$ie[te_s]; doll_te <- F$ed[te_s]
  idx_tr <- flags_for_rules(vis, trs, strata_of(F$hh[tr_s]), label = "")
  idx_te <- flags_for_rules(vis, tess, strata_of(F$hh[te_s]), label = "")
  np <- !pile_all[te_s]; pos <- cumsum(np)
  idx_np <- lapply(idx_te, function(v) pos[v[np[v]]])
  for (b in BUDGETS) {
    w <- walk_fill(idx_tr, nrow(trs), b); sel <- c(w$frozen, w$buffer)
    s22 <- score_2024(sel, idx_te, nrow(tess), b, err_te, doll_te)
    snp <- score_2024(sel, idx_np, sum(np), b, err_te[np], doll_te[np])
    rows[[length(rows) + 1]] <- data.frame(
      arm = TAG, seed = SEED_ARM, mtry = MTRY_ARM, state = state, budget = b, n_visible = nrow(vis), n_tagged = N_TAGGED, n_pile_tagged = N_PILE,
      n_core = length(w$frozen), n_buffer = length(w$buffer), deepest_rank = max(sel, 0L),
      n_sel_regime = sum(nzchar(vis$regime[sel])), n_sel_regime_below = sum(vis$lcb_below_regime_rate[sel]),
      n_sel_new = sum(Reduce(`|`, lapply(new_feats, function(f) uses(vis$rule[sel], f)), FALSE)),
      n_te = nrow(tess), n_flagged = s22[["n_flagged"]], n_errors_caught = s22[["n_errors"]],
      precision = round(s22[["precision"]], 4), base_rate_te = round(mean(err_te), 4),
      dollar_recall = round(s22[["dollar_recall"]], 4),
      n_te_np = sum(np), n_flagged_np = snp[["n_flagged"]], n_errors_np = snp[["n_errors"]],
      precision_np = round(snp[["precision"]], 4), dollar_recall_np = round(snp[["dollar_recall"]], 4),
      fill_gap_core = w$gap_core, fill_gap_total = w$gap_total)
  }
  rm(idx_tr, idx_te, idx_np); invisible(gc())
}
res <- bind_rows(rows)
fn_st <- file.path(OUT_DIR, sprintf("states_%s.csv", TAG))
write.csv(res, paste0(fn_st, ".tmp"), row.names = FALSE); stopifnot(file.rename(paste0(fn_st, ".tmp"), fn_st))
for (b in BUDGETS) { x <- res[res$budget == b, ]
  stamp("arm %s | budget %.0f%%: %d states | pooled precision %.4f (%d of %d), non-pile %.4f (%d of %d) | median %.4f | deepest rank %d | selected rules regime-limited %d (below own regime %d), using a new feature %d",
        TAG, 100 * b, nrow(x), sum(x$n_errors_caught) / sum(x$n_flagged), sum(x$n_errors_caught), sum(x$n_flagged),
        sum(x$n_errors_np) / sum(x$n_flagged_np), sum(x$n_errors_np), sum(x$n_flagged_np),
        median(x$precision), max(x$deepest_rank), sum(x$n_sel_regime), sum(x$n_sel_regime_below), sum(x$n_sel_new)) }
stamp("arm %s done", TAG)
