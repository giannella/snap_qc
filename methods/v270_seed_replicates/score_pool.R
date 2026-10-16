# v2.7 seed-replicate study, step 2: score one admitted national pool on FY2024.
#   (a) the top RULE_TOP visible rules on all FY2024 rows (flags, errors, error
#       dollars), for the rule-level readout;
#   (b) the 49-state readout: the national pool alone, walked on each state's
#       FY2022-23 rows with the shipped fill (fresh-share 0.50, buffer to 3x)
#       and scored on the state's FY2024 rows at the 5% and 10% budgets, with
#       the benchmark's own walk_fill() and score_2024(). The walk runs over
#       the FULL visible pool, as the benchmark's does: no ranked window, so no
#       window certificate is needed (ledger, evaluation machinery: "never cap
#       the pool at a fixed rank as policy").
# No state pools: the question is about the national mine. Results therefore do
# not carry to the blended benchmark's numbers.
#   SR_FRAME=final SR_SEED=118 Rscript methods/v270_seed_replicates/score_pool.R
# SR_POOL=<path> scores another admitted pool file (the three-seed pooled pool,
# or the benchmark's cached seed-117 pool for the anchor); SR_TAG names its
# outputs; SR_STATES limits the states (comma-separated); SR_SKIP_RULES=1 skips (a).
source("methods/v270_seed_replicates/common.R")
FR <- Sys.getenv("SR_FRAME"); SEED <- suppressWarnings(as.integer(Sys.getenv("SR_SEED")))
stopifnot(FR %in% names(FRAMES))
POOL <- Sys.getenv("SR_POOL", if (is.na(SEED)) "" else pool_file(FR, SEED, "admitted"))
TAG <- Sys.getenv("SR_TAG", sprintf("%s_seed%d", FR, SEED))
RULE_TOP <- 10000L        # the rule-level readout uses the top 200 / 1,000 / 3,000
stopifnot(file.exists(POOL))
F <- load_frame(FR)
pool <- tag_artifacts(readRDS(POOL))
vis <- pool[!pool$artifact_i, , drop = FALSE]          # as the benchmark: tagged rules dropped before the walk
stopifnot(!is.unsorted(-vis$lcb))
N_TAGGED <- sum(pool$artifact_i)
stamp("%s: pool %s | %d admitted, %d tagged and dropped, %d visible", TAG, POOL, nrow(pool), N_TAGGED, nrow(vis))

## (a) rule level, all FY2024 rows
if (!identical(Sys.getenv("SR_SKIP_RULES"), "1")) {
  te <- which(F$yr == TEST_YEAR); tes <- F$adf[te, , drop = FALSE]
  ie_te <- F$ie[te]; ed_te <- F$ed[te]
  rl <- vis[seq_len(min(RULE_TOP, nrow(vis))), c("hh", "rule", "n", "k", "lcb")]
  sc <- reduce_flags_for_rules(rl, tes, strata_of(F$hh[te]),
                               function(ix) c(length(ix), sum(ie_te[ix]), sum(ed_te[ix])), label = TAG)
  rl$n_te <- sc[, 1]; rl$k_te <- sc[, 2]; rl$d_te <- sc[, 3]
  saveRDS(list(tag = TAG, frame = FR, seed = SEED, rules = rl,
               tr_strata_n = vapply(strata_of(F$hh[F$is_tr]), length, integer(1)),
               te_strata_n = vapply(strata_of(F$hh[te]), length, integer(1))),
          file.path(OUT_DIR, sprintf("rules_%s.rds", TAG)))
  stamp("%s: rule-level scores saved (top %d rules)", TAG, nrow(rl))
  rm(tes, sc, rl); invisible(gc())
}

## (b) 49-state walk, national pool only, full pool
STATES <- sort(unique(F$st))
if (nzchar(Sys.getenv("SR_STATES"))) STATES <- intersect(STATES, trimws(strsplit(Sys.getenv("SR_STATES"), ",")[[1]]))
if (SMOKE && !nzchar(Sys.getenv("SR_STATES"))) STATES <- c("Washington", "Maine", "Mississippi", "Delaware")
rows <- list()
for (state in STATES) {
  tr_s <- which(F$st == state & F$is_tr); te_s <- which(F$st == state & F$yr == TEST_YEAR)
  trs <- F$adf[tr_s, , drop = FALSE]; tess <- F$adf[te_s, , drop = FALSE]
  err_te <- F$ie[te_s]; doll_te <- F$ed[te_s]
  idx_tr <- flags_for_rules(vis, trs, strata_of(F$hh[tr_s]), label = "")
  idx_te <- flags_for_rules(vis, tess, strata_of(F$hh[te_s]), label = "")
  for (b in BUDGETS) {
    w <- walk_fill(idx_tr, nrow(trs), b); sel <- c(w$frozen, w$buffer)
    s24 <- score_2024(sel, idx_te, nrow(tess), b, err_te, doll_te)
    rows[[length(rows) + 1]] <- data.frame(
      tag = TAG, frame = FR, seed = SEED, state = state, budget = b,
      n_visible = nrow(vis), n_tagged = N_TAGGED,
      n_core = length(w$frozen), n_buffer = length(w$buffer), deepest_rank = max(sel, 0L),
      n_te = nrow(tess), n_flagged = s24[["n_flagged"]], n_errors_caught = s24[["n_errors"]],
      precision = round(s24[["precision"]], 4), base_rate_te = round(mean(err_te), 4),
      dollar_recall = round(s24[["dollar_recall"]], 4),
      fill_gap_core = w$gap_core, fill_gap_total = w$gap_total)
  }
  rm(idx_tr, idx_te); invisible(gc())
}
res <- bind_rows(rows)
write.csv(res, file.path(OUT_DIR, sprintf("states_%s.csv", TAG)), row.names = FALSE)
for (b in BUDGETS) { x <- res[res$budget == b, ]
  stamp("%s | budget %.0f%%: %d states | pooled precision %.4f (%d of %d) | median %.4f | deepest rank used %d | cells with a fill gap %d",
        TAG, 100 * b, nrow(x), sum(x$n_errors_caught) / sum(x$n_flagged), sum(x$n_errors_caught), sum(x$n_flagged),
        median(x$precision), max(x$deepest_rank), sum(x$fill_gap_core > 0 | x$fill_gap_total > 0)) }

## anchor: a state whose own pool is empty in the benchmark gets the same list from the national pool alone
if (nzchar(Sys.getenv("SR_ANCHOR_CSV"))) {
  ref <- read.csv(Sys.getenv("SR_ANCHOR_CSV")); ref <- ref[ref$n_state_pool == 0, ]
  m <- merge(res, ref, by = c("state", "budget"), suffixes = c("", "_ref"))
  ok <- with(m, n_core == n_core_ref & n_buffer == n_buffer_ref & n_flagged == n_flagged_ref &
               n_errors_caught == n_errors_caught_ref & abs(dollar_recall - dollar_recall_ref) < 1e-4)
  stamp("ANCHOR vs the benchmark on states with an empty own pool: %d of %d state-budget cells identical (%s)",
        sum(ok), nrow(m), paste(unique(m$state), collapse = ", "))
  print(m[, c("state", "budget", "n_core", "n_core_ref", "n_buffer", "n_buffer_ref", "n_flagged", "n_flagged_ref", "n_errors_caught", "n_errors_caught_ref")])
  stopifnot(nrow(m) > 0, all(ok))
}
stamp("%s done", TAG)
