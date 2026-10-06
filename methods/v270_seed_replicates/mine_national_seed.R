# v2.7 seed-replicate study, step 1: one national FY2022-23 mine on one frame at
# one seed, scored on its training rows and admitted with the shipped recipe.
# The mining block is the benchmark's national block (its lines 291-320) with
# SEED and the frame taken from the environment. See design_note.md.
#   SR_FRAME=final SR_SEED=118 Rscript methods/v270_seed_replicates/mine_national_seed.R
# Writes pools/raw_<frame>_seed<seed>.rds (every candidate with train n, k and
# the artifact counts) and pools/admitted_<frame>_seed<seed>.rds (the admitted
# pool in the benchmark's cache format). An existing admitted file is kept.
source("methods/v270_seed_replicates/common.R")
FR <- Sys.getenv("SR_FRAME"); SEED <- as.integer(Sys.getenv("SR_SEED"))
stopifnot(FR %in% names(FRAMES), !is.na(SEED))
fn_adm <- pool_file(FR, SEED, "admitted"); fn_raw <- pool_file(FR, SEED, "raw")
if (file.exists(fn_adm)) {   # an admitted pool on disk is kept (night 3 reuses two cached benchmark pools, which have no raw file)
  stamp("%s seed %d: pool already on disk, nothing to mine", FR, SEED)
  quit(save = "no", status = 0)
}
F <- load_frame(FR)
tr_rows <- which(F$is_tr)
trn <- F$adf[tr_rows, , drop = FALSE]
ie_tr <- F$ie[tr_rows]; mm_tr <- F$mm[tr_rows]
strata_tr_nat <- strata_of(F$hh[tr_rows])
stamp("%s seed %d%s: mining the national pool (FY%s, %d rows, %d errors) ...",
      FR, SEED, if (SMOKE) " [SMOKE]" else "", paste(TRAIN_YEARS, collapse = "+"), nrow(trn), sum(ie_tr))
rdf <- mine_rule_vocabulary(
  trn, list(any_error = list(rows = seq_len(nrow(trn)), ie = ie_tr)),
  strata_tr_nat, VOCAB19, xgb = XGB, rf = RF,
  signif_digits = SIGNIF_DIGITS, seed = SEED, verbose = TRUE,
  binary_features = BINARY_FEATURES)
stamp("raw rules: %d; scoring on the training rows ...", nrow(rdf))
sc <- reduce_flags_for_rules(
  rdf, trn, strata_tr_nat,
  function(ix) c(length(ix), sum(ie_tr[ix]), sum(mm_tr[ix]), sum(mm_tr[ix] & ie_tr[ix])),
  label = sprintf("%s seed %d", FR, SEED))
base_by_hh <- vapply(strata_tr_nat, function(rows) mean(ie_tr[rows]), numeric(1))
natl <- admit_rank(rdf, sc[, 1], sc[, 2], base_by_hh[rdf$hh])
ix_adm <- match(paste(natl$hh, natl$rule), paste(rdf$hh, rdf$rule))
natl$mm_n <- sc[ix_adm, 3]; natl$mm_k <- sc[ix_adm, 4]
rdf$n <- as.integer(sc[, 1]); rdf$k <- sc[, 2]; rdf$mm_n <- sc[, 3]; rdf$mm_k <- sc[, 4]
attr(rdf, "base_by_hh") <- base_by_hh
saveRDS(rdf, fn_raw); saveRDS(natl, fn_adm)
stamp("%s seed %d: %d raw, %d admitted -> %s", FR, SEED, nrow(rdf), nrow(natl), fn_adm)

# Anchor (seed 117, full ensembles): the pool should equal the one the v2.7
# benchmark arm cached for this frame. Reported, never a stop: if the engines
# are not reproducible at a fixed seed, a same-seed re-mine is one more draw,
# and the readout says so.
# The block also runs under SMOKE (where the pools necessarily differ) so the
# code path is exercised, and inside try() so it can never fail the mine.
if (SEED == 117L && FR %in% names(BENCH_POOL)) try({
  ref <- readRDS(BENCH_POOL[[FR]])
  key <- function(x) paste(x$hh, x$rule, x$n, x$k, x$mm_n, x$mm_k)
  same <- nrow(ref) == nrow(natl) && identical(key(ref), key(natl)) &&
    isTRUE(all.equal(ref$lcb, natl$lcb))
  shared <- mean(paste(natl$hh, natl$rule) %in% paste(ref$hh, ref$rule))
  stamp("ANCHOR %s seed 117 vs the benchmark's cached pool: %s (%d vs %d rules; %.1f%% of this pool's rules are in the cached one)%s",
        FR, if (same) "IDENTICAL" else "DIFFERS", nrow(natl), nrow(ref), 100 * shared,
        if (SMOKE) " [SMOKE: expected to differ]" else "")
  writeLines(sprintf("%s,%s,%d,%d,%.4f", FR, if (same) "identical" else "differs",
                     nrow(natl), nrow(ref), shared),
             file.path(OUT_DIR, sprintf("anchor_pool_%s.csv", FR)))
}, silent = FALSE)
