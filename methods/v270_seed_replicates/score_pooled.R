# v2.7 seed-replicate study, step 3: on one frame, the top of the ranking from
# one seed's candidates, from two seeds' candidates pooled, and from all three.
# Each arm's candidates (raw rules with train n and k) get the shipped admission
# in ONE BH pass over that arm's candidates (so the bar adapts to the larger
# search) and the 99% Wilson LCB ordering; then the arm is scored on all FY2024
# rows. Also writes the three-seed pooled admitted pool, which score_pool.R
# walks state by state.
#   SR_FRAME=final Rscript methods/v270_seed_replicates/score_pooled.R
# The matched-share curve (precision when the union first reaches a share of
# FY2024 cases) is the like-for-like comparison across arms. The fixed top-K
# table is NOT like-for-like: K rules reach less deep into a pooled ranking.
source("methods/v270_seed_replicates/common.R")
FR <- Sys.getenv("SR_FRAME"); stopifnot(FR %in% names(FRAMES))
CHUNK <- if (SMOKE) 1000L else 4000L      # rules scored per pass, until every share line is reached
SHARES <- c(.01, .02, .03, .04, .05, .06, .08, .10, .12, .15)
F <- load_frame(FR)
tr <- which(F$is_tr); te <- which(F$yr == TEST_YEAR)
tes <- F$adf[te, , drop = FALSE]; ie_te <- F$ie[te]; ed_te <- F$ed[te]; NT <- length(te)
S_te <- strata_of(F$hh[te])
base_by_hh <- vapply(strata_of(F$hh[tr]), function(r) mean(F$ie[tr][r]), numeric(1))
RAW <- lapply(setNames(SEEDS, paste0("s", SEEDS)), function(s) readRDS(pool_file(FR, s, "raw")))
ARMS <- c(as.list(setNames(names(RAW), names(RAW))),
          lapply(setNames(combn(names(RAW), 2, simplify = FALSE), combn(names(RAW), 2, paste, collapse = "+")), identity),
          list(pooled3 = names(RAW)))
curve <- list(); cal <- list(); MASK <- list()
for (a in names(ARMS)) {
  cand <- bind_rows(RAW[ARMS[[a]]])
  cand <- cand[!duplicated(cand[, c("hh", "rule")]), ]
  cand <- cand[order(cand$hh, cand$rule, method = "radix"), ]
  adm <- tag_artifacts(admit_rank(cand, cand$n, cand$k, base_by_hh[cand$hh]))
  if (length(ARMS[[a]]) == 1L) {    # a single seed must re-admit exactly its own admitted pool, in the same order
    own <- readRDS(pool_file(FR, as.integer(sub("s", "", a)), "admitted"))
    stopifnot(identical(paste(adm$hh, adm$rule, adm$n, adm$k), paste(own$hh, own$rule, own$n, own$k)))
  }
  if (a == "pooled3") saveRDS(adm, file.path(POOL_DIR, sprintf("admitted_%s_pooled3.rds", FR)))
  vis <- adm[!adm$artifact_i, , drop = FALSE]
  un <- logical(NT); j <- 1L; done <- 0L; first <- NULL
  while (j <= length(SHARES) && done < nrow(vis)) {
    rows <- (done + 1L):min(done + CHUNK, nrow(vis))
    top <- vis[rows, , drop = FALSE]
    idx <- flags_for_rules(top, tes, S_te, label = "")
    if (is.null(first)) { first <- top; first$n_te <- lengths(idx); first$k_te <- vapply(idx, function(ix) sum(ie_te[ix]), numeric(1)) }
    for (i in seq_along(idx)) {
      un[idx[[i]]] <- TRUE
      while (j <= length(SHARES) && sum(un) >= SHARES[j] * NT) {
        curve[[length(curve) + 1]] <- data.frame(
          frame = FR, arm = a, seeds = length(ARMS[[a]]), candidates = nrow(cand), admitted = nrow(adm), tagged = sum(adm$artifact_i),
          share = SHARES[j], rules = rows[i], flagged = sum(un), errors = sum(ie_te[un]),
          precision = sum(ie_te[un]) / sum(un), recall = sum(ie_te[un]) / sum(ie_te),
          dollar_recall = sum(ed_te[un]) / sum(ed_te))
        MASK[[paste(a, SHARES[j])]] <- un & ie_te
        j <- j + 1L
      }
      if (j > length(SHARES)) break
    }
    done <- max(rows); rm(idx)
  }
  stopifnot(j > length(SHARES))      # every arm reaches every share line; a pool too small to flag 15% would be a different study
  for (K in c(200L, 1000L)) {
    q <- first[seq_len(min(K, nrow(first))), ]; ok <- q$n_te >= 10
    cal[[length(cal) + 1]] <- data.frame(
      frame = FR, arm = a, seeds = length(ARMS[[a]]), topK = K, median_train_n = median(q$n), share_train_n_lt50 = mean(q$n < 50),
      median_train_precision = median(q$k / q$n), median_lcb = median(q$lcb),
      flagwt_test_precision = sum(q$k_te) / sum(q$n_te), median_test_precision = median((q$k_te / q$n_te)[ok]),
      share_under_bound = mean((q$k_te / q$n_te)[ok] < q$lcb[ok]), median_margin = median((q$k_te / q$n_te)[ok] - q$lcb[ok]),
      share_lt10_test_flags = mean(!ok))
  }
  stamp("%s arm %s: %d candidates, %d admitted, %d visible; %d rules scored", FR, a, nrow(cand), nrow(adm), nrow(vis), done)
}
jac <- function(x, y) sum(x & y) / sum(x | y)
arms <- names(ARMS); jr <- list()
for (s in c(0.05, 0.10)) for (p in combn(arms, 2, simplify = FALSE)) {
  x <- MASK[[paste(p[1], s)]]; y <- MASK[[paste(p[2], s)]]
  jr[[length(jr) + 1]] <- data.frame(frame = FR, share = s, arm_a = p[1], arm_b = p[2], jaccard_errors_caught = jac(x, y),
                                     shared_seeds = length(intersect(ARMS[[p[1]]], ARMS[[p[2]]])))
}
write.csv(bind_rows(curve), file.path(OUT_DIR, sprintf("pooled_curve_%s.csv", FR)), row.names = FALSE)
write.csv(bind_rows(cal), file.path(OUT_DIR, sprintf("pooled_calibration_%s.csv", FR)), row.names = FALSE)
write.csv(bind_rows(jr), file.path(OUT_DIR, sprintf("pooled_jaccard_%s.csv", FR)), row.names = FALSE)
stamp("%s pooled arms done", FR)
