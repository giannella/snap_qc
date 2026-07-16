# Layers 1 + 3 audition (design_selection_layers_v3.md): empirical-Bayes
# ranking and FDR admission, on the train2223/test24 benchmark. EXPLORATORY
# (scored on 2024); adoption requires the honest designs.
#
# Grid per vocabulary arm (orig / all5, from cached pools):
#   rank_stat:  lcb   (Wilson LCB, z = 2.326 -- the production baseline)
#               ebm   (beta-binomial posterior mean, prior fit per stratum)
#               ebq05 (posterior 5% quantile)
#   admission:  floor (support >= 30 only -- baseline)
#               fdr10, fdr05 (Benjamini-Hochberg vs the pool stratum base
#               rate at alpha = .10 / .05, on top of the support floor)
#   collapse:   JACCARD_SET (1 = off, plus the layer-2 winner; set after the
#               neardup_collapse_sweep_v2.R results)
#
# Expects `reg_model_data`. Output:
#   methods/state_similarity_v2/transfer_benchmark_train2223_test24/
#     estimation_admission_sweep.csv

suppressMessages(library(dplyr))
source("rule_mining_helpers.R")

TARGETS <- c("Massachusetts", "Michigan", "North Carolina", "Connecticut",
             "Arizona", "Washington",
             "Louisiana", "Virginia", "California", "Texas",
             "Mississippi", "New Jersey", "Colorado",
             "Maine", "Maryland", "Missouri",
             "District of Columbia", "Tennessee")
BUDGETS <- c(0.05, 0.10)
BUFFER_MULT <- 3
LCB_Z <- 2.326
N_FLOOR <- 30
K_TOP <- 3000
if (!exists("JACCARD_SET")) JACCARD_SET <- c(1, 0.9)  # override in runner
TRAIN_YEARS <- c("2022", "2023")
TEST_YEAR   <- "2024"

out_dir  <- "methods/state_similarity_v2/transfer_benchmark_train2223_test24"
CACHE5   <- file.path(out_dir, "pool_cache_5frames")
CACHEOLD <- file.path(out_dir, "pool_cache")

features <- c(
  "HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size",
  "expedited_i", "cat_elig", "rawben_rel_max", "medical_deductions",
  "shelter_expenses_by_hh_size", "utilities", "married", "homeless",
  "rawearn_by_hh_size", "rawunearn_by_hh_size", "rawgross_by_hh_size",
  "percent_abawd", "unc_rawben_rel_max",
  "months_since_cert_n", "count_divisible_by_100"
)
HH_LEVELS <- c("1", "2-3", "4+")
hh_group_of <- function(n) {
  n <- suppressWarnings(as.numeric(as.character(n)))
  ifelse(is.na(n), NA_character_, ifelse(n <= 1, "1", ifelse(n <= 3, "2-3", "4+")))
}
targets_of <- function(df) {
  ie <- !is.na(df$over_threshold) & df$over_threshold != 0
  amt <- df$total_error_amount; amt[is.na(amt)] <- 0
  list(ie = ie, ed = ifelse(ie, abs(amt), 0))
}

adf <- prep_features(reg_model_data %>%
                       filter(fiscal_year %in% c(TRAIN_YEARS, TEST_YEAR)),
                     features)$data
yr <- as.character(adf$fiscal_year)
st <- as.character(adf$state)
in_tr <- yr %in% TRAIN_YEARS

sig <- gsub("[^A-Za-z]", "", paste(sort(unique(st)), collapse = "_"))
nat_key_old <- if (nchar(sig) <= 80) sig else
  sprintf("%s_%08x", substr(sig, 1, 60),
          sum(utf8ToInt(sig) * seq_along(utf8ToInt(sig))) %% .Machine$integer.max)

natl5   <- readRDS(file.path(CACHE5, "pool_national.rds"))
natlold <- readRDS(file.path(CACHEOLD, sprintf("pool_%s.rds", nat_key_old)))
natlold$mined_frames <- "any_error"

# pool-level stratum base rates on the pool's own training caseload
base_rates <- function(pool_states) {
  rows <- st %in% pool_states & in_tr
  ie <- !is.na(adf$over_threshold[rows]) & adf$over_threshold[rows] != 0
  hh <- hh_group_of(adf$cert_HH_size_FS_n[rows])
  vapply(setNames(nm = HH_LEVELS), function(h) mean(ie[hh %in% h]), numeric(1))
}
base_nat <- base_rates(sort(unique(st)))

# beta-binomial ML fit on (k, n) -- one prior per stratum
fit_bb <- function(k, n) {
  ll <- function(par) {
    a <- exp(par[1]); b <- exp(par[2])
    -sum(lbeta(a + k, b + n - k) - lbeta(a, b))
  }
  p0 <- mean(k / n); v0 <- max(var(k / n), 1e-4)
  m0 <- max(p0 * (1 - p0) / v0 - 1, 2)
  o <- optim(log(c(p0 * m0, (1 - p0) * m0)), ll, method = "Nelder-Mead")
  exp(o$par)
}

eb_stats <- function(pool) {
  k <- round(pool$precision_train * pool$n_flagged_train)
  n <- pool$n_flagged_train
  ebm <- ebq <- numeric(nrow(pool))
  for (h in HH_LEVELS) {
    ix <- which(pool$hh == h)
    if (length(ix) < 50) { ebm[ix] <- k[ix] / n[ix]; ebq[ix] <- k[ix] / n[ix]; next }
    ab <- fit_bb(k[ix], n[ix])
    ebm[ix] <- (ab[1] + k[ix]) / (ab[1] + ab[2] + n[ix])
    ebq[ix] <- qbeta(0.05, ab[1] + k[ix], ab[2] + n[ix] - k[ix])
    cat(sprintf("    prior hh %s: Beta(%.2f, %.2f) mean %.3f\n",
                h, ab[1], ab[2], ab[1] / sum(ab)))
  }
  list(ebm = ebm, ebq05 = ebq)
}

# BH admission vs the pool's stratum base rate (one-sided binomial p-values)
fdr_admit <- function(pool, base_by_hh, alpha) {
  k <- round(pool$precision_train * pool$n_flagged_train)
  n <- pool$n_flagged_train
  p0 <- base_by_hh[pool$hh]
  pv <- pbinom(k - 1, n, p0, lower.tail = FALSE)
  m <- length(pv)
  o <- order(pv)
  thr <- max(c(0L, which(pv[o] <= alpha * seq_len(m) / m)))
  admit <- rep(FALSE, m)
  if (thr > 0) admit[o[seq_len(thr)]] <- TRUE
  admit
}

neardup_drop <- function(pool, idx_tr, ord, jac) {
  if (jac >= 1) return(integer(0))
  top <- ord[seq_len(min(K_TOP, length(ord)))]
  drop <- integer(0)
  for (h in unique(pool$hh[top])) {
    cand <- top[pool$hh[top] == h]
    sz <- lengths(idx_tr[cand])
    cand <- cand[sz > 0]; sz <- sz[sz > 0]
    m <- length(cand)
    if (m < 2) next
    o <- order(sz); cand <- cand[o]; sz <- sz[o]
    uf <- seq_len(m)
    find <- function(x) { while (uf[x] != x) { uf[x] <<- uf[uf[x]]; x <- uf[x] }; x }
    for (i in seq_len(m - 1)) {
      si <- sz[i]; ii <- idx_tr[[cand[i]]]
      for (j in (i + 1):m) {
        if (sz[j] * jac > si) break
        inter <- length(intersect(ii, idx_tr[[cand[j]]]))
        if (inter / (si + sz[j] - inter) >= jac) {
          ri <- find(i); rj <- find(j)
          if (ri != rj) uf[ri] <- rj
        }
      }
    }
    roots <- vapply(seq_len(m), find, integer(1))
    for (r in unique(roots)) {
      mem <- cand[roots == r]
      if (length(mem) < 2) next
      keep <- mem[order(-pool$n_flagged_train[mem], -pool$stat[mem], pool$rule[mem])][1]
      drop <- c(drop, setdiff(mem, keep))
    }
  }
  drop
}

walk_eval <- function(pool, idx_tr, idx_te, n_tr_rows, n_te_rows, tg_te, b) {
  cap <- floor(b * n_tr_rows); cap_buf <- floor(BUFFER_MULT * b * n_tr_rows)
  un <- rep(FALSE, n_tr_rows); n_in <- 0L
  frozen <- integer(0); buffer <- integer(0)
  ord <- order(-pool$stat)
  for (i in ord) {
    add <- sum(!un[idx_tr[[i]]])
    if (add == 0) next
    if (n_in + add <= cap) {
      un[idx_tr[[i]]] <- TRUE; n_in <- n_in + add; frozen <- c(frozen, i)
    } else if (n_in + add <= cap_buf) {
      un[idx_tr[[i]]] <- TRUE; n_in <- n_in + add; buffer <- c(buffer, i)
    }
  }
  cap24 <- floor(b * n_te_rows)
  un24 <- rep(FALSE, n_te_rows); used <- 0L
  for (i in c(frozen, buffer)) {
    add <- sum(!un24[idx_te[[i]]])
    if (add > 0 && sum(un24) + add <= cap24) {
      un24[idx_te[[i]]] <- TRUE; used <- used + 1L
    }
  }
  nb <- sum(un24)
  data.frame(n_deployed = used, workload = round(nb / n_te_rows, 4),
             precision = round(ifelse(nb > 0, sum(tg_te$ie[un24]) / nb, NA), 4),
             dollar_recall = round(sum(tg_te$ed[un24]) / sum(tg_te$ed), 4))
}

res <- list()
for (target in TARGETS) {
  own5   <- readRDS(file.path(CACHE5, sprintf("pool_%s.rds", gsub("[^A-Za-z]", "", target))))
  ownold <- readRDS(file.path(CACHEOLD, sprintf("pool_%s.rds", gsub("[^A-Za-z]", "", target))))
  ownold$mined_frames <- "any_error"
  base_own <- base_rates(target)
  cols <- c("hh", "rule", "mined_frames", "n_flagged_train", "precision_train")
  arms <- list(
    orig = list(natl = natlold[, cols], own = ownold[, cols]),
    all5 = list(natl = natl5[, cols],   own = own5[, cols])
  )

  tr <- adf[st == target & in_tr, , drop = FALSE]
  te <- adf[st == target & yr == TEST_YEAR, , drop = FALSE]
  tg_te <- targets_of(te)
  strata_tr <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(tr$cert_HH_size_FS_n) %in% h))
  strata_te <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(te$cert_HH_size_FS_n) %in% h))

  for (arm in names(arms)) {
    a <- arms[[arm]]
    # per-pool stats BEFORE blending (priors and FDR are pool-level decisions)
    prep_pool <- function(p, base_by_hh, lbl) {
      p <- p[p$n_flagged_train >= N_FLOOR, , drop = FALSE]
      k <- round(p$precision_train * p$n_flagged_train)
      p$lcb <- wilson_lcb(k, p$n_flagged_train, LCB_Z)
      cat(sprintf("  [%s %s] priors:\n", arm, lbl))
      eb <- eb_stats(p)
      p$ebm <- eb$ebm; p$ebq05 <- eb$ebq05
      p$adm_fdr10 <- fdr_admit(p, base_by_hh, 0.10)
      p$adm_fdr05 <- fdr_admit(p, base_by_hh, 0.05)
      p
    }
    natp <- prep_pool(a$natl, base_nat, "national")
    ownp <- prep_pool(a$own, base_own, "state")
    pool <- bind_rows(natp, ownp)
    idx_tr <- flags_for_rules(pool, tr, strata_tr, label = "")
    idx_te <- flags_for_rules(pool, te, strata_te, label = "")

    for (rk in c("lcb", "ebm", "ebq05")) {
      for (adm in c("floor", "fdr10", "fdr05")) {
        ok0 <- if (adm == "floor") rep(TRUE, nrow(pool)) else pool[[paste0("adm_", adm)]]
        p <- pool[ok0, , drop = FALSE]
        p$stat <- p[[rk]]
        k0 <- which(ok0)
        # blend dedup on the active stat
        o <- order(-p$stat)
        dup <- duplicated(paste(p$hh[o], p$rule[o], sep = "\r"))
        sel <- o[!dup]
        p2 <- p[sel, , drop = FALSE]; k2 <- k0[sel]
        itr <- idx_tr[k2]; ite <- idx_te[k2]
        ord2 <- order(-p2$stat)
        for (jac in JACCARD_SET) {
          dr <- neardup_drop(p2, itr, ord2, jac)
          ok <- setdiff(seq_len(nrow(p2)), dr)
          p3 <- p2[ok, , drop = FALSE]
          for (b in BUDGETS) {
            ev <- walk_eval(p3, itr[ok], ite[ok], nrow(tr), nrow(te), tg_te, b)
            res[[length(res) + 1]] <- cbind(
              data.frame(target = target, arm = arm, rank_stat = rk,
                         admission = adm, jaccard = jac, budget = b,
                         n_admitted = nrow(p3),
                         target_base_rate = round(mean(tg_te$ie), 4)), ev)
          }
        }
      }
    }
    cat(sprintf("%-22s %s done\n", target, arm))
  }
  saveRDS(bind_rows(res), file.path(out_dir, "estimation_admission_partial.rds"))
}
out <- bind_rows(res)
write.csv(out, file.path(out_dir, "estimation_admission_sweep.csv"), row.names = FALSE)
cat(sprintf("wrote %s (%d rows)\n",
            file.path(out_dir, "estimation_admission_sweep.csv"), nrow(out)))

cat("\nmedians (precision / dollar recall):\n")
sm <- out %>% group_by(arm, rank_stat, admission, jaccard, budget) %>%
  summarise(med_prec = median(precision, na.rm = TRUE),
            med_dollars = median(dollar_recall, na.rm = TRUE), .groups = "drop") %>%
  arrange(budget, desc(med_prec))
print(as.data.frame(sm), row.names = FALSE)
