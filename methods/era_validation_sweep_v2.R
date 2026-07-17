# Era validation, step 2: the pre-registered sweep (train 2017-18, test 2019).
# See methods/preregistration_era_validation_2026-07.md for arms, formulas,
# expectations, and decision rules — all fixed before this run.
#
# Comparisons (factorial main effects, not a full cross):
#   admission  {prod, fdr10f}          x ordering lcb z=2.326
#   ordering   admission=prod          x {z1.645, z2.326, z2.576, z3.09, zN, famEB}
#   dollar     admission=prod          x {lcb99, dpf, dpflb}, judged on dollars
#   xfit       national-only lists     x {xfit half-B lcb, full-mine lcb}
#              (skipped with a message if raw_xfit_national.rds is absent)
#
# Output (long format, one row per state x comparison x arm x budget —
# feeds visualize_era_validation_v2.R):
#   methods/state_similarity_v2/era_validation_train1718_test19/
#     era_validation_results.csv

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
TRAIN_YEARS <- c("2017", "2018")
TEST_YEAR   <- "2019"
Z_LADDER <- c(z1.645 = 1.645, z2.326 = 2.326, z2.576 = 2.576, z3.09 = 3.09)
ZN_ALPHA0 <- 0.01; ZN_N0 <- 48429   # anchored per the pre-registration
K_TOP <- 3000; JAC <- 0.95          # famEB family window (option C machinery)

ERA_DIR <- "methods/state_similarity_v2/era_validation_train1718_test19"
RAWDIR  <- file.path(ERA_DIR, "raw_vocab")

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

adf <- prep_features(reg_model_data %>%
                       filter(fiscal_year %in% c(TRAIN_YEARS, TEST_YEAR)),
                     features)$data
yr <- as.character(adf$fiscal_year)
st <- as.character(adf$state)
in_tr <- yr %in% TRAIN_YEARS
ie_all <- !is.na(adf$over_threshold) & adf$over_threshold != 0
ed_all <- ifelse(ie_all, abs(ifelse(is.na(adf$total_error_amount), 0,
                                    adf$total_error_amount)), 0)

bh_admit <- function(k, n, p0, alpha) {
  pv <- pbinom(k - 1, n, p0, lower.tail = FALSE)
  m <- length(pv); o <- order(pv)
  thr <- max(c(0L, which(pv[o] <= alpha * seq_len(m) / m)))
  admit <- rep(FALSE, m)
  if (thr > 0) admit[o[seq_len(thr)]] <- TRUE
  admit
}

load_raw <- function(key) {
  r <- readRDS(file.path(RAWDIR, sprintf("raw_%s.rds", key)))
  base <- attr(r, "base_rates")
  r$raw_prec <- ifelse(r$n > 0, r$k / r$n, NA_real_)
  r$base <- base[r$hh]
  r$adm_prod   <- !is.na(r$raw_prec) & r$n >= 30 & r$raw_prec >= 0.05 & r$raw_prec > r$base
  r$adm_fdr10f <- bh_admit(r$k, r$n, r$base, 0.10) & r$n >= 30
  r
}

fit_bb <- function(k, n) {
  ll <- function(par) {
    a <- exp(par[1]); b <- exp(par[2])
    -sum(lbeta(a + k, b + n - k) - lbeta(a, b))
  }
  p0 <- mean(k / n); v0 <- max(var(k / n), 1e-4)
  m0 <- max(p0 * (1 - p0) / v0 - 1, 2)
  exp(optim(log(c(p0 * m0, (1 - p0) * m0)), ll, method = "Nelder-Mead")$par)
}

neardup_drop <- function(pool, idx_tr, ord, jac = JAC, k_top = K_TOP) {
  top <- ord[seq_len(min(k_top, length(ord)))]
  drop <- integer(0)
  for (h in unique(pool$hh[top])) {
    cand <- top[pool$hh[top] == h]
    sz <- lengths(idx_tr[cand]); cand <- cand[sz > 0]; sz <- sz[sz > 0]
    m <- length(cand); if (m < 2) next
    o <- order(sz); cand <- cand[o]; sz <- sz[o]
    uf <- seq_len(m)
    find <- function(x) { while (uf[x] != x) { uf[x] <<- uf[uf[x]]; x <- uf[x] }; x }
    for (i in seq_len(m - 1)) {
      si <- sz[i]; ii <- idx_tr[[cand[i]]]
      for (j in (i + 1):m) {
        if (sz[j] * jac > si) break
        inter <- length(intersect(ii, idx_tr[[cand[j]]]))
        if (inter / (si + sz[j] - inter) >= jac) {
          ri <- find(i); rj <- find(j); if (ri != rj) uf[ri] <- rj
        }
      }
    }
    roots <- vapply(seq_len(m), find, integer(1))
    for (r in unique(roots)) {
      mem <- cand[roots == r]; if (length(mem) < 2) next
      keep <- mem[order(-pool$n[mem], pool$rule[mem])][1]
      drop <- c(drop, setdiff(mem, keep))
    }
  }
  drop
}

walk_eval <- function(stat, idx_tr, idx_te, n_tr_rows, n_te_rows, ie_te, ed_te, b) {
  cap <- floor(b * n_tr_rows); cap_buf <- floor(BUFFER_MULT * b * n_tr_rows)
  un <- rep(FALSE, n_tr_rows); n_in <- 0L
  frozen <- integer(0); buffer <- integer(0)
  for (i in order(-stat)) {
    if (is.na(stat[i])) next
    add <- sum(!un[idx_tr[[i]]])
    if (add == 0) next
    if (n_in + add <= cap) {
      un[idx_tr[[i]]] <- TRUE; n_in <- n_in + add; frozen <- c(frozen, i)
    } else if (n_in + add <= cap_buf) {
      un[idx_tr[[i]]] <- TRUE; n_in <- n_in + add; buffer <- c(buffer, i)
    }
  }
  cap19 <- floor(b * n_te_rows)
  un19 <- rep(FALSE, n_te_rows); used <- 0L
  for (i in c(frozen, buffer)) {
    add <- sum(!un19[idx_te[[i]]])
    if (add > 0 && sum(un19) + add <= cap19) {
      un19[idx_te[[i]]] <- TRUE; used <- used + 1L
    }
  }
  nb <- sum(un19)
  data.frame(n_deployed = used, workload = round(nb / n_te_rows, 4),
             precision = round(ifelse(nb > 0, sum(ie_te[un19]) / nb, NA), 4),
             dollar_recall = round(sum(ed_te[un19]) / sum(ed_te), 4))
}

natl <- load_raw("national")
cat(sprintf("national raw %d | prod %d | fdr10f %d\n",
            nrow(natl), sum(natl$adm_prod), sum(natl$adm_fdr10f)))

# national dollar stats (once) on prod-admitted subset
nat_rows_tr <- which(in_tr)
dstats_path <- file.path(ERA_DIR, "dstats_national.rds")
if (file.exists(dstats_path)) {
  nds <- readRDS(dstats_path)
} else {
  sub <- natl[natl$adm_prod, , drop = FALSE]
  train <- adf[nat_rows_tr, , drop = FALSE]
  strata_tr <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(train$cert_HH_size_FS_n) %in% h))
  idx <- flags_for_rules(sub, train, strata_tr, label = "national dollars")
  ed <- ed_all[nat_rows_tr]
  nds <- data.frame(rule = sub$rule, hh = sub$hh,
    dpf = vapply(idx, function(ix) sum(ed[ix]), numeric(1)) / pmax(lengths(idx), 1),
    dpflb = vapply(seq_along(idx), function(i) {
      d <- ed[idx[[i]]]; n <- length(d)
      if (n < 2) return(NA_real_)
      expm1(mean(log1p(d)) - 2.326 * sd(log1p(d)) / sqrt(n))
    }, numeric(1)))
  saveRDS(nds, dstats_path)
  rm(idx); invisible(gc())
}

xfit_path <- file.path(RAWDIR, "raw_xfit_national.rds")
HAVE_XFIT <- file.exists(xfit_path)
if (HAVE_XFIT) {
  xf <- readRDS(xfit_path)
  xf$raw_prec <- ifelse(xf$n > 0, xf$k / xf$n, NA_real_)
  xf$base <- attr(xf, "base_rates")[xf$hh]
  xf$adm <- !is.na(xf$raw_prec) & xf$n >= 30 & xf$raw_prec >= 0.05 & xf$raw_prec > xf$base
  xf <- xf[xf$adm, , drop = FALSE]
  xf$lcb <- wilson_lcb(xf$k, xf$n, 2.326)
  cat(sprintf("xfit half-B admitted: %d rules\n", nrow(xf)))
} else cat("NOTE: raw_xfit_national.rds absent - xfit comparison skipped\n")

res <- list()
add_row <- function(comparison, admission, ordering, target, budget,
                    n_admitted, ev, base) {
  res[[length(res) + 1]] <<- cbind(
    data.frame(era = "train1718_test19", comparison = comparison,
               admission = admission, ordering = ordering, target = target,
               budget = budget, n_admitted = n_admitted,
               target_base_rate = round(base, 4)), ev)
}

for (target in TARGETS) {
  own <- load_raw(gsub("[^A-Za-z]", "", target))
  # union pool for one flags pass
  keep_n <- natl$adm_prod | natl$adm_fdr10f
  keep_o <- own$adm_prod | own$adm_fdr10f
  cols <- c("rule", "hh", "n", "k", "adm_prod", "adm_fdr10f")
  pool <- bind_rows(natl[keep_n, cols], own[keep_o, cols])
  pool$lcb2326 <- wilson_lcb(pool$k, pool$n, 2.326)

  tr <- adf[st == target & in_tr, , drop = FALSE]
  te <- adf[st == target & yr == TEST_YEAR, , drop = FALSE]
  rows_te <- which(st == target & yr == TEST_YEAR)
  strata_tr <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(tr$cert_HH_size_FS_n) %in% h))
  strata_te <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(te$cert_HH_size_FS_n) %in% h))
  idx_tr <- flags_for_rules(pool, tr, strata_tr, label = "")
  idx_te <- flags_for_rules(pool, te, strata_te, label = "")
  ie_te <- ie_all[rows_te]; ed_te <- ed_all[rows_te]
  base <- mean(ie_te)

  dedup_sel <- function(sel, stat) {
    o <- order(-stat[sel])
    dup <- duplicated(paste(pool$hh[sel][o], pool$rule[sel][o], sep = "\r"))
    sel[o[!dup]]
  }

  ## admission comparison (ordering fixed at lcb z=2.326)
  for (a in c("prod", "fdr10f")) {
    sel <- dedup_sel(which(pool[[paste0("adm_", a)]]), pool$lcb2326)
    for (b in BUDGETS)
      add_row("admission", a, "lcb_z2.326", target, b, length(sel),
              walk_eval(pool$lcb2326[sel], idx_tr[sel], idx_te[sel],
                        nrow(tr), nrow(te), ie_te, ed_te, b), base)
  }

  ## ordering comparison (admission fixed at prod)
  sel0 <- dedup_sel(which(pool$adm_prod), pool$lcb2326)
  N <- length(sel0)
  for (zn in names(Z_LADDER)) {
    stat <- wilson_lcb(pool$k[sel0], pool$n[sel0], Z_LADDER[[zn]])
    for (b in BUDGETS)
      add_row("ordering", "prod", zn, target, b, N,
              walk_eval(stat, idx_tr[sel0], idx_te[sel0],
                        nrow(tr), nrow(te), ie_te, ed_te, b), base)
  }
  zN <- qnorm(1 - pmin(0.5, ZN_ALPHA0 * ZN_N0 / N))
  statN <- wilson_lcb(pool$k[sel0], pool$n[sel0], zN)
  for (b in BUDGETS)
    add_row("ordering", "prod", "zN", target, b, N,
            walk_eval(statN, idx_tr[sel0], idx_te[sel0],
                      nrow(tr), nrow(te), ie_te, ed_te, b), base)
  ## famEB: collapse families, prior on representatives, posterior-mean order
  ord0 <- order(-pool$lcb2326[sel0])
  dr <- neardup_drop(pool[sel0, ], idx_tr[sel0], ord0)
  reps <- sel0[setdiff(seq_along(sel0), dr)]
  ebm <- rep(NA_real_, length(reps))
  for (h in HH_LEVELS) {
    ix <- which(pool$hh[reps] == h)
    if (length(ix) < 50) { ebm[ix] <- pool$k[reps][ix] / pool$n[reps][ix]; next }
    ab <- fit_bb(pool$k[reps][ix], pool$n[reps][ix])
    ebm[ix] <- (ab[1] + pool$k[reps][ix]) / (ab[1] + ab[2] + pool$n[reps][ix])
  }
  for (b in BUDGETS)
    add_row("ordering", "prod", "famEB", target, b, length(reps),
            walk_eval(ebm, idx_tr[reps], idx_te[reps],
                      nrow(tr), nrow(te), ie_te, ed_te, b), base)

  ## dollar comparison (admission=prod; state pool gets its own dollar stats)
  ds_own_path <- file.path(ERA_DIR, sprintf("dstats_%s.rds", gsub("[^A-Za-z]", "", target)))
  if (file.exists(ds_own_path)) {
    dso <- readRDS(ds_own_path)
  } else {
    subo <- own[own$adm_prod, , drop = FALSE]
    idxo <- flags_for_rules(subo, tr, strata_tr, label = "")
    edtr <- ed_all[st == target & in_tr]
    dso <- data.frame(rule = subo$rule, hh = subo$hh,
      dpf = vapply(idxo, function(ix) sum(edtr[ix]), numeric(1)) / pmax(lengths(idxo), 1),
      dpflb = vapply(seq_along(idxo), function(i) {
        d <- edtr[idxo[[i]]]; n <- length(d)
        if (n < 2) return(NA_real_)
        expm1(mean(log1p(d)) - 2.326 * sd(log1p(d)) / sqrt(n))
      }, numeric(1)))
    saveRDS(dso, ds_own_path)
  }
  dmap <- bind_rows(nds, dso)
  key_pool <- paste(pool$hh[sel0], pool$rule[sel0], sep = "\r")
  m <- match(key_pool, paste(dmap$hh, dmap$rule, sep = "\r"))
  for (v in c("dpf", "dpflb")) {
    stat <- dmap[[v]][m]
    for (b in BUDGETS)
      add_row("dollar", "prod", v, target, b, length(sel0),
              walk_eval(stat, idx_tr[sel0], idx_te[sel0],
                        nrow(tr), nrow(te), ie_te, ed_te, b), base)
  }
  for (b in BUDGETS)
    add_row("dollar", "prod", "lcb99", target, b, length(sel0),
            walk_eval(pool$lcb2326[sel0], idx_tr[sel0], idx_te[sel0],
                      nrow(tr), nrow(te), ie_te, ed_te, b), base)

  ## xfit comparison (national-only lists)
  if (HAVE_XFIT) {
    natl_only <- dedup_sel(which(seq_len(nrow(pool)) <= sum(keep_n) & pool$adm_prod),
                           pool$lcb2326)
    xpool <- xf
    xidx_tr <- flags_for_rules(xpool, tr, strata_tr, label = "")
    xidx_te <- flags_for_rules(xpool, te, strata_te, label = "")
    for (b in BUDGETS) {
      add_row("xfit", "prod", "fullmine_lcb", target, b, length(natl_only),
              walk_eval(pool$lcb2326[natl_only], idx_tr[natl_only],
                        idx_te[natl_only], nrow(tr), nrow(te), ie_te, ed_te, b), base)
      add_row("xfit", "halfmine_prod", "xfit_lcb", target, b, nrow(xpool),
              walk_eval(xpool$lcb, xidx_tr, xidx_te,
                        nrow(tr), nrow(te), ie_te, ed_te, b), base)
    }
  }
  cat(sprintf("%-22s done\n", target))
  saveRDS(bind_rows(res), file.path(ERA_DIR, "era_validation_partial.rds"))
}
out <- bind_rows(res)
write.csv(out, file.path(ERA_DIR, "era_validation_results.csv"), row.names = FALSE)
cat(sprintf("wrote %s (%d rows)\n",
            file.path(ERA_DIR, "era_validation_results.csv"), nrow(out)))

cat("\nmedians by comparison x arm (precision / dollar recall):\n")
sm <- out %>% group_by(comparison, admission, ordering, budget) %>%
  summarise(med_prec = median(precision, na.rm = TRUE),
            med_dollars = median(dollar_recall, na.rm = TRUE), .groups = "drop") %>%
  arrange(comparison, budget, desc(med_prec))
print(as.data.frame(sm), row.names = FALSE, max = 400)
