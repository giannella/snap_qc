# ──────────────────────────────────────────────────────────────────────────────
# DEPLOYMENT-GRADE benchmark: honest on BOTH axes (state and time).
#
#   train: 2022 + 2023 only     test: the target state's 2024
#
# Every approach is scored on a year no rule ever saw; transfer approaches
# are additionally scored on a state no rule ever saw. Approaches per target:
#   own_state     rules mined on the target's OWN 2022-23 (temporal holdout
#                 only; hard support floor n >= 30 per the state-scale lesson)
#   national_loo  rules mined on all other states' 2022-23
#   transfer_nb   rules mined on the 5 most-similar states' 2022-23, with
#                 similarity computed from 2022-23 fire rates only
# Review-budget evaluation (5% / 10%): rules added in descending train-LCB
# order while the union fits the budget.
#
# KNOWN LIMITATION (accepted): the rule vocabulary used to DEFINE fire-rate
# similarity comes from the national shortlist, which was mined on 2022+2024.
# Only rule SHAPES leak; all similarity and training data here are 2022-23.
#
# All outputs carry the train2223_test24 tag to distinguish them from the
# same-era benchmark (methods/state_similarity_v2/transfer_benchmark/):
#   methods/state_similarity_v2/similarity_nb_2022_2023.csv
#   methods/state_similarity_v2/transfer_benchmark_train2223_test24/
#     pool_cache/ , deployment_menu_train2223_test24.csv
# Expects `reg_model_data`.
# ──────────────────────────────────────────────────────────────────────────────

suppressMessages(library(dplyr))
source("rule_mining_helpers.R")
set.seed(117)

TARGETS <- c("Louisiana", "Washington", "Virginia", "Arizona", "Connecticut",
             "Michigan", "North Carolina", "California", "Texas",
             "Mississippi", "New Jersey", "Colorado")
K_NEIGHBORS <- 5
TRAIN_YEARS <- c("2022", "2023")
TEST_YEAR   <- "2024"
BUDGETS     <- c(0.05, 0.10)

NATIONAL_CSV <- "inclusion_rules_by_hh_size_v2/final_rules_highprecision_all_frames.csv"
out_dir   <- "methods/state_similarity_v2/transfer_benchmark_train2223_test24"
CACHE_DIR <- file.path(out_dir, "pool_cache")
dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)

XGB <- list(nrounds = 1000, max_depth = 4, eta = 0.02, subsample = 0.20)
RF  <- list(num_trees = 1000, max_depth = 4, mtry = 2, min_node_size = 20)
SIGNIF_DIGITS <- 3
LCB_Z         <- 2.326
MIN_TRAIN_FLAGGED <- 30
STATE_COL <- "state"
HH_SIZE_COL <- "cert_HH_size_FS_n"
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
targets_of <- function(df) {
  ie <- !is.na(df$over_threshold) & df$over_threshold != 0
  amt <- df$total_error_amount; amt[is.na(amt)] <- 0
  list(ie = ie, ed = ifelse(ie, abs(amt), 0))
}

pf  <- prep_features(reg_model_data %>%
                       filter(fiscal_year %in% c(TRAIN_YEARS, TEST_YEAR)), features)
adf <- pf$data
pv  <- pf$features
yr  <- as.character(adf$fiscal_year)
st  <- as.character(adf[[STATE_COL]])
in_train_years <- yr %in% TRAIN_YEARS
cat(sprintf("frame: %d rows (train years %s, test %s)\n",
            nrow(adf), paste(TRAIN_YEARS, collapse = "+"), TEST_YEAR))

## ── 1. NB similarity from 2022-23 fire rates ─────────────────────────────────
nb_path <- "methods/state_similarity_v2/similarity_nb_2022_2023.csv"
if (!file.exists(nb_path)) {
  rules <- read.csv(NATIONAL_CSV, stringsAsFactors = FALSE) %>% distinct(rule, hh)
  strata_all <- lapply(setNames(nm = unique(rules$hh)), function(h)
    which(hh_group_of(adf[[HH_SIZE_COL]]) %in% h))
  idx <- flags_for_rules(rules, adf, strata_all, label = "similarity vocabulary")
  states <- sort(unique(st))
  denom <- sapply(states, function(s) {
    rows <- which(st == s & in_train_years)
    vapply(rules$hh, function(h) length(intersect(rows, strata_all[[h]])), numeric(1))
  })
  num <- sapply(states, function(s) {
    vapply(idx, function(ix) sum(st[ix] == s & in_train_years[ix]), numeric(1))
  })
  k <- num
  p <- (k + 0.5) / (denom + 1)
  lp <- log(p); l1p <- log1p(-p)
  n_s <- length(states)
  kl <- matrix(0, n_s, n_s, dimnames = list(states, states))
  for (a in seq_len(n_s)) {
    pa <- p[, a]
    kl[a, ] <- colSums((pa - p) * (lp[, a] - lp) +
                       ((1 - pa) - (1 - p)) * (l1p[, a] - l1p))
  }
  write.csv(round(1 / (1 + kl), 5), nb_path)
  cat(sprintf("wrote %s\n", nb_path))
}
S <- as.matrix(read.csv(nb_path, row.names = 1, check.names = FALSE))
colnames(S) <- rownames(S)

## ── 2. pool mining on 2022-23 (cached) ───────────────────────────────────────
mine_pool <- function(pool_states) {
  sig <- gsub("[^A-Za-z]", "", paste(sort(pool_states), collapse = "_"))
  key <- if (nchar(sig) <= 80) sig else
    sprintf("%s_%08x", substr(sig, 1, 60),
            sum(utf8ToInt(sig) * seq_along(utf8ToInt(sig))) %% .Machine$integer.max)
  cache <- file.path(CACHE_DIR, sprintf("pool_%s.rds", key))
  if (file.exists(cache)) return(readRDS(cache))

  train <- adf[st %in% pool_states & in_train_years, , drop = FALSE]
  tg_tr <- targets_of(train)
  strata_tr <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(train[[HH_SIZE_COL]]) %in% h))
  rules_df <- bind_rows(lapply(HH_LEVELS, function(h) {
    ix <- strata_tr[[h]]
    sub <- train[ix, , drop = FALSE]; ie_s <- tg_tr$ie[ix]
    if (nrow(sub) < 100 || sum(ie_s) < 10) return(NULL)
    rx <- canonicalize_rules(
      generate_rules_xgboost(sub, ie_s, pv, nrounds = XGB$nrounds,
                             max_depth = XGB$max_depth, eta = XGB$eta,
                             subsample = XGB$subsample, seed = 117), SIGNIF_DIGITS)
    rr <- canonicalize_rules(
      generate_rules_ranger(sub, ie_s, pv, num_trees = RF$num_trees,
                            max_depth = RF$max_depth, mtry = RF$mtry,
                            min_node_size = RF$min_node_size, seed = 117), SIGNIF_DIGITS)
    bind_rows(data.frame(rule = rx, hh = h, stringsAsFactors = FALSE),
              data.frame(rule = rr, hh = h, stringsAsFactors = FALSE))
  })) %>% distinct(hh, rule)
  if (is.null(rules_df) || nrow(rules_df) == 0) return(NULL)

  idx_tr <- flags_for_rules(rules_df, train, strata_tr, label = "pool-train")
  n_tr <- lengths(idx_tr)
  k_tr <- vapply(idx_tr, function(ix) sum(tg_tr$ie[ix]), numeric(1))
  raw  <- ifelse(n_tr > 0, k_tr / n_tr, NA_real_)
  base <- vapply(rules_df$hh, function(h) mean(tg_tr$ie[strata_tr[[h]]]), numeric(1))
  keep <- !is.na(raw) & n_tr >= MIN_TRAIN_FLAGGED & raw >= 0.05 & raw > base
  rules_df <- rules_df[keep, , drop = FALSE]; idx_tr <- idx_tr[keep]
  rules_df$n_flagged_train <- n_tr[keep]
  rules_df$precision_train <- round(raw[keep], 4)
  rules_df$precision_train_lcb <- round(wilson_lcb(k_tr[keep], n_tr[keep], LCB_Z), 4)
  drop_cov <- dedup_exact_coverage(rules_df, idx_tr)
  rules_df <- rules_df[!drop_cov, , drop = FALSE]; idx_tr <- idx_tr[!drop_cov]
  drop_dom <- dedup_dominated(rules_df, rules_df$precision_train_lcb)
  rules_df <- rules_df[!drop_dom, , drop = FALSE]
  saveRDS(rules_df, cache)
  rules_df
}

## ── 3. budget-fill on the target's 2024 ──────────────────────────────────────
budgeted_union <- function(rules_df, stat, idx, n_rows, tg, budget) {
  cap <- floor(budget * n_rows)
  un <- rep(FALSE, n_rows); n_used <- 0L; n_in <- 0L
  for (i in order(-stat)) {
    if (is.na(stat[i])) next
    ix <- idx[[i]]
    add <- sum(!un[ix])
    if (n_in + add <= cap) { un[ix] <- TRUE; n_in <- n_in + add; n_used <- n_used + 1L }
  }
  k <- sum(tg$ie[un]); d <- sum(tg$ed[un])
  data.frame(n_rules_used = n_used, n_flagged = n_in,
             workload = round(n_in / n_rows, 4),
             precision = round(ifelse(n_in > 0, k / n_in, NA), 4),
             recall = round(k / sum(tg$ie), 4),
             dollar_recall = round(d / sum(tg$ed), 4))
}

res <- list()
for (target in TARGETS) {
  tgt <- adf[st == target & yr == TEST_YEAR, , drop = FALSE]
  tg  <- targets_of(tgt)
  strata_t <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(tgt[[HH_SIZE_COL]]) %in% h))
  cat(sprintf("\n== %s: %d test-2024 rows, base %.1f%%\n",
              target, nrow(tgt), 100 * mean(tg$ie)))

  nbv <- sort(S[target, setdiff(colnames(S), target)], decreasing = TRUE)
  pools <- list(
    own_state    = target,
    transfer_nb  = names(nbv)[seq_len(K_NEIGHBORS)],
    national_loo = setdiff(sort(unique(st)), target)
  )
  cat(sprintf("  nb pool: %s\n", paste(pools$transfer_nb, collapse = ", ")))
  for (nm in names(pools)) {
    rl <- mine_pool(pools[[nm]])
    if (is.null(rl) || nrow(rl) == 0) {
      cat(sprintf("  %-13s NO rules survive\n", nm)); next
    }
    idx <- flags_for_rules(rl, tgt, strata_t, label = "")
    for (b in BUDGETS) {
      out <- budgeted_union(rl, rl$precision_train_lcb, idx, nrow(tgt), tg, b)
      out$target <- target; out$approach <- nm; out$budget <- b
      out$n_pool_rules <- nrow(rl)
      out$target_base_rate <- round(mean(tg$ie), 4)
      res[[length(res) + 1]] <- out
      cat(sprintf("  %-13s budget %2.0f%%: %4d rules, prec %.3f, $%3.0f%%\n",
                  nm, 100 * b, out$n_rules_used, out$precision,
                  100 * out$dollar_recall))
    }
  }
  saveRDS(bind_rows(res), file.path(out_dir, "deployment_partial.rds"))
}
out <- bind_rows(res)
write.csv(out, file.path(out_dir, "deployment_menu_train2223_test24.csv"),
          row.names = FALSE)
cat(sprintf("\nwrote %s (%d rows)\n",
            file.path(out_dir, "deployment_menu_train2223_test24.csv"), nrow(out)))
