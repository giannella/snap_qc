# BLENDED frozen lists, FIVE-FRAME vocabulary variant: re-validates the
# delivery recipe after the typed-frame mining change (mine_rule_vocabulary:
# 4 typed frames + any_error, provenance-tagged) on the same time-shifted
# benchmark as blended_frozen_lists_v2.R -- train 2022-23, walk the frozen
# list on each state's 2024. Everything downstream of mining (LCB variants,
# merge, freeze/walk protocol, scoring) is copied verbatim from
# blended_frozen_lists_v2.R so the two results files differ ONLY in the rule
# vocabulary:
#   blended_frozen_results.csv          any-error-only pools (original)
#   blended_frozen_results_5frames.csv  five-frame pools (this script)
#
# Pools are mined per frame with checkpoints (the memory lesson from the
# delivery builder: five frames' flag indices at once exhaust RAM at
# national scale) into pool_cache_5frames/.
# Expects `reg_model_data`.

suppressMessages(library(dplyr))
source("rule_mining_helpers.R")
set.seed(117)

TARGETS <- c("Louisiana", "Washington", "Virginia", "Arizona", "Connecticut",
             "Michigan", "North Carolina", "California", "Texas",
             "Mississippi", "New Jersey", "Colorado",
             "Maine", "Maryland", "Missouri", "Massachusetts",
             "District of Columbia", "Tennessee")
BUDGETS <- c(0.05, 0.10)
BUFFER_MULT <- 3
Z_VARIANTS <- c(lcb99 = 2.326, lcb98 = 2.054)
MINING_FRAMES <- c("earned_overissuance", "unearned_overissuance",
                   "underissuance", "other_error", "any_error")
TRAIN_YEARS <- c("2022", "2023")
TEST_YEAR   <- "2024"
XGB <- list(nrounds = 1000, max_depth = 4, eta = 0.02, subsample = 0.20)
RF  <- list(num_trees = 1000, max_depth = 4, mtry = 2, min_node_size = 20)
SIGNIF_DIGITS <- 3
LCB_Z <- 2.326
MIN_TRAIN_FLAGGED <- 30

out_dir   <- "methods/state_similarity_v2/transfer_benchmark_train2223_test24"
CACHE_DIR <- file.path(out_dir, "pool_cache_5frames")
dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)

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
st  <- as.character(adf$state)
in_train_years <- yr %in% TRAIN_YEARS
cat(sprintf("frame: %d rows (train %s, test %s) | frames: %s\n",
            nrow(adf), paste(TRAIN_YEARS, collapse = "+"), TEST_YEAR,
            paste(MINING_FRAMES, collapse = ", ")))

## ── five-frame pool mining on 2022-23, per-frame checkpoints ─────────────────
mine_pool_typed <- function(pool_states, key) {
  cache <- file.path(CACHE_DIR, sprintf("pool_%s.rds", key))
  if (file.exists(cache)) return(readRDS(cache))
  train <- adf[st %in% pool_states & in_train_years, , drop = FALSE]
  tg_tr <- targets_of(train)
  strata_tr <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(train$cert_HH_size_FS_n) %in% h))
  es <- as.character(train$error_status)
  empty <- data.frame(hh = character(), rule = character(), engines = character(),
                      mined_frames = character(), n_flagged_train = integer(),
                      precision_train = numeric(), precision_train_lcb = numeric(),
                      stringsAsFactors = FALSE)
  filt <- list()
  for (fn in MINING_FRAMES) {
    ck <- file.path(CACHE_DIR, sprintf("filtered_%s_%s.rds", key, fn))
    if (file.exists(ck)) { filt[[fn]] <- readRDS(ck); next }
    rows <- if (fn == "any_error") seq_len(nrow(train))
            else which(es %in% c(fn, "no_error"))
    rdf <- mine_rule_vocabulary(
      train, setNames(list(list(rows = rows, ie = tg_tr$ie[rows])), fn),
      strata_tr, pv, xgb = XGB, rf = RF,
      signif_digits = SIGNIF_DIGITS, seed = 117)
    if (is.null(rdf) || nrow(rdf) == 0) {
      saveRDS(empty, ck); filt[[fn]] <- empty; next
    }
    idx <- flags_for_rules(rdf, train, strata_tr,
                           label = sprintf("pool-train %s", fn))
    n_tr <- lengths(idx)
    k_tr <- vapply(idx, function(ix) sum(tg_tr$ie[ix]), numeric(1))
    raw  <- ifelse(n_tr > 0, k_tr / n_tr, NA_real_)
    base <- vapply(rdf$hh, function(h) mean(tg_tr$ie[strata_tr[[h]]]), numeric(1))
    keep <- !is.na(raw) & n_tr >= MIN_TRAIN_FLAGGED & raw >= 0.05 & raw > base
    rdf <- rdf[keep, , drop = FALSE]
    rdf$n_flagged_train <- n_tr[keep]
    rdf$precision_train <- round(raw[keep], 4)
    rdf$precision_train_lcb <- round(wilson_lcb(k_tr[keep], n_tr[keep], LCB_Z), 4)
    saveRDS(rdf, ck)
    filt[[fn]] <- rdf
    rm(idx); invisible(gc())
  }
  all_f <- do.call(rbind, filt)
  if (is.null(all_f) || nrow(all_f) == 0) return(NULL)
  key2 <- paste(all_f$hh, all_f$rule, sep = "\r")
  eng <- vapply(split(all_f$engines, key2), function(e)
    paste(sort(unique(unlist(strsplit(e, "+", fixed = TRUE)))), collapse = "+"),
    character(1))
  frs <- vapply(split(all_f$mined_frames, key2), function(f)
    paste(sort(unique(unlist(strsplit(f, "+", fixed = TRUE)))), collapse = "+"),
    character(1))
  first <- !duplicated(key2)
  rules_df <- all_f[first, , drop = FALSE]
  rules_df$engines <- unname(eng[key2[first]])
  rules_df$mined_frames <- unname(frs[key2[first]])
  rules_df <- rules_df[order(rules_df$hh, rules_df$rule, method = "radix"), ,
                       drop = FALSE]
  rownames(rules_df) <- NULL
  idx_tr <- flags_for_rules(rules_df, train, strata_tr, label = "pool-dedup")
  drop_cov <- dedup_exact_coverage(rules_df, idx_tr)
  rules_df <- rules_df[!drop_cov, , drop = FALSE]; idx_tr <- idx_tr[!drop_cov]
  drop_dom <- dedup_dominated(rules_df, rules_df$precision_train_lcb)
  rules_df <- rules_df[!drop_dom, , drop = FALSE]
  saveRDS(rules_df, cache)
  rules_df
}

natl <- mine_pool_typed(sort(unique(st)), "national")
natl$source <- "national"
cat(sprintf("national 2022-23 five-frame pool: %d rules\n", nrow(natl)))

lcb_at <- function(pool, z) {
  k <- round(pool$precision_train * pool$n_flagged_train)
  wilson_lcb(k, pool$n_flagged_train, z)
}

## ── freeze/walk/score: verbatim from blended_frozen_lists_v2.R ───────────────
res <- list()
for (target in TARGETS) {
  own <- mine_pool_typed(target, gsub("[^A-Za-z]", "", target))
  if (is.null(own)) own <- natl[0, ]
  if (nrow(own)) own$source <- "state"
  cat(sprintf("%s own pool: %d rules\n", target, nrow(own)))
  pool <- bind_rows(natl, own)
  for (v in names(Z_VARIANTS)) pool[[v]] <- lcb_at(pool, Z_VARIANTS[[v]])
  # same rule mined by both pools: keep the higher-bound version
  pool <- pool %>% arrange(desc(lcb99)) %>% distinct(hh, rule, .keep_all = TRUE)

  tr <- adf[st == target & in_train_years, , drop = FALSE]
  te <- adf[st == target & yr == TEST_YEAR, , drop = FALSE]
  tg_te <- targets_of(te)
  strata_tr <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(tr$cert_HH_size_FS_n) %in% h))
  strata_te <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(te$cert_HH_size_FS_n) %in% h))
  idx_tr <- flags_for_rules(pool, tr, strata_tr, label = "")
  idx_te <- flags_for_rules(pool, te, strata_te, label = "")

  for (v in names(Z_VARIANTS)) {
    ord <- order(-pool[[v]])
    for (b in BUDGETS) {
      cap <- floor(b * nrow(tr)); cap_buf <- floor(BUFFER_MULT * b * nrow(tr))
      un <- rep(FALSE, nrow(tr)); n_in <- 0L
      frozen <- integer(0); buffer <- integer(0)
      for (i in ord) {
        add <- sum(!un[idx_tr[[i]]])
        if (add == 0) next
        if (n_in + add <= cap) {
          un[idx_tr[[i]]] <- TRUE; n_in <- n_in + add; frozen <- c(frozen, i)
        } else if (n_in + add <= cap_buf) {
          un[idx_tr[[i]]] <- TRUE; n_in <- n_in + add; buffer <- c(buffer, i)
        }
      }
      cap24 <- floor(b * nrow(te))
      un24 <- rep(FALSE, nrow(te)); used <- integer(0)
      for (i in c(frozen, buffer)) {
        add <- sum(!un24[idx_te[[i]]])
        if (add > 0 && sum(un24) + add <= cap24) {
          un24[idx_te[[i]]] <- TRUE; used <- c(used, i)
        }
      }
      nb <- sum(un24); kb <- sum(tg_te$ie[un24]); db <- sum(tg_te$ed[un24])
      res[[length(res) + 1]] <- data.frame(
        target = target, budget = b, variant = v,
        n_shipped = length(frozen) + length(buffer),
        n_deployed = length(used),
        n_deployed_state = sum(pool$source[used] == "state"),
        workload = round(nb / nrow(te), 4),
        precision = round(ifelse(nb > 0, kb / nb, NA), 4),
        recall = round(kb / sum(tg_te$ie), 4),
        dollar_recall = round(db / sum(tg_te$ed), 4),
        target_base_rate = round(mean(tg_te$ie), 4))
      cat(sprintf("%-22s %s %2.0f%%: %3d deployed (%2d state) | wkld %4.1f%% | prec %.3f | $%3.0f%%\n",
                  target, v, 100 * b, length(used),
                  sum(pool$source[used] == "state"),
                  100 * nb / nrow(te), ifelse(nb > 0, kb / nb, NA),
                  100 * db / sum(tg_te$ed)))
    }
  }
  saveRDS(bind_rows(res), file.path(out_dir, "blended_frozen_5frames_partial.rds"))
}
out <- bind_rows(res)
write.csv(out, file.path(out_dir, "blended_frozen_results_5frames.csv"), row.names = FALSE)
cat(sprintf("wrote %s (%d rows)\n",
            file.path(out_dir, "blended_frozen_results_5frames.csv"), nrow(out)))

## ── head-to-head vs the any-error-only vocabulary ────────────────────────────
old <- read.csv(file.path(out_dir, "blended_frozen_results.csv"),
                stringsAsFactors = FALSE)
cmp <- out %>% filter(variant == "lcb99") %>%
  select(target, budget, precision, dollar_recall) %>%
  inner_join(old %>% filter(variant == "lcb99") %>%
               select(target, budget, precision, dollar_recall, target_base_rate),
             by = c("target", "budget"), suffix = c("_5frames", "_anyerror"))
cat("\nmedians (lcb99):\n")
print(cmp %>% group_by(budget) %>%
        summarise(prec_5frames = median(precision_5frames),
                  prec_anyerror = median(precision_anyerror),
                  dollars_5frames = median(dollar_recall_5frames),
                  dollars_anyerror = median(dollar_recall_anyerror),
                  .groups = "drop") %>% as.data.frame(), row.names = FALSE)
cat("\nstates above base rate with any-error vocabulary but below with 5-frame:\n")
print(cmp %>% filter(precision_anyerror > target_base_rate,
                     precision_5frames <= target_base_rate) %>%
        as.data.frame(), row.names = FALSE)
