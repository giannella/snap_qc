# ──────────────────────────────────────────────────────────────────────────────
# Leave-one-state-out NEIGHBOR-TRANSFER benchmark.
#
# For each TARGET state and each similarity definition (fire-rate / policy /
# blended), take the top-K most similar states (target excluded), mine
# any-error inclusion rules on the NEIGHBOR pool (2022-24, both engines,
# tuned settings, LCB filter), then score the shortlist union on the TARGET's
# 2022-24 cases. Era match is baked in (validated on the Louisiana transfer:
# same-era neighbors worked where mixed-era pooling collapsed).
#
# Baseline per target: the national high-precision shortlist evaluated on the
# same target cases. CAVEAT: national rules were trained on 2022+2024
# INCLUDING the target's own cases, so the baseline is flattered; the
# transfer pools never see the target.
#
# Pools are cached by their state-set signature (many target x similarity
# combinations share pools), checkpointed to CACHE_DIR so the run resumes.
#
# Expects `reg_model_data`. Outputs -> methods/state_similarity_v2/transfer_benchmark/.
# ──────────────────────────────────────────────────────────────────────────────

suppressMessages(library(dplyr))
source("rule_mining_helpers.R")
set.seed(117)

TARGETS <- c("Louisiana", "Washington", "Virginia", "Arizona", "Connecticut",
             "Michigan", "North Carolina", "California", "Texas",
             "Mississippi", "New Jersey", "Colorado")
K_NEIGHBORS <- 5
YEARS       <- c("2022", "2023", "2024")   # train (neighbors) AND test (target)

# Era-matched similarity: the benchmark trains and tests on 2022-24, so
# neighbors are chosen on the 2022-24 matrices.
SIM_FILES <- c(fire    = "methods/state_similarity_v2/similarity_matrix_sqrt_2022_2024.csv",
               idf     = "methods/state_similarity_v2/similarity_idf_2022_2024.csv",
               nb      = "methods/state_similarity_v2/similarity_nb_2022_2024.csv",
               policy  = "methods/state_similarity_v2/similarity_policy_2022_2024.csv",
               blended = "methods/state_similarity_v2/similarity_blended_2022_2024.csv")

# Leave-one-state-out NATIONAL control: mine the SAME any-error recipe on all
# states except the target - the honest version of the national baseline
# (national_asis trained on the target's own 2022/24 cases). Expensive
# (~full-scale mine per target); cached like any pool.
RUN_LOO_NATIONAL <- TRUE

NATIONAL_CSV <- "inclusion_rules_by_hh_size_v2/final_rules_highprecision_all_frames.csv"

XGB <- list(nrounds = 1000, max_depth = 4, eta = 0.02, subsample = 0.20)
RF  <- list(num_trees = 1000, max_depth = 4, mtry = 2, min_node_size = 20)
SIGNIF_DIGITS <- 3
LCB_Z         <- 2.326
MIN_TRAIN_FLAGGED <- 30    # state-scale lesson: hard support floor
MIN_PRECISION     <- 0.20
FLOORS            <- c(0.20, 0.25, 0.30)

out_dir   <- "methods/state_similarity_v2/transfer_benchmark"
CACHE_DIR <- file.path(out_dir, "pool_cache")
dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)

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

pf  <- prep_features(reg_model_data %>% filter(fiscal_year %in% YEARS), features)
adf <- pf$data
pv  <- pf$features   # prepped feature names (prep may coerce/rename)
cat(sprintf("frame: %d rows, %s\n", nrow(adf), paste(range(YEARS), collapse = "-")))

sims <- lapply(SIM_FILES, function(f) {
  S <- as.matrix(read.csv(f, row.names = 1, check.names = FALSE))
  colnames(S) <- rownames(S)  # guard against header name-mangling
  S
})

## ── mine a pool (cached) ─────────────────────────────────────────────────────
mine_pool <- function(pool_states) {
  sig <- paste(sort(pool_states), collapse = "_")
  sig <- gsub("[^A-Za-z]", "", sig)
  # long signatures (e.g. leave-one-out pools) would truncate identically;
  # append a positional checksum so distinct state sets get distinct keys
  key <- if (nchar(sig) <= 80) sig else
    sprintf("%s_%08x", substr(sig, 1, 60),
            sum(utf8ToInt(sig) * seq_along(utf8ToInt(sig))) %% .Machine$integer.max)
  cache <- file.path(CACHE_DIR, sprintf("pool_%s.rds", key))
  if (file.exists(cache)) return(readRDS(cache))

  train <- adf[adf[[STATE_COL]] %in% pool_states, , drop = FALSE]
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

## ── score a rule set's union on a target state ───────────────────────────────
score_on_target <- function(rules_df, target_state, stat) {
  tgt <- adf[adf[[STATE_COL]] == target_state, , drop = FALSE]
  tg  <- targets_of(tgt)
  strata_t <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(tgt[[HH_SIZE_COL]]) %in% h))
  idx <- flags_for_rules(rules_df, tgt, strata_t, label = "")
  sw  <- precision_sweep(stat, !is.na(stat), idx, tg$ie, tg$ed, FLOORS)
  sw$n_target_rows <- nrow(tgt)
  sw$target_base_rate <- round(mean(tg$ie), 4)
  sw
}

## ── national baseline rules ──────────────────────────────────────────────────
natl <- read.csv(NATIONAL_CSV, stringsAsFactors = FALSE) %>%
  distinct(rule, hh, .keep_all = TRUE)

## ── run ──────────────────────────────────────────────────────────────────────
res <- list()
for (target in TARGETS) {
  cat(sprintf("\n======== TARGET: %s ========\n", target))
  bl <- score_on_target(natl, target, natl$precision_train_lcb) %>%
    mutate(approach = "national_asis", target = target, pool = "national")
  res[[length(res) + 1]] <- bl

  for (sim_name in names(sims)) {
    S <- sims[[sim_name]]
    if (!target %in% rownames(S)) { cat("  no similarity row; skipped\n"); next }
    nb <- sort(S[target, setdiff(colnames(S), target)], decreasing = TRUE)
    pool <- names(nb)[seq_len(K_NEIGHBORS)]
    cat(sprintf("  [%s] pool: %s\n", sim_name, paste(pool, collapse = ", ")))
    rules <- mine_pool(pool)
    if (is.null(rules) || nrow(rules) == 0) next
    cat(sprintf("  [%s] %d pool rules survive screen+dedup\n", sim_name, nrow(rules)))
    sc <- score_on_target(rules, target, rules$precision_train_lcb) %>%
      mutate(approach = paste0("transfer_", sim_name), target = target,
             pool = paste(pool, collapse = "|"))
    res[[length(res) + 1]] <- sc
  }

  if (RUN_LOO_NATIONAL) {
    loo_pool <- setdiff(sort(unique(as.character(adf[[STATE_COL]]))), target)
    cat(sprintf("  [national_loo] mining all %d states except %s\n",
                length(loo_pool), target))
    rules <- mine_pool(loo_pool)
    if (!is.null(rules) && nrow(rules) > 0) {
      cat(sprintf("  [national_loo] %d rules survive screen+dedup\n", nrow(rules)))
      sc <- score_on_target(rules, target, rules$precision_train_lcb) %>%
        mutate(approach = "national_loo", target = target,
               pool = "all_minus_target")
      res[[length(res) + 1]] <- sc
    }
  }
  saveRDS(bind_rows(res), file.path(out_dir, "benchmark_partial.rds"))
}

out <- bind_rows(res)
write.csv(out, file.path(out_dir, "transfer_benchmark_results.csv"), row.names = FALSE)
cat(sprintf("\nwrote %s (%d rows)\n",
            file.path(out_dir, "transfer_benchmark_results.csv"), nrow(out)))
