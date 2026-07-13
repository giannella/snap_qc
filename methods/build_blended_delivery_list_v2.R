# DELIVERY builder: the blended frozen list for one state, built on ALL
# public years (2022-2024) -- the version a state is actually handed
# (modeling_findings.md #16). NO holdout: every public year trains, so the
# performance expectation comes from the one-year-ahead benchmark
# (sections 15-16), and the state's own internal validation on its newer
# data is the honest judge before deployment.
#
# Recipe: mine the national pool (all states) and the state's own pool on
# 2022-24; rank every rule by its own-training 99% Wilson LCB (the blend
# scale); fill against the state's 2022-24 caseload to the review budget
# (core) and to 3x depth (buffer); export the ranked lists.
#
# DELIVERY_STATE can be pre-set in a runner before source().
# Expects `reg_model_data`. Outputs -> state_delivery_lists/ (tracked and
# public: these lists are batch-built from public data for any state, unlike
# the single-state engagement work that stays in gitignored custom_one_off/).
# The national all-years pool is cached in methods/delivery_pools_2022_2024/
# for reuse across states.

suppressMessages(library(dplyr))
source("rule_mining_helpers.R")
set.seed(117)

if (!exists("DELIVERY_STATE")) DELIVERY_STATE <- "Connecticut"
YEARS   <- c("2022", "2023", "2024")
BUDGETS <- c(0.05, 0.10)
BUFFER_MULT <- 3
LCB_Z <- 2.326
XGB <- list(nrounds = 1000, max_depth = 4, eta = 0.02, subsample = 0.20)
RF  <- list(num_trees = 1000, max_depth = 4, mtry = 2, min_node_size = 20)
SIGNIF_DIGITS <- 3
MIN_TRAIN_FLAGGED <- 30

POOL_CACHE <- "methods/delivery_pools_2022_2024"
out_dir <- "state_delivery_lists"
dir.create(POOL_CACHE, showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

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
pv  <- pf$features
st  <- as.character(adf$state)
cat(sprintf("frame: %d rows, years %s\n", nrow(adf), paste(YEARS, collapse = "+")))

mine_pool <- function(pool_states, cache_name) {
  cache <- file.path(POOL_CACHE, sprintf("pool_%s.rds", cache_name))
  if (file.exists(cache)) return(readRDS(cache))
  train <- adf[st %in% pool_states, , drop = FALSE]
  tg_tr <- targets_of(train)
  strata_tr <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(train$cert_HH_size_FS_n) %in% h))
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

natl <- mine_pool(sort(unique(st)), "national")
natl$source <- "national"
cat(sprintf("national 2022-24 pool: %d rules\n", nrow(natl)))
own <- mine_pool(DELIVERY_STATE, gsub("[^A-Za-z]", "", DELIVERY_STATE))
if (is.null(own)) {
  own <- natl[0, ]
  cat("own pool: no rules survive\n")
} else {
  own$source <- "state"
  cat(sprintf("own 2022-24 pool: %d rules\n", nrow(own)))
}

pool <- bind_rows(natl, own) %>%
  arrange(desc(precision_train_lcb)) %>%
  distinct(hh, rule, .keep_all = TRUE)

tr <- adf[st == DELIVERY_STATE, , drop = FALSE]
strata_tr <- lapply(setNames(nm = HH_LEVELS), function(h)
  which(hh_group_of(tr$cert_HH_size_FS_n) %in% h))
idx_tr <- flags_for_rules(pool, tr, strata_tr, label = "")
ord <- order(-pool$precision_train_lcb)

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
  sel <- c(frozen, buffer)
  hand <- pool[sel, c("rule", "hh", "source", "n_flagged_train",
                      "precision_train", "precision_train_lcb")]
  hand$rank <- seq_along(sel)
  hand$role <- rep(c("core", "buffer"), c(length(frozen), length(buffer)))
  fn <- file.path(out_dir, sprintf("blended_delivery_%s_2022_2024_budget%02.0f.csv",
                                   gsub(" ", "_", DELIVERY_STATE), 100 * b))
  write.csv(hand, fn, row.names = FALSE)
  cat(sprintf("budget %2.0f%%: core %d + buffer %d (state rules: %d core, %d buffer) -> %s\n",
              100 * b, length(frozen), length(buffer),
              sum(pool$source[frozen] == "state"),
              sum(pool$source[buffer] == "state"), fn))
}
cat("delivery lists written. NOTE: no holdout -- all public years train;\n")
cat("expected performance is the one-year-ahead benchmark (findings 15-16);\n")
cat("the state validates on its own internal data before relying on it.\n")
