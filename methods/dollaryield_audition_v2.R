# Dollaryield pairing audition (goal-parametric harness, 2026-07-16).
# Goal metric: share of error DOLLARS caught at a fixed review workload,
# scored on 2024. EXPLORATORY - adoption requires the honest designs.
#
# Pre-registered expectation (from dollar_persistence_check_v2.R: per-rule
# dollars-per-flag persists train->test MORE strongly than precision):
# dollar-aware rankings beat the lcb99 baseline on median dollar recall at
# both budgets without a large precision cost.
#
# Ranking variants (computed on each rule's own training pool):
#   lcb99   baseline: 99% Wilson lower bound of any-error precision
#   dpf     mean error dollars per flagged case (raw)
#   dpflb   conservative dollars/flag: log-scale lower bound
#           exp(mean(log1p(d)) - z*sd(log1p(d))/sqrt(n)) - 1, z = 2.326
#   eylb    expected conservative dollar yield: lcb99 x (dollars per HIT)
#
# Uses the cached any-error benchmark pools (train 2022-23). National-pool
# dollar stats are computed once and cached.
# Output: methods/state_similarity_v2/transfer_benchmark_train2223_test24/
#         dollaryield_audition.csv

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
TRAIN_YEARS <- c("2022", "2023")
TEST_YEAR   <- "2024"

out_dir  <- "methods/state_similarity_v2/transfer_benchmark_train2223_test24"
CACHEOLD <- file.path(out_dir, "pool_cache")
DCACHE   <- file.path(out_dir, "dollar_stats_cache")
dir.create(DCACHE, showWarnings = FALSE)

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

sig <- gsub("[^A-Za-z]", "", paste(sort(unique(st)), collapse = "_"))
nat_key <- if (nchar(sig) <= 80) sig else
  sprintf("%s_%08x", substr(sig, 1, 60),
          sum(utf8ToInt(sig) * seq_along(utf8ToInt(sig))) %% .Machine$integer.max)

# dollar stats for a pool on ITS OWN training caseload (rows selector)
dollar_stats <- function(pool, rows, cache_name) {
  cache <- file.path(DCACHE, sprintf("dstats_%s.rds", cache_name))
  if (file.exists(cache)) return(readRDS(cache))
  train <- adf[rows, , drop = FALSE]
  strata_tr <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(train$cert_HH_size_FS_n) %in% h))
  idx <- flags_for_rules(pool, train, strata_tr, label = cache_name)
  ed <- ed_all[rows]; ie <- ie_all[rows]
  n <- lengths(idx)
  dsum <- vapply(idx, function(ix) sum(ed[ix]), numeric(1))
  khit <- vapply(idx, function(ix) sum(ie[ix]), numeric(1))
  lmu <- vapply(idx, function(ix) mean(log1p(ed[ix])), numeric(1))
  lsd <- vapply(idx, function(ix) sd(log1p(ed[ix])), numeric(1))
  out <- data.frame(dpf = ifelse(n > 0, dsum / n, NA_real_),
                    dph = ifelse(khit > 0, dsum / khit, 0),
                    dpflb = ifelse(n > 1, expm1(lmu - LCB_Z * lsd / sqrt(n)), NA_real_))
  saveRDS(out, cache)
  out
}

natl <- readRDS(file.path(CACHEOLD, sprintf("pool_%s.rds", nat_key)))
nds <- dollar_stats(natl, which(st %in% unique(st) & in_tr), "national")
natl <- cbind(natl, nds)
cat(sprintf("national pool dollar stats: %d rules\n", nrow(natl)))

lcb_of <- function(pool) {
  k <- round(pool$precision_train * pool$n_flagged_train)
  wilson_lcb(k, pool$n_flagged_train, LCB_Z)
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
  cap24 <- floor(b * n_te_rows)
  un24 <- rep(FALSE, n_te_rows)
  for (i in c(frozen, buffer)) {
    add <- sum(!un24[idx_te[[i]]])
    if (add > 0 && sum(un24) + add <= cap24) un24[idx_te[[i]]] <- TRUE
  }
  nb <- sum(un24)
  data.frame(workload = round(nb / n_te_rows, 4),
             precision = round(ifelse(nb > 0, sum(ie_te[un24]) / nb, NA), 4),
             dollar_recall = round(sum(ed_te[un24]) / sum(ed_te), 4))
}

res <- list()
for (target in TARGETS) {
  own <- readRDS(file.path(CACHEOLD, sprintf("pool_%s.rds", gsub("[^A-Za-z]", "", target))))
  rows_own_tr <- which(st == target & in_tr)
  ods <- dollar_stats(own, rows_own_tr, gsub("[^A-Za-z]", "", target))
  own <- cbind(own, ods)
  pool <- bind_rows(natl, own)
  pool$lcb99 <- lcb_of(pool)
  pool$eylb <- pool$lcb99 * pool$dph
  # blend dedup: identical rule keeps the higher-lcb version (as production)
  o <- order(-pool$lcb99)
  dup <- duplicated(paste(pool$hh[o], pool$rule[o], sep = "\r"))
  pool <- pool[o[!dup], , drop = FALSE]

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

  for (v in c("lcb99", "dpf", "dpflb", "eylb")) {
    for (b in BUDGETS) {
      ev <- walk_eval(pool[[v]], idx_tr, idx_te, nrow(tr), nrow(te), ie_te, ed_te, b)
      res[[length(res) + 1]] <- cbind(
        data.frame(target = target, variant = v, budget = b,
                   target_base_rate = round(mean(ie_te), 4)), ev)
    }
  }
  cat(sprintf("%-22s done\n", target))
  saveRDS(bind_rows(res), file.path(out_dir, "dollaryield_partial.rds"))
}
out <- bind_rows(res)
write.csv(out, file.path(out_dir, "dollaryield_audition.csv"), row.names = FALSE)
cat(sprintf("wrote %s (%d rows)\n",
            file.path(out_dir, "dollaryield_audition.csv"), nrow(out)))

cat("\nmedians (dollar recall / precision):\n")
sm <- out %>% group_by(variant, budget) %>%
  summarise(med_dollars = median(dollar_recall, na.rm = TRUE),
            med_prec = median(precision, na.rm = TRUE), .groups = "drop") %>%
  arrange(budget, desc(med_dollars))
print(as.data.frame(sm), row.names = FALSE)
