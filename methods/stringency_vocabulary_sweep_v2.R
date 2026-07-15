# Stringency x vocabulary sweep on the train2223/test24 benchmark (EXPLORATORY:
# every combination is scored on 2024, so whatever wins here is selected ON the
# test year -- adopting a winner requires the national-scale 22/23 tuning step
# before it ships; see the discussion of 2026-07-15).
#
# Question: does raising the selection stringency (LCB z and/or the support
# floor) rescue the five-frame vocabulary, whose 3.3x candidate pool at fixed
# z = 2.326 lost budget-filled precision (blended_frozen_results_5frames.csv)?
#
# Vocabulary arms, all built from CACHED pools (no re-mining):
#   orig    the original any-error pools (pool_cache/) -- the published anchor
#   pooled  five-frame pool rules whose mined_frames includes any_error
#   typed   five-frame pool rules whose mined_frames includes a typed frame
#   all5    the full five-frame pools (pool_cache_5frames/)
# pooled/typed overlap on rules mined by both; that is the point -- they are
# the vocabularies typed-only / pooled-only mining would have produced, under
# the same dedup.
#
# Grid: z in {2.326 (99%), 2.576 (99.5%), 2.74 (multiplicity-matched: keeps the
# expected lucky-survivor count of z=2.326 at 48k candidates when the pool is
# 3.3x larger), 3.09 (99.9%)}; support floor in {30, 50, 100}.
# Freeze/walk protocol identical to blended_frozen_lists_v2.R.
#
# Expects `reg_model_data`. Output:
#   methods/state_similarity_v2/transfer_benchmark_train2223_test24/
#     stringency_vocabulary_sweep.csv

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
Z_GRID <- c(z2.326 = 2.326, z2.576 = 2.576, z2.74 = 2.74, z3.09 = 3.09)
N_FLOORS <- c(30, 50, 100)
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

sig <- gsub("[^A-Za-z]", "", paste(sort(unique(st)), collapse = "_"))
nat_key_old <- if (nchar(sig) <= 80) sig else
  sprintf("%s_%08x", substr(sig, 1, 60),
          sum(utf8ToInt(sig) * seq_along(utf8ToInt(sig))) %% .Machine$integer.max)

natl5   <- readRDS(file.path(CACHE5, "pool_national.rds"))
natlold <- readRDS(file.path(CACHEOLD, sprintf("pool_%s.rds", nat_key_old)))
natlold$engines <- NA_character_; natlold$mined_frames <- "any_error"
cat(sprintf("national pools: five-frame %d rules | original any-error %d rules\n",
            nrow(natl5), nrow(natlold)))

TYPED <- c("earned_overissuance", "unearned_overissuance",
           "underissuance", "other_error")
has_frame <- function(pool, frames) {
  vapply(strsplit(pool$mined_frames, "+", fixed = TRUE),
         function(f) any(f %in% frames), logical(1))
}

lcb_of <- function(pool, z) {
  k <- round(pool$precision_train * pool$n_flagged_train)
  wilson_lcb(k, pool$n_flagged_train, z)
}

# walk once per (arm, z, floor, budget): freeze on the state's 2022-23, then
# walk the frozen order on 2024 while capacity fits
walk_eval <- function(pool, idx_tr, idx_te, n_tr_rows, n_te_rows, tg_te, b) {
  cap <- floor(b * n_tr_rows); cap_buf <- floor(BUFFER_MULT * b * n_tr_rows)
  un <- rep(FALSE, n_tr_rows); n_in <- 0L
  frozen <- integer(0); buffer <- integer(0)
  ord <- order(-pool$lcb)
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
  ownold$engines <- NA_character_; ownold$mined_frames <- "any_error"
  cols <- c("hh", "rule", "mined_frames", "n_flagged_train", "precision_train")
  pool5 <- bind_rows(natl5[, cols] %>% mutate(pool = "national"),
                     own5[, cols]  %>% mutate(pool = "state"))
  poolo <- bind_rows(natlold[, cols] %>% mutate(pool = "national"),
                     ownold[, cols]  %>% mutate(pool = "state"))

  tr <- adf[st == target & yr %in% TRAIN_YEARS, , drop = FALSE]
  te <- adf[st == target & yr == TEST_YEAR, , drop = FALSE]
  tg_te <- targets_of(te)
  strata_tr <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(tr$cert_HH_size_FS_n) %in% h))
  strata_te <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(te$cert_HH_size_FS_n) %in% h))
  # one flags pass per pool family; every arm subsets by row index
  idx5_tr <- flags_for_rules(pool5, tr, strata_tr, label = "")
  idx5_te <- flags_for_rules(pool5, te, strata_te, label = "")
  idxo_tr <- flags_for_rules(poolo, tr, strata_tr, label = "")
  idxo_te <- flags_for_rules(poolo, te, strata_te, label = "")

  arms <- list(
    orig   = list(pool = poolo, tr = idxo_tr, te = idxo_te,
                  sub = rep(TRUE, nrow(poolo))),
    pooled = list(pool = pool5, tr = idx5_tr, te = idx5_te,
                  sub = has_frame(pool5, "any_error")),
    typed  = list(pool = pool5, tr = idx5_tr, te = idx5_te,
                  sub = has_frame(pool5, TYPED)),
    all5   = list(pool = pool5, tr = idx5_tr, te = idx5_te,
                  sub = rep(TRUE, nrow(pool5)))
  )
  for (arm in names(arms)) {
    a <- arms[[arm]]
    for (nf in N_FLOORS) {
      keep <- which(a$sub & a$pool$n_flagged_train >= nf)
      for (zn in names(Z_GRID)) {
        p <- a$pool[keep, , drop = FALSE]
        p$lcb <- lcb_of(p, Z_GRID[[zn]])
        # blend dedup: same rule in both pools keeps the higher-bound version
        o <- order(-p$lcb)
        dup <- duplicated(paste(p$hh[o], p$rule[o], sep = "\r"))
        sel <- o[!dup]
        p2 <- p[sel, , drop = FALSE]
        k2 <- keep[sel]
        for (b in BUDGETS) {
          ev <- walk_eval(p2, a$tr[k2], a$te[k2], nrow(tr), nrow(te), tg_te, b)
          res[[length(res) + 1]] <- cbind(
            data.frame(target = target, arm = arm, n_floor = nf, z = zn,
                       budget = b, n_pool = nrow(p2),
                       target_base_rate = round(mean(tg_te$ie), 4)), ev)
        }
      }
    }
    cat(sprintf("%-22s %s done\n", target, arm))
  }
  saveRDS(bind_rows(res), file.path(out_dir, "stringency_sweep_partial.rds"))
}
out <- bind_rows(res)
write.csv(out, file.path(out_dir, "stringency_vocabulary_sweep.csv"), row.names = FALSE)
cat(sprintf("wrote %s (%d rows)\n",
            file.path(out_dir, "stringency_vocabulary_sweep.csv"), nrow(out)))

cat("\nmedians by arm x z x floor (precision / dollar recall):\n")
sm <- out %>% group_by(arm, z, n_floor, budget) %>%
  summarise(med_prec = median(precision, na.rm = TRUE),
            med_dollars = median(dollar_recall, na.rm = TRUE), .groups = "drop") %>%
  arrange(budget, desc(med_prec))
print(as.data.frame(sm), row.names = FALSE)
