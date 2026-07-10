# BLENDED frozen lists: merge each state's OWN mined pool with the national
# pool into ONE prioritized set, ranked by each rule's own-training Wilson
# LCB (national rules bounded on national 2022-23 train, state rules on the
# state's own 2022-23 train). Both statements read "at least this precision
# with X% confidence", so the merge is coherent and the LCB applies the
# certainty discount automatically -- a small-support state rule outranks
# national rules only if its precision overcomes the penalty.
#
# Two ranking variants: z = 2.326 (99%, the production stringency) and
# z = 2.054 (98%, slightly relaxed to let more state rules surface).
# Same freeze/walk protocol as frozen_list_experiment_v2.R (core to budget,
# buffer to 3x, walk on 2024 while capacity fits).
#
# Expects `reg_model_data`. Output:
#   methods/state_similarity_v2/transfer_benchmark_train2223_test24/
#     blended_frozen_results.csv  (one row per state x budget x variant,
#     with the deployed state-rule share)

suppressMessages(library(dplyr))
source("rule_mining_helpers.R")

TARGETS <- c("Louisiana", "Washington", "Virginia", "Arizona", "Connecticut",
             "Michigan", "North Carolina", "California", "Texas",
             "Mississippi", "New Jersey", "Colorado",
             "Maine", "Maryland", "Missouri", "Massachusetts",
             "District of Columbia", "Tennessee")
BUDGETS <- c(0.05, 0.10)
BUFFER_MULT <- 3
Z_VARIANTS <- c(lcb99 = 2.326, lcb98 = 2.054)
out_dir <- "methods/state_similarity_v2/transfer_benchmark_train2223_test24"

features <- c(
  "HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size",
  "expedited_i", "cat_elig", "rawben_rel_max", "medical_deductions",
  "shelter_expenses_by_hh_size", "utilities", "married", "homeless",
  "rawearn_by_hh_size", "rawunearn_by_hh_size", "rawgross_by_hh_size",
  "percent_abawd", "unc_rawben_rel_max",
  "months_since_cert_n", "count_divisible_by_100"
)
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
                       filter(fiscal_year %in% c("2022", "2023", "2024")),
                     features)$data
yr <- as.character(adf$fiscal_year)
st <- as.character(adf$state)

sig <- gsub("[^A-Za-z]", "", paste(sort(unique(st)), collapse = "_"))
nat_key <- if (nchar(sig) <= 80) sig else
  sprintf("%s_%08x", substr(sig, 1, 60),
          sum(utf8ToInt(sig) * seq_along(utf8ToInt(sig))) %% .Machine$integer.max)
natl <- readRDS(file.path(out_dir, "pool_cache", sprintf("pool_%s.rds", nat_key)))
natl$source <- "national"
cat(sprintf("national pool: %d rules\n", nrow(natl)))

lcb_at <- function(pool, z) {
  k <- round(pool$precision_train * pool$n_flagged_train)
  wilson_lcb(k, pool$n_flagged_train, z)
}

res <- list()
for (target in TARGETS) {
  own_path <- file.path(out_dir, "pool_cache",
                        sprintf("pool_%s.rds", gsub("[^A-Za-z]", "", target)))
  own <- readRDS(own_path)
  own$source <- "state"
  pool <- bind_rows(natl, own)
  for (v in names(Z_VARIANTS)) pool[[v]] <- lcb_at(pool, Z_VARIANTS[[v]])
  # same rule mined by both pools: keep the higher-bound version
  pool <- pool %>% arrange(desc(lcb99)) %>% distinct(hh, rule, .keep_all = TRUE)

  tr <- adf[st == target & yr %in% c("2022", "2023"), , drop = FALSE]
  te <- adf[st == target & yr == "2024", , drop = FALSE]
  tg_te <- targets_of(te)
  strata_tr <- lapply(setNames(nm = c("1", "2-3", "4+")), function(h)
    which(hh_group_of(tr$cert_HH_size_FS_n) %in% h))
  strata_te <- lapply(setNames(nm = c("1", "2-3", "4+")), function(h)
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
  saveRDS(bind_rows(res), file.path(out_dir, "blended_frozen_partial.rds"))
}
out <- bind_rows(res)
write.csv(out, file.path(out_dir, "blended_frozen_results.csv"), row.names = FALSE)
cat(sprintf("wrote %s (%d rows)\n",
            file.path(out_dir, "blended_frozen_results.csv"), nrow(out)))
