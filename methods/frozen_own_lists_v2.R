# Frozen lists from each state's OWN mined pool (the second regime of the
# two-regime rule), built exactly like the national frozen lists: fill the
# state's own-pool ranking against its 2022-23 caseload to the budget (core)
# and to 3x depth (buffer); deploy by walking the list on 2024 while
# capacity fits. Own pools come from the deployment benchmark cache.
#
# Also writes the TWO-REGIME HANDOFF table: per state x budget, the regime
# chosen (by 2024 precision of the batch-filled arms, as in
# visualize_two_regime_choice_v2.R), and how many rules that state would
# actually be handed (shipped = core + buffer) and typically run (activated).
#
# Expects `reg_model_data`. Outputs (same folder):
#   frozen_lists/frozen_own_list_<state>_budget05/10.csv
#   frozen_own_list_results.csv
#   two_regime_handoff.csv

suppressMessages(library(dplyr))
source("rule_mining_helpers.R")

TARGETS <- c("Louisiana", "Washington", "Virginia", "Arizona", "Connecticut",
             "Michigan", "North Carolina", "California", "Texas",
             "Mississippi", "New Jersey", "Colorado",
             "Maine", "Maryland", "Missouri", "Massachusetts",
             "District of Columbia", "Tennessee")
BUDGETS <- c(0.05, 0.10)
BUFFER_MULT <- 3
out_dir <- "methods/state_similarity_v2/transfer_benchmark_train2223_test24"
list_dir <- file.path(out_dir, "frozen_lists")

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

res <- list()
for (target in TARGETS) {
  key <- gsub("[^A-Za-z]", "", target)
  pool_path <- file.path(out_dir, "pool_cache", sprintf("pool_%s.rds", key))
  if (!file.exists(pool_path)) { cat(sprintf("%s: no own pool\n", target)); next }
  pool <- readRDS(pool_path)
  ord <- order(-pool$precision_train_lcb)

  tr <- adf[st == target & yr %in% c("2022", "2023"), , drop = FALSE]
  te <- adf[st == target & yr == "2024", , drop = FALSE]
  tg_te <- targets_of(te)
  strata_tr <- lapply(setNames(nm = c("1", "2-3", "4+")), function(h)
    which(hh_group_of(tr$cert_HH_size_FS_n) %in% h))
  strata_te <- lapply(setNames(nm = c("1", "2-3", "4+")), function(h)
    which(hh_group_of(te$cert_HH_size_FS_n) %in% h))
  idx_tr <- flags_for_rules(pool, tr, strata_tr, label = "")
  idx_te <- flags_for_rules(pool, te, strata_te, label = "")

  for (b in BUDGETS) {
    cap <- floor(b * nrow(tr)); cap_buf <- floor(BUFFER_MULT * b * nrow(tr))
    un <- rep(FALSE, nrow(tr)); n_in <- 0L
    frozen <- integer(0); buffer <- integer(0)
    for (i in ord) {
      ix <- idx_tr[[i]]
      add <- sum(!un[ix])
      if (add == 0) next
      if (n_in + add <= cap) {
        un[ix] <- TRUE; n_in <- n_in + add; frozen <- c(frozen, i)
      } else if (n_in + add <= cap_buf) {
        un[ix] <- TRUE; n_in <- n_in + add; buffer <- c(buffer, i)
      }
    }
    cap24 <- floor(b * nrow(te))
    un24 <- rep(FALSE, nrow(te)); n_used <- 0L
    for (i in c(frozen, buffer)) {
      add <- sum(!un24[idx_te[[i]]])
      if (add > 0 && sum(un24) + add <= cap24) {
        un24[idx_te[[i]]] <- TRUE; n_used <- n_used + 1L
      }
    }
    nb <- sum(un24); kb <- sum(tg_te$ie[un24]); db <- sum(tg_te$ed[un24])
    res[[length(res) + 1]] <- data.frame(
      target = target, budget = b,
      n_rules_frozen = length(frozen), n_rules_buffer = length(buffer),
      n_rules_deployed = n_used,
      workload_deployed = round(nb / nrow(te), 4),
      precision_deployed = round(ifelse(nb > 0, kb / nb, NA), 4),
      dollar_recall_deployed = round(db / sum(tg_te$ed), 4),
      target_base_rate = round(mean(tg_te$ie), 4))
    cat(sprintf("%-22s %2.0f%%: %3d+%3d shipped, %3d deployed | wkld %4.1f%% | prec %.3f\n",
                target, 100 * b, length(frozen), length(buffer), n_used,
                100 * nb / nrow(te), ifelse(nb > 0, kb / nb, NA)))
    sel <- c(frozen, buffer)
    hand <- pool[sel, c("rule", "hh", "n_flagged_train",
                        "precision_train", "precision_train_lcb")]
    hand$rank <- seq_along(sel)
    hand$role <- rep(c("core", "buffer"), c(length(frozen), length(buffer)))
    write.csv(hand, file.path(list_dir, sprintf("frozen_own_list_%s_budget%02.0f.csv",
                                                gsub(" ", "_", target), 100 * b)),
              row.names = FALSE)
  }
}
own <- bind_rows(res)
write.csv(own, file.path(out_dir, "frozen_own_list_results.csv"), row.names = FALSE)

## ── two-regime handoff table ──────────────────────────────────────────────────
menu <- bind_rows(
  read.csv(file.path(out_dir, "deployment_menu_train2223_test24.csv")),
  read.csv(file.path(out_dir, "deployment_menu_workshop_extension.csv"))) %>%
  filter(approach %in% c("national_all", "own_state")) %>%
  distinct(target, approach, budget, .keep_all = TRUE)
regime <- menu %>% group_by(target, budget) %>%
  slice_max(precision, n = 1, with_ties = FALSE) %>% ungroup() %>%
  transmute(target, budget,
            regime = ifelse(approach == "national_all",
                            "national", "own rules"))
natl <- read.csv(file.path(out_dir, "frozen_list_results.csv")) %>%
  transmute(target, budget, regime = "national",
            n_shipped = n_rules_frozen + n_rules_buffer,
            n_activated = n_rules_deployed)
ownx <- own %>%
  transmute(target, budget, regime = "own rules",
            n_shipped = n_rules_frozen + n_rules_buffer,
            n_activated = n_rules_deployed)
handoff <- regime %>%
  left_join(bind_rows(natl, ownx), by = c("target", "budget", "regime"))
write.csv(handoff, file.path(out_dir, "two_regime_handoff.csv"), row.names = FALSE)
cat("\n== two-regime handoff ==\n")
print(as.data.frame(handoff %>% arrange(budget, target)), row.names = FALSE)
