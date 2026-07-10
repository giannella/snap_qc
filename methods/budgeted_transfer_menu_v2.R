# ──────────────────────────────────────────────────────────────────────────────
# Budget-constrained view of the transfer benchmark: given a review capacity
# (share of the target state's caseload), what does each approach deliver?
#
# Rules are added in descending pool-train LCB order; a rule joins the union
# only if the union stays within budget (oversized rules are skipped, later
# smaller ones may still fit). Reported precision/recall are on the target
# state, which the transfer pools never saw. Reuses the benchmark's cached
# pools (state_similarity_v2/transfer_benchmark/pool_cache/).
#
# Expects `reg_model_data`. Output ->
#   state_similarity_v2/transfer_benchmark/budgeted_menu_results.csv
# ──────────────────────────────────────────────────────────────────────────────

suppressMessages(library(dplyr))
source("rule_mining_helpers.R")

TARGETS <- c("Louisiana", "Washington", "Virginia", "Arizona", "Connecticut",
             "Michigan", "North Carolina", "California", "Texas",
             "Mississippi", "New Jersey", "Colorado")
K_NEIGHBORS <- 5
YEARS   <- c("2022", "2023", "2024")
BUDGETS <- c(0.05, 0.10)

SIM_FILES <- c(fire    = "state_similarity_v2/similarity_matrix_sqrt_2022_2024.csv",
               idf     = "state_similarity_v2/similarity_idf_2022_2024.csv",
               nb      = "state_similarity_v2/similarity_nb_2022_2024.csv",
               policy  = "state_similarity_v2/similarity_policy_2022_2024.csv",
               blended = "state_similarity_v2/similarity_blended_2022_2024.csv")
INCLUDE_LOO_NATIONAL <- TRUE   # honest national control (cached pools)
NATIONAL_CSV <- "inclusion_rules_by_hh_size_v2/final_rules_highprecision_all_frames.csv"
CACHE_DIR <- "state_similarity_v2/transfer_benchmark/pool_cache"
OUT_CSV   <- "state_similarity_v2/transfer_benchmark/budgeted_menu_results.csv"

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

adf <- prep_features(reg_model_data %>% filter(fiscal_year %in% YEARS),
                     features)$data
sims <- lapply(SIM_FILES, function(f) {
  S <- as.matrix(read.csv(f, row.names = 1, check.names = FALSE))
  colnames(S) <- rownames(S)
  S
})
natl <- read.csv(NATIONAL_CSV, stringsAsFactors = FALSE) %>%
  distinct(rule, hh, .keep_all = TRUE)

pool_rules <- function(pool_states) {
  sig <- gsub("[^A-Za-z]", "", paste(sort(pool_states), collapse = "_"))
  key <- if (nchar(sig) <= 80) sig else
    sprintf("%s_%08x", substr(sig, 1, 60),
            sum(utf8ToInt(sig) * seq_along(utf8ToInt(sig))) %% .Machine$integer.max)
  f <- file.path(CACHE_DIR, sprintf("pool_%s.rds", key))
  if (file.exists(f)) readRDS(f) else NULL
}

budgeted_union <- function(rules_df, stat, idx, n_rows, tg, budget) {
  cap <- floor(budget * n_rows)
  un <- rep(FALSE, n_rows)
  n_used <- 0L; n_in <- 0L
  for (i in order(-stat)) {
    if (is.na(stat[i])) next
    ix <- idx[[i]]
    add <- sum(!un[ix])
    if (n_in + add <= cap) {
      un[ix] <- TRUE; n_in <- n_in + add; n_used <- n_used + 1L
    }
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
  tgt <- adf[adf[[STATE_COL]] == target, , drop = FALSE]
  tg  <- targets_of(tgt)
  strata_t <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(tgt[[HH_SIZE_COL]]) %in% h))
  cat(sprintf("\n== %s (%d rows, base %.1f%%)\n", target, nrow(tgt), 100 * mean(tg$ie)))

  sets <- list(national_asis = list(rules = natl, stat = natl$precision_train_lcb))
  if (INCLUDE_LOO_NATIONAL) {
    loo <- pool_rules(setdiff(sort(unique(as.character(adf[[STATE_COL]]))), target))
    if (!is.null(loo))
      sets$national_loo <- list(rules = loo, stat = loo$precision_train_lcb)
  }
  for (sim_name in names(sims)) {
    S <- sims[[sim_name]]
    if (!target %in% rownames(S)) next
    nb <- sort(S[target, setdiff(colnames(S), target)], decreasing = TRUE)
    rl <- pool_rules(names(nb)[seq_len(K_NEIGHBORS)])
    if (!is.null(rl))
      sets[[paste0("transfer_", sim_name)]] <-
        list(rules = rl, stat = rl$precision_train_lcb)
  }
  for (nm in names(sets)) {
    rl <- sets[[nm]]$rules
    idx <- flags_for_rules(rl, tgt, strata_t, label = "")
    for (b in BUDGETS) {
      out <- budgeted_union(rl, sets[[nm]]$stat, idx, nrow(tgt), tg, b)
      out$target <- target; out$approach <- nm; out$budget <- b
      res[[length(res) + 1]] <- out
      cat(sprintf("  %-18s budget %2.0f%%: %4d rules, workload %5.1f%%, prec %.3f, $%3.0f%%\n",
                  nm, 100 * b, out$n_rules_used, 100 * out$workload,
                  out$precision, 100 * out$dollar_recall))
    }
  }
}
out <- bind_rows(res)
write.csv(out, OUT_CSV, row.names = FALSE)
cat(sprintf("\nwrote %s (%d rows)\n", OUT_CSV, nrow(out)))
