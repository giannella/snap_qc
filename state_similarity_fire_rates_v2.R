# ──────────────────────────────────────────────────────────────────────────────
# State similarity by rule FIRE RATES (option A of the transferability designs).
#
# For every state, compute the share of its caseload flagged by each national
# shortlist rule (within the rule's household-size stratum). States whose
# caseloads exercise the same rule regions get high cosine similarity — a
# label-free measure that is stable even for small states, used to pick donor
# pools for pooled mining/tuning (first target: Louisiana).
#
# Rules are evaluated ONCE on the full national frame via the shared
# condition-index evaluator; fire rates are then sliced by state.
#
# Primary metric: cosine on sqrt(fire rates) — damps the dominance of a few
# broad rules. Cosine on raw rates is reported as a stability check.
#
# Expects `reg_model_data`. Outputs -> state_similarity_v2/.
# ──────────────────────────────────────────────────────────────────────────────

suppressMessages(library(dplyr))
source("rule_mining_helpers.R")

YEARS      <- c("2017", "2018", "2019", "2022", "2023", "2024")  # all available
FOCAL      <- "Louisiana"
RULES_CSV  <- "inclusion_rules_by_hh_size_v2/final_rules_highprecision_all_frames.csv"
STATE_COL  <- "state"
HH_SIZE_COL <- "cert_HH_size_FS_n"
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

out_dir <- "state_similarity_v2"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

## ── 1. Data + rules ───────────────────────────────────────────────────────────

adf <- prep_features(reg_model_data %>% filter(fiscal_year %in% YEARS),
                     features)$data
rules <- read.csv(RULES_CSV, stringsAsFactors = FALSE) %>% distinct(rule, hh)
cat(sprintf("%d rows (%s), %d rules\n", nrow(adf),
            paste(range(as.numeric(YEARS)), collapse = "-"), nrow(rules)))

strata <- lapply(setNames(nm = unique(rules$hh)), function(h)
  which(hh_group_of(adf[[HH_SIZE_COL]]) %in% h))

t0 <- Sys.time()
idx <- flags_for_rules(rules, adf, strata, label = "national frame, once")
cat(sprintf("flag evaluation: %.0fs\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

## ── 2. Fire-rate matrix: states x rules ───────────────────────────────────────

st <- as.character(adf[[STATE_COL]])
states <- sort(unique(st))
# per-state stratum sizes (denominators)
denom <- sapply(states, function(s) {
  rows <- which(st == s)
  vapply(rules$hh, function(h) length(intersect(rows, strata[[h]])), numeric(1))
})  # rules x states

# numerators: count of each rule's flags falling in each state
state_of_row <- st
num <- sapply(states, function(s) {
  vapply(idx, function(ix) sum(state_of_row[ix] == s), numeric(1))
})  # rules x states

rate <- num / pmax(denom, 1)          # rules x states fire rates
cat(sprintf("matrix: %d rules x %d states | median caseload rows/state: %d\n",
            nrow(rate), ncol(rate), median(table(st))))

## ── 3. Cosine similarity to the focal state ───────────────────────────────────

cosine <- function(M) {              # columns = states
  Mn <- sweep(M, 2, sqrt(colSums(M^2)) + 1e-12, "/")
  t(Mn) %*% Mn
}
sim_sqrt <- cosine(sqrt(rate))
sim_raw  <- cosine(rate)

res <- data.frame(
  state = states,
  cosine_sqrt = sim_sqrt[, FOCAL],
  cosine_raw  = sim_raw[, FOCAL],
  n_rows = as.integer(table(st)[states])
) %>%
  filter(state != FOCAL) %>%
  arrange(desc(cosine_sqrt)) %>%
  mutate(rank_sqrt = row_number(),
         rank_raw = rank(-cosine_raw))

write.csv(res, file.path(out_dir, sprintf("similarity_to_%s.csv", FOCAL)),
          row.names = FALSE)
write.csv(data.frame(state = states, round(sim_sqrt, 4)),
          file.path(out_dir, "similarity_matrix_sqrt.csv"), row.names = FALSE)

cat(sprintf("\nStates most similar to %s (cosine on sqrt fire rates; raw-rate rank in parens):\n", FOCAL))
top <- head(res, 12)
for (i in seq_len(nrow(top)))
  cat(sprintf("  %2d. %-22s %.4f  (raw rank %2.0f) | rows %d\n",
              i, top$state[i], top$cosine_sqrt[i], top$rank_raw[i], top$n_rows[i]))
cat(sprintf("\nLeast similar (bottom 5):\n"))
bot <- tail(res, 5)
for (i in seq_len(nrow(bot)))
  cat(sprintf("      %-22s %.4f\n", bot$state[i], bot$cosine_sqrt[i]))
cat(sprintf("\nWrote %s and the full matrix to %s/\n",
            sprintf("similarity_to_%s.csv", FOCAL), out_dir))
