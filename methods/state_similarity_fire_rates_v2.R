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
# Expects `reg_model_data`. Outputs -> methods/state_similarity_v2/.
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

out_dir <- "methods/state_similarity_v2"
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

## ── 2. Per-ERA fire-rate matrices and similarities ────────────────────────────
# Flags were evaluated once on all years; each era's matrices come from
# slicing those flags. Eras are kept separate because state error structure
# drifts (the LA transfer showed era match is load-bearing).

ERAS <- list("2017_2019" = c("2017", "2018", "2019"),
             "2022_2024" = c("2022", "2023", "2024"))

st <- as.character(adf[[STATE_COL]])
yr <- as.character(adf[["fiscal_year"]])
states <- sort(unique(st))

cosine <- function(M) {              # columns = states
  Mn <- sweep(M, 2, sqrt(colSums(M^2)) + 1e-12, "/")
  t(Mn) %*% Mn
}

for (era_name in names(ERAS)) {
  in_era <- yr %in% ERAS[[era_name]]
  cat(sprintf("\n──── era %s: %d rows ────\n", era_name, sum(in_era)))

  denom <- sapply(states, function(s) {
    rows <- which(st == s & in_era)
    vapply(rules$hh, function(h) length(intersect(rows, strata[[h]])), numeric(1))
  })  # rules x states
  num <- sapply(states, function(s) {
    vapply(idx, function(ix) sum(st[ix] == s & in_era[ix]), numeric(1))
  })  # rules x states

  rate <- num / pmax(denom, 1)
  cat(sprintf("matrix: %d rules x %d states\n", nrow(rate), ncol(rate)))
  saveRDS(rate, file.path(out_dir,
          sprintf("fire_rates_rules_by_state_%s.rds", era_name)))

  sim_sqrt <- cosine(sqrt(rate))
  sim_raw  <- cosine(rate)

  # Inverse-frequency weighting: sharing a RARELY-firing rule is stronger
  # evidence of similarity than sharing a broad one. Each rule's sqrt fire
  # rates are scaled by log(N_states / n_states_where_it_fires) before the
  # cosine (TF-IDF, with states as documents and rules as terms).
  df_states <- rowSums(rate > 0)
  idf       <- log(ncol(rate) / pmax(df_states, 1))
  sim_idf   <- cosine(sqrt(rate) * idf)

  write.csv(round(sim_sqrt, 4),
            file.path(out_dir, sprintf("similarity_matrix_sqrt_%s.csv", era_name)))
  write.csv(round(sim_idf, 4),
            file.path(out_dir, sprintf("similarity_idf_%s.csv", era_name)))

  res <- data.frame(
    state = states,
    cosine_sqrt = sim_sqrt[, FOCAL],
    cosine_idf  = sim_idf[, FOCAL],
    cosine_raw  = sim_raw[, FOCAL],
    n_rows = as.integer(table(st[in_era])[states])
  ) %>%
    filter(state != FOCAL) %>%
    arrange(desc(cosine_sqrt)) %>%
    mutate(rank_sqrt = row_number(),
           rank_idf = rank(-cosine_idf),
           rank_raw = rank(-cosine_raw))
  write.csv(res, file.path(out_dir,
            sprintf("similarity_to_%s_%s.csv", FOCAL, era_name)), row.names = FALSE)

  cat(sprintf("states most similar to %s (%s; sqrt cosine, idf rank in parens):\n",
              FOCAL, era_name))
  top <- head(res, 10)
  for (i in seq_len(nrow(top)))
    cat(sprintf("  %2d. %-22s %.4f  (idf rank %2.0f) | rows %d\n",
                i, top$state[i], top$cosine_sqrt[i], top$rank_idf[i], top$n_rows[i]))
}
cat(sprintf("\nWrote per-era matrices to %s/\n", out_dir))
