# ──────────────────────────────────────────────────────────────────────────────
# State similarity via NAIVE BAYES over rule firings (option D).
#
# Each state is summarized by its per-rule firing probabilities (the saved
# fire-rate matrices, computed on the ladder-collapsed high-precision
# shortlist). Distance between states = symmetrized KL divergence between
# their smoothed Bernoulli firing profiles, summed over rules; similarity is
# reported as 1 / (1 + KL). Compared with the inverse-frequency cosine:
#   - a rule firing EVERYWHERE but at different rates carries NB weight
#     (IDF weights it zero);
#   - a rule firing in few states on a handful of cases carries little NB
#     evidence (IDF gives it its maximum weight).
# Laplace smoothing (+0.5 / +1) uses per-state stratum sizes as denominators.
# The naive independence assumption double-counts overlapping rules; using
# the ladder-collapsed shortlist keeps that in check.
#
# Expects `reg_model_data` and state_similarity_v2/fire_rates_rules_by_state_
# <era>.rds (from state_similarity_fire_rates_v2.R, same RULES_CSV).
# Output -> state_similarity_v2/similarity_nb_<era>.csv
# ──────────────────────────────────────────────────────────────────────────────

suppressMessages(library(dplyr))

RULES_CSV <- "inclusion_rules_by_hh_size_v2/final_rules_highprecision_all_frames.csv"
ERAS <- list("2017_2019" = c("2017", "2018", "2019"),
             "2022_2024" = c("2022", "2023", "2024"))
out_dir <- "state_similarity_v2"

HH_SIZE_COL <- "cert_HH_size_FS_n"
hh_group_of <- function(n) {
  n <- suppressWarnings(as.numeric(as.character(n)))
  ifelse(is.na(n), NA_character_, ifelse(n <= 1, "1", ifelse(n <= 3, "2-3", "4+")))
}

# rules in the same order the fire-rate script used (distinct(rule, hh))
rules <- read.csv(RULES_CSV, stringsAsFactors = FALSE) %>% distinct(rule, hh)

for (era_name in names(ERAS)) {
  rate <- readRDS(file.path(out_dir,
          sprintf("fire_rates_rules_by_state_%s.rds", era_name)))
  stopifnot(nrow(rate) == nrow(rules))
  states <- colnames(rate)

  d <- reg_model_data %>% filter(fiscal_year %in% ERAS[[era_name]])
  d$hh_grp <- hh_group_of(d[[HH_SIZE_COL]])
  denom_tab <- table(factor(as.character(d$state), levels = states), d$hh_grp)
  denom <- t(denom_tab)[rules$hh, states, drop = FALSE] |> unname()  # rules x states

  k <- rate * denom                        # firing counts
  p <- (k + 0.5) / (denom + 1)             # smoothed Bernoulli rates

  # symmetrized KL, summed over rules, all pairs at once
  n_s <- length(states)
  kl <- matrix(0, n_s, n_s, dimnames = list(states, states))
  lp  <- log(p); l1p <- log1p(-p)
  for (a in seq_len(n_s)) {
    pa <- p[, a]
    # KL(a||b) + KL(b||a) summed over rules, vectorized over b
    term <- (pa - p) * (lp[, a] - lp) + ((1 - pa) - (1 - p)) * (l1p[, a] - l1p)
    kl[a, ] <- colSums(term)
  }
  sim <- 1 / (1 + kl)
  write.csv(round(sim, 5),
            file.path(out_dir, sprintf("similarity_nb_%s.csv", era_name)))
  cat(sprintf("era %s: NB similarity over %d rules x %d states | mean off-diag %.3f\n",
              era_name, nrow(rules), n_s, mean(sim[upper.tri(sim)])))

  for (st in c("Louisiana", "Washington", "Virginia")) {
    if (st %in% states) {
      v <- sort(sim[st, setdiff(states, st)], decreasing = TRUE)[1:8]
      cat(sprintf("  %s NB neighbors: %s\n", st,
                  paste(sprintf("%s (%.3f)", names(v), v), collapse = ", ")))
    }
  }
}
