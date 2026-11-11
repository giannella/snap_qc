# Descriptive (2026-10-29, after the confirmation readout): what each state's
# top 5% under the confirmed score looks like - how many cases, how many votes
# at the top and at the bottom of the list, how many unflagged cases get in.
#   SR_WINDOW=fy2022 Rscript methods/vote_scoring/top5_profile.R
# Same setup as confirm_window.R (window pool grouped on its mining years,
# representative vote over 300 rules, frozen FY2019 curve and size model), no
# bootstrap. Writes confirm/<window>/top5_cases_seed*.csv (one row per listed
# case) and confirm/<window>/top5_states.csv (one row per state x seed).
WINDOW_ARG <- Sys.getenv("SR_WINDOW"); stopifnot(WINDOW_ARG %in% c("fy2022", "fy2024"))
source("methods/v270_seed_replicates/common.R")
source("methods/vote_scoring/vote_groups.R"); source("methods/vote_scoring/vote_fit.R")
OUT <- file.path("methods/vote_scoring/confirm", WINDOW)
POOLS <- list(
  fy2022 = c(`117` = "methods/v270_cpi_benchmark/fy2022_cpi/cache/bench_national_117.rds",
             `118` = "methods/v270_seed_replicates/fy2022/pools/admitted_final_seed118.rds",
             `119` = "methods/v270_seed_replicates/fy2022/pools/admitted_final_seed119.rds"),
  fy2024 = c(`117` = "methods/v270_cpi_benchmark/fy2024_cpi/cache/bench_national_117.rds",
             `118` = "methods/v270_seed_replicates/pools/admitted_final_seed118.rds",
             `119` = "methods/v270_seed_replicates/pools/admitted_final_seed119.rds"))[[WINDOW]]
set.seed(if (WINDOW == "fy2022") 20261029L else 20261030L)

F <- load_frame("final")
tr <- which(F$is_tr); te <- which(F$yr == TEST_YEAR); NT <- length(te)
S_tr <- strata_of(F$hh[tr]); S_te <- strata_of(F$hh[te])
ie <- F$ie[te]; ed <- F$ed[te]; hh <- F$hh[te]; st <- F$st[te]
ben <- num(F$adf$rawben[te]); hsz <- num(F$adf$cert_HH_size_FS_n[te])
ie_tr <- F$ie[tr]

one_seed <- function(s) {
  fz <- readRDS(sprintf("methods/vote_scoring/fy2019/frozen_seed%s.rds", s))
  pool <- read_pool(POOLS[[s]], TRAIN_YEARS)
  rules <- top_rules(pool)
  fl_tr <- flags_for_rules(rules, F$adf[tr, , drop = FALSE], S_tr)
  stopifnot(all(lengths(fl_tr) == rules$n), all(vapply(fl_tr, function(ix) sum(ie_tr[ix]), numeric(1)) == rules$k))
  al <- allot(build_groups(rules, fl_tr), 300L, "rep")
  vote <- votes_of(al, flags_for_rules(al, F$adf[te, , drop = FALSE], S_te), NT)$vote
  p <- pred_curve(fz$curve, vote, hh)
  full <- p * predict(fz$size_fit, size_data(ed, hh, ben, hsz, vote), type = "response")
  cases <- bind_rows(lapply(split(seq_len(NT), st), function(ix) {
    k <- max(1L, round(0.05 * length(ix)))
    f <- ix[order(-full[ix], -ben[ix], runif(length(ix)))][seq_len(k)]
    data.frame(seed = s, state = st[f[1]], state_cases = length(ix), rank = seq_len(k), hh = hh[f], vote = vote[f],
               p_error = p[f], expected = full[f], benefit = ben[f], error = ie[f], error_dollars = ed[f])
  }))
  write.csv(cases, file.path(OUT, sprintf("top5_cases_seed%s.csv", s)), row.names = FALSE)
  stamp("seed %s done", s)
  cases %>% group_by(seed, state, state_cases) %>%
    summarise(listed = n(), vote_rank1 = vote[1], vote_max = max(vote), vote_median = median(vote),
              vote_last = vote[n()], vote_min = min(vote), share_vote0 = mean(vote == 0),
              precision = mean(error), per_case = mean(error_dollars), .groups = "drop")
}
states <- bind_rows(lapply(names(POOLS), one_seed))
write.csv(states, file.path(OUT, "top5_states.csv"), row.names = FALSE)
stamp("%s: top-5%% profile written", WINDOW)
