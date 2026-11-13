# Scores a case ranking on the evaluation benchmark (additional_data/eval_benchmark_<TAG>.rds), the way findings
# §46-§52 and the results map do. Built 2026-11-12 with build_eval_benchmark.R.
#
#   source("methods/eval_benchmark/score_ranking.R")
#   bench <- read_benchmark()                                   # additional_data/eval_benchmark_f1109.rds
#   tab <- score_ranking(bench, score, year = "2024")            # per state, at 5% and 10%
#   summarise_ranking(tab)                                      # pooled and median-state figures
#
# score: either a numeric vector with one value per case of that year, in frame_row order
#        (bench[bench$fiscal_year == year, ]), or a data frame with columns fiscal_year, state, hhldno and score.
#        Higher means review first. Every case needs a finite score, dropped cases included (they are not flagged,
#        but the flag count comes from all of a state's cases).
# The rule: at budget b, a state flags max(1, round(b x all of its cases that year)) cases (R's round, which rounds
# halves to even), filled with its highest-scored kept cases, ties broken by frame_row. Errors and error dollars are
# summed over the flagged cases; errors_all and dollars_all are over the state's kept cases. times_random is
# dollar_recall / b: 1 means the flags hold the error dollars a random pick of the same share would.
# Rule lists that take a rule whole or skip it are not a ranking; score them with their own walk, using the same
# kept cases and flag count.

read_benchmark <- function(path = "additional_data/eval_benchmark_f1109.rds") {
  b <- readRDS(path)
  need <- c("frame_row", "fiscal_year", "state", "hhldno", "error", "error_dollars", "kept")
  stopifnot(is.data.frame(b), all(need %in% names(b)), !is.null(attr(b, "frame_sha256")))
  b
}

score_ranking <- function(bench, score, year, budgets = c(0.05, 0.10)) {
  year <- as.character(year)
  B <- bench[bench$fiscal_year == year, , drop = FALSE]
  if (!nrow(B)) stop("no cases for fiscal year ", year)
  B <- B[order(B$frame_row), , drop = FALSE]
  if (is.data.frame(score)) {
    stopifnot(all(c("fiscal_year", "state", "hhldno", "score") %in% names(score)))
    s <- score[as.character(score$fiscal_year) == year, , drop = FALSE]
    k_b <- paste(B$state, B$hhldno); k_s <- paste(as.character(s$state), s$hhldno)
    if (anyDuplicated(k_s)) stop("score has duplicate (state, hhldno) keys for ", year)
    m <- match(k_b, k_s)
    if (anyNA(m)) stop(sum(is.na(m)), " benchmark cases have no score for ", year)
    sc <- as.numeric(s$score[m])
  } else {
    sc <- as.numeric(score)
    if (length(sc) != nrow(B)) stop(sprintf("score has %d values; fiscal year %s has %d cases", length(sc), year, nrow(B)))
  }
  if (!all(is.finite(sc))) stop("every case needs a finite score")
  SS <- split(seq_len(nrow(B)), B$state)
  out <- lapply(budgets, function(b) do.call(rbind, lapply(names(SS), function(s) {
    ix <- SS[[s]]; kk <- max(1L, round(b * length(ix))); ix2 <- ix[B$kept[ix]]
    sel <- ix2[order(-sc[ix2], ix2)[seq_len(min(kk, length(ix2)))]]
    data.frame(score_year = year, budget = b, state = s, cases = length(ix), kept = length(ix2), flags = kk,
               errors_caught = sum(B$error[sel]), dollars_caught = sum(B$error_dollars[sel]),
               errors_all = sum(B$error[ix2]), dollars_all = sum(B$error_dollars[ix2]), stringsAsFactors = FALSE)
  })))
  tab <- do.call(rbind, out); rownames(tab) <- NULL
  tab$precision <- tab$errors_caught / tab$flags
  tab$dollars_per_flag <- tab$dollars_caught / tab$flags
  tab$dollar_recall <- tab$dollars_caught / tab$dollars_all
  tab$times_random <- tab$dollar_recall / tab$budget
  tab$error_rate <- tab$errors_all / tab$kept
  tab
}

summarise_ranking <- function(tab) {
  out <- do.call(rbind, lapply(split(tab, list(tab$score_year, tab$budget), drop = TRUE), function(x) data.frame(
    score_year = x$score_year[1], budget = x$budget[1], states = nrow(x), flags = sum(x$flags),
    precision = sum(x$errors_caught) / sum(x$flags), base_rate = sum(x$errors_all) / sum(x$kept),
    dollar_recall = sum(x$dollars_caught) / sum(x$dollars_all),
    times_random = sum(x$dollars_caught) / sum(x$dollars_all) / x$budget[1],
    median_state_times_random = median(x$times_random), states_below_1x = sum(x$times_random < 1))))
  rownames(out) <- NULL
  out
}
