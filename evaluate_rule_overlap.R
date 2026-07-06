# evaluate_rule_overlap.R
# Measures empirical overlap between inclusion rules: how much each pair of rules
# targets the same cases, within a household-size stratum. Report only. Nothing is
# excluded here. Use it to see where the redundancy actually lives before deciding
# whether to act on it.
#
# Two overlap views per stratum:
#   error_jaccard - overlap of the ERROR cases each rule flags (shared targeting)
#   flag_jaccard  - overlap of ALL cases each rule flags (shared footprint)
#
# Outputs:
#   rule_overlap_by_rule.csv  - one row per rule: nearest neighbour and overlap summaries
#   rule_overlap_pairs.csv    - every rule pair above PAIR_REPORT_MIN, for inspection

# ---- config ----------------------------------------------------------------
RULE_CSV        <- "final_by_HHsize_inclusion_rules_highprecision.csv"
CASE_DATA       <- reg_model_data  # path to the scored case data (.rds or .csv)
HH_COL          <- "HH_size_n"
ERROR_STATUS    <- "error_status"
FOCAL_ERROR     <- "earned_overissuance"
RULE_COL        <- "rule"
HH_RULE_COL     <- "hh_size"
ID_COL          <- "rule_id"
PAIR_REPORT_MIN <- 0.50                  # only list pairs whose error_jaccard is at least this
NEAR_DUP_MIN    <- 0.80                  # a pair at or above this counts as a near-duplicate
OUT_BY_RULE     <- "rule_overlap_by_rule.csv"
OUT_PAIRS       <- "rule_overlap_pairs.csv"

hh_group_of <- function(n) {
  g <- as.character(pmin(as.integer(n), 5L))
  ifelse(g == "5", "5+", g)
}

jaccard <- function(a, b) {
  if (length(a) == 0 && length(b) == 0) return(NA_real_)
  u <- length(union(a, b))
  if (u == 0) return(NA_real_)
  length(intersect(a, b)) / u
}

# ---- load ------------------------------------------------------------------
rules <- read.csv(RULE_CSV, stringsAsFactors = FALSE, check.names = FALSE)
rules[[HH_RULE_COL]] <- as.character(rules[[HH_RULE_COL]])
dat <- reg_model_data
dat$.grp <- hh_group_of(dat[[HH_COL]])

by_rule <- list()
pairs   <- list()

for (hh in unique(rules[[HH_RULE_COL]])) {
  ridx <- which(rules[[HH_RULE_COL]] == hh)
  if (length(ridx) < 2L) next
  sub <- dat[dat$.grp == hh, , drop = FALSE]
  if (nrow(sub) == 0) next
  err <- which(sub[[ERROR_STATUS]] == FOCAL_ERROR)

  # row sets each rule flags, restricted to error cases and to all cases
  err_sets  <- vector("list", length(ridx))
  flag_sets <- vector("list", length(ridx))
  ids       <- rules[[ID_COL]][ridx]
  for (k in seq_along(ridx)) {
    mask <- with(sub, eval(parse(text = rules[[RULE_COL]][ridx[k]])))
    hit  <- which(mask)
    flag_sets[[k]] <- hit
    err_sets[[k]]  <- intersect(hit, err)
  }

  n <- length(ridx)
  max_ej   <- rep(NA_real_, n); near_id <- rep(NA_character_, n)
  mean_ej  <- rep(NA_real_, n); n_neardup <- rep(0L, n)
  for (a in seq_len(n)) {
    ejs <- rep(NA_real_, n)
    for (b in seq_len(n)) {
      if (a == b) next
      ej <- jaccard(err_sets[[a]], err_sets[[b]])
      ejs[b] <- ej
      if (!is.na(ej) && a < b && ej >= PAIR_REPORT_MIN) {
        pairs[[length(pairs) + 1L]] <- data.frame(
          hh_size = hh, rule_a = ids[a], rule_b = ids[b],
          error_jaccard = round(ej, 3),
          flag_jaccard  = round(jaccard(flag_sets[[a]], flag_sets[[b]]), 3),
          n_err_a = length(err_sets[[a]]), n_err_b = length(err_sets[[b]]),
          stringsAsFactors = FALSE)
      }
    }
    if (any(!is.na(ejs))) {
      max_ej[a]    <- max(ejs, na.rm = TRUE)
      near_id[a]   <- ids[which.max(replace(ejs, is.na(ejs), -1))]
      mean_ej[a]   <- mean(ejs, na.rm = TRUE)
      n_neardup[a] <- sum(ejs >= NEAR_DUP_MIN, na.rm = TRUE)
    }
  }
  by_rule[[length(by_rule) + 1L]] <- data.frame(
    hh_size = hh, rule_id = ids,
    n_errors_targeted = vapply(err_sets, length, integer(1)),
    max_error_jaccard = round(max_ej, 3),
    nearest_rule      = near_id,
    mean_error_jaccard = round(mean_ej, 3),
    n_near_duplicates = n_neardup,
    stringsAsFactors = FALSE)
}

by_rule_df <- do.call(rbind, by_rule)
pairs_df   <- if (length(pairs)) do.call(rbind, pairs) else
  data.frame(hh_size = character(0))

write.csv(by_rule_df, OUT_BY_RULE, row.names = FALSE)
write.csv(pairs_df,   OUT_PAIRS,   row.names = FALSE)

cat("Per-stratum near-duplicate counts (error_jaccard >=", NEAR_DUP_MIN, "):\n")
if (nrow(by_rule_df)) {
  agg <- aggregate(n_near_duplicates ~ hh_size, by_rule_df,
                   function(x) sum(x > 0))
  names(agg)[2] <- "rules_with_a_near_dup"
  print(agg)
}
cat(sprintf("\nWrote %s (%d rules) and %s (%d pairs)\n",
            OUT_BY_RULE, nrow(by_rule_df), OUT_PAIRS, nrow(pairs_df)))

