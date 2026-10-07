# Reconstruction income piles: how much of the shipped v2.7 lists rests on them
# (diagnostic, 2026-10-07; read-only, no mining, nothing shipped changes).
#
# The munging reconstruction (adjust_income, then scale_by_ratio) places every
# downward income correction on a case issued the maximum benefit at the
# first amount that reproduces the maximum, then rescales those cases by one
# ratio per household size. Where that point depends only on the year's
# tables, many different cases get the identical reconstructed amount (e.g.
# one-person earned income $589 / $622 / $663 in FY2022 / 23 / 24). A state's
# own case file carries real pre-review amounts, so it has no such piles.
#
# Definitions (FY2022-24 rows of the final v2.7 frame, the delivery lists'
# training data):
#   pile case  = correctednotes earn_down (unearn_down) whose reconstructed
#                as-recorded earned (unearned) amount is shared by at least
#                PILE_MIN down-corrected cases of the same fiscal year and
#                household size (sizes 6+ pooled); reported at PILE_MIN 10 and 5
#   at-max down-correction (the mechanism, broader) = earn_down or
#                unearn_down on a case issued the maximum benefit
# Rule level: every rule on the 98 blended + 98 national-only lists, scored on
# its own training rows (national rules: all FY2022-24 rows of its household
# size stratum; state-pool rules: that state's rows). Recomputed flag counts
# must equal the lists' n_flagged_train (asserted). Tag = at least 25% of the
# rule's training flags, or of its training errors, are pile cases (the
# existing artifact gate's MM_TAG_SHARE). Precision and the 99% Wilson bound
# are reported with and without pile cases.
# List level: each list's core rules, unioned on the state's FY2022-24 cases
# (the workbook demo data): pile share of flagged cases, and union precision
# with and without pile cases.
# Caveat: held-out tests on the public frame cannot show the internal-data
# effect, because test years carry the same reconstruction; "without pile
# cases" is a sensitivity, not a prediction of internal-data precision.
#   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" methods/reconstruction_income_piles/pile_diagnostic.R > methods/reconstruction_income_piles/pile_diagnostic.log 2>&1
setwd("C:/Users/ericg/snap_qc")
suppressMessages(library(dplyr))
source("rule_mining_helpers.R")
OUT <- "methods/reconstruction_income_piles"
LCB_Z <- 2.326; TAG <- 0.25
stamp <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), sprintf(...)))
hh_group_of <- function(n) {
  n <- suppressWarnings(as.numeric(as.character(n)))
  ifelse(is.na(n), NA_character_, ifelse(n <= 1, "1", ifelse(n <= 3, "2-3", "4+")))
}

## ---- frame and pile flags ---------------------------------------------------
reg_model_data <- readRDS("reg_model_data.rds")
stopifnot(nrow(reg_model_data) == 231619L)
adf0 <- reg_model_data %>% filter(fiscal_year %in% c("2022", "2023", "2024"))
stopifnot(nrow(adf0) == 115559L)
vocab <- c("HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size",
           "expedited_i", "bbce_state_i", "rawben_rel_max", "medical_deductions",
           "shelter_expenses_by_hh_size", "utilities_sua", "married", "homeless",
           "percent_abawd", "unc_rawben_rel_max", "months_since_cert_n",
           "count_divisible_by_100", "gross_by_hh_size", "earned_by_hh_size",
           "unearned_by_hh_size")
adf <- prep_features(adf0, vocab)$data
ie <- !is.na(adf$over_threshold) & adf$over_threshold != 0
st <- as.character(adf$state); hh <- hh_group_of(adf$cert_HH_size_FS_n)
size6 <- pmin(adf$cert_HH_size_FS_n, 6)
notes <- as.character(adf$correctednotes)

pile_flag <- function(min_n) {
  out <- rep(FALSE, nrow(adf))
  for (spec in list(c("earn_down", "rawearn_nominal"), c("unearn_down", "rawunearn_nominal"))) {
    amt <- adf[[spec[2]]]
    elig <- !is.na(notes) & notes == spec[1] & !is.na(amt) & amt > 0
    key <- paste(adf$fiscal_year, size6, amt)
    cnt <- ave(as.integer(elig), key, FUN = sum)
    out <- out | (elig & cnt >= min_n)
  }
  out
}
pile10 <- pile_flag(10); pile5 <- pile_flag(5)
atmax_down <- !is.na(notes) & notes %in% c("earn_down", "unearn_down") & adf$rawben >= adf$benmax
stamp("FY2022-24 rows %d, errors %d (base %.3f)", nrow(adf), sum(ie), mean(ie))
for (nm in c("pile10", "pile5", "atmax_down")) {
  f <- get(nm)
  stamp("  %-10s %5d rows (%.2f%%), error rate %.3f; share inside at-max down-corrections %.3f",
        nm, sum(f), 100 * mean(f), mean(ie[f]), mean(atmax_down[f]))
}
piles_tbl <- data.frame(fiscal_year = adf$fiscal_year, size = size6, note = notes, state = st,
                        earned = adf$rawearn_nominal, unearned = adf$rawunearn_nominal,
                        error = ie, pile10 = pile10) %>%
  filter(pile10) %>%
  mutate(amount = ifelse(note == "earn_down", earned, unearned)) %>%
  group_by(note, fiscal_year, size, amount) %>%
  summarise(cases = n(), errors = sum(error), states = n_distinct(state), .groups = "drop")
write.csv(piles_tbl, file.path(OUT, "piles_fy2022_2024.csv"), row.names = FALSE)

## ---- the shipped lists' rules -----------------------------------------------
files <- Sys.glob("state_delivery_lists/*_delivery_*_2022_2024_budget*.csv")
stopifnot(length(files) == 196L)
meta <- function(fn) {
  b <- basename(fn)
  list(type = sub("_delivery_.*", "", b),
       state = gsub("_", " ", sub("^(blended|national)_delivery_(.*)_2022_2024_budget\\d+\\.csv$", "\\2", b)),
       budget = as.integer(sub(".*budget(\\d+)\\.csv$", "\\1", b)) / 100)
}
lists <- lapply(files, function(fn) {
  m <- meta(fn); x <- read.csv(fn, check.names = FALSE)
  data.frame(file = basename(fn), type = m$type, state = m$state, budget = m$budget,
             rank = x$rank, role = x$role, rule = x$rule, hh = as.character(x$hh),
             pool = x$pool, n_flagged_train = x$n_flagged_train, stringsAsFactors = FALSE)
})
L <- bind_rows(lists)
L$unit <- ifelse(L$pool == "state", L$state, "national")
rules <- L %>% distinct(unit, hh, rule, n_flagged_train)
stopifnot(!anyDuplicated(rules[, c("unit", "hh", "rule")]))
stamp("list rows %d; unique rules %d (national %d, state %d)", nrow(L), nrow(rules),
      sum(rules$unit == "national"), sum(rules$unit != "national"))

score_unit <- function(rr, rows) {
  strata <- lapply(setNames(nm = c("1", "2-3", "4+")), function(h) which(hh[rows] == h))
  fl <- flags_for_rules(rr, adf[rows, , drop = FALSE], strata)
  g <- function(v) vapply(fl, function(ix) sum(v[rows][ix]), numeric(1))
  data.frame(n = lengths(fl), k = g(ie), pile_n = g(pile10), pile_k = g(pile10 & ie),
             pile5_n = g(pile5), atmax_n = g(atmax_down))
}
res <- list()
nat <- rules[rules$unit == "national", ]
res[[1]] <- cbind(nat, score_unit(nat, seq_len(nrow(adf))))
for (s in sort(unique(rules$unit[rules$unit != "national"]))) {
  rr <- rules[rules$unit == s, ]
  res[[length(res) + 1]] <- cbind(rr, score_unit(rr, which(st == s)))
}
R <- bind_rows(res)
bad <- R[R$n != R$n_flagged_train, ]
if (nrow(bad)) { print(head(bad)); stop(sprintf("%d rules do not reproduce n_flagged_train", nrow(bad))) }
stamp("all %d rules reproduce their training flag counts", nrow(R))
R <- R %>% mutate(
  share_flags = pile_n / n, share_errors = ifelse(k > 0, pile_k / k, 0),
  tagged = share_flags >= TAG | share_errors >= TAG,
  precision = k / n, precision_wo = ifelse(n - pile_n > 0, (k - pile_k) / (n - pile_n), NA),
  lcb = wilson_lcb(k, n, LCB_Z), lcb_wo = ifelse(n - pile_n > 0, wilson_lcb(k - pile_k, n - pile_n, LCB_Z), NA),
  share_flags_pile5 = pile5_n / n, share_flags_atmax = atmax_n / n)
write.csv(R, file.path(OUT, "rule_pile_share.csv"), row.names = FALSE)
for (u in c("national", "state")) {
  q <- if (u == "national") R[R$unit == "national", ] else R[R$unit != "national", ]
  stamp("%s rules %d: tagged %d (%.1f%%) | share of flags from piles: median %.3f, 90th pct %.3f, max %.3f | any pile flag: %d",
        u, nrow(q), sum(q$tagged), 100 * mean(q$tagged), median(q$share_flags),
        quantile(q$share_flags, 0.9), max(q$share_flags), sum(q$pile_n > 0))
}

## ---- list level: core rules on the state's FY2022-24 cases ------------------
LL <- list()
for (fn in unique(L$file)) {
  x <- L[L$file == fn & L$role == "core", ]
  rows <- which(st == x$state[1])
  strata <- lapply(setNames(nm = c("1", "2-3", "4+")), function(h) which(hh[rows] == h))
  fl <- flags_for_rules(x, adf[rows, , drop = FALSE], strata)
  u <- sort(unique(unlist(fl)))
  r <- rows[u]
  key <- paste(ifelse(x$pool == "state", x$state, "national"), x$hh, x$rule)
  tg <- R$tagged[match(key, paste(R$unit, R$hh, R$rule))]
  LL[[length(LL) + 1]] <- data.frame(
    file = fn, type = x$type[1], state = x$state[1], budget = x$budget[1],
    core_rules = nrow(x), core_rules_tagged = sum(tg), n_cases = length(rows),
    flagged = length(r), errors = sum(ie[r]), pile_flagged = sum(pile10[r]),
    pile_errors = sum(ie[r] & pile10[r]),
    precision = sum(ie[r]) / max(length(r), 1),
    precision_wo = sum(ie[r] & !pile10[r]) / max(sum(!pile10[r]), 1))
}
LL <- bind_rows(LL) %>% mutate(pile_share_flagged = pile_flagged / pmax(flagged, 1),
                               d_precision = precision_wo - precision)
write.csv(LL, file.path(OUT, "list_pile_share.csv"), row.names = FALSE)
for (ty in c("blended", "national")) for (b in c(0.05, 0.10)) {
  q <- LL[LL$type == ty & LL$budget == b, ]
  stamp("%s %.0f%%: lists with a tagged core rule %d/49 | pile share of flagged cases median %.3f, max %.3f (%s) | precision with %.3f vs without %.3f (pooled); within-list change median %+.3f, worst %+.3f (%s)",
        ty, 100 * b, sum(q$core_rules_tagged > 0), median(q$pile_share_flagged), max(q$pile_share_flagged),
        q$state[which.max(q$pile_share_flagged)], sum(q$errors) / sum(q$flagged),
        sum(q$errors - q$pile_errors) / sum(q$flagged - q$pile_flagged), median(q$d_precision),
        min(q$d_precision), q$state[which.min(q$d_precision)])
}
cat("PILE DIAGNOSTIC DONE\n")
