# Do different partitions flag the SAME cases in different ways, or different
# cases?
#
# methods/partition_signature_overlap_v2.R showed the top of the ranked pool is
# nearly partition-specific: two partitions' top-100 share 3.2% of signatures and
# essentially 0% of exact rule texts. That is a statement about the RULES. It
# says nothing about whether the delivered lists do the same work.
#
# This measures the delivered lists at the case level. For each state and each of
# the five cached partitions, walk the national-only out-of-fold list against the
# state's FY2024 caseload and record WHICH cases are flagged and WHICH errors are
# caught. Then compare across partitions.
#
# Pre-registered expectation (written before running): if the rule churn is
# cosmetic, the errors caught should overlap far more than the rules do. Bar: a
# median pairwise Jaccard on errors caught above 0.5 would say the deliverable is
# stable in what it does; below ~0.3 would say partition choice changes which
# errors a state finds, which is a property states should be told about.
#
# Note the walk fills to the same capacity for every partition, so the flagged
# sets are near-identical in SIZE; Jaccard therefore compares composition only.
#
# Reads cached mines. Mines nothing. Expects `reg_model_data`.

suppressMessages({ library(dplyr) })
source("rule_mining_helpers.R")

TRAIN_YEARS <- c("2022", "2023"); TEST_YEAR <- "2024"
BUDGETS <- c(0.05, 0.10); BUFFER_MULT <- 3
LCB_Z <- 2.326; FDR_ALPHA <- 0.10; MIN_N <- 30; TOPK <- 20000; REPS <- 5
MINES   <- "methods/state_similarity_v2/crossfit_ranking_train2223_test24/mines"
OUT_DIR <- "methods/national_only_xfit"
# a spread of caseload sizes and base rates rather than all 49, since the
# question is about typical behaviour and each state costs a flag build
TARGETS <- c("California", "Texas", "Michigan", "Massachusetts", "Arizona",
             "Washington", "Louisiana", "Maine", "New Jersey", "Mississippi")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

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
  "percent_abawd", "unc_rawben_rel_max", "months_since_cert_n",
  "count_divisible_by_100")
stamp <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"),
                                   sprintf(...)))

pf <- prep_features(reg_model_data %>%
                      filter(fiscal_year %in% c(TRAIN_YEARS, TEST_YEAR)), features)
adf <- pf$data
st <- as.character(adf$state); yr <- as.character(adf$fiscal_year)
ie_all <- !is.na(adf$over_threshold) & adf$over_threshold != 0
strata_of <- function(df) lapply(setNames(nm = HH_LEVELS), function(h)
  which(hh_group_of(df$cert_HH_size_FS_n) %in% h))

bh_admit <- function(k, n, p0, alpha = FDR_ALPHA) {
  pv <- pbinom(k - 1, n, p0, lower.tail = FALSE)
  m <- length(pv); o <- order(pv)
  thr <- max(c(0L, which(pv[o] <= alpha * seq_len(m) / m)))
  a <- rep(FALSE, m); if (thr > 0) a[o[seq_len(thr)]] <- TRUE
  a
}

# same walk as the main run, but returns the flagged MASK rather than a summary
walk_mask <- function(stat, idx_tr, idx_te, n_tr, n_te, b) {
  cap <- floor(b * n_tr); cap_buf <- floor(BUFFER_MULT * b * n_tr)
  un <- rep(FALSE, n_tr); n_in <- 0L; frozen <- integer(0); buffer <- integer(0)
  for (i in order(-stat)) {
    if (is.na(stat[i])) next
    add <- sum(!un[idx_tr[[i]]]); if (add == 0) next
    if (n_in + add <= cap) { un[idx_tr[[i]]] <- TRUE; n_in <- n_in + add; frozen <- c(frozen, i) }
    else if (n_in + add <= cap_buf) { un[idx_tr[[i]]] <- TRUE; n_in <- n_in + add; buffer <- c(buffer, i) }
  }
  cap24 <- floor(b * n_te); un24 <- rep(FALSE, n_te)
  for (i in c(frozen, buffer)) {
    add <- sum(!un24[idx_te[[i]]])
    if (add > 0 && sum(un24) + add <= cap24) un24[idx_te[[i]]] <- TRUE
  }
  un24
}

windows <- list()
for (rp in seq_len(REPS)) {
  r <- readRDS(file.path(MINES, sprintf("national_rep%d.rds", rp)))
  base_B <- attr(r, "base_B")
  r$lcb_hon <- wilson_lcb(r$k_hon, r$n_hon, LCB_Z)
  adm <- rep(FALSE, nrow(r))
  for (h in HH_LEVELS) {
    ix <- which(r$hh == h); if (!length(ix)) next
    adm[ix] <- bh_admit(r$k_hon[ix], r$n_hon[ix], base_B[[h]])
  }
  k <- which(adm & r$n_hon >= MIN_N)
  o <- k[order(-r$lcb_hon[k])][seq_len(min(TOPK, length(k)))]
  windows[[rp]] <- list(rows = o, stat = r$lcb_hon[o], pool = r)
  stamp("rep %d window: %d rules", rp, length(o))
}

jac <- function(a, b) if (length(union(a, b))) length(intersect(a, b)) / length(union(a, b)) else NA
res <- list()
for (target in TARGETS) {
  tr_rows <- which(st == target & yr %in% TRAIN_YEARS)
  te_rows <- which(st == target & yr == TEST_YEAR)
  tr <- adf[tr_rows, , drop = FALSE]; te <- adf[te_rows, , drop = FALSE]
  strata_tr <- strata_of(tr); strata_te <- strata_of(te)
  err_te <- which(ie_all[te_rows])
  masks <- list()
  for (rp in seq_len(REPS)) {
    w <- windows[[rp]]
    sub <- w$pool[w$rows, , drop = FALSE]
    idx_tr <- flags_for_rules(sub, tr, strata_tr, label = "")
    idx_te <- flags_for_rules(sub, te, strata_te, label = "")
    for (b in BUDGETS) {
      m <- walk_mask(w$stat, idx_tr, idx_te, nrow(tr), nrow(te), b)
      masks[[paste(rp, b)]] <- which(m)
    }
    rm(idx_tr, idx_te); invisible(gc())
  }
  for (b in BUDGETS) {
    fl <- lapply(seq_len(REPS), function(rp) masks[[paste(rp, b)]])
    er <- lapply(fl, function(f) intersect(f, err_te))
    jf <- c(); je <- c()
    for (a in 1:(REPS - 1)) for (bb in (a + 1):REPS) {
      jf <- c(jf, jac(fl[[a]], fl[[bb]])); je <- c(je, jac(er[[a]], er[[bb]]))
    }
    res[[length(res) + 1]] <- data.frame(
      target = target, budget = b, n_te = nrow(te), n_err = length(err_te),
      flagged_med = median(vapply(fl, length, integer(1))),
      errors_med  = median(vapply(er, length, integer(1))),
      jaccard_flagged = round(median(jf), 4),
      jaccard_errors  = round(median(je), 4),
      errors_in_all5 = length(Reduce(intersect, er)),
      errors_in_any  = length(Reduce(union, er)))
    }
  stamp("  %s done", target)
}
out <- bind_rows(res)
out$share_errors_in_all5 <- round(out$errors_in_all5 / pmax(out$errors_in_any, 1), 4)
write.csv(out, file.path(OUT_DIR, "partition_case_overlap.csv"), row.names = FALSE)
print(out, row.names = FALSE)
for (b in BUDGETS)
  stamp("budget %.0f%%: median Jaccard flagged %.3f | errors caught %.3f | share of errors caught by ALL five partitions %.3f",
        100 * b, median(out$jaccard_flagged[out$budget == b]),
        median(out$jaccard_errors[out$budget == b]),
        median(out$share_errors_in_all5[out$budget == b]))
