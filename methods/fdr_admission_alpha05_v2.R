# FDR-done-right, step 2: test admission rules as TRUE build-time gates on
# the raw unfiltered vocabularies (fdr_raw_vocabulary_mine_v2.R). EXPLORATORY
# (scored on 2024); adoption requires the honest designs.
#
# Admission arms, applied per pool (national / state) to raw candidates:
#   prod    the production keep filter: n >= 30 & raw >= 0.05 & raw > base
#   fdr10   Benjamini-Hochberg vs the stratum base rate at alpha = .10,
#           NO support floor (the self-calibration claim under test)
#   fdr05   same at alpha = .05
#   fdr10f  BH at .10 composed with the n >= 30 floor
#
# To isolate admission, NO coverage/dominance dedup is applied in ANY arm
# (the walk is robust to redundancy: zero-new-case rules are skipped).
# Downstream is the identical blend/freeze/walk on each state's 2024.
# Pre-registered expectations: (1) fdr10f tracks prod at national scale
# (admission is not the binding constraint there); (2) floorless FDR either
# prevents the small-support collapse at state scale (self-calibration works)
# or reproduces it (the n >= 30 floor stays); which one is the finding.
#
# Output: methods/state_similarity_v2/transfer_benchmark_train2223_test24/
#         fdr_admission_audition.csv

suppressMessages(library(dplyr))
source("rule_mining_helpers.R")

TARGETS <- c("Massachusetts", "Michigan", "North Carolina", "Connecticut",
             "Arizona", "Washington",
             "Louisiana", "Virginia", "California", "Texas",
             "Mississippi", "New Jersey", "Colorado",
             "Maine", "Maryland", "Missouri",
             "District of Columbia", "Tennessee")
BUDGETS <- c(0.05, 0.10)
BUFFER_MULT <- 3
LCB_Z <- 2.326
TRAIN_YEARS <- c("2022", "2023")
TEST_YEAR   <- "2024"

out_dir <- "methods/state_similarity_v2/transfer_benchmark_train2223_test24"
RAWDIR  <- file.path(out_dir, "fdr_raw_vocab")

features <- c(
  "HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size",
  "expedited_i", "cat_elig", "rawben_rel_max", "medical_deductions",
  "shelter_expenses_by_hh_size", "utilities", "married", "homeless",
  "rawearn_by_hh_size", "rawunearn_by_hh_size", "rawgross_by_hh_size",
  "percent_abawd", "unc_rawben_rel_max",
  "months_since_cert_n", "count_divisible_by_100"
)
HH_LEVELS <- c("1", "2-3", "4+")
hh_group_of <- function(n) {
  n <- suppressWarnings(as.numeric(as.character(n)))
  ifelse(is.na(n), NA_character_, ifelse(n <= 1, "1", ifelse(n <= 3, "2-3", "4+")))
}

adf <- prep_features(reg_model_data %>%
                       filter(fiscal_year %in% c(TRAIN_YEARS, TEST_YEAR)),
                     features)$data
yr <- as.character(adf$fiscal_year)
st <- as.character(adf$state)
in_tr <- yr %in% TRAIN_YEARS
ie_all <- !is.na(adf$over_threshold) & adf$over_threshold != 0
ed_all <- ifelse(ie_all, abs(ifelse(is.na(adf$total_error_amount), 0,
                                    adf$total_error_amount)), 0)

bh_admit <- function(k, n, p0, alpha) {
  pv <- pbinom(k - 1, n, p0, lower.tail = FALSE)
  m <- length(pv)
  o <- order(pv)
  thr <- max(c(0L, which(pv[o] <= alpha * seq_len(m) / m)))
  admit <- rep(FALSE, m)
  if (thr > 0) admit[o[seq_len(thr)]] <- TRUE
  admit
}

load_raw <- function(key) {
  r <- readRDS(file.path(RAWDIR, sprintf("raw_%s.rds", key)))
  base <- attr(r, "base_rates")
  r$raw_prec <- ifelse(r$n > 0, r$k / r$n, NA_real_)
  r$base <- base[r$hh]
  r$adm_prod   <- !is.na(r$raw_prec) & r$n >= 30 & r$raw_prec >= 0.05 & r$raw_prec > r$base
  r$adm_fdr10  <- bh_admit(r$k, r$n, r$base, 0.10)
  r$adm_fdr05  <- bh_admit(r$k, r$n, r$base, 0.05)
  r$adm_fdr10f <- r$adm_fdr10 & r$n >= 30
  r$adm_fdr05f <- r$adm_fdr05 & r$n >= 30
  r$lcb <- wilson_lcb(r$k, r$n, LCB_Z)
  r
}

walk_eval <- function(stat, idx_tr, idx_te, n_tr_rows, n_te_rows, ie_te, ed_te, b) {
  cap <- floor(b * n_tr_rows); cap_buf <- floor(BUFFER_MULT * b * n_tr_rows)
  un <- rep(FALSE, n_tr_rows); n_in <- 0L
  frozen <- integer(0); buffer <- integer(0)
  for (i in order(-stat)) {
    if (is.na(stat[i])) next
    add <- sum(!un[idx_tr[[i]]])
    if (add == 0) next
    if (n_in + add <= cap) {
      un[idx_tr[[i]]] <- TRUE; n_in <- n_in + add; frozen <- c(frozen, i)
    } else if (n_in + add <= cap_buf) {
      un[idx_tr[[i]]] <- TRUE; n_in <- n_in + add; buffer <- c(buffer, i)
    }
  }
  cap24 <- floor(b * n_te_rows)
  un24 <- rep(FALSE, n_te_rows); used <- 0L
  for (i in c(frozen, buffer)) {
    add <- sum(!un24[idx_te[[i]]])
    if (add > 0 && sum(un24) + add <= cap24) {
      un24[idx_te[[i]]] <- TRUE; used <- used + 1L
    }
  }
  nb <- sum(un24)
  data.frame(n_deployed = used, workload = round(nb / n_te_rows, 4),
             precision = round(ifelse(nb > 0, sum(ie_te[un24]) / nb, NA), 4),
             dollar_recall = round(sum(ed_te[un24]) / sum(ed_te), 4))
}

natl <- load_raw("national")
cat(sprintf("national raw: %d candidates | admitted prod %d, fdr10 %d, fdr05 %d, fdr10f %d\n",
            nrow(natl), sum(natl$adm_fdr10f), sum(natl$adm_fdr05f), 0L, 0L))

# Only the two floored arms: fdr10f is the shipped admission, re-run as a
# reproduction check against the recorded 0.3345 / 0.2753; fdr05f is the cell
# the original audition never filled (its fdr05 arm was floorless).
ARMS <- c("fdr10f", "fdr05f")
res_dir <- out_dir   # artifacts land beside the parent audition
dir.create(res_dir, showWarnings = FALSE, recursive = TRUE)
res <- list()
for (target in TARGETS) {
  own <- load_raw(gsub("[^A-Za-z]", "", target))
  cat(sprintf("%-22s raw %6d | prod %5d fdr10 %5d fdr05 %5d fdr10f %5d\n",
              target, nrow(own), sum(own$adm_fdr10f), sum(own$adm_fdr05f), 0L, 0L))
  # union of all arms' admitted rules -> ONE flags pass per state
  keep_any_n <- Reduce(`|`, lapply(ARMS, function(a) natl[[paste0("adm_", a)]]))
  keep_any_o <- Reduce(`|`, lapply(ARMS, function(a) own[[paste0("adm_", a)]]))
  pool <- bind_rows(natl[keep_any_n, , drop = FALSE],
                    own[keep_any_o, , drop = FALSE])
  tr <- adf[st == target & in_tr, , drop = FALSE]
  te <- adf[st == target & yr == TEST_YEAR, , drop = FALSE]
  rows_te <- which(st == target & yr == TEST_YEAR)
  strata_tr <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(tr$cert_HH_size_FS_n) %in% h))
  strata_te <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(te$cert_HH_size_FS_n) %in% h))
  idx_tr <- flags_for_rules(pool, tr, strata_tr, label = "")
  idx_te <- flags_for_rules(pool, te, strata_te, label = "")
  ie_te <- ie_all[rows_te]; ed_te <- ed_all[rows_te]

  for (a in ARMS) {
    ok <- pool[[paste0("adm_", a)]]
    p <- pool[ok, , drop = FALSE]
    # blend dedup on rule text: keep higher-lcb version
    o <- order(-p$lcb)
    dup <- duplicated(paste(p$hh[o], p$rule[o], sep = "\r"))
    sel <- which(ok)[o[!dup]]
    p2 <- pool[sel, , drop = FALSE]
    for (b in BUDGETS) {
      ev <- walk_eval(p2$lcb, idx_tr[sel], idx_te[sel], nrow(tr), nrow(te),
                      ie_te, ed_te, b)
      res[[length(res) + 1]] <- cbind(
        data.frame(target = target, arm = a, budget = b,
                   n_admitted = nrow(p2),
                   target_base_rate = round(mean(ie_te), 4)), ev)
    }
  }
  saveRDS(bind_rows(res), file.path(res_dir, "fdr05f_partial.rds"))
}
out <- bind_rows(res)
write.csv(out, file.path(res_dir, "fdr05f_audition.csv"), row.names = FALSE)
cat(sprintf("wrote %s (%d rows)\n",
            file.path(res_dir, "fdr05f_audition.csv"), nrow(out)))

cat("\nmedians (precision / dollar recall / admitted):\n")
sm <- out %>% group_by(arm, budget) %>%
  summarise(med_prec = median(precision, na.rm = TRUE),
            med_dollars = median(dollar_recall, na.rm = TRUE),
            med_admitted = median(n_admitted), .groups = "drop") %>%
  arrange(budget, desc(med_prec))
print(as.data.frame(sm), row.names = FALSE)
