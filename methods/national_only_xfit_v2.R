# National-only cross-fit ordering: does ranking on an out-of-fold statistic
# beat ranking on the self-scored one, and how much does either depend on which
# half we happened to mine?
#
# WHY NATIONAL ONLY. The 2026-08-04 study cross-fitted the national pool AND
# each state's own pool, which halved state pools to 48-140 errors and destroyed
# them (RESUME.md, "Invalid designs"). Blending a cross-fitted national pool with
# a self-scored state pool is not an option either: measured on these same mines,
# the self-scored bound overstates the out-of-fold bound by +0.105 at the top 1%
# nationally and +0.123 for state pools, so one sorted scale would systematically
# promote state rules. Dropping the state pool removes the mismatch, and section
# 14 already makes the plain national list the default a state deploys.
#
# NO MINING. Reads the five cached national mines, each an independent random
# 50/50 split of FY2022-23 carrying both statistics for the same 138k rules:
# n_self/k_self on the mining half, n_hon/k_hon on the held-out half.
#
# ARMS, per partition:
#   selfscored  rank by the mining half's Wilson LCB
#   outoffold   rank by the held-out half's Wilson LCB
# under two admission variants:
#   common  both arms draw from ONE admitted set (out-of-fold BH + n_hon >= 30),
#           so the only difference is the sort. Isolates the ordering effect.
#   own     each arm admits on its own statistic, which is closer to what each
#           would actually do in production, but changes two things at once.
#
# Evaluated on ALL 49 states: fill each state's FY2022-23 caseload to the review
# budget as core plus buffer to 3x, freeze, then walk against that state's FY2024
# cases. FY2024 is untouched by every part of the construction.
#
# Expects `reg_model_data`.

suppressMessages({ library(dplyr) })
source("rule_mining_helpers.R")

TRAIN_YEARS <- c("2022", "2023")
TEST_YEAR   <- "2024"
BUDGETS     <- c(0.05, 0.10)
BUFFER_MULT <- 3
LCB_Z       <- 2.326
FDR_ALPHA   <- 0.10
MIN_N       <- 30
TOPK        <- 20000
REPS        <- 5
MINES   <- "methods/state_similarity_v2/crossfit_ranking_train2223_test24/mines"
OUT_DIR <- "methods/national_only_xfit"
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

pf  <- prep_features(reg_model_data %>%
                       filter(fiscal_year %in% c(TRAIN_YEARS, TEST_YEAR)), features)
adf <- pf$data
st  <- as.character(adf$state)
yr  <- as.character(adf$fiscal_year)
ie_all <- !is.na(adf$over_threshold) & adf$over_threshold != 0
ed_all <- ifelse(ie_all, abs(ifelse(is.na(adf$total_error_amount), 0,
                                    adf$total_error_amount)), 0)
strata_of <- function(df) lapply(setNames(nm = HH_LEVELS), function(h)
  which(hh_group_of(df$cert_HH_size_FS_n) %in% h))
stamp("frame: %d rows | train %s | test %s", nrow(adf),
      paste(TRAIN_YEARS, collapse = "+"), TEST_YEAR)

bh_admit <- function(k, n, p0, alpha = FDR_ALPHA) {
  pv <- pbinom(k - 1, n, p0, lower.tail = FALSE)
  m <- length(pv); o <- order(pv)
  thr <- max(c(0L, which(pv[o] <= alpha * seq_len(m) / m)))
  a <- rep(FALSE, m); if (thr > 0) a[o[seq_len(thr)]] <- TRUE
  a
}

walk_eval <- function(stat, idx_tr, idx_te, n_tr, n_te, ie_te, ed_te, b) {
  cap <- floor(b * n_tr); cap_buf <- floor(BUFFER_MULT * b * n_tr)
  un <- rep(FALSE, n_tr); n_in <- 0L; frozen <- integer(0); buffer <- integer(0)
  for (i in order(-stat)) {
    if (is.na(stat[i])) next
    add <- sum(!un[idx_tr[[i]]])
    if (add == 0) next
    if (n_in + add <= cap) { un[idx_tr[[i]]] <- TRUE; n_in <- n_in + add; frozen <- c(frozen, i) }
    else if (n_in + add <= cap_buf) { un[idx_tr[[i]]] <- TRUE; n_in <- n_in + add; buffer <- c(buffer, i) }
  }
  slack <- cap_buf - n_in
  cap24 <- floor(b * n_te); un24 <- rep(FALSE, n_te); used <- 0L
  for (i in c(frozen, buffer)) {
    add <- sum(!un24[idx_te[[i]]])
    if (add > 0 && sum(un24) + add <= cap24) { un24[idx_te[[i]]] <- TRUE; used <- used + 1L }
  }
  nb <- sum(un24)
  data.frame(n_deployed = used, workload = round(nb / n_te, 4),
             precision = round(ifelse(nb > 0, sum(ie_te[un24]) / nb, NA), 4),
             dollar_recall = round(sum(ed_te[un24]) / sum(ed_te), 4),
             n_core = length(frozen), n_buffer = length(buffer), slack = slack)
}

## ---- build the ranked windows once per partition ---------------------------
# Each entry: the rules to evaluate and the statistic that orders them.
windows <- list()
for (rp in seq_len(REPS)) {
  r <- readRDS(file.path(MINES, sprintf("national_rep%d.rds", rp)))
  base_A <- attr(r, "base_A"); base_B <- attr(r, "base_B")
  r$lcb_self <- wilson_lcb(r$k_self, r$n_self, LCB_Z)
  r$lcb_hon  <- wilson_lcb(r$k_hon,  r$n_hon,  LCB_Z)
  # BH is run within stratum against that stratum's base rate, matching the
  # delivery builder
  adm_hon <- adm_self <- rep(FALSE, nrow(r))
  for (h in HH_LEVELS) {
    ix <- which(r$hh == h)
    if (!length(ix)) next
    adm_hon[ix]  <- bh_admit(r$k_hon[ix],  r$n_hon[ix],  base_B[[h]])
    adm_self[ix] <- bh_admit(r$k_self[ix], r$n_self[ix], base_A[[h]])
  }
  keep_common <- adm_hon  & r$n_hon  >= MIN_N
  keep_self   <- adm_self & r$n_self >= MIN_N
  add <- function(nm, variant, keep, stat) {
    k <- which(keep)
    if (!length(k)) return(NULL)
    o <- k[order(-stat[k])][seq_len(min(TOPK, length(k)))]
    list(rep = rp, arm = nm, variant = variant, rows = o,
         stat = stat[o], n_admitted = length(k), pool = r)
  }
  windows[[length(windows) + 1]] <- add("outoffold",  "common", keep_common, r$lcb_hon)
  windows[[length(windows) + 1]] <- add("selfscored", "common", keep_common, r$lcb_self)
  windows[[length(windows) + 1]] <- add("outoffold",  "own",    keep_common, r$lcb_hon)
  windows[[length(windows) + 1]] <- add("selfscored", "own",    keep_self,   r$lcb_self)
  stamp("rep %d: %d rules | admitted common(out-of-fold) %d | admitted own(self) %d",
        rp, nrow(r), sum(keep_common), sum(keep_self))
}
windows <- Filter(Negate(is.null), windows)

## ---- evaluate every state --------------------------------------------------
targets <- sort(unique(st))
stamp("evaluating %d states x %d windows x %d budgets",
      length(targets), length(windows), length(BUDGETS))
res <- list()
for (target in targets) {
  tr_rows <- which(st == target & yr %in% TRAIN_YEARS)
  te_rows <- which(st == target & yr == TEST_YEAR)
  if (!length(tr_rows) || !length(te_rows)) { stamp("  %s: skipped", target); next }
  tr <- adf[tr_rows, , drop = FALSE]; te <- adf[te_rows, , drop = FALSE]
  strata_tr <- strata_of(tr); strata_te <- strata_of(te)
  ie_te <- ie_all[te_rows]; ed_te <- ed_all[te_rows]

  # flags once per (state, partition) over the union of that partition's windows
  for (rp in seq_len(REPS)) {
    ws <- Filter(function(w) w$rep == rp, windows)
    if (!length(ws)) next
    u <- sort(unique(unlist(lapply(ws, `[[`, "rows"))))
    sub <- ws[[1]]$pool[u, , drop = FALSE]
    idx_tr <- flags_for_rules(sub, tr, strata_tr, label = "")
    idx_te <- flags_for_rules(sub, te, strata_te, label = "")
    map <- setNames(seq_along(u), u)
    for (w in ws) {
      pos <- unname(map[as.character(w$rows)])
      for (b in BUDGETS) {
        ev <- walk_eval(w$stat, idx_tr[pos], idx_te[pos], nrow(tr), nrow(te),
                        ie_te, ed_te, b)
        if (ev$slack > 0)
          stamp("  WARNING %s rep%d %s/%s b=%.2f: slack %d", target, rp,
                w$arm, w$variant, b, ev$slack)
        res[[length(res) + 1]] <- cbind(
          data.frame(target = target, rep = rp, arm = w$arm, variant = w$variant,
                     budget = b, n_admitted = w$n_admitted,
                     n_scanned = length(w$rows),
                     base_rate = round(mean(ie_te), 4)), ev)
      }
    }
    rm(idx_tr, idx_te); invisible(gc())
  }
  stamp("  %s done (%d rows so far)", target, length(res))
}
out <- bind_rows(res)
write.csv(out, file.path(OUT_DIR, "national_only_xfit.csv"), row.names = FALSE)
stamp("=== wrote %s (%d rows) ===",
      file.path(OUT_DIR, "national_only_xfit.csv"), nrow(out))
