# ──────────────────────────────────────────────────────────────────────────────
# Deployment-grade re-run of the state adaptation question (supersedes the
# 2023-judged grid search in state_rules_v2/ for this comparison): does a
# state do better taking the national rules as-is, re-FILTERING them on its
# own recent data, or filtering AND re-TUNING their thresholds?
#
#   pool:  the national_all pool from the deployment benchmark cache --
#          mined on ALL states' 2022+2023 (methods/
#          deployment_benchmark_train2223_test24.R). Honest for a 2024 test.
#   train: the target state's own 2022-23 (adaptation decisions only)
#   test:  the target state's 2024, at 5% / 10% review budgets
#
# Arms per state (all budget-filled in descending deployed-statistic order):
#   national_asis   pool at national thresholds, national train-LCB order
#   filtered        national thresholds; keep rules whose STATE-train 90% LCB
#                   >= 0.20 with n >= 30 flagged (the state-scale support
#                   lesson); rank by state-train LCB
#   tuned           partition-aware threshold variants per rule (as in
#                   state_threshold_gridsearch_v2.R); qualify variants at
#                   90% LCB >= 0.20 with n >= 30; deploy the LCB-max variant
#   hybrid          the settled state-scale scheme (modeling_findings.md #9):
#                   qualify at 90% LCB >= 0.20 with n >= 5; deploy the
#                   dollar-max qualifying variant; rank by state-train LCB
#
# Tuning arms consider the top TUNE_TOP_RULES pool rules by national LCB
# (variant evaluation is per-rule x per-variant; the full 40k+ pool is not
# tractable and states deploy short lists anyway). The filtered arm and the
# as-is arm use the full pool.
#
# Expects `reg_model_data`. Outputs (not committed until reviewed):
#   methods/state_similarity_v2/transfer_benchmark_train2223_test24/
#     deployment_state_adaptation.csv       one row per state x arm x budget
#     deployment_state_adaptation_summary_budget05/10.csv  (#9-format table)
# ──────────────────────────────────────────────────────────────────────────────

suppressMessages(library(dplyr))
source("rule_mining_helpers.R")
set.seed(117)

TARGETS <- c("Louisiana", "Washington", "Virginia", "Arizona", "Connecticut",
             "Michigan", "North Carolina", "California", "Texas",
             "Mississippi", "New Jersey", "Colorado",
             "Maine", "Maryland", "Missouri", "Massachusetts",
             "District of Columbia", "Tennessee")
TRAIN_YEARS <- c("2022", "2023")
TEST_YEAR   <- "2024"
BUDGETS     <- c(0.05, 0.10)

STATE_LCB_Z         <- 1.2816   # 90% one-sided: the settled state-scale gate
MIN_STATE_PRECISION <- 0.20
SUPPORT_CONF        <- 30       # filtered + tuned arms (state-scale lesson)
SUPPORT_HYBRID      <- 5        # hybrid arm (gridsearch settled default)
TUNE_TOP_RULES      <- 500
FACTORS_FINE   <- c(0.75, 0.90, 1.00, 1.10, 1.25)
FACTORS_COARSE <- c(0.90, 1.00, 1.10)
MAX_VARIANTS   <- 700

out_dir   <- "methods/state_similarity_v2/transfer_benchmark_train2223_test24"
CACHE_DIR <- file.path(out_dir, "pool_cache")

HH_SIZE_COL <- "cert_HH_size_FS_n"
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
  "percent_abawd", "unc_rawben_rel_max",
  "months_since_cert_n", "count_divisible_by_100"
)
targets_of <- function(df) {
  ie <- !is.na(df$over_threshold) & df$over_threshold != 0
  amt <- df$total_error_amount; amt[is.na(amt)] <- 0
  list(ie = ie, ed = ifelse(ie, abs(amt), 0))
}

adf <- prep_features(reg_model_data %>%
                       filter(fiscal_year %in% c(TRAIN_YEARS, TEST_YEAR)),
                     features)$data
yr <- as.character(adf$fiscal_year)
st <- as.character(adf$state)
in_train_years <- yr %in% TRAIN_YEARS

## ── the national_all pool from the deployment benchmark cache ────────────────
pool_key <- local({
  sig <- gsub("[^A-Za-z]", "", paste(sort(unique(st)), collapse = "_"))
  if (nchar(sig) <= 80) sig else
    sprintf("%s_%08x", substr(sig, 1, 60),
            sum(utf8ToInt(sig) * seq_along(utf8ToInt(sig))) %% .Machine$integer.max)
})
pool_path <- file.path(CACHE_DIR, sprintf("pool_%s.rds", pool_key))
stopifnot(file.exists(pool_path))
pool <- readRDS(pool_path)
cat(sprintf("national_all pool: %d rules (from %s)\n", nrow(pool), pool_path))

## ── threshold variants (partition-aware; as in state_threshold_gridsearch) ───
rule_variants <- function(rule, uniq = NULL) {
  p <- .parse_rule(rule)
  if (is.null(p)) return(rule)
  fac <- if (nrow(p) <= 3) FACTORS_FINE else FACTORS_COARSE
  grids <- lapply(seq_len(nrow(p)), function(i) {
    v <- unique(signif(p$thr[i] * fac, 4))
    if (p$thr[i] == 0) v <- 0
    uv <- if (is.null(uniq)) NULL else uniq[[p$var[i]]]
    if (!is.null(uv) && length(uv) > 0) {
      v <- v[order(abs(v - p$thr[i]))]
      key <- if (p$op[i] %in% c(">", "<="))
        findInterval(v, uv)
      else
        findInterval(v, uv, left.open = TRUE)
      keep <- !duplicated(key)
      v <- v[keep]; key <- key[keep]
      ord <- order(v); v <- v[ord]; key <- key[ord]
      never_fires <- if (p$op[i] %in% c(">", ">=")) key == length(uv) else key == 0
      v <- v[!never_fires]
      if (length(v) == 0) v <- p$thr[i]
    }
    v
  })
  combos <- expand.grid(grids, KEEP.OUT.ATTRS = FALSE)
  if (nrow(combos) > MAX_VARIANTS) combos <- combos[seq_len(MAX_VARIANTS), , drop = FALSE]
  vapply(seq_len(nrow(combos)), function(k) {
    paste(sprintf("%s %s %s", p$var, p$op,
                  format(as.numeric(combos[k, ]), digits = 15, scientific = FALSE)),
          collapse = " & ")
  }, "")
}

eval_rule_on <- function(rule, df, stratum_rows) {
  v <- tryCatch(with(df, eval(parse(text = rule))), error = function(e) NULL)
  if (is.null(v)) return(integer(0))
  v[is.na(v)] <- FALSE
  intersect(which(v), stratum_rows)
}

# n_rules_used counts only rules that ADD at least one new case (redundant
# rules are skipped, not counted): the counted rules alone reproduce the union.
budgeted_union <- function(stat, idx, n_rows, tg, budget) {
  cap <- floor(budget * n_rows)
  un <- rep(FALSE, n_rows); n_used <- 0L; n_in <- 0L
  for (i in order(-stat)) {
    if (is.na(stat[i])) next
    ix <- idx[[i]]
    add <- sum(!un[ix])
    if (add > 0 && n_in + add <= cap) {
      un[ix] <- TRUE; n_in <- n_in + add; n_used <- n_used + 1L
    }
  }
  k <- sum(tg$ie[un]); d <- sum(tg$ed[un])
  data.frame(n_rules_used = n_used, n_flagged = n_in,
             workload = round(n_in / n_rows, 4),
             precision = round(ifelse(n_in > 0, k / n_in, NA), 4),
             recall = round(k / sum(tg$ie), 4),
             dollar_recall = round(d / sum(tg$ed), 4))
}

## ── per-state arms ────────────────────────────────────────────────────────────
res <- list()
emit <- function(target, approach, n_dep, stat, idx_te, n_te, tg_te, base) {
  for (b in BUDGETS) {
    out <- budgeted_union(stat, idx_te, n_te, tg_te, b)
    out$target <- target; out$approach <- approach; out$budget <- b
    out$n_deployable_rules <- n_dep
    out$target_base_rate <- round(base, 4)
    res[[length(res) + 1]] <<- out
    cat(sprintf("  %-14s budget %2.0f%%: %4d rules, prec %s, $%3.0f%%\n",
                approach, 100 * b, out$n_rules_used,
                ifelse(is.na(out$precision), "  NA ", sprintf("%.3f", out$precision)),
                100 * out$dollar_recall))
  }
}

for (target in TARGETS) {
  tr <- adf[st == target & in_train_years, , drop = FALSE]
  te <- adf[st == target & yr == TEST_YEAR, , drop = FALSE]
  tg_tr <- targets_of(tr); tg_te <- targets_of(te)
  base <- mean(tg_te$ie)
  strata_tr <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(tr[[HH_SIZE_COL]]) %in% h))
  strata_te <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(te[[HH_SIZE_COL]]) %in% h))
  cat(sprintf("\n== %s: train %d rows (%.1f%% err) | test-2024 %d rows (%.1f%% err)\n",
              target, nrow(tr), 100 * mean(tg_tr$ie), nrow(te), 100 * base))

  # full pool evaluated once on state train and state 2024
  idx_tr <- flags_for_rules(pool, tr, strata_tr, label = "")
  idx_te <- flags_for_rules(pool, te, strata_te, label = "")
  n_tr <- lengths(idx_tr)
  k_tr <- vapply(idx_tr, function(ix) sum(tg_tr$ie[ix]), numeric(1))
  lcb_st <- wilson_lcb(k_tr, n_tr, STATE_LCB_Z)

  # arm 1: national as-is (national LCB order, full pool)
  emit(target, "national_asis", nrow(pool), pool$precision_train_lcb,
       idx_te, nrow(te), tg_te, base)

  # arm 2: filtered (national thresholds, state qualification, state-LCB order)
  qual_f <- which(n_tr >= SUPPORT_CONF & lcb_st >= MIN_STATE_PRECISION)
  emit(target, "filtered", length(qual_f), lcb_st[qual_f],
       idx_te[qual_f], nrow(te), tg_te, base)

  # tuning arms: top TUNE_TOP_RULES by national LCB that fire in the state
  cand <- order(-pool$precision_train_lcb)
  cand <- cand[n_tr[cand] > 0][seq_len(min(TUNE_TOP_RULES, sum(n_tr > 0)))]
  vars_used <- unique(unlist(lapply(pool$rule[cand],
                                    function(r) .parse_rule(r)$var)))
  uniq <- lapply(setNames(nm = vars_used), function(v)
    sort(unique(suppressWarnings(as.numeric(tr[[v]])))))

  tuned_rows  <- list()   # per candidate rule: deployed variant per scheme
  hybrid_rows <- list()
  for (i in cand) {
    hh <- pool$hh[i]
    variants <- unique(c(pool$rule[i], rule_variants(pool$rule[i], uniq)))
    ev <- lapply(variants, function(vr) {
      ix <- eval_rule_on(vr, tr, strata_tr[[hh]])
      c(n = length(ix), k = sum(tg_tr$ie[ix]), d = sum(tg_tr$ed[ix]))
    })
    n <- vapply(ev, `[[`, 0, "n"); k <- vapply(ev, `[[`, 0, "k")
    d <- vapply(ev, `[[`, 0, "d")
    lcb <- wilson_lcb(k, n, STATE_LCB_Z)
    qt <- which(n >= SUPPORT_CONF & lcb >= MIN_STATE_PRECISION)
    if (length(qt) > 0) {
      j <- qt[which.max(lcb[qt])]
      tuned_rows[[length(tuned_rows) + 1]] <-
        list(rule = variants[j], hh = hh, stat = lcb[j])
    }
    qh <- which(n >= SUPPORT_HYBRID & lcb >= MIN_STATE_PRECISION)
    if (length(qh) > 0) {
      j <- qh[which.max(d[qh])]
      hybrid_rows[[length(hybrid_rows) + 1]] <-
        list(rule = variants[j], hh = hh, stat = lcb[j])
    }
  }
  for (arm in list(list(name = "tuned", rows = tuned_rows),
                   list(name = "hybrid", rows = hybrid_rows))) {
    if (length(arm$rows) == 0) {
      emit(target, arm$name, 0, numeric(0), list(), nrow(te), tg_te, base)
      next
    }
    dep <- bind_rows(lapply(arm$rows, as.data.frame))
    idx_dep <- lapply(seq_len(nrow(dep)), function(j)
      eval_rule_on(dep$rule[j], te, strata_te[[dep$hh[j]]]))
    emit(target, arm$name, nrow(dep), dep$stat, idx_dep, nrow(te), tg_te, base)
  }
  saveRDS(bind_rows(res), file.path(out_dir, "deployment_state_adaptation_partial.rds"))
}

out <- bind_rows(res)
write.csv(out, file.path(out_dir, "deployment_state_adaptation.csv"), row.names = FALSE)

## ── #9-format summary tables, one per budget ──────────────────────────────────
for (b in BUDGETS) {
  tb <- out %>%
    filter(budget == b) %>%
    mutate(cell = sprintf("%.3f @ %.1f%% ($%.1f%%)",
                          precision, 100 * recall, 100 * dollar_recall)) %>%
    select(target, approach, n_deployable_rules, cell) %>%
    tidyr::pivot_wider(names_from = approach,
                       values_from = c(n_deployable_rules, cell))
  fn <- file.path(out_dir, sprintf("deployment_state_adaptation_summary_budget%02.0f.csv", 100 * b))
  write.csv(tb, fn, row.names = FALSE)
  cat(sprintf("wrote %s\n", fn))
}
cat(sprintf("wrote %s (%d rows)\n",
            file.path(out_dir, "deployment_state_adaptation.csv"), nrow(out)))
