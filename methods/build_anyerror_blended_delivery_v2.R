# ANY-ERROR blended delivery lists + a 2024 holdout scorecard.
#
# Same two builds as methods/build_typed_blended_delivery_v2.R, on the shipped
# any-error vocabulary instead of the typed five-frame one:
#
#   BENCH  mine on 2022-2023, fill each state's list against its 2022-23
#          caseload, then score that frozen list on the state's 2024 cases.
#          Nothing from 2024 touches mining, admission, ranking, or fill.
#   FINAL  mine on 2022-2024 and fill against the 2022-24 caseload. This is
#          the rule set a state is handed; it has no holdout, exactly as in
#          INCL_build_blended_delivery_list_v2.R, and BENCH is what prices it.
#
# The FINAL half reproduces INCL_build_blended_delivery_list_v2.R with
# MINING_FRAMES <- "any_error": same pool cache, same rank statistic, same fill,
# same 13-column schema, same plain filenames. It is included here so one run
# produces the delivered lists and the scorecard that prices them from a single
# pass over the caches, and so a rebuild can be diffed against the committed
# lists as a reproducibility check.
#
# The recipe is the settled one: xgboost + ranger at depth 4, per household-size
# stratum (1 / 2-3 / 4+); admission by Benjamini-Hochberg at FDR 10% vs the
# stratum base rate AND n >= 30; ordering by the one-sided 99% Wilson lower
# bound of training precision; fill to the review budget as "core" plus
# substitutes to 3x depth as "buffer".
#
# Outputs, all written as each state finishes so a kill loses at most one state:
#   state_delivery_lists/blended_delivery_<State>_2022_2024_budget{05,10}.csv
#   methods/anyerror_blended_holdout_2024/holdout_metrics.json  (+ .jsonl, .csv)
#   methods/anyerror_blended_holdout_2024/bench_list_<State>_budget{05,10}.csv
#
# Expects `reg_model_data` in the environment. Mined pools are cached per
# (pool x frame x era); with both eras' any-error mines already on disk this
# run does no mining at all.

suppressMessages({ library(dplyr); library(jsonlite) })
source("rule_mining_helpers.R")
set.seed(117)

ALL_YEARS    <- c("2022", "2023", "2024")
BENCH_YEARS  <- c("2022", "2023")
HOLDOUT_YEAR <- "2024"
FINAL_YEARS  <- ALL_YEARS
BUDGETS      <- c(0.05, 0.10)
BUFFER_MULT  <- 3
LCB_Z        <- 2.326
# Engine settings and the output/cache paths can be pre-set before source() --
# a smoke test shrinks the engines and redirects every path to a scratch
# directory so it cannot touch real caches or delivery lists.
if (!exists("XGB")) XGB <- list(nrounds = 1000, max_depth = 4, eta = 0.02, subsample = 0.20)
if (!exists("RF"))  RF  <- list(num_trees = 1000, max_depth = 4, mtry = 2, min_node_size = 20)
SIGNIF_DIGITS     <- 3
MIN_TRAIN_FLAGGED <- 30
FDR_ALPHA         <- 0.10
ADMISSION         <- "fdr10"
RANK_STAT         <- "precision_train_lcb"

MINING_FRAMES <- "any_error"
FRAME_TAG     <- "anyerror"

# Both cache directories already hold filtered_<pool>_any_error_fdr10.rds for
# all 50 pools (49 states + national), mined under exactly these settings: the
# 2022-24 files by the shipped delivery builder, the 2022-23 files by the typed
# build (the any-error frame is mined identically either way, so the files are
# vocabulary-neutral and were copied over). The merged pool_<pool>_anyerror_*
# caches exist for 2022-24 only, so this run rebuilds them for 2022-23 -- dedup,
# not mining.
if (!exists("CACHE_BENCH")) CACHE_BENCH <- "methods/delivery_pools_2022_2023_anyerror"
if (!exists("CACHE_FINAL")) CACHE_FINAL <- "methods/delivery_pools_2022_2024_v3"
if (!exists("LIST_DIR"))    LIST_DIR    <- "state_delivery_lists"
if (!exists("BENCH_DIR"))   BENCH_DIR   <- "methods/anyerror_blended_holdout_2024"
for (d in c(CACHE_BENCH, CACHE_FINAL, LIST_DIR, BENCH_DIR))
  dir.create(d, showWarnings = FALSE, recursive = TRUE)

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
  ie  <- !is.na(df$over_threshold) & df$over_threshold != 0
  amt <- df$total_error_amount; amt[is.na(amt)] <- 0
  list(ie = ie, ed = ifelse(ie, abs(amt), 0))
}
stamp <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"),
                                   sprintf(...)))

# Features are coerced ONCE over all three years, then rows are subset by era.
# prep_features only recodes types (two-level factors to 0/1); it computes no
# statistics from the outcome, so a rule's text means the same thing in every
# era and the 2024 holdout stays untouched by mining.
pf  <- prep_features(reg_model_data %>% filter(fiscal_year %in% ALL_YEARS), features)
adf <- pf$data
pvars <- pf$features
st  <- as.character(adf$state)
fy  <- as.character(adf$fiscal_year)
stamp("frame: %d rows over %s | vocabulary: %s", nrow(adf),
      paste(ALL_YEARS, collapse = "+"), FRAME_TAG)

## ── Memory-frugal dedup ───────────────────────────────────────────────────────
# Both dedup stages are stratum-local -- exact coverage cannot match across
# disjoint strata, and dominance groups by (hh, signature) -- so we walk one
# stratum at a time against a stratum-local condition index. Same result as the
# whole-pool dedup in the shipped builder, a fraction of the peak memory.
dedup_pool_by_stratum <- function(rules_df, train, strata_idx, stat) {
  keep <- rep(FALSE, nrow(rules_df))
  for (h in HH_LEVELS) {
    rows <- which(rules_df$hh == h)
    if (!length(rows)) next
    sub  <- train[strata_idx[[h]], , drop = FALSE]
    ci   <- build_condition_index(rules_df$rule[rows])
    stamp("  dedup hh=%-3s : %d rules -> %d unique conditions", h,
          length(rows), length(ci$conditions))
    idx  <- eval_conditions(ci$conditions, sub)
    fl   <- rule_flag_idx(ci$rule_conds, idx)
    rm(idx); invisible(gc())
    sdf  <- rules_df[rows, , drop = FALSE]
    dcov <- dedup_exact_coverage(sdf, fl)
    rm(fl); invisible(gc())
    rows <- rows[!dcov]; sdf <- sdf[!dcov, , drop = FALSE]
    ddom <- dedup_dominated(sdf, stat[rows])
    keep[rows[!ddom]] <- TRUE
    stamp("  dedup hh=%-3s : coverage -%d, dominance -%d, kept %d", h,
          sum(dcov), sum(ddom), sum(!ddom))
    invisible(gc())
  }
  rules_df[keep, , drop = FALSE]
}

## ── Mine one pool for one era ─────────────────────────────────────────────────
mine_pool <- function(pool_states, cache_name, years, cache_dir) {
  cache <- file.path(cache_dir, sprintf("pool_%s_%s_%s.rds", cache_name, FRAME_TAG, ADMISSION))
  if (file.exists(cache)) return(readRDS(cache))
  sel   <- st %in% pool_states & fy %in% years
  train <- adf[sel, , drop = FALSE]
  tg_tr <- targets_of(train)
  strata_tr <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(train$cert_HH_size_FS_n) %in% h))
  empty <- data.frame(hh = character(), rule = character(), engines = character(),
                      mined_frames = character(), n_flagged_train = integer(),
                      precision_train = numeric(), precision_train_lcb = numeric(),
                      dollars_per_flag_train = numeric(), stringsAsFactors = FALSE)

  # Mine, score on the any-error target over the pool's full caseload, admit,
  # keep the survivors. Checkpointed per frame; with the caches in place this
  # loop only reads.
  filt <- list()
  for (fn in MINING_FRAMES) {
    ck <- file.path(cache_dir, sprintf("filtered_%s_%s_%s.rds", cache_name, fn, ADMISSION))
    if (file.exists(ck)) {
      filt[[fn]] <- readRDS(ck)
      stamp("  %-22s cached  (%d rules)", fn, nrow(filt[[fn]]))
      next
    }
    stamp("  %-22s mining on %d rows ...", fn, nrow(train))
    rdf <- mine_rule_vocabulary(
      train, setNames(list(list(rows = seq_len(nrow(train)), ie = tg_tr$ie)), fn),
      strata_tr, pvars, xgb = XGB, rf = RF,
      signif_digits = SIGNIF_DIGITS, seed = 117)
    if (is.null(rdf) || nrow(rdf) == 0) {
      saveRDS(empty, ck); filt[[fn]] <- empty; next
    }
    stats <- reduce_flags_for_rules(
      rdf, train, strata_tr,
      fun = function(ix) c(length(ix), sum(tg_tr$ie[ix]), sum(tg_tr$ed[ix])),
      label = sprintf("score %s", fn))
    n_tr <- as.integer(stats[, 1]); k_tr <- stats[, 2]; d_tr <- stats[, 3]
    raw  <- ifelse(n_tr > 0, k_tr / n_tr, NA_real_)
    base_by_hh <- vapply(strata_tr, function(rr) mean(tg_tr$ie[rr]), numeric(1))
    base <- base_by_hh[rdf$hh]
    # BH within this frame's candidates, one-sided vs the stratum base rate.
    # Named `pvals`: a local `pv` would shadow the feature vector.
    pvals <- pbinom(k_tr - 1, n_tr, base, lower.tail = FALSE)
    m <- length(pvals); o <- order(pvals)
    thr <- max(c(0L, which(pvals[o] <= FDR_ALPHA * seq_len(m) / m)))
    bh <- rep(FALSE, m); if (thr > 0) bh[o[seq_len(thr)]] <- TRUE
    keep <- bh & n_tr >= MIN_TRAIN_FLAGGED
    rdf <- rdf[keep, , drop = FALSE]
    rdf$n_flagged_train       <- n_tr[keep]
    rdf$precision_train       <- round(raw[keep], 4)
    rdf$precision_train_lcb   <- round(wilson_lcb(k_tr[keep], n_tr[keep], LCB_Z), 4)
    rdf$dollars_per_flag_train<- round(d_tr[keep] / n_tr[keep], 2)
    saveRDS(rdf, ck)
    filt[[fn]] <- rdf
    stamp("  %-22s admitted %d of %d", fn, nrow(rdf), m)
    rm(stats); invisible(gc())
  }

  all_f <- do.call(rbind, filt)
  if (is.null(all_f) || nrow(all_f) == 0) return(NULL)
  all_f <- all_f[order(all_f$hh, all_f$rule, method = "radix"), , drop = FALSE]
  rownames(all_f) <- NULL
  stamp("  %d admitted rules before dedup", nrow(all_f))
  rules_df <- dedup_pool_by_stratum(all_f, train, strata_tr, all_f[[RANK_STAT]])
  saveRDS(rules_df, cache)
  rules_df
}

## ── Fill a frozen list against a caseload ─────────────────────────────────────
# Walks the pool in rank order and takes a rule only if it adds cases not
# already flagged: into "core" while the review budget holds, then into
# "buffer" out to BUFFER_MULT x depth. Outcome-free -- nothing here looks at
# whether the cases it adds are errors.
fill_list <- function(pool, idx, n_rows, budget) {
  ord <- order(-pool[[RANK_STAT]], pool$hh, pool$rule, method = "radix")
  cap <- floor(budget * n_rows); cap_buf <- floor(BUFFER_MULT * budget * n_rows)
  un <- rep(FALSE, n_rows); n_in <- 0L
  frozen <- integer(0); buffer <- integer(0)
  for (i in ord) {
    add <- sum(!un[idx[[i]]])
    if (add == 0) next
    if (n_in + add <= cap) {
      un[idx[[i]]] <- TRUE; n_in <- n_in + add; frozen <- c(frozen, i)
    } else if (n_in + add <= cap_buf) {
      un[idx[[i]]] <- TRUE; n_in <- n_in + add; buffer <- c(buffer, i)
    }
  }
  list(core = frozen, buffer = buffer)
}

# The delivered schema, identical to INCL_build_blended_delivery_list_v2.R.
hand_off <- function(pool, sel, idx, n_core, n_rows) {
  cols <- c("rule", "hh", "pool", "engines", "mined_frames",
            "n_flagged_train", "precision_train", "precision_train_lcb",
            "dollars_per_flag_train")
  hand <- pool[sel, cols]
  hand$n_flagged_state <- lengths(idx[sel])
  # marginal new cases at each rank, walked in the DELIVERED order (core then
  # buffer) -- what a state activating rules in rank order actually sees
  un <- rep(FALSE, n_rows)
  nn <- integer(length(sel))
  for (j in seq_along(sel)) {
    ix <- idx[[sel[j]]]; nn[j] <- sum(!un[ix]); un[ix] <- TRUE
  }
  hand$n_new_at_rank <- nn
  hand$rank <- seq_along(sel)
  hand$role <- rep(c("core", "buffer"), c(n_core, length(sel) - n_core))
  hand
}

## ── Flags against several datasets from one condition index ───────────────────
# flags_for_rules() rebuilds the condition index every call. The bench pass
# needs the SAME rules against two datasets (the state's 2022-23 rows and its
# 2024 rows), so the index is built once and only the evaluation repeats.
flags_from_index <- function(ci, hh, data, strata_idx) {
  idx <- eval_conditions(ci$conditions, data)
  out <- vector("list", length(hh))
  for (h in unique(hh)) {
    rows <- which(hh == h)
    out[rows] <- rule_flag_idx(ci$rule_conds[rows], idx, base_idx = strata_idx[[h]])
  }
  out
}

## ── National pools, both eras ─────────────────────────────────────────────────
all_states <- sort(unique(st[fy %in% ALL_YEARS]))   # the national pool is always all 49
states <- if (exists("DELIVERY_STATES")) DELIVERY_STATES else all_states
stamp("=== PHASE 1: national pool, %s (bench) ===", paste(BENCH_YEARS, collapse = "-"))
natl_b <- mine_pool(all_states, "national", BENCH_YEARS, CACHE_BENCH)
natl_b$pool <- "national"
stamp("national bench pool: %d rules", nrow(natl_b))

stamp("=== PHASE 2: national pool, %s (final) ===", paste(FINAL_YEARS, collapse = "-"))
natl_f <- mine_pool(all_states, "national", FINAL_YEARS, CACHE_FINAL)
natl_f$pool <- "national"
stamp("national final pool: %d rules", nrow(natl_f))

## ── Per state ─────────────────────────────────────────────────────────────────
jsonl <- file.path(BENCH_DIR, "holdout_metrics.jsonl")
records <- list()
if (file.exists(jsonl)) {
  records <- lapply(readLines(jsonl, warn = FALSE), function(l)
    tryCatch(fromJSON(l), error = function(e) NULL))
  records <- Filter(Negate(is.null), records)
  stamp("resuming: %d records already on disk", length(records))
}

write_outputs <- function() {
  if (!length(records)) return(invisible(NULL))
  df <- bind_rows(lapply(records, as.data.frame, stringsAsFactors = FALSE))
  write.csv(df, file.path(BENCH_DIR, "holdout_metrics.csv"), row.names = FALSE)
  write_json(list(
    holdout_fiscal_year = HOLDOUT_YEAR,
    train_years         = paste(BENCH_YEARS, collapse = "-"),
    delivered_rule_years= paste(FINAL_YEARS, collapse = "-"),
    vocabulary          = "any_error (the shipped delivery vocabulary)",
    admission           = sprintf("BH FDR %.0f%% vs stratum base rate AND n >= %d",
                                  100 * FDR_ALPHA, MIN_TRAIN_FLAGGED),
    ranking             = sprintf("one-sided %.1f%% Wilson LCB of training precision",
                                  100 * (1 - pnorm(-LCB_Z))),
    per_basis           = "dollar recall: flagged error dollars / total error dollars",
    n_records           = length(records),
    records             = df),
    file.path(BENCH_DIR, "holdout_metrics.json"),
    auto_unbox = TRUE, digits = 6, pretty = TRUE)
}

done <- if (length(records))
  unique(vapply(records, function(r) as.character(r$state), character(1))) else character(0)

for (S in states) {
  # Resume: a state is finished once it has scorecard records AND its 2022-24
  # lists have been rewritten by this run. `done` is read from the jsonl, which
  # is only written after the bench pass, so a kill mid-state re-does that state.
  if (S %in% done) { stamp("skip %s (done)", S); next }
  stamp("########## %s ##########", S)

  ## BENCH: 2022-23 pool -> list filled on 2022-23 -> scored on 2024
  own_b <- mine_pool(S, gsub("[^A-Za-z]", "", S), BENCH_YEARS, CACHE_BENCH)
  if (is.null(own_b)) { own_b <- natl_b[0, ] } else { own_b$pool <- "state" }
  pool_b <- bind_rows(natl_b, own_b) %>%
    arrange(desc(.data[[RANK_STAT]]), hh, rule) %>%
    distinct(hh, rule, .keep_all = TRUE)

  tr_rows <- which(st == S & fy %in% BENCH_YEARS)
  ho_rows <- which(st == S & fy == HOLDOUT_YEAR)
  tr <- adf[tr_rows, , drop = FALSE]; ho <- adf[ho_rows, , drop = FALSE]
  strata_b  <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(tr$cert_HH_size_FS_n) %in% h))
  strata_ho <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(ho$cert_HH_size_FS_n) %in% h))
  ci_b   <- build_condition_index(pool_b$rule)
  idx_b  <- flags_from_index(ci_b, pool_b$hh, tr, strata_b)
  idx_ho <- flags_from_index(ci_b, pool_b$hh, ho, strata_ho)

  tg_ho <- targets_of(ho)
  w     <- ho$fywgt; w[is.na(w)] <- 0
  ben   <- ho$benefit_amount_FS; ben[is.na(ben)] <- 0
  tot_err_unw <- sum(tg_ho$ed)
  tot_err_wtd <- sum(w * tg_ho$ed)

  new_records <- list()
  for (b in BUDGETS) {
    f  <- fill_list(pool_b, idx_b, nrow(tr), b)
    hb <- hand_off(pool_b, c(f$core, f$buffer), idx_b, length(f$core), nrow(tr))
    write.csv(hb, file.path(BENCH_DIR, sprintf("bench_list_%s_budget%02.0f.csv",
              gsub(" ", "_", S), 100 * b)), row.names = FALSE)

    # the review budget is the CORE list; buffer rules are substitutes a state
    # swaps in, not extra capacity, so they are not scored here
    hit <- rep(FALSE, nrow(ho))
    for (i in f$core) hit[idx_ho[[i]]] <- TRUE
    fl_err_unw <- sum(tg_ho$ed[hit])
    fl_err_wtd <- sum((w * tg_ho$ed)[hit])
    new_records[[length(new_records) + 1]] <- list(
      state                    = S,
      holdout_fiscal_year      = HOLDOUT_YEAR,
      budget_pct               = 100 * b,
      n_rules_core             = length(f$core),
      n_rules_buffer           = length(f$buffer),
      # the three the scorecard is built around
      per_reduction_pts_unweighted = if (tot_err_unw > 0) 100 * fl_err_unw / tot_err_unw else NA_real_,
      per_reduction_pts_weighted   = if (tot_err_wtd > 0) 100 * fl_err_wtd / tot_err_wtd else NA_real_,
      per_total_weighted_error_dollars = tot_err_wtd,
      # counts
      n_errors_flagged_holdout = sum(tg_ho$ie & hit),
      n_errors_holdout         = sum(tg_ho$ie),
      n_cases_holdout          = nrow(ho),
      n_cases_flagged_holdout  = sum(hit),
      # components, so any other rate can be rebuilt without re-running
      error_dollars_flagged_holdout_weighted   = fl_err_wtd,
      error_dollars_flagged_holdout_unweighted = fl_err_unw,
      error_dollars_holdout_unweighted         = tot_err_unw,
      issuance_dollars_holdout_weighted        = sum(w * ben),
      weighted_cases_holdout                   = sum(w),
      precision_holdout        = if (sum(hit) > 0) sum(tg_ho$ie & hit) / sum(hit) else NA_real_,
      base_rate_holdout        = mean(tg_ho$ie),
      flagged_share_of_caseload= sum(hit) / nrow(ho))
    stamp("  bench %2.0f%%: %d core rules | flagged %d cases (%.1f%%), %d/%d errors | PER red %.1f%% unwtd, %.1f%% wtd",
          100 * b, length(f$core), sum(hit), 100 * sum(hit) / nrow(ho),
          sum(tg_ho$ie & hit), sum(tg_ho$ie),
          100 * fl_err_unw / max(tot_err_unw, 1), 100 * fl_err_wtd / max(tot_err_wtd, 1))
  }
  ## FINAL: 2022-24 pool -> the delivered list, no holdout
  own_f <- mine_pool(S, gsub("[^A-Za-z]", "", S), FINAL_YEARS, CACHE_FINAL)
  if (is.null(own_f)) { own_f <- natl_f[0, ] } else { own_f$pool <- "state" }
  pool_f <- bind_rows(natl_f, own_f) %>%
    arrange(desc(.data[[RANK_STAT]]), hh, rule) %>%
    distinct(hh, rule, .keep_all = TRUE)
  fr <- adf[st == S & fy %in% FINAL_YEARS, , drop = FALSE]
  strata_f <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(fr$cert_HH_size_FS_n) %in% h))
  idx_f <- flags_for_rules(pool_f, fr, strata_f)
  for (b in BUDGETS) {
    f  <- fill_list(pool_f, idx_f, nrow(fr), b)
    hf <- hand_off(pool_f, c(f$core, f$buffer), idx_f, length(f$core), nrow(fr))
    fn <- file.path(LIST_DIR, sprintf(
      "blended_delivery_%s_2022_2024_budget%02.0f.csv",
      gsub(" ", "_", S), 100 * b))
    write.csv(hf, fn, row.names = FALSE)
    stamp("  final %2.0f%%: core %d + buffer %d (%d state rules) -> %s",
          100 * b, length(f$core), length(f$buffer),
          sum(hf$pool == "state"), basename(fn))
  }

  # Records are committed to disk only now, after the delivered lists are
  # written, so `done` means "both halves finished" and a kill mid-state simply
  # redoes that state. A re-run of a state replaces its earlier records rather
  # than duplicating them.
  records <- Filter(function(r) !identical(as.character(r$state), S), records)
  records <- c(records, new_records)
  con <- file(jsonl, open = "w")
  for (r in records) writeLines(toJSON(r, auto_unbox = TRUE, digits = 6), con)
  close(con)
  write_outputs()

  rm(pool_b, pool_f, ci_b, idx_b, idx_ho, idx_f); invisible(gc())
}

write_outputs()
stamp("=== complete: %d records over %d states ===", length(records), length(states))
