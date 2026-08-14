# Three-arm frozen lists per state, 49 states: STATE-ONLY, NATIONAL-ONLY,
# and BLENDED, all built from the CACHED v2.5.0 benchmark pools (mined
# FY2022-23, seed 117, methods/v250_benchmark_2024/cache/) - evaluation
# only, no mining. Each arm's list is built on the state's FY2022-23
# caseload with the shipped fresh-share walk (f = 0.50, tolerated
# per-phase gaps), frozen, walked on the state's FY2024 cases to that
# year's cap, outcome-free. Purpose: the per-state figure comparing the
# blend to its two ingredients (states_two_regimes successor at 49
# states), and reusable per-state single-pool lists whose performance is
# easy to recompute.
#
# Consistency guard: the blended arm reproduces the v2.5.0 benchmark
# (same caches, same walk); its precision is asserted equal to
# methods/v250_benchmark_2024/v250_benchmark_2024.csv per state x budget.
#
# Artifact gates (findings 38, mandatory): tagged rules (mm-share >= 0.25
# of flags or errors) are dropped from every arm's pool before the fill;
# the blend head gate mirrors the benchmark. State pools report-only.
#
# Outputs -> methods/threearm_2024/:
#   threearm_results_2024.csv          one row per state x arm x budget
#   lists/list_<arm>_<State>_budget05/10.csv   the frozen lists (walk order)
#
# Requires the benchmark caches for all 49 states + national; stops if
# any is missing (run the benchmark first - this script never mines).
# Expects `reg_model_data`. Run via runners/run_threearm_2024.R.

suppressMessages(library(dplyr))
source("rule_mining_helpers.R")

SEED        <- 117
BUDGETS     <- c(0.05, 0.10)
BUFFER_MULT <- 3
FRESH_MIN   <- 0.50
TRAIN_YEARS <- c("2022", "2023")
TEST_YEAR   <- "2024"
VOCAB19 <- c(
  "HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size",
  "expedited_i", "bbce_state_i", "rawben_rel_max", "medical_deductions",
  "shelter_expenses_by_hh_size", "utilities", "married", "homeless",
  "percent_abawd", "unc_rawben_rel_max", "months_since_cert_n",
  "count_divisible_by_100",
  "gross_by_hh_size", "earned_by_hh_size", "unearned_by_hh_size")
MM_TAG_SHARE <- 0.25
MM_TOP40_MAX <- 1L; MM_TOP10_MAX <- 0L

CACHE_DIR <- "methods/v250_benchmark_2024/cache"
BENCH_CSV <- "methods/v250_benchmark_2024/v250_benchmark_2024.csv"
OUT_DIR   <- "methods/threearm_2024"
LIST_DIR  <- file.path(OUT_DIR, "lists")
dir.create(LIST_DIR, showWarnings = FALSE, recursive = TRUE)

HH_LEVELS <- c("1", "2-3", "4+")
hh_group_of <- function(n) {
  n <- suppressWarnings(as.numeric(as.character(n)))
  ifelse(is.na(n), NA_character_, ifelse(n <= 1, "1", ifelse(n <= 3, "2-3", "4+")))
}
stamp <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"),
                                   sprintf(...)))

## ---- frame (identical prep to the benchmark) --------------------------------
stopifnot(nrow(reg_model_data) == 231619L)
adf0 <- reg_model_data %>% filter(fiscal_year %in% c(TRAIN_YEARS, TEST_YEAR))
pf  <- prep_features(adf0, VOCAB19)
adf <- pf$data
stopifnot(length(setdiff(VOCAB19, pf$features)) == 0)
st <- as.character(adf$state)
yr <- as.character(adf$fiscal_year)
ie_all <- !is.na(adf$over_threshold) & adf$over_threshold != 0
ed_all <- ifelse(ie_all, abs(ifelse(is.na(adf$total_error_amount), 0,
                                    adf$total_error_amount)), 0)
stopifnot(sum(yr %in% TRAIN_YEARS) == 76031L,
          sum(ie_all[yr %in% TRAIN_YEARS]) == 8397L,
          sum(yr == TEST_YEAR) == 39528L,
          sum(ie_all[yr == TEST_YEAR]) == 4764L)
hh_all <- hh_group_of(adf$cert_HH_size_FS_n)
is_tr <- yr %in% TRAIN_YEARS
STATES <- sort(unique(st))

bench_ref <- read.csv(BENCH_CSV)

## ---- pools from cache (never mine here) -------------------------------------
tag_pool <- function(adm, unit) {
  if (!nrow(adm)) {
    adm$mm_share_flags <- numeric(0); adm$mm_share_errors <- numeric(0)
    adm$mm_inflation <- numeric(0); adm$artifact_i <- logical(0)
    return(adm)
  }
  adm$mm_share_flags  <- round(adm$mm_n / adm$n, 4)
  adm$mm_share_errors <- round(ifelse(adm$k > 0, adm$mm_k / adm$k, 0), 4)
  adm$mm_inflation    <- round(adm$mm_k / adm$n, 4)
  adm$artifact_i <- adm$mm_share_flags >= MM_TAG_SHARE |
    adm$mm_share_errors >= MM_TAG_SHARE
  stamp("  [%s] pool %d rules | tagged %d (dropped before every fill)",
        unit, nrow(adm), sum(adm$artifact_i))
  adm
}

nat_fn <- file.path(CACHE_DIR, sprintf("bench_national_%d.rds", SEED))
stopifnot(file.exists(nat_fn))
natl <- readRDS(nat_fn)
stopifnot("mm_n" %in% names(natl))
natl <- tag_pool(natl, "national")
natl$pool <- "national"

blend_head_gate <- function(pool_shadow, unit) {
  top40 <- sum(pool_shadow$artifact_i[seq_len(min(40L, nrow(pool_shadow)))])
  top10 <- sum(pool_shadow$artifact_i[seq_len(min(10L, nrow(pool_shadow)))])
  if (top40 > MM_TOP40_MAX || top10 > MM_TOP10_MAX)
    stop(sprintf("DISPLACEMENT HALT [blend head, %s]: top40 %d | top10 %d",
                 unit, top40, top10))
}

## ---- walk + scoring (verbatim from the benchmark) ---------------------------
walk_fill <- function(idx_tr, n_tr, b) {
  cap <- floor(b * n_tr); cap_buf <- floor(BUFFER_MULT * b * n_tr)
  nfl <- vapply(idx_tr, length, integer(1))
  un <- rep(FALSE, n_tr); n_in <- 0L; n_core <- 0L
  frozen <- integer(0); buffer <- integer(0)
  for (i in seq_along(idx_tr)) {
    add <- sum(!un[idx_tr[[i]]]); if (add == 0) next
    if (n_in + add <= cap) {
      un[idx_tr[[i]]] <- TRUE; n_in <- n_in + add; n_core <- n_core + add
      frozen <- c(frozen, i)
    } else if (n_in + add <= cap_buf) {
      un[idx_tr[[i]]] <- TRUE; n_in <- n_in + add; buffer <- c(buffer, i)
    }
  }
  gap_core <- 0L; gap_total <- 0L
  if (FRESH_MIN > 0) {
    C0 <- n_core; CT <- n_in
    un <- rep(FALSE, n_tr); n_in <- 0L
    taken <- logical(length(idx_tr))
    frozen <- integer(0); buffer <- integer(0)
    for (ph in 1:2) {
      tgt <- if (ph == 1) C0 else CT
      for (ps in 1:2) {
        if (n_in >= tgt) break
        for (i in seq_along(idx_tr)) {
          if (taken[i]) next
          ix <- idx_tr[[i]]
          add <- sum(!un[ix])
          if (add == 0L) next
          if (ps == 1 && add / nfl[i] < FRESH_MIN) next
          if (n_in + add > tgt) next
          un[ix] <- TRUE; n_in <- n_in + add; taken[i] <- TRUE
          if (ph == 1) frozen <- c(frozen, i) else buffer <- c(buffer, i)
          if (n_in == tgt) break
        }
      }
      if (ph == 1) gap_core <- C0 - n_in
    }
    gap_total <- CT - n_in
    stopifnot(gap_core >= 0L, gap_total >= 0L)
  }
  list(frozen = frozen, buffer = buffer, fill_cases = n_in,
       gap_core = gap_core, gap_total = gap_total)
}

score_2024 <- function(sel, idx_te, n_te, b, err_te, doll_te) {
  cap24 <- floor(b * n_te); un24 <- rep(FALSE, n_te)
  for (i in sel) {
    add <- sum(!un24[idx_te[[i]]])
    if (add > 0 && sum(un24) + add <= cap24) un24[idx_te[[i]]] <- TRUE
  }
  fl <- un24
  prec <- sum(fl & err_te) / max(sum(fl), 1)
  c(n_flagged = sum(fl), n_errors = sum(fl & err_te), precision = prec,
    dollar_recall = sum(doll_te[fl]) / max(sum(doll_te), 1))
}

## ---- per-state, three arms ---------------------------------------------------
COLS <- c("hh", "rule", "n", "k", "lcb", "mm_share_flags",
          "mm_share_errors", "mm_inflation", "artifact_i", "pool")
blend_of <- function(a, b) bind_rows(a, b) %>%
  arrange(desc(lcb), desc(n), hh, rule) %>%
  distinct(hh, rule, .keep_all = TRUE)

rows <- list()
for (state in STATES) {
  tr_s <- which(st == state & is_tr)
  te_s <- which(st == state & yr == TEST_YEAR)
  trs <- adf[tr_s, , drop = FALSE]; tes <- adf[te_s, , drop = FALSE]
  err_te <- ie_all[te_s]; doll_te <- ed_all[te_s]
  strata_trs <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_all[tr_s] %in% h))
  strata_tes <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_all[te_s] %in% h))

  own_fn <- file.path(CACHE_DIR, sprintf("bench_state_%s_%d.rds",
                                         gsub(" ", "_", state), SEED))
  stopifnot(file.exists(own_fn))
  own <- readRDS(own_fn)
  if (nrow(own)) stopifnot("mm_n" %in% names(own))
  own <- tag_pool(own, state)
  if (nrow(own)) own$pool <- "state"

  shadow <- blend_of(natl[, COLS], if (nrow(own)) own[, COLS] else NULL)
  blend_head_gate(shadow, state)

  arms <- list(
    state    = if (nrow(own)) own[!own$artifact_i, COLS, drop = FALSE] else natl[0, COLS],
    national = natl[!natl$artifact_i, COLS],
    blended  = blend_of(natl[!natl$artifact_i, COLS],
                        if (nrow(own)) own[!own$artifact_i, COLS, drop = FALSE] else NULL))

  for (arm in names(arms)) {
    pool <- arms[[arm]]
    if (!nrow(pool)) {
      for (b in BUDGETS)
        rows[[length(rows) + 1]] <- data.frame(
          state = state, arm = arm, budget = b, n_pool = 0L, n_core = 0L,
          n_buffer = 0L, n_te = nrow(tes), n_flagged = 0L,
          n_errors_caught = 0L,
          precision = NA_real_, base_rate_te = round(mean(err_te), 4),
          dollar_recall = NA_real_, fill_gap_core = NA_integer_,
          fill_gap_total = NA_integer_)
      next
    }
    idx_tr <- flags_for_rules(pool, trs, strata_trs, label = "")
    idx_te <- flags_for_rules(pool, tes, strata_tes, label = "")
    for (b in BUDGETS) {
      wf <- walk_fill(idx_tr, nrow(trs), b)
      sel <- c(wf$frozen, wf$buffer)
      s24 <- score_2024(sel, idx_te, nrow(tes), b, err_te, doll_te)
      rows[[length(rows) + 1]] <- data.frame(
        state = state, arm = arm, budget = b, n_pool = nrow(pool),
        n_core = length(wf$frozen), n_buffer = length(wf$buffer),
        n_te = nrow(tes),
        n_flagged = s24[["n_flagged"]], n_errors_caught = s24[["n_errors"]],
        precision = round(s24[["precision"]], 4),
        base_rate_te = round(mean(err_te), 4),
        dollar_recall = round(s24[["dollar_recall"]], 4),
        fill_gap_core = wf$gap_core, fill_gap_total = wf$gap_total)
      lst <- pool[sel, c("hh", "rule", "pool", "n", "k", "lcb",
                         "mm_share_flags", "mm_share_errors", "mm_inflation")]
      names(lst)[names(lst) == "n"] <- "n_flagged_train"
      names(lst)[names(lst) == "k"] <- "k_errors_train"
      names(lst)[names(lst) == "lcb"] <- "precision_train_lcb"
      lst$rank <- seq_len(nrow(lst))
      lst$role <- c(rep("core", length(wf$frozen)),
                    rep("buffer", length(wf$buffer)))
      write.csv(lst[, c("rank", "role", "hh", "rule", "pool",
                        "n_flagged_train", "k_errors_train",
                        "precision_train_lcb", "mm_share_flags",
                        "mm_share_errors", "mm_inflation")],
                file.path(LIST_DIR, sprintf("list_%s_%s_budget%02d.csv",
                                            arm, gsub(" ", "_", state),
                                            round(100 * b))),
                row.names = FALSE)
    }
    rm(idx_tr, idx_te)
  }
  stamp("  %s done (own pool %d rules)", state, nrow(own))
  rm(trs, tes); invisible(gc())
}

res <- bind_rows(rows)
write.csv(res, file.path(OUT_DIR, "threearm_results_2024.csv"), row.names = FALSE)

## ---- consistency guard: blended arm must reproduce the benchmark ------------
chk <- res %>% filter(arm == "blended") %>%
  inner_join(bench_ref %>% select(state, budget, precision_ref = precision,
                                  dollar_ref = dollar_recall),
             by = c("state", "budget"))
bad <- chk %>% filter(abs(precision - precision_ref) > 1e-6 |
                      abs(dollar_recall - dollar_ref) > 1e-6)
if (nrow(bad)) {
  print(bad[, c("state", "budget", "precision", "precision_ref")])
  stop("blended arm does not reproduce the benchmark - investigate before using")
}
stamp("consistency guard: blended arm reproduces the benchmark in %d of %d cells",
      nrow(chk), nrow(chk))

for (b in BUDGETS) {
  s <- res %>% filter(budget == b) %>%
    tidyr::pivot_wider(id_cols = state, names_from = arm,
                       values_from = precision, names_prefix = "p_")
  has_own <- !is.na(s$p_state)
  stamp("SUMMARY %d%%: blended >= best single arm in %d of %d states with an own pool; %d states have no own-pool list",
        round(100 * b),
        sum(s$p_blended[has_own] >=
              pmax(s$p_state[has_own], s$p_national[has_own]) - 1e-9),
        sum(has_own), sum(!has_own))
}
stamp("three-arm results written to %s", OUT_DIR)
