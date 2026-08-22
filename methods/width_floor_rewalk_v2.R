# 49-state with/without narrow-interval-floor re-walk (design note:
# methods/width_floor_rewalk/design_note.md, 2026-08-21). Three arms over the
# cached FY2022-23 benchmark pools: the blended list as shipped (unfiltered;
# must reproduce the committed benchmark), and the same blend with rules
# dropped whose narrowest two-sided dollar interval has relative width below
# 0.02 / 0.05 (the section-40 fragile tail), applied BEFORE the fill so the
# buffer refill replaces them. Evaluation only, no mining; walk and scoring
# are the threearm/benchmark implementations verbatim. Expects
# `reg_model_data`. Run via runners/run_width_floor_rewalk.R.
suppressMessages(library(dplyr))
source("rule_mining_helpers.R")

SEED        <- 117
BUDGETS     <- c(0.05, 0.10)
BUFFER_MULT <- 3
FRESH_MIN   <- 0.50
TRAIN_YEARS <- c("2022", "2023")
TEST_YEAR   <- "2024"
FLOORS      <- c(floor02 = 0.02, floor05 = 0.05)
HARM_THR    <- -0.05
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
OUT_DIR   <- "methods/width_floor_rewalk"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

HH_LEVELS <- c("1", "2-3", "4+")
hh_group_of <- function(n) {
  n <- suppressWarnings(as.numeric(as.character(n)))
  ifelse(is.na(n), NA_character_, ifelse(n <= 1, "1", ifelse(n <= 3, "2-3", "4+")))
}
stamp <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"),
                                   sprintf(...)))

## ---- the section-40 width test, reused verbatim -----------------------------
DOLLAR_VARS <- c("medical_deductions", "utilities", "earned_by_hh_size",
                 "unearned_by_hh_size", "gross_by_hh_size",
                 "shelter_expenses_by_hh_size", "total_deductions_by_hh_size")
COND_PAT <- "([A-Za-z_][A-Za-z0-9_]*)\\s*(>=|<=|>|<)\\s*(-?[0-9.]+)"
rule_rel_width <- function(txt) {
  parts <- regmatches(txt, gregexpr(COND_PAT, txt, perl = TRUE))[[1]]
  if (!length(parts)) return(NA_real_)
  cc <- do.call(rbind, lapply(parts, function(p) {
    mm <- regmatches(p, regexec(COND_PAT, p))[[1]]
    data.frame(var = mm[2], op = mm[3], thr = as.numeric(mm[4]))
  }))
  w <- NA_real_
  for (v in intersect(unique(cc$var), DOLLAR_VARS)) {
    lo <- cc$thr[cc$var == v & cc$op %in% c(">", ">=")]
    hi <- cc$thr[cc$var == v & cc$op %in% c("<", "<=")]
    if (length(lo) && length(hi) && min(hi) > 0 && min(hi) > max(lo)) {
      rw <- (min(hi) - max(lo)) / min(hi)
      if (is.na(w) || rw < w) w <- rw
    }
  }
  w
}

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

## ---- walk + scoring (verbatim from the benchmark/threearm) ------------------
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

  blend <- blend_of(natl[!natl$artifact_i, COLS],
                    if (nrow(own)) own[!own$artifact_i, COLS, drop = FALSE] else NULL)
  rw <- vapply(blend$rule, rule_rel_width, numeric(1))
  arms <- c(list(unfiltered = blend),
            lapply(FLOORS, function(f) blend[is.na(rw) | rw >= f, , drop = FALSE]))
  stamp("%s: blend %d rules | narrow < 0.02: %d | < 0.05: %d",
        state, nrow(blend), sum(!is.na(rw) & rw < 0.02),
        sum(!is.na(rw) & rw < 0.05))

  for (arm in names(arms)) {
    pool <- arms[[arm]]
    idx_tr <- flags_for_rules(pool, trs, strata_trs, label = "")
    idx_te <- flags_for_rules(pool, tes, strata_tes, label = "")
    for (b in BUDGETS) {
      wf <- walk_fill(idx_tr, nrow(trs), b)
      sel <- c(wf$frozen, wf$buffer)
      s24 <- score_2024(sel, idx_te, nrow(tes), b, err_te, doll_te)
      rows[[length(rows) + 1]] <- data.frame(
        state = state, arm = arm, budget = b, n_pool = nrow(pool),
        n_dropped_by_floor = nrow(blend) - nrow(pool),
        n_core = length(wf$frozen), n_buffer = length(wf$buffer),
        n_te = nrow(tes),
        n_flagged = s24[["n_flagged"]], n_errors_caught = s24[["n_errors"]],
        precision = round(s24[["precision"]], 4),
        base_rate_te = round(mean(err_te), 4),
        dollar_recall = round(s24[["dollar_recall"]], 4),
        fill_gap_core = wf$gap_core, fill_gap_total = wf$gap_total)
    }
    rm(idx_tr, idx_te)
  }
  rm(trs, tes); invisible(gc())
}

res <- bind_rows(rows)
write.csv(res, file.path(OUT_DIR, "width_floor_results_2024.csv"),
          row.names = FALSE)

## ---- consistency guard: unfiltered arm must reproduce the benchmark ---------
chk <- res %>% filter(arm == "unfiltered") %>%
  inner_join(bench_ref %>% select(state, budget, precision_ref = precision,
                                  dollar_ref = dollar_recall),
             by = c("state", "budget"))
bad <- chk %>% filter(abs(precision - precision_ref) > 1e-6 |
                      abs(dollar_recall - dollar_ref) > 1e-6)
if (nrow(bad)) {
  print(bad[, c("state", "budget", "precision", "precision_ref")])
  stop("unfiltered arm does not reproduce the benchmark - investigate first")
}
stamp("consistency guard: unfiltered arm reproduces the benchmark in %d of %d cells",
      nrow(chk), nrow(chk))

## ---- paired readout with the mandatory companions ---------------------------
base <- res %>% filter(arm == "unfiltered") %>%
  select(state, budget, precision_base = precision,
         dollar_base = dollar_recall)
paired <- res %>% filter(arm != "unfiltered") %>%
  inner_join(base, by = c("state", "budget")) %>%
  mutate(d_precision = precision - precision_base,
         d_dollar_recall = dollar_recall - dollar_base)
write.csv(paired, file.path(OUT_DIR, "per_state_paired.csv"), row.names = FALSE)

sink(file.path(OUT_DIR, "summary.txt"), split = TRUE)
on.exit(sink(), add = TRUE)     # a failure below must not leave the sink open
for (arm in names(FLOORS)) for (b in BUDGETS) {
  p <- paired %>% filter(arm == !!arm, budget == b)
  stamp("%s @ %d%%: d_precision median %+.4f | mean %+.4f | harmed(<%.2f) %d | helped(>+0.05) %d of %d",
        arm, round(100 * b), median(p$d_precision), mean(p$d_precision),
        HARM_THR, sum(p$d_precision < HARM_THR),
        sum(p$d_precision > 0.05), nrow(p))
  stamp("%s @ %d%%: d_dollar   median %+.4f | mean %+.4f | harmed(<%.2f) %d | helped(>+0.05) %d | dropped rules med %.1f",
        arm, round(100 * b), median(p$d_dollar_recall), mean(p$d_dollar_recall),
        HARM_THR, sum(p$d_dollar_recall < HARM_THR),
        sum(p$d_dollar_recall > 0.05), median(p$n_dropped_by_floor))
  stamp("%s @ %d%%: fill gaps: %d of %d cells with gap_total > 0 (unfiltered: %d)",
        arm, round(100 * b),
        sum(res$fill_gap_total[res$arm == arm & res$budget == b] > 0),
        sum(res$arm == arm & res$budget == b),
        sum(res$fill_gap_total[res$arm == "unfiltered" & res$budget == b] > 0))
}
stamp("pre-registered bar (design_note.md, one-sided non-inferiority): ship a")
stamp("floor arm only if, at BOTH budgets and for BOTH metrics, median d >=")
stamp("-0.005, mean d >= -0.01, and zero states harmed (< %.2f). Positive", HARM_THR)
stamp("medians are one-era observations, never improvement claims. Preference:")
stamp("0.05, else 0.02, else unshipped.")
sink()
on.exit()
stamp("done; outputs in %s", OUT_DIR)
