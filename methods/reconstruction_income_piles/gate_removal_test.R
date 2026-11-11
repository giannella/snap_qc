# Pile gate removal test (2026-10-07, issue #29). One component varies: the
# pile gate (rules with >= 0.25 of their training flags or errors on
# reconstruction income-pile rows dropped from each pool before the fill).
# Everything else is the v2.7 benchmark recipe verbatim: the seed-117 pools
# the v2.7 benchmark mined on the final frame, artifact gate, 99% LCB order,
# fresh-share walk, 3x buffer, 5% / 10% budgets. Both list types (blended,
# national-only), gate off vs on, two windows:
#   SEL_WINDOW=w2224  pools mined on FY2022-23, scored on FY2024
#   SEL_WINDOW=w1719  pools mined on FY2017-19, scored on FY2022, FY2023,
#                     FY2024, each year walked at its own budget
# Each test year is scored twice: on all its cases, and on the cases that are
# not pile cases (pile rows removed from the caseload before the walk; the
# budget is a share of the remaining cases). Pile cases are error-rich in the
# test years too, so the all-cases reading is expected to favor the ungated
# lists; the non-pile reading is the one that stands in for a state's own
# case file, which has no piles. Pile flags: income_pile_rows() over the
# window's rows (piles are defined within each fiscal year).
# By construction: the ungated blended arm on all cases must reproduce the
# benchmark CSV cell for cell; recomputed training counts must equal the
# pools' n for every rule that gets a pile tag.
#   SEL_WINDOW=w1719 "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" methods/reconstruction_income_piles/gate_removal_test.R
setwd("C:/Users/ericg/snap_qc")
suppressMessages(library(dplyr))
source("rule_mining_helpers.R")

WINDOW <- Sys.getenv("SEL_WINDOW")
stopifnot(WINDOW %in% c("w2224", "w1719"))
W <- switch(WINDOW,
  w2224 = list(train = c("2022", "2023"), test = "2024",
               cache = "methods/v270_cpi_benchmark/fy2024_cpi/cache",
               bench = "methods/v270_cpi_benchmark/fy2024_cpi/v250_benchmark_2024.csv",
               bench_year = "2024", expect_train = c(76031L, 8397L)),
  w1719 = list(train = c("2017", "2018", "2019"), test = c("2022", "2023", "2024"),
               cache = "methods/v270_cpi_benchmark/fy2022_cpi/cache",
               bench = "methods/v270_cpi_benchmark/fy2022_cpi/v250_benchmark_2024.csv",
               bench_year = "2022", expect_train = c(116060L, 10920L)))
OUT_DIR <- "methods/reconstruction_income_piles"
SEED <- 117; BUDGETS <- c(0.05, 0.10); BUFFER_MULT <- 3; FRESH_MIN <- 0.50
MM_TAG_SHARE <- 0.25; MM_POOL_MAX <- 0.02; MM_TOP40_MAX <- 1L; MM_TOP10_MAX <- 0L
PILE_TAG_SHARE <- 0.25
VOCAB19 <- c("HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size",
             "expedited_i", "bbce_state_i", "rawben_rel_max", "medical_deductions",
             "shelter_expenses_by_hh_size", "utilities_sua", "married", "homeless",
             "percent_abawd", "unc_rawben_rel_max", "months_since_cert_n",
             "count_divisible_by_100", "gross_by_hh_size", "earned_by_hh_size",
             "unearned_by_hh_size")
HH_LEVELS <- c("1", "2-3", "4+")
hh_group_of <- function(n) {
  n <- suppressWarnings(as.numeric(as.character(n)))
  ifelse(is.na(n), NA_character_, ifelse(n <= 1, "1", ifelse(n <= 3, "2-3", "4+")))
}
stamp <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), sprintf(...)))

## ---- frame ------------------------------------------------------------------
reg_model_data <- readRDS("reg_model_data.rds")
stopifnot(nrow(reg_model_data) == 231619L)
adf0 <- reg_model_data %>% filter(fiscal_year %in% c(W$train, W$test))
adf <- prep_features(adf0, VOCAB19)$data
st <- as.character(adf$state); yr <- as.character(adf$fiscal_year)
ie_all <- !is.na(adf$over_threshold) & adf$over_threshold != 0
ed_all <- ifelse(ie_all, abs(ifelse(is.na(adf$total_error_amount), 0, adf$total_error_amount)), 0)
hh_all <- hh_group_of(adf$cert_HH_size_FS_n)
is_tr <- yr %in% W$train
stopifnot(sum(is_tr) == W$expect_train[1], sum(ie_all[is_tr]) == W$expect_train[2])
pile_all <- income_pile_rows(adf0)
for (y in c(W$train, W$test))
  stamp("FY%s: %d rows, %d errors | pile rows %d (errors %d)", y, sum(yr == y), sum(ie_all[yr == y]),
        sum(pile_all[yr == y]), sum(pile_all[yr == y] & ie_all[yr == y]))

## ---- gates, walk, scoring (benchmark verbatim) ------------------------------
mm_tag <- function(adm, head_gate = FALSE) {
  if (!nrow(adm)) { adm$artifact_i <- logical(0); return(adm) }
  sf <- round(adm$mm_n / adm$n, 4); se <- round(ifelse(adm$k > 0, adm$mm_k / adm$k, 0), 4)
  adm$artifact_i <- sf >= MM_TAG_SHARE | se >= MM_TAG_SHARE
  top40 <- sum(adm$artifact_i[seq_len(min(40L, nrow(adm)))])
  top10 <- sum(adm$artifact_i[seq_len(min(10L, nrow(adm)))])
  if (mean(sf >= MM_TAG_SHARE) > MM_POOL_MAX || (head_gate && (top40 > MM_TOP40_MAX || top10 > MM_TOP10_MAX)))
    stop("artifact gate would have halted the benchmark; the cache does not match its run")
  adm
}
pile_tag <- function(adm, data, strata, pile, ie) {
  if (!nrow(adm)) { adm$pile_i <- logical(0); return(adm) }
  sc <- reduce_flags_for_rules(adm, data, strata, function(ix)
    c(length(ix), sum(pile[ix]), sum(pile[ix] & ie[ix])))
  stopifnot(all(sc[, 1] == adm$n))
  adm$pile_i <- round(sc[, 2] / adm$n, 4) >= PILE_TAG_SHARE |
                round(ifelse(adm$k > 0, sc[, 3] / adm$k, 0), 4) >= PILE_TAG_SHARE
  adm
}
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
  list(frozen = frozen, buffer = buffer, gap_core = gap_core, gap_total = gap_total)
}
score_year <- function(sel, idx_te, n_te, b, err_te, doll_te) {
  cap <- floor(b * n_te); un <- rep(FALSE, n_te)
  for (i in sel) {
    add <- sum(!un[idx_te[[i]]])
    if (add > 0 && sum(un) + add <= cap) un[idx_te[[i]]] <- TRUE
  }
  c(n_flagged = sum(un), n_errors = sum(un & err_te),
    dollars_caught = sum(doll_te[un]), dollars_total = sum(doll_te))
}

## ---- national pool ----------------------------------------------------------
natl <- readRDS(file.path(W$cache, sprintf("bench_national_%d.rds", SEED)))
tr_rows <- which(is_tr)
strata_tr_nat <- lapply(setNames(nm = HH_LEVELS), function(h) which(hh_all[tr_rows] %in% h))
natl <- mm_tag(natl, head_gate = TRUE)
natl <- pile_tag(natl, adf[tr_rows, , drop = FALSE], strata_tr_nat, pile_all[tr_rows], ie_all[tr_rows])
natl$pool <- "national"
stamp("national pool: %d rules | artifact-tagged %d | pile-tagged %d (%.1f%%; %d not also artifact-tagged)",
      nrow(natl), sum(natl$artifact_i), sum(natl$pile_i), 100 * mean(natl$pile_i),
      sum(natl$pile_i & !natl$artifact_i))
cols <- c("hh", "rule", "n", "lcb", "artifact_i", "pile_i", "pool")
blend_of <- function(a, b) bind_rows(a, b) %>% arrange(desc(lcb), desc(n), hh, rule) %>%
  distinct(hh, rule, .keep_all = TRUE)

## ---- per state --------------------------------------------------------------
STATES <- sort(unique(st))
if (nzchar(Sys.getenv("SEL_STATES"))) STATES <- strsplit(Sys.getenv("SEL_STATES"), ",")[[1]]
rows <- list()
for (state in STATES) {
  periods <- c("train", W$test)
  rs <- lapply(setNames(nm = periods), function(p)
    which(st == state & (if (p == "train") is_tr else yr == p)))
  own <- readRDS(file.path(W$cache, sprintf("bench_state_%s_%d.rds", gsub(" ", "_", state), SEED)))
  own <- mm_tag(own)
  strata_tr_s <- lapply(setNames(nm = HH_LEVELS), function(h) which(hh_all[rs$train] %in% h))
  own <- pile_tag(own, adf[rs$train, , drop = FALSE], strata_tr_s, pile_all[rs$train], ie_all[rs$train])
  own_in <- if (nrow(own)) { own$pool <- "state"; own[, cols] } else NULL
  keep <- function(x, gate) if (is.null(x)) NULL else x[!x$artifact_i & (!gate | !x$pile_i), , drop = FALSE]
  pools <- list(
    blended_off  = blend_of(keep(natl[, cols], FALSE), keep(own_in, FALSE)),
    blended_on   = blend_of(keep(natl[, cols], TRUE),  keep(own_in, TRUE)),
    national_off = keep(natl[, cols], FALSE),
    national_on  = keep(natl[, cols], TRUE))
  # flags depend on (hh, rule) only: score the union of keys once
  U <- pools$blended_off
  key_u <- paste(U$hh, U$rule)
  stopifnot(all(unlist(lapply(pools, function(p) paste(p$hh, p$rule) %in% key_u))))
  all_rows <- unlist(rs, use.names = FALSE); off <- cumsum(c(0L, lengths(rs)))
  sdf <- adf[all_rows, , drop = FALSE]
  strata_s <- lapply(setNames(nm = HH_LEVELS), function(h) which(hh_all[all_rows] %in% h))
  idx_all <- flags_for_rules(U, sdf, strata_s)
  idx_of <- function(p) {
    j <- match(p, periods); lo <- off[j] + 1L; hi <- off[j + 1L]
    lapply(idx_all, function(ix) ix[ix >= lo & ix <= hi] - (lo - 1L))
  }
  idx_tr <- idx_of("train"); n_tr <- length(rs$train)
  te <- lapply(setNames(nm = W$test), function(y) {
    r <- rs[[y]]; ix <- idx_of(y); np <- !pile_all[r]; pos <- cumsum(np)
    list(all = list(idx = ix, err = ie_all[r], doll = ed_all[r]),
         nonpile = list(idx = lapply(ix, function(v) pos[v[np[v]]]), err = ie_all[r][np], doll = ed_all[r][np]))
  })
  for (arm in names(pools)) {
    m <- match(paste(pools[[arm]]$hh, pools[[arm]]$rule), key_u)
    for (b in BUDGETS) {
      wf <- walk_fill(idx_tr[m], n_tr, b)
      sel <- m[c(wf$frozen, wf$buffer)]
      n_state_core <- sum(pools[[arm]]$pool[wf$frozen] == "state")
      for (y in W$test) for (sc in c("all", "nonpile")) {
        TS <- te[[y]][[sc]]
        s <- score_year(sel, TS$idx, length(TS$err), b, TS$err, TS$doll)
        rows[[length(rows) + 1L]] <- data.frame(
          window = WINDOW, state = state, budget = b, test_year = y,
          list = sub("_.*", "", arm), gate = sub(".*_", "", arm), scoring = sc,
          n_te = length(TS$err), n_err_te = sum(TS$err),
          n_flagged = s[["n_flagged"]], n_errors = s[["n_errors"]],
          dollars_caught = round(s[["dollars_caught"]], 2), dollars_total = round(s[["dollars_total"]], 2),
          n_rules = length(sel), n_state_rules_core = n_state_core,
          fill_gap_core = wf$gap_core, fill_gap_total = wf$gap_total)
      }
    }
  }
  stamp("  %s done (union %d rules; pile-tagged national %d, state %d)", state, nrow(U),
        sum(natl$pile_i), if (nrow(own)) sum(own$pile_i) else 0L)
  rm(idx_all, idx_tr, te, sdf); invisible(gc())
}
res <- bind_rows(rows)
out_fn <- file.path(OUT_DIR, sprintf("gate_scores_%s%s.csv", WINDOW,
                                     if (nzchar(Sys.getenv("SEL_TAG"))) paste0("_", Sys.getenv("SEL_TAG"))
                                     else if (nzchar(Sys.getenv("SEL_STATES"))) "_smoke" else ""))
write.csv(res, out_fn, row.names = FALSE)
stamp("wrote %s (%d rows)", out_fn, nrow(res))

## ---- reproduction check against the benchmark CSV ---------------------------
bench <- read.csv(W$bench) %>% select(state, budget, n_flagged_b = n_flagged,
                                      n_errors_b = n_errors_caught, n_core_state_b = n_state_rules_core)
cmp <- res %>% filter(list == "blended", gate == "off", scoring == "all", test_year == W$bench_year) %>%
  inner_join(bench, by = c("state", "budget"))
stopifnot(nrow(cmp) == length(STATES) * length(BUDGETS))
bad <- cmp %>% filter(n_flagged != n_flagged_b | n_errors != n_errors_b | n_state_rules_core != n_core_state_b)
if (nrow(bad)) { print(bad); stop("ungated blended arm does not reproduce the benchmark") }
stamp("ungated blended arm reproduces the benchmark on FY%s in all %d cells", W$bench_year, nrow(cmp))
cat("GATE REMOVAL TEST DONE\n")
