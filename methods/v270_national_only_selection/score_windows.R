# National-only vs blended, per state and budget, re-scored from the cached
# v2.7 benchmark pools (2026-10-06). No mining: both arms are walked from the
# same seed-117 pools that methods/v270_cpi_benchmark/ mined on the final
# frame, with the benchmark's admission, artifact gates, fresh-share walk and
# 5% / 10% budgets copied verbatim from methods/v250_benchmark_2024_utilrel_v2.R.
# The question is whether a state's own data adds to the national pool, so
# the arms differ in one component only: the blended arm walks the national
# pool merged with the state's pool; the national-only arm walks the national
# pool alone.
#   SEL_WINDOW=w2224  pools mined on FY2022-23 (fy2024_cpi cache), scored on FY2024
#   SEL_WINDOW=w1719  pools mined on FY2017-19 (fy2022_cpi cache), scored on
#                     FY2022, FY2023 and FY2024, each year walked separately at
#                     its own budget (a state reviews one year's caseload at a time)
# Built by construction to reproduce the benchmark: the blended arm on FY2024
# (w2224) and on FY2022 (w1719) must equal the benchmark CSVs cell for cell,
# and recomputed national train counts must equal the cached ones.
#   SEL_WINDOW=w1719 "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" methods/v270_national_only_selection/score_windows.R > sel_w1719.log 2>&1
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
OUT_DIR <- "methods/v270_national_only_selection"
SEED <- 117; BUDGETS <- c(0.05, 0.10); BUFFER_MULT <- 3; FRESH_MIN <- 0.50
MM_TAG_SHARE <- 0.25; MM_POOL_MAX <- 0.02; MM_TOP40_MAX <- 1L; MM_TOP10_MAX <- 0L

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

## ---- frame (the final v2.7 frame the caches were mined on) ------------------
reg_model_data <- readRDS("reg_model_data.rds")
stopifnot(nrow(reg_model_data) == 231619L)
adf0 <- reg_model_data %>% filter(fiscal_year %in% c(W$train, W$test))
pf <- prep_features(adf0, VOCAB19); adf <- pf$data
stopifnot(length(setdiff(VOCAB19, pf$features)) == 0)
st <- as.character(adf$state); yr <- as.character(adf$fiscal_year)
ie_all <- !is.na(adf$over_threshold) & adf$over_threshold != 0
ed_all <- ifelse(ie_all, abs(ifelse(is.na(adf$total_error_amount), 0, adf$total_error_amount)), 0)
hh_all <- hh_group_of(adf$cert_HH_size_FS_n)
is_tr <- yr %in% W$train
stopifnot(sum(is_tr) == W$expect_train[1], sum(ie_all[is_tr]) == W$expect_train[2])
for (y in W$test) stamp("test FY%s: %d rows, %d errors", y, sum(yr == y), sum(ie_all[yr == y]))

## ---- gates, walk and scoring: verbatim from the benchmark -------------------
tag_and_gate <- function(adm, head_gate = FALSE) {
  if (!nrow(adm)) { adm$artifact_i <- logical(0); return(adm) }
  adm$mm_share_flags  <- round(adm$mm_n / adm$n, 4)
  adm$mm_share_errors <- round(ifelse(adm$k > 0, adm$mm_k / adm$k, 0), 4)
  adm$mm_inflation    <- round(adm$mm_k / adm$n, 4)
  flag_tag <- adm$mm_share_flags >= MM_TAG_SHARE
  adm$artifact_i <- flag_tag | adm$mm_share_errors >= MM_TAG_SHARE
  top40 <- sum(adm$artifact_i[seq_len(min(40L, nrow(adm)))])
  top10 <- sum(adm$artifact_i[seq_len(min(10L, nrow(adm)))])
  if (mean(flag_tag) > MM_POOL_MAX || (head_gate && (top40 > MM_TOP40_MAX || top10 > MM_TOP10_MAX)))
    stop("artifact gate would have halted the benchmark; the cache does not match its run")
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
stopifnot(all(c("hh", "rule", "n", "k", "lcb", "mm_n", "mm_k") %in% names(natl)))
# by construction: the cache was mined on this frame (train counts reproduce)
tr_rows <- which(is_tr)
set.seed(1); chk <- natl[sort(sample(nrow(natl), min(400L, nrow(natl)))), ]
strata_tr_nat <- lapply(setNames(nm = HH_LEVELS), function(h) which(hh_all[tr_rows] %in% h))
fl_chk <- flags_for_rules(chk, adf[tr_rows, , drop = FALSE], strata_tr_nat)
stopifnot(all(vapply(fl_chk, length, integer(1)) == chk$n),
          all(vapply(fl_chk, function(ix) sum(ie_all[tr_rows][ix]), numeric(1)) == chk$k))
stamp("national pool: %d rules; 400 sampled train counts reproduce on this frame", nrow(natl))
natl <- tag_and_gate(natl, head_gate = TRUE); natl$pool <- "national"
cols <- c("hh", "rule", "n", "lcb", "artifact_i", "pool")
blend_of <- function(a, b) bind_rows(a, b) %>% arrange(desc(lcb), desc(n), hh, rule) %>%
  distinct(hh, rule, .keep_all = TRUE)
natl_vis <- natl[!natl$artifact_i, cols]

## ---- per state --------------------------------------------------------------
STATES <- sort(unique(st))
if (nzchar(Sys.getenv("SEL_STATES"))) STATES <- strsplit(Sys.getenv("SEL_STATES"), ",")[[1]]
rows <- list()
for (state in STATES) {
  own <- readRDS(file.path(W$cache, sprintf("bench_state_%s_%d.rds", gsub(" ", "_", state), SEED)))
  own <- tag_and_gate(own)
  own_in <- if (nrow(own)) { own$pool <- "state"; own[!own$artifact_i, cols] } else NULL
  pool <- blend_of(natl_vis, own_in)
  # national-only arm = the national pool alone, in its own order; flags are
  # a function of (hh, rule) only, so its rules index into the blend's flags
  nat_ix <- match(paste(natl_vis$hh, natl_vis$rule), paste(pool$hh, pool$rule))
  stopifnot(!anyNA(nat_ix))

  periods <- c("train", W$test)
  rs <- lapply(setNames(nm = periods), function(p)
    which(st == state & (if (p == "train") is_tr else yr == p)))
  all_rows <- unlist(rs, use.names = FALSE)
  off <- cumsum(c(0L, lengths(rs)))
  sdf <- adf[all_rows, , drop = FALSE]
  strata_s <- lapply(setNames(nm = HH_LEVELS), function(h) which(hh_all[all_rows] %in% h))
  idx_all <- flags_for_rules(pool, sdf, strata_s)
  idx_of <- function(p) {
    j <- match(p, periods); lo <- off[j] + 1L; hi <- off[j + 1L]
    lapply(idx_all, function(ix) ix[ix >= lo & ix <= hi] - (lo - 1L))
  }
  idx_tr <- idx_of("train"); n_tr <- length(rs$train)
  idx_te <- lapply(setNames(nm = W$test), idx_of)

  for (b in BUDGETS) {
    arms <- list(
      blended  = { wf <- walk_fill(idx_tr, n_tr, b)
                   list(sel = c(wf$frozen, wf$buffer), wf = wf,
                        n_state_core = sum(pool$pool[wf$frozen] == "state")) },
      national = { wf <- walk_fill(idx_tr[nat_ix], n_tr, b)
                   list(sel = nat_ix[c(wf$frozen, wf$buffer)], wf = wf, n_state_core = 0L) })
    for (a in names(arms)) for (y in W$test) {
      te <- rs[[y]]
      s <- score_year(arms[[a]]$sel, idx_te[[y]], length(te), b, ie_all[te], ed_all[te])
      rows[[length(rows) + 1L]] <- data.frame(
        window = WINDOW, state = state, budget = b, test_year = y, arm = a,
        n_te = length(te), n_err_te = sum(ie_all[te]),
        n_flagged = s[["n_flagged"]], n_errors = s[["n_errors"]],
        precision = round(s[["n_errors"]] / max(s[["n_flagged"]], 1), 4),
        dollars_caught = round(s[["dollars_caught"]], 2), dollars_total = round(s[["dollars_total"]], 2),
        dollar_recall = round(s[["dollars_caught"]] / max(s[["dollars_total"]], 1), 4),
        n_rules = length(arms[[a]]$sel), n_state_rules_core = arms[[a]]$n_state_core,
        fill_gap_core = arms[[a]]$wf$gap_core, fill_gap_total = arms[[a]]$wf$gap_total)
    }
  }
  stamp("  %s done (pool %d rules, %d state)", state, nrow(pool), sum(pool$pool == "state"))
  rm(idx_all, idx_tr, idx_te, sdf); invisible(gc())
}
res <- bind_rows(rows)
# SEL_TAG names a state chunk (parallel runs); SEL_STATES without a tag is a smoke
out_fn <- file.path(OUT_DIR, sprintf("scores_%s%s.csv", WINDOW,
                                     if (nzchar(Sys.getenv("SEL_TAG"))) paste0("_", Sys.getenv("SEL_TAG"))
                                     else if (nzchar(Sys.getenv("SEL_STATES"))) "_smoke" else ""))
write.csv(res, out_fn, row.names = FALSE)
stamp("wrote %s (%d rows)", out_fn, nrow(res))

## ---- reproduction check against the benchmark CSV ---------------------------
bench <- read.csv(W$bench) %>% select(state, budget, n_flagged_b = n_flagged,
                                      n_errors_b = n_errors_caught, n_core_state_b = n_state_rules_core)
cmp <- res %>% filter(arm == "blended", test_year == W$bench_year) %>%
  inner_join(bench, by = c("state", "budget"))
stopifnot(nrow(cmp) == length(STATES) * length(BUDGETS))
bad <- cmp %>% filter(n_flagged != n_flagged_b | n_errors != n_errors_b | n_state_rules_core != n_core_state_b)
if (nrow(bad)) { print(bad); stop("blended arm does not reproduce the benchmark") }
stamp("blended arm reproduces the benchmark on FY%s in all %d cells", W$bench_year, nrow(cmp))
cat("SCORE_WINDOWS DONE\n")
