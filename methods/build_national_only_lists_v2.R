# NATIONAL-ONLY production delivery lists, for the states where the
# one-year-ahead benchmark says the national pool alone matched or beat
# the blend. Decided 2026-08-14: states whose blended list trails their
# national-only list on the FY2024 test get a deployable national-only
# list alongside the blended default in state_delivery_lists/.
#
# Selection: methods/threearm_2024/threearm_results_2024.csv (the
# three-arm evaluation: pools mined FY2022-23, walked FY2024). A state x
# budget cell is selected when national precision >= blended precision
# (ties INCLUDED - where the blend deployed no state rules the two lists
# coincide and the file is an explicit copy). Selection is benchmark
# vintage; the LISTS built here are production vintage: the cached
# all-years (FY2022-24) national pool from the v2.5.0 build
# (methods/v250_candidate_lists/cache/national_pool_117.rds), walked
# against each selected state's own FY2022-24 caseload with the shipped
# fresh-share walk. Artifact-tagged rules dropped before the fill
# (findings 38); head gate on the national pool.
#
# Outputs -> methods/national_only_lists/ (staging):
#   national_delivery_<State>_2022_2024_budgetXX.csv  (shipped schema;
#   prefix "national_" parallel to "blended_", decided 2026-08-14)
#   selection_2024.csv   the full selection table with both arms' numbers
# The runner then characterizes the lists (section 29 adapter) and joins
# the curated columns before they are copied into state_delivery_lists/.
#
# Evaluation-only: never mines; stops if the production cache is absent.
# Expects `reg_model_data`. Run via runners/run_national_only_build.R.

suppressMessages(library(dplyr))
source("rule_mining_helpers.R")

SEED        <- 117
BUDGETS     <- c(0.05, 0.10)
BUFFER_MULT <- 3
FRESH_MIN   <- 0.50
MM_TAG_SHARE <- 0.25
MM_TOP40_MAX <- 1L; MM_TOP10_MAX <- 0L
VOCAB19 <- c(
  "HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size",
  "expedited_i", "bbce_state_i", "rawben_rel_max", "medical_deductions",
  "shelter_expenses_by_hh_size", "utilities", "married", "homeless",
  "percent_abawd", "unc_rawben_rel_max", "months_since_cert_n",
  "count_divisible_by_100",
  "gross_by_hh_size", "earned_by_hh_size", "unearned_by_hh_size")

CACHE_FN <- sprintf("methods/v250_candidate_lists/cache/national_pool_%d.rds", SEED)
THREEARM <- "methods/threearm_2024/threearm_results_2024.csv"
OUT_DIR  <- "methods/national_only_lists"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

HH_LEVELS <- c("1", "2-3", "4+")
hh_group_of <- function(n) {
  n <- suppressWarnings(as.numeric(as.character(n)))
  ifelse(is.na(n), NA_character_, ifelse(n <= 1, "1", ifelse(n <= 3, "2-3", "4+")))
}
stamp <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"),
                                   sprintf(...)))

## ---- selection from the three-arm benchmark ---------------------------------
stopifnot(file.exists(THREEARM))
ta <- read.csv(THREEARM)
sel_tbl <- ta %>%
  filter(arm %in% c("national", "blended")) %>%
  select(state, arm, budget, precision, dollar_recall) %>%
  tidyr::pivot_wider(names_from = arm,
                     values_from = c(precision, dollar_recall)) %>%
  mutate(selected = precision_national >= precision_blended)
write.csv(sel_tbl, file.path(OUT_DIR, "selection_2024.csv"), row.names = FALSE)
sel_cells <- sel_tbl %>% filter(selected)
stamp("selection: %d of %d state x budget cells (%d distinct states); ties included",
      nrow(sel_cells), nrow(sel_tbl), length(unique(sel_cells$state)))

## ---- frame (mirrors the v2.5.0 production build) ----------------------------
stopifnot(nrow(reg_model_data) == 231619L)
adf0 <- reg_model_data %>% filter(fiscal_year %in% c("2022", "2023", "2024"))
pf  <- prep_features(adf0, VOCAB19)
adf <- pf$data
stopifnot(length(setdiff(VOCAB19, pf$features)) == 0)
st <- as.character(adf$state)
ie_all <- !is.na(adf$over_threshold) & adf$over_threshold != 0
stopifnot(nrow(adf) == 115559L, sum(ie_all) == 13161L)
hh_all <- hh_group_of(adf$cert_HH_size_FS_n)

## ---- production national pool from cache (never mine) -----------------------
stopifnot(file.exists(CACHE_FN))
natl <- readRDS(CACHE_FN)
stopifnot("mm_n" %in% names(natl))
natl$mm_share_flags  <- round(natl$mm_n / natl$n, 4)
natl$mm_share_errors <- round(ifelse(natl$k > 0, natl$mm_k / natl$k, 0), 4)
natl$mm_inflation    <- round(natl$mm_k / natl$n, 4)
natl$artifact_i <- natl$mm_share_flags >= MM_TAG_SHARE |
  natl$mm_share_errors >= MM_TAG_SHARE
share_flag <- mean(natl$mm_share_flags >= MM_TAG_SHARE)
top40 <- sum(natl$artifact_i[seq_len(min(40L, nrow(natl)))])
top10 <- sum(natl$artifact_i[seq_len(min(10L, nrow(natl)))])
stamp("national pool: %d rules | tagged %d (flag-share %.2f%%) | head: top40 %d, top10 %d",
      nrow(natl), sum(natl$artifact_i), 100 * share_flag, top40, top10)
if (share_flag > 0.02 || top40 > MM_TOP40_MAX || top10 > MM_TOP10_MAX)
  stop("DISPLACEMENT HALT [national]: pool-share or head gate breached (statistician's gate)")
pool <- natl[!natl$artifact_i, , drop = FALSE]
pool$pool <- "national"

# the section-29 adapter needs the frame export beside the lists
ffp_src <- "methods/v250_candidate_lists/frame_for_profiles.csv"
stopifnot(file.exists(ffp_src))
file.copy(ffp_src, file.path(OUT_DIR, "frame_for_profiles.csv"), overwrite = TRUE)

## ---- walk each selected state (shipped walk, verbatim) ----------------------
build_summary <- list()
for (state in unique(sel_cells$state)) {
  s_rows <- which(st == state)
  trs <- adf[s_rows, , drop = FALSE]
  strata_s <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_all[s_rows] %in% h))
  idx_tr <- flags_for_rules(pool, trs, strata_s, label = "")
  nfl <- lengths(idx_tr)

  for (b in sel_cells$budget[sel_cells$state == state]) {
    cap <- floor(b * nrow(trs)); cap_buf <- floor(BUFFER_MULT * b * nrow(trs))
    un <- rep(FALSE, nrow(trs)); n_in <- 0L; n_core <- 0L
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
      un <- rep(FALSE, nrow(trs)); n_in <- 0L
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
    sel <- c(frozen, buffer)
    hand <- data.frame(
      rank = seq_along(sel),
      role = rep(c("core", "buffer"), c(length(frozen), length(buffer))),
      rule = pool$rule[sel], hh = pool$hh[sel], pool = pool$pool[sel],
      engines = pool$engines[sel], mined_frames = pool$mined_frames[sel],
      n_flagged_train = pool$n[sel],
      precision_train = round(pool$k[sel] / pool$n[sel], 4),
      precision_train_lcb = round(pool$lcb[sel], 4),
      dollars_per_flag_train = round(pool$doll[sel] / pool$n[sel], 2),
      mm_share_flags = pool$mm_share_flags[sel],
      mm_share_errors = pool$mm_share_errors[sel],
      mm_inflation = pool$mm_inflation[sel],
      n_flagged_state = nfl[sel])
    un2 <- rep(FALSE, nrow(trs)); nn <- integer(length(sel))
    for (j in seq_along(sel)) {
      ix <- idx_tr[[sel[j]]]
      nn[j] <- sum(!un2[ix]); un2[ix] <- TRUE
    }
    hand$n_new_at_rank <- nn
    fn <- file.path(OUT_DIR, sprintf("national_delivery_%s_2022_2024_budget%02.0f.csv",
                                     gsub(" ", "_", state), 100 * b))
    write.csv(hand, fn, row.names = FALSE)
    build_summary[[length(build_summary) + 1]] <- data.frame(
      state = state, budget = b, n_pool = nrow(pool),
      n_core = length(frozen), n_buffer = length(buffer),
      fill_cases = n_in, fill_gap_core = gap_core,
      fill_gap_total = gap_total, cap_buf = cap_buf)
  }
  stamp("  %s: national-only list(s) written (gaps core %s)", state,
        paste(vapply(build_summary[vapply(build_summary, function(x) x$state == state, TRUE)],
                     function(x) x$fill_gap_core, integer(1)), collapse = "/"))
  rm(trs, idx_tr); invisible(gc())
}
bs <- bind_rows(build_summary)
write.csv(bs, file.path(OUT_DIR, "build_summary_national_only.csv"),
          row.names = FALSE)
nz <- bs$fill_gap_total[bs$fill_gap_total > 0]
stamp("national-only build done -> %s | %d lists | fill gaps: %d of %d cells > 0 (max %d)",
      OUT_DIR, nrow(bs), length(nz), nrow(bs), if (length(nz)) max(nz) else 0L)
