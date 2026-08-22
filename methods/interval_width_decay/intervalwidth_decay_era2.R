# Held-out decay by dollar-interval width, ERA 2 (design_note.md, 2026-08-21;
# replication on the second cached era mandated by the pre-run review).
# One fixed 2017-2018-era any-error national pool evaluated as-is on the
# held-out FY2019 frame; rules bucketed post hoc by relative interval width
# on dollar variables. No mining, no tuning, no writes outside methods/.
#
# Era-2 pool: the cached national FY2017-18 mine
# (methods/state_similarity_v2/era_validation_train1718_test19/raw_vocab/
# raw_national.rds, 145,313 rules with cached train n/k). The cache carries
# no lcb column, so the admitted pool is derived from the cached n/k with the
# SHIPPED recipe — pooled BH at FDR 10% against the cached FY2017-18 stratum
# base rates AND n >= 30, lcb = 99% Wilson LCB — the identical construction
# that (a) built era 1's bench_national_117.rds (admit_rank in
# methods/v250_benchmark_2024_v2.R) and (b) the pre-registered era-2
# fresh-share replication applied to this same artifact
# (methods/era2_freshshare_replication_plan_2026-08-06.md, Arm B).
#
# Frame: the cache predates the 2026-08-08 benmerge frame change, so both the
# cached n/k and the FY2019 evaluation use the archived frame it was mined on
# (archive_data/reg_model_data_pre_benmerge_2026-08-08.rds; build
# 79,907/7,115 and test 39,221/3,931 match the cache attributes and the
# era-2 plan exactly; full 145,313-rule n/k determinism PASS recorded in
# methods/freshshare_rewalk_era2/summary.txt, 2026-08-07). Evaluating the
# cached pool on the current rebuilt frame would mix frame versions inside
# d_raw, the review's primary readout; era 1 is frame-consistent, so era 2
# stays frame-consistent too. Note: column names n24/k24/share_n24_lt10 are
# kept verbatim for mechanical comparability with era 1; in this era they
# hold held-out FY2019 counts.
suppressMessages(library(dplyr))

POOL <- "methods/state_similarity_v2/era_validation_train1718_test19/raw_vocab/raw_national.rds"
FRAME <- "archive_data/reg_model_data_pre_benmerge_2026-08-08.rds"
HELD_OUT_YEAR <- "2019"
BUILD_YEARS <- c("2017", "2018")
FDR_ALPHA <- 0.10
MIN_N <- 30
LCB_Z <- 2.326
OUT_DIR <- "methods/interval_width_decay"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

DOLLAR_VARS <- c("medical_deductions", "utilities", "earned_by_hh_size",
                 "unearned_by_hh_size", "gross_by_hh_size",
                 "shelter_expenses_by_hh_size", "total_deductions_by_hh_size")
BUCKETS <- c(0, 0.02, 0.05, 0.15, 0.50, Inf)
BUCKET_LABELS <- c("<=2%", "2-5%", "5-15%", "15-50%", ">50%")
MIN_BUCKET_RULES <- 30      # merge-upward floor (design note: engineering
MIN_BUCKET_FLAGGED <- 100   # requirement, not a judged failure)

# rule_mining_helpers.R::wilson_lcb, inlined verbatim to avoid loading the
# full pipeline for one six-line function
wilson_lcb <- function(k, n, z = 1.645) {
  p <- ifelse(n > 0, k / n, NA_real_); z2 <- z * z
  ifelse(n > 0,
         (p + z2 / (2 * n) - z * sqrt(p * (1 - p) / n + z2 / (4 * n * n))) / (1 + z2 / n),
         NA_real_)
}

d <- readRDS(FRAME)
yr <- as.character(d$fiscal_year)

# admitted pool from the cached raw vocabulary (shipped recipe; see header)
raw <- readRDS(POOL)
base_by_hh <- attr(raw, "base_rates")
stopifnot(!is.null(base_by_hh), identical(names(base_by_hh), c("1", "2-3", "4+")))
# cache-consistency assertions against the archived frame
b <- d[yr %in% BUILD_YEARS, , drop = FALSE]
stopifnot(nrow(b) == attr(raw, "n_train_rows"))
bhh_num <- suppressWarnings(as.numeric(as.character(b$cert_HH_size_FS_n)))
bhh <- ifelse(bhh_num <= 1, "1", ifelse(bhh_num <= 3, "2-3", "4+"))
b_err <- !is.na(b$over_threshold) & b$over_threshold != 0
base_frame <- vapply(c("1", "2-3", "4+"), function(g) mean(b_err[bhh == g]),
                     numeric(1))
stopifnot(isTRUE(all.equal(unname(base_by_hh), unname(base_frame),
                           tolerance = 1e-12)))
pvals <- pbinom(raw$k - 1, raw$n, base_by_hh[raw$hh], lower.tail = FALSE)
m <- length(pvals); o <- order(pvals)
thr <- max(c(0L, which(pvals[o] <= FDR_ALPHA * seq_len(m) / m)))
bh <- rep(FALSE, m); if (thr > 0) bh[o[seq_len(thr)]] <- TRUE
pool <- raw[bh & raw$n >= MIN_N, , drop = FALSE]
pool$lcb <- wilson_lcb(pool$k, pool$n, LCB_Z)
pool <- pool[order(-pool$lcb, -pool$n, pool$hh, pool$rule, method = "radix"),
             , drop = FALSE]
cat(sprintf("pool: %d rules admitted of %d cached (train era 2017-2018)\n",
            nrow(pool), nrow(raw)))

# held-out year: FY2019, all states, pipeline error convention
h <- d %>% filter(as.character(fiscal_year) == HELD_OUT_YEAR)
hh_num <- suppressWarnings(as.numeric(as.character(h$cert_HH_size_FS_n)))
h$hh_group <- ifelse(hh_num <= 1, "1", ifelse(hh_num <= 3, "2-3", "4+"))
is_err <- !is.na(h$over_threshold) & h$over_threshold != 0
cat(sprintf("held-out FY%s: %d rows, %d errors (%.1f%%)\n",
            HELD_OUT_YEAR, nrow(h), sum(is_err), 100 * mean(is_err)))

COND_PAT <- "([A-Za-z_][A-Za-z0-9_]*)\\s*(>=|<=|>|<)\\s*(-?[0-9.]+)"
parse_conds <- function(txt) {
  parts <- regmatches(txt, gregexpr(COND_PAT, txt, perl = TRUE))[[1]]
  if (!length(parts)) return(NULL)
  do.call(rbind, lapply(parts, function(p) {
    mm <- regmatches(p, regexec(COND_PAT, p))[[1]]
    data.frame(var = mm[2], op = mm[3], thr = as.numeric(mm[4]))
  }))
}

# narrowest relative dollar-interval width per rule (NA = reference class);
# binding bounds are max(lo) and min(hi) (review 2026-08-21: correct by
# construction, though the canonicalizer already collapses same-direction
# bounds so no pool rule carries duplicates)
rel_width <- function(cc) {
  if (is.null(cc)) return(NA_real_)
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

# pre-extract feature columns once; evaluate each rule's mask on FY2019.
# Factor/character columns must NOT go through bare as.numeric (review
# 2026-08-21: `homeless` is a factor with levels FALSE/TRUE, whose level
# codes 1/2 would corrupt every homeless rule) — map them to 0/1 first.
vars_used <- sort(unique(unlist(lapply(pool$rule, function(r)
  unique(parse_conds(r)$var)))))
stopifnot(all(vars_used %in% names(h)))
cat("feature column classes on the held-out frame:\n")
print(vapply(vars_used, function(v) class(h[[v]])[1], character(1)))
X <- lapply(setNames(vars_used, vars_used), function(v) {
  x <- h[[v]]
  if (is.factor(x) || is.character(x))
    x <- as.character(x) %in% c("TRUE", "1")
  suppressWarnings(as.numeric(x))
})
hh_vec <- h$hh_group

n24 <- k24 <- integer(nrow(pool))
rw <- numeric(nrow(pool))
t0 <- Sys.time()
for (i in seq_len(nrow(pool))) {
  cc <- parse_conds(pool$rule[i])
  stopifnot(!is.null(cc))
  rw[i] <- rel_width(cc)
  m <- hh_vec == as.character(pool$hh[i])
  for (j in seq_len(nrow(cc))) {
    x <- X[[cc$var[j]]]
    cm <- switch(cc$op[j],
                 ">=" = x >= cc$thr[j], ">" = x > cc$thr[j],
                 "<=" = x <= cc$thr[j], "<" = x < cc$thr[j])
    cm[is.na(cm)] <- FALSE
    m <- m & cm
  }
  n24[i] <- sum(m); k24[i] <- sum(m & is_err)
  if (i %% 10000 == 0)
    cat(sprintf("  %d/%d rules evaluated (%.0fs)\n", i, nrow(pool),
                as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}

res <- pool %>%
  mutate(rel_width = rw, n24 = n24, k24 = k24,
         prec_train = k / n,
         bucket = ifelse(is.na(rel_width), "one-sided/non-dollar ref",
                         as.character(cut(rel_width, BUCKETS, BUCKET_LABELS,
                                          include.lowest = TRUE))))
write.csv(res %>% select(hh, rule, n, k, lcb, prec_train, rel_width, bucket,
                         n24, k24),
          file.path(OUT_DIR, "per_rule_decay_era2.csv"), row.names = FALSE)

# support printout FIRST (design note), then the summary
supp <- res %>% group_by(bucket) %>%
  summarise(rules = n(), flagged24 = sum(n24), errors24 = sum(k24),
            .groups = "drop")
cat("\nsupport by bucket (rules / held-out flagged / held-out errors):\n")
print(as.data.frame(supp))
thin <- supp %>% filter(rules < MIN_BUCKET_RULES | flagged24 < MIN_BUCKET_FLAGGED)
if (nrow(thin))
  cat("NOTE: buckets under the support floor (merge upward when reading):",
      paste(thin$bucket, collapse = ", "), "\n")

# per-rule decays on the scored subset (review 2026-08-21: never mix
# unweighted means with pooled ratios, and read the width claim primarily
# from d_raw within a shared train-n band — decay-vs-LCB differs across
# buckets mechanically under the null because narrow rules have small n).
# share_n24_lt10 reports reach-collapse, which no precision column can see.
res <- res %>%
  mutate(prec24 = ifelse(n24 >= 10, k24 / n24, NA),
         d_raw = prec24 - prec_train,
         d_lcb = prec24 - lcb)

bucket_summary <- function(dd) {
  dd %>%
    group_by(bucket) %>%
    summarise(
      rules = n(),
      med_train_n = median(n),
      med_lcb = median(lcb),
      mean_prec_train = mean(prec_train),
      share_n24_lt10 = mean(n24 < 10),
      n_scored = sum(!is.na(prec24)),
      mean_d_raw = mean(d_raw, na.rm = TRUE),
      med_d_raw = median(d_raw, na.rm = TRUE),
      mean_d_lcb = mean(d_lcb, na.rm = TRUE),
      med_d_lcb = median(d_lcb, na.rm = TRUE),
      .groups = "drop") %>%
    arrange(match(bucket, c(BUCKET_LABELS, "one-sided/non-dollar ref")))
}

summary_tbl <- bucket_summary(res)
cat("\ndecay by interval-width bucket, ALL rules (held-out FY2019):\n")
print(as.data.frame(summary_tbl), digits = 3)
write.csv(summary_tbl, file.path(OUT_DIR, "decay_by_bucket_era2.csv"),
          row.names = FALSE)

band_tbl <- bucket_summary(res %>% filter(n >= 30, n <= 300))
cat("\nsame, TRAIN-N BAND n in [30, 300] (the primary read — the band the\n")
cat("narrow rules live in, shared across buckets):\n")
print(as.data.frame(band_tbl), digits = 3)
write.csv(band_tbl, file.path(OUT_DIR, "decay_by_bucket_trainband_era2.csv"),
          row.names = FALSE)
cat("\ndone; outputs in", OUT_DIR, "\n")
