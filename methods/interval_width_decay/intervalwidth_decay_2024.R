# Held-out decay by dollar-interval width (design_note.md, 2026-08-21).
# One fixed 2022-2023-era any-error pool evaluated as-is on the held-out
# FY2024 frame; rules bucketed post hoc by relative interval width on dollar
# variables. No mining, no tuning, no writes outside methods/.
suppressMessages(library(dplyr))

POOL <- "methods/v250_benchmark_2024/cache/bench_national_117.rds"
OUT_DIR <- "methods/interval_width_decay"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

DOLLAR_VARS <- c("medical_deductions", "utilities", "earned_by_hh_size",
                 "unearned_by_hh_size", "gross_by_hh_size",
                 "shelter_expenses_by_hh_size", "total_deductions_by_hh_size")
BUCKETS <- c(0, 0.02, 0.05, 0.15, 0.50, Inf)
BUCKET_LABELS <- c("<=2%", "2-5%", "5-15%", "15-50%", ">50%")
MIN_BUCKET_RULES <- 30      # merge-upward floor (design note: engineering
MIN_BUCKET_FLAGGED <- 100   # requirement, not a judged failure)

pool <- readRDS(POOL)
cat(sprintf("pool: %d rules (train era 2022-2023)\n", nrow(pool)))

# held-out year: FY2024, all states, pipeline error convention
d <- readRDS("reg_model_data.rds")
h <- d %>% filter(as.character(fiscal_year) == "2024")
hh_num <- suppressWarnings(as.numeric(as.character(h$cert_HH_size_FS_n)))
h$hh_group <- ifelse(hh_num <= 1, "1", ifelse(hh_num <= 3, "2-3", "4+"))
is_err <- !is.na(h$over_threshold) & h$over_threshold != 0
cat(sprintf("held-out FY2024: %d rows, %d errors (%.1f%%)\n",
            nrow(h), sum(is_err), 100 * mean(is_err)))

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

# pre-extract feature columns once; evaluate each rule's mask on FY2024.
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
          file.path(OUT_DIR, "per_rule_decay_2024.csv"), row.names = FALSE)

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
cat("\ndecay by interval-width bucket, ALL rules (held-out FY2024):\n")
print(as.data.frame(summary_tbl), digits = 3)
write.csv(summary_tbl, file.path(OUT_DIR, "decay_by_bucket_2024.csv"),
          row.names = FALSE)

band_tbl <- bucket_summary(res %>% filter(n >= 30, n <= 300))
cat("\nsame, TRAIN-N BAND n in [30, 300] (the primary read — the band the\n")
cat("narrow rules live in, shared across buckets):\n")
print(as.data.frame(band_tbl), digits = 3)
write.csv(band_tbl, file.path(OUT_DIR, "decay_by_bucket_trainband_2024.csv"),
          row.names = FALSE)
cat("\ndone; outputs in", OUT_DIR, "\n")
