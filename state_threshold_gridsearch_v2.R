# â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
# STATE threshold grid search + hold-out test (v2 stack)
#
# Takes a per-frame selection of the national rules (up to MAX_RULES_PER_FRAME
# per error frame by national train LCB; earned income admitted at a relaxed
# floor), and for each state independently:
#
#   1. grid-searches each rule's numeric thresholds on the STATE's training
#      years. A variant QUALIFIES if it flags >= MIN_STATE_FLAGGED state-train
#      cases at raw precision >= MIN_STATE_PRECISION; among qualifiers the
#      tuned variant is the one capturing the most error DOLLARS (precision
#      adequate, then maximize reach). No LCB at state level â€” the support
#      floor bounds the noise and the hold-out year is the honest judge;
#   2. tests on the state's hold-out year, reporting each rule under BOTH its
#      tuned and national thresholds;
#   3. compares three unions on the hold-out: tuned thresholds (qualified
#      rules), national thresholds (same qualified rules), and the FULL
#      national selection as-is (no local qualification) â€” the fallback a
#      small state would deploy when its sample can't support local tuning.
#
# Scored against ANY over-threshold error (state samples are too thin for
# typed evaluation; reviews find whatever error is present). Tuned-variant
# selection happens on state-train, so train stats are optimistic â€” only the
# hold-out columns count.
#
# Expects `reg_model_data`. Reads inclusion_rules_by_hh_size_v2/*_rules_all.csv.
# Outputs one CSV per state plus a cross-state summary in state_rules_v2/.
# â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

library(dplyr)
source("rule_mining_helpers.R")
set.seed(117)

## â”€â”€ 0. Config â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

STATES <- c("Washington", "Connecticut", "North Carolina", "Louisiana",
            "Michigan", "Virginia", "Arizona")
STATE_COL <- "state"

YEAR_COL    <- "fiscal_year"
TRAIN_YEARS <- c("2022", "2024")
TEST_YEARS  <- c("2023")

TARGET_IS_ERROR <- quote(!is.na(over_threshold) & over_threshold != 0)
ERR_AMT_COL     <- "total_error_amount"

HH_SIZE_COL <- "cert_HH_size_FS_n"
hh_group_of <- function(n) {
  n <- suppressWarnings(as.numeric(as.character(n)))
  ifelse(is.na(n), NA_character_, ifelse(n <= 1, "1", ifelse(n <= 3, "2-3", "4+")))
}

# Per-frame selection of national rules (see header).
FRAME_FLOORS <- c(earned_income = 0.15, unearned_income = 0.20,
                  underissuance = 0.20, other_error = 0.20)
MAX_RULES_PER_FRAME <- 60
NATIONAL_RULES_DIR  <- "inclusion_rules_by_hh_size_v2"

# Threshold grid: each numeric condition's threshold is scaled by these factors
# (1 = national value). Rules with many conditions get the coarse grid.
FACTORS_FINE   <- c(0.75, 0.90, 1.00, 1.10, 1.25)
FACTORS_COARSE <- c(0.90, 1.00, 1.10)
MAX_VARIANTS   <- 700

# State qualification. Two modes, both followed by the same tuned-variant
# selection (maximize error dollars captured on state-train among qualifiers):
#   "lcb"           â€” variant's Wilson LCB (STATE_LCB_Z) of state-train
#                     precision >= MIN_STATE_PRECISION, support >=
#                     MIN_STATE_FLAGGED_LCB (support scales with precision).
#                     DEFAULT: in the 2026-07 three-way comparison this hybrid
#                     (LCB gate + dollar-max selection) dominated the simple
#                     floor in Arizona and was the middle ground elsewhere.
#   "support_floor" â€” variant flags >= MIN_STATE_FLAGGED state-train cases at
#                     raw precision >= MIN_STATE_PRECISION (simpler to explain;
#                     more reach, less precision)
QUALIFY_MODE          <- "lcb"             # or "support_floor"
MIN_STATE_FLAGGED     <- 20
MIN_STATE_PRECISION   <- 0.20
STATE_LCB_Z           <- 1.2816            # 90% one-sided (lcb mode only)
MIN_STATE_FLAGGED_LCB <- 5                 # support backstop (lcb mode only)

out_dir <- "state_rules_v2"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
stopifnot(QUALIFY_MODE %in% c("support_floor", "lcb"))

## â”€â”€ 1. National rules + data prep â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

cat("national rule selection (per-frame floor + quota):\n")
national <- bind_rows(lapply(names(FRAME_FLOORS), function(fr) {
  r <- read.csv(file.path(NATIONAL_RULES_DIR, sprintf("%s_rules_all.csv", fr)),
                stringsAsFactors = FALSE)
  r <- r[!is.na(r$precision_train_lcb) &
           r$precision_train_lcb >= FRAME_FLOORS[[fr]], , drop = FALSE]
  r <- head(r[order(-r$precision_train_lcb), , drop = FALSE], MAX_RULES_PER_FRAME)
  cat(sprintf("  %-16s floor %.2f -> %d rules\n", fr, FRAME_FLOORS[[fr]], nrow(r)))
  data.frame(rule = r$rule, hh = r$hh, error_frame = fr,
             national_lcb = r$precision_train_lcb, stringsAsFactors = FALSE)
}))
cat(sprintf("national selection: %d rules total\n", nrow(national)))

features <- c(
  "HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size",
  "expedited_i", "bbce_state_i",  # bbce_state_i replaced cat_elig 2026-08-13 (FY2024 recode made 1-vs-2 splits era-markers) "rawben_rel_max", "medical_deductions",
  "shelter_expenses_by_hh_size", "utilities", "married", "homeless",
  "rawearn_by_hh_size", "rawunearn_by_hh_size", "rawgross_by_hh_size",
  "percent_abawd", "unc_rawben_rel_max",
  "months_since_cert_n", "count_divisible_by_100"
)
adf <- prep_features(reg_model_data %>%
                       filter(.data[[YEAR_COL]] %in% c(TRAIN_YEARS, TEST_YEARS)),
                     features)$data

missing_states <- setdiff(STATES, unique(as.character(adf[[STATE_COL]])))
if (length(missing_states) > 0)
  stop("States not found in data: ", paste(missing_states, collapse = ", "))

targets_of <- function(df) {
  ie <- eval(TARGET_IS_ERROR, envir = df); ie[is.na(ie)] <- FALSE
  amt <- df[[ERR_AMT_COL]]; amt[is.na(amt)] <- 0
  list(ie = ie, ed = ifelse(ie, abs(amt), 0))
}

## â”€â”€ 2. Rule threshold variants â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

# uniq: per-variable sorted unique observed values (from the STATE's train
# data). Scaled thresholds are kept only if they induce a DIFFERENT partition
# of the observed values: two cuts with no data point between them flag the
# same cases, so evaluating both is wasted work. Binary indicators (0/1)
# collapse to a single variant automatically; continuous variables shed
# duplicates wherever the state's data is sparse. Cuts the whole condition
# can never satisfy in this state (fires on nothing) are dropped outright.
rule_variants <- function(rule, uniq = NULL) {
  p <- .parse_rule(rule)
  if (is.null(p)) return(rule)
  fac <- if (nrow(p) <= 3) FACTORS_FINE else FACTORS_COARSE
  grids <- lapply(seq_len(nrow(p)), function(i) {
    v <- unique(signif(p$thr[i] * fac, 4))
    if (p$thr[i] == 0) v <- 0
    uv <- if (is.null(uniq)) NULL else uniq[[p$var[i]]]
    if (!is.null(uv) && length(uv) > 0) {
      # within a duplicate group keep the cut closest to the national value:
      # equivalents flag identically on state train but deploy differently
      v <- v[order(abs(v - p$thr[i]))]
      key <- if (p$op[i] %in% c(">", "<="))
        findInterval(v, uv)                    # cut position: count of uv <= t
      else
        findInterval(v, uv, left.open = TRUE)  # count of uv < t
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

## â”€â”€ 3. Per-state pipeline â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

run_state <- function(st) {
  sdf <- adf[as.character(adf[[STATE_COL]]) == st, , drop = FALSE]
  yr  <- as.character(sdf[[YEAR_COL]])
  tr  <- sdf[yr %in% TRAIN_YEARS, , drop = FALSE]
  te  <- sdf[yr %in% TEST_YEARS, , drop = FALSE]
  tg_tr <- targets_of(tr); tg_te <- targets_of(te)
  str_tr <- lapply(setNames(nm = unique(national$hh)), function(h)
    which(hh_group_of(tr[[HH_SIZE_COL]]) %in% h))
  str_te <- lapply(setNames(nm = unique(national$hh)), function(h)
    which(hh_group_of(te[[HH_SIZE_COL]]) %in% h))
  cat(sprintf("\n===== %s: train %d rows (%d errors, %.1f%%) | test %d rows (%d errors) =====\n",
              st, nrow(tr), sum(tg_tr$ie), 100 * mean(tg_tr$ie),
              nrow(te), sum(tg_te$ie)))

  # observed unique values per variable used by any rule: drives the
  # partition-aware variant dedupe (see rule_variants) and surfaces
  # effectively-categorical fields
  vars_used <- unique(unlist(lapply(national$rule,
                                    function(r) .parse_rule(r)$var)))
  uniq <- lapply(setNames(nm = vars_used), function(v)
    sort(unique(suppressWarnings(as.numeric(tr[[v]])))))
  lowcard <- lengths(uniq)[lengths(uniq) <= 5]
  if (length(lowcard) > 0)
    cat(sprintf("  low-cardinality variables (single cut each): %s\n",
                paste(sprintf("%s(%d)", names(lowcard), lowcard), collapse = ", ")))

  test_of <- function(vr, hh) {
    ix <- eval_rule_on(vr, te, str_te[[hh]])
    n <- length(ix); k <- sum(tg_te$ie[ix])
    c(n = n, k = k, prec = ifelse(n > 0, k / n, NA_real_))
  }

  res <- bind_rows(lapply(seq_len(nrow(national)), function(i) {
    hh <- national$hh[i]
    variants <- unique(c(national$rule[i], rule_variants(national$rule[i], uniq)))
    st_v <- lapply(variants, function(vr) {
      ix <- eval_rule_on(vr, tr, str_tr[[hh]])
      c(n = length(ix), k = sum(tg_tr$ie[ix]), d = sum(tg_tr$ed[ix]))
    })
    n <- vapply(st_v, `[[`, 0, "n"); k <- vapply(st_v, `[[`, 0, "k")
    d <- vapply(st_v, `[[`, 0, "d")
    if (sum(n) == 0) return(NULL)             # rule never fires in this state
    prec <- ifelse(n > 0, k / n, NA_real_)
    qual <- if (QUALIFY_MODE == "lcb") {
      which(n >= MIN_STATE_FLAGGED_LCB &
              wilson_lcb(k, n, STATE_LCB_Z) >= MIN_STATE_PRECISION)
    } else {
      which(n >= MIN_STATE_FLAGGED & !is.na(prec) & prec >= MIN_STATE_PRECISION)
    }
    qualified <- length(qual) > 0
    best <- if (qualified) qual[which.max(d[qual])]
            else which.max(ifelse(is.na(prec), -Inf, prec))
    t_tuned <- test_of(variants[best], hh); t_natl <- test_of(national$rule[i], hh)
    data.frame(
      state = st, error_frame = national$error_frame[i], hh_size = hh,
      rule_national = national$rule[i], rule_tuned = variants[best],
      tuned_is_national = best == 1, qualified = qualified,
      train_n = n[best], train_errors = k[best],
      train_precision = round(prec[best], 3), train_dollars = round(d[best]),
      test_n_tuned = t_tuned["n"], test_precision_tuned = round(t_tuned["prec"], 3),
      test_n_national = t_natl["n"], test_precision_national = round(t_natl["prec"], 3),
      stringsAsFactors = FALSE)
  }))
  if (is.null(res) || nrow(res) == 0) {
    cat("  no rules evaluable in this state\n"); return(NULL)
  }
  keep <- res[res$qualified, , drop = FALSE]
  crit <- if (QUALIFY_MODE == "lcb")
    sprintf("train-precision LCB(z=%.2f) >= %.2f, >= %d flagged",
            STATE_LCB_Z, MIN_STATE_PRECISION, MIN_STATE_FLAGGED_LCB)
  else
    sprintf(">= %d flagged at >= %.2f precision on train",
            MIN_STATE_FLAGGED, MIN_STATE_PRECISION)
  cat(sprintf("  rules qualified (%s): %d of %d evaluable\n",
              crit, nrow(keep), nrow(res)))
  write.csv(res, file.path(out_dir, sprintf("rules_gridsearch_%s.csv", gsub(" ", "_", st))),
            row.names = FALSE)

  union_of <- function(rules_vec, hh_vec, label) {
    un <- rep(FALSE, nrow(te))
    for (j in seq_along(rules_vec)) {
      un[eval_rule_on(rules_vec[j], te, str_te[[hh_vec[j]]])] <- TRUE
    }
    n <- sum(un); k <- sum(tg_te$ie[un])
    out <- c(flagged = n, errors = k,
             precision = ifelse(n > 0, round(k / n, 3), NA),
             recall = round(k / max(sum(tg_te$ie), 1), 3),
             dollar_recall = round(sum(tg_te$ed[un]) / max(sum(tg_te$ed), 1), 3),
             workload = round(n / nrow(te), 3))
    cat(sprintf("  TEST union (%-18s): flag %4.0f | precision %.3f | recall %.3f | $recall %.3f\n",
                label, out["flagged"], out["precision"], out["recall"], out["dollar_recall"]))
    out
  }
  u_all <- union_of(national$rule, national$hh, "national as-is")
  if (nrow(keep) > 0) {
    u_tun <- union_of(keep$rule_tuned, keep$hh_size, "tuned")
    u_nat <- union_of(keep$rule_national, keep$hh_size, "national, kept only")
  } else {
    u_tun <- u_nat <- setNames(rep(NA_real_, 6), names(u_all))
  }
  data.frame(state = st, n_rules_qualified = nrow(keep),
             t(u_tun) |> `colnames<-`(paste0(names(u_tun), "_tuned")),
             t(u_nat) |> `colnames<-`(paste0(names(u_nat), "_natl_kept")),
             t(u_all) |> `colnames<-`(paste0(names(u_all), "_natl_all")))
}

state_summaries <- bind_rows(lapply(STATES, run_state))
write.csv(state_summaries, file.path(out_dir, "state_union_summary.csv"), row.names = FALSE)
cat("\n===== cross-state summary =====\n")
print(as.data.frame(state_summaries), row.names = FALSE)
cat(sprintf("\nWrote state outputs to %s/\n", out_dir))

## â”€â”€ Notes â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
# - Qualification is raw (support + precision floors), not LCB-based: n >= 20
#   bounds the noise, the criterion is transparent to states, and the hold-out
#   year is the honest judge. Tuned variants maximize TRAIN dollar recall among
#   qualifiers, so tuned train stats are optimistic by construction.
# - "national as-is" = the full national selection at national thresholds with
#   NO local qualification: the deployment fallback for states whose samples
#   are too small to tune locally. Compare it to the tuned union per state.
# - Rules can qualify in one state and not another; each state CSV lists every
#   rule that fires in the state, with both thresholds, for expert review.
