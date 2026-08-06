# Marginal precision diagnostic for the delivered lists (descriptive, methods/ only).
#
# QUESTION (Eric, 2026-08-06): the delivery fill walk admits any rule that adds
# at least one new case, regardless of the quality of its marginal slice. How
# big is that effect in the shipped lists? Mechanism measured: residual adverse
# selection -- overlapping high-precision rules share the errors, so a later
# rule's "new" cases skew toward its false positives, and its own precision
# overstates its marginal precision.
#
# Two bases, each anchored to a committed artifact by a hard assertion:
#   TRAIN   -- the 98 shipped lists (state_delivery_lists/, built and walked on
#              all public years 2022-24). Assertion: the recomputed new-case
#              count at every rank equals the CSV's n_new_at_rank.
#   HOLDOUT -- the 98 bench lists (methods/anyerror_blended_holdout_2024/,
#              built on FY2022-23), walked against the FY2024 caseload with the
#              capacity-capped refill walk of methods/add_refill_metrics_v2.R.
#              Assertions: the frozen-core union reproduces
#              n_cases_flagged_holdout / n_errors_flagged_holdout, and the
#              refill walk reproduces n_rules_deployed_refill /
#              n_cases_flagged_refill / n_errors_flagged_refill /
#              precision_refill in holdout_metrics.json for every state and
#              budget (Michigan at 10%: 19 rules, 86 cases, 24 errors, 0.2791).
#   The bench lists are ALSO re-walked on their own FY2022-23 build caseload
#   (asserted against their n_new_at_rank) to obtain each bench rule's TRAIN
#   marginal precision for the honest reordering variant.
#
# Outputs (methods/marginal_precision_diagnostic/):
#   per_rule_marginal.csv              one row per delivered/deployed rule
#   adverse_selection_by_decile.csv    pooled decile curve (own vs marginal)
#   adverse_selection_by_state_decile.csv  the per-state medians (small cells)
#   scenario_shares.csv                Eric's scenario counted
#   headroom_oracle.csv                ORACLE re-walk + honest variant
# plus a one-screen printed summary. Read-only on state_delivery_lists/ and on
# the bench dir; no CHANGELOG, no version bump.
#
# ORACLE is an upper bound, not achievable: it reorders by REALIZED marginal
# precision on the evaluation outcomes themselves. The honest variant (holdout
# basis only) orders by TRAIN marginal precision and evaluates on 2024; it is
# exploratory, not validated.

suppressMessages({ library(dplyr); library(jsonlite) })
source("rule_mining_helpers.R")

t0 <- Sys.time()

DELIV_DIR    <- "state_delivery_lists"
BENCH_DIR    <- "methods/anyerror_blended_holdout_2024"
OUT_DIR      <- "methods/marginal_precision_diagnostic"
ALL_YEARS    <- c("2022", "2023", "2024")
BENCH_YEARS  <- c("2022", "2023")
HOLDOUT_YEAR <- "2024"
BUDGETS      <- c(0.05, 0.10)

dir.create(OUT_DIR, showWarnings = FALSE)

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
stamp <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"),
                                   sprintf(...)))

if (!exists("reg_model_data")) reg_model_data <- readRDS("reg_model_data.rds")
pf  <- prep_features(reg_model_data %>% filter(fiscal_year %in% ALL_YEARS), features)
adf <- pf$data
st  <- as.character(adf$state)
fy  <- as.character(adf$fiscal_year)
err_all <- !is.na(adf$over_threshold) & adf$over_threshold != 0

json_rec <- fromJSON(file.path(BENCH_DIR, "holdout_metrics.json"))$records
states   <- sort(unique(json_rec$state))
stamp("frame: %d rows (%s); %d states in the scorecard", nrow(adf),
      paste(ALL_YEARS, collapse = "-"), length(states))

## ── shared machinery ──────────────────────────────────────────────────────────

# Flag index for a delivered list against one caseload, stratum-restricted.
flags_for_list <- function(lst, data, strata_idx) {
  ci  <- build_condition_index(lst$rule)
  cnd <- eval_conditions(ci$conditions, data)
  idx <- vector("list", nrow(lst))
  for (h in unique(lst$hh)) {
    rows <- which(lst$hh == h)
    idx[rows] <- rule_flag_idx(ci$rule_conds[rows], cnd, base_idx = strata_idx[[h]])
  }
  idx
}

# Walk ALL rules in the given order (the builder's n_new_at_rank recompute):
# every rule joins the union; n_new / k_new are its marginal slice.
walk_all <- function(idx, err, n_rows) {
  un <- rep(FALSE, n_rows)
  n_new <- integer(length(idx)); k_new <- integer(length(idx))
  for (i in seq_along(idx)) {
    ix <- idx[[i]]; newix <- ix[!un[ix]]
    n_new[i] <- length(newix); k_new[i] <- sum(err[newix])
    un[ix] <- TRUE
  }
  list(n_new = n_new, k_new = k_new, hit = un)
}

# Capacity-capped refill walk (mirrors methods/add_refill_metrics_v2.R exactly:
# take a rule when it adds cases and the running total fits the cap).
refill_walk <- function(idx, err, n_rows, cap) {
  un <- rep(FALSE, n_rows); n_in <- 0L
  deployed <- logical(length(idx))
  n_new <- integer(length(idx)); k_new <- integer(length(idx))
  for (i in seq_along(idx)) {
    ix <- idx[[i]]; newix <- ix[!un[ix]]; add <- length(newix)
    if (add > 0L && n_in + add <= cap) {
      un[ix] <- TRUE; n_in <- n_in + add
      deployed[i] <- TRUE; n_new[i] <- add; k_new[i] <- sum(err[newix])
    }
  }
  list(deployed = deployed, n_new = n_new, k_new = k_new, hit = un)
}

# ORACLE greedy re-walk: at each step take the eligible rule (adds cases, fits
# the cap) with the highest REALIZED marginal precision on `err`; ties break to
# the larger slice, then the lower original index. Upper bound, not achievable.
greedy_oracle <- function(idx, err, n_rows, cap) {
  un <- rep(FALSE, n_rows); n_in <- 0L
  active <- rep(TRUE, length(idx))
  repeat {
    best <- 0L; best_mp <- -1; best_add <- 0L
    for (i in which(active)) {
      ix <- idx[[i]]; newix <- ix[!un[ix]]; add <- length(newix)
      if (add == 0L) { active[i] <- FALSE; next }
      if (n_in + add > cap) next
      mp <- sum(err[newix]) / add
      if (mp > best_mp || (mp == best_mp && add > best_add)) {
        best <- i; best_mp <- mp; best_add <- add
      }
    }
    if (best == 0L) break
    un[idx[[best]]] <- TRUE; n_in <- sum(un); active[best] <- FALSE
  }
  list(hit = un)
}

union_stats <- function(hit, err)
  list(n = sum(hit), k = sum(err & hit),
       prec = if (sum(hit) > 0) sum(err & hit) / sum(hit) else NA_real_)

near <- function(a, b, tol = 5e-5) is.finite(a) && is.finite(b) && abs(a - b) < tol

## ── main loop ────────────────────────────────────────────────────────────────

per_rule  <- list()
headroom  <- list()
base_rates <- list()
n_assert_lists <- 0L; n_assert_scorecard <- 0L

for (S in states) {
  tS <- Sys.time()
  rows_tr <- which(st == S)                       # 2022-24 (adf is already 2022-24)
  rows_bt <- which(st == S & fy %in% BENCH_YEARS) # 2022-23
  rows_ho <- which(st == S & fy == HOLDOUT_YEAR)  # 2024
  cl <- list(train = adf[rows_tr, , drop = FALSE],
             bt    = adf[rows_bt, , drop = FALSE],
             ho    = adf[rows_ho, , drop = FALSE])
  strata <- lapply(cl, function(d) lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(d$cert_HH_size_FS_n) %in% h)))
  errv <- lapply(cl, function(d) !is.na(d$over_threshold) & d$over_threshold != 0)
  base <- vapply(errv, mean, numeric(1))
  base_rates[[S]] <- data.frame(state = S, base_rate_train = base[["train"]],
                                base_rate_holdout = base[["ho"]])

  for (b in BUDGETS) {
    btag <- sprintf("budget%02.0f", 100 * b)
    key  <- sprintf("%s_%s", S, btag)

    ## ── A. TRAIN basis: the shipped list on the 2022-24 caseload ─────────────
    f_del <- file.path(DELIV_DIR, sprintf("blended_delivery_%s_2022_2024_%s.csv",
                                          gsub(" ", "_", S), btag))
    lst <- read.csv(f_del, stringsAsFactors = FALSE)
    lst <- lst[order(lst$rank), , drop = FALSE]
    idx_tr <- flags_for_list(lst, cl$train, strata$train)

    if (!identical(unname(lengths(idx_tr)), as.integer(lst$n_flagged_state)))
      stop(sprintf("ASSERTION FAILED [%s %s]: recomputed n_flagged_state differs from the shipped CSV at rank(s) %s. The rule evaluation was not reproduced; no downstream number can be trusted.",
                   S, btag, paste(head(lst$rank[lengths(idx_tr) != lst$n_flagged_state]), collapse = ", ")))
    wa <- walk_all(idx_tr, errv$train, nrow(cl$train))
    if (!identical(wa$n_new, as.integer(lst$n_new_at_rank)))
      stop(sprintf("ASSERTION FAILED [%s %s]: recomputed new-case counts differ from the shipped CSV's n_new_at_rank at rank(s) %s. The walk semantics were not reproduced; no downstream number can be trusted.",
                   S, btag, paste(head(lst$rank[wa$n_new != lst$n_new_at_rank]), collapse = ", ")))
    n_assert_lists <- n_assert_lists + 1L

    own_k <- vapply(idx_tr, function(ix) sum(errv$train[ix]), integer(1))
    per_rule[[paste0(key, "_train")]] <- data.frame(
      state = S, budget = 100 * b, basis = "train",
      rank = lst$rank, role = lst$role, pool = lst$pool, hh = lst$hh,
      rule = lst$rule,
      n_new = wa$n_new, k_new = wa$k_new,
      marginal_precision = ifelse(wa$n_new > 0, wa$k_new / wa$n_new, NA_real_),
      own_n_flagged = as.integer(lengths(idx_tr)), own_k_flagged = own_k,
      own_precision_state = ifelse(lengths(idx_tr) > 0, own_k / lengths(idx_tr), NA_real_),
      precision_train_lcb = lst$precision_train_lcb,
      base_rate = base[["train"]], stringsAsFactors = FALSE)

    # Shipped comparator: the SAME rule set (core + buffer) walked in rank order
    # to the SAME cap the oracle gets, so only the walk ORDER varies between the
    # two arms (senior-statistician review, 2026-08-06). The core-only union is
    # kept as a separate reference column, clearly named.
    cap_tr  <- floor(b * nrow(cl$train))
    ship_rf <- refill_walk(idx_tr, errv$train, nrow(cl$train), cap_tr)
    ship_tr <- union_stats(ship_rf$hit, errv$train)
    hit_core_tr <- rep(FALSE, nrow(cl$train))
    for (i in which(lst$role == "core")) hit_core_tr[idx_tr[[i]]] <- TRUE
    core_tr <- union_stats(hit_core_tr, errv$train)
    ora_tr  <- union_stats(greedy_oracle(idx_tr, errv$train, nrow(cl$train), cap_tr)$hit,
                           errv$train)
    headroom[[paste0(key, "_train")]] <- data.frame(
      state = S, budget = 100 * b, basis = "train", cap = cap_tr,
      shipped_n = ship_tr$n, shipped_k = ship_tr$k, shipped_prec = ship_tr$prec,
      core_only_n = core_tr$n, core_only_prec = core_tr$prec,
      oracle_n = ora_tr$n, oracle_k = ora_tr$k, oracle_prec = ora_tr$prec,
      oracle_delta = ora_tr$prec - ship_tr$prec,
      honest_n = NA_integer_, honest_k = NA_integer_, honest_prec = NA_real_,
      honest_delta = NA_real_, stringsAsFactors = FALSE)

    ## ── B. HOLDOUT basis: the bench list on the FY2024 caseload ──────────────
    f_bch <- file.path(BENCH_DIR, sprintf("bench_list_%s_%s.csv",
                                          gsub(" ", "_", S), btag))
    bl <- read.csv(f_bch, stringsAsFactors = FALSE)
    bl <- bl[order(bl$rank), , drop = FALSE]

    # (i) re-walk on the FY2022-23 build caseload: assert + train marginals
    idx_bt <- flags_for_list(bl, cl$bt, strata$bt)
    wb <- walk_all(idx_bt, errv$bt, nrow(cl$bt))
    if (!identical(wb$n_new, as.integer(bl$n_new_at_rank)))
      stop(sprintf("ASSERTION FAILED [%s %s bench]: recomputed new-case counts on the FY2022-23 caseload differ from the bench CSV's n_new_at_rank at rank(s) %s. The walk semantics were not reproduced; no downstream number can be trusted.",
                   S, btag, paste(head(bl$rank[wb$n_new != bl$n_new_at_rank]), collapse = ", ")))
    mp_train_bench <- ifelse(wb$n_new > 0, wb$k_new / wb$n_new, -1)

    # (ii) FY2024: frozen-core union + capacity-capped refill walk, both asserted
    idx_ho <- flags_for_list(bl, cl$ho, strata$ho)
    rec <- json_rec[json_rec$state == S & json_rec$budget_pct == 100 * b, , drop = FALSE]
    if (nrow(rec) != 1) stop(sprintf("no unique scorecard record for %s at %s", S, btag))

    hit_fc <- rep(FALSE, nrow(cl$ho))
    for (i in which(bl$role == "core")) hit_fc[idx_ho[[i]]] <- TRUE
    fc <- union_stats(hit_fc, errv$ho)
    if (fc$n != rec$n_cases_flagged_holdout || fc$k != rec$n_errors_flagged_holdout)
      stop(sprintf("ASSERTION FAILED [%s %s]: frozen-core union on FY2024 gives %d cases / %d errors; scorecard says %d / %d. Scoring semantics not reproduced.",
                   S, btag, fc$n, fc$k, rec$n_cases_flagged_holdout, rec$n_errors_flagged_holdout))

    cap_ho <- floor(b * nrow(cl$ho))
    rf <- refill_walk(idx_ho, errv$ho, nrow(cl$ho), cap_ho)
    rs <- union_stats(rf$hit, errv$ho)
    ok <- sum(rf$deployed) == rec$n_rules_deployed_refill &&
          rs$n == rec$n_cases_flagged_refill &&
          rs$k == rec$n_errors_flagged_refill &&
          near(rs$prec, rec$precision_refill)
    if (!ok)
      stop(sprintf("ASSERTION FAILED [%s %s]: refill walk gives %d rules / %d cases / %d errors / prec %.4f; scorecard says %d / %d / %d / %.4f. Walk semantics not reproduced.",
                   S, btag, sum(rf$deployed), rs$n, rs$k, rs$prec,
                   rec$n_rules_deployed_refill, rec$n_cases_flagged_refill,
                   rec$n_errors_flagged_refill, rec$precision_refill))
    n_assert_scorecard <- n_assert_scorecard + 1L

    dep <- which(rf$deployed)
    own_k_ho <- vapply(idx_ho, function(ix) sum(errv$ho[ix]), integer(1))
    per_rule[[paste0(key, "_holdout")]] <- data.frame(
      state = S, budget = 100 * b, basis = "holdout",
      rank = bl$rank[dep], role = bl$role[dep], pool = bl$pool[dep], hh = bl$hh[dep],
      rule = bl$rule[dep],
      n_new = rf$n_new[dep], k_new = rf$k_new[dep],
      marginal_precision = rf$k_new[dep] / rf$n_new[dep],
      own_n_flagged = as.integer(lengths(idx_ho)[dep]), own_k_flagged = own_k_ho[dep],
      own_precision_state = ifelse(lengths(idx_ho)[dep] > 0,
                                   own_k_ho[dep] / lengths(idx_ho)[dep], NA_real_),
      precision_train_lcb = bl$precision_train_lcb[dep],
      base_rate = base[["ho"]], stringsAsFactors = FALSE)

    # (iii) headroom on the holdout basis: ORACLE + honest (train-marginal order)
    ora_ho <- union_stats(greedy_oracle(idx_ho, errv$ho, nrow(cl$ho), cap_ho)$hit,
                          errv$ho)
    hon_ord <- order(-mp_train_bench, bl$rank)
    hon <- refill_walk(idx_ho[hon_ord], errv$ho, nrow(cl$ho), cap_ho)
    hs  <- union_stats(hon$hit, errv$ho)
    headroom[[paste0(key, "_holdout")]] <- data.frame(
      state = S, budget = 100 * b, basis = "holdout", cap = cap_ho,
      shipped_n = rs$n, shipped_k = rs$k, shipped_prec = rs$prec,
      core_only_n = fc$n, core_only_prec = fc$prec,
      oracle_n = ora_ho$n, oracle_k = ora_ho$k, oracle_prec = ora_ho$prec,
      oracle_delta = ora_ho$prec - rs$prec,
      honest_n = hs$n, honest_k = hs$k, honest_prec = hs$prec,
      honest_delta = hs$prec - rs$prec, stringsAsFactors = FALSE)
  }
  el <- as.numeric(difftime(Sys.time(), tS, units = "secs"))
  stamp("%-22s done in %5.1fs%s", S, el,
        if (el > 120) "  <-- exceeded the ~2 minute budget" else "")
}

# A truncated scorecard or file set must not pass silently.
if (length(states) != 49 || n_assert_lists != 98L || n_assert_scorecard != 98L)
  stop(sprintf("ASSERTION FAILED: expected 49 states and 98+98 verified lists; got %d states, %d shipped-list checks, %d scorecard checks.",
               length(states), n_assert_lists, n_assert_scorecard))

pr <- bind_rows(per_rule)
hr <- bind_rows(headroom)
br <- bind_rows(base_rates)
write.csv(pr, file.path(OUT_DIR, "per_rule_marginal.csv"), row.names = FALSE)
write.csv(hr, file.path(OUT_DIR, "headroom_oracle.csv"), row.names = FALSE)

## ── 2. adverse-selection curve by rank decile ────────────────────────────────
pr <- pr %>%
  group_by(state, budget, basis) %>%
  arrange(rank, .by_group = TRUE) %>%
  mutate(decile = pmin(10L, ceiling(10 * row_number() / n()))) %>%
  ungroup()

dec_pooled <- pr %>%
  group_by(basis, budget, decile) %>%
  summarise(n_rules = n(),
            median_own_precision = median(own_precision_state, na.rm = TRUE),
            median_marginal_precision = median(marginal_precision, na.rm = TRUE),
            median_gap = median(own_precision_state - marginal_precision, na.rm = TRUE),
            share_k_new_zero = mean(k_new == 0),
            .groups = "drop")
write.csv(dec_pooled, file.path(OUT_DIR, "adverse_selection_by_decile.csv"),
          row.names = FALSE)

dec_state <- pr %>%
  group_by(state, basis, budget, decile) %>%
  summarise(n_rules = n(),
            median_own_precision = median(own_precision_state, na.rm = TRUE),
            median_marginal_precision = median(marginal_precision, na.rm = TRUE),
            .groups = "drop")
write.csv(dec_state, file.path(OUT_DIR, "adverse_selection_by_state_decile.csv"),
          row.names = FALSE)

## ── 3. Eric's scenario counted ───────────────────────────────────────────────
scen_scope <- bind_rows(
  pr %>% filter(basis == "train") %>% mutate(scope = "train_all_listed"),
  pr %>% filter(basis == "train", role == "core") %>% mutate(scope = "train_core_only"),
  pr %>% filter(basis == "holdout") %>% mutate(scope = "holdout_deployed"))

scen <- scen_scope %>%
  group_by(basis, budget, scope) %>%
  summarise(n_rules = n(),
            share_rules_below_base = mean(n_new > 0 & marginal_precision < base_rate),
            share_capacity_below_base =
              sum(n_new[n_new > 0 & marginal_precision < base_rate]) / sum(n_new),
            share_rules_k_new_zero = mean(k_new == 0),
            share_capacity_k_new_zero = sum(n_new[k_new == 0]) / sum(n_new),
            # binomial context: a small clean slice is expected even from a good
            # rule; this is the share of zero-error slices you would see if every
            # rule's new cases erred at the rule's OWN precision
            expected_share_k0_at_own_prec =
              mean((1 - pmin(pmax(own_precision_state, 0), 1))^n_new, na.rm = TRUE),
            median_n_new = median(n_new),
            .groups = "drop")
scen_state <- scen_scope %>%
  group_by(state, basis, budget, scope) %>%
  summarise(sh_below = mean(n_new > 0 & marginal_precision < base_rate),
            sh_k0 = mean(k_new == 0), .groups = "drop") %>%
  group_by(basis, budget, scope) %>%
  summarise(median_state_share_rules_below_base = median(sh_below),
            median_state_share_rules_k_new_zero = median(sh_k0), .groups = "drop")
scen <- left_join(scen, scen_state, by = c("basis", "budget", "scope"))
write.csv(scen, file.path(OUT_DIR, "scenario_shares.csv"), row.names = FALSE)
write.csv(br, file.path(OUT_DIR, "state_base_rates.csv"), row.names = FALSE)

## ── 5. one-screen summary ────────────────────────────────────────────────────
cat("\n================ MARGINAL PRECISION DIAGNOSTIC: SUMMARY ================\n")
cat(sprintf("Assertions: %d/98 shipped-list walks reproduced n_new_at_rank exactly;\n",
            n_assert_lists))
cat(sprintf("            %d/98 bench walks reproduced n_new_at_rank (FY2022-23) AND the\n",
            n_assert_scorecard))
cat("            committed FY2024 scorecard (frozen-core cases/errors and refill\n")
cat("            rules/cases/errors/precision) exactly. All must be 98 to trust this.\n\n")

cat("-- Adverse-selection curve (pooled medians by rank decile) --\n")
cat("   own = the rule's precision on ALL cases it flags in the basis caseload;\n")
cat("   marginal = precision of its NEW cases only, in the walk.\n")
for (bs in c("train", "holdout")) for (bd in c(5, 10)) {
  d <- dec_pooled %>% filter(basis == bs, budget == bd)
  cat(sprintf("  %s basis, %d%% budget:\n", toupper(bs), bd))
  cat("    decile   :", sprintf("%6d", d$decile), "\n")
  cat("    own      :", sprintf("%6.3f", d$median_own_precision), "\n")
  cat("    marginal :", sprintf("%6.3f", d$median_marginal_precision), "\n")
  cat("    gap      :", sprintf("%6.3f", d$median_gap),
      " <- within-rule paired (own - marginal); nets out rank decay\n")
}

cat("\n-- Eric's scenario (delivered rules whose marginal slice is worse than the\n")
cat("   state's base error rate, and rules whose new cases contain no error) --\n")
print(as.data.frame(scen %>% mutate(across(where(is.numeric), ~ round(.x, 3)))),
      row.names = FALSE)

cat("\n-- Headroom (ORACLE = re-walk by realized marginal precision, an upper\n")
cat("   bound, NOT achievable; honest = order by train marginal precision,\n")
cat("   evaluated on 2024, exploratory, not validated) --\n")
hs <- hr %>% group_by(basis, budget) %>%
  summarise(median_shipped_prec = median(shipped_prec),
            median_oracle_prec = median(oracle_prec),
            median_oracle_delta = median(oracle_delta),
            median_honest_prec = median(honest_prec),
            median_honest_delta = median(honest_delta), .groups = "drop")
print(as.data.frame(hs %>% mutate(across(where(is.numeric), ~ round(.x, 4)))),
      row.names = FALSE)

cat(sprintf("\nWall time: %.1f minutes. Outputs in %s/\n",
            as.numeric(difftime(Sys.time(), t0, units = "mins")), OUT_DIR))
