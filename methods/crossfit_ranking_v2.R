# Cross-fitted ranking with discovery frequency (design B).
#
# The defect this addresses: today a rule's ranking statistic is computed on the
# same rows that selected it, so the bound at the top of the list is inflated by
# selection. Section 22 measured that directly (0.216 against 0.200 at the 5%
# budget, ranking on an untouched half versus the mined half). Sections 25 and 26
# then showed that nothing on the admission side can reach a delivered list, and
# that raising the support floor makes it worse, which leaves the ordering as the
# only lever.
#
# The scheme, per pool (national, and each state's own):
#
#   for rep in 1..REPS
#     split the pool's 2022-23 rows at random into halves A and B, stratified by
#       household-size stratum AND error status so both halves carry the same
#       base rate (a state has ~180 errors in ~1500 rows; an unstratified split
#       moves its base rate enough to make honest scores incomparable)
#     mine A  ->  candidate rules
#     score those candidates on B  ->  the HONEST statistics (n, errors, dollars)
#
# A rule discovered in several reps keeps the honest score from the LOWEST rep
# that found it, and nothing is pooled across reps. That is deliberate: rows held
# out in rep 1 sit inside the mining half of rep 3, so pooling complements would
# quietly reintroduce the contamination this design exists to remove. The price
# is that each rule's score uses half the pool's rows; discovery frequency is what
# the other reps buy instead.
#
#   discovery frequency = the number of reps (out of REPS) whose mining half
#   produced the rule. It is free once the mines exist, it is computed only from
#   mining halves, and it is the first admission-style test that can act at the
#   TOP of the ranking: a thin top-ranked rule that one split found and four did
#   not is exactly the case a blanket support floor cannot separate (section 26).
#
# Arms are cheap once the mines exist, so several admission rules are derived from
# one set of mines. Everything downstream is the settled recipe and is identical
# across arms and to the baseline: blend the state's own pool into the national
# pool on one scale, fill the state's 2022-23 caseload to the review budget as
# core plus buffer to 3x depth, then walk the frozen list against the state's 2024
# cases. The baseline arm is the shipped recipe, read from the cached full-data
# vocabulary, so it re-derives the figures recorded in sections 25 and 26.
#
# EXPLORATORY: scored on 2024, single era, and the walk re-fills against the test
# year (see section 25's caveat), so arms are comparable to each other and to the
# auditions, not to the frozen-list scorecard.
#
# Checkpointed per (pool, rep): a kill re-mines only what had not finished.
# Expects `reg_model_data`.

suppressMessages(library(dplyr))
source("rule_mining_helpers.R")

TRAIN_YEARS <- c("2022", "2023")
TEST_YEAR   <- "2024"
BUDGETS     <- c(0.05, 0.10)
BUFFER_MULT <- 3
LCB_Z       <- 2.326
FDR_ALPHA   <- 0.10
SIGNIF_DIGITS <- 3
if (!exists("REPS"))  REPS  <- 5
if (!exists("TOPK"))  TOPK  <- 20000   # see the scan-depth note at the eval loop
if (!exists("XGB")) XGB <- list(nrounds = 1000, max_depth = 4, eta = 0.02, subsample = 0.20)
if (!exists("RF"))  RF  <- list(num_trees = 1000, max_depth = 4, mtry = 2, min_node_size = 20)

# the 18 states whose full-data vocabularies are cached, so the baseline arm is
# exactly the one recorded in sections 25 and 26
if (!exists("TARGETS")) TARGETS <- c(
  "Massachusetts", "Michigan", "North Carolina", "Connecticut", "Arizona",
  "Washington", "Louisiana", "Virginia", "California", "Texas", "Mississippi",
  "New Jersey", "Colorado", "Maine", "Maryland", "Missouri",
  "District of Columbia", "Tennessee")

OUT_DIR <- if (exists("OUT_DIR")) OUT_DIR else
  "methods/state_similarity_v2/crossfit_ranking_train2223_test24"
CACHE   <- file.path(OUT_DIR, "mines")
RAWDIR  <- "methods/state_similarity_v2/transfer_benchmark_train2223_test24/fdr_raw_vocab"
dir.create(CACHE, showWarnings = FALSE, recursive = TRUE)

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

pf  <- prep_features(reg_model_data %>%
                       filter(fiscal_year %in% c(TRAIN_YEARS, TEST_YEAR)), features)
adf <- pf$data
pvars <- pf$features
st  <- as.character(adf$state)
yr  <- as.character(adf$fiscal_year)
in_tr <- yr %in% TRAIN_YEARS
ie_all <- !is.na(adf$over_threshold) & adf$over_threshold != 0
ed_all <- ifelse(ie_all, abs(ifelse(is.na(adf$total_error_amount), 0,
                                    adf$total_error_amount)), 0)
hh_all <- hh_group_of(adf$cert_HH_size_FS_n)
stamp("frame: %d rows | train %s | test %s | reps %d",
      nrow(adf), paste(TRAIN_YEARS, collapse = "+"), TEST_YEAR, REPS)

strata_of <- function(df) lapply(setNames(nm = HH_LEVELS), function(h)
  which(hh_group_of(df$cert_HH_size_FS_n) %in% h))

# Half-split stratified by stratum x error status: within each cell, a random
# half goes to the mining side. Keeps both halves at the same base rate.
split_half <- function(rows, seed) {
  set.seed(seed)
  cell <- paste(hh_all[rows], ie_all[rows])
  mine <- logical(length(rows))
  for (g in unique(cell)) {
    ix <- which(cell == g)
    take <- sample(ix, floor(length(ix) / 2))
    mine[take] <- TRUE
  }
  list(mine = rows[mine], score = rows[!mine])
}

## one (pool, rep): mine on half, score honestly on the other half
mine_rep <- function(pool_rows, pool_key, rep) {
  ck <- file.path(CACHE, sprintf("%s_rep%d.rds", pool_key, rep))
  if (file.exists(ck)) return(readRDS(ck))
  sp <- split_half(pool_rows, seed = 1000 * rep + nchar(pool_key) + rep)
  A <- adf[sp$mine, , drop = FALSE]; B <- adf[sp$score, , drop = FALSE]
  ie_A <- ie_all[sp$mine]
  strata_A <- strata_of(A); strata_B <- strata_of(B)
  rdf <- mine_rule_vocabulary(
    A, list(any_error = list(rows = seq_len(nrow(A)), ie = ie_A)),
    strata_A, pvars, xgb = XGB, rf = RF,
    signif_digits = SIGNIF_DIGITS, seed = 117 + rep, verbose = FALSE)
  # an empty rep still has to carry the base rates and the full column set, or a
  # pool whose first rep mines nothing would break the union below
  empty <- data.frame(hh = character(), rule = character(), engines = character(),
                      n_hon = integer(), k_hon = numeric(), d_hon = numeric(),
                      n_self = integer(), k_self = numeric(),
                      rep = integer(), stringsAsFactors = FALSE)
  attr(empty, "base_B") <- vapply(strata_B, function(rr) mean(ie_all[sp$score][rr]), numeric(1))
  attr(empty, "base_A") <- vapply(strata_A, function(rr) mean(ie_A[rr]), numeric(1))
  if (is.null(rdf) || nrow(rdf) == 0) { saveRDS(empty, ck); return(empty) }
  ie_B <- ie_all[sp$score]; ed_B <- ed_all[sp$score]
  stats <- reduce_flags_for_rules(
    rdf, B, strata_B,
    fun = function(ix) c(length(ix), sum(ie_B[ix]), sum(ed_B[ix])),
    label = sprintf("%s rep%d honest", pool_key, rep))
  # the SAME rules scored on the half they were mined from. This is the control
  # that separates the two things the design changes at once: mining on half the
  # data (which costs vocabulary) and scoring out of fold (the effect under
  # test). Without it, a loss against the full-data baseline cannot be attributed.
  ie_A <- ie_all[sp$mine]; ed_A <- ed_all[sp$mine]
  self <- reduce_flags_for_rules(
    rdf, A, strata_A,
    fun = function(ix) c(length(ix), sum(ie_A[ix]), sum(ed_A[ix])),
    label = sprintf("%s rep%d self", pool_key, rep))
  out <- data.frame(hh = rdf$hh, rule = rdf$rule, engines = rdf$engines,
                    n_hon = as.integer(stats[, 1]), k_hon = stats[, 2],
                    d_hon = stats[, 3],
                    n_self = as.integer(self[, 1]), k_self = self[, 2],
                    rep = rep, stringsAsFactors = FALSE)
  attr(out, "base_A") <- vapply(strata_A, function(rr) mean(ie_A[rr]), numeric(1))
  # the honest base rate per stratum, carried so admission uses the scoring half
  attr(out, "base_B") <- vapply(strata_B, function(rr) mean(ie_B[rr]), numeric(1))
  attr(out, "n_A") <- nrow(A); attr(out, "n_B") <- nrow(B)
  saveRDS(out, ck)
  stamp("  %-22s rep%d: mined %d on %d rows, scored on %d", pool_key, rep,
        nrow(rdf), nrow(A), nrow(B))
  out
}

# union across reps: first discovering rep supplies the honest score, all reps
# supply the frequency
build_pool <- function(pool_rows, pool_key) {
  reps <- lapply(seq_len(REPS), function(r) mine_rep(pool_rows, pool_key, r))
  # take the base rates from the first rep that carries them; a cache written by
  # an earlier version of this script may not have them at all, in which case
  # recompute from the pool's own rows rather than failing an overnight run
  base_B <- Find(Negate(is.null), lapply(reps, attr, "base_B"))
  base_A <- Find(Negate(is.null), lapply(reps, attr, "base_A"))
  if (is.null(base_B) || is.null(base_A)) {
    br <- vapply(setNames(nm = HH_LEVELS), function(h)
      mean(ie_all[pool_rows][hh_all[pool_rows] %in% h]), numeric(1))
    if (is.null(base_B)) base_B <- br
    if (is.null(base_A)) base_A <- br
    stamp("  %s: base rates recomputed from pool rows", pool_key)
  }
  all_r <- bind_rows(reps)
  if (!nrow(all_r)) return(NULL)
  all_r$key <- paste(all_r$hh, all_r$rule, sep = "\r")
  freq <- table(unique(all_r[, c("key", "rep")])$key)
  # Frequency at rule-TEXT level is nearly always 1: thresholds are canonicalised
  # to three significant digits, so two random halves move every cutpoint enough
  # to make the text differ (measured on a 2-rep smoke: 11,638 rules seen once,
  # 125 twice). The question worth asking is whether a rule of the same SHAPE
  # keeps being found, so frequency is also computed over the signature that
  # dedup_dominated groups by: the sorted set of (variable, direction) pairs.
  all_r$sig <- paste(all_r$hh,
                     vapply(all_r$rule, function(r) .rule_struct(r)$sig, ""))
  freq_sig <- table(unique(all_r[, c("sig", "rep")])$sig)
  first <- all_r %>% arrange(key, rep) %>% distinct(key, .keep_all = TRUE)
  first$freq <- as.integer(freq[first$key])
  first$freq_sig <- as.integer(freq_sig[first$sig])
  first$base <- base_B[first$hh]
  first$base_self <- base_A[first$hh]
  first$prec_self <- ifelse(first$n_self > 0, first$k_self / first$n_self, NA_real_)
  first$lcb_self  <- wilson_lcb(first$k_self, first$n_self, LCB_Z)
  first$prec_hon <- ifelse(first$n_hon > 0, first$k_hon / first$n_hon, NA_real_)
  first$lcb_hon  <- wilson_lcb(first$k_hon, first$n_hon, LCB_Z)
  first$dpf_hon  <- ifelse(first$n_hon > 0, first$d_hon / first$n_hon, NA_real_)
  first
}

# BH within a candidate set, one-sided against the stratum base rate
bh_admit <- function(k, n, p0, alpha = FDR_ALPHA) {
  pv <- pbinom(k - 1, n, p0, lower.tail = FALSE)
  m <- length(pv); o <- order(pv)
  thr <- max(c(0L, which(pv[o] <= alpha * seq_len(m) / m)))
  a <- rep(FALSE, m); if (thr > 0) a[o[seq_len(thr)]] <- TRUE
  a
}

## the arms, each a logical over a pool plus the statistic to rank by
ARMS <- list(
  # the control: mined on half, scored on THAT SAME half. Differs from the
  # baseline only in having half the mining data, so baseline vs this is the
  # cost of halving, and this vs honest_f15 is the effect of scoring out of fold.
  selfscored_half = list(keep = function(p) bh_admit(p$k_self, p$n_self, p$base_self) & p$n_self >= 15,
                         stat = function(p) p$lcb_self),
  honest_f30   = list(keep = function(p) bh_admit(p$k_hon, p$n_hon, p$base) & p$n_hon >= 30,
                      stat = function(p) p$lcb_hon),
  honest_f15   = list(keep = function(p) bh_admit(p$k_hon, p$n_hon, p$base) & p$n_hon >= 15,
                      stat = function(p) p$lcb_hon),
  honest_sig3  = list(keep = function(p) bh_admit(p$k_hon, p$n_hon, p$base) & p$n_hon >= 15 & p$freq_sig >= 3,
                      stat = function(p) p$lcb_hon),
  honest_sig5  = list(keep = function(p) bh_admit(p$k_hon, p$n_hon, p$base) & p$n_hon >= 15 & p$freq_sig >= REPS,
                      stat = function(p) p$lcb_hon),
  # frequency as the primary sort, honest bound as the tie-break within a
  # frequency band: tests whether "how often was this shape rediscovered" orders
  # the top of the list better than "how precise did it look"
  sig_then_lcb = list(keep = function(p) bh_admit(p$k_hon, p$n_hon, p$base) & p$n_hon >= 15,
                      stat = function(p) p$freq_sig + pmin(p$lcb_hon, 0.999))
)

## the frozen walk, identical to the admission auditions
#
# `slack` is the pruning certificate. The walk consumes a fixed capacity
# (BUFFER_MULT x budget x caseload) in rank order, so once that capacity is full
# no lower-ranked rule can enter, whatever the pool holds below. When the caller
# passes only the top K rules and the walk returns slack == 0, the result is
# provably identical to walking the whole admitted pool: the first K steps are
# the same in both, and after them nothing else fits. slack > 0 means the window
# was too small to be sure, and the caller re-runs that arm unpruned.
walk_eval <- function(stat, idx_tr, idx_te, n_tr, n_te, ie_te, ed_te, b) {
  cap <- floor(b * n_tr); cap_buf <- floor(BUFFER_MULT * b * n_tr)
  un <- rep(FALSE, n_tr); n_in <- 0L; frozen <- integer(0); buffer <- integer(0)
  for (i in order(-stat)) {
    if (is.na(stat[i])) next
    add <- sum(!un[idx_tr[[i]]])
    if (add == 0) next
    if (n_in + add <= cap) { un[idx_tr[[i]]] <- TRUE; n_in <- n_in + add; frozen <- c(frozen, i) }
    else if (n_in + add <= cap_buf) { un[idx_tr[[i]]] <- TRUE; n_in <- n_in + add; buffer <- c(buffer, i) }
  }
  slack <- cap_buf - n_in
  cap24 <- floor(b * n_te); un24 <- rep(FALSE, n_te); used <- 0L
  for (i in c(frozen, buffer)) {
    add <- sum(!un24[idx_te[[i]]])
    if (add > 0 && sum(un24) + add <= cap24) { un24[idx_te[[i]]] <- TRUE; used <- used + 1L }
  }
  nb <- sum(un24)
  data.frame(n_deployed = used, workload = round(nb / n_te, 4),
             precision = round(ifelse(nb > 0, sum(ie_te[un24]) / nb, NA), 4),
             dollar_recall = round(sum(ed_te[un24]) / sum(ed_te), 4),
             n_core = length(frozen), n_buffer = length(buffer), slack = slack)
}

## baseline: the shipped recipe from the cached full-data vocabulary
load_baseline <- function(key) {
  f <- file.path(RAWDIR, sprintf("raw_%s.rds", key))
  if (!file.exists(f)) return(NULL)
  r <- readRDS(f)
  r$base <- attr(r, "base_rates")[r$hh]
  r$keep <- bh_admit(r$k, r$n, r$base) & r$n >= 30
  r$stat <- wilson_lcb(r$k, r$n, LCB_Z)
  r
}

stamp("=== national pool ===")
natl_rows <- which(in_tr)
natl <- build_pool(natl_rows, "national")
stamp("national: %d unique rules over %d reps", nrow(natl), REPS)
# both frequency tables, because the stability arms turn on the second one:
# exact rule text almost never repeats across splits (thresholds are canonicalised
# to three significant digits and move with the split), whereas the signature,
# the sorted set of variable and direction pairs, does.
stamp("  discovery by rule text : %s",
      paste(sprintf("%dx:%d", seq_len(REPS), tabulate(natl$freq, REPS)), collapse = " "))
stamp("  discovery by signature : %s",
      paste(sprintf("%dx:%d", seq_len(REPS), tabulate(natl$freq_sig, REPS)), collapse = " "))
natl_base <- load_baseline("national")

res <- list()
for (target in TARGETS) {
  key <- gsub("[^A-Za-z]", "", target)
  stamp("########## %s ##########", target)
  own_rows <- which(st == target & in_tr)
  own <- build_pool(own_rows, key)
  own_base <- load_baseline(key)

  tr <- adf[own_rows, , drop = FALSE]
  te_rows <- which(st == target & yr == TEST_YEAR)
  te <- adf[te_rows, , drop = FALSE]
  strata_tr <- strata_of(tr); strata_te <- strata_of(te)
  ie_te <- ie_all[te_rows]; ed_te <- ed_all[te_rows]

  # every arm plus the baseline draws from one union, so flags are built once
  pool <- bind_rows(
    natl %>% mutate(pool = "national"),
    if (!is.null(own)) own %>% mutate(pool = "state") else NULL)
  pool <- pool %>% arrange(desc(lcb_hon), hh, rule) %>% distinct(hh, rule, .keep_all = TRUE)
  bl <- bind_rows(natl_base %>% mutate(pool = "national"),
                  if (!is.null(own_base)) own_base %>% mutate(pool = "state") else NULL)
  bl <- bl %>% filter(keep) %>% arrange(desc(stat), hh, rule) %>%
    distinct(hh, rule, .keep_all = TRUE)

  # Each arm can only ever reach the top TOPK rules by its own statistic: the
  # walk consumes capacity in rank order and stops when the 3x buffer cap is
  # full. Measured over the 49 delivered lists, the deepest any state reached
  # was rank 9,072 (Arkansas at the 10% budget), so TOPK carries better than 2x
  # margin; walk_eval's slack certificate then proves exactness case by case.
  # The windows are unioned so flags are built ONCE per state rather than once
  # per arm, which is what made the first run take 75 minutes a state.
  arm_defs <- c(lapply(names(ARMS), function(nm) {
      k <- which(ARMS[[nm]]$keep(pool))
      if (!length(k)) return(NULL)
      s <- ARMS[[nm]]$stat(pool)[k]
      list(nm = nm, src = "pool", rows = k[order(-s)][seq_len(min(TOPK, length(k)))],
           n_admitted = length(k))
    }),
    list(if (nrow(bl)) list(nm = "baseline", src = "bl",
                            rows = order(-bl$stat)[seq_len(min(TOPK, nrow(bl)))],
                            n_admitted = nrow(bl)) else NULL))
  arm_defs <- Filter(Negate(is.null), arm_defs)

  u_pool <- sort(unique(unlist(lapply(Filter(function(a) a$src == "pool", arm_defs), `[[`, "rows"))))
  u_bl   <- sort(unique(unlist(lapply(Filter(function(a) a$src == "bl",   arm_defs), `[[`, "rows"))))
  idx <- list(pool = list(), bl = list())
  if (length(u_pool)) {
    sub <- pool[u_pool, , drop = FALSE]
    idx$pool$tr <- flags_for_rules(sub, tr, strata_tr, label = "")
    idx$pool$te <- flags_for_rules(sub, te, strata_te, label = "")
    idx$pool$map <- setNames(seq_along(u_pool), u_pool)
  }
  if (length(u_bl)) {
    sub <- bl[u_bl, , drop = FALSE]
    idx$bl$tr <- flags_for_rules(sub, tr, strata_tr, label = "")
    idx$bl$te <- flags_for_rules(sub, te, strata_te, label = "")
    idx$bl$map <- setNames(seq_along(u_bl), u_bl)
  }
  stamp("  flags built once over %d pool rules + %d baseline rules for %d arms",
        length(u_pool), length(u_bl), length(arm_defs))

  for (a in arm_defs) {
    src <- idx[[a$src]]
    pos <- unname(src$map[as.character(a$rows)])
    p    <- if (a$src == "pool") pool[a$rows, , drop = FALSE] else bl[a$rows, , drop = FALSE]
    stat <- if (a$src == "pool") ARMS[[a$nm]]$stat(pool)[a$rows] else bl$stat[a$rows]
    for (b in BUDGETS) {
      ev <- walk_eval(stat, src$tr[pos], src$te[pos], nrow(tr), nrow(te), ie_te, ed_te, b)
      if (ev$slack > 0)
        stamp("  WARNING %s %s budget %.2f: slack %d, top-%d window may have truncated the fill",
              target, a$nm, b, ev$slack, TOPK)
      res[[length(res) + 1]] <- cbind(
        data.frame(target = target, arm = a$nm, budget = b, n_admitted = a$n_admitted,
                   n_scanned = length(a$rows),
                   n_state_rules = sum(p$pool == "state"),
                   target_base_rate = round(mean(ie_te), 4)), ev)
    }
  }
  rm(idx); invisible(gc())
  saveRDS(bind_rows(res), file.path(OUT_DIR, "crossfit_partial.rds"))
  stamp("  %s done (%d rows so far)", target, length(res))
}

out <- bind_rows(res)
write.csv(out, file.path(OUT_DIR, "crossfit_ranking.csv"), row.names = FALSE)
cat("\nmedians (precision / dollar recall / admitted):\n")
print(as.data.frame(out %>% group_by(arm, budget) %>%
  summarise(med_prec = median(precision, na.rm = TRUE),
            med_dollars = median(dollar_recall, na.rm = TRUE),
            med_admitted = median(n_admitted),
            med_deployed = median(n_deployed), .groups = "drop") %>%
  arrange(budget, desc(med_prec))), row.names = FALSE)
stamp("=== wrote %s (%d rows) ===", file.path(OUT_DIR, "crossfit_ranking.csv"), nrow(out))
