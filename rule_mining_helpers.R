# ──────────────────────────────────────────────────────────────────────────────
# rule_mining_helpers.R — shared machinery for the post-{pre} rule-mining stack
#
# Every driver script (INCL_*_v2, EXCL_*_v2, tuning, comparisons) runs the same
# five-stage pipeline through these helpers:
#
#   generate  ->  canonicalize  ->  dedup  ->  evaluate  ->  sweep / shortlist
#
# Design decisions (see design_drop_pre_architecture.md for rationale):
#   - Engines: {ranger} (forests, incl. mtry = 1 diversity variant) and
#     {xgboost} (boosted trees). Every tree node's path becomes a candidate rule.
#   - Thresholds are rounded to a few significant digits so rules are
#     state-presentable; all stats are measured AFTER rounding, never stale.
#   - Dedup: exact text -> exact coverage -> same-structure dominance (drop a
#     rule when a looser same-shape rule has at least its train statistic).
#   - The selection statistic is the one-sided Wilson LOWER BOUND of train
#     precision, which removes the winner's curse (validated 2026-07-04).
#   - Rule flags are stored as integer index vectors and built from a table of
#     unique conditions (each condition evaluated once per dataset), so memory
#     and parse costs stay flat even with 100k+ raw rules.
#
# All features must be numeric or logical (logicals are coerced to 0/1).
# Multi-level factors are rejected with a clear error — recode upstream.
# ──────────────────────────────────────────────────────────────────────────────

## ── 0. Small utilities ────────────────────────────────────────────────────────

# One-sided Wilson lower bound for a binomial proportion (vectorized).
wilson_lcb <- function(k, n, z = 1.645) {
  p <- ifelse(n > 0, k / n, NA_real_); z2 <- z * z
  ifelse(n > 0,
         (p + z2 / (2 * n) - z * sqrt(p * (1 - p) / n + z2 / (4 * n * n))) / (1 + z2 / n),
         NA_real_)
}

# Keep features that exist and vary; coerce logicals and TWO-level
# factors/characters to 0/1 (1 = the level sorting last, printed so rule text
# is interpretable). Multi-level factors are refused — recode upstream.
prep_features <- function(df, features) {
  pv <- features[features %in% names(df)]
  for (v in pv) {
    x <- df[[v]]
    if (is.logical(x)) df[[v]] <- as.integer(x)
    else if (is.factor(x) || is.character(x)) {
      u <- sort(unique(as.character(x[!is.na(x)])))
      if (length(u) == 2) {
        df[[v]] <- as.integer(as.character(x) == u[2])
        cat(sprintf("  note: %s coerced to 0/1 (1 = \"%s\")\n", v, u[2]))
      } else {
        stop("Feature '", v, "' has ", length(u),
             " levels; the v2 stack needs numeric/logical/2-level features. Recode upstream.")
      }
    }
  }
  pv <- pv[vapply(df[pv], function(x)
    !all(is.na(x)) && length(unique(x[!is.na(x)])) > 1, logical(1))]
  list(data = df, features = pv)
}

## ── 1. Generate: tree ensembles -> rule strings ──────────────────────────────
# A rule is the conjunction of split conditions on the path from the root to a
# node ("x <= 3.5 & y > 100"). Every non-root node contributes one rule, as in
# {pre}. Rows with NA in any feature are dropped before fitting (the flag
# evaluator later treats NA conditions as FALSE, matching current behaviour).

# Walk one ranger tree table (ranger::treeInfo) into rule strings.
.rules_from_ranger_tree <- function(ti) {
  n <- nrow(ti)
  cond <- character(n); parent <- integer(n)   # row index = nodeID + 1
  for (i in seq_len(n)) {
    if (isTRUE(ti$terminal[i])) next
    l <- ti$leftChild[i] + 1L; r <- ti$rightChild[i] + 1L
    v <- ti$splitvarName[i];   s <- ti$splitval[i]
    cond[l] <- sprintf("%s <= %s", v, format(s, digits = 15)); parent[l] <- i
    cond[r] <- sprintf("%s > %s",  v, format(s, digits = 15)); parent[r] <- i
  }
  out <- character(0)
  for (i in seq_len(n)[-1]) {
    path <- character(0); j <- i
    while (j != 1L) { path <- c(cond[j], path); j <- parent[j] }
    out <- c(out, paste(path, collapse = " & "))
  }
  out
}

generate_rules_ranger <- function(df, is_error, features,
                                  num_trees = 500, max_depth = 4, mtry = NULL,
                                  min_node_size = 20, seed = 1) {
  cc <- stats::complete.cases(df[features])
  X  <- df[cc, features, drop = FALSE]
  y  <- factor(ifelse(is_error[cc], "error", "clean"), levels = c("clean", "error"))
  fit <- ranger::ranger(x = X, y = y,
                        num.trees = num_trees, max.depth = max_depth,
                        mtry = if (is.null(mtry)) floor(sqrt(length(features))) else mtry,
                        min.node.size = min_node_size, seed = seed)
  unique(unlist(lapply(seq_len(num_trees), function(t)
    .rules_from_ranger_tree(ranger::treeInfo(fit, t)))))
}

# Walk one xgboost tree (rows of xgb.model.dt.tree for a single Tree id).
# xgboost's Yes-branch is `feature < Split`, No-branch `feature >= Split`.
.rules_from_xgb_tree <- function(tt) {
  n <- nrow(tt)
  id2row <- stats::setNames(seq_len(n), tt$ID)
  cond <- character(n); parent <- integer(n)
  for (i in seq_len(n)) {
    if (tt$Feature[i] == "Leaf") next
    yi <- id2row[[tt$Yes[i]]]; ni <- id2row[[tt$No[i]]]
    v  <- tt$Feature[i];       s <- tt$Split[i]
    cond[yi] <- sprintf("%s < %s",  v, format(s, digits = 15)); parent[yi] <- i
    cond[ni] <- sprintf("%s >= %s", v, format(s, digits = 15)); parent[ni] <- i
  }
  root <- which(tt$Node == 0)
  out <- character(0)
  for (i in seq_len(n)) {
    if (i == root) next
    path <- character(0); j <- i
    while (j != root) { path <- c(cond[j], path); j <- parent[j] }
    out <- c(out, paste(path, collapse = " & "))
  }
  out
}

generate_rules_xgboost <- function(df, is_error, features,
                                   nrounds = 300, max_depth = 4, eta = 0.05,
                                   subsample = 0.5, seed = 1, params_extra = list()) {
  cc <- stats::complete.cases(df[features])
  X  <- as.matrix(df[cc, features, drop = FALSE]); storage.mode(X) <- "double"
  set.seed(seed)
  fit <- xgboost::xgb.train(
    params  = c(list(objective = "binary:logistic", max_depth = max_depth,
                     eta = eta, subsample = subsample), params_extra),
    data    = xgboost::xgb.DMatrix(X, label = as.numeric(is_error[cc]), nthread = 1),
    nrounds = nrounds, verbose = 0)
  dt <- xgboost::xgb.model.dt.tree(model = fit)
  dt <- as.data.frame(dt)
  unique(unlist(lapply(split(dt, dt$Tree), .rules_from_xgb_tree)))
}

# Bagged rpart engine (CART): many shallow rpart trees, each on a subsample —
# {pre}'s rule generator minus the boosting and the lasso. Fit as anova on the
# 0/1 outcome so splits happen at low base rates; cp near zero so depth is
# governed by max_depth/min_bucket rather than the complexity penalty.
generate_rules_rpart <- function(df, is_error, features,
                                 num_trees = 1000, max_depth = 4,
                                 sample_frac = 0.2, min_bucket = 20, seed = 1) {
  cc <- stats::complete.cases(df[features])
  dat <- cbind(.y = as.numeric(is_error[cc]), df[cc, features, drop = FALSE])
  set.seed(seed)
  n <- nrow(dat)
  out <- vector("list", num_trees)
  for (t in seq_len(num_trees)) {
    ix <- sample.int(n, size = max(2L, floor(sample_frac * n)))
    fit <- rpart::rpart(.y ~ ., data = dat[ix, , drop = FALSE], method = "anova",
                        control = rpart::rpart.control(maxdepth = max_depth,
                                                       minbucket = min_bucket,
                                                       cp = 1e-5, xval = 0))
    if (nrow(fit$frame) <= 1) next
    paths <- rpart::path.rpart(fit, nodes = rownames(fit$frame), print.it = FALSE)
    out[[t]] <- vapply(paths[-1], function(p) paste(p[-1], collapse = " & "), "")
  }
  unique(unlist(out))
}

## ── 2. Canonicalize ───────────────────────────────────────────────────────────
# Parse "var op number" conditions, round thresholds to `signif_digits`
# significant digits (state-presentable; collapses near-identical cutpoints),
# collapse repeated same-variable same-direction bounds to the binding one,
# drop self-contradictory rules, and order conditions by variable name so that
# identical rules print identically.

.COND_RE <- "^\\s*([A-Za-z._][A-Za-z0-9._]*)\\s*(<=|>=|<|>)\\s*(-?[0-9]*\\.?[0-9]+(?:[eE][+-]?[0-9]+)?)\\s*$"

.parse_rule <- function(rule) {
  conds <- strsplit(rule, " & ", fixed = TRUE)[[1]]
  m <- regmatches(conds, regexec(.COND_RE, conds))
  if (any(vapply(m, length, integer(1)) != 4)) return(NULL)  # unparseable -> drop
  data.frame(var = vapply(m, `[`, "", 2),
             op  = vapply(m, `[`, "", 3),
             thr = as.numeric(vapply(m, `[`, "", 4)),
             stringsAsFactors = FALSE)
}

canonicalize_rule <- function(rule, signif_digits = 3) {
  p <- .parse_rule(rule)
  if (is.null(p)) return(NA_character_)
  p$thr <- signif(p$thr, signif_digits)
  p$dir <- ifelse(p$op %in% c("<", "<="), "upper", "lower")

  keep <- rep(TRUE, nrow(p))
  for (g in unique(paste(p$var, p$dir))) {
    ix <- which(paste(p$var, p$dir) == g)
    if (length(ix) < 2) next
    strict <- p$op[ix] %in% c("<", ">")
    ord <- if (p$dir[ix[1]] == "upper") order(p$thr[ix], !strict)   # tightest upper
           else                          order(-p$thr[ix], !strict) # tightest lower
    keep[setdiff(ix, ix[ord[1]])] <- FALSE
  }
  p <- p[keep, , drop = FALSE]

  # contradiction check per variable: an upper bound at or below a lower bound
  for (v in unique(p$var)) {
    up <- p[p$var == v & p$dir == "upper", ]; lo <- p[p$var == v & p$dir == "lower", ]
    if (nrow(up) == 1 && nrow(lo) == 1) {
      if (up$thr < lo$thr) return(NA_character_)
      if (up$thr == lo$thr && (up$op == "<" || lo$op == ">")) return(NA_character_)
    }
  }

  p <- p[order(p$var, p$dir), , drop = FALSE]
  paste(sprintf("%s %s %s", p$var, p$op, format(p$thr, digits = 15, scientific = FALSE)),
        collapse = " & ")
}

canonicalize_rules <- function(rules, signif_digits = 3) {
  out <- vapply(rules, canonicalize_rule, "", signif_digits = signif_digits,
                USE.NAMES = FALSE)
  unique(out[!is.na(out)])
}

## ── 3. Evaluate: condition-indexed sparse flags ───────────────────────────────
# Rules repeat the same conditions constantly, so each UNIQUE condition is
# evaluated once per dataset into an integer index vector; a rule's flags are
# the intersection of its conditions' vectors (optionally within a stratum).
# NA condition results count as FALSE, matching the v1 flag_rule behaviour.

build_condition_index <- function(rules) {
  cond_list <- strsplit(rules, " & ", fixed = TRUE)
  conditions <- unique(unlist(cond_list))
  list(conditions = conditions,
       rule_conds = lapply(cond_list, function(cs) match(cs, conditions)))
}

eval_conditions <- function(conditions, data) {
  lapply(conditions, function(cd) {
    v <- tryCatch(with(data, eval(parse(text = cd))), error = function(e) NULL)
    if (is.null(v)) integer(0) else which(!is.na(v) & v)
  })
}

# Sparse flags for every rule: intersect its condition index vectors, starting
# from `base_idx` (e.g., the rows of the rule's household-size stratum).
rule_flag_idx <- function(rule_conds, cond_idx, base_idx = NULL) {
  lapply(rule_conds, function(ci) {
    out <- cond_idx[[ci[1]]]
    if (!is.null(base_idx)) out <- intersect(base_idx, out)
    for (k in ci[-1]) {
      if (length(out) == 0) break
      out <- intersect(out, cond_idx[[k]])
    }
    out
  })
}

# Convenience wrapper: flags for a rules data.frame (columns rule, hh) against
# one dataset. `strata_idx` is a named list mapping stratum label -> row indices.
flags_for_rules <- function(rules_df, data, strata_idx, label = "") {
  ci  <- build_condition_index(rules_df$rule)
  if (nzchar(label))
    cat(sprintf("  [%s] %d rules -> %d unique conditions\n",
                label, nrow(rules_df), length(ci$conditions)))
  idx <- eval_conditions(ci$conditions, data)
  out <- vector("list", nrow(rules_df))
  for (h in unique(rules_df$hh)) {
    rows <- which(rules_df$hh == h)
    out[rows] <- rule_flag_idx(ci$rule_conds[rows], idx, base_idx = strata_idx[[h]])
  }
  out
}

## ── 4. Dedup ──────────────────────────────────────────────────────────────────

# (a) exact coverage: rules flagging the SAME training cases collapse to one —
# the one with the fewest conditions (then shortest text). Cheap prehash on
# (length, first, last, sum) keeps identical() comparisons within tiny groups.
dedup_exact_coverage <- function(rules_df, idx_tr) {
  key <- vapply(idx_tr, function(ix)
    paste(length(ix),
          if (length(ix) > 0) ix[1] else 0L,
          if (length(ix) > 0) ix[length(ix)] else 0L,
          sum(as.numeric(ix))), "")
  n_conds <- lengths(strsplit(rules_df$rule, " & ", fixed = TRUE))
  drop <- rep(FALSE, nrow(rules_df))
  for (g in unique(key[duplicated(key)])) {
    ix <- which(key == g)
    ord <- ix[order(n_conds[ix], nchar(rules_df$rule[ix]))]
    for (a in ord[-1]) {
      for (b in ord) {
        if (a == b || drop[b]) next
        if (identical(idx_tr[[a]], idx_tr[[b]])) { drop[a] <- TRUE; break }
      }
    }
  }
  drop
}

# (b) same-structure dominance: within one stratum, group rules by SIGNATURE =
# the sorted set of (variable, direction) pairs. Members differ only in
# thresholds, so one covers a superset of another when every bound is looser or
# equal. Drop rule A when a superset rule B has train stat >= A's: wherever A
# qualifies, B also qualifies and contains A, so A can never add recall.
.rule_struct <- function(rule) {
  p <- .parse_rule(rule)
  dir <- ifelse(p$op %in% c("<", "<="), "upper", "lower")
  key <- paste0(p$var, "|", dir)
  ord <- order(key)
  list(sig = paste(key[ord], collapse = ";"),
       thr = p$thr[ord], dir = dir[ord])
}

# B superset-or-equal of A (same signature assumed)
.covers <- function(bs, as) {
  all(ifelse(bs$dir == "upper", bs$thr >= as$thr, bs$thr <= as$thr))
}

dedup_dominated <- function(rules_df, stat) {
  structs <- lapply(rules_df$rule, .rule_struct)
  sig  <- paste(rules_df$hh, vapply(structs, function(s) s$sig, ""))
  drop <- rep(FALSE, nrow(rules_df))
  for (g in unique(sig[duplicated(sig)])) {
    ix <- which(sig == g)
    if (length(structs[[ix[1]]]$thr) == 1) {
      # single bound: totally ordered by threshold. Walk from loosest to
      # tightest keeping a running max of the stat; drop anything not above it.
      loose_first <- if (structs[[ix[1]]]$dir == "upper")
        ix[order(-vapply(structs[ix], function(s) s$thr, 0))]
      else ix[order( vapply(structs[ix], function(s) s$thr, 0))]
      best <- -Inf
      for (a in loose_first) {
        if (!is.na(stat[a]) && stat[a] > best) best <- stat[a] else drop[a] <- TRUE
      }
    } else {
      # a rule can only be dominated by one with an equal-or-higher stat, so
      # order the group by stat and scan upward only; break on first dominator
      ord <- ix[order(-stat[ix], na.last = TRUE)]
      for (pos in seq_along(ord)[-1]) {
        a <- ord[pos]
        if (is.na(stat[a])) next
        for (pos_b in seq_len(pos - 1)) {
          b <- ord[pos_b]
          if (drop[b] || is.na(stat[b])) next
          if (.covers(structs[[b]], structs[[a]]) &&
              !identical(structs[[b]]$thr, structs[[a]]$thr)) { drop[a] <- TRUE; break }
        }
      }
    }
  }
  drop
}

## ── 5. Sweep + per-rule evaluation ────────────────────────────────────────────

# Union sweep over selection-statistic thresholds, single pass from high to low.
# `stat_tr` is the per-rule selection statistic (Wilson LCB of train precision).
# An error caught by several rules counts once (union); no rule is penalized
# for re-catching errors other rules already found.
precision_sweep <- function(stat_tr, usable, idx_h, ie_h, ed_h, grid) {
  N_h <- length(ie_h); err_h <- sum(ie_h); dol_h <- sum(ed_h)
  un <- rep(FALSE, N_h); in_union <- rep(FALSE, length(stat_tr)); out <- list()
  for (t in sort(grid, decreasing = TRUE)) {
    new_rules <- which(usable & !in_union & stat_tr >= t)
    for (k in new_rules) un[idx_h[[k]]] <- TRUE
    in_union[new_rules] <- TRUE
    if (!any(in_union)) next
    nfl <- sum(un); tp <- sum(un & ie_h)
    out[[length(out) + 1L]] <- data.frame(
      threshold = t, n_rules = sum(in_union),
      n_flagged = nfl, workload = nfl / N_h,
      precision = if (nfl > 0) tp / nfl else NA_real_,
      recall = if (err_h > 0) tp / err_h else NA_real_,
      dollar_recall = if (dol_h > 0) sum(ed_h[un]) / dol_h else NA_real_)
  }
  out <- do.call(rbind, out)
  if (!is.null(out)) out <- out[order(out$threshold), , drop = FALSE]
  out
}

# Per-rule stats on one dataset (within the rule's stratum). Returns columns to
# cbind onto the rules data.frame; `prefix` names them (e.g. "train", "holdout").
eval_rules_on <- function(rules_df, idx, ie, ed, strata_idx, prefix) {
  n_flag <- lengths(idx)
  k      <- vapply(idx, function(ix) sum(ie[ix]), numeric(1))
  n_str  <- vapply(rules_df$hh, function(h) length(strata_idx[[h]]), numeric(1))
  err_str <- vapply(rules_df$hh, function(h) sum(ie[strata_idx[[h]]]), numeric(1))
  dol_str <- vapply(rules_df$hh, function(h) sum(ed[strata_idx[[h]]]), numeric(1))
  dol_cau <- vapply(idx, function(ix) sum(ed[ix]), numeric(1))
  out <- data.frame(
    n_flagged = n_flag,
    errors_caught = k,
    precision     = ifelse(n_flag > 0, round(k / n_flag, 4), NA_real_),
    recall        = ifelse(err_str > 0, round(k / err_str, 4), NA_real_),
    dollar_recall = ifelse(dol_str > 0, round(dol_cau / dol_str, 4), NA_real_),
    workload_pct  = round(100 * n_flag / pmax(n_str, 1), 3))
  names(out) <- paste0(names(out), "_", prefix)
  out
}

## ── 6. Output helpers ─────────────────────────────────────────────────────────

save_png <- function(plot, file, w, h, dpi = 300) {
  grDevices::png(file, width = w, height = h, units = "in", res = dpi, type = "cairo")
  on.exit(grDevices::dev.off()); print(plot)
}
