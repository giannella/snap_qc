# Rule groups and votes for the case-scoring-by-rule-votes study
# (methods/vote_scoring/design_note.md, "The score"). Sourced by the
# exploration and confirmation scripts after methods/v270_seed_replicates/common.R.
#
# Groups: within each household-size group, the pool's top 1,000 visible rules in
# 99% LCB order; a rule joins the first existing group whose leading rule shares
# more than half of their combined flagged mining-year cases (Jaccard > 0.5),
# else starts a new group. Groups keep the order of their leading rules.
TOP_PER_HH <- 1000L
JACCARD_MIN <- 0.5

top_rules <- function(pool) {
  pool <- tag_artifacts(pool)
  vis <- pool[!pool$artifact_i, , drop = FALSE]
  bind_rows(lapply(HH_LEVELS, function(h) {
    p <- vis[vis$hh == h, , drop = FALSE]
    p <- p[order(-p$lcb), , drop = FALSE]
    p[seq_len(min(TOP_PER_HH, nrow(p))), , drop = FALSE]
  }))
}

# rules: top_rules() output; flags_tr: their flag-index vectors on mining-year rows
build_groups <- function(rules, flags_tr) {
  rules$group <- NA_integer_; rules$rank_in_hh <- NA_integer_; rules$lead <- FALSE
  for (h in HH_LEVELS) {
    ix <- which(rules$hh == h)
    lead_ix <- integer(0)
    for (i in ix) {
      fi <- flags_tr[[i]]; joined <- FALSE
      for (g in seq_along(lead_ix)) {
        fl <- flags_tr[[lead_ix[g]]]
        inter <- length(intersect(fi, fl))
        if (inter / (length(fi) + length(fl) - inter) > JACCARD_MIN) {
          rules$group[i] <- g; joined <- TRUE; break
        }
      }
      if (!joined) { lead_ix <- c(lead_ix, i); rules$group[i] <- length(lead_ix); rules$lead[i] <- TRUE }
    }
    rules$rank_in_hh[ix] <- seq_along(ix)
  }
  rules
}

# The rules a workbook of R rules carries, by vote definition, R/3 per size group.
#   any: groups in rank order while their members total at most R/3
#   rep: the leading rules of the top R/3 groups
allot <- function(groups, R, def = c("any", "rep")) {
  def <- match.arg(def); per <- R %/% 3L
  bind_rows(lapply(HH_LEVELS, function(h) {
    g <- groups[groups$hh == h, , drop = FALSE]
    if (def == "rep") return(g[g$lead & g$group <= per, , drop = FALSE])
    sz <- tabulate(g$group, nbins = max(g$group))
    keep <- which(cumsum(sz) <= per)
    g[g$group %in% keep, , drop = FALSE]
  }))
}

# Vote per case: the number of allotted groups with at least one flagging rule.
# flags: flag-index vectors of the allotted rules (rows of `al`) on the scored
# rows; n_rows: the number of scored rows. Also returns the highest LCB among
# a case's flagging rules (0 when none flags it).
votes_of <- function(al, flags, n_rows) {
  key <- paste(al$hh, al$group)
  vote <- integer(n_rows); maxlcb <- numeric(n_rows)
  for (k in unique(key)) {
    hit <- unique(unlist(flags[key == k]))
    vote[hit] <- vote[hit] + 1L
  }
  for (i in seq_along(flags)) {
    f <- flags[[i]]
    if (length(f)) maxlcb[f] <- pmax(maxlcb[f], al$lcb[i])
  }
  list(vote = vote, maxlcb = maxlcb)
}
