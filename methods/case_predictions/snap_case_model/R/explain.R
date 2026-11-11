# Name the state's flagging rules that a case meets: an explanation of a high score,
# not an input to it.
#
# The rules come from the state's delivery list (the same list behind the state's Excel
# workbook). Rules that use count_divisible_by_100 are left out, as in the workbook,
# because that feature cannot be built from the input columns. Each rule applies only to
# its household-size group (1, 2-3 or 4+); a blank input never meets a condition.

hh_group_of <- function(n) ifelse(is.na(n), NA_character_, ifelse(n <= 1, "1", ifelse(n <= 3, "2-3", "4+")))

# one condition, e.g. "gross_by_hh_size <= 17.5", as a logical vector over the cases
.condition <- function(cond, feat) {
  p <- regmatches(cond, regexec("^([A-Za-z_][A-Za-z0-9_]*) (>=|<=|>|<|==) (-?[0-9.]+(e[-+]?[0-9]+)?)$", cond))[[1]]
  if (!length(p)) stop("cannot read rule condition: ", cond)
  x <- feat[[p[2]]]; if (is.null(x)) stop("rule uses an unknown feature: ", p[2])
  v <- switch(p[3], ">=" = x >= as.numeric(p[4]), "<=" = x <= as.numeric(p[4]), ">" = x > as.numeric(p[4]),
              "<" = x < as.numeric(p[4]), "==" = x == as.numeric(p[4]))
  !is.na(v) & v
}

rules_flagging <- function(feat, rules) {
  rules <- rules[!grepl("count_divisible_by_100", rules$rule, fixed = TRUE), ]
  hh <- hh_group_of(feat$HH_size_n)
  conds <- strsplit(rules$rule, " & ", fixed = TRUE)
  cache <- list()
  out <- vector("list", nrow(feat)); for (i in seq_along(out)) out[[i]] <- integer(0)
  for (k in seq_len(nrow(rules))) {
    v <- !is.na(hh) & hh == as.character(rules$hh[k])
    for (cd in conds[[k]]) {
      if (is.null(cache[[cd]])) cache[[cd]] <- .condition(cd, feat)
      v <- v & cache[[cd]]
    }
    for (i in which(v)) out[[i]] <- c(out[[i]], rules$rank[k])
  }
  list(ranks = vapply(out, function(r) paste(sort(r), collapse = " "), ""),
       n_rules = nrow(rules))
}
