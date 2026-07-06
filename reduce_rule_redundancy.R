# reduce_rule_redundancy.R
# Cleans a by-HH-size inclusion-rule table in three passes:
#   1. collapse same-variable same-direction conditions to the binding threshold (table only)
#   2. flag geometrically dominated rules: region is a subset of another rule whose
#      precision is at least as high (table only)
#   3. prune near-vacuous conditions whose removal barely changes the flagged set (needs case data)
#
# Nothing is deleted silently. The annotated output keeps every input rule with reason
# columns. The cleaned output is the filtered, condition-pruned result.
# Passes 1-2 run on the rule table alone. Pass 3 and the precision recompute activate
# only when CASE_DATA is set.

# ---- config ----------------------------------------------------------------
RULE_CSV       <- "inclusion_rules_by_hh_size/final_by_HHsize_inclusion_rules_highprecision.csv"
CASE_DATA      <- reg_model_data                  # path to reg_model_data (.rds or .csv); NULL skips pass 3
HH_COL         <- "HH_size_n"           # household-size column in the case data
ERROR_STATUS   <- "error_status"        # error-label column in the case data
FOCAL_ERROR    <- "earned_overissuance" # value in ERROR_STATUS counted as an error here
RULE_COL       <- "rule"
PREC_COL       <- "precision"
HH_RULE_COL    <- "hh_size"
VACUOUS_KEEP   <- 0.98                  # a condition is near-vacuous if the full rule keeps
                                        # at least this share of the set defined without it
MIN_FLAG_PRUNE <- 20L                   # do not prune conditions on rules flagging fewer cases
OUT_ANNOT      <- "rules_redundancy_annotated.csv"
OUT_CLEAN      <- "rules_redundancy_cleaned.csv"

# map a numeric household size to the strata labels used in the rule table
hh_group_of <- function(n) {
  g <- as.character(pmin(as.integer(n), 5L))
  ifelse(g == "5", "5+", g)
}

# ---- rule parsing ----------------------------------------------------------
COND_RE <- "^\\s*([A-Za-z0-9_.]+)\\s*(>=|<=|>|<|==)\\s*(-?[0-9.]+(?:[eE][-+]?[0-9]+)?)\\s*$"

split_conds <- function(rule) trimws(strsplit(rule, " & ", fixed = TRUE)[[1]])

parse_numeric <- function(cond) {
  m <- regmatches(cond, regexec(COND_RE, cond))[[1]]
  if (length(m) == 0) return(NULL)
  list(var = m[2], op = m[3], val = as.numeric(m[4]))
}

# ---- pass 1: collapse same-variable same-direction conditions --------------
collapse_rule <- function(rule) {
  conds  <- split_conds(rule)
  parsed <- lapply(conds, parse_numeric)
  is_num <- !vapply(parsed, is.null, logical(1))
  if (!any(is_num)) return(rule)
  lowers <- list(); uppers <- list(); changed <- FALSE
  for (i in which(is_num)) {
    p <- parsed[[i]]
    if (p$op %in% c(">", ">=")) {
      if (!is.null(lowers[[p$var]])) changed <- TRUE
      if (is.null(lowers[[p$var]]) || p$val > lowers[[p$var]]$val) lowers[[p$var]] <- p
    } else {
      if (!is.null(uppers[[p$var]])) changed <- TRUE
      if (is.null(uppers[[p$var]]) || p$val < uppers[[p$var]]$val) uppers[[p$var]] <- p
    }
  }
  if (!changed) return(rule)                 # no same-direction duplicate, leave untouched
  keep <- conds[!is_num]                     # pass categoricals and anything unparsed through
  for (k in names(lowers)) keep <- c(keep, sprintf("%s %s %s", k, lowers[[k]]$op, lowers[[k]]$val))
  for (k in names(uppers)) keep <- c(keep, sprintf("%s %s %s", k, uppers[[k]]$op, uppers[[k]]$val))
  paste(keep, collapse = " & ")
}

# ---- pass 2: geometric dominance -------------------------------------------
rule_box <- function(rule) {
  box <- list(); cats <- character(0)
  for (cond in split_conds(rule)) {
    p <- parse_numeric(cond)
    if (is.null(p)) { cats <- c(cats, cond); next }
    b <- box[[p$var]]
    if (is.null(b)) b <- list(lo = -Inf, lo_in = TRUE, hi = Inf, hi_in = TRUE)
    if (p$op %in% c(">", ">=")) {
      if (p$val > b$lo) { b$lo <- p$val; b$lo_in <- (p$op == ">=") }
    } else {
      if (p$val < b$hi) { b$hi <- p$val; b$hi_in <- (p$op == "<=") }
    }
    box[[p$var]] <- b
  }
  list(box = box, cats = sort(cats))
}

# region(A) is a subset of region(B): for every variable B constrains, A is at least as tight
is_subset <- function(A, B) {
  for (v in names(B$box)) {
    if (is.null(A$box[[v]])) return(FALSE)
    a <- A$box[[v]]; b <- B$box[[v]]
    if (a$lo < b$lo || (a$lo == b$lo && b$lo_in && !a$lo_in)) return(FALSE)
    if (a$hi > b$hi || (a$hi == b$hi && b$hi_in && !a$hi_in)) return(FALSE)
  }
  all(B$cats %in% A$cats)
}

flag_dominated <- function(df) {
  df$dominated   <- FALSE
  df$dominated_by <- NA_character_
  for (hh in unique(df[[HH_RULE_COL]])) {
    idx   <- which(df[[HH_RULE_COL]] == hh)
    boxes <- lapply(df[[RULE_COL]][idx], rule_box)
    prec  <- df[[PREC_COL]][idx]
    for (a in seq_along(idx)) {
      for (b in seq_along(idx)) {
        if (a == b || df$dominated[idx[b]]) next
        if (prec[b] >= prec[a] && is_subset(boxes[[a]], boxes[[b]])) {
          df$dominated[idx[a]]    <- TRUE
          df$dominated_by[idx[a]] <- df$rule_id[idx[b]]
          break
        }
      }
    }
  }
  df
}

# ---- pass 3: near-vacuous condition pruning (requires case data) ------------
prune_vacuous <- function(rule, sub, keep_share, min_flag) {
  kept <- split_conds(rule); dropped <- character(0)
  full_mask <- with(sub, eval(parse(text = rule)))
  if (sum(full_mask, na.rm = TRUE) < min_flag) return(list(rule = rule, dropped = dropped))
  repeat {
    if (length(kept) <= 1L) break
    n_full <- sum(with(sub, eval(parse(text = paste(kept, collapse = " & ")))), na.rm = TRUE)
    shares <- rep(NA_real_, length(kept))
    for (i in seq_along(kept)) {
      rest  <- kept[-i]
      n_rest <- sum(with(sub, eval(parse(text = paste(rest, collapse = " & ")))), na.rm = TRUE)
      if (n_rest > 0) shares[i] <- n_full / n_rest
    }
    i_max <- which.max(shares)
    if (length(i_max) == 0 || is.na(shares[i_max]) || shares[i_max] < keep_share) break
    dropped <- c(dropped, kept[i_max]); kept <- kept[-i_max]
  }
  list(rule = paste(kept, collapse = " & "), dropped = dropped)
}

rule_metrics <- function(rule, sub, err_flag) {
  mask <- with(sub, eval(parse(text = rule)))
  nf   <- sum(mask, na.rm = TRUE)
  list(n_flagged = nf,
       precision = if (nf > 0) mean(err_flag[mask], na.rm = TRUE) else NA_real_)
}

# ---- driver ----------------------------------------------------------------
rules <- read.csv(RULE_CSV, stringsAsFactors = FALSE, check.names = FALSE)
rules[[HH_RULE_COL]] <- as.character(rules[[HH_RULE_COL]])

# pass 1
orig <- rules[[RULE_COL]]
rules[[RULE_COL]]    <- vapply(orig, collapse_rule, character(1))
rules$collapsed      <- rules[[RULE_COL]] != orig

# pass 2
rules <- flag_dominated(rules)

# pass 3 plus precision recompute
rules$pruned_conditions <- ""
rules$rule_pruned       <- rules[[RULE_COL]]
rules$precision_recomputed <- NA_real_
rules$n_flagged_recomputed <- NA_integer_

if (!is.null(CASE_DATA)) {
  dat <- if (grepl("\\.rds$", CASE_DATA, ignore.case = TRUE)) readRDS(CASE_DATA) else
    read.csv(CASE_DATA, stringsAsFactors = FALSE, check.names = FALSE)
  dat$.grp <- hh_group_of(dat[[HH_COL]])
  for (i in which(!rules$dominated)) {
    hh  <- rules[[HH_RULE_COL]][i]
    sub <- dat[dat$.grp == hh, , drop = FALSE]
    if (nrow(sub) == 0) next
    err_flag <- sub[[ERROR_STATUS]] == FOCAL_ERROR
    pr <- prune_vacuous(rules[[RULE_COL]][i], sub, VACUOUS_KEEP, MIN_FLAG_PRUNE)
    rules$rule_pruned[i]       <- pr$rule
    rules$pruned_conditions[i] <- paste(pr$dropped, collapse = " | ")
    m <- rule_metrics(pr$rule, sub, err_flag)
    rules$n_flagged_recomputed[i] <- m$n_flagged
    rules$precision_recomputed[i] <- round(m$precision, 4)
  }
}

# ---- outputs ---------------------------------------------------------------
write.csv(rules, OUT_ANNOT, row.names = FALSE)

clean <- rules[!rules$dominated, , drop = FALSE]
clean[[RULE_COL]] <- clean$rule_pruned
clean <- clean[, setdiff(names(clean), c("rule_pruned")), drop = FALSE]
write.csv(clean, OUT_CLEAN, row.names = FALSE)

cat(sprintf("Input rules:        %d\n", nrow(rules)))
cat(sprintf("Same-direction collapsed: %d\n", sum(rules$collapsed)))
cat(sprintf("Dominated (dropped):      %d\n", sum(rules$dominated)))
if (!is.null(CASE_DATA))
  cat(sprintf("Rules with pruned conditions: %d\n", sum(nchar(rules$pruned_conditions) > 0)))
cat(sprintf("Cleaned rules out:  %d\n", nrow(clean)))
cat(sprintf("Wrote %s and %s\n", OUT_ANNOT, OUT_CLEAN))
