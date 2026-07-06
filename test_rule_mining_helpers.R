source("rule_mining_helpers.R")
set.seed(42)
ok <- function(name, cond) cat(sprintf("%-52s %s\n", name, if (isTRUE(cond)) "PASS" else "FAIL"))

## synthetic data: error concentrated where x1 high & x2 low
n <- 6000
d <- data.frame(x1 = runif(n), x2 = runif(n), x3 = rnorm(n),
                flag_l = runif(n) > 0.7, grp = sample(c("1","2-3","4+"), n, TRUE))
p <- plogis(-3.4 + 3.5 * d$x1 - 2.5 * d$x2 + 1.2 * (d$flag_l))
d$is_err <- runif(n) < p
cat(sprintf("synthetic base rate: %.3f\n\n", mean(d$is_err)))

pf <- prep_features(d, c("x1","x2","x3","flag_l","nonexistent"))
d2 <- pf$data; feats <- pf$features
ok("prep_features keeps 4, coerces logical", length(feats) == 4 && is.integer(d2$flag_l))

## engines
rr <- generate_rules_ranger(d2, d2$is_err, feats, num_trees = 30, max_depth = 3,
                            mtry = 1, min_node_size = 20, seed = 7)
rx <- generate_rules_xgboost(d2, d2$is_err, feats, nrounds = 30, max_depth = 3,
                             eta = 0.1, subsample = 0.7, seed = 7)
cat(sprintf("ranger rules: %d | xgboost rules: %d\n", length(rr), length(rx)))
ok("ranger produced rules", length(rr) > 20)
ok("xgboost produced rules", length(rx) > 20)
parse_ok <- function(rs) all(vapply(rs, function(r)
  !inherits(tryCatch(eval(parse(text = r), envir = d2), error = function(e) e), "error"), logical(1)))
ok("all ranger rules evaluate", parse_ok(rr))
ok("all xgboost rules evaluate", parse_ok(rx))
rp <- generate_rules_rpart(d2, d2$is_err, feats, num_trees = 20, max_depth = 3,
                           sample_frac = 0.3, min_bucket = 10, seed = 7)
cat(sprintf("rpart rules: %d\n", length(rp)))
ok("rpart produced rules", length(rp) > 20)
ok("all rpart rules evaluate", parse_ok(rp))
ok("rpart rules canonicalize", length(canonicalize_rules(rp)) > 10)

## canonicalization
ok("collapse duplicate bounds",
   canonicalize_rule("x1 <= 5 & x2 > 1 & x1 <= 3") == "x1 <= 3 & x2 > 1")
ok("contradiction dropped", is.na(canonicalize_rule("x1 <= 2 & x1 > 5")))
ok("ordering canonical",
   canonicalize_rule("x2 > 1 & x1 <= 3") == canonicalize_rule("x1 <= 3 & x2 > 1"))
ok("rounding to 3 signif", grepl("0.123", canonicalize_rule("x1 <= 0.1234567"), fixed = TRUE))

rules <- canonicalize_rules(c(rr, rx), signif_digits = 3)
cat(sprintf("canonical pooled rules: %d\n", length(rules)))

## flags vs direct evaluation
rules_df <- data.frame(rule = rules, hh = sample(c("1","2-3","4+"), length(rules), TRUE),
                       stringsAsFactors = FALSE)
strata_idx <- lapply(setNames(nm = c("1","2-3","4+")), function(h) which(d2$grp == h))
idx <- flags_for_rules(rules_df, d2, strata_idx, label = "smoke")
direct <- function(i) {
  v <- eval(parse(text = rules_df$rule[i]), envir = d2); v[is.na(v)] <- FALSE
  sort(intersect(which(v), strata_idx[[rules_df$hh[i]]]))
}
chk <- vapply(sample(seq_len(nrow(rules_df)), 25), function(i)
  identical(sort(idx[[i]]), direct(i)), logical(1))
ok("sparse flags match direct eval (25 sampled)", all(chk))

## exact-coverage dedup: same coverage, different text
rd <- data.frame(rule = c("x1 <= 0.5", "x1 <= 0.5 & x2 <= 2", "x1 <= 0.4"), hh = "1",
                 stringsAsFactors = FALSE)
ix <- flags_for_rules(rd, d2, strata_idx)
dr <- dedup_exact_coverage(rd, ix)
ok("coverage dedup drops 2-cond twin, keeps 1-cond", identical(dr, c(FALSE, TRUE, FALSE)))

## dominance: single-bound family
rdd <- data.frame(rule = c("x1 > 0.7", "x1 > 0.5", "x1 > 0.3"), hh = "1",
                  stringsAsFactors = FALSE)
# stats: tight 0.5, mid 0.6 (dominates tight), loose 0.4 (keeps: adds coverage)
dd <- dedup_dominated(rdd, stat = c(0.5, 0.6, 0.4))
ok("dominance drops tighter rule with lower stat", identical(dd, c(TRUE, FALSE, FALSE)))

## multi-bound dominance
rdm <- data.frame(rule = c("x1 > 0.6 & x2 <= 0.3", "x1 > 0.5 & x2 <= 0.4"), hh = "1",
                  stringsAsFactors = FALSE)
dm <- dedup_dominated(rdm, stat = c(0.30, 0.35))  # B looser AND better -> drop A
ok("multi-bound dominance", identical(dm, c(TRUE, FALSE)))

## ladder collapse (stat_eps): tighter rung within eps of looser -> dropped;
## clearly better tighter rung -> kept
rl <- data.frame(rule = c("x1 > 0.985", "x1 > 0.983"), hh = "1",
                 stringsAsFactors = FALSE)
ok("eps collapses near-twin rung",
   identical(dedup_dominated(rl, stat = c(0.208, 0.205), stat_eps = 0.01),
             c(TRUE, FALSE)))
ok("distinct rung survives eps",
   identical(dedup_dominated(rl, stat = c(0.30, 0.205), stat_eps = 0.01),
             c(FALSE, FALSE)))
ok("stat_eps = 0 restores pure dominance",
   identical(dedup_dominated(rl, stat = c(0.208, 0.205), stat_eps = 0),
             c(FALSE, FALSE)))

## deliverable-time collapse: one rung per family (max stat), other structures kept
rc <- data.frame(rule = c("x1 > 0.985", "x1 > 0.983", "x2 <= 0.4 & x1 > 0.5"),
                 hh = "1", stringsAsFactors = FALSE)
ok("collapse_ladders keeps max-stat rung + other structures",
   identical(collapse_ladders(rc, stat = c(0.208, 0.205, 0.15)),
             c(TRUE, FALSE, TRUE)))

## sweep end-to-end
k_tr <- vapply(idx, function(ix) sum(d2$is_err[ix]), numeric(1))
n_tr <- lengths(idx)
stat <- wilson_lcb(k_tr, n_tr)
usable <- !is.na(stat) & n_tr >= 10
sw <- precision_sweep(stat, usable, idx, d2$is_err, as.numeric(d2$is_err),
                      grid = seq(0.05, 0.6, by = 0.05))
print(sw[, c("threshold","n_rules","n_flagged","precision","recall")], row.names = FALSE)
ok("sweep recall monotone non-increasing in threshold",
   all(diff(sw$recall) <= 1e-12))
ok("wilson_lcb sanity (4/20 < 40/200)", wilson_lcb(4, 20) < wilson_lcb(40, 200))
cat("\nsmoke test done\n")
