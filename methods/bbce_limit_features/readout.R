# BBCE-limit feature exploration: readout over whichever arms finished.
# Each arm against arm A (the shipped 19 features, also ranger alone), paired
# by state, on all FY2022 cases and on the non-pile cases; the standard
# companions (median, mean, states worse / better by more than 0.05); arm A
# at seed 118 as the direct reading of ranger-only seed noise. Descriptive:
# the trigger for a second night is a screen, not evidence (see section 0).
#   Rscript methods/bbce_limit_features/readout.R
suppressMessages(library(dplyr))
options(width = 220)
setwd(Sys.getenv("SR_ROOT", "C:/Users/ericg/snap_qc_v27"))
SMOKE <- identical(Sys.getenv("SMOKE"), "1")
D <- if (SMOKE) "methods/bbce_limit_features/smoke" else "methods/bbce_limit_features"
LAB <- c(A = "A: shipped 19", A_seed118 = "A at seed 118 (seed noise)", B = "B: + distance to limit", C = "C: + BBCE limit",
         D = "D: + distance + limit", E = "E: + income % of poverty + limit")
# the largest three-seed range of errors caught in this window, over the three
# frames measured (CPI-off 25 / 6, 09-29 18 / 57, final 21 / 43; xgboost +
# ranger pools; methods/v270_seed_replicates/fy2022/readout.md)
YARD <- c(`0.05` = 25, `0.1` = 57)
out <- character(0); say <- function(...) { s <- sprintf(...); cat(s, "\n", sep = ""); out <<- c(out, s) }
tab <- function(df) { x <- capture.output(print(as.data.frame(df), row.names = FALSE)); cat(x, sep = "\n"); out <<- c(out, "```", x, "```") }
arms <- names(LAB)[file.exists(file.path(D, sprintf("states_%s.csv", names(LAB))))]
say("# BBCE-limit features: readout%s", if (SMOKE) " [SMOKE]" else "")
say("Generated %s. Mined FY2017-19 with ranger alone (1000 trees, depth 4, mtry 2), seed 117 (arm A also at 118); scored FY2022. National pool only, mismatch-tagged and income-pile-tagged rules dropped (the shipped gates), 49 state lists, shipped fill, 5%% and 10%% budgets. Arms finished: %s.",
    format(Sys.time(), "%Y-%m-%d %H:%M"), paste(arms, collapse = ", "))
if (!length(arms)) { writeLines(out, file.path(D, "readout.md")); quit(save = "no") }
st <- bind_rows(lapply(arms, function(a) read.csv(file.path(D, sprintf("states_%s.csv", a)))))

pair <- function(a, b, sc = c("all", "nonpile")) {
  sc <- match.arg(sc); sfx <- if (sc == "all") "" else "_np"
  pc <- paste0("precision", sfx); ec <- if (sc == "all") "n_errors_caught" else "n_errors_np"
  fc <- paste0("n_flagged", sfx); dc <- paste0("dollar_recall", sfx)
  x <- st %>% filter(budget == b, arm %in% c("A", a)) %>% select(arm, state, p = all_of(pc), e = all_of(ec), f = all_of(fc), d = all_of(dc)) %>%
    tidyr::pivot_wider(names_from = arm, values_from = c(p, e, f, d))
  g <- function(v, arm) x[[paste0(v, "_", arm)]]
  dp <- g("p", a) - g("p", "A")
  list(errors = sum(g("e", a)) - sum(g("e", "A")),
       pooled = sum(g("e", a)) / sum(g("f", a)) - sum(g("e", "A")) / sum(g("f", "A")),
       median = median(dp), mean = mean(dp), up = sum(dp > 0), down = sum(dp < 0),
       harmed = sum(dp < -0.05), helped = sum(dp > 0.05), dollars = mean(g("d", a) - g("d", "A")))
}

## 0. how to read one seed
noise <- if ("A_seed118" %in% arms) sapply(c(0.05, 0.10), function(b) abs(pair("A_seed118", b)$errors)) else c(NA, NA)
thr <- pmax(YARD, ifelse(is.na(noise), 0, noise))
say(""); say("## 0. How to read one seed")
say("A shared seed does not pair the arms: adding a feature changes ranger's random draws, so each arm-minus-A difference is a difference of two independent draws, and the best of four arms is a selected maximum. The thresholds below screen for a second night; they are not evidence that an arm is better, and a difference inside them is no evidence that it is not.")
say("Thresholds, errors caught on all cases: 5%% budget %d, 10%% budget %d (the larger of the three-seed range on xgboost + ranger pools in this window, %d / %d, and arm A's own seed-118 minus seed-117 difference, %s / %s).",
    thr[1], thr[2], YARD[1], YARD[2], noise[1], noise[2])
say("Second night (three seeds for that arm, then both engines) if an arm beats A by more than both thresholds, its non-pile change has the same sign at both budgets, its median and mean changes are positive, and it harms no more states than it helps.")

say(""); say("## 1. Pooled over the 49 state lists")
tab(st %>% group_by(arm, budget) %>% summarise(errors = sum(n_errors_caught), flagged = sum(n_flagged),
      pooled_precision = round(sum(n_errors_caught) / sum(n_flagged), 4),
      errors_np = sum(n_errors_np), precision_np = round(sum(n_errors_np) / sum(n_flagged_np), 4),
      median_state = round(median(precision), 4), mean_state = round(mean(precision), 4), mean_dollar_recall = round(mean(dollar_recall), 4),
      visible = first(n_visible), pile_dropped = first(n_pile_tagged), deepest_rank = max(deepest_rank),
      sel_new = sum(n_sel_new), sel_regime = sum(n_sel_regime), sel_regime_below_own = sum(n_sel_regime_below), .groups = "drop") %>%
    mutate(arm = LAB[arm]) %>% arrange(budget, arm))
say("errors_np / precision_np: the same lists scored on FY2022 cases that are not income-pile cases. sel_* count rule slots across the 49 lists: using a new feature; limited by a state-level feature (BBCE flag or limit); and of those, with a 99%% bound at or below their own regime's training error rate.")

if ("A" %in% arms && length(arms) > 1) {
  say(""); say("## 2. Each arm minus A, paired by state")
  say("| arm | budget | cases | errors caught | pooled precision | median | mean | states up / down | worse by > 0.05 / better by > 0.05 | mean dollar recall |")
  say("|---|---|---|---|---|---|---|---|---|---|")
  for (a in setdiff(arms, "A")) for (b in c(0.05, 0.10)) for (sc in c("all", "nonpile")) {
    p <- pair(a, b, sc)
    say("| %s | %.0f%% | %s | %+d | %+.4f | %+.4f | %+.4f | %d / %d | %d / %d | %+.4f |", LAB[[a]], 100 * b, sc, p$errors, p$pooled,
        p$median, p$mean, p$up, p$down, p$harmed, p$helped, p$dollars)
  }
  bi <- read.csv("additional_data/bbce_income.csv", check.names = FALSE, fileEncoding = "UTF-8-BOM")
  l <- function(y) { v <- suppressWarnings(as.numeric(bi[[y]])); ifelse(is.na(v), 130, v) * ifelse(is.na(v), -1, 1) }   # no BBCE coded apart from BBCE at 130
  groups <- list(
    `minority regimes in FY2022 (no BBCE, or BBCE at 130%)` = bi$state_name[is.na(suppressWarnings(as.numeric(bi[["2022"]]))) | suppressWarnings(as.numeric(bi[["2022"]])) <= 130],
    `limit or BBCE status in FY2022 differs from some FY2017-19 year` = bi$state_name[l("2022") != l("2017") | l("2022") != l("2018") | l("2022") != l("2019")])
  for (gname in names(groups)) {
    say(""); say("Subgroup: %s. Errors caught, each arm minus A.", gname)
    tab(st %>% filter(state %in% groups[[gname]]) %>% group_by(arm, budget) %>%
          summarise(states = n(), errors = sum(n_errors_caught), flagged = sum(n_flagged), errors_np = sum(n_errors_np), .groups = "drop") %>%
          group_by(budget) %>% mutate(minus_A = errors - errors[arm == "A"], minus_A_np = errors_np - errors_np[arm == "A"]) %>% ungroup() %>%
          mutate(arm = LAB[arm]) %>% arrange(budget, arm))
  }
}

say(""); say("## 3. Rules: how often the new features are used, and the regime check")
rl <- lapply(arms, function(a) { fn <- file.path(D, sprintf("rules_%s.rds", a)); if (file.exists(fn)) readRDS(fn) })
names(rl) <- arms; rl <- rl[!vapply(rl, is.null, logical(1))]
tab(bind_rows(lapply(rl, function(r) {
  top <- r$rules[seq_len(min(1000, nrow(r$rules))), ]
  data.frame(arm = LAB[[r$tag]], visible = r$n_visible, tagged = r$n_tagged, pile_dropped = r$n_pile,
             uses_dist = r$feature_use[["dist_to_limit_pts"]], uses_limit = r$feature_use[["bbce_limit_pct"]],
             uses_pov = r$feature_use[["gross_pov_pct"]], uses_bbce_flag = r$feature_use[["bbce_state_i"]],
             top1000_new = sum(top$uses_dist_to_limit_pts | top$uses_bbce_limit_pct | top$uses_gross_pov_pct),
             top1000_test_precision = round(sum(top$k_te) / sum(top$n_te), 4),
             top1000_below_bound = round(mean((top$k_te / top$n_te)[top$n_te >= 10] < top$lcb[top$n_te >= 10]), 4),
             regime_limited = nrow(r$regime), regime_below_own = sum(r$regime$lcb_below_regime_rate),
             regime_below_own_top1000 = sum(r$regime$lcb_below_regime_rate & r$regime$rank <= 1000))
})))
say("uses_* count visible rules with a condition on that feature. regime_below_own: regime-limited rules whose 99%% bound is at or below their own regime's training error rate in their household-size group (they pass admission on the regime's base rate alone).")
say(""); say("Top 1,000 rules on FY2022, those using a new feature against the rest: flag-weighted test precision.")
tab(bind_rows(lapply(rl, function(r) { top <- r$rules[seq_len(min(1000, nrow(r$rules))), ]
  u <- top$uses_dist_to_limit_pts | top$uses_bbce_limit_pct | top$uses_gross_pov_pct
  data.frame(arm = LAB[[r$tag]], rules = sum(u), test_precision_new = if (any(u)) round(sum(top$k_te[u]) / sum(top$n_te[u]), 4) else NA,
             test_precision_other = round(sum(top$k_te[!u]) / sum(top$n_te[!u]), 4)) })))
writeLines(out, file.path(D, "readout.md"))
