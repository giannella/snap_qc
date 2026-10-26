# BBCE-limit feature exploration, night 2 (2026-10-23): readout over whichever
# arms finished. Two questions, both against ranger-alone pools mined on
# FY2017-19 and scored on FY2022 (see design_note.md, "Night 2"):
#   1. Does a larger mtry (3, 4) let the limit features help? Each feature arm
#      against arm A at the SAME mtry, and against arm A at mtry 2 (the shipped
#      setting); arm A at mtry 3 and 4 against arm A at mtry 2.
#   2. The regimes plan's Tier 1: the six light state options (arm T1, mtry 2)
#      against arm A.
# Night 1's arms (A, A_seed118, D, E at mtry 2) are read from disk as the
# mtry-2 references. Descriptive: the screen for more seeds is not evidence.
#   Rscript methods/bbce_limit_features/readout_night2.R
suppressMessages(library(dplyr))
options(width = 220)
setwd(Sys.getenv("SR_ROOT", "C:/Users/ericg/snap_qc_v27"))
SMOKE <- identical(Sys.getenv("SMOKE"), "1")
D <- if (SMOKE) "methods/bbce_limit_features/smoke" else "methods/bbce_limit_features"
LAB <- c(A = "A: shipped 19, mtry 2", A_seed118 = "A, mtry 2, seed 118 (seed noise)",
         D = "D: + distance + limit, mtry 2", E = "E: + income % of poverty + limit, mtry 2",
         A_mtry3 = "A: shipped 19, mtry 3", D_mtry3 = "D: + distance + limit, mtry 3", E_mtry3 = "E: + income % of poverty + limit, mtry 3",
         A_mtry4 = "A: shipped 19, mtry 4", D_mtry4 = "D: + distance + limit, mtry 4", E_mtry4 = "E: + income % of poverty + limit, mtry 4",
         A_mtry4_seed118 = "A, mtry 4, seed 118 (seed noise)", T1 = "T1: + six light state options, mtry 2")
# the largest three-seed range of errors caught in this window (CPI-off 25 / 6,
# 09-29 18 / 57, final 21 / 43; xgboost + ranger pools;
# methods/v270_seed_replicates/fy2022/readout.md)
YARD <- c(`0.05` = 25, `0.1` = 57)
# Tier 1 subgroups, written by light_support.R from the frame and the option
# tables: states whose light options differ between some FY2017-19 year and
# FY2022; states whose SMD is below Part B minus $35 in FY2022 (no state is
# below it in FY2017-19)
sgc <- read.csv("methods/bbce_limit_features/light_subgroups.csv")
T1_CHANGED <- sgc$state[sgc$subgroup == "light_option_changed"]
SMD_BELOW_22 <- sgc$state[sgc$subgroup == "smd_below_line_fy2022"]
stopifnot(length(T1_CHANGED) == 20, length(SMD_BELOW_22) == 8)
# night 1's regime subgroups, built the same way as readout.R does
bi <- read.csv("additional_data/bbce_income.csv", check.names = FALSE, fileEncoding = "UTF-8-BOM")
lim <- function(y) { v <- suppressWarnings(as.numeric(bi[[y]])); ifelse(is.na(v), 130, v) * ifelse(is.na(v), -1, 1) }
REG_GROUPS <- list(
  `minority regimes in FY2022 (no BBCE, or BBCE at 130%)` = bi$state_name[is.na(suppressWarnings(as.numeric(bi[["2022"]]))) | suppressWarnings(as.numeric(bi[["2022"]])) <= 130],
  `limit or BBCE status in FY2022 differs from some FY2017-19 year` = bi$state_name[lim("2022") != lim("2017") | lim("2022") != lim("2018") | lim("2022") != lim("2019")])
out <- character(0); say <- function(...) { s <- sprintf(...); cat(s, "\n", sep = ""); out <<- c(out, s) }
tab <- function(df) { x <- capture.output(print(as.data.frame(df), row.names = FALSE)); cat(x, sep = "\n"); out <<- c(out, "```", x, "```") }
arms <- names(LAB)[file.exists(file.path(D, sprintf("states_%s.csv", names(LAB))))]
say("# BBCE-limit features, night 2: mtry 3 and 4, and the light state options%s", if (SMOKE) " [SMOKE]" else "")
say("Generated %s. Mined FY2017-19 with ranger alone (1000 trees, depth 4), seed 117 unless noted; scored FY2022. National pool only, mismatch-tagged and income-pile-tagged rules dropped, 49 state lists, shipped fill, 5%% and 10%% budgets. Arms on disk: %s.",
    format(Sys.time(), "%Y-%m-%d %H:%M"), paste(arms, collapse = ", "))
if (!length(arms)) { writeLines(out, file.path(D, "readout_night2.md")); quit(save = "no") }
st <- bind_rows(lapply(arms, function(a) { x <- read.csv(file.path(D, sprintf("states_%s.csv", a))); x$arm <- a; x }))

pair <- function(a, base, b, sc = c("all", "nonpile")) {
  sc <- match.arg(sc); sfx <- if (sc == "all") "" else "_np"
  pc <- paste0("precision", sfx); ec <- if (sc == "all") "n_errors_caught" else "n_errors_np"
  fc <- paste0("n_flagged", sfx); dc <- paste0("dollar_recall", sfx)
  g <- function(arm) st %>% filter(budget == b, arm == !!arm) %>% arrange(state) %>%
    select(state, p = all_of(pc), e = all_of(ec), f = all_of(fc), d = all_of(dc))
  x <- g(a); y <- g(base); stopifnot(identical(x$state, y$state))
  dp <- x$p - y$p
  list(errors = sum(x$e) - sum(y$e), pooled = sum(x$e) / sum(x$f) - sum(y$e) / sum(y$f),
       median = median(dp), mean = mean(dp), up = sum(dp > 0), down = sum(dp < 0),
       harmed = sum(dp < -0.05), helped = sum(dp > 0.05), dollars = mean(x$d - y$d))
}
have <- function(...) all(c(...) %in% arms)

## 0. how to read one seed
gap <- function(a, base) if (have(a, base)) sapply(c(0.05, 0.10), function(b) abs(pair(a, base, b)$errors)) else c(NA, NA)
n2 <- gap("A_seed118", "A"); n4 <- gap("A_mtry4_seed118", "A_mtry4")
thr <- pmax(YARD, ifelse(is.na(n2), 0, n2), ifelse(is.na(n4), 0, n4))
say(""); say("## 0. How to read one seed")
say("A shared seed does not pair the arms: a different feature list or mtry changes ranger's random draws, so each difference is a difference of two independent draws, and the best of several arms is a selected maximum. The thresholds screen for more seeds; they are not evidence that an arm is better, and a difference inside them is no evidence that it is not.")
say("Thresholds, errors caught on all cases: 5%% budget %d, 10%% budget %d (the larger of the three-seed range on xgboost + ranger pools in this window, %d / %d, and arm A's seed-118 minus seed-117 difference at mtry 2, %s / %s, and at mtry 4, %s / %s).",
    thr[1], thr[2], YARD[1], YARD[2], n2[1], n2[2], n4[1], n4[2])
say("There is no seed-noise reading at mtry 3; the mtry-2 and mtry-4 readings stand in for it.")
say("More seeds for an arm only if it beats arm A at mtry 2 (what ships) by more than both thresholds, its non-pile change has the same sign at both budgets, its median and mean changes are positive, and it harms no more states than it helps.")

say(""); say("## 1. Pooled over the 49 state lists")
tab(st %>% group_by(arm, budget) %>% summarise(errors = sum(n_errors_caught), flagged = sum(n_flagged),
      pooled_precision = round(sum(n_errors_caught) / sum(n_flagged), 4),
      errors_np = sum(n_errors_np), precision_np = round(sum(n_errors_np) / sum(n_flagged_np), 4),
      median_state = round(median(precision), 4), mean_state = round(mean(precision), 4), mean_dollar_recall = round(mean(dollar_recall), 4),
      visible = first(n_visible), pile_dropped = first(n_pile_tagged), deepest_rank = max(deepest_rank),
      sel_new = sum(n_sel_new), sel_regime = sum(n_sel_regime), sel_regime_below_own = sum(n_sel_regime_below), .groups = "drop") %>%
    mutate(arm = LAB[arm]) %>% arrange(budget, arm))
say("errors_np / precision_np: the same lists scored on FY2022 cases that are not income-pile cases. sel_* count rule slots across the 49 lists: using a feature the arm adds; limited by a state-level feature (BBCE flag or limit, or a light option); and of those, with a 99%% bound at or below their own regime's training error rate.")

CMP <- list(
  `Seed noise` = list(c("A_seed118", "A"), c("A_mtry4_seed118", "A_mtry4")),
  `mtry alone (the shipped 19 features)` = list(c("A_mtry3", "A"), c("A_mtry4", "A")),
  `Limit features at the same mtry` = list(c("D", "A"), c("E", "A"), c("D_mtry3", "A_mtry3"), c("E_mtry3", "A_mtry3"),
                                           c("D_mtry4", "A_mtry4"), c("E_mtry4", "A_mtry4")),
  `Limit features at mtry 3 or 4 against the shipped setting (A, mtry 2)` = list(c("D_mtry3", "A"), c("E_mtry3", "A"), c("D_mtry4", "A"), c("E_mtry4", "A")),
  `Tier 1: the six light state options` = list(c("T1", "A")))
say(""); say("## 2. Paired by state")
for (sec in names(CMP)) {
  rows <- Filter(function(p) have(p), CMP[[sec]]); if (!length(rows)) next
  say(""); say("### %s", sec)
  say("| arm | minus | budget | cases | errors caught | pooled precision | median | mean | states up / down | worse by > 0.05 / better by > 0.05 | mean dollar recall |")
  say("|---|---|---|---|---|---|---|---|---|---|---|")
  for (p in rows) for (b in c(0.05, 0.10)) for (sc in c("all", "nonpile")) {
    r <- pair(p[1], p[2], b, sc)
    say("| %s | %s | %.0f%% | %s | %+d | %+.4f | %+.4f | %+.4f | %d / %d | %d / %d | %+.4f |", LAB[[p[1]]], p[2], 100 * b, sc, r$errors, r$pooled,
        r$median, r$mean, r$up, r$down, r$harmed, r$helped, r$dollars)
  }
}

sub_tab <- function(states, keep) tab(st %>% filter(arm %in% keep, state %in% states) %>% group_by(arm, budget) %>%
  summarise(states = n(), errors = sum(n_errors_caught), flagged = sum(n_flagged), errors_np = sum(n_errors_np), .groups = "drop") %>%
  group_by(budget) %>% mutate(minus_A = errors - errors[arm == "A"], minus_A_np = errors_np - errors_np[arm == "A"]) %>% ungroup() %>%
  mutate(arm = LAB[arm]) %>% arrange(budget, arm))
if ("A" %in% arms) for (gname in names(REG_GROUPS)) {
  say(""); say("Subgroup (as night 1): %s (%d states). Errors caught, each arm minus A at mtry 2.", gname, length(intersect(REG_GROUPS[[gname]], st$state)))
  sub_tab(REG_GROUPS[[gname]], setdiff(arms, "T1"))
}
if (have("T1", "A")) {
  for (g in list(list(n = "states whose light options in FY2022 differ from some FY2017-19 year", s = T1_CHANGED),
                 list(n = "states whose SMD is below Part B minus $35 in FY2022", s = SMD_BELOW_22))) {
    say(""); say("Subgroup for T1: %s (%d). Errors caught, T1 minus A.", g$n, length(g$s))
    tab(st %>% filter(arm %in% c("A", "T1"), state %in% g$s) %>% group_by(arm, budget) %>%
          summarise(states = n(), errors = sum(n_errors_caught), flagged = sum(n_flagged), errors_np = sum(n_errors_np), .groups = "drop") %>%
          group_by(budget) %>% mutate(minus_A = errors - errors[arm == "A"], minus_A_np = errors_np - errors_np[arm == "A"]) %>% ungroup() %>%
          mutate(arm = LAB[arm]) %>% arrange(budget, arm))
  }
}

say(""); say("## 3. Rules: how often the added features are used, and the regime check")
rl <- lapply(arms, function(a) { fn <- file.path(D, sprintf("rules_%s.rds", a)); if (file.exists(fn)) readRDS(fn) })
names(rl) <- arms; rl <- rl[!vapply(rl, is.null, logical(1))]
fu <- function(r, f) if (f %in% names(r$feature_use)) r$feature_use[[f]] else 0L
LIGHT <- c("smd_gap_dollars", "smd_i", "esap_i", "cap_standard_i", "cap_modified_i", "county_admin_i", "se_other_method_i", "tba_i")
NEWF <- c("dist_to_limit_pts", "bbce_limit_pct", "gross_pov_pct", LIGHT)
added <- function(top) Reduce(`|`, lapply(intersect(paste0("uses_", NEWF), names(top)), function(cn) top[[cn]]), rep(FALSE, nrow(top)))
tab(bind_rows(lapply(rl, function(r) {
  top <- r$rules[seq_len(min(1000, nrow(r$rules))), ]
  data.frame(arm = LAB[[r$tag]], visible = r$n_visible, tagged = r$n_tagged, pile_dropped = r$n_pile,
             uses_dist = fu(r, "dist_to_limit_pts"), uses_limit = fu(r, "bbce_limit_pct"), uses_pov = fu(r, "gross_pov_pct"),
             uses_bbce_flag = fu(r, "bbce_state_i"), top1000_added = sum(added(top)),
             top1000_test_precision = round(sum(top$k_te) / sum(top$n_te), 4),
             regime_limited = nrow(r$regime), regime_below_own = sum(r$regime$lcb_below_regime_rate),
             regime_below_own_top1000 = sum(r$regime$lcb_below_regime_rate & r$regime$rank <= 1000))
})))
say("uses_* count visible rules with a condition on that feature. regime_below_own: regime-limited rules whose 99%% bound is at or below their own regime's training error rate in their household-size group.")
lt <- rl[vapply(rl, function(r) sum(vapply(LIGHT, function(f) fu(r, f), numeric(1))) > 0, logical(1))]
if (length(lt)) {
  say(""); say("Light options: visible rules using each.")
  tab(bind_rows(lapply(lt, function(r) data.frame(arm = LAB[[r$tag]], t(vapply(LIGHT, function(f) fu(r, f), numeric(1)))))))
}
say(""); say("Top 1,000 rules on FY2022, those using an added feature against the rest: flag-weighted test precision.")
tab(bind_rows(lapply(rl, function(r) { top <- r$rules[seq_len(min(1000, nrow(r$rules))), ]; u <- added(top)
  data.frame(arm = LAB[[r$tag]], rules = sum(u), test_precision_added = if (any(u)) round(sum(top$k_te[u]) / sum(top$n_te[u]), 4) else NA,
             test_precision_other = round(sum(top$k_te[!u]) / sum(top$n_te[!u]), 4)) })))
writeLines(out, file.path(D, "readout_night2.md"))
