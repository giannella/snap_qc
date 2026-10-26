# BBCE-limit feature exploration, night 3 (2026-10-24): readout over whichever
# runs finished. Three seeds (117-119) of four cells, arms A (the shipped 19
# features) and E (+ income as a percent of poverty + the BBCE limit) at mtry 2
# and mtry 3, ranger alone, in two windows: mine FY2017-19 and score FY2022
# (three years ahead, the primary window), and mine FY2022-23 and score FY2024
# (one year ahead). Questions (see design_note.md, "Night 3"):
#   1. Does E at mtry 3 beat what ships (A at mtry 2)?
#   2. Is that mtry, the features, or both? (the 2 x 2 of A/E by mtry 2/3)
# Seeds do not pair across cells, so every contrast is a difference of seed
# means with its standard error from the seed-to-seed spread (two degrees of
# freedom per cell: a rough yardstick).
#   Rscript methods/bbce_limit_features/readout_night3.R
suppressMessages(library(dplyr))
options(width = 220)
setwd(Sys.getenv("SR_ROOT", "C:/Users/ericg/snap_qc_v27"))
SMOKE <- identical(Sys.getenv("SMOKE"), "1")
BASE <- if (SMOKE) "methods/bbce_limit_features/smoke" else "methods/bbce_limit_features"
WDIR <- c(fy2022 = BASE, fy2024 = file.path(BASE, "fy2024"))
WLAB <- c(fy2022 = "mine FY2017-19, score FY2022 (three years ahead)", fy2024 = "mine FY2022-23, score FY2024 (one year ahead)")
CELLS <- c(A2 = "A", A3 = "A_mtry3", E2 = "E", E3 = "E_mtry3")
CLAB <- c(A2 = "A, mtry 2 (ships)", A3 = "A, mtry 3", E2 = "E, mtry 2", E3 = "E, mtry 3")
SEEDS <- 117:119
tag <- function(cell, s) paste0(CELLS[[cell]], if (s == 117L) "" else sprintf("_seed%d", s))
runs <- expand.grid(window = names(WDIR), cell = names(CELLS), seed = SEEDS, stringsAsFactors = FALSE)
runs$tag <- mapply(tag, runs$cell, runs$seed)
runs$fn <- file.path(WDIR[runs$window], sprintf("states_%s.csv", runs$tag))
runs <- runs[file.exists(runs$fn), ]

out <- character(0); say <- function(...) { s <- sprintf(...); cat(s, "\n", sep = ""); out <<- c(out, s) }
tab <- function(df) { x <- capture.output(print(as.data.frame(df), row.names = FALSE)); cat(x, sep = "\n"); out <<- c(out, "```", x, "```") }
say("# BBCE-limit features, night 3: three seeds of A and E at mtry 2 and 3, two windows%s", if (SMOKE) " [SMOKE]" else "")
say("Generated %s. Ranger alone (1000 trees, depth 4). National pool only, mismatch-tagged and income-pile-tagged rules dropped, the state lists walked with the shipped fill and scored at 5%% and 10%% budgets. A = the shipped 19 features; E = A + income as a percent of poverty + the state's BBCE gross limit. Runs on disk: %d of %d.",
    format(Sys.time(), "%Y-%m-%d %H:%M"), nrow(runs), length(WDIR) * length(CELLS) * length(SEEDS))
if (!nrow(runs)) { writeLines(out, file.path(BASE, "readout_night3.md")); quit(save = "no") }
st <- bind_rows(lapply(seq_len(nrow(runs)), function(i) {
  x <- read.csv(runs$fn[i]); x$window <- runs$window[i]; x$cell <- runs$cell[i]; x$seed <- runs$seed[i]; x }))

say(""); say("## 0. How to read this")
say("Each cell is three independent ranger draws (seeds 117, 118, 119). A contrast is the difference of two cells' seed means; its standard error comes from the spread of the three seeds in each cell, so with two degrees of freedom per cell it is a rough yardstick, not a test.")
say("A contrast holds in a window when, at both budgets, the seed-mean gain in errors caught is positive and larger than twice its standard error (the larger of the cells' own and the within-mtry pooled one), the gain on non-pile cases is positive, the median and mean of the state-by-state differences (each state's precision averaged over its three seeds) are positive, and no more states are worse by more than 0.05 than are better by that much. A change goes on to the xgboost + ranger test only if it holds in both windows and its FY2022 gain on seeds 118-119 alone is positive at both budgets.")
say("Selection: night 2 picked E at mtry 3 as the lead because its seed-117 draw was the best of seven arms against A at mtry 2. That draw is one of the three FY2022 seeds here, so the FY2022 three-seed reading carries some of its luck. The FY2022 reading on seeds 118-119 leaves it out, and the FY2024 window is wholly new: it is the only reading free of the selection.")

## 1. every run
say(""); say("## 1. Every run, pooled over the state lists")
per_run <- st %>% group_by(window, cell, seed, budget) %>%
  summarise(states = n(), errors = sum(n_errors_caught), flagged = sum(n_flagged),
            pooled_precision = round(sum(n_errors_caught) / sum(n_flagged), 4), errors_np = sum(n_errors_np),
            median_state = round(median(precision), 4), mean_state = round(mean(precision), 4),
            mean_dollar_recall = round(mean(dollar_recall), 4), visible = first(n_visible),
            pile_dropped = first(n_pile_tagged), .groups = "drop") %>%
  mutate(cell = CLAB[cell]) %>% arrange(window, budget, cell, seed)
for (w in names(WDIR)) if (any(per_run$window == w)) { say(""); say("%s", WLAB[[w]]); tab(per_run %>% filter(window == w) %>% select(-window)) }

## 2. cells: seed means and spread
say(""); say("## 2. Cells: seed mean and range of errors caught")
cell_sum <- st %>% group_by(window, cell, seed, budget) %>%
  summarise(e = sum(n_errors_caught), enp = sum(n_errors_np), .groups = "drop") %>%
  group_by(window, cell, budget) %>%
  summarise(seeds = n(), errors_mean = round(mean(e), 1), errors_sd = round(sd(e), 1), errors_min = min(e), errors_max = max(e),
            errors_np_mean = round(mean(enp), 1), .groups = "drop") %>%
  mutate(cell = CLAB[cell]) %>% arrange(window, budget, cell)
tab(cell_sum)

## 3. contrasts
CON <- list(`E, mtry 3 minus A, mtry 2 (question 1)` = list(plus = "E3", minus = "A2"),
            `A, mtry 3 minus A, mtry 2 (mtry alone)` = list(plus = "A3", minus = "A2"),
            `E, mtry 2 minus A, mtry 2 (features at mtry 2)` = list(plus = "E2", minus = "A2"),
            `E, mtry 3 minus A, mtry 3 (features at mtry 3)` = list(plus = "E3", minus = "A3"),
            `interaction: (E3 - A3) minus (E2 - A2)` = list(plus = c("E3", "A2"), minus = c("A3", "E2")))
# the readings: all three seeds in each window; in the FY2022 window also seeds
# 118-119 alone, which leave out nights 1-2's seed-117 runs (E at mtry 3, seed
# 117 is the draw that suggested this night's question)
READINGS <- list(fy2022 = list(`3 seeds` = SEEDS, `seeds 118-119` = c(118L, 119L)), fy2024 = list(`3 seeds` = SEEDS))
MTRY_OF <- c(A2 = 2L, A3 = 3L, E2 = 2L, E3 = 3L)
tot <- st %>% group_by(window, cell, seed, budget) %>% summarise(e = sum(n_errors_caught), enp = sum(n_errors_np), .groups = "drop")
sel <- function(w, b, c, seeds, col) tot[[col]][tot$window == w & tot$budget == b & tot$cell == c & tot$seed %in% seeds]
smean <- function(w, b, cells, seeds, col) sum(vapply(cells, function(c) mean(sel(w, b, c, seeds, col)), numeric(1)))
# SE two ways: from each cell's own seeds; and from the seed variance pooled
# over the A and E cells at the same mtry (seed noise differs by mtry: night
# 2's second seed moved 6 / 9 at mtry 2 and 20 / 44 at mtry 4). The larger is used.
se_own <- function(w, b, cells, seeds) sqrt(sum(vapply(cells, function(c) {
  z <- sel(w, b, c, seeds, "e"); if (length(z) < 2) NA_real_ else var(z) / length(z) }, numeric(1))))
pooled_var <- function(w, b, c, seeds) {
  zs <- lapply(names(MTRY_OF)[MTRY_OF == MTRY_OF[[c]]], function(x) sel(w, b, x, seeds, "e"))
  ns <- vapply(zs, length, integer(1)); if (any(ns < 2)) return(NA_real_)
  sum(vapply(zs, function(z) sum((z - mean(z))^2), numeric(1))) / sum(ns - 1L) }
se_pool <- function(w, b, cells, seeds) sqrt(sum(vapply(cells, function(c) pooled_var(w, b, c, seeds) / length(sel(w, b, c, seeds, "e")), numeric(1))))
# per state: precision and dollar recall averaged over the reading's seeds
state_diff <- function(w, b, plus, minus, seeds, col) {
  g <- function(c) st %>% filter(window == w, budget == b, cell == c, seed %in% seeds) %>% group_by(state) %>%
    summarise(p = mean(precision), pnp = mean(precision_np), d = mean(dollar_recall), .groups = "drop") %>% arrange(state)
  parts <- lapply(c(plus, minus), g); stopifnot(all(vapply(parts, function(x) identical(x$state, parts[[1]]$state), logical(1))))
  Reduce(`+`, lapply(parts[seq_along(plus)], `[[`, col)) - Reduce(`+`, lapply(parts[length(plus) + seq_along(minus)], `[[`, col))
}
say(""); say("## 3. Contrasts")
say("gain = difference of seed means in errors caught; se_cell from each cell's own seeds, se_pooled from the seed variance pooled over the A and E cells at the same mtry; the rule uses the larger. State columns use each state's precision (and dollar recall) averaged over the reading's seeds. pass = the reading rule in section 0 at that budget (NA for the interaction, which is descriptive); median_zero_only = the budget fails only because the median state change is exactly 0.")
crow <- list()
for (w in names(WDIR)) for (rd in names(READINGS[[w]])) for (cn in names(CON)) {
  sds <- READINGS[[w]][[rd]]; pl <- CON[[cn]]$plus; mi <- CON[[cn]]$minus
  if (!all(vapply(c(pl, mi), function(c) length(sel(w, 0.05, c, sds, "e")) == length(sds), logical(1)))) next
  for (b in c(0.05, 0.10)) {
    g <- smean(w, b, pl, sds, "e") - smean(w, b, mi, sds, "e"); gnp <- smean(w, b, pl, sds, "enp") - smean(w, b, mi, sds, "enp")
    s1 <- se_own(w, b, c(pl, mi), sds); s2 <- se_pool(w, b, c(pl, mi), sds); se <- suppressWarnings(max(s1, s2, na.rm = TRUE))
    d <- state_diff(w, b, pl, mi, sds, "p"); dnp <- state_diff(w, b, pl, mi, sds, "pnp"); dd <- state_diff(w, b, pl, mi, sds, "d")
    crow[[length(crow) + 1]] <- data.frame(window = w, reading = rd, contrast = cn, budget = b, gain = round(g, 1),
      se_cell = round(s1, 1), se_pooled = round(s2, 1), gain_over_2se = is.finite(se) && g > 2 * se, gain_np = round(gnp, 1),
      median_state = round(median(d), 4), mean_state = round(mean(d), 4), median_state_np = round(median(dnp), 4),
      up = sum(d > 0), down = sum(d < 0), worse_05 = sum(d < -0.05), better_05 = sum(d > 0.05), dollar_recall = round(mean(dd), 4))
  }
}
if (length(crow)) {
  cr <- bind_rows(crow)
  judged <- !grepl("^interaction", cr$contrast)
  rest <- cr$gain_over_2se & cr$gain_np > 0 & cr$mean_state > 0 & cr$worse_05 <= cr$better_05
  cr$pass <- ifelse(judged, rest & cr$median_state > 0, NA)
  cr$median_zero_only <- ifelse(judged, rest & cr$median_state == 0, NA)
  for (w in unique(cr$window)) for (rd in unique(cr$reading[cr$window == w])) {
    say(""); say("%s, %s%s", WLAB[[w]], rd, if (rd == "seeds 118-119") " (leaves out the seed-117 runs, among them the draw that suggested the question; one degree of freedom per cell, so read the signs)" else "")
    tab(cr %>% filter(window == w, reading == rd) %>% select(-window, -reading))
  }
  ctr <- names(CON)[!grepl("^interaction", names(CON))]
  verdict <- function(w, rd, what) {
    x <- cr[cr$window == w & cr$reading == rd & cr$contrast %in% ctr, ]
    if (!nrow(x)) return(rep(NA, length(ctr)))
    vapply(ctr, function(cn) { y <- x[x$contrast == cn, ]
      if (nrow(y) != 2) NA else if (what == "pass") all(y$pass) else all(y$gain > 0) }, logical(1))
  }
  hold <- data.frame(contrast = ctr, fy2022_holds = verdict("fy2022", "3 seeds", "pass"),
                     fy2022_seeds_118_119_gain_positive = verdict("fy2022", "seeds 118-119", "gain"),
                     fy2024_holds = verdict("fy2024", "3 seeds", "pass"), row.names = NULL)
  hold$goes_on <- hold$fy2022_holds & hold$fy2022_seeds_118_119_gain_positive & hold$fy2024_holds
  say(""); say("Holds, by window. The FY2022 three-seed reading includes the seed-117 runs, so it carries the selection of the draw that suggested the question; FY2024 is the only reading free of it. A change goes on to the xgboost + ranger test only if it holds in both windows and its FY2022 gain on seeds 118-119 alone is positive at both budgets.")
  tab(hold)
}

## 3b. subgroups: the states where regime-limited rules can lose their flags
bi <- read.csv("additional_data/bbce_income.csv", check.names = FALSE, fileEncoding = "UTF-8-BOM")
lim_of <- function(y) { v <- suppressWarnings(as.numeric(bi[[y]])); ifelse(is.na(v), -130, v) }   # no BBCE coded apart from a 130% limit
WYEARS <- list(fy2022 = list(train = c("2017", "2018", "2019"), test = "2022"), fy2024 = list(train = c("2022", "2023"), test = "2024"))
say(""); say("## 3b. Subgroups: errors caught, seed mean, each cell minus A at mtry 2")
for (w in intersect(names(WYEARS), unique(st$window))) {
  ty <- WYEARS[[w]]$test; tl <- lim_of(ty)
  grp <- list(bi$state_name[abs(tl) <= 130],
              bi$state_name[Reduce(`|`, lapply(WYEARS[[w]]$train, function(y) lim_of(y) != tl))])
  names(grp) <- c(sprintf("minority regimes in FY%s (no BBCE, or BBCE at 130%%)", ty), sprintf("limit or BBCE status in FY%s differs from a training year", ty))
  for (gn in names(grp)) {
    s_in <- intersect(grp[[gn]], unique(st$state[st$window == w]))
    say(""); say("%s: %s (%d states)", WLAB[[w]], gn, length(s_in))
    if (!length(s_in) || !any(st$window == w & st$cell == "A2")) next
    tab(st %>% filter(window == w, state %in% s_in) %>% group_by(cell, seed, budget) %>%
          summarise(e = sum(n_errors_caught), enp = sum(n_errors_np), .groups = "drop") %>% group_by(cell, budget) %>%
          summarise(seeds = n(), errors = round(mean(e), 1), errors_np = round(mean(enp), 1), .groups = "drop") %>%
          group_by(budget) %>% mutate(minus_A2 = errors - errors[cell == "A2"], minus_A2_np = errors_np - errors_np[cell == "A2"]) %>%
          ungroup() %>% mutate(cell = CLAB[cell]) %>% arrange(budget, cell))
  }
}

## 4. rules
say(""); say("## 4. Rules: precision of the top 1,000 on the test year, seed means")
rr <- bind_rows(lapply(seq_len(nrow(runs)), function(i) {
  fn <- file.path(WDIR[runs$window[i]], sprintf("rules_%s.rds", runs$tag[i])); if (!file.exists(fn)) return(NULL)
  r <- readRDS(fn); top <- r$rules[seq_len(min(1000, nrow(r$rules))), ]
  u <- top$uses_gross_pov_pct | top$uses_bbce_limit_pct
  data.frame(window = runs$window[i], cell = runs$cell[i], seed = runs$seed[i], visible = r$n_visible, pile_dropped = r$n_pile,
             top1000_precision = sum(top$k_te) / sum(top$n_te), top1000_added = sum(u),
             added_precision = if (any(u)) sum(top$k_te[u]) / sum(top$n_te[u]) else NA_real_,
             other_precision = sum(top$k_te[!u]) / sum(top$n_te[!u]),
             regime_below_own_top1000 = sum(r$regime$lcb_below_regime_rate & r$regime$rank <= 1000))
}))
if (nrow(rr)) tab(rr %>% group_by(window, cell) %>%
  summarise(seeds = n(), visible = round(mean(visible)), pile_dropped = round(mean(pile_dropped)),
            top1000_precision = round(mean(top1000_precision), 4), top1000_added = round(mean(top1000_added)),
            added_precision = round(mean(added_precision), 4), other_precision = round(mean(other_precision), 4),
            regime_below_own_top1000 = sum(regime_below_own_top1000), .groups = "drop") %>%
  mutate(cell = CLAB[cell]) %>% arrange(window, cell))
say("added / other: the top 1,000 rules using income as a percent of poverty or the BBCE limit, against the rest (flag-weighted test-year precision). regime_below_own_top1000: summed over seeds.")
writeLines(out, file.path(BASE, "readout_night3.md"))
