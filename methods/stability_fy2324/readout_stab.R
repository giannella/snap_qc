# Year-to-year stability check, the readout (2026-11-11). Eric: "We're just trying to see if there's consistency in
# what works for states over time." Joins the national rule lists (rules_stab.R: national list, pile gate on, kept
# cases) and the vote score (votes_stab.R), both trained on FY2017-19 + FY2022 and scored on FY2023 and FY2024.
# Stops unless both sit on the same kept cases, errors, error dollars and flag counts in every state-year-budget.
# For each state, year and budget, the gap = rules minus votes, in times-random terms:
#   cases    (errors caught / flags) / (kept errors / kept cases), flags = the shared flag count
#   dollars  (error dollars caught / kept error dollars) / budget
# The question is whether a state's FY2023 gap predicts its FY2024 gap: across states, the Pearson and Spearman
# correlations, a one-sided permutation p-value for the Pearson correlation (10,000 shuffles, seed 117), and how
# often the same method wins in both years (ties dropped). Pooled national lifts by year are printed for context.
#   [SMOKE=1] "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" methods/stability_fy2324/readout_stab.R
# Writes methods/stability_fy2324[/smoke]/stability_by_state.csv and stability_summary.csv.
setwd("C:/Users/ericg/snap_qc_v27")
SMOKE_RUN <- identical(Sys.getenv("SMOKE"), "1")
OUT <- if (SMOKE_RUN) "methods/stability_fy2324/smoke" else "methods/stability_fy2324"
r <- read.csv(file.path(OUT, sprintf("gate_scores_w1719_%s.csv", if (SMOKE_RUN) "rules_smoke" else "rules")))
r <- r[r$list == "national" & r$gate == "on" & r$scoring == "kept", ]
v <- read.csv(file.path(OUT, "votes_by_state.csv"))
r$year <- as.character(r$test_year); v$year <- as.character(v$year)
m <- merge(r, v, by = c("state", "year", "budget"), suffixes = c(".r", ".v"))
stopifnot(nrow(m) == nrow(r), nrow(m) == if (SMOKE_RUN) nrow(m) else nrow(v),
          all(m$n_te == m$kept), all(m$n_err_te == m$errors_all), all(abs(m$dollars_total - m$dollars_all) < 0.005),
          all(m$n_cap == m$flags), all(m$n_flagged <= m$n_cap))
rate <- m$errors_all / m$kept
m$cases_r <- (m$n_errors / m$flags) / rate; m$cases_v <- (m$errors_caught / m$flags) / rate
m$doll_r <- m$dollars_caught.r / m$dollars_all / m$budget; m$doll_v <- m$dollars_caught.v / m$dollars_all / m$budget
m$gap_cases <- m$cases_r - m$cases_v; m$gap_doll <- m$doll_r - m$doll_v
m$gap_errors <- m$n_errors - m$errors_caught
keep_cols <- c("state", "year", "budget", "cases", "kept", "flags", "n_flagged", "errors_all", "n_errors", "errors_caught",
               "dollars_all", "dollars_caught.r", "dollars_caught.v", "cases_r", "cases_v", "doll_r", "doll_v",
               "gap_errors", "gap_cases", "gap_doll")
by_state <- m[order(m$budget, m$state, m$year), keep_cols]
write.csv(by_state, file.path(OUT, "stability_by_state.csv"), row.names = FALSE)

set.seed(117)
summ <- list()
for (b in sort(unique(m$budget))) {
  x <- m[m$budget == b, ]
  cat(sprintf("\n== %g%% budget ==\n", 100 * b))
  for (y in sort(unique(x$year))) {
    z <- x[x$year == y, ]
    cat(sprintf("  FY%s pooled: cases rules %.2fx, votes %.2fx | dollars rules %.2fx, votes %.2fx | errors caught rules %d, votes %d\n",
                y, (sum(z$n_errors) / sum(z$flags)) / (sum(z$errors_all) / sum(z$kept)),
                (sum(z$errors_caught) / sum(z$flags)) / (sum(z$errors_all) / sum(z$kept)),
                sum(z$dollars_caught.r) / sum(z$dollars_all) / b, sum(z$dollars_caught.v) / sum(z$dollars_all) / b,
                sum(z$n_errors), sum(z$errors_caught)))
  }
  w <- reshape(x[, c("state", "year", "gap_cases", "gap_doll")], idvar = "state", timevar = "year", direction = "wide")
  w <- w[complete.cases(w), ]
  for (g in c("gap_cases", "gap_doll")) {
    a <- w[[paste0(g, ".2023")]]; c2 <- w[[paste0(g, ".2024")]]
    rp <- cor(a, c2); rs <- cor(a, c2, method = "spearman")
    perm <- replicate(10000, cor(a, sample(c2))); p <- mean(perm >= rp)
    nz <- a != 0 & c2 != 0; agree <- sum(sign(a[nz]) == sign(c2[nz]))
    cat(sprintf("  %s: FY2023 vs FY2024 gap across %d states: Pearson %+.2f (one-sided permutation p = %.3f), Spearman %+.2f; same winner both years in %d of %d states\n",
                if (g == "gap_cases") "cases  " else "dollars", nrow(w), rp, p, rs, agree, sum(nz)))
    summ[[length(summ) + 1]] <- data.frame(budget = b, measure = sub("gap_", "", g), states = nrow(w), pearson = rp,
                                           perm_p = p, spearman = rs, same_winner = agree, decided = sum(nz))
  }
}
write.csv(do.call(rbind, summ), file.path(OUT, "stability_summary.csv"), row.names = FALSE)
cat("\nwrote", file.path(OUT, "stability_by_state.csv"), "and stability_summary.csv\n")
