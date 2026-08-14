# Charts from the v2.5.0 recipe benchmark (train FY2022-23, walk each
# state's FY2024; methods/v250_benchmark_2024_v2.R; findings section 39).
# The per-state dotplot is the successor to the deck's
# deploy_national_dotplot_* figures (which drew the 12-state deployment
# menu); these draw all 49 states from the release benchmark, and the
# lists here are the blended recipe (state + national pools on one scale).
#
# Charts (into methods/v250_benchmark_2024/):
#   v250_state_dotplot_budget05/10.png  precision per state, 95% interval,
#                                       open circle = state 2024 base rate
#
# Reads methods/v250_benchmark_2024/v250_benchmark_2024.json

suppressMessages({library(dplyr); library(ggplot2)})
source("rule_mining_helpers.R")
out <- "methods/v250_benchmark_2024"

d <- jsonlite::fromJSON(file.path(out, "v250_benchmark_2024.json"))$records

wilson_ci <- function(k, n, z = 1.96) {
  p <- k / n; dd <- 1 + z^2 / n
  c_ <- (p + z^2 / (2 * n)) / dd
  h  <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / dd
  list(lo = pmax(0, c_ - h), hi = pmin(1, c_ + h))
}

make_dotplot <- function(budget_val, fname, title_pct) {
  dn <- d %>% filter(budget == budget_val)
  ci <- wilson_ci(dn$n_errors_caught, dn$n_flagged)
  dn$lo <- ci$lo; dn$hi <- ci$hi
  med <- median(dn$precision)

  p <- ggplot(dn, aes(x = precision,
                      y = factor(state, levels = rev(sort(unique(state)))))) +
    geom_vline(xintercept = med, colour = "grey80", linetype = "dashed") +
    geom_point(aes(x = base_rate_te), shape = 1, size = 2.0, colour = "grey45") +
    geom_pointrange(aes(xmin = lo, xmax = hi), size = 0.28) +
    labs(x = sprintf("precision at a %s review budget (95%% interval)\nopen circle = state 2024 base error rate; dashed line = median state (%.3f)",
                     title_pct, med),
         y = NULL,
         title = sprintf("The delivered lists at a %s review budget,\ntested on 2024", title_pct),
         subtitle = "v2.5.0 blended lists (state + national rules) mined on\n2022-23 only, scored on the state's 2024 cases; everything\nright of the open circle is lift over random review") +
    expand_limits(x = 0) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank())
  save_png(p, file.path(out, fname), 7.2, 9.4)
}
make_dotplot(0.05, "v250_state_dotplot_budget05.png", "5%")
make_dotplot(0.10, "v250_state_dotplot_budget10.png", "10%")

cat("wrote v2.5.0 benchmark state dotplots to", out, "\n")
