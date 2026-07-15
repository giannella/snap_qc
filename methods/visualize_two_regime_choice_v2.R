# Best performance per state under the TWO-REGIME rule (modeling_findings
# #9/#14): deploy the national rules by default; switch to the state's own
# mined rules where they beat the national list. One point per state = the
# chosen regime, from the deployment benchmark (train 2022-23, test 2024,
# budget-filled at the same review volume).
#
# HONESTY NOTE: the regime is chosen by 2024 precision -- a best-of-two pick
# on the test year, so medians are mildly flattered. A state would choose
# via its own validation year; the chart is the menu's upper bound.
#
# Output: two_regime_best_budget05/10.png

suppressMessages({library(dplyr); library(ggplot2); library(tidyr)})
source("rule_mining_helpers.R")
out <- "methods/state_similarity_v2/transfer_benchmark_train2223_test24"

d <- bind_rows(
  read.csv(file.path(out, "deployment_menu_train2223_test24.csv"), stringsAsFactors = FALSE),
  read.csv(file.path(out, "deployment_menu_workshop_extension.csv"), stringsAsFactors = FALSE)) %>%
  filter(approach %in% c("national_all", "own_state")) %>%
  distinct(target, approach, budget, .keep_all = TRUE) %>%
  mutate(regime = ifelse(approach == "national_all",
                         "national rules", "state's own rules"))

wilson_ci <- function(k, n, z = 1.96) {
  p <- k / n; dd <- 1 + z^2 / n
  c_ <- (p + z^2 / (2 * n)) / dd
  h  <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / dd
  list(lo = c_ - h, hi = c_ + h)
}

make_chart <- function(budget_val, fname, title_pct) {
  best <- d %>%
    filter(budget == budget_val) %>%
    group_by(target) %>%
    slice_max(precision, n = 1, with_ties = FALSE) %>%
    ungroup()
  ci <- wilson_ci(round(best$precision * best$n_flagged), best$n_flagged)
  best$lo <- ci$lo; best$hi <- ci$hi
  ord <- rev(sort(unique(best$target)))

  dd <- best %>%
    select(target, regime, precision, `share of error $ caught` = dollar_recall,
           target_base_rate, lo, hi) %>%
    pivot_longer(c(precision, `share of error $ caught`), names_to = "metric") %>%
    mutate(metric = factor(metric, levels = c("precision", "share of error $ caught")),
           target = factor(target, levels = ord))
  prec_rows <- dd %>% filter(metric == "precision")

  p <- ggplot(dd, aes(x = value, y = target)) +
    geom_point(data = prec_rows, aes(x = target_base_rate),
               shape = 1, size = 2.2, colour = "grey45") +
    geom_linerange(data = prec_rows, aes(xmin = lo, xmax = hi), linewidth = 0.4) +
    geom_point(aes(shape = regime), size = 2.3, fill = "black") +
    scale_shape_manual(values = c("national rules" = 16,
                                  "state's own rules" = 24),
                       name = NULL) +
    facet_wrap(~metric, nrow = 1, scales = "free_x") +
    labs(x = sprintf("at a %s review budget, on the state's 2024 cases (95%% interval on precision)", title_pct),
         y = NULL,
         title = sprintf("The two-regime rule at a %s budget", title_pct),
         subtitle = "each state's better option: national rules by default, its own\nrules where they win; open circle = state base error rate") +
    theme_minimal(base_size = 12.5) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position = "top")
  save_png(p, file.path(out, fname), 6.6, 5.8)
  invisible(best)
}
b10 <- make_chart(0.10, "two_regime_best_budget10.png", "10%")
b05 <- make_chart(0.05, "two_regime_best_budget05.png", "5%")
cat("own-rules states at 10%:",
    paste(b10$target[b10$regime == "state's own rules"], collapse = ", "), "\n")
cat("own-rules states at  5%:",
    paste(b05$target[b05$regime == "state's own rules"], collapse = ", "), "\n")
cat("median chosen precision: 5% =", median(b05$precision),
    "| 10% =", median(b10$precision), "\n")
cat("wrote two-regime charts to", out, "\n")
