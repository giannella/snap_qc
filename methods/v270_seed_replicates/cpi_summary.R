# Summary of the v2.7 CPI measurements (2026-10-06): the CPI step alone (final
# frame minus the CPI-off frame, same munging code) and the three ordered
# refinements (09-25 -> 09-28 -> 09-29 -> final), in both windows, from the
# per-seed state lists on disk. Error dollars caught = sum over states of the
# list's dollar recall times the state's test-year error dollars (identical in
# every frame: same cases, same error amounts).
#   Rscript methods/v270_seed_replicates/cpi_summary.R
# Writes methods/v270_seed_replicates/cpi_summary_by_seed.csv and
# cpi_summary_contrasts.csv.
suppressMessages(library(dplyr))
options(width = 220)
setwd("C:/Users/ericg/snap_qc")
W <- list(fy2024 = list(dir = "methods/v270_seed_replicates", test = "2024"),
          fy2022 = list(dir = "methods/v270_seed_replicates/fy2022", test = "2022"))
fr <- readRDS("reg_model_data.rds")
rows <- list()
for (w in names(W)) {
  d <- fr[as.character(fr$fiscal_year) == W[[w]]$test, ]
  ie <- !is.na(d$over_threshold) & d$over_threshold != 0
  D <- tapply(ifelse(ie, abs(ifelse(is.na(d$total_error_amount), 0, d$total_error_amount)), 0), as.character(d$state), sum)
  for (f in c("cpioff", "f0925", "f0928", "f0929", "final")) for (s in c(117, 118, 119)) {
    fn <- file.path(W[[w]]$dir, sprintf("states_%s_seed%d.csv", f, s)); if (!file.exists(fn)) next
    x <- read.csv(fn)
    for (b in c(0.05, 0.10)) { y <- x[x$budget == b, ]
      rows[[length(rows) + 1]] <- data.frame(window = w, frame = f, seed = s, budget = b, states = nrow(y), errors = sum(y$n_errors_caught),
        flagged = sum(y$n_flagged), precision = sum(y$n_errors_caught) / sum(y$n_flagged), error_dollars = sum(y$dollar_recall * D[y$state]),
        all_error_dollars = sum(D[y$state])) } }
}
r <- bind_rows(rows); write.csv(r, "methods/v270_seed_replicates/cpi_summary_by_seed.csv", row.names = FALSE)
pair <- function(w, A, B, b) {
  a <- r[r$window == w & r$frame == A & r$budget == b, ]; z <- r[r$window == w & r$frame == B & r$budget == b, ]
  m <- merge(a, z, by = "seed", suffixes = c("_a", "_b")); if (!nrow(m)) return(NULL)
  data.frame(window = w, before = A, after = B, budget = b, seeds = paste(m$seed, collapse = "/"),
    precision_before = paste(sprintf("%.4f", m$precision_a), collapse = " / "), precision_after = paste(sprintf("%.4f", m$precision_b), collapse = " / "),
    rel_precision_by_seed = paste(sprintf("%+.1f%%", 100 * (m$precision_b / m$precision_a - 1)), collapse = " / "),
    rel_precision_pooled = round(100 * ((sum(m$errors_b) / sum(m$flagged_b)) / (sum(m$errors_a) / sum(m$flagged_a)) - 1), 1),
    d_errors_by_seed = paste(sprintf("%+d", m$errors_b - m$errors_a), collapse = " / "), d_errors_mean = round(mean(m$errors_b - m$errors_a), 1),
    flagged_per_list_set = round(mean(m$flagged_a)),
    dollars_before = paste(sprintf("%.0f", m$error_dollars_a), collapse = " / "), dollars_after = paste(sprintf("%.0f", m$error_dollars_b), collapse = " / "),
    rel_dollars_by_seed = paste(sprintf("%+.1f%%", 100 * (m$error_dollars_b / m$error_dollars_a - 1)), collapse = " / "),
    rel_dollars_pooled = round(100 * (sum(m$error_dollars_b) / sum(m$error_dollars_a) - 1), 1),
    share_dollars_before = round(mean(m$error_dollars_a / m$all_error_dollars_a), 3), share_dollars_after = round(mean(m$error_dollars_b / m$all_error_dollars_b), 3))
}
cs <- bind_rows(lapply(names(W), function(w) bind_rows(lapply(c(0.05, 0.10), function(b) bind_rows(
  pair(w, "cpioff", "final", b), pair(w, "f0925", "f0928", b), pair(w, "f0928", "f0929", b), pair(w, "f0929", "final", b), pair(w, "f0925", "final", b))))))
write.csv(cs, "methods/v270_seed_replicates/cpi_summary_contrasts.csv", row.names = FALSE)
print(cs %>% select(window, before, after, budget, seeds, precision_before, precision_after, rel_precision_pooled, d_errors_by_seed, d_errors_mean, flagged_per_list_set), row.names = FALSE)
cat("\n"); print(cs %>% filter(before == "cpioff") %>% select(window, budget, dollars_before, dollars_after, rel_dollars_by_seed, rel_dollars_pooled, share_dollars_before, share_dollars_after), row.names = FALSE)
cat("\nseed spread (range of errors caught across three seeds of one frame):\n")
print(r %>% group_by(window, budget, frame) %>% filter(n() == 3) %>% summarise(range_errors = max(errors) - min(errors), .groups = "drop") %>% as.data.frame(), row.names = FALSE)
cat("\nall FY test-year error dollars in the 49-state sample:", paste(sprintf("%s $%s", names(W), sapply(names(W), function(w) format(round(r$all_error_dollars[r$window == w][1]), big.mark = ","))), collapse = "; "), "\n")
