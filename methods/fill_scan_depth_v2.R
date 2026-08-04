# How far down the ranked pool does the fill actually reach?
#
# A delivered list is built by walking the ranked pool from the top and taking a
# rule whenever it adds cases nobody above it already flagged, until the review
# budget is full (those rules are the "core") and then out to three times that
# depth (the "buffer", substitutes a state can swap in). The rules it SKIPS on
# the way down are ones that add nothing new. So the number of rules delivered
# and the number of rules examined are different quantities, and only the second
# tells you how much of the pool the pipeline needs.
#
# This measures the second. For each delivered list, take the lowest 99% Wilson
# bound among the rules delivered, then count how many rules in that state's
# blended pool rank at or above it. That count is the scan depth.
#
# Reads the delivered lists and the pool caches; evaluates no rules, runs in
# about a minute. Writes methods/anyerror_blended_holdout_2024/fill_scan_depth.csv
suppressMessages({ library(dplyr); library(jsonlite) })
POOL <- "methods/delivery_pools_2022_2024_v3"
OUT  <- "methods/anyerror_blended_holdout_2024/fill_scan_depth.csv"

natl <- readRDS(file.path(POOL, "pool_national_anyerror_fdr10.rds"))
natl$pool <- "national"

rows <- list()
for (f in list.files("state_delivery_lists", pattern = "budget(05|10)\\.csv$",
                     full.names = TRUE)) {
  state <- sub("^blended_delivery_(.*)_2022_2024_budget(05|10)\\.csv$", "\\1", basename(f))
  bud   <- ifelse(grepl("budget05", f), 5, 10)
  d <- read.csv(f, stringsAsFactors = FALSE)
  key <- gsub("[^A-Za-z]", "", gsub("_", " ", state))
  pf  <- file.path(POOL, sprintf("pool_%s_anyerror_fdr10.rds", key))
  own <- if (file.exists(pf)) { o <- readRDS(pf); o$pool <- "state"; o } else NULL
  pool <- bind_rows(natl, own) %>%
    arrange(desc(precision_train_lcb), hh, rule) %>%
    distinct(hh, rule, .keep_all = TRUE)
  rows[[length(rows) + 1]] <- data.frame(
    state = gsub("_", " ", state), budget = bud, pool_size = nrow(pool),
    n_core = sum(d$role == "core"), n_delivered = nrow(d),
    depth_core = sum(pool$precision_train_lcb >= min(d$precision_train_lcb[d$role == "core"])),
    depth_delivered = sum(pool$precision_train_lcb >= min(d$precision_train_lcb)),
    # how wide the delivered rules are: a list built from rules that each flag
    # many cases fills its budget in few rules, one built from narrow rules needs
    # more of them, and needs to look at more to find them
    med_cases_per_rule = median(d$n_flagged_state),
    med_new_per_rule   = median(d$n_new_at_rank),
    stringsAsFactors = FALSE)
}
r <- bind_rows(rows) %>% mutate(depth_share = round(depth_delivered / pool_size, 4))

# is a deep scan a symptom of a list that struggles to fill its budget? the
# scorecard carries the frozen-core fill ratio, so the two can be compared
sc <- bind_rows(lapply(readLines("methods/anyerror_blended_holdout_2024/holdout_metrics.jsonl",
                                 warn = FALSE), function(l) as.data.frame(fromJSON(l))))
sc <- sc %>% transmute(state, budget = budget_pct,
                       fill = flagged_share_of_caseload / (budget_pct / 100))
r <- r %>% left_join(sc, by = c("state", "budget"))

for (b in c(5, 10)) {
  s <- r %>% filter(budget == b)
  cat(sprintf("=== budget %2d%% (%d states) ===\n", b, nrow(s)))
  cat(sprintf("  pool size                 median %6.0f\n", median(s$pool_size)))
  cat(sprintf("  rules delivered           median %6.0f (core %.0f)\n",
              median(s$n_delivered), median(s$n_core)))
  cat(sprintf("  scan depth, core only     median %6.0f  max %6.0f\n",
              median(s$depth_core), max(s$depth_core)))
  cat(sprintf("  scan depth, core + buffer median %6.0f  max %6.0f  (%.1f%% of the pool at the max)\n",
              median(s$depth_delivered), max(s$depth_delivered),
              100 * max(s$depth_share)))
  cat(sprintf("  states past rank 1000: %d of %d | past 5000: %d | past 20000: %d\n",
              sum(s$depth_delivered > 1000), nrow(s), sum(s$depth_delivered > 5000),
              sum(s$depth_delivered > 20000)))
  cat(sprintf("  deepest five: %s\n", paste(sprintf("%s %.0f",
      head(s$state[order(-s$depth_delivered)], 5),
      head(sort(s$depth_delivered, decreasing = TRUE), 5)), collapse = "; ")))
  cat(sprintf("  correlation of scan depth with the frozen-core fill ratio: %+.2f\n",
              cor(s$depth_delivered, s$fill, use = "complete.obs")))
  cat(sprintf("  correlation of scan depth with rule width: %+.2f (cases flagged), %+.2f (new cases added)\n\n",
              cor(s$depth_delivered, s$med_cases_per_rule, use = "complete.obs"),
              cor(s$depth_delivered, s$med_new_per_rule, use = "complete.obs")))
}
write.csv(r, OUT, row.names = FALSE)
cat(sprintf("wrote %s\n", OUT))
