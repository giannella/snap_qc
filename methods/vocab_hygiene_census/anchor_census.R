# Anchor-quality census (descriptive, 2026-08-21): mode-of-utilities vs
# max-of-utilities as the state-year anchor for a relative/tier encoding.
suppressMessages(library(dplyr))
d <- readRDS("reg_model_data.rds") %>%
  filter(as.character(fiscal_year) %in%
           c("2017", "2018", "2019", "2022", "2023", "2024"),
         utilities > 0)
a <- d %>%
  group_by(state = as.character(state), fy = as.character(fiscal_year)) %>%
  summarise(n = n(),
            mx = max(utilities),
            md = as.numeric(names(sort(table(round(utilities)),
                                       decreasing = TRUE))[1]),
            mode_share = max(table(round(utilities))) / n(),
            n_distinct = n_distinct(round(utilities)), .groups = "drop")
cat("state-years:", nrow(a), "\n")
cat("mode == max (within $1):", sum(abs(a$mx - a$md) < 1), "of", nrow(a), "\n")
cat("mode share quartiles:",
    paste(round(quantile(a$mode_share, c(.25, .5, .75)), 2), collapse = " / "),
    "\n")
g <- a %>% arrange(state, fy) %>% group_by(state) %>%
  mutate(r_max = mx / lag(mx), r_md = md / lag(md)) %>%
  ungroup() %>%
  filter(!is.na(r_max), fy %in% c("2018", "2019", "2023", "2024"))
cat("year-over-year anchor log-ratio sd: max", round(sd(log(g$r_max)), 3),
    "| mode", round(sd(log(g$r_md)), 3), "\n")
cat("anchor moved >20% year-over-year: max", sum(abs(log(g$r_max)) > log(1.2)),
    "| mode", sum(abs(log(g$r_md)) > log(1.2)), "of", nrow(g), "\n")
# split by tier-like vs many-valued state-years
a$tiered <- a$n_distinct <= 10
for (t in c(TRUE, FALSE)) {
  s <- a[a$tiered == t, ]
  cat(sprintf("%s state-years (n=%d): mode==max %d (%.0f%%), median mode share %.2f\n",
              ifelse(t, "tier-like (<=10 vals)", "many-valued"), nrow(s),
              sum(abs(s$mx - s$md) < 1), 100 * mean(abs(s$mx - s$md) < 1),
              median(s$mode_share)))
}
write.csv(a, "methods/vocab_hygiene_census/utilities_anchor_census.csv",
          row.names = FALSE)
cat("written: methods/vocab_hygiene_census/utilities_anchor_census.csv\n")
