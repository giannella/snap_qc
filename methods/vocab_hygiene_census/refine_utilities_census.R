# Refinement of the 2026-08-21 utilities census (descriptive, no modeling):
# the original drift count treated every state's distinct positive utilities
# values as SUA tiers, which overstates drift in states where utilities is
# genuinely continuous (actual-cost states). Here a state-year is TIER-VALUED
# when it carries at most MAX_TIERS distinct positive values; a state is a
# tier state when all three FY2022-24 years are tier-valued. The drift count
# is then recomputed within tier states only.
suppressMessages(library(dplyr))

MAX_TIERS <- 6
OUT_DIR <- "methods/vocab_hygiene_census"

d <- readRDS("reg_model_data.rds")
tiers <- d %>%
  filter(as.character(fiscal_year) %in% c("2022", "2023", "2024"),
         utilities > 0) %>%
  group_by(state = as.character(state), fy = as.character(fiscal_year)) %>%
  summarise(n_distinct_vals = n_distinct(round(utilities)),
            vals = list(sort(unique(round(utilities)))), .groups = "drop")

state_class <- tiers %>%
  group_by(state) %>%
  summarise(max_distinct = max(n_distinct_vals),
            tier_state = all(n_distinct_vals <= MAX_TIERS), .groups = "drop")
cat(sprintf("tier states (<= %d distinct positive utilities values in every "
            , MAX_TIERS))
cat(sprintf("FY22-24 year): %d of %d\n",
            sum(state_class$tier_state), nrow(state_class)))
write.csv(state_class %>% arrange(desc(max_distinct)),
          file.path(OUT_DIR, "utilities_state_tier_classes.csv"),
          row.names = FALSE)

u <- read.csv(file.path(OUT_DIR, "utilities_rules_delivered.csv"),
              stringsAsFactors = FALSE) %>%
  left_join(state_class, by = "state")
cat(sprintf("\ndelivered utilities rules: %d total\n", nrow(u)))
cat(sprintf("  in TIER states:        %4d, drift-sensitive %4d (%.0f%%)\n",
            sum(u$tier_state), sum(u$drift & u$tier_state),
            100 * mean(u$drift[u$tier_state])))
cat(sprintf("  in CONTINUOUS states:  %4d, drift-flagged  %4d (%.0f%%; the "
            , sum(!u$tier_state), sum(u$drift & !u$tier_state),
            100 * mean(u$drift[!u$tier_state])))
cat("tier construct does not apply there)\n")
write.csv(u, file.path(OUT_DIR, "utilities_rules_delivered_refined.csv"),
          row.names = FALSE)
