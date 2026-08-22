# Descriptive (2026-08-21): mass of positive-utilities cases in the band
# [mode - 200, mode) per state-year, to decide whether the SUA tier's HIGH
# threshold (utilities >= mode - 200) can be applied uniformly or must be
# targeted at multi-schedule states (VA/TN hh-size variants, NY regions).
suppressMessages(library(dplyr))
d <- readRDS("reg_model_data.rds") %>%
  filter(as.character(fiscal_year) %in% c("2022", "2023", "2024"),
         utilities > 0)
a <- d %>%
  group_by(state = as.character(state), fy = as.character(fiscal_year)) %>%
  summarise(n = n(),
            md = as.numeric(names(sort(table(round(utilities)),
                                       decreasing = TRUE))[1]),
            share_at_or_above_mode = mean(utilities >= md),
            share_in_band = mean(utilities >= md - 200 & utilities < md),
            .groups = "drop")
cat("state-years:", nrow(a), "\n")
cat("share_in_band quartiles:",
    paste(round(quantile(a$share_in_band, c(.25, .5, .75, .95)), 3),
          collapse = " / "), "\n")
hi <- a %>% filter(share_in_band > 0.10) %>% arrange(desc(share_in_band))
cat("state-years with >10% of positive cases in [mode-200, mode):",
    nrow(hi), "\n")
print(as.data.frame(head(hi, 15)), digits = 3)
cat("\nVA / TN / NY:\n")
print(as.data.frame(a %>% filter(state %in% c("Virginia", "Tennessee",
                                              "New York"))), digits = 3)
write.csv(a, "methods/vocab_hygiene_census/utilities_high_band_census.csv",
          row.names = FALSE)
