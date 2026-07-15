# Full-state visibility dot plot (revD deck): what share of each state's
# FY22-24 error population (over-threshold errors + ineligible exclusions)
# the public QC files can even show. Replaces the 6-row table on the
# limitations slide. Guidance line at ~60%: states below it should treat
# public-data rules as a supplement and mine internally.
#
# Reads methods/state_error_accounting/visibility_by_state_2022_2024.csv
# Writes methods/state_error_accounting/visibility_dotplot_2022_2024.png

suppressMessages({library(dplyr); library(ggplot2)})
source("rule_mining_helpers.R")

acct <- "methods/state_error_accounting"
d <- read.csv(file.path(acct, "visibility_by_state_2022_2024.csv"),
              stringsAsFactors = FALSE) %>%
  arrange(pct_visible) %>%
  mutate(state = factor(state, levels = state))

p <- ggplot(d, aes(x = pct_visible, y = state)) +
  geom_vline(xintercept = 60, linetype = "dashed", colour = "grey55") +
  geom_point(size = 1.7) +
  annotate("text", x = 3, y = nrow(d) - 4.5, hjust = 0, vjust = 1, size = 3.0,
           colour = "grey25", lineheight = 1.1,
           label = "dashed line: below ~60% visibility, treat\nthese rules as a supplement and mine on\ninternal data (it has the ineligible cases)") +
  labs(x = "share of the state's FY22-24 error cases visible in the public files (%)",
       y = NULL,
       title = "Share of each state's error cases the public files show",
       subtitle = "the excluded cases are ineligible determinations") +
  scale_x_continuous(limits = c(0, 100)) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 6.4))

save_png(p, file.path(acct, "visibility_dotplot_2022_2024.png"), 5.6, 6.2)
cat("wrote", file.path(acct, "visibility_dotplot_2022_2024.png"), "\n")
