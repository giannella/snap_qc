# Reconstructed generating script for presentation_figures/esap_error_mix.png
# (the original was never committed). This one COMPUTES the error-type mix from the
# modelling frame rather than hardcoding shares, so it always reflects the current
# frame (the original PNG's 64/18/11/6 split predates the 2026-07-07 rebuild and may
# be stale, the same way other_error counts are under review).
#
# NOTE: not run in this environment because reg_model_data.rds is not present here.
# Run it on a machine that has the frame:  Rscript methods/draw_esap_error_mix.R
# If a column name below differs in your frame, adjust the three config lines.
suppressMessages({library(ggplot2); library(dplyr)})
source("rule_mining_helpers.R")

# ---- config (adjust if your frame uses different names) ----
YEAR_COL     <- "year"                # the FY / year column in reg_model_data
YEAR_KEEP    <- 2023                  # error mix is shown for one year
ELDERLY_COL  <- "elderly_disabled_i"  # 1 = elderly/disabled household, else 0
# ------------------------------------------------------------

d <- readRDS("reg_model_data.rds")
errors <- d %>%
  filter(.data[[YEAR_COL]] == YEAR_KEEP,
         !is.na(over_threshold), over_threshold != 0)

etype <- c(other_error = "other (deductions etc.)",
           unearned_overissuance = "unearned income overpayment",
           earned_overissuance = "earned income overpayment",
           underissuance = "underpayment")
etype_levels <- unname(etype)  # stack order, top to bottom

mix <- errors %>%
  mutate(group = ifelse(.data[[ELDERLY_COL]] == 1,
                        "elderly / disabled households", "other households"),
         etype = factor(etype[as.character(error_status)], levels = etype_levels)) %>%
  filter(!is.na(etype)) %>%
  count(group, etype) %>%
  group_by(group) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()

# headline numbers for the subtitle, computed from the frame
grp <- d %>% filter(.data[[YEAR_COL]] == YEAR_KEEP) %>%
  summarise(cases = mean(.data[[ELDERLY_COL]] == 1))
err_share <- errors %>% summarise(e = mean(.data[[ELDERLY_COL]] == 1)) %>% pull(e)

cols <- c("other (deductions etc.)" = "#1F6FB2",
          "unearned income overpayment" = "#5FA8DC",
          "earned income overpayment" = "#C0392B",
          "underpayment" = "#E1A15A")

p <- ggplot(mix, aes(group, share, fill = etype)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = scales::percent(share, accuracy = 1)),
            position = position_stack(vjust = 0.5), colour = "white", size = 4.6) +
  scale_fill_manual(values = cols, name = "Error type",
                    breaks = etype_levels) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Elderly/disabled households don't need a separate model; their errors just differ in KIND",
    subtitle = sprintf(paste0("They are no more error-prone than other households (%.0f%% of errors, ",
                              "%.0f%% of caseload). But their\nerrors concentrate in the easiest types to ",
                              "detect, so the rules find far more of them."),
                       100 * err_share, 100 * grp$cases),
    x = NULL, y = "Share of the group's errors (2023)") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())

save_png(p, "presentation_figures/esap_error_mix.png", 10.8, 6.0)
cat("wrote esap_error_mix.png\n")
