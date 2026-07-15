# Re-renders the yearswap strata figure from the saved sweeps CSV with the
# correct year labels (the in-flight run loaded the script before the
# subtitle was made dynamic). No mining.
suppressMessages({library(dplyr); library(ggplot2)})
source("rule_mining_helpers.R")
out_dir <- "methods/compare_hh_strata_v2/yearswap_train2223_test24"
sweeps <- read.csv(file.path(out_dir, "strata_sweeps.csv"), stringsAsFactors = FALSE)

cols <- c("Pooled (no split)" = "#8c8c8c", "1 / 2-3 / 4+" = "#d1495b",
          "1 / 2 / 3 / 4 / 5+" = "#0073b7")
p <- ggplot(sweeps, aes(x, precision, color = scheme)) +
  geom_line(linewidth = 0.9) + geom_point(size = 1.1) +
  geom_text(aes(label = sprintf("%.2f", threshold)), size = 2.4, vjust = -0.75,
            show.legend = FALSE, check_overlap = TRUE) +
  scale_color_manual(values = cols) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Hold-out dollar recall of the union (all 2024 errors)",
       y = "Hold-out precision of the union", color = NULL,
       title = "Household-size stratification on the v2 stack",
       subtitle = "Same engines and filtering; only the partition differs. Pooled keeps HH size as a feature.\nany-error frame, trained 2022/2023, scored on 2024; point labels = LCB floor") +
  theme_minimal(base_size = 12) + theme(legend.position = "top")
save_png(p, file.path(out_dir, "strata_sweeps.png"), 9, 5.5)
cat("re-rendered", file.path(out_dir, "strata_sweeps.png"), "\n")
