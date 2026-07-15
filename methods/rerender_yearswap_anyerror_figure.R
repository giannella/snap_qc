# Re-renders the year-swap typed-vs-pooled figure from its saved sweep CSV
# with a correct x-axis year (the original render inherited a hardcoded
# "(2023)" label; source script now fixed). No mining.
suppressMessages({library(dplyr); library(ggplot2)})
source("rule_mining_helpers.R")
out_dir <- "methods/compare_anyerror_vs_typed_v2/yearswap_train2223_test24"
overall <- read.csv(file.path(out_dir, "anyerror_vs_typed_sweep.csv"),
                    stringsAsFactors = FALSE)

cols <- c("Typed (4 frames)" = "#d1495b", "Any-error (1 model)" = "#0073b7",
          "Combined" = "#1b1b1b")
p <- ggplot(overall, aes(x, precision, color = approach)) +
  geom_line(linewidth = 0.9) + geom_point(size = 1.2) +
  geom_text(aes(label = sprintf("%.2f", threshold)), size = 2.4, vjust = -0.75,
            show.legend = FALSE, check_overlap = TRUE) +
  scale_color_manual(values = cols) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Hold-out recall of ALL payment error DOLLARS (2024)",
       y = "Hold-out precision of the union of kept rules",
       color = NULL,
       title = "Does mining by error type beat one all-errors model?",
       subtitle = "Same engines, screens and 90% Wilson-LCB selection on any-error train precision;\ntrained 2022/2023, scored on 2024; point labels = LCB floor",
       caption = "xgboost (1,000 rounds) + ranger (2,500 trees, mtry=1), depth 4, per HH stratum 1 / 2-3 / 4+") +
  theme_minimal(base_size = 13) + theme(legend.position = "top")
save_png(p, file.path(out_dir, "anyerror_vs_typed_sweep.png"), 9, 5.5)
cat("re-rendered", file.path(out_dir, "anyerror_vs_typed_sweep.png"), "\n")
