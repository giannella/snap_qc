# Regenerate *_lcb_sweep.png charts from their CSVs with fixed axes
# (x 0-1, y 0-0.6) so charts are directly comparable across frames and runs.
# Plot code mirrors INCL_find_inclusion_rules_by_hh_size_v2.R.

suppressMessages({library(dplyr); library(ggplot2)})
source("rule_mining_helpers.R")

FOLDERS <- c("inclusion_rules_by_hh_size_v2",
             "inclusion_rules_by_hh_size_v2/run3_singleelement_frame")
TRAIN_LABEL   <- "2022/2024"
HOLDOUT_LABEL <- "2023"

for (dir in FOLDERS) {
  for (f in list.files(dir, pattern = "_lcb_sweep\\.csv$", full.names = TRUE)) {
    frame_name <- sub("_lcb_sweep\\.csv$", "", basename(f))
    sweeps <- read.csv(f, stringsAsFactors = FALSE)
    sweep_long <- bind_rows(
      sweeps %>% mutate(metric = "hold-out precision", value = precision),
      sweeps %>% mutate(metric = "hold-out share of error $ caught", value = x))
    p <- ggplot(sweep_long, aes(threshold, value, linetype = scoring)) +
      geom_line(linewidth = 0.8) + geom_point(size = 1.0) +
      facet_wrap(~metric, nrow = 1) +
      labs(x = "99% lower bound precision",
           y = NULL, linetype = "Scored against",
           title = sprintf("What the kept rules achieve together, by precision floor - %s", frame_name),
           subtitle = sprintf("xgboost + ranger, trained %s, scored on %s; compare lines vertically at any floor",
                              TRAIN_LABEL, HOLDOUT_LABEL)) +
      coord_cartesian(xlim = c(0.05, max(0.7, max(sweeps$threshold) + 0.05)),
                      ylim = c(0, 1)) +
      theme_minimal(base_size = 12) + theme(legend.position = "top")
    save_png(p, sub("\\.csv$", ".png", f), 9, 4.5)
    cat("regenerated", sub("\\.csv$", ".png", f), "\n")
  }
}
