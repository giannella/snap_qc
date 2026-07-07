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
    p <- ggplot(sweeps, aes(x, precision, linetype = scoring)) +
      geom_line(linewidth = 0.8) + geom_point(size = 1.0) +
      geom_text(aes(label = sprintf("%.2f", threshold)), size = 2.4, vjust = -0.7,
                colour = "grey50", show.legend = FALSE, check_overlap = TRUE) +
      labs(x = "Hold-out dollar recall of the union (denominator follows scoring)",
           y = "Hold-out precision of the union",
           linetype = "Scored against",
           title = sprintf("Union precision-recall across precision floors - %s", frame_name),
           subtitle = sprintf("xgboost + ranger, trained %s, scored on %s; point labels = 99%% lower bound precision floor",
                              TRAIN_LABEL, HOLDOUT_LABEL)) +
      coord_cartesian(xlim = c(0, 1), ylim = c(0, 0.6)) +
      theme_minimal(base_size = 12) + theme(legend.position = "top")
    save_png(p, sub("\\.csv$", ".png", f), 8, 5)
    cat("regenerated", sub("\\.csv$", ".png", f), "\n")
  }
}
