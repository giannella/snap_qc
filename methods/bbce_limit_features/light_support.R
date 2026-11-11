# Night 2 support for the light state options (2026-10-23): rows and errors
# for each flag level x household-size group in training (FY2017-19), the
# range of smd_gap_dollars, and the two Tier 1 subgroups the night-2 readout
# reads (written to light_subgroups.csv with their FY2022 rows and errors).
#   Rscript methods/bbce_limit_features/light_support.R
source(file.path(Sys.getenv("SR_ROOT", "C:/Users/ericg/snap_qc_v27"), "methods/bbce_limit_features/common.R"))
suppressMessages(library(dplyr))
options(width = 200)
F <- load_frame_arm(); a <- F$adf; tr <- F$is_tr; te <- F$yr == TEST_YEAR
flags <- setdiff(LIGHT, "smd_gap_dollars")
sup <- bind_rows(lapply(flags, function(v) data.frame(feature = v, level = a[[v]][tr], hh = F$hh[tr], err = F$ie[tr]))) %>%
  group_by(feature, level, hh) %>% summarise(rows = n(), errors = sum(err), .groups = "drop")
cat("Training (FY2017-19) rows / errors by flag level and household-size group:\n"); print(as.data.frame(sup), row.names = FALSE)
cat("\nThinnest cell per flag:\n"); print(as.data.frame(sup %>% group_by(feature) %>% slice_min(errors, n = 1, with_ties = FALSE)), row.names = FALSE)
g <- a$smd_gap_dollars
cat(sprintf("\nsmd_gap_dollars, training: SMD states %s to %s, no-SMD states %s to %s; FY2022: SMD states %s to %s, no-SMD states %s\n",
            min(g[tr & a$smd_i == 1]), max(g[tr & a$smd_i == 1]), min(g[tr & a$smd_i == 0]), max(g[tr & a$smd_i == 0]),
            min(g[te & a$smd_i == 1]), max(g[te & a$smd_i == 1]), paste(unique(g[te & a$smd_i == 0]), collapse = "/")))

# subgroups: (1) states with any light option in FY2022 different from some
# FY2017-19 year; (2) states with an SMD below Part B minus $35 in FY2022
key <- function(rows) tapply(apply(as.matrix(a[rows, flags]), 1, paste, collapse = ""), F$st[rows], function(z) paste(unique(z), collapse = "/"))
k22 <- key(te); ktr <- lapply(TRAIN_YEARS, function(y) key(F$yr == y))
changed <- sort(names(k22)[vapply(names(k22), function(s) any(vapply(ktr, function(k) !identical(k[[s]], k22[[s]]), logical(1))), logical(1))])
below <- sort(unique(F$st[te & a$smd_i == 1 & g < 0]))
sg <- rbind(data.frame(subgroup = "light_option_changed", state = changed), data.frame(subgroup = "smd_below_line_fy2022", state = below))
sg <- sg %>% rowwise() %>% mutate(rows_fy2022 = sum(te & F$st == state), errors_fy2022 = sum(te & F$st == state & F$ie)) %>% ungroup()
cat("\nSubgroups, FY2022 rows / errors:\n"); print(as.data.frame(sg %>% group_by(subgroup) %>% summarise(states = n(), rows = sum(rows_fy2022), errors = sum(errors_fy2022))), row.names = FALSE)
print(as.data.frame(sg), row.names = FALSE)
write.csv(sg, file.path("methods/bbce_limit_features", "light_subgroups.csv"), row.names = FALSE)
