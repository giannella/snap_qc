# Percentile -> dollar-value map for FY2024, per state x household-size cell.
#
# For each of the seven percentile variables (Ben's features.R construction:
# CPI-deflated values ranked by cume_dist within state_name x rawusize, zeros
# pinned to percentile 0), this maps the requested percentiles to the
# CPI-deflated FY2024 value at that percentile in the cell. Percentile 0 is
# the zero mass (value 0 by construction); percentiles >= 1 are quantiles of
# the NON-ZERO deflated FY2024 values, matching the cume_dist semantics that
# rank only non-zero observations.
#
# Requested by Eric 2026-08-11 alongside the new exploratory study S37
# (Ben's within-state pooled-years percentiles vs the per_hh_size variables).
# Output: methods/state_percentile_runoff_v2/percentile_value_map_fy2024.csv

suppressMessages(library(dplyr))
setwd("C:/Users/ericg/snap_qc")

PCTS <- c(0, 1, 20, 30, 40, 50, 60, 70, 90, 99, 100)   # Eric's list, verbatim
VARS <- c(rawgrinc_p  = "rawgross",
          rawearn_p   = "rawearn",
          rawunearn_p = "rawunearn",
          rawmedded_p = "medical_deductions",
          rawdepded_p = "rawdepded",
          rawcsded_p  = "rawcsded",
          rawrent_p   = "rawrent")

d <- readRDS("reg_model_data.rds")
stopifnot(all(VARS %in% names(d)))
size_col <- if ("rawusize" %in% names(d)) "rawusize" else "cert_HH_size_FS_n"

yd <- read.csv("additional_data/year_data.csv")
cpi24 <- yd$cpi[yd$year == 2024]
stopifnot(length(cpi24) == 1, !is.na(cpi24))

d24 <- d[d$fiscal_year == 2024, ]
cat("FY2024 rows:", nrow(d24), "| size column:", size_col,
    "| CPI 2024:", cpi24, "\n")

rows <- list()
for (pv in names(VARS)) {
  src <- VARS[[pv]]
  cells <- split(seq_len(nrow(d24)),
                 paste(d24$state_name, d24[[size_col]], sep = "|"))
  for (cl in names(cells)) {
    idx <- cells[[cl]]
    v   <- d24[[src]][idx] / cpi24
    v   <- v[!is.na(v)]
    nz  <- v[v != 0]
    q   <- vapply(PCTS, function(p) {
      if (p == 0) 0
      else if (length(nz) == 0) NA_real_
      else unname(quantile(nz, p / 100, type = 7))
    }, numeric(1))
    parts <- strsplit(cl, "|", fixed = TRUE)[[1]]
    rows[[length(rows) + 1]] <- data.frame(
      state = parts[1], hh_size = parts[2],
      variable = pv, source_column = src,
      n_2024 = length(v), n_nonzero = length(nz),
      n_distinct = length(unique(v)),
      t(setNames(round(q, 4), paste0("p", PCTS, "_defl"))),
      t(setNames(round(q * cpi24, 0), paste0("p", PCTS, "_usd"))),
      check.names = FALSE)
  }
}
out <- bind_rows(rows) %>% arrange(state, as.numeric(hh_size), variable)
write.csv(out, "methods/state_percentile_runoff_v2/percentile_value_map_fy2024.csv",
          row.names = FALSE)
cat("wrote", nrow(out), "rows (state x size x variable)\n")
print(head(out[out$state == "Alabama" & out$variable == "rawmedded_p", ], 4))
