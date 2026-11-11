# Step 4 follow-up (2026-10-31): what 419044a changed, and the probe on the cases it left alone.
#   SR_ROOT=C:/Users/ericg/snap_qc_v27 Rscript methods/munging_drop_diagnosis/at_419044a.R
source("methods/munging_drop_diagnosis/probe_lib.R")
D <- "C:/Users/ericg/snap_qc_bisect_frames/"
raw <- readRDS(paste0(D, "frame_419044a.rds")); raw <- raw[raw$fiscal_year %in% 2022:2024, ]
raw$key <- paste(raw$fiscal_year, raw$state, raw$hhldno)
cat("imputed_at_bound in frame:", "imputed_at_bound" %in% names(raw), "\n")
if ("imputed_at_bound" %in% names(raw)) print(table(raw$imputed_at_bound, raw$fiscal_year, useNA = "ifany"))
A <- probe_frame(paste0(D, "frame_02ca411.rds")); B <- probe_frame(paste0(D, "frame_419044a.rds"))
A <- A[A$fiscal_year %in% 2022:2024, ]; B <- B[B$fiscal_year %in% 2022:2024, ]
stopifnot(identical(A$key, B$key))
ch <- abs(A$unc_rawben_rel_max - B$unc_rawben_rel_max) > 1e-6 | abs(A$gross_by_hh_size - B$gross_by_hh_size) > 1e-6
ch[is.na(ch)] <- TRUE
cat(sprintf("cases changed by 419044a: %d (errors %d, clean %d)\n", sum(ch), sum(ch & A$ie), sum(ch & !A$ie)))
band <- function(d, lo, hi) { x <- d$unc_rawben_rel_max; b <- x >= lo & x <= hi & !is.na(x)
  sprintf("[%.2f,%.2f]: errors %.1f%% clean %.1f%% error rate %.1f%%", lo, hi, 100*mean(b[d$ie]), 100*mean(b[!d$ie]), 100*mean(d$ie[b])) }
for (f in c("A","B")) { d <- get(f); cat(f, band(d, 0.995, 1.005), "|", band(d, 1.005, 1.10), "\n") }
# probe on the cases 419044a did not change (test year)
for (f in c("A","B")) { d <- get(f); s <- probe_scores(d); te <- s$te; keep <- !ch[te]
  p <- s$p[keep]; ie <- d$ie[te][keep]; ed <- d$ed[te][keep]; o <- order(-p)[seq_len(round(.05*length(p)))]
  cat(f, sprintf("unchanged FY2024 cases %d: auc %.3f prec05 %.3f dpf05 %.1f\n", sum(keep), probe_auc(p, ie), mean(ie[o]), mean(ed[o]))) }
