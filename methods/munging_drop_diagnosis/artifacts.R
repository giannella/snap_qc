# Step 5 of plan.md (2026-10-31): recreation marks on f1027 and f1030.
#   SR_ROOT=C:/Users/ericg/snap_qc_v27 Rscript methods/munging_drop_diagnosis/artifacts.R
# Marks (reconstructed side; FY2022-24):
#   unc_at_max     uncapped benefit within $1 of the maximum allotment
#   unc_at_min     uncapped benefit within $1 of the minimum benefit (1-2 person units)
#   shelter_at_cap uncapped excess shelter deduction within $5 of the cap
#   small_amount   an income or expense amount above $0 and at most $25 (review-year dollars)
#   recreated      the case carries a reconstruction note
# Readings: (a) each mark's share among error and clean cases, and the error
# rate with the mark; (b) a probe on the marks alone; (c) the share of the main
# probe's top-5% errors that carry a mark; (d) the main probe's FY2024 reading
# on cases with no mark on either frame.
source("methods/munging_drop_diagnosis/probe_lib.R")
OUT <- "methods/munging_drop_diagnosis"
MARKS <- c("unc_at_max", "unc_at_min", "shelter_at_cap", "small_amount")
marks_of <- function(d) {
  n <- function(x) suppressWarnings(as.numeric(x))
  amt <- sapply(c("rawearn_nominal", "rawunearn_nominal", "rawrent_nominal", "rawmedded_nominal",
                  "rawdepded_nominal", "rawcsded_nominal"), function(v) n(d[[v]]))
  data.frame(
    unc_at_max = abs(n(d$rawben_uncapped) - n(d$benmax)) <= 1,
    unc_at_min = n(d$rawhsize) <= 2 & abs(n(d$rawben_uncapped) - n(d$rawminimum_ben)) <= 1,
    shelter_at_cap = abs(n(d$rawsltded_uncapped) - n(d$max_shelter_deduction)) <= 5,
    small_amount = rowSums(amt > 0 & amt <= 25, na.rm = TRUE) > 0,
    recreated = !is.na(d$correctednotes) & nzchar(as.character(d$correctednotes))) |>
    lapply(function(x) ifelse(is.na(x), FALSE, x)) |> as.data.frame()
}
FR <- list(f1027 = probe_frame(FRAMES[["f1027"]]), f1030 = probe_frame(FRAMES[["f1030"]]))
FR <- lapply(FR, function(d) d[d$fiscal_year %in% 2022:2024, ])
stopifnot(all(FR$f1030$key %in% FR$f1027$key))
FR$f1027 <- FR$f1027[match(FR$f1030$key, FR$f1027$key), ]   # the same cases, in the same order
MK <- lapply(FR, marks_of)

# (a) prevalence
prev <- do.call(rbind, lapply(names(FR), function(f) { d <- FR[[f]]; m <- MK[[f]]
  do.call(rbind, lapply(names(m), function(k) data.frame(frame = f, mark = k,
    share_of_errors = mean(m[[k]][d$ie]), share_of_clean = mean(m[[k]][!d$ie]),
    error_rate_with_mark = mean(d$ie[m[[k]]]), cases = sum(m[[k]])))) }))
prev$base <- vapply(prev$frame, function(f) mean(FR[[f]]$ie), numeric(1))
write.csv(prev, file.path(OUT, "artifacts_prevalence.csv"), row.names = FALSE)
print(prev, digits = 3)

# (b) marks alone (plus household-size group, which the probe splits on)
marks_only <- do.call(rbind, lapply(names(FR), function(f) {
  d <- FR[[f]]; m <- MK[[f]]
  dd <- d; for (v in VOCAB19) dd[[v]] <- 0
  for (k in MARKS) dd[[VOCAB19[match(k, MARKS)]]] <- as.numeric(m[[k]])
  cbind(frame = f, probe(dd))
}))
write.csv(marks_only, file.path(OUT, "artifacts_marks_only.csv"), row.names = FALSE)
print(marks_only, digits = 3)

# (c) and (d)
anymark <- function(f) Reduce(`|`, MK[[f]][MARKS])
marked_either <- anymark("f1027") | anymark("f1030")
cd <- list()
for (f in names(FR)) {
  d <- FR[[f]]; s <- probe_scores(d); te <- s$te; p <- s$p; ie <- d$ie[te]; ed <- d$ed[te]
  mk <- anymark(f)[te]
  o5 <- order(-p)[seq_len(round(0.05 * length(p)))]
  nm <- !marked_either[te]
  pn <- p[nm]; o5n <- order(-pn)[seq_len(round(0.05 * length(pn)))]
  cd[[f]] <- data.frame(frame = f, top5_precision = mean(ie[o5]), top5_errors_with_mark = mean(mk[o5][ie[o5]]),
    all_errors_with_mark = mean(mk[ie]), unmarked_cases = sum(nm),
    unmarked_auc = probe_auc(pn, ie[nm]), unmarked_top5_precision = mean(ie[nm][o5n]),
    unmarked_top5_per_flag = mean(ed[nm][o5n]))
}
cd <- do.call(rbind, cd)
write.csv(cd, file.path(OUT, "artifacts_probe.csv"), row.names = FALSE)
print(cd, digits = 3)
