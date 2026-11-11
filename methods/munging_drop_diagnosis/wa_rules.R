# WA's two preauthorization tests (note from WA, 2026-10-31) on f1027 and f1030,
# FY2022-24: does the recreation change (419044a) take them away?
#   SR_ROOT=C:/Users/ericg/snap_qc_v27 Rscript methods/munging_drop_diagnosis/wa_rules.R
source("methods/munging_drop_diagnosis/probe_lib.R")
OUT <- "methods/munging_drop_diagnosis"
RULES <- list(
  test1_old = function(d) d$earned_by_hh_size > 300 & d$rawben_rel_max > 0.6 & d$shelter_expenses_by_hh_size < 300,
  test2_new = function(d) d$earned_by_hh_size > 200 & d$rawben_rel_max > 0.6 & d$shelter_expenses_by_hh_size < 600)
FR <- list(f1027 = probe_frame(FRAMES[["f1027"]]), f1030 = probe_frame(FRAMES[["f1030"]]))
FR <- lapply(FR, function(d) d[d$fiscal_year %in% 2022:2024, ])
FR$f1027 <- FR$f1027[match(FR$f1030$key, FR$f1027$key), ]
stamp("state values: %s", paste(head(sort(unique(FR$f1030$state)), 5), collapse = ", "))
redrawn <- { r <- readRDS(FRAMES[["f1030"]]); r <- r[r$fiscal_year %in% 2022:2024, ]
  k <- paste(r$fiscal_year, r$state, r$hhldno); !is.na(r$imputed_at_bound[match(FR$f1030$key, k)]) }
res <- list()
for (scope in c("Washington", "national")) for (f in names(FR)) for (rn in names(RULES)) {
  d <- FR[[f]]; s <- if (scope == "national") rep(TRUE, nrow(d)) else d$state %in% c("Washington", "WA", "53")
  hit <- RULES[[rn]](d) %in% TRUE
  h <- hit & s
  res[[length(res) + 1]] <- data.frame(scope, frame = f, rule = rn, cases = sum(s), flagged = sum(h),
    flag_share = mean(hit[s]), precision = mean(d$ie[h]), base_rate = mean(d$ie[s]),
    lift = mean(d$ie[h]) / mean(d$ie[s]), error_recall = sum(d$ie[h]) / sum(d$ie[s]),
    dollar_recall = sum(d$ed[h]) / sum(d$ed[s]), per_flag = mean(d$ed[h]),
    flagged_redrawn = sum(h & redrawn))
}
res <- do.call(rbind, res)
write.csv(res, file.path(OUT, "wa_rules.csv"), row.names = FALSE)
print(res, digits = 3)
# which cases move in or out of each rule between the frames (Washington)
for (rn in names(RULES)) {
  s <- FR$f1030$state %in% c("Washington", "WA", "53")
  a <- RULES[[rn]](FR$f1027) %in% TRUE & s; b <- RULES[[rn]](FR$f1030) %in% TRUE & s
  cat(sprintf("%s WA: in both %d; f1027 only %d (errors %d, redrawn %d); f1030 only %d (errors %d, redrawn %d)\n", rn,
    sum(a & b), sum(a & !b), sum(a & !b & FR$f1030$ie), sum(a & !b & redrawn),
    sum(b & !a), sum(b & !a & FR$f1030$ie), sum(b & !a & redrawn)))
}
