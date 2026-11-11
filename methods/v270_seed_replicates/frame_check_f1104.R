# Frame check: Ben's donor matching (ben/state-options to 7d9fe2d, merged 2026-11-04 at cc051b4)
# against the frame f1030 (e3714039).
#   a = additional_data/reg_model_data.rds (f1030, e3714039) before promotion; archive_data/reg_model_data_f1030_e3714039.rds after
#   b = reg_model_data.rds (rebuilt 2026-11-04 by the munging script at 7d9fe2d)
#   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" methods/v270_seed_replicates/frame_check_f1104.R
suppressMessages(library(dplyr))
setwd("C:/Users/ericg/snap_qc_v27")
source("rule_mining_helpers.R")
a <- readRDS(if (file.exists("archive_data/reg_model_data_f1030_e3714039.rds")) "archive_data/reg_model_data_f1030_e3714039.rds" else "additional_data/reg_model_data.rds")
b <- readRDS("reg_model_data.rds")
cat(sprintf("rows: %d -> %d\n", nrow(a), nrow(b)))
err <- function(d) !is.na(d$over_threshold) & d$over_threshold != 0
by_year <- function(d) d %>% mutate(e = err(d)) %>% group_by(fiscal_year) %>%
  summarise(rows = n(), errors = sum(e), .groups = "drop")
print(full_join(by_year(a), by_year(b), by = "fiscal_year", suffix = c("_a", "_b")), n = 20)

key <- intersect(c("state", "fiscal_year", "CASE_ID", "caseid", "case_id", "HHLDNO", "hhldno"), names(a))
cat("key columns:", paste(key, collapse = ", "), "\n")
ka <- do.call(paste, a[key]); kb <- do.call(paste, b[key])
cat(sprintf("keys unique: a %s, b %s\n", !anyDuplicated(ka), !anyDuplicated(kb)))
cat(sprintf("in a not b: %d | in b not a: %d\n", sum(!ka %in% kb), sum(!kb %in% ka)))
gone <- a[!ka %in% kb, ]
if (nrow(gone)) {
  cat("dropped rows by year / error:\n"); print(table(gone$fiscal_year, err(gone)))
}
m <- match(ka, kb); ok <- !is.na(m)
ea <- err(a)[ok]; eb <- err(b)[m[ok]]
cat(sprintf("matched rows: %d; error label a->b: stays error %d, error->clean %d, clean->error %d\n",
            sum(ok), sum(ea & eb), sum(ea & !eb), sum(!ea & eb)))
for (yy in list(2017:2019, 2022:2024)) {
  s <- a$fiscal_year[ok] %in% yy
  cat(sprintf("  FY%d-%d: error->clean %d, clean->error %d\n", min(yy), max(yy),
              sum((ea & !eb)[s]), sum((!ea & eb)[s])))
}
da <- ifelse(ea, abs(a$total_error_amount[ok]), 0); db <- ifelse(eb, abs(b$total_error_amount[m[ok]]), 0)
cat(sprintf("error $ on matched rows: %.0f -> %.0f\n", sum(da, na.rm = TRUE), sum(db, na.rm = TRUE)))

for (nm in c("a", "b")) {
  d <- get(nm); s <- d$fiscal_year %in% 2022:2024; d2 <- d[s, ]
  pile <- income_pile_rows(d2)
  mm <- d2$rawben >= d2$benmax & d2$rawben_uncapped < d2$benmax
  cat(sprintf("%s FY2022-24: rows %d, errors %d, pile rows %d (errors %d), mismatch rows %d\n",
              nm, nrow(d2), sum(err(d2)), sum(pile), sum(pile & err(d2)), sum(mm, na.rm = TRUE)))
}
cat("columns only in a:", paste(setdiff(names(a), names(b)), collapse = ", "), "\n")
cat("columns only in b:", paste(setdiff(names(b), names(a)), collapse = ", "), "\n")

# rebuild misses (issued vs recreated benefit), stuck-case smoothing and the per-window counts common.R checks
for (nm in c("a", "b")) {
  d <- get(nm); gap <- abs(as.numeric(d$rawben) - as.numeric(d$raw_benefit_amount))
  cat(sprintf("%s: rebuild misses > $1 %d (errors %d), > $5 %d; imputed_at_bound %s\n", nm, sum(gap > 1, na.rm = TRUE),
              sum(gap > 1 & err(d), na.rm = TRUE), sum(gap > 5, na.rm = TRUE),
              paste(names(table(d$imputed_at_bound)), table(d$imputed_at_bound), collapse = ", ")))
  yr <- as.character(d$fiscal_year); e <- err(d)
  W <- list(fy2024 = list(2022:2023, 2024), fy2022 = list(2017:2019, 2022), fy2019 = list(2017:2018, 2019),
            deploy = list(2022:2024, integer(0)))
  for (w in names(W)) cat(sprintf("  %s window: train rows %d, errors %d; test rows %d, errors %d\n", w,
    sum(yr %in% W[[w]][[1]]), sum(e[yr %in% W[[w]][[1]]]), sum(yr %in% W[[w]][[2]]), sum(e[yr %in% W[[w]][[2]]])))
}
if (nrow(gone)) { cat("dropped rows by state:\n"); print(sort(table(gone$state), decreasing = TRUE)) }
