# Frame check: Ben's reconstruction rewrite (ben/state-options to 2570391,
# merged 2026-10-30) against the frame eb989fda (f1027).
#   a = archive_data/reg_model_data_f1027_eb989fda.rds (eb989fda; was additional_data/ until 2026-10-30)
#   b = reg_model_data.rds (rebuilt 2026-10-30 by the munging script; now also additional_data/)
#   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" methods/v270_seed_replicates/frame_check_f1030.R
suppressMessages(library(dplyr))
setwd("C:/Users/ericg/snap_qc_v27")
source("rule_mining_helpers.R")
a <- readRDS("archive_data/reg_model_data_f1027_eb989fda.rds")
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
