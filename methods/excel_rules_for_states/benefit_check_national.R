# National estimate for the workbooks' benefit check (element 520, benefit calculation errors; Eric, 2026-11-09).
#
# The check flags a case when the benefit recalculated from its inputs with the review year's tables differs
# from the issued benefit by more than that year's federal tolerance. Cases with an allotment adjustment
# (ALLADJ 2 = prorated, 3 = other) are skipped, since their issued benefit is not a full month's benefit.
#
# The Step 2 testing data cannot score this check. Their inputs are reconstructed from the reviewer's values,
# and where the reconstruction fails to reach the issued benefit the gap marks the reconstruction failure, not
# the agency's arithmetic (error rate 0.48 on such rows against 0.11 overall). So Step 3 quotes this national
# estimate in the rule text (precision, recall, $ per flag), computed as follows on FY2022-24, all states:
#   - the benefit is recalculated exactly as the workbook does it (HOUSEHOLD_SIZE = certified size, nominal
#     inputs, the review year's maximum allotment, standard deduction and shelter cap, minimum benefit for
#     sizes 1-2), so cases matched only with last year's tables (munging "prior_year_tables") are flagged too;
#   - an error case counts as caught only if element 520 is among its elements. Other error cases are counted
#     as not flagged: with a state's own records their inputs would reproduce the issued benefit;
#   - every clean case the check flags counts as a false flag.
# Precision, recall, dollar recall and workload follow build_workbook_v2.score_list; the dollars are the
# over-tolerance error amounts (the official definition).
#
# Usage (from the repo root): Rscript methods/excel_rules_for_states/benefit_check_national.R
#   -> methods/excel_rules_for_states/benefit_check_national.csv (read by build_workbook_v2.py)
YEARS <- 2022:2024
OUT <- "methods/excel_rules_for_states/benefit_check_national.csv"
RDS <- "additional_data/reg_model_data.rds"

cols <- c("fiscal_year", "state", "cert_HH_size_FS_n", "rawearn_nominal", "rawunearn_nominal", "rawdepded_nominal",
          "rawmedded_nominal", "rawcsded_nominal", "rawrent_nominal", "rawutil_nominal", "rawhomeless_ded_nominal",
          "fsnelder", "fsndis", "rawben", "raw_benefit_amount", "threshold", "over_threshold", "total_error_amount",
          "alladj", "correctednotes", paste0("element", 1:9))
r <- readRDS(RDS); stopifnot(all(cols %in% names(r)))
sha <- substr(digest::digest(file = RDS, algo = "sha256"), 1, 8)
r <- r[as.character(r$fiscal_year) %in% YEARS, cols]; invisible(gc())
num <- function(v) suppressWarnings(as.numeric(as.character(v)))
z <- function(v) { v <- num(v); v[is.na(v)] <- 0; v }

fy <- num(r$fiscal_year); hh <- num(r$cert_HH_size_FS_n); sz <- pmin(pmax(hh, 1), 20)
yd <- read.csv("additional_data/year_data.csv", check.names = FALSE); names(yd)[1] <- "year"
ma <- read.csv("additional_data/max_allotments.csv", check.names = FALSE)
sd <- read.csv("additional_data/standard_deductions.csv", check.names = FALSE)
at <- function(tab, col) tab[[col]][match(fy, tab$year)]
pick <- function(tab) vapply(seq_along(fy), function(i) num(tab[match(fy[i], tab$year), as.character(sz[i])]), 0)
benmax <- pick(ma)
stdded <- pick(sd) - ifelse(r$state == "Illinois", z(at(sd, "IL_OFFSET")), 0)
maxsh <- ifelse(z(r$fsnelder) + z(r$fsndis) > 0, Inf, at(yd, "max_shelter_deduction"))
minben <- ifelse(hh < 3, at(yd, "min_allotment"), 0)
thr <- at(yd, "error_threshold")
stopifnot(!anyNA(benmax), !anyNA(stdded), !anyNA(thr), all(thr == num(r$threshold)))

# the workbook's nominal chain (make_input_workbook.feature_formulas, _c_benrec)
earn <- z(r$rawearn_nominal); hless <- z(r$rawhomeless_ded_nominal)
netbs <- earn + z(r$rawunearn_nominal) - (floor(earn * 0.2) + z(r$rawdepded_nominal) + z(r$rawmedded_nominal) +
                                          z(r$rawcsded_nominal) + stdded)
slt <- ifelse(hless > 0, 0, pmin(pmax(z(r$rawrent_nominal) + z(r$rawutil_nominal) - pmax(netbs * 0.5, 0), 0), maxsh))
benrec <- pmin(pmax(floor(benmax - 0.3 * floor(netbs - (slt + hless))), minben), benmax)

issued <- num(r$rawben); y <- num(r$over_threshold) %in% 1; amt <- ifelse(y, z(r$total_error_amount), 0)
adj <- num(r$alladj) %in% c(2, 3)
flag <- !adj & round(abs(benrec - issued)) > thr
e520 <- Reduce(`|`, lapply(paste0("element", 1:9), function(v) num(r[[v]]) %in% 520))
py <- grepl("prior_year_tables", r$correctednotes)

# the recalculation must reproduce the frame's recreated benefit wherever the munging did not swap tables in
same <- benrec == num(r$raw_benefit_amount)
cat(sprintf("frame %s, FY%d-%d: %d cases; recalculation equals the frame's recreated benefit on %.2f%% of rows without last year's tables\n",
            sha, min(YEARS), max(YEARS), nrow(r), 100 * mean(same[!py])))
stopifnot(mean(same[!py]) > 0.99)

caught <- flag & y & e520                 # genuine flags
false_flag <- flag & !y                   # clean cases flagged
grp <- ifelse(hh <= 1, "1", ifelse(hh <= 3, "2-3", "4+"))
one <- function(w) {
  n <- sum(caught[w]) + sum(false_flag[w]); tp <- sum(caught[w]); dol <- sum(amt[w & caught])
  data.frame(cases = sum(w), flagged = n, errors = tp, dollars = round(dol, 2),
             prec = if (n) tp / n else 0, rec = tp / sum(y), drec = dol / sum(amt), work = n / sum(w),
             dollars_per_flag = if (n) dol / n else NA_real_,
             excluded_error_flags = sum(flag[w] & y[w] & !e520[w]), skipped_adjusted = sum(adj[w]),
             prior_year_flags = sum(flag[w] & py[w]))
}
res <- do.call(rbind, lapply(c("all", "1", "2-3", "4+"), function(g) cbind(hh = g, one(if (g == "all") rep(TRUE, nrow(r)) else (grp == g) %in% TRUE))))
res <- cbind(res, years = sprintf("FY%d-%d", min(YEARS), max(YEARS)), frame_sha = sha)
print(res, digits = 3)
write.csv(res, OUT, row.names = FALSE)
cat("wrote", OUT, "\n")
