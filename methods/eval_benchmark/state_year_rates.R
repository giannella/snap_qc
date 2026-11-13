# Error rates by state and fiscal year on the evaluation benchmark, against the public QC files they come from
# (2026-11-12; Eric: "tell me the est. error rates using it for each state by year and let me know if anything seems
# disproportionately off compared to the input data we get from the public QC files").
# Three sets of cases per state-year, all weighted by the public file's FYWGT (joined by state and HHLDNO):
#   public  every case in qc_data/qc_pub_fy<YYYY>.sav for the 49 jurisdictions the frame keeps (no AK, HI, Guam, VI)
#   frame   the benchmark's cases (frame f1109), after the munging's drops
#   kept    the cases held-out scoring counts
# Case rate: share of cases with an error over the year's tolerance (AMTERR > threshold, year_data.csv).
# Dollar rate: error dollars of those cases over benefits issued (RAWBEN), as the payment error rate is built; it is
# not FNS's published rate, which adds a regression adjustment and covers cases outside the public file.
# Writes methods/eval_benchmark/state_year_rates_f1109.csv. Run from the repository root.
suppressMessages(library(haven))
stopifnot(file.exists("methods/eval_benchmark/score_ranking.R"))
source("methods/eval_benchmark/score_ranking.R")
bench <- read_benchmark()
YEARS <- c("2017", "2018", "2019", "2022", "2023", "2024")
fips <- read.csv("additional_data/state_data.csv", check.names = FALSE); names(fips) <- sub("^﻿", "", names(fips))
yd <- read.csv("additional_data/year_data.csv", check.names = FALSE); names(yd) <- sub("^﻿", "", names(yd))
thr <- setNames(yd$error_threshold, yd$year)

pub <- do.call(rbind, lapply(YEARS, function(y) {
  d <- read_sav(sprintf("qc_data/qc_pub_fy%s.sav", y), col_select = c("STATE", "HHLDNO", "FYWGT", "AMTERR", "RAWBEN", "FSBEN", "ALLADJ", "STATUS", "MN_FIP", "SSI_CAP"))
  d <- as.data.frame(lapply(d, function(v) as.numeric(zap_labels(v))))
  d$fiscal_year <- y; d$state <- fips$state[match(d$STATE, fips$fips)]
  d$error <- (d$AMTERR > thr[[y]]) %in% TRUE
  d
}))
pub <- pub[pub$state %in% unique(bench$state), ]
stopifnot(!anyDuplicated(paste(pub$fiscal_year, pub$state, pub$HHLDNO)))
m <- match(paste(bench$fiscal_year, bench$state, bench$hhldno), paste(pub$fiscal_year, pub$state, pub$HHLDNO))
cat(sprintf("benchmark cases matched to a public-file case: %d of %d\n", sum(!is.na(m)), nrow(bench)))
stopifnot(!anyNA(m))
bench$w <- pub$FYWGT[m]
cat(sprintf("benchmark error label equals the public file's (AMTERR > threshold): %.2f%% of cases (%d differ)\n",
            100 * mean(bench$error == pub$error[m]), sum(bench$error != pub$error[m])))
in_frame <- seq_len(nrow(pub)) %in% m
# why a public case is not in the frame, in the munging's order (the SSI-CAP flag is 1-4 for CAP units; the tech doc's
# 2/3/4 are consistent / inconsistent / NYSCAP); the AMTERR check is approximated on cases not prorated
pub$fate <- ifelse(in_frame, "frame",
            ifelse(!(pub$MN_FIP %in% 0), "mfip",
            ifelse(!(pub$SSI_CAP %in% 0), "ssi_cap",
            ifelse(pub$ALLADJ %in% 2, "prorated",
            ifelse(!((abs(abs(pub$RAWBEN - pub$FSBEN) - pub$AMTERR) <= 5) %in% TRUE), "amterr_check", "other")))))

rates <- function(err, edol, ben, w) c(cases = length(err), errors = sum(err), case_rate = sum(w * err) / sum(w),
                                       dollar_rate = sum(w * edol * err) / sum(w * ben), weight = sum(w))
grid <- expand.grid(state = sort(unique(bench$state)), fiscal_year = YEARS, stringsAsFactors = FALSE)
out <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
  s <- grid$state[i]; y <- grid$fiscal_year[i]
  P <- pub[pub$state == s & pub$fiscal_year == y, ]; B <- bench[bench$state == s & bench$fiscal_year == y, ]; K <- B[B$kept, ]
  rp <- rates(P$error, P$AMTERR, P$RAWBEN, P$FYWGT)
  rf <- rates(B$error, B$error_dollars, B$issued_benefit, B$w)
  rk <- rates(K$error, K$error_dollars, K$issued_benefit, K$w)
  fate <- function(f) c(sum(P$fate == f), sum(P$fate == f & P$error))
  data.frame(state = s, fiscal_year = y,
             public_cases = rp[["cases"]], public_errors = rp[["errors"]], public_case_rate = rp[["case_rate"]], public_dollar_rate = rp[["dollar_rate"]],
             frame_cases = rf[["cases"]], frame_errors = rf[["errors"]], frame_case_rate = rf[["case_rate"]], frame_dollar_rate = rf[["dollar_rate"]],
             kept_cases = rk[["cases"]], kept_errors = rk[["errors"]], kept_case_rate = rk[["case_rate"]], kept_dollar_rate = rk[["dollar_rate"]],
             drop_mfip = fate("mfip")[1], drop_mfip_errors = fate("mfip")[2],
             drop_ssi_cap = fate("ssi_cap")[1], drop_ssi_cap_errors = fate("ssi_cap")[2],
             drop_amterr_check = fate("amterr_check")[1], drop_amterr_check_errors = fate("amterr_check")[2],
             drop_prorated = fate("prorated")[1], drop_prorated_errors = fate("prorated")[2],
             drop_other_munging = fate("other")[1], drop_other_munging_errors = fate("other")[2],
             drop_kept_filter = nrow(B) - nrow(K), drop_kept_filter_errors = sum(B$error) - sum(K$error),
             stringsAsFactors = FALSE)
}))

# Kept rate over public rate, by state-year, beside the national (case) and median state (dollar) ratios
for (y in YEARS) {
  x <- out[out$fiscal_year == y, ]
  r_case <- with(x, sum(kept_case_rate * kept_cases) / sum(kept_cases)) / with(x, sum(public_case_rate * public_cases) / sum(public_cases))
  r_dol <- median(x$kept_dollar_rate / x$public_dollar_rate)
  i <- out$fiscal_year == y
  out$case_ratio[i] <- out$kept_case_rate[i] / out$public_case_rate[i]
  out$case_ratio_national[i] <- r_case
  out$dollar_ratio[i] <- out$kept_dollar_rate[i] / out$public_dollar_rate[i]
  out$dollar_ratio_median[i] <- r_dol
}
out$case_share_kept <- out$kept_cases / out$public_cases
out$error_share_kept <- out$kept_errors / out$public_errors
write.csv(out, "methods/eval_benchmark/state_year_rates_f1109.csv", row.names = FALSE)

cat("\nNational, weighted:\n")
for (y in YEARS) { x <- out[out$fiscal_year == y, ]
  cat(sprintf("FY%s cases public %d frame %d kept %d | errors %d %d %d | case-rate ratio kept/public %.3f | median dollar-rate ratio %.3f\n", y,
              sum(x$public_cases), sum(x$frame_cases), sum(x$kept_cases), sum(x$public_errors), sum(x$frame_errors), sum(x$kept_errors),
              x$case_ratio_national[1], x$dollar_ratio_median[1])) }
cat("\nCase share kept (kept / public), distribution over state-years:\n"); print(round(quantile(out$case_share_kept, c(0, .01, .05, .5, .95, 1)), 3))
cat("Error share kept, distribution:\n"); print(round(quantile(out$error_share_kept, c(0, .01, .05, .5, .95, 1)), 3))
cat("Dollar ratio, distribution:\n"); print(round(quantile(out$dollar_ratio, c(0, .01, .05, .5, .95, .99, 1)), 3))
