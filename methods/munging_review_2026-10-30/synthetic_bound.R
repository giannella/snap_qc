# Munging review 2026-10-30 (EXPLORATORY): synthetic income errors that land on the maximum benefit.
# Clean cases between the limits get an income cut (earned or unearned) large enough that the recreated benefit
# is the maximum allotment. Step 7 (tracked recreate_elements) then finds its stop point, and the tracked
# impute_at_bound("max") redraws it using the real donors in corrected.rds. Compares stop point and draw with the
# injected truth under two truth mechanisms. Writes ONLY to methods/munging_review_2026-10-30/scratch/.
#   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" methods/munging_review_2026-10-30/synthetic_bound.R [n]
args <- commandArgs(TRUE)
n_per <- if (length(args)) as.integer(args[1]) else 800
setwd("C:/Users/ericg/snap_qc_v27")
out <- "methods/munging_review_2026-10-30/scratch/"
load(paste0(out, "pre7_env.RData"))          # functions and constants up to Step 7 (built by synthetic_multielement.R)
src <- readLines("1_data_munging_and_raw_variable_reconstruction_for_using_public_qc_data.R")
a <- grep("^impute_seed", src); b <- grep("^mydata\\$imputed_at_bound <- NA_character_", src) - 2
eval(parse(text = src[a:b]))                 # closing_move, impute_at_bound, impute_past_shelter_cap (definitions only)
suppressMessages(library(dplyr))
corr <- readRDS("corrected.rds")
corr <- calculate_raw_benefits(corr)
set.seed(20261030)

make_cases <- function(col, fs, code, mech) {
  pool <- which(is.na(corr$ELEMENT1) & corr$STATUS %in% 1 & corr$correctednotes == "no_change" &
                  (corr$rawben_recreated == corr$RAWBEN) %in% TRUE & corr$rawhomeless_ded == 0 &
                  corr$RAWBEN < corr$rawbenmax & corr[[fs]] > 50)
  # donor ratios as impute_at_bound sees them: closing move col_down, between the limits, matched
  notes <- corr$correctednotes; cl <- closing_move(notes)
  mv <- paste0(sub("raw", "", col), "_down")
  don <- which(!grepl("_error", notes) & cl %in% mv & (abs(corr$rawben_recreated - corr$RAWBEN) <= zero_margin) %in% TRUE &
                 corr$RAWBEN < corr$rawbenmax & !(corr$rawminimum_ben > 0 & corr$RAWBEN == corr$rawminimum_ben))
  dr <- as.numeric(corr[[col]][don]) / as.numeric(corr[[fs]][don]); dr <- dr[is.finite(dr) & dr < 1 & dr >= 0]
  rows <- integer(0); truth <- numeric(0); tries <- 0
  while (length(rows) < n_per && tries < 200 * n_per) {
    tries <- tries + 1
    i <- pool[sample.int(length(pool), 1)]
    r <- if (mech == "donor_ratio") dr[sample.int(length(dr), 1)] else if (runif(1) < 0.5) 0 else runif(1)
    tv <- floor(as.numeric(corr[[fs]][i]) * r)
    bi <- lapply(corr[i, benefit_inputs], as.numeric); bi[[col]] <- tv
    if (calculate_raw_benefits(bi)$rawben_recreated != corr$rawbenmax[i]) next
    rows <- c(rows, i); truth <- c(truth, tv)
  }
  d <- corr[rows, ]
  d$RAWBEN <- d$rawbenmax
  for (k in 1:9) for (p in c("ELEMENT", "NATURE", "AGENCY", "AMOUNT", "E_FINDG")) d[[paste0(p, k)]] <- NA_real_
  d$ELEMENT1 <- code; d$NATURE1 <- 35; d$AGENCY1 <- 1; d$E_FINDG1 <- 2
  d$AMTERR <- d$RAWBEN - d$FSBEN; d$AMOUNT1 <- d$AMTERR
  d$amount_pattern <- NA_character_; d$correctednotes <- "no_change"; d$correctedamount <- 0
  d$util_secondary_shift <- 0; d$homeless_secondary_shift <- 0; d$cascade_type <- NA_character_; d$cascade_hit_limit <- NA
  for (v in unique(step7_vars)) d[[v]] <- as.numeric(d[[c(rawearn = "FSEARN", rawunearn = "FSUNEARN", rawrent = "RENT",
                                         rawutil = "UTIL", rawmedded = "FSMEDDED", rawdepded = "FSDEPDED", rawcsded = "FSCSDED")[[v]]]])
  d <- recreate_elements(d)
  d$HHLDNO <- -seq_len(nrow(d))   # unique (negative) keys
  list(d = d, truth = truth, col = col, fs = fs, mech = mech, tries = tries)
}

pile_share <- function(x, fy, size) {   # share of rows in a (fy, size, amount) cell of >= 10 identical positive amounts
  k <- paste(fy, pmin(size, 6), x); cnt <- ave(rep(1, length(x)), k, FUN = sum)
  mean(x > 0 & cnt >= 10)
}
res <- list()
for (mech in c("donor_ratio", "half_zero_uniform")) for (spec in list(c("rawearn", "FSEARN", 311), c("rawunearn", "FSUNEARN", 331))) {
  s <- make_cases(spec[1], spec[2], as.numeric(spec[3]), mech)
  d <- s$d; col <- s$col
  stop_val <- d[[col]]
  reached <- d$correctednotes %in% paste0(sub("raw", "", col), "_down")
  combo <- bind_rows(corr, d)
  combo$imputed_at_bound <- NA_character_
  combo <- impute_at_bound(combo, "max")
  syn <- (nrow(corr) + 1):nrow(combo)
  imp <- combo[[col]][syn]
  tr <- s$truth
  f <- function(x) c(MAE = mean(abs(x - tr)), medAE = median(abs(x - tr)), bias = mean(x - tr),
                     zero_true = mean(tr == 0), zero_hit = mean((x == 0) == (tr == 0)),
                     KS = suppressWarnings(ks.test(x, tr)$statistic[[1]]),
                     pile = pile_share(x, d$fiscal_year, d$rawusize))
  out_row <- rbind(stop_point = f(stop_val), imputed = f(imp), truth = f(tr))
  cat(sprintf("\n== %s, %s: n=%d (tries %d), Step 7 reached %d, imputed %d ==\n", mech, col, nrow(d), s$tries,
              sum(reached), sum(!is.na(combo$imputed_at_bound[syn]))))
  print(round(out_row, 3))
  res[[paste(mech, col)]] <- data.frame(mech = mech, col = col, truth = tr, stop = stop_val, imputed = imp,
                                         imputed_flag = combo$imputed_at_bound[syn], fs = as.numeric(d[[s$fs]]))
}
saveRDS(res, paste0(out, "synthetic_bound_results.rds"))
