# Munging review 2026-10-30 (EXPLORATORY): synthetic two-element errors with known raw values.
# Takes clean cases (raw = FS), injects errors in two Step 7 variables, writes the synthetic RAWBEN, AMOUNT1-2,
# E_FINDG1-2 and NATURE1-2 under several AMOUNT reporting patterns, runs the tracked recreate_elements(), and
# measures recovery of the injected raw values. Reads the tracked script and the all_years.rds checkpoint;
# writes ONLY to methods/munging_review_2026-10-30/scratch/.
#   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" methods/munging_review_2026-10-30/synthetic_multielement.R [n]
args <- commandArgs(TRUE)
n_cases <- if (length(args)) as.integer(args[1]) else 600
setwd("C:/Users/ericg/snap_qc_v27")
out <- "methods/munging_review_2026-10-30/scratch/"
src <- readLines("1_data_munging_and_raw_variable_reconstruction_for_using_public_qc_data.R")
call_line <- grep("^  mydata <- recreate_elements\\(mydata\\)", src)
stopifnot(length(call_line) == 1)
# libraries/flags (lines up to "# 1. Load data"), then the checkpoint, then filters through Step 6 / Step 7 defs
load_line <- grep("^# 1. Load data", src); filt_line <- grep("^# 2. Filter dataset", src)
pre_file <- paste0(out, "pre7.rds")
if (!file.exists(pre_file)) {
  eval(parse(text = src[1:(load_line - 1)]))
  folder <- paste0(here::here(), "/")
  source("features.R")
  mydata <- readRDS(paste0(folder, "all_years.rds"))
  body <- src[filt_line:(call_line - 1)]
  stopifnot(!any(grepl("saveRDS|write\\.csv|writeLines", body)))
  eval(parse(text = c(body, "}")))   # close the open if (correct_variables) block
  saveRDS(mydata, pre_file)
  save.image(paste0(out, "pre7_env.RData"))
} else {
  load(paste0(out, "pre7_env.RData"))
}
mydata <- calculate_raw_benefits(mydata)
cat("pre-Step-7 rows:", nrow(mydata), "\n")

# ---- donor distributions for realistic error sizes: identified single-variable errors from corrected.rds
corr <- readRDS("corrected.rds")
single_ratio <- function(v, fs, note_re) {
  r <- which(grepl(note_re, corr$correctednotes) & !grepl("\\+|_error", corr$correctednotes) & is.na(corr$ELEMENT2))
  x <- as.numeric(corr[[v]][r]) / as.numeric(corr[[fs]][r]); x[is.finite(x) & x > 0 & x != 1]
}
ratios <- list(rawearn = single_ratio("rawearn", "FSEARN", "^earn_(up|down)$"),
               rawunearn = single_ratio("rawunearn", "FSUNEARN", "^unearn_(up|down)$"),
               rawrent = single_ratio("rawrent", "RENT", "^rent_(up|down)$"))
for (v in names(ratios)) cat(v, "donor ratios:", length(ratios[[v]]), "median", round(median(ratios[[v]]), 3), "\n")
rm(corr)

set.seed(20261030)
pool <- which(is.na(mydata$ELEMENT1) & mydata$STATUS %in% 1 & (mydata$rawben_recreated == mydata$RAWBEN) %in% TRUE &
                mydata$rawhomeless_ded == 0 & mydata$correctednotes == "no_change" &
                mydata$RAWBEN > mydata$rawminimum_ben & mydata$RAWBEN < mydata$rawbenmax)
cat("clean pool (between the limits):", length(pool), "\n")
code <- c(rawearn = 311, rawunearn = 331, rawrent = 363, rawutil = 364)
pairs <- list(c("rawearn", "rawunearn"), c("rawearn", "rawrent"), c("rawunearn", "rawrent"),
              c("rawearn", "rawutil"), c("rawunearn", "rawutil"), c("rawrent", "rawutil"))
fs_of <- c(rawearn = "FSEARN", rawunearn = "FSUNEARN", rawrent = "RENT", rawutil = "UTIL")

draw_true <- function(i, v) {
  cur <- mydata[[v]][i]
  if (v == "rawutil") {
    t <- tryCatch(util_tiers(mydata$state_name[i], mydata$year[i], mydata$month[i], mydata$rawusize[i]), error = function(e) numeric(0))
    t <- t[abs(t - cur) >= util_min_step]
    return(if (length(t)) t[sample.int(length(t), 1)] else NA)
  }
  if (cur <= 0) return(NA)
  max(0, round(cur * ratios[[v]][sample.int(length(ratios[[v]]), 1)]))
}
ben_of <- function(rows, vals) {   # benefit with the listed variables set
  bi <- lapply(mydata[rows, benefit_inputs], as.numeric)
  for (v in names(vals)) bi[[v]] <- vals[[v]]
  calculate_raw_benefits(bi)$rawben_recreated
}

# Build the synthetic cases
cases <- list(); tries <- 0
while (length(cases) < n_cases && tries < 20 * n_cases) {
  tries <- tries + 1
  i <- pool[sample.int(length(pool), 1)]
  pr <- pairs[[sample.int(length(pairs), 1)]]
  tv <- setNames(vapply(pr, function(v) draw_true(i, v), numeric(1)), pr)
  if (anyNA(tv)) next
  fsb <- mydata$RAWBEN[i]
  b1 <- ben_of(i, tv[1]); b2 <- ben_of(i, tv[2]); b12 <- ben_of(i, as.list(tv))
  if (b12 == fsb || b1 == fsb || b2 == fsb) next            # each element must change the benefit
  cases[[length(cases) + 1]] <- list(i = i, v = pr, tv = tv, fsb = fsb, raw = b12, d1 = b1 - fsb, d2 = b12 - b1)
}
cat("synthetic cases:", length(cases), "after", tries, "tries\n")

nat_for <- function(v, true, fs) {
  inc <- v %in% c("rawearn", "rawunearn")
  if (inc) (if (true < fs) 35 else 44) else (if (true < fs) 52 else 53)
}
build <- function(pattern, natures = TRUE) {
  rows <- vapply(cases, `[[`, numeric(1), "i")
  d <- mydata[rows, ]
  for (k in 1:9) for (p in c("ELEMENT", "NATURE", "AGENCY", "AMOUNT", "E_FINDG")) d[[paste0(p, k)]] <- NA_real_
  for (j in seq_along(cases)) {
    cs <- cases[[j]]
    d$RAWBEN[j] <- cs$raw; d$FSBEN[j] <- cs$fsb
    net <- cs$raw - cs$fsb; d$AMTERR[j] <- abs(net)
    eff <- c(cs$d1, cs$d2)
    for (k in 1:2) {
      v <- cs$v[k]
      d[[paste0("ELEMENT", k)]][j] <- code[[v]]
      d[[paste0("NATURE", k)]][j] <- if (natures) nat_for(v, cs$tv[[k]], mydata[[v]][cs$i]) else 0
      d[[paste0("AGENCY", k)]][j] <- 1
      own <- eff[k]
      d[[paste0("E_FINDG", k)]][j] <- switch(pattern, copied = , all_zero = , amount1_only = , even_split = if (net > 0) 2 else 3,
                                             if (own > 0) 2 else 3)
      d[[paste0("AMOUNT", k)]][j] <- switch(pattern,
        clean_split = abs(own), copied = abs(net), all_zero = 0,
        amount1_only = if (k == 1) abs(net) else 0, even_split = floor(abs(net) / 2))
    }
  }
  d$amount_pattern <- amount_pattern(d)
  d$correctednotes <- "no_change"; d$correctedamount <- 0
  d$util_secondary_shift <- 0; d$homeless_secondary_shift <- 0
  d$cascade_type <- NA_character_; d$cascade_hit_limit <- NA
  d
}

score <- function(d, r, label) {
  res <- data.frame(variant = label, case = seq_along(cases),
                    pair = vapply(cases, function(c) paste(sub("raw", "", c$v), collapse = "+"), ""),
                    pattern_seen = d$amount_pattern, cascade = r$cascade_type,
                    reached = (abs(r$rawben_recreated - r$RAWBEN) <= 0),
                    reached5 = (abs(r$rawben_recreated - r$RAWBEN) <= 5),
                    note = r$correctednotes, stringsAsFactors = FALSE)
  for (k in 1:2) {
    tv <- vapply(cases, function(c) c$tv[[k]], 0)
    fs <- vapply(cases, function(c) mydata[[c$v[k]]][c$i], 0)
    rv <- vapply(seq_along(cases), function(j) r[[cases[[j]]$v[k]]][j], 0)
    res[[paste0("true", k)]] <- tv; res[[paste0("fs", k)]] <- fs; res[[paste0("rec", k)]] <- rv
    res[[paste0("abserr", k)]] <- abs(rv - tv)
    res[[paste0("ok", k)]] <- abs(rv - tv) <= pmax(10, 0.02 * abs(tv))   # within $10 or 2%
    res[[paste0("movedright", k)]] <- sign(rv - fs) == sign(tv - fs)
  }
  res
}

t0 <- Sys.time()
all <- list()
for (pt in c("clean_split", "copied", "all_zero", "amount1_only", "even_split")) {
  d <- build(pt)
  r <- recreate_elements(d)
  all[[pt]] <- score(d, r, pt)
  cat(pt, "done", format(Sys.time() - t0), "\n")
}
d <- build("all_zero", natures = FALSE); r <- recreate_elements(d); all[["all_zero_nonature"]] <- score(d, r, "all_zero_nonature")
res <- do.call(rbind, all)
# Baseline: no reconstruction (raw = FS)
base <- all[[1]]; base$variant <- "no_reconstruction"; base$rec1 <- base$fs1; base$rec2 <- base$fs2
base$abserr1 <- abs(base$fs1 - base$true1); base$abserr2 <- abs(base$fs2 - base$true2)
base$ok1 <- base$abserr1 <= pmax(10, 0.02 * abs(base$true1)); base$ok2 <- base$abserr2 <= pmax(10, 0.02 * abs(base$true2))
base$reached <- FALSE; base$reached5 <- FALSE; base$movedright1 <- FALSE; base$movedright2 <- FALSE
res <- rbind(res, base)
saveRDS(list(cases = cases, res = res), paste0(out, "synthetic_multielement_results.rds"))
write.csv(res, paste0(out, "synthetic_multielement_results.csv"), row.names = FALSE)

library(dplyr)
summ <- res %>% group_by(variant) %>% summarise(n = n(), reached = round(100 * mean(reached), 1),
  both_ok = round(100 * mean(ok1 & ok2), 1), slot1_ok = round(100 * mean(ok1), 1), slot2_ok = round(100 * mean(ok2), 1),
  both_dir = round(100 * mean(movedright1 & movedright2), 1),
  med_err1 = median(abserr1), med_err2 = median(abserr2),
  mean_err = round(mean(c(abserr1, abserr2))), .groups = "drop")
cat("\n== Recovery of injected raw values (ok = within $10 or 2%) ==\n"); print(as.data.frame(summ), row.names = FALSE)
cat("\npattern that amount_pattern() assigns to each injected variant:\n"); print(table(res$variant, res$pattern_seen, useNA = "ifany"))
cat("\nboth_ok by pair x variant:\n")
print(round(100 * tapply(res$ok1 & res$ok2, list(res$pair, res$variant), mean)))
cat("\nreached RAWBEN but wrong values (reached & !(ok1&ok2)), % of cases:\n")
print(round(100 * tapply(res$reached & !(res$ok1 & res$ok2), res$variant, mean), 1))
