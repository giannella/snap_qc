# What does the BENMAX == rawbenmax filter cost, and is it recoverable?
#
# rerun_munging_min_exclusions.R relaxed three exclusions and missed a fourth:
# line ~367 of the munging script drops every row where the file's own BENMAX
# disagrees with the max allotment looked up from additional_data. Its comment
# names the reason ("Some states included the additional 2021 pandemic 15%
# allotment boost / Drop these rows"), and it is why the rebuilt frame has no
# FY2021 at all.
#
# This runs the munging script only as far as that filter, with the same three
# relaxations, and reports what the filter removes: how many rows, from which
# years, and whether BENMAX/rawbenmax is the 1.15 pandemic ratio (recoverable by
# using the file's own BENMAX) or something else (not recoverable).
#
#   Rscript methods/measure_benmax_filter.R

SCRIPT <- "1_data_munging_and_raw_variable_reconstruction_for_using_public_qc_data.R"
if (!file.exists(SCRIPT)) stop("run from the snap_qc repo root")
txt <- paste(readLines(SCRIPT, warn = FALSE), collapse = "\n")

CUT <- "mydata <- mydata[mydata$BENMAX == mydata$rawbenmax, ]"
if (length(gregexpr(CUT, txt, fixed = TRUE)[[1]]) != 1) stop("BENMAX filter not found once")

subs <- list(
  c("exclude_2020_2021 <- TRUE", "exclude_2020_2021 <- FALSE"),
  c("mydata <- mydata[abs(mydata$absbendiff - mydata$AMTERR) <= 5, ]",
    "mydata$amterr_reconciles <- !is.na(mydata$absbendiff) & !is.na(mydata$AMTERR) & abs(mydata$absbendiff - mydata$AMTERR) <= 5"),
  c("mydata <- mydata %>%\n  filter(!is.na(RENT), !is.na(UTIL))",
    "mydata$RENT[is.na(mydata$RENT)] <- 0; mydata$UTIL[is.na(mydata$UTIL)] <- 0")
)
for (s in subs) {
  if (length(gregexpr(s[1], txt, fixed = TRUE)[[1]]) != 1 ||
      gregexpr(s[1], txt, fixed = TRUE)[[1]][1] == -1)
    stop("substitution did not match once: ", substr(s[1], 1, 50))
  txt <- sub(s[1], s[2], txt, fixed = TRUE)
}

# stop at the filter and report instead of applying it
head_txt <- substr(txt, 1, gregexpr(CUT, txt, fixed = TRUE)[[1]][1] - 1)
report <- '
cat("\\n================ BENMAX FILTER ================\\n")
cat("rows reaching the filter:", nrow(mydata), "\\n")
drop <- !(mydata$BENMAX == mydata$rawbenmax)
drop[is.na(drop)] <- TRUE
mydata$is_err <- !is.na(mydata$over_threshold) & mydata$over_threshold != 0
cat("rows the filter DROPS:", sum(drop), sprintf("(%.1f%%)", 100*mean(drop)), "\\n")
cat("errors among them:", sum(mydata$is_err[drop]),
    sprintf("(%.1f%% of the %d errors present)", 100*sum(mydata$is_err[drop])/sum(mydata$is_err),
            sum(mydata$is_err)), "\\n")
cat("\\ndropped rows by fiscal year (and share of that year):\\n")
print(round(rbind(dropped = tapply(drop, mydata$fiscal_year, sum),
                  total   = tapply(drop, mydata$fiscal_year, length),
                  pct     = 100*tapply(drop, mydata$fiscal_year, mean)), 1))
cat("\\nBENMAX / rawbenmax on the dropped rows (is it the 1.15 pandemic boost?):\\n")
r <- round(mydata$BENMAX[drop] / mydata$rawbenmax[drop], 3)
print(head(sort(table(r), decreasing = TRUE), 12))
cat("\\nshare of dropped rows at exactly 1.15:", sprintf("%.1f%%", 100*mean(r == 1.15, na.rm = TRUE)), "\\n")
cat("share where rawbenmax is NA:", sprintf("%.1f%%", 100*mean(is.na(mydata$rawbenmax[drop]))), "\\n")
cat("\\ndropped-row ratio by fiscal year, share at 1.15:\\n")
print(round(tapply(r == 1.15, mydata$fiscal_year[drop], function(x) 100*mean(x, na.rm=TRUE)), 1))
cat("==============================================\\n")
'
env <- new.env(parent = globalenv())
eval(parse(text = paste0(head_txt, report)), envir = env)
