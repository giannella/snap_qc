# Splits the gap between FNS published payment error rates and the case model's dollar error rate
# into (a) published rate vs the error dollars visible in the public QC file and (b) visible
# error dollars vs the model's predicted dollars, on each state's FY2024 cases, and relates the
# gap to the share of error cases each state's public file shows (methods/state_error_accounting/).
# Run from the repo root on the f1109 frame. Writes case_model_validation/rate_gap_fy2024.csv.
#   Rscript methods/case_predictions/rate_gap_fy2024.R
CPI_LAST <- 1.058   # the bundle's size predictions are in FY2026 dollars; / CPI_LAST puts them in FY2024 dollars
lv <- readRDS("methods/case_predictions/s46/out_f1109/deploy_B_bundle.rds")$levels
a <- readRDS("additional_data/reg_model_data.rds"); stopifnot(nrow(a) == 237557L)
a <- a[as.character(a$year) == "2024", ]
y <- !is.na(a$over_threshold) & a$over_threshold != 0
d <- ifelse(y, abs(ifelse(is.na(a$total_error_amount), 0, a$total_error_amount)), 0)
iss <- pmax(a$rawben, 0)
visible <- tapply(d, a$state, sum) / tapply(iss, a$state, sum)
vis <- read.csv("methods/state_error_accounting/visibility_by_state_2022_2024.csv")
m <- data.frame(state = lv$state, fns_fy2025 = lv$fns_rate_fy2025, visible_fy2024 = as.numeric(visible[lv$state]),
                model_fy2024 = lv$model_rate / CPI_LAST)
m$fns_over_model <- m$fns_fy2025 / m$model_fy2024
m$fns_over_visible <- m$fns_fy2025 / m$visible_fy2024
m$visible_over_model <- m$visible_fy2024 / m$model_fy2024
m <- merge(m, vis[, c("state", "pct_visible")], by = "state")
stopifnot(nrow(m) == 49L)
q <- function(v) sprintf("median %.2f (%.2f to %.2f)", median(v), min(v), max(v))
cat("FNS / model:  ", q(m$fns_over_model), "\n")
cat("FNS / visible:", q(m$fns_over_visible), "\n")
cat("visible / model:", q(m$visible_over_model), "\n")
cat(sprintf("Spearman, FNS/model vs share of error cases visible (FY2022-24): %.2f\n",
            cor(m$fns_over_model, m$pct_visible, method = "spearman")))
cat(sprintf("median 100 / share visible: %.2f\n", median(100 / m$pct_visible)))
write.csv(m, "methods/case_predictions/case_model_validation/rate_gap_fy2024.csv", row.names = FALSE)
