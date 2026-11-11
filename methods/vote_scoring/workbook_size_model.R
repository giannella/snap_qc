# Error-size model for the state workbooks' vote score (2026-10-28): a gamma
# regression (log link) of error dollars on log issued benefit and household
# size (6+ pooled), fitted on every state's FY2022-24 over-threshold errors,
# the years the delivered lists were mined on. The study's FY2019 error-size
# model also carries the vote, but its vote counts a different rule set (300
# national representative rules), so the workbook uses the benefit-and-size
# form, the runner-up on FY2019 (design_note.md, "Exploration result").
#   Rscript methods/vote_scoring/workbook_size_model.R
# Writes methods/vote_scoring/workbook_size_coefs_fy2022_24.csv.
setwd("C:/Users/ericg/snap_qc")
d <- readRDS("reg_model_data.rds")
d <- d[as.character(d$fiscal_year) %in% c("2022", "2023", "2024"), ]
num <- function(x) { x <- suppressWarnings(as.numeric(as.character(x))); x[is.na(x)] <- 0; x }
ie <- !is.na(d$over_threshold) & d$over_threshold != 0
ed <- abs(num(d$total_error_amount)); ben <- num(d$rawben); hs <- num(d$cert_HH_size_FS_n)   # the workbook's HOUSEHOLD_SIZE (export_state_frame.R)
x <- data.frame(ed = ed, lben = log(pmax(ben, 1)),
                hsz = factor(ifelse(hs >= 6, "6+", as.character(pmax(1, hs))), levels = c("1", "2", "3", "4", "5", "6+")))
x <- x[ie & ed > 0, ]
m <- glm(ed ~ lben + hsz, family = Gamma(link = "log"), data = x, control = glm.control(maxit = 100))
cat(sprintf("FY2022-24 over-threshold errors: %d, mean $%.1f\n", nrow(x), mean(x$ed)))
print(round(coef(m), 5))
write.csv(data.frame(term = names(coef(m)), estimate = unname(coef(m))),
          "methods/vote_scoring/workbook_size_coefs_fy2022_24.csv", row.names = FALSE)
