# Promotion check (2026-08-22): the frame's canonical utilities_sua
# (features.R add_sua_tier) must equal, row for row, the study-local
# construction the era-1 benchmark and the staged build used (mode_pos on
# the final `utilities` column, grouped by state_name x fiscal_year,
# band 200). If they agree on every row, the era-1 result and the staged
# lists are results FOR the promoted definition; if not, promotion
# changed the variable and everything downstream must be re-run.
suppressMessages(library(dplyr))
d <- readRDS("reg_model_data.rds")
stopifnot("utilities_sua" %in% names(d))
mode_pos <- function(x) {
  x <- round(x[x > 0])
  as.numeric(names(sort(table(x), decreasing = TRUE))[1])
}
s <- d %>% group_by(state_name, fiscal_year) %>%
  mutate(study_tier = ifelse(utilities <= 0, 0L,
                             ifelse(utilities < mode_pos(utilities) - 200,
                                    1L, 2L))) %>%
  ungroup()
eq <- s$utilities_sua == s$study_tier
cat(sprintf("rows: %d | identical: %d | differing: %d\n",
            nrow(s), sum(eq), sum(!eq)))
cat("frame tier distribution:",
    paste(names(table(d$utilities_sua)), table(d$utilities_sua),
          sep = "=", collapse = " "), "\n")
if (any(!eq)) {
  print(head(s[!eq, c("state_name", "fiscal_year", "utilities",
                      "utilities_sua", "study_tier")], 10))
  stop("canonical tier differs from the study construction")
}
cat("EQUIVALENT: promoted definition == study definition on every row\n")
