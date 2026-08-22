# Era-2 pre-launch check required by the 2026-08-21 review: on the
# FY2017-19 state-year cells, are utilities values integral, does any cell
# have a tied top count (mode_pos tie rule would fire), and is any mode
# <= 200 (tier 1 would be definitionally empty under the $200 band)?
suppressMessages(library(dplyr))
d <- readRDS("reg_model_data.rds") %>%
  filter(as.character(fiscal_year) %in% c("2017", "2018", "2019"),
         utilities > 0)
a <- d %>%
  group_by(state = as.character(state), fy = as.character(fiscal_year)) %>%
  summarise(integral = all(abs(utilities - round(utilities)) < 1e-9),
            tb = list(table(round(utilities))), .groups = "drop") %>%
  mutate(tied = vapply(tb, function(t) sum(t == max(t)) > 1, logical(1)),
         md = vapply(tb, function(t)
           as.numeric(names(sort(t, decreasing = TRUE))[1]), numeric(1)))
cat("FY2017-19 state-year cells:", nrow(a),
    "| non-integral:", sum(!a$integral),
    "| tied top count:", sum(a$tied),
    "| mode <= 200:", sum(a$md <= 200),
    "| mode range:", min(a$md), "-", max(a$md), "\n")
if (any(a$tied)) print(a[a$tied, c("state", "fy", "md")])
write.csv(a[, c("state", "fy", "integral", "tied", "md")],
          "methods/v250_benchmark_2024_utilrel/era2_precheck.csv",
          row.names = FALSE)
