# Era-2 pre-launch check (review 2026-08-22): does the frame's bbce_state_i
# on FY2017-19 agree with the USDA state-options panel's BBCE status (the
# 2018 edition reports options as of Oct 1 2017, i.e. FY2018)?
suppressMessages(library(dplyr))
d <- readRDS("reg_model_data.rds") %>%
  filter(as.character(fiscal_year) %in% c("2017", "2018", "2019"))
fl <- d %>%
  group_by(state = as.character(state_name), fy = as.character(fiscal_year)) %>%
  summarise(bbce_flag = as.integer(mean(as.integer(bbce_state_i)) >= 0.5),
            share_cat = mean(as.integer(cat_elig) >= 1, na.rm = TRUE),
            .groups = "drop")
so <- read.csv("additional_data/snap_state_options_all_years.csv",
               stringsAsFactors = FALSE)
e18 <- so[so$Year == 2018, c("State", "BBCE")]
e18$usda_bbce <- as.integer(!grepl("no", e18$BBCE, ignore.case = TRUE))
m <- fl %>% filter(fy == "2018") %>%
  inner_join(e18, by = c("state" = "State"))
cat("FY2018 state-years matched to the 2018 USDA edition:", nrow(m), "\n")
cat("agree:", sum(m$bbce_flag == m$usda_bbce), "| disagree:",
    sum(m$bbce_flag != m$usda_bbce), "\n")
dis <- m[m$bbce_flag != m$usda_bbce, c("state", "bbce_flag", "share_cat", "BBCE")]
if (nrow(dis)) print(as.data.frame(dis), digits = 3)
write.csv(m, "methods/v250_benchmark_2024_utilrel/era2_bbce_check.csv",
          row.names = FALSE)
