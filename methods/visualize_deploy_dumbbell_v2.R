# Dumbbell version of the national-deployment dot plot (revD deck).
# For each of the 18 benchmark states: a segment from the state's 2024 base
# error rate (open dot = what unaided review of the same volume gets) to the
# precision the national rules delivered at a 10% review budget on 2024.
#
# Reads methods/state_similarity_v2/transfer_benchmark_train2223_test24/
#   deployment_menu_train2223_test24.csv  (12 benchmark states)
#   deployment_menu_workshop_extension.csv (6 workshop states)
# Writes deploy_national_dumbbell_budget10.png to the same folder.

suppressMessages({library(dplyr); library(ggplot2)})
source("rule_mining_helpers.R")
out <- "methods/state_similarity_v2/transfer_benchmark_train2223_test24"

d <- bind_rows(
  read.csv(file.path(out, "deployment_menu_train2223_test24.csv"),
           stringsAsFactors = FALSE),
  read.csv(file.path(out, "deployment_menu_workshop_extension.csv"),
           stringsAsFactors = FALSE)
) %>%
  filter(approach == "national_all", budget == 0.1)

stopifnot(nrow(d) == 18)
d$target <- factor(d$target, levels = rev(sort(unique(d$target))))

p <- ggplot(d, aes(y = target)) +
  geom_segment(aes(x = target_base_rate, xend = precision,
                   y = target, yend = target), colour = "grey60") +
  geom_point(aes(x = target_base_rate), shape = 1, size = 2.6, colour = "grey30") +
  geom_point(aes(x = precision), shape = 16, size = 2.6, colour = "black") +
  annotate("text", x = 0.245, y = "Texas", hjust = 0, vjust = 0.5,
           size = 3.0, colour = "grey25", lineheight = 1.05,
           label = "open dot = what unaided review of\nthe same volume gets (base error rate)\nfilled dot = the national rules") +
  labs(x = "share of reviewed cases with a payment error, state's 2024 cases",
       y = NULL,
       title = "National rules at a 10% review budget, tested on 2024",
       subtitle = "rules mined on all states' 2022-23 data; the segment is each state's\ngain over unaided review of the same number of cases (1.5-3.4x)") +
  scale_x_continuous(limits = c(0, 0.46)) +
  theme_minimal(base_size = 12.5) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())

save_png(p, file.path(out, "deploy_national_dumbbell_budget10.png"), 6.6, 5.6)
cat("wrote", file.path(out, "deploy_national_dumbbell_budget10.png"), "\n")
