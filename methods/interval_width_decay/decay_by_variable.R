# EXPLORATORY decay-by-variable cut (2026-08-21): a secondary read of the
# reviewed interval-width evaluation's per-rule artifacts (design_note.md,
# section 40) — no new evaluation, no re-mining. For each vocabulary
# variable, how do rules that USE it decay held-out vs rules that do not,
# at matched train support (n in [30, 300], the section-40 band)?
# Motivating question: utilities thresholds are drift-sensitive by
# construction (tier levels reset yearly); does that show up in held-out
# performance? Caveat stated up front: rules use 1-4 variables, so the
# per-variable groups OVERLAP — this is attribution by membership, not a
# decomposition, and any signal here feeds a designed study, not a decision.
suppressMessages(library(dplyr))

OUT_DIR <- "methods/interval_width_decay"
COND_PAT <- "([A-Za-z_][A-Za-z0-9_]*)\\s*(>=|<=|>|<)\\s*(-?[0-9.]+)"

vars_of <- function(txt)
  unique(regmatches(txt, gregexpr(COND_PAT, txt, perl = TRUE))[[1]] |>
           sub(pattern = "\\s*(>=|<=|>|<).*$", replacement = ""))

cut_one <- function(csv, era_label) {
  d <- read.csv(csv, stringsAsFactors = FALSE) %>%
    filter(n >= 30, n <= 300) %>%
    mutate(prec24 = ifelse(n24 >= 10, k24 / n24, NA),
           d_raw = prec24 - prec_train,
           d_lcb = prec24 - lcb)
  vl <- lapply(d$rule, vars_of)
  allv <- sort(unique(unlist(vl)))
  base <- tibble(variable = "(ALL BAND RULES)", era = era_label,
                 rules = nrow(d),
                 med_train_n = median(d$n),
                 reach_collapse = mean(d$n24 < 10),
                 med_d_raw = median(d$d_raw, na.rm = TRUE),
                 med_d_lcb = median(d$d_lcb, na.rm = TRUE),
                 med_d_raw_others = NA_real_)
  rows <- lapply(allv, function(v) {
    use <- vapply(vl, function(x) v %in% x, logical(1))
    if (sum(use) < 30) return(NULL)      # support floor for a readable median
    tibble(variable = v, era = era_label,
           rules = sum(use),
           med_train_n = median(d$n[use]),
           reach_collapse = mean(d$n24[use] < 10),
           med_d_raw = median(d$d_raw[use], na.rm = TRUE),
           med_d_lcb = median(d$d_lcb[use], na.rm = TRUE),
           med_d_raw_others = median(d$d_raw[!use], na.rm = TRUE))
  })
  bind_rows(c(list(base), rows))
}

e1 <- cut_one(file.path(OUT_DIR, "per_rule_decay_2024.csv"), "2022-23 -> 2024")
e2 <- cut_one(file.path(OUT_DIR, "per_rule_decay_era2.csv"), "2017-18 -> 2019")
out <- bind_rows(e1, e2) %>%
  mutate(excess_vs_others = med_d_raw - med_d_raw_others)

for (era in unique(out$era)) {
  cat(sprintf("\n== era %s (train-n band [30,300]; groups overlap) ==\n", era))
  tb <- out %>% filter(era == !!era) %>%
    arrange(is.na(excess_vs_others), med_d_raw) %>%
    select(-era)
  print(as.data.frame(tb), digits = 3, row.names = FALSE)
}
write.csv(out, file.path(OUT_DIR, "decay_by_variable_exploratory.csv"),
          row.names = FALSE)
cat("\nEXPLORATORY: groups overlap (rules carry 1-4 variables); any signal\n")
cat("here motivates a designed study, not a vocabulary decision.\n")
cat("written:", file.path(OUT_DIR, "decay_by_variable_exploratory.csv"), "\n")
