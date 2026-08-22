# EXPLORATORY anchor-translation test (2026-08-21): before mining a
# relative utilities encoding, which state-year anchor (max vs mode of
# positive utilities) better preserves EXISTING utilities rules' reach and
# precision across the year boundary? Equivalence used: evaluating a rule
# with its threshold re-anchored by t * anchor_test/anchor_train is the
# same as deflating the test year's utilities by anchor_train/anchor_test
# and evaluating the rule unchanged. Three arms = three test-year utilities
# columns (raw dollars / max-deflated / mode-deflated); the fixed era-1
# pool and FY2024 held-out slice are the section-40 machinery. Train
# anchor = FY2023 (the later train year). No mining; feeds the choice of
# tonight's variant, not a shipping decision.
suppressMessages(library(dplyr))

POOL <- "methods/v250_benchmark_2024/cache/bench_national_117.rds"
OUT_DIR <- "methods/interval_width_decay"
COND_PAT <- "([A-Za-z_][A-Za-z0-9_]*)\\s*(>=|<=|>|<)\\s*(-?[0-9.]+)"
parse_conds <- function(txt) {
  parts <- regmatches(txt, gregexpr(COND_PAT, txt, perl = TRUE))[[1]]
  if (!length(parts)) return(NULL)
  do.call(rbind, lapply(parts, function(p) {
    mm <- regmatches(p, regexec(COND_PAT, p))[[1]]
    data.frame(var = mm[2], op = mm[3], thr = as.numeric(mm[4]))
  }))
}

pool <- readRDS(POOL)
uses_util <- vapply(pool$rule, function(r) {
  cc <- parse_conds(r); !is.null(cc) && "utilities" %in% cc$var
}, logical(1))
pool <- pool[uses_util, ]
cat(sprintf("utilities-conditioning pool rules: %d\n", nrow(pool)))

d <- readRDS("reg_model_data.rds")
anchors <- d %>%
  filter(as.character(fiscal_year) %in% c("2023", "2024"), utilities > 0) %>%
  group_by(state = as.character(state), fy = as.character(fiscal_year)) %>%
  summarise(mx = max(utilities),
            md = as.numeric(names(sort(table(round(utilities)),
                                       decreasing = TRUE))[1]),
            .groups = "drop") %>%
  tidyr::pivot_wider(names_from = fy, values_from = c(mx, md))

h <- d %>% filter(as.character(fiscal_year) == "2024")
hh_num <- suppressWarnings(as.numeric(as.character(h$cert_HH_size_FS_n)))
h$hh_group <- ifelse(hh_num <= 1, "1", ifelse(hh_num <= 3, "2-3", "4+"))
is_err <- !is.na(h$over_threshold) & h$over_threshold != 0
h <- h %>% left_join(anchors, by = c("state"))
stopifnot(!any(is.na(h$mx_2023)), !any(is.na(h$md_2023)))
cat(sprintf("held-out FY2024: %d rows, %d errors\n", nrow(h), sum(is_err)))

ARMS <- list(
  raw  = h$utilities,
  maxA = h$utilities * (h$mx_2023 / h$mx_2024),
  modeA = h$utilities * (h$md_2023 / h$md_2024))

vars_used <- sort(unique(unlist(lapply(pool$rule, function(r)
  unique(parse_conds(r)$var)))))
X <- lapply(setNames(vars_used, vars_used), function(v) {
  x <- h[[v]]
  if (is.factor(x) || is.character(x)) x <- as.character(x) %in% c("TRUE", "1")
  suppressWarnings(as.numeric(x))
})
hh_vec <- h$hh_group

res <- list()
for (arm in names(ARMS)) {
  Xa <- X; Xa[["utilities"]] <- ARMS[[arm]]
  n24 <- k24 <- integer(nrow(pool))
  for (i in seq_len(nrow(pool))) {
    cc <- parse_conds(pool$rule[i])
    m <- hh_vec == as.character(pool$hh[i])
    for (j in seq_len(nrow(cc))) {
      x <- Xa[[cc$var[j]]]
      cm <- switch(cc$op[j],
                   ">=" = x >= cc$thr[j], ">" = x > cc$thr[j],
                   "<=" = x <= cc$thr[j], "<" = x < cc$thr[j])
      cm[is.na(cm)] <- FALSE
      m <- m & cm
    }
    n24[i] <- sum(m); k24[i] <- sum(m & is_err)
  }
  res[[arm]] <- data.frame(arm = arm, hh = pool$hh, rule = pool$rule,
                           n = pool$n, k = pool$k, lcb = pool$lcb,
                           n24 = n24, k24 = k24)
}
out <- bind_rows(res) %>%
  mutate(prec_train = k / n,
         prec24 = ifelse(n24 >= 10, k24 / n24, NA),
         d_raw = prec24 - prec_train)
write.csv(out, file.path(OUT_DIR, "anchor_translation_per_rule_2024.csv"),
          row.names = FALSE)

cat("\ntrain-n band [30,300], utilities-conditioning rules, by arm:\n")
band <- out %>% filter(n >= 30, n <= 300) %>%
  group_by(arm) %>%
  summarise(rules = n(),
            reach_collapse = mean(n24 < 10),
            med_n24_ratio = median(n24 / pmax(n, 1)),
            med_d_raw = median(d_raw, na.rm = TRUE),
            mean_d_raw = mean(d_raw, na.rm = TRUE),
            .groups = "drop")
print(as.data.frame(band), digits = 3)
write.csv(band, file.path(OUT_DIR, "anchor_translation_summary_2024.csv"),
          row.names = FALSE)
cat("\nEXPLORATORY: picks tonight's variant anchor; ships nothing.\n")
