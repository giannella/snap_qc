# Independent R re-implementation of a state's delivery rules over
# reg_model_data.rds, for cross-checking the workbook's Summary (National)
# tab. Driven by crosscheck_rules.py; can also run standalone:
#
#   Rscript crosscheck_rules.R --state "Washington" --csv <delivery.csv> \
#           --out <per_rule.csv> [--years 2022,2023,2024] [--repo <snap_qc>] \
#           [--role core]
#
# Mirrors build_workbook_v2.py's py_score():
#   - a case counts for a rule only within the rule's hh stratum
#   - conditions AND'ed, NA -> FALSE
#   - error = over_threshold != 0 (non-NA); dollars = round(abs(total_error_amount))
suppressMessages(library(dplyr))

args <- commandArgs(trailingOnly = TRUE)
arg <- function(name, default = NULL) {
  i <- match(paste0("--", name), args)
  if (is.na(i) || i == length(args)) return(default)
  args[i + 1]
}
STATE <- arg("state"); CSV <- arg("csv"); OUT <- arg("out")
YEARS <- strsplit(arg("years", "2022,2023,2024"), ",")[[1]]
REPO  <- arg("repo", getwd())
ROLE  <- arg("role", "core")
if (is.null(STATE) || is.null(CSV) || is.null(OUT)) stop("need --state, --csv, --out")

setwd(REPO)
source("rule_mining_helpers.R")

FEATURES <- c(
  "HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size",
  "expedited_i", "bbce_state_i", "rawben_rel_max", "medical_deductions",
  "shelter_expenses_by_hh_size", "utilities", "married", "homeless",
  "earned_by_hh_size", "unearned_by_hh_size", "gross_by_hh_size",
  "percent_abawd", "unc_rawben_rel_max",
  "months_since_cert_n", "count_divisible_by_100"
)

d <- readRDS("reg_model_data.rds") %>%
  filter(as.character(state) == STATE, as.character(fiscal_year) %in% YEARS)
pf <- prep_features(d, FEATURES)
w  <- pf$data
for (v in setdiff(intersect(FEATURES, names(w)), pf$features))
  if (is.logical(w[[v]])) w[[v]] <- as.integer(w[[v]])

hh_group <- with(w, {
  n <- suppressWarnings(as.numeric(as.character(cert_HH_size_FS_n)))
  ifelse(is.na(n), NA_character_, ifelse(n <= 1, "1", ifelse(n <= 3, "2-3", "4+")))
})
stopifnot(!any(is.na(hh_group)))
is_err <- !is.na(w$over_threshold) & w$over_threshold != 0
amt    <- round(abs(ifelse(is.na(w$total_error_amount), 0, w$total_error_amount)))
ed     <- ifelse(is_err, amt, 0)

# ROLE "all" takes every row: the workbook's effective-rules CSV
# (.build/effective_rules_<ABBR>.csv) already carries exactly the rules on
# the tab (transformed core + promoted buffer), in tab order
rules <- read.csv(CSV, stringsAsFactors = FALSE)
if (ROLE != "all") rules <- rules %>% filter(role == ROLE) %>% arrange(rank)
cat(sprintf("frame: %d rows, %d errors | rules: %d %s\n",
            nrow(w), sum(is_err), nrow(rules), ROLE))

parse_rule <- function(txt) {
  pat <- "([A-Za-z_][A-Za-z0-9_]*)\\s*(>=|<=|>|<|==)\\s*(-?[0-9.]+)"
  parts <- regmatches(txt, gregexpr(pat, txt, perl = TRUE))[[1]]
  lapply(parts, function(p) {
    mm <- regmatches(p, regexec(pat, p))[[1]]
    list(var = mm[2], op = mm[3], thr = as.numeric(mm[4]))
  })
}

rule_mask <- function(conds, hh) {
  m <- hh_group == hh
  for (c in conds) {
    x <- as.numeric(w[[c$var]])
    cm <- switch(c$op,
                 ">=" = x >= c$thr, ">" = x > c$thr,
                 "<=" = x <= c$thr, "<" = x < c$thr, "==" = x == c$thr)
    cm[is.na(cm)] <- FALSE
    m <- m & cm
  }
  m
}

res <- vector("list", nrow(rules))
union_mask <- rep(FALSE, nrow(w))
for (i in seq_len(nrow(rules))) {
  conds <- parse_rule(rules$rule[i])
  m <- rule_mask(conds, as.character(rules$hh[i]))
  union_mask <- union_mask | m
  in_hh <- hh_group == as.character(rules$hh[i])
  res[[i]] <- data.frame(
    rank = rules$rank[i], hh = rules$hh[i],
    n_flagged = sum(m), errors = sum(m & is_err), dollars = sum(ed[m]),
    precision = ifelse(sum(m) > 0, sum(m & is_err) / sum(m), 0),
    # per-rule recall / $ recall use GRAND totals (all strata) since
    # 2026-08-18, matching build_workbook_v2.score_list; workload stays
    # within the rule's stratum
    recall = sum(m & is_err) / max(sum(is_err), 1),
    dollar_recall = sum(ed[m]) / max(sum(ed), 1e-9),
    workload = sum(m) / max(sum(in_hh), 1))
}
write.csv(bind_rows(res), OUT, row.names = FALSE)

un <- function(sel) data.frame(
  flagged = sum(union_mask & sel), errors = sum(union_mask & sel & is_err),
  dollars = sum(ed[union_mask & sel]),
  tot_cases = sum(sel), tot_err = sum(is_err & sel), tot_ed = sum(ed[sel]))
u <- rbind(cbind(scope = "all", un(rep(TRUE, nrow(w)))),
           cbind(scope = "1",   un(hh_group == "1")),
           cbind(scope = "2-3", un(hh_group == "2-3")),
           cbind(scope = "4+",  un(hh_group == "4+")))
write.csv(u, sub("\\.csv$", "_union.csv", OUT), row.names = FALSE)
print(u)
cat("wrote", OUT, "\n")
