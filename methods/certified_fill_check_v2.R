# Does the certified, pruned fill reproduce the shipped delivery lists exactly?
#
# Findings 27 established that the fill reads only as far down the ranked pool as
# it needs, deepest rank 9,072 of 39,807 across the 49 shipped lists, and that a
# window is safe when the walk exhausts its capacity inside it (zero "slack").
# That was measured by counting ranks. This runs the actual fill on a 20,000-rule
# window for every state and both budgets, and checks two things:
#
#   1. slack == 0, the certificate that no rule below the window could have
#      entered, so the pruned answer is the full-pool answer;
#   2. the resulting list is identical, rule for rule and role for role, to the
#      committed CSV in state_delivery_lists/.
#
# The second is the production-relevant claim: pruning is an evaluation
# optimisation, not a change to what a state receives.
#
# Reads the pool caches and the delivered lists. No mining. Expects
# `reg_model_data`.
suppressMessages(library(dplyr))
source("rule_mining_helpers.R")

YEARS   <- c("2022", "2023", "2024")
BUDGETS <- c(0.05, 0.10)
BUFFER_MULT <- 3
if (!exists("TOPK")) TOPK <- 20000
POOL <- "methods/delivery_pools_2022_2024_v3"
LIST <- "state_delivery_lists"
OUT  <- "methods/anyerror_blended_holdout_2024/certified_fill_check.csv"

HH_LEVELS <- c("1", "2-3", "4+")
hh_group_of <- function(n) {
  n <- suppressWarnings(as.numeric(as.character(n)))
  ifelse(is.na(n), NA_character_, ifelse(n <= 1, "1", ifelse(n <= 3, "2-3", "4+")))
}
features <- c(
  "HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size",
  "expedited_i", "cat_elig", "rawben_rel_max", "medical_deductions",
  "shelter_expenses_by_hh_size", "utilities", "married", "homeless",
  "rawearn_by_hh_size", "rawunearn_by_hh_size", "rawgross_by_hh_size",
  "percent_abawd", "unc_rawben_rel_max",
  "months_since_cert_n", "count_divisible_by_100"
)
stamp <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"),
                                   sprintf(...)))

adf <- prep_features(reg_model_data %>% filter(fiscal_year %in% YEARS), features)$data
st  <- as.character(adf$state)
natl <- readRDS(file.path(POOL, "pool_national_anyerror_fdr10.rds"))
natl$pool <- "national"
stamp("national pool %d rules | window %d", nrow(natl), TOPK)

# the shipped fill, restricted to the top TOPK by the ranking statistic, with the
# leftover capacity returned as the exactness certificate
fill_windowed <- function(pool, idx, n_rows, budget) {
  cap <- floor(budget * n_rows); cap_buf <- floor(BUFFER_MULT * budget * n_rows)
  un <- rep(FALSE, n_rows); n_in <- 0L
  core <- integer(0); buffer <- integer(0)
  for (i in seq_along(idx)) {          # idx is already in rank order
    add <- sum(!un[idx[[i]]])
    if (add == 0) next
    if (n_in + add <= cap) { un[idx[[i]]] <- TRUE; n_in <- n_in + add; core <- c(core, i) }
    else if (n_in + add <= cap_buf) { un[idx[[i]]] <- TRUE; n_in <- n_in + add; buffer <- c(buffer, i) }
  }
  list(core = core, buffer = buffer, slack = cap_buf - n_in)
}

states <- sort(unique(st))
rows <- list()
for (S in states) {
  key <- gsub("[^A-Za-z]", "", S)
  pf  <- file.path(POOL, sprintf("pool_%s_anyerror_fdr10.rds", key))
  own <- if (file.exists(pf)) { o <- readRDS(pf); o$pool <- "state"; o } else NULL
  pool <- bind_rows(natl, own) %>%
    arrange(desc(precision_train_lcb), hh, rule) %>%
    distinct(hh, rule, .keep_all = TRUE)
  win <- pool[seq_len(min(TOPK, nrow(pool))), , drop = FALSE]
  tr  <- adf[st == S, , drop = FALSE]
  strata_tr <- lapply(setNames(nm = HH_LEVELS), function(h)
    which(hh_group_of(tr$cert_HH_size_FS_n) %in% h))
  idx <- flags_for_rules(win, tr, strata_tr, label = "")
  for (b in BUDGETS) {
    f <- fill_windowed(win, idx, nrow(tr), b)
    sel <- c(f$core, f$buffer)
    got <- win[sel, c("rule", "hh")]
    got$role <- rep(c("core", "buffer"), c(length(f$core), length(sel) - length(f$core)))
    fn <- file.path(LIST, sprintf("blended_delivery_%s_2022_2024_budget%02.0f.csv",
                                  gsub(" ", "_", S), 100 * b))
    ship <- read.csv(fn, stringsAsFactors = FALSE)[, c("rule", "hh", "role")]
    same <- nrow(got) == nrow(ship) && all(got$rule == ship$rule) &&
            all(got$hh == ship$hh) && all(got$role == ship$role)
    rows[[length(rows) + 1]] <- data.frame(
      state = S, budget = 100 * b, pool_size = nrow(pool), window = nrow(win),
      n_core = length(f$core), n_buffer = length(f$buffer),
      slack = f$slack, matches_shipped = same, n_shipped = nrow(ship),
      stringsAsFactors = FALSE)
    stamp("  %-22s %2.0f%%: core %3d buffer %3d | slack %3d | matches shipped list: %s",
          S, 100 * b, length(f$core), length(f$buffer), f$slack, same)
  }
  rm(idx); invisible(gc())
}
r <- bind_rows(rows)
write.csv(r, OUT, row.names = FALSE)
cat(sprintf("\n=== %d states x %d budgets ===\n", length(states), length(BUDGETS)))
cat(sprintf("slack zero          : %d of %d (max slack %d)\n",
            sum(r$slack == 0), nrow(r), max(r$slack)))
cat(sprintf("matches shipped list: %d of %d\n", sum(r$matches_shipped), nrow(r)))
if (any(!r$matches_shipped))
  print(as.data.frame(r %>% filter(!matches_shipped)), row.names = FALSE)
cat(sprintf("wrote %s\n", OUT))
