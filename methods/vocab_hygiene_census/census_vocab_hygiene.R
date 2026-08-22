# Descriptive census for the 2026-08-21 vocabulary-hygiene questions (no
# modeling, no mining): how big are the three problems before we spend a
# mining cycle on them?
#   A. How many rules condition on `utilities`, and how many of their
#      thresholds are drift-sensitive (the threshold's position among the
#      state-year SUA tiers changes across FY2022-24)?
#   B. How many rules condition on `medical_deductions`, and how many of
#      their thresholds fall inside the state's training-era SMD range
#      (the dead zone extends to the CURRENT SMD, which needs 2025/2026
#      rows we do not yet carry; this census reports the training-era part)?
#   C. How narrow are two-sided dollar intervals in the pool and the
#      delivered lists (candidate "fishing" rules)?
# Outputs: printed summary + CSVs in methods/vocab_hygiene_census/.
suppressMessages(library(dplyr))

OUT_DIR <- "methods/vocab_hygiene_census"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

DOLLAR_VARS <- c("medical_deductions", "utilities", "earned_by_hh_size",
                 "unearned_by_hh_size", "gross_by_hh_size",
                 "shelter_expenses_by_hh_size", "total_deductions_by_hh_size")

COND_PAT <- "([A-Za-z_][A-Za-z0-9_]*)\\s*(>=|<=|>|<)\\s*(-?[0-9.]+)"
parse_conds <- function(txt) {
  parts <- regmatches(txt, gregexpr(COND_PAT, txt, perl = TRUE))[[1]]
  if (!length(parts)) return(NULL)
  do.call(rbind, lapply(parts, function(p) {
    mm <- regmatches(p, regexec(COND_PAT, p))[[1]]
    data.frame(var = mm[2], op = mm[3], thr = as.numeric(mm[4]))
  }))
}

# ── inputs ───────────────────────────────────────────────────────────────────
pool <- readRDS("state_delivery_lists/national_rule_pool_2022_2024_v250.rds")
cat(sprintf("pool: %d rules\n", nrow(pool)))

files <- Sys.glob("state_delivery_lists/blended_delivery_*_budget10.csv")
lists <- bind_rows(lapply(files, function(f) {
  st <- sub("blended_delivery_(.*)_2022_2024_budget10\\.csv", "\\1", basename(f))
  read.csv(f, stringsAsFactors = FALSE) %>%
    filter(role == "core") %>%
    mutate(state = gsub("_", " ", st))
}))
cat(sprintf("delivered core rules: %d across %d states\n",
            nrow(lists), length(files)))

d <- readRDS("reg_model_data.rds")
smd <- read.csv("additional_data/standard_medical_deductions.csv",
                check.names = FALSE)

# parse every rule once
pool_conds <- lapply(pool$rule, parse_conds)
list_conds <- lapply(lists$rule, parse_conds)
uses_var <- function(conds_list, v)
  vapply(conds_list, function(cc) !is.null(cc) && v %in% cc$var, logical(1))

# ── A. utilities ─────────────────────────────────────────────────────────────
pool_util <- uses_var(pool_conds, "utilities")
list_util <- uses_var(list_conds, "utilities")
cat(sprintf("\n[A] utilities rules: pool %d/%d (%.1f%%) | delivered %d/%d (%.1f%%)\n",
            sum(pool_util), nrow(pool), 100 * mean(pool_util),
            sum(list_util), nrow(lists), 100 * mean(list_util)))

# state-year SUA tier proxy: distinct positive utilities values observed in
# the frame (the munging snaps rawutil to the state-year's valid UTIL values)
tiers <- d %>%
  filter(as.character(fiscal_year) %in% c("2022", "2023", "2024"),
         utilities > 0) %>%
  group_by(state = as.character(state), fy = as.character(fiscal_year)) %>%
  summarise(vals = list(sort(unique(round(utilities)))), .groups = "drop")

# a utilities threshold is DRIFT-SENSITIVE for a state when the number of
# that state's tiers sitting at or below the threshold differs across years
tier_pos <- function(st, thr) {
  tv <- tiers %>% filter(state == st)
  if (!nrow(tv)) return(NA_integer_)
  pos <- vapply(tv$vals, function(v) sum(v <= thr), integer(1))
  if (length(unique(pos)) > 1) -1L else pos[1]
}
util_rows <- lists[list_util, c("state", "rank", "rule", "hh")]
util_rows$drift <- NA
for (i in seq_len(nrow(util_rows))) {
  cc <- parse_conds(util_rows$rule[i])
  thr <- cc$thr[cc$var == "utilities"]
  util_rows$drift[i] <- any(vapply(thr, function(t)
    identical(tier_pos(util_rows$state[i], t), -1L), logical(1)))
}
cat(sprintf("[A] delivered utilities rules with a DRIFT-SENSITIVE threshold (tier position changes across FY22-24): %d of %d\n",
            sum(util_rows$drift, na.rm = TRUE), nrow(util_rows)))
write.csv(util_rows, file.path(OUT_DIR, "utilities_rules_delivered.csv"),
          row.names = FALSE)

# ── B. medical deductions vs the SMD range ───────────────────────────────────
pool_med <- uses_var(pool_conds, "medical_deductions")
list_med <- uses_var(list_conds, "medical_deductions")
cat(sprintf("\n[B] medical_deductions rules: pool %d/%d (%.1f%%) | delivered %d/%d (%.1f%%)\n",
            sum(pool_med), nrow(pool), 100 * mean(pool_med),
            sum(list_med), nrow(lists), 100 * mean(list_med)))

smd_cols <- intersect(c("2022", "2023", "2024"), names(smd))
smd_rng <- smd %>%
  rowwise() %>%
  mutate(smd_min = suppressWarnings(min(c_across(all_of(smd_cols)), na.rm = TRUE)),
         smd_max = suppressWarnings(max(c_across(all_of(smd_cols)), na.rm = TRUE))) %>%
  ungroup() %>%
  select(state_name, smd_min, smd_max) %>%
  filter(is.finite(smd_min))
med_rows <- lists[list_med, c("state", "rank", "rule", "hh")]
med_rows <- med_rows %>% left_join(smd_rng, by = c("state" = "state_name"))
med_rows$in_training_smd_range <- NA
for (i in seq_len(nrow(med_rows))) {
  if (!is.finite(med_rows$smd_min[i])) next
  cc <- parse_conds(med_rows$rule[i])
  thr <- cc$thr[cc$var == "medical_deductions"]
  med_rows$in_training_smd_range[i] <-
    any(thr >= med_rows$smd_min[i] & thr <= med_rows$smd_max[i])
}
cat(sprintf("[B] delivered med rules in SMD states: %d; thresholds inside the training-era SMD range: %d\n",
            sum(is.finite(med_rows$smd_min)),
            sum(med_rows$in_training_smd_range, na.rm = TRUE)))
cat("[B] NB the full dead zone runs to the CURRENT SMD; the SMD table ends at",
    max(as.numeric(intersect(names(smd), as.character(2017:2030)))), "\n")
write.csv(med_rows, file.path(OUT_DIR, "medical_rules_delivered.csv"),
          row.names = FALSE)

# ── C. narrow two-sided dollar intervals ─────────────────────────────────────
interval_stats <- function(conds_list, meta) {
  out <- list()
  for (i in seq_along(conds_list)) {
    cc <- conds_list[[i]]
    if (is.null(cc)) next
    for (v in intersect(unique(cc$var), DOLLAR_VARS)) {
      lo <- cc$thr[cc$var == v & cc$op %in% c(">", ">=")]
      hi <- cc$thr[cc$var == v & cc$op %in% c("<", "<=")]
      if (length(lo) && length(hi)) {
        w <- max(hi) - max(lo)
        out[[length(out) + 1]] <- data.frame(
          idx = i, var = v, lo = max(lo), hi = max(hi),
          width = w, rel_width = w / max(hi))
      }
    }
  }
  if (!length(out)) return(NULL)
  bind_rows(out) %>% bind_cols(meta[.$idx, , drop = FALSE])
}
pool_iv <- interval_stats(pool_conds, pool[, c("hh", "rule")])
list_iv <- interval_stats(list_conds, lists[, c("state", "rank", "rule")])
for (nm in c("pool", "delivered")) {
  iv <- if (nm == "pool") pool_iv else list_iv
  if (is.null(iv)) next
  cat(sprintf("\n[C] %s: %d two-sided dollar intervals\n", nm, nrow(iv)))
  cat(sprintf("    width  <= $10: %d | <= $25: %d | <= $50: %d\n",
              sum(iv$width <= 10), sum(iv$width <= 25), sum(iv$width <= 50)))
  cat(sprintf("    rel width <= 2%%: %d | <= 5%%: %d | median rel %.1f%%\n",
              sum(iv$rel_width <= .02), sum(iv$rel_width <= .05),
              100 * median(iv$rel_width)))
}
if (!is.null(list_iv))
  write.csv(list_iv %>% arrange(width),
            file.path(OUT_DIR, "delivered_dollar_intervals.csv"),
            row.names = FALSE)
if (!is.null(pool_iv))
  write.csv(pool_iv %>% arrange(width) %>% head(200),
            file.path(OUT_DIR, "pool_narrowest_intervals_top200.csv"),
            row.names = FALSE)
cat("\ncensus done; CSVs in", OUT_DIR, "\n")
