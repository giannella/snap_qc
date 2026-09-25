# Readout for the CPI-inflation test (design note: design_note.md).
#   Rscript methods/cpi_inflate_test/readout.R            # full outputs
#   SMOKE=1 Rscript methods/cpi_inflate_test/readout.R    # smoke outputs
# Run from the worktree root. Reads the per-arm pools / sweeps / lists written
# by cpi_inflate_oneyear_ahead_v2.R, the frame diff and the frames, and writes:
#   readout_family.csv       rule-level aggregates by window x arm x scope x group
#   readout_family_by_n.csv  the same within train-n buckets (all admitted)
#   readout_feature.csv      by mined dollar feature
#   readout_contrasts.csv    frame contrasts (cpi - nominal at each seed) and
#                            seed contrasts (seed 2 - seed 1 on each frame)
#   readout_overlap.csv      share of an arm's rules found verbatim in its reference arm
#   readout_sweep.csv        pool-level union metrics by LCB floor
#   readout_lists.csv        per-state paired list-level differences, summarised
#   readout_list_rule_mix.csv share of test-year list rules that are affected
#
# AFFECTED is defined by effect, not by rule text (review 2026-09-20): a
# condition on a CPI-changed feature is SENSITIVE when it flags a different
# set of cases on the CPI frame than on the nominal frame (window rows, train
# + test). A zero-vs-positive cut (0 x ratio = 0) flags the same cases on
# both frames and is not sensitive. A rule is affected when it carries at
# least one sensitive condition; a rule that names a changed feature only
# through insensitive conditions is reported as its own group. By-feature
# membership runs through sensitive conditions only.
# Rule-level aggregates describe heavily overlapping rules, so no rule-level
# interval is computed; the seed contrasts are the reference for what a
# re-mine with no design change moves.

suppressMessages(library(dplyr))
source("rule_mining_helpers.R")
SMOKE <- identical(Sys.getenv("SMOKE"), "1")
OUT <- "methods/cpi_inflate_test/out"
IN  <- if (SMOKE) file.path(OUT, "smoke") else OUT
FR  <- "methods/cpi_inflate_test/frames"
ERAS <- c("1718_19", "2223_24", "1719_22")
ARMS <- c("nominal", "cpi", "nominal_seed2", "cpi_seed2")
YEARS  <- list("1718_19" = c(2017, 2018, 2019), "2223_24" = c(2022, 2023, 2024),
               "1719_22" = c(2017, 2018, 2019, 2022))
TARGET <- c("1718_19" = 2019, "2223_24" = 2024, "1719_22" = 2022)
# contrast = arm minus reference
PAIRS <- data.frame(
  arm       = c("cpi", "cpi_seed2", "nominal_seed2", "cpi_seed2"),
  reference = c("nominal", "nominal_seed2", "nominal", "cpi"),
  contrast  = c("frame (CPI - nominal), seed 117", "frame (CPI - nominal), seed 118",
                "seed (118 - 117), nominal frame", "seed (118 - 117), CPI frame"))
DOLLAR_FEATS <- c("earned_by_hh_size", "unearned_by_hh_size", "gross_by_hh_size",
                  "medical_deductions", "shelter_expenses_by_hh_size",
                  "total_deductions_by_hh_size")
VOCAB19 <- c("HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size",
             "expedited_i", "bbce_state_i", "rawben_rel_max", "medical_deductions",
             "shelter_expenses_by_hh_size", "utilities_sua", "married", "homeless",
             "percent_abawd", "unc_rawben_rel_max", "months_since_cert_n",
             "count_divisible_by_100", "gross_by_hh_size", "earned_by_hh_size",
             "unearned_by_hh_size")
COND_RE <- "^\\s*([A-Za-z._][A-Za-z0-9._]*)\\s*(<=|>=|<|>)"

agg <- function(d) d %>% summarise(
  n_rules = n(), med_n_train = median(n),
  med_prec_train = median(prec_tr), med_lcb = median(lcb),
  med_prec_test = median(prec_te, na.rm = TRUE),
  pooled_prec_test = sum(k_te) / max(sum(n_te), 1),
  pooled_prec_test_nominal_rows = sum(k_te_nomtest) / max(sum(n_te_nomtest), 1),
  med_prec_test_nomrows = median(prec_te_nom, na.rm = TRUE),
  med_decay_nomrows = median(prec_te_nom - prec_tr, na.rm = TRUE),
  mean_decay_nomrows = mean(prec_te_nom - prec_tr, na.rm = TRUE),
  med_margin_nomrows = median(prec_te_nom - lcb, na.rm = TRUE),
  share_below_lcb_nomrows = mean(prec_te_nom - lcb < 0, na.rm = TRUE),
  share_reach_collapse_nomrows = mean(n_te_nomtest < 10),
  med_abs_log_reach_nomrows = median(abs(log(pmax(reach_ratio_nom, 1e-6)))),
  med_decay = median(decay, na.rm = TRUE), mean_decay = mean(decay, na.rm = TRUE),
  med_margin = median(margin, na.rm = TRUE),
  share_below_lcb = mean(margin < 0, na.rm = TRUE),
  share_reach_collapse = mean(n_te < 10),
  med_reach_ratio = median(reach_ratio),
  med_abs_log_reach = median(abs(log(pmax(reach_ratio, 1e-6)))),
  .groups = "drop")

pools <- list(); fam_rows <- list(); famn_rows <- list(); feat_rows <- list()
ov_rows <- list(); sens_rows <- list(); affected_keys <- list()
for (era in ERAS) {
  fd <- file.path(OUT, sprintf("frame_diff_%d.csv", TARGET[[era]]))
  stopifnot(file.exists(fd))
  d <- read.csv(fd)
  cpi_feats <- intersect(DOLLAR_FEATS, d$column[d$status == "changed"])
  nom_feats <- setdiff(DOLLAR_FEATS, cpi_feats)
  cat(sprintf("\n[%s] CPI-changed mined features: %s\n       dollar features left nominal: %s\n",
              era, paste(cpi_feats, collapse = ", "), paste(nom_feats, collapse = ", ")))
  raw <- list()
  for (arm in ARMS) {
    fn <- file.path(IN, sprintf("pool_%s_%s.rds", era, arm))
    if (!file.exists(fn)) { cat("  missing:", fn, "\n"); next }
    raw[[arm]] <- readRDS(fn)
  }
  if (!length(raw)) next

  ## sensitive conditions: evaluated on both frames, window rows
  all_conds <- unique(unlist(lapply(raw, function(p) strsplit(p$rule, " & ", fixed = TRUE))))
  cvar <- sub(paste0(COND_RE, ".*$"), "\\1", all_conds)
  test_conds <- all_conds[cvar %in% cpi_feats]
  fa <- prep_features(readRDS(file.path(FR, "reg_model_data_nominal.rds")) %>%
                        filter(fiscal_year %in% YEARS[[era]]), VOCAB19)$data[, cpi_feats, drop = FALSE]
  fb <- prep_features(readRDS(file.path(FR, sprintf("reg_model_data_%d.rds", TARGET[[era]]))) %>%
                        filter(fiscal_year %in% YEARS[[era]]), VOCAB19)$data[, cpi_feats, drop = FALSE]
  stopifnot(nrow(fa) == nrow(fb))
  sens <- vapply(test_conds, function(cd) {
    e <- parse(text = cd)
    a <- eval(e, fa); b <- eval(e, fb)
    a[is.na(a)] <- FALSE; b[is.na(b)] <- FALSE
    any(a != b)
  }, logical(1))
  rm(fa, fb); invisible(gc())
  sens_conds <- test_conds[sens]
  sens_rows[[era]] <- data.frame(era = era, feature = cvar[cvar %in% cpi_feats], sensitive = sens) %>%
    group_by(era, feature) %>%
    summarise(unique_conditions = n(), sensitive_conditions = sum(sensitive), .groups = "drop")

  for (arm in names(raw)) {
    p <- raw[[arm]]
    n_tagged <- sum(p$artifact_i)
    p <- p[!p$artifact_i, , drop = FALSE]
    # recorded-dollar test rows: for nominal arms they ARE the test rows
    if (!"n_te_nomtest" %in% names(p) || !grepl("^cpi", arm)) {
      stopifnot(!grepl("^cpi", arm))
      p$n_te_nomtest <- p$n_te; p$k_te_nomtest <- p$k_te
    }
    p$rule_id <- seq_len(nrow(p))
    cl <- strsplit(p$rule, " & ", fixed = TRUE)
    cd <- data.frame(rule_id = rep(p$rule_id, lengths(cl)), cond = unlist(cl),
                     stringsAsFactors = FALSE)
    cd$var <- sub(paste0(COND_RE, ".*$"), "\\1", cd$cond)
    cd$upper <- grepl("<", cd$cond, fixed = TRUE)
    cd$sens <- cd$cond %in% sens_conds
    per_rule <- cd %>% group_by(rule_id) %>% summarise(
      names_cpi = any(var %in% cpi_feats), has_sens = any(sens),
      names_nom = any(var %in% nom_feats),
      n_sens_feats = n_distinct(var[sens]), .groups = "drop")
    two_sided <- cd %>% filter(var %in% cpi_feats) %>% group_by(rule_id, var) %>%
      summarise(ts = any(upper) && any(!upper) && any(sens), .groups = "drop") %>%
      group_by(rule_id) %>% summarise(two_sided = any(ts), .groups = "drop")
    p <- p %>% left_join(per_rule, by = "rule_id") %>% left_join(two_sided, by = "rule_id")
    p$two_sided[is.na(p$two_sided)] <- FALSE
    p$affected <- ifelse(p$has_sens, "affected",
                  ifelse(p$names_cpi, "names a CPI-changed field, same cases flagged on both frames",
                         "not affected"))
    p$detail <- ifelse(p$has_sens,
                       ifelse(p$two_sided, "affected: two-sided interval on a CPI-changed field",
                              "affected: one-sided cuts only"),
                ifelse(p$names_cpi, NA_character_,
                ifelse(p$names_nom, "not affected: conditions on a dollar field left nominal",
                       "not affected: no dollar field")))
    p$prec_tr <- p$k / p$n
    p$prec_te <- ifelse(p$n_te > 0, p$k_te / p$n_te, NA_real_)
    p$decay   <- p$prec_te - p$prec_tr
    p$margin  <- p$prec_te - p$lcb
    p$reach_ratio <- (p$n_te / p$n_stratum_te) / (p$n / p$n_stratum_tr)
    p$prec_te_nom <- ifelse(p$n_te_nomtest > 0, p$k_te_nomtest / p$n_te_nomtest, NA_real_)
    p$reach_ratio_nom <- (p$n_te_nomtest / p$n_stratum_te) / (p$n / p$n_stratum_tr)
    p$n_bucket <- cut(p$n, c(29, 99, 299, Inf), labels = c("30-99", "100-299", "300+"))
    sens_by_rule <- cd %>% filter(sens) %>% distinct(rule_id, var)
    pools[[paste(era, arm)]] <- p
    affected_keys[[paste(era, arm)]] <- paste(p$hh, p$rule)[p$has_sens]

    for (scope in c("all admitted", "LCB >= 0.20", "top 1000 by LCB")) {
      ps <- switch(scope, "all admitted" = p, "LCB >= 0.20" = p[p$lcb >= 0.20, ],
                   "top 1000 by LCB" = p[seq_len(min(1000L, nrow(p))), ])
      if (!nrow(ps)) next
      fam_rows[[length(fam_rows) + 1L]] <- bind_rows(
        ps %>% group_by(group = affected) %>% agg(),
        ps %>% filter(!is.na(detail)) %>% group_by(group = detail) %>% agg(),
        ps %>% mutate(group = "whole pool") %>% group_by(group) %>% agg()) %>%
        mutate(era = era, arm = arm, scope = scope, n_pool = nrow(ps),
               share_of_pool = n_rules / nrow(ps), n_artifact_tagged = n_tagged,
               .before = 1)
      for (f in DOLLAR_FEATS) {
        ids  <- if (f %in% cpi_feats) sens_by_rule$rule_id[sens_by_rule$var == f]
                else unique(cd$rule_id[cd$var == f])
        has  <- ps$rule_id %in% ids
        only <- has & ps$n_sens_feats == 1L
        if (!any(has)) next
        feat_rows[[length(feat_rows) + 1L]] <- bind_rows(
          ps[has, ] %>% mutate(membership = if (f %in% cpi_feats)
            "sensitive condition on the field" else "conditions on the field") %>%
            group_by(membership) %>% agg(),
          if (f %in% cpi_feats && any(only))
            ps[only, ] %>% mutate(membership = "its only sensitive field") %>%
              group_by(membership) %>% agg()) %>%
          mutate(era = era, arm = arm, scope = scope, feature = f,
                 cpi_changed = f %in% cpi_feats, n_pool = nrow(ps),
                 share_of_pool = n_rules / nrow(ps), .before = 1)
      }
    }
    famn_rows[[length(famn_rows) + 1L]] <- p %>%
      group_by(group = affected, n_bucket) %>% agg() %>%
      mutate(era = era, arm = arm, .before = 1)
  }
  for (i in seq_len(nrow(PAIRS))) {
    a <- pools[[paste(era, PAIRS$arm[i])]]; b <- pools[[paste(era, PAIRS$reference[i])]]
    if (is.null(a) || is.null(b)) next
    ov_rows[[length(ov_rows) + 1L]] <- a %>%
      mutate(in_ref = paste(hh, rule) %in% paste(b$hh, b$rule)) %>%
      group_by(group = affected) %>%
      summarise(n_rules = n(), share_verbatim_in_reference = mean(in_ref), .groups = "drop") %>%
      mutate(era = era, contrast = PAIRS$contrast[i], .before = 1)
  }
}
fam <- bind_rows(fam_rows); famn <- bind_rows(famn_rows); feat <- bind_rows(feat_rows)
write.csv(fam,  file.path(IN, "readout_family.csv"),  row.names = FALSE)
write.csv(famn, file.path(IN, "readout_family_by_n.csv"), row.names = FALSE)
write.csv(feat, file.path(IN, "readout_feature.csv"), row.names = FALSE)
write.csv(bind_rows(ov_rows), file.path(IN, "readout_overlap.csv"), row.names = FALSE)
write.csv(bind_rows(sens_rows), file.path(IN, "readout_sensitive_conditions.csv"), row.names = FALSE)

## contrasts
METRICS <- c("n_rules", "share_of_pool", "med_n_train", "med_prec_test", "pooled_prec_test",
             "med_decay", "mean_decay", "med_margin", "share_below_lcb",
             "share_reach_collapse", "med_abs_log_reach",
             "pooled_prec_test_nominal_rows", "med_prec_test_nomrows", "med_decay_nomrows",
             "mean_decay_nomrows", "med_margin_nomrows", "share_below_lcb_nomrows",
             "share_reach_collapse_nomrows", "med_abs_log_reach_nomrows")
contrast <- function(tab, keys) bind_rows(lapply(seq_len(nrow(PAIRS)), function(i) {
  a <- tab %>% filter(arm == PAIRS$arm[i]) %>% select(all_of(c(keys, METRICS)))
  b <- tab %>% filter(arm == PAIRS$reference[i]) %>% select(all_of(c(keys, METRICS)))
  j <- inner_join(a, b, by = keys, suffix = c("", "_ref"))
  if (!nrow(j)) return(NULL)
  for (m in METRICS) j[[paste0("d_", m)]] <- j[[m]] - j[[paste0(m, "_ref")]]
  j %>% mutate(contrast = PAIRS$contrast[i]) %>%
    select(all_of(keys), contrast, starts_with("d_"), n_rules_ref, med_decay_ref,
           pooled_prec_test_ref)
}))
keys <- c("table", "era", "scope", "group", "feature", "membership")
con <- bind_rows(
  contrast(fam %>% mutate(table = "family", feature = NA_character_,
                          membership = NA_character_), keys),
  contrast(feat %>% mutate(table = "feature", group = NA_character_), keys))
write.csv(con, file.path(IN, "readout_contrasts.csv"), row.names = FALSE)

## pool-level sweep
sw <- bind_rows(lapply(list.files(IN, "^sweep_.*\\.csv$", full.names = TRUE), read.csv))
if (nrow(sw)) write.csv(sw %>% arrange(era, lcb_floor, arm),
                        file.path(IN, "readout_sweep.csv"), row.names = FALSE)

## list level: per-state paired differences
ls_all <- bind_rows(lapply(list.files(IN, "^lists_.*\\.csv$", full.names = TRUE), read.csv))
if (nrow(ls_all)) {
  # recorded-dollar test rows: nominal arms coincide; CPI arms of the first two
  # windows were not list-scored on them (rule level identical to 4 decimals)
  if (!"precision_nomrows" %in% names(ls_all)) { ls_all$precision_nomrows <- NA_real_; ls_all$dollar_recall_nomrows <- NA_real_ }
  nomarm <- !grepl("^cpi", ls_all$arm)
  ls_all$precision_nomrows[nomarm] <- ls_all$precision[nomarm]
  ls_all$dollar_recall_nomrows[nomarm] <- ls_all$dollar_recall[nomarm]
  BBCE_FLIP <- c("Louisiana", "Virginia", "Mississippi", "Indiana")  # bbce_state_i differs between FY2017-19 and FY2022
  paired <- bind_rows(lapply(seq_len(nrow(PAIRS)), function(i) {
    a <- ls_all %>% filter(arm == PAIRS$arm[i])
    b <- ls_all %>% filter(arm == PAIRS$reference[i]) %>%
      select(era, state, budget, precision, dollar_recall, precision_nomrows, dollar_recall_nomrows)
    inner_join(a, b, by = c("era", "state", "budget"), suffix = c("", "_ref")) %>%
      mutate(contrast = PAIRS$contrast[i],
             d_precision = precision - precision_ref,
             d_dollar_recall = dollar_recall - dollar_recall_ref,
             d_precision_nomrows = precision_nomrows - precision_nomrows_ref,
             d_dollar_recall_nomrows = dollar_recall_nomrows - dollar_recall_nomrows_ref)
  }))
  write.csv(paired, file.path(IN, "readout_lists_paired_by_state.csv"), row.names = FALSE)
  lst <- paired %>% group_by(era, budget, contrast) %>% summarise(
    states = n(),
    median_precision_ref = median(precision_ref), median_precision = median(precision),
    d_precision_median = median(d_precision), d_precision_mean = mean(d_precision),
    n_pos = sum(d_precision > 0), n_neg = sum(d_precision < 0),
    harmed_lt_m05 = sum(d_precision < -0.05), helped_gt_p05 = sum(d_precision > 0.05),
    d_dollar_recall_median = median(d_dollar_recall),
    d_dollar_recall_mean = mean(d_dollar_recall),
    dollar_n_pos = sum(d_dollar_recall > 0), dollar_n_neg = sum(d_dollar_recall < 0),
    dollar_harmed_lt_m05 = sum(d_dollar_recall < -0.05), .groups = "drop")
  write.csv(lst, file.path(IN, "readout_lists.csv"), row.names = FALSE)
  summ_nom <- function(d) d %>% group_by(era, budget, contrast) %>% summarise(
    states = n(),
    median_precision_ref = median(precision_nomrows_ref), median_precision = median(precision_nomrows),
    d_precision_median = median(d_precision_nomrows), d_precision_mean = mean(d_precision_nomrows),
    n_pos = sum(d_precision_nomrows > 0), n_neg = sum(d_precision_nomrows < 0),
    harmed_lt_m05 = sum(d_precision_nomrows < -0.05), helped_gt_p05 = sum(d_precision_nomrows > 0.05),
    d_dollar_recall_median = median(d_dollar_recall_nomrows),
    d_dollar_recall_mean = mean(d_dollar_recall_nomrows), .groups = "drop")
  pn <- paired %>% filter(!is.na(d_precision_nomrows))
  if (nrow(pn)) {
    write.csv(summ_nom(pn), file.path(IN, "readout_lists_recorded_test_rows.csv"), row.names = FALSE)
    write.csv(summ_nom(pn %>% filter(era == "1719_22", !state %in% BBCE_FLIP)),
              file.path(IN, "readout_lists_recorded_test_rows_excl_bbce_flip.csv"), row.names = FALSE)
  }
}

## share of the rules used on the test-year lists that are affected
lr <- list.files(IN, "^list_rules_.*\\.rds$", full.names = TRUE)
if (length(lr)) {
  used <- bind_rows(lapply(lr, readRDS))
  used$affected <- NA
  for (g in unique(paste(used$era, used$arm))) {
    i <- paste(used$era, used$arm) == g
    used$affected[i] <- paste(used$hh[i], used$rule[i]) %in% affected_keys[[g]]
  }
  write.csv(used %>% group_by(era, arm, budget) %>%
              summarise(list_rules = n(), share_affected = mean(affected), .groups = "drop"),
            file.path(IN, "readout_list_rule_mix.csv"), row.names = FALSE)
}

options(width = 220)
cat("\n== family table (scope: all admitted) ==\n")
print(as.data.frame(fam %>% filter(scope == "all admitted") %>%
  select(era, arm, group, n_rules, share_of_pool, med_prec_train, med_lcb, med_prec_test,
         pooled_prec_test, med_decay, med_margin, share_below_lcb, share_reach_collapse,
         med_abs_log_reach) %>% mutate(across(where(is.numeric), ~ round(.x, 4)))),
  row.names = FALSE)
if (exists("lst")) { cat("\n== lists ==\n"); print(as.data.frame(lst), row.names = FALSE) }
