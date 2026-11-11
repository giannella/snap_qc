# Check the state-facing model package against the research code. Run from the repo root
# after make_case_model_package.R:
#   Rscript methods/case_predictions/validate_case_model_package.R
# 1. the package's model file holds the deploy bundle's ten seed pairs byte for byte;
# 2. features built from the raw input columns match the frame's features, every state,
#    FY2017-19 and FY2022-24 (each era scored as one file, as a state would send it);
# 3. scores from those features match scores from the frame's own features (the
#    computation predict_cases.R does), and the package's output for one state matches
#    predict_cases.R's output file case for case.
# Writes methods/case_predictions/case_model_validation/.
suppressMessages(library(xgboost))
source("methods/case_predictions/case_model_dev_helpers.R")
PKG <- normalizePath("methods/case_predictions/dist/snap_case_model")
BUNDLE <- Sys.getenv("CM_BUNDLE", "methods/case_predictions/s46/out_f1109/deploy_B_bundle.rds")
OUTV <- "methods/case_predictions/case_model_validation"; dir.create(OUTV, showWarnings = FALSE)
say <- function(...) cat(sprintf(...), "\n", sep = "")
source(file.path(PKG, "predict.R"))
m <- get_model(); b <- readRDS(BUNDLE)

# 1. model identity
same <- identical(lapply(m$models, function(s) s[c("p", "size")]), lapply(b$models, function(s) s[c("p", "size")])) &&
  identical(m[c("inputs", "base_inputs", "states", "size_offset_glm", "cpi_by_year", "train_years")],
            b[c("inputs", "base_inputs", "states", "size_offset_glm", "cpi_by_year", "train_years")])
say("1. package model file = deploy bundle (models, inputs, states, offset, CPI): %s", same); stopifnot(same)

# 2-3. every state, two eras
reg <- readRDS("additional_data/reg_model_data.rds")
stopifnot(digest::digest(file = "additional_data/reg_model_data.rds", algo = "sha256") == b$frame_sha256)
tables <- load_tables(file.path(PKG, "lookups"))
ERAS <- list(`FY2017-19` = c("2017", "2018", "2019"), `FY2022-24` = c("2022", "2023", "2024"))
close <- function(a, b) (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & abs(a - b) <= 1e-6 + 1e-9 * abs(b))
fm <- list(); sp <- list()
for (st in m$states) for (en in names(ERAS)) {
  w <- reg[as.character(reg$state) == st & as.character(reg$fiscal_year) %in% ERAS[[en]], ]
  if (!nrow(w)) next
  inp <- check_input(frame_to_input(w))
  pf <- build_features(inp, st, tables)
  ff <- frame_features(w, m$base_inputs)
  fm[[length(fm) + 1]] <- data.frame(state = st, era = en, n = nrow(w), feature = m$base_inputs,
                                     match = vapply(m$base_inputs, function(v) mean(close(pf[[v]], ff[[v]])), 0))
  ben <- attr(pf, "recomputed_benefit"); ben[is.na(ben)] <- inp$ORIGINAL_BENEFIT_AMOUNT[is.na(ben)]
  sp_ <- score_cases(pf, st, ben, m); sf <- score_cases(ff, st, w$raw_benefit_amount, m)
  dp <- sp_$p_error * sp_$size; df_ <- sf$p_error * sf$size
  yr <- as.character(w$fiscal_year)
  top <- unlist(lapply(split(seq_along(yr), yr), function(i) {           # top 10% within each fiscal year
    k <- ceiling(0.10 * length(i)); a <- i[order(-dp[i])][1:k]; f <- i[order(-df_[i])][1:k]; length(intersect(a, f)) / k }))
  sp[[length(sp) + 1]] <- data.frame(state = st, era = en, n = nrow(w),
    benefit_match = mean(close(attr(pf, "recomputed_benefit"), w$raw_benefit_amount)),
    cases_all_features_match = mean(Reduce(`&`, lapply(m$base_inputs, function(v) close(pf[[v]], ff[[v]])))),
    p_identical = mean(abs(sp_$p_error - sf$p_error) < 1e-9), max_abs_p_diff = max(abs(sp_$p_error - sf$p_error)),
    max_abs_dollar_diff = max(abs(dp - df_)), spearman_dollars = cor(dp, df_, method = "spearman"),
    top10_overlap = min(top))
}
fm <- do.call(rbind, fm); sp <- do.call(rbind, sp)
write.csv(fm, file.path(OUTV, "feature_match_by_state.csv"), row.names = FALSE)
write.csv(sp, file.path(OUTV, "score_parity_by_state.csv"), row.names = FALSE)
for (en in names(ERAS)) {
  f <- fm[fm$era == en, ]; s <- sp[sp$era == en, ]
  agg <- aggregate(cbind(match = f$match * f$n, n = f$n) ~ feature, f, sum)
  say("\n2. %s: feature match, all states pooled (%d cases, %d states); lowest single state in brackets", en, sum(s$n), nrow(s))
  for (k in seq_len(nrow(agg))) { v <- agg$feature[k]; lo <- f[f$feature == v, ][which.min(f$match[f$feature == v]), ]
    say("   %-28s %7.3f%%   [%s %.3f%%]", v, 100 * agg$match[k] / agg$n[k], lo$state, 100 * lo$match) }
  say("3. %s scores: cases with every feature matching %.3f%%; P identical %.3f%%; max |P diff| %.2e; max |$ diff| %.2f;",
      en, 100 * sum(s$cases_all_features_match * s$n) / sum(s$n), 100 * sum(s$p_identical * s$n) / sum(s$n),
      max(s$max_abs_p_diff), max(s$max_abs_dollar_diff))
  say("   lowest state Spearman (expected $) %.4f (%s); lowest top-10%% overlap %.3f (%s); recomputed benefit = frame's in %.3f%% of cases",
      min(s$spearman_dollars), s$state[which.min(s$spearman_dollars)], min(s$top10_overlap), s$state[which.min(s$top10_overlap)],
      100 * sum(s$benefit_match * s$n) / sum(s$n))
}

# 3b. end to end against the live script, one state's FY2024 frame rows
xs <- Sys.getenv("CM_XCHECK_STATE", "Washington")
pcf <- file.path("methods/case_predictions", sprintf("predictions_%s.csv", gsub(" ", "_", xs)))
bak <- tempfile(); had <- file.exists(pcf); if (had) file.copy(pcf, bak)      # predict_cases.R overwrites a tracked file
Sys.setenv(PC_STATE = xs, PC_YEARS = "2024", PC_MODE = "predict", SR_ROOT = getwd())   # SR_ROOT: common.R works from this checkout
rc <- system2(file.path(R.home("bin"), "Rscript"), "methods/case_predictions/predict_cases.R", stdout = FALSE, stderr = FALSE)
stopifnot(rc == 0)
pc <- read.csv(pcf, stringsAsFactors = FALSE)
if (had) file.copy(bak, pcf, overwrite = TRUE) else unlink(pcf)
w <- reg[as.character(reg$state) == xs & as.character(reg$fiscal_year) == "2024", ]
pk <- predict_error(frame_to_input(w), xs, quiet = TRUE)
pk$case <- sub("^SAMPLE-", "", pk$CASE_ID); j <- match(pc$case, pk$case); stopifnot(!anyNA(j))
# the package leaves out rules using count_divisible_by_100 (as the workbook does); drop them from predict_cases.R's list too
rl <- read.csv(file.path(PKG, "rules", sprintf("blended_delivery_%s_2022_2024_budget10.csv", gsub(" ", "_", xs))), stringsAsFactors = FALSE)
cdr <- rl$rank[grepl("count_divisible_by_100", rl$rule, fixed = TRUE)]
pc$list_rules_flagging <- vapply(strsplit(ifelse(is.na(pc$list_rules_flagging), "", as.character(pc$list_rules_flagging)), " "),
                                 function(r) paste(setdiff(r, as.character(cdr)), collapse = " "), "")
say("\n3b. %s FY2024, package vs predict_cases.R output (%d cases): max |P diff| %.2e, max |expected $ diff| %.2f, same priority_rank %.1f%%, same rules listed %.1f%%",
    xs, nrow(pc), max(abs(pc$p_error - pk$p_error[j])), max(abs(pc$expected_error_dollars - pk$expected_error_dollars[j])),
    100 * mean(pc$priority_rank == pk$priority_rank[j]),
    100 * mean(ifelse(is.na(pc$list_rules_flagging), "", pc$list_rules_flagging) == pk$list_rules_flagging[j]))

# 4. a column the state does not record: each input column blanked, or filled with 0, one at a
# time, on every state's FY2024 rows; overlap of the state's top 10% with the full-input ranking.
# Also the share of cases blank in training, per model input.
na_tr <- colMeans(is.na(frame_features(reg[as.character(reg$fiscal_year) %in% m$train_years, ], m$base_inputs)))
say("\n4. share of training cases with a blank input (largest): %s",
    paste(sprintf("%s %.3f%%", names(sort(na_tr, decreasing = TRUE))[1:3], 100 * sort(na_tr, decreasing = TRUE)[1:3]), collapse = "; "))
w24 <- reg[as.character(reg$fiscal_year) == "2024", ]
cols <- setdiff(INPUT_COLUMNS, c("CASE_ID", "REVIEW_FISCAL_YEAR", "HOUSEHOLD_SIZE"))
ce <- list()
for (st in m$states) {
  inp <- frame_to_input(w24[as.character(w24$state) == st, ])
  base <- predict_error(inp, st, quiet = TRUE); base <- base[order(base$CASE_ID), ]
  k <- ceiling(0.1 * nrow(base)); topb <- base$CASE_ID[base$priority_rank <= k]
  for (c in cols) for (how in c("blank", "zero")) {
    if (how == "zero" && c == "ORIGINAL_BENEFIT_AMOUNT") next
    x <- inp; x[[c]] <- if (how == "blank") NA else 0
    p <- predict_error(x, st, quiet = TRUE, .allow_empty_columns = TRUE)   # measures what the all-blank stop prevents
    p <- p[order(p$CASE_ID), ]
    ce[[length(ce) + 1]] <- data.frame(state = st, column = c, filled = how, n = nrow(base),
      mean_p_full = mean(base$p_error), mean_p = mean(p$p_error),
      spearman = cor(base$expected_error_dollars, p$expected_error_dollars, method = "spearman"),
      top10_overlap = length(intersect(topb, p$CASE_ID[p$priority_rank <= k])) / k)
  }
}
ce <- do.call(rbind, ce)
write.csv(ce, file.path(OUTV, "column_effects_fy2024.csv"), row.names = FALSE)
a <- aggregate(cbind(mean_p_full, mean_p, top10_overlap) ~ column + filled, ce, median)
a$worst_state_top10 <- aggregate(top10_overlap ~ column + filled, ce, min)$top10_overlap
a <- reshape(a, idvar = "column", timevar = "filled", direction = "wide")
say("   median over states (worst state) of top-10%% overlap, and median mean P (full input %.3f):", median(ce$mean_p_full))
a <- a[order(-a$top10_overlap.zero), ]
for (k in seq_len(nrow(a))) say("   %-26s blank %.2f (%.2f) P %.3f   zero %s", a$column[k], a$top10_overlap.blank[k], a$worst_state_top10.blank[k],
                                a$mean_p.blank[k], if (is.na(a$top10_overlap.zero[k])) "-" else
                                  sprintf("%.2f (%.2f) P %.3f", a$top10_overlap.zero[k], a$worst_state_top10.zero[k], a$mean_p.zero[k]))
