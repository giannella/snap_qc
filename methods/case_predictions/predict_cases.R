# Per-case predictions for states that deploy a model rather than a workbook: candidate B of §46
# (modeling_findings_detailed.md §46, Amendment 2 and §48; Eric, 2026-11-06). It replaces the option-D
# stack, which is kept in archive_optionD/predict_cases_optionD.R.
#
# B is a tuned xgboost model of P(error over the federal tolerance) with a state term, times a boosted gamma
# model of the error amount given an error (the size), bagged over ten seeds. Expected $ = P x size. Cases are
# ranked within the state by expected $. This is the live script. By default it uses the bundle fitted by
# s46/s46_deploy_f1109.R (s46_post.R's deploy step on frame f1109, the latest reconstructed data) and the
# f1109 delivery lists in state_delivery_lists (Eric, 2026-11-10); it only scores cases with that bundle. The
# selection study and its tests are in s46/ (on f1109: s46_f1109.R, out_f1109_eval/); this script does no
# evaluation of its own.
#
# No level is applied. P and the dollars are the model's own: they are not set to the state's FNS payment
# error rate (Eric, 2026-11-03). Dollars are in FY2024 dollars, the latest year the model was trained on.
#
# Inputs are the 18 leak-free features the miners use (bundle$base_inputs), the state, and the benefit for the
# size model's offset: the recreated benefit (raw_benefit_amount) on the frame, or the issued benefit (rawben)
# in a state's own file, which has only that one. The delivery lists are not inputs; the output names the
# state's list rules that flag each case, as an explanation.
#
# Usage (from the repo root):
#   PC_STATE=Washington [PC_INPUT=cases.csv] [PC_YEARS=2024] Rscript methods/case_predictions/predict_cases.R
#     PC_INPUT: the state's cases, one row per case, with the 18 input columns, rawben or raw_benefit_amount,
#               and optionally CASE_ID, cert_HH_size_FS_n and count_divisible_by_100 (the list rules need all
#               19 miner features to name the rules that flag a case). Without it, the state's frame rows for PC_YEARS
#               are scored (in-sample: a demonstration only). Writes predictions_<State>.csv.
#   PC_MODE=check Rscript methods/case_predictions/predict_cases.R
#     scores every state's FY2024 frame rows and checks P and the model's dollar rate against the levels the
#     deploy step computed inside the bundle; a mismatch stops the script.
#   PC_BUNDLE (default s46/out_f1109/deploy_B_bundle.rds); SNAP_LIST_DIR (default state_delivery_lists).
#   The frame read for in-sample scoring and the check is the bundle's own (b$frame).
source("methods/v270_seed_replicates/common.R")   # FRAMES, VOCAB19, prep_features, hh_group_of, flags_for_rules, stamp
suppressMessages(library(xgboost))
MODE <- Sys.getenv("PC_MODE", "predict"); stopifnot(MODE %in% c("predict", "check"))
OUT <- "methods/case_predictions"
BUNDLE <- Sys.getenv("PC_BUNDLE", file.path(OUT, "s46/out_f1109/deploy_B_bundle.rds"))
LIST_DIR <- Sys.getenv("SNAP_LIST_DIR", "state_delivery_lists")
b <- readRDS(BUNDLE)
stopifnot(!is.null(b$frame), b$frame %in% names(FRAMES), length(b$models) == 10)
CPI_LAST <- unname(b$cpi_by_year[length(b$cpi_by_year)])   # training dollars are deflated by year; report in the latest year's $

as_num <- function(v) if (is.logical(v)) as.numeric(v) else if (is.character(v))
  as.numeric(c("TRUE" = 1, "FALSE" = 0)[toupper(v)]) else as.numeric(v)

# the design matrix in the bundle's column order: base inputs, within state-year percentiles if the bundle uses
# them, then the state one-hot
design <- function(df, state) {
  stopifnot(all(b$base_inputs %in% names(df)), state %in% b$states)
  M <- sapply(b$base_inputs, function(f) as_num(df[[f]])); if (!is.matrix(M)) M <- matrix(M, nrow = 1, dimnames = list(NULL, b$base_inputs))
  if (b$pct) {
    pf <- sub("^pct_", "", grep("^pct_", b$inputs, value = TRUE)); yr <- if ("fiscal_year" %in% names(df)) as.character(df$fiscal_year) else rep("", nrow(df))
    P <- sapply(pf, function(f) ave(as_num(df[[f]]), yr, FUN = function(v) rank(v, na.last = "keep", ties.method = "average") / sum(!is.na(v))))
    if (!is.matrix(P)) P <- matrix(P, nrow = 1); colnames(P) <- paste0("pct_", pf); M <- cbind(M, P) }
  S <- matrix(0, nrow(df), length(b$states), dimnames = list(NULL, paste0("st_", make.names(b$states))))
  S[, paste0("st_", make.names(state))] <- 1
  M <- cbind(M, S); stopifnot(identical(colnames(M), b$inputs)); storage.mode(M) <- "double"; M
}
lben_of <- function(df) log1p(pmax(if ("raw_benefit_amount" %in% names(df)) df$raw_benefit_amount else df$rawben, 0))

# bagged P and size (size in training dollars, i.e. deflated) for one state's cases
score_B <- function(df, state) {
  M <- design(df, state); dm <- xgb.DMatrix(M)
  off <- b$size_offset_glm
  dms <- if (is.null(off)) dm else xgb.DMatrix(M, base_margin = as.numeric(off[1] + off[2] * lben_of(df)))
  p <- rowMeans(sapply(b$models, function(m) predict(xgb.load.raw(m$p), dm)))
  sz <- rowMeans(sapply(b$models, function(m) predict(xgb.load.raw(m$size), dms)))
  data.frame(p_error = p, size = sz)
}

# the frame rows, prepared on the training years together, as the deploy step prepared them
frame_rows <- function() {
  reg <- readRDS(FRAMES[[b$frame]]); stopifnot(nrow(reg) == FRAME_ROWS[[b$frame]])
  reg <- reg[as.character(reg$fiscal_year) %in% b$train_years, ]
  a <- prep_features(reg, VOCAB19)$data; stopifnot(nrow(a) == nrow(reg)); a
}

if (MODE == "check") {
  a <- frame_rows(); a <- a[as.character(a$fiscal_year) == "2024", ]
  lv <- b$levels; d <- data.frame(state = lv$state, p_diff = NA_real_, rate_diff = NA_real_)
  for (k in seq_len(nrow(lv))) { w <- which(as.character(a$state) == lv$state[k]); s <- score_B(a[w, ], lv$state[k])
    stopifnot(length(w) == lv$n_fy2024[k])
    d$p_diff[k] <- mean(s$p_error) - lv$model_mean_p[k]
    d$rate_diff[k] <- sum(s$p_error * s$size) / sum(pmax(a$rawben[w], 0)) - lv$model_rate[k] }
  stamp("check on %d states' FY2024 rows (bundle %s, frame %s): largest |mean P difference| %.2e, largest |dollar-rate difference| %.2e",
        nrow(d), BUNDLE, b$frame, max(abs(d$p_diff)), max(abs(d$rate_diff)))
  stopifnot(max(abs(d$p_diff)) < 1e-9, max(abs(d$rate_diff)) < 1e-9)
  stamp("check passed")
}

if (MODE == "predict") {
  STATE <- Sys.getenv("PC_STATE"); stopifnot(nzchar(STATE))
  inp <- Sys.getenv("PC_INPUT")
  if (nzchar(inp)) {
    df <- read.csv(inp, stringsAsFactors = FALSE, check.names = FALSE)
    id <- if ("CASE_ID" %in% names(df)) df$CASE_ID else seq_len(nrow(df))
  } else {   # the state's frame rows: in-sample, a demonstration only
    yrs <- strsplit(Sys.getenv("PC_YEARS", "2024"), ",")[[1]]; stopifnot(all(yrs %in% b$train_years))
    a <- frame_rows(); df <- a[a$state == STATE & as.character(a$fiscal_year) %in% yrs, ]; stopifnot(nrow(df) > 0)
    id <- paste(df$fiscal_year, df$hhldno, sep = "-")
  }
  s <- score_B(df, STATE)
  dollars <- s$p_error * s$size / CPI_LAST
  hh <- hh_group_of(if ("cert_HH_size_FS_n" %in% names(df)) df$cert_HH_size_FS_n else df$HH_size_n)
  # the state's delivery-list rules that flag each case (explanation, not input)
  lst <- file.path(LIST_DIR, sprintf("blended_delivery_%s_2022_2024_budget10.csv", gsub(" ", "_", STATE)))
  why <- rep("", nrow(df))
  if (!all(VOCAB19 %in% names(df))) {
    stamp("list_rules_flagging left blank: the rules need all of %s", paste(VOCAB19, collapse = ", "))
  } else if (file.exists(lst)) {
    rl <- read.csv(lst, stringsAsFactors = FALSE); rl$hh <- as.character(rl$hh)
    fl <- flags_for_rules(rl, df, strata_of(hh))
    hits <- split(rep(rl$rank, lengths(fl)), factor(unlist(fl), levels = seq_len(nrow(df))))
    why <- vapply(hits, function(r) paste(sort(r), collapse = " "), "")
  } else stamp("no list at %s: list_rules_flagging left blank", lst)
  o <- data.frame(case = id, hh_group = hh, issued_benefit = if ("rawben" %in% names(df)) df$rawben else df$raw_benefit_amount,
                  p_error = round(s$p_error, 4), expected_error_dollars = round(dollars, 2),
                  error_size_if_error = round(s$size / CPI_LAST, 2),
                  priority_rank = rank(-dollars, ties.method = "first"), rank_by_p_error = rank(-s$p_error, ties.method = "first"),
                  list_rules_flagging = why, stringsAsFactors = FALSE)
  o <- o[order(o$priority_rank), ]
  fn <- file.path(OUT, sprintf("predictions_%s.csv", gsub(" ", "_", STATE)))
  write.csv(o, fn, row.names = FALSE)
  stamp("%s: %d cases scored -> %s (bundle %s, frame %s, trained FY%s); mean P %.3f, expected error $ per case %.2f (FY2024 $)",
        STATE, nrow(o), fn, basename(BUNDLE), b$frame, paste(b$train_years, collapse = "+"), mean(s$p_error), mean(dollars))
}
