# v1 (RuleFit/{pre}-based). Still supported; documented in the README legacy section.
# Recommended successor: tune_engine_params_v2.R — see README "Migrating from v1 to v2".
# ──────────────────────────────────────────────────────────────────────────────
# pre() parameter sweep for SNAP review targeting (companion to by_hh_size_6)
#
# Fits RuleFit once per configuration, then draws each fit's precision-recall
# frontier (built with the same greedy "net" as script 6) so you can see how the
# PR curve moves as you vary one pre() parameter at a time:
#
#   maxdepth   (2, 3, 4)        rpart trees
#   learnrate                   rpart trees
#   ntrees                      rpart trees
#   sampfrac                    rpart trees
#
# Each parameter is swept one-at-a-time around a baseline; the baseline value is
# included in its own sweep, so every facet shows the baseline plus its variations.
# verbose = TRUE on every fit, as requested.
#
# It sweeps on ONE pooled dataset (all household sizes; HH size stays a predictor),
# fits on TRAIN_YEARS, and scores the PR frontier on HOLDOUT_YEARS (set that to
# NULL to score in-sample). Every fit is wrapped so an invalid parameter combo is
# logged and skipped rather than killing the run.
# ──────────────────────────────────────────────────────────────────────────────

library(pre)
library(dplyr)
library(ggplot2)
set.seed(117)

## ── 0. Config ─────────────────────────────────────────────────────────────────

# error_status-filtered universe (NO year filter here; years are split below).
focal_df <- reg_model_data %>%
  filter(error_status %in% c("earned_overissuance", "unearned_overissuance", "no_error"))

YEAR_COL      <- "fiscal_year"
TRAIN_YEARS   <- c("2022", "2024")
HOLDOUT_YEARS <- c("2023")     # set to NULL to score the frontier in-sample

OBJECTIVE <- "dollars"         # "counts" or "dollars"; sets the recall basis on the x-axis
PENALTY   <- "lambda.min"      # min (not 1se): a sweep needs enough rules to differentiate;
# 1se is so sparse that ntrees/maxdepth collapse onto one curve
NET_EPS   <- 1

TARGET_IS_ERROR <- quote(!is.na(over_threshold) & over_threshold == 1)
ERR_AMT_COL     <- "total_error_amount"

features <- c(
  "HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size",
  "expedited_i", "cat_elig", "rawben_rel_max", "medical_deductions",
  "shelter_expenses_by_hh_size", "utilities", "married", "homeless",
  "rawearn_by_hh_size", "rawunearn_by_hh_size", "rawgross_by_hh_size",
  "percent_abawd", "unc_rawben_rel_max",
  "months_since_cert_n", "count_divisible_by_100"
)

# Baseline (the point each one-at-a-time sweep moves away from) and the swept values.
# sampfrac is the focus: a fine grid over (0, 0.5]. sampfrac must be > 0, so it
# starts at 0.05, not 0. maxdepth and ntrees stay at baseline (single curve each).
BASE <- list(maxdepth = 4L, learnrate = 0.01, ntrees = 2500, sampfrac = 0.2)
MAXDEPTH_VALS  <- c(4L)
LEARNRATE_VALS <- c(0.005, 0.01)
NTREES_VALS    <- c(2500)
SAMPFRAC_VALS  <- seq(0.10, 0.50, by = 0.10)

out_dir <- "methods/parameter_tuning"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
stopifnot(OBJECTIVE %in% c("counts", "dollars"))

## ── 1. Helpers ────────────────────────────────────────────────────────────────

flag_rule <- function(rule, data) {
  out <- tryCatch(with(data, eval(parse(text = rule))), error = function(e) rep(NA, nrow(data)))
  out[is.na(out)] <- FALSE
  as.logical(out)
}

make_target <- function(df) {
  ie <- eval(TARGET_IS_ERROR, envir = df); ie[is.na(ie)] <- FALSE
  amt <- if (ERR_AMT_COL %in% names(df)) df[[ERR_AMT_COL]] else rep(0, nrow(df))
  amt[is.na(amt)] <- 0
  list(ie = ie, ed = ifelse(ie, abs(amt), 0))
}

# Cairo-backed save, to dodge wedged default graphics devices on Windows.
save_png <- function(plot, file, w, h, dpi = 300) {
  png(file, width = w, height = h, units = "in", res = dpi, type = "cairo")
  on.exit(dev.off()); print(plot)
}

# Greedy net: OR rules to climb recall at the best value-per-clean trade, recording
# the cumulative operating point after each addition (the PR frontier).
greedy_path <- function(flag_list, ie, ed) {
  N <- length(ie); total_err <- sum(ie); total_dol <- sum(ed)
  if (length(flag_list) == 0 || total_err == 0) return(tibble())
  protect <- if (OBJECTIVE == "dollars") ed else as.numeric(ie)
  flagged <- rep(FALSE, N); remaining <- seq_along(flag_list); path <- list(); step <- 0L
  repeat {
    best <- NULL; best_score <- -Inf; best_new <- NULL
    for (k in remaining) {
      nf <- flagged | flag_list[[k]]; newc <- nf & !flagged
      if (!any(newc)) next
      sc <- sum(protect[newc]) / (sum(newc & !ie) + NET_EPS)
      if (sc > best_score) { best_score <- sc; best <- k; best_new <- nf }
    }
    if (is.null(best)) break
    flagged <- best_new; remaining <- setdiff(remaining, best); step <- step + 1L
    nfl <- sum(flagged); tp <- sum(flagged & ie)
    path[[step]] <- tibble(
      step = step, n_flagged = nfl, workload = nfl / N,
      recall = tp / total_err,
      dollar_recall = if (total_dol > 0) sum(ed[flagged]) / total_dol else NA_real_,
      precision = tp / nfl
    )
    if (length(remaining) == 0) break
  }
  out <- bind_rows(path)
  if (nrow(out) > 0) out$x <- if (OBJECTIVE == "dollars") out$dollar_recall else out$recall
  out
}

# Keep INCLUDE-direction rules (matched TRAIN subset dirtier / higher $ density).
keep_include <- function(rules, train_data, ie_tr, ed_tr) {
  if (length(rules) == 0) return(character(0))
  base_rate <- mean(ie_tr); base_dens <- if (sum(ed_tr) > 0) sum(ed_tr) / length(ed_tr) else NA_real_
  ok <- vapply(rules, function(rd) {
    f <- flag_rule(rd, train_data); if (sum(f) == 0) return(FALSE)
    if (OBJECTIVE == "dollars") (sum(ed_tr[f]) / sum(f)) > base_dens else mean(ie_tr[f]) > base_rate
  }, logical(1))
  unique(rules[ok])
}

## ── 2. Prepare the sweep data (fit on TRAIN, score frontier on HOLDOUT) ───────

yr        <- as.character(focal_df[[YEAR_COL]])
train_src <- focal_df[yr %in% as.character(TRAIN_YEARS), , drop = FALSE]
eval_src  <- if (is.null(HOLDOUT_YEARS)) train_src else focal_df[yr %in% as.character(HOLDOUT_YEARS), , drop = FALSE]

train_src$over_threshold <- as.integer(as.character(train_src$over_threshold))
eval_src$over_threshold  <- as.integer(as.character(eval_src$over_threshold))

# One pooled model across all household sizes: HH_size_n stays in `features` as a predictor.
tg_tr <- make_target(train_src); ie_tr0 <- tg_tr$ie; ed_tr0 <- tg_tr$ed
train_src$.is_error <- ie_tr0
pv <- features
pv <- pv[pv %in% names(train_src)]
pv <- pv[sapply(train_src[pv], function(x) !all(is.na(x)) && length(unique(x[!is.na(x)])) > 1)]

cc <- stats::complete.cases(train_src[c(".is_error", pv)])
train_data <- train_src[cc, , drop = FALSE]; ed_tr <- ed_tr0[cc]; ie_tr <- train_data$.is_error
to_factor <- pv[vapply(train_data[pv], function(x) is.character(x) || is.logical(x), logical(1))]
for (v in to_factor) train_data[[v]] <- factor(train_data[[v]])
train_data[pv] <- lapply(train_data[pv], function(x) if (is.factor(x)) droplevels(x) else x)
pv <- pv[vapply(train_data[pv], function(x) length(unique(x)) > 1, logical(1))]

if (OBJECTIVE == "dollars") { train_data$.target <- ed_tr; fam <- "gaussian" } else { train_data$.target <- factor(ifelse(ie_tr, "error", "clean"), levels = c("error", "clean")); fam <- "binomial" }
form <- as.formula(paste(".target ~", paste(pv, collapse = " + ")))

tg_ev <- make_target(eval_src); ie_ev <- tg_ev$ie; ed_ev <- tg_ev$ed
cat(sprintf("Pooled sweep (all HH) | train %d rows (%d errors) | eval %d rows (%d errors) | %d predictors\n",
            nrow(train_data), sum(ie_tr), nrow(eval_src), sum(ie_ev), length(pv)))

## ── 3. Build the configuration list ───────────────────────────────────────────

build_args <- function(cfg) {
  list(
    formula = form, data = train_data[c(".target", pv)], family = fam, type = "rules",
    ntrees = cfg$ntrees, maxdepth = as.integer(cfg$maxdepth), sampfrac = cfg$sampfrac,
    learnrate = cfg$learnrate,
    use.grad = TRUE, tree.unbiased = FALSE, randomForest = FALSE,
    removeduplicates = TRUE, removecomplements = TRUE,
    nfolds = 5, verbose = TRUE
  )
}

configs <- list()
add_cfg <- function(...) configs[[length(configs) + 1]] <<- list(...)

for (v in MAXDEPTH_VALS)  add_cfg(method = "rpart", sweep = "maxdepth",  label = as.character(v),
                                  maxdepth = v, learnrate = BASE$learnrate, ntrees = BASE$ntrees, sampfrac = BASE$sampfrac,
                                  mtry = NULL, tree.unbiased = FALSE)
for (v in LEARNRATE_VALS) add_cfg(method = "rpart", sweep = "learnrate", label = format(v),
                                  maxdepth = BASE$maxdepth, learnrate = v, ntrees = BASE$ntrees, sampfrac = BASE$sampfrac,
                                  mtry = NULL, tree.unbiased = FALSE)
for (v in NTREES_VALS)    add_cfg(method = "rpart", sweep = "ntrees",    label = as.character(v),
                                  maxdepth = BASE$maxdepth, learnrate = BASE$learnrate, ntrees = v, sampfrac = BASE$sampfrac,
                                  mtry = NULL, tree.unbiased = FALSE)
for (v in SAMPFRAC_VALS)  add_cfg(method = "rpart", sweep = "sampfrac",  label = format(v),
                                  maxdepth = BASE$maxdepth, learnrate = BASE$learnrate, ntrees = BASE$ntrees, sampfrac = v,
                                  mtry = NULL, tree.unbiased = FALSE)

## ── 4. Fit each config and build its PR frontier ──────────────────────────────

evaluate_config <- function(cfg) {
  cat(sprintf("\n========== %s = %s (%s) ==========\n", cfg$sweep, cfg$label, cfg$method))
  fit <- tryCatch(do.call(pre, build_args(cfg)),
                  error = function(e) { message("  fit failed: ", conditionMessage(e)); NULL })
  if (is.null(fit)) return(NULL)
  rules <- tryCatch({
    gr <- function(pp) coef(fit, penalty.par.val = pp) %>% filter(rule != "(Intercept)", coefficient != 0)
    r0 <- gr(PENALTY)
    if (nrow(r0) == 0 && PENALTY == "lambda.1se") r0 <- gr("lambda.min")
    r0$description
  }, error = function(e) { message("  coef failed: ", conditionMessage(e)); character(0) })
  
  rules <- keep_include(rules, train_data, ie_tr, ed_tr)
  cat(sprintf("  INCLUDE rules: %d\n", length(rules)))
  if (length(rules) == 0) return(NULL)
  
  gp <- greedy_path(lapply(rules, flag_rule, data = eval_src), ie_ev, ed_ev)
  if (nrow(gp) == 0) return(NULL)
  gp$sweep <- cfg$sweep; gp$label <- cfg$label; gp$method <- cfg$method
  gp$n_rules <- length(rules)
  gp
}

paths <- bind_rows(lapply(configs, evaluate_config))
if (nrow(paths) == 0) stop("No configuration produced a usable PR frontier.")
write.csv(paths, file.path(out_dir, "param_sweep_pr_paths.csv"), row.names = FALSE)

# per-config summary: rule count and how far up the recall axis each curve reaches
summary_tbl <- paths %>% group_by(sweep, label, method) %>%
  summarise(n_rules = first(n_rules), max_recall = round(max(x, na.rm = TRUE), 3),
            precision_at_max = round(precision[which.max(x)], 3), .groups = "drop")
write.csv(summary_tbl, file.path(out_dir, "param_sweep_summary.csv"), row.names = FALSE)
cat("\n"); print(as.data.frame(summary_tbl))

## ── 5. Plot: one panel per swept parameter, each with its own legend ──────────

# Report any configuration that produced no frontier (failed fit or no INCLUDE rules).
all_cfg <- bind_rows(lapply(configs, function(c) tibble(sweep = c$sweep, label = c$label)))
got     <- distinct(paths, sweep, label)
missing <- anti_join(all_cfg, got, by = c("sweep", "label"))
if (nrow(missing) > 0) {
  cat("\nConfigs that produced NO frontier (fit failed or no INCLUDE rules):\n")
  print(as.data.frame(missing))
}

xlab <- if (OBJECTIVE == "dollars") "Recall of error dollars" else "Recall of errors"
sub  <- sprintf("all household sizes pooled | trained %s | scored %s | penalty %s",
                paste(TRAIN_YEARS, collapse = "/"),
                if (is.null(HOLDOUT_YEARS)) "in-sample" else paste(HOLDOUT_YEARS, collapse = "/"),
                PENALTY)

# Build a separate plot per sweep so each gets its own (numerically ordered) legend.
make_panel <- function(sw) {
  d    <- paths %>% filter(sweep == sw)
  labs <- unique(d$label)
  nums <- suppressWarnings(as.numeric(gsub("mtry=", "", labs)))
  ord  <- if (all(!is.na(nums))) labs[order(nums)] else sort(labs)
  d$label <- factor(d$label, levels = ord)
  ggplot(d, aes(x, precision, color = label, group = label)) +
    geom_line(linewidth = 0.7) + geom_point(size = 0.9) +
    labs(x = xlab, y = "Precision", color = sw, title = sw) +
    theme_minimal(base_size = 11) + theme(legend.position = "right")
}
panels <- lapply(unique(paths$sweep), make_panel)

if (requireNamespace("patchwork", quietly = TRUE)) {
  combined <- patchwork::wrap_plots(panels, ncol = 2) +
    patchwork::plot_annotation(title = "pre() parameter sweep: precision-recall frontier",
                               subtitle = sub)
  save_png(combined, file.path(out_dir, "param_sweep_pr.png"), 13, 9)
} else {
  # no patchwork: one file per parameter
  for (i in seq_along(panels))
    save_png(panels[[i]], file.path(out_dir,
                                    sprintf("param_sweep_pr_%s.png", gsub("[^a-z0-9]+", "_", tolower(unique(paths$sweep)[i])))),
             7, 5)
}
cat(sprintf("\nWrote plot(s) and CSVs to %s/\n", out_dir))

## ── 6. Notes ──────────────────────────────────────────────────────────────────
# - One parameter is varied at a time around BASE; the baseline value sits inside
#   each sweep, so a facet shows the baseline curve plus its neighbours.
# - Curves are scored on HOLDOUT_YEARS by default, so a more complex fit that only
#   raises in-sample precision will show up as a curve that does NOT improve (or
#   drops) here. Set HOLDOUT_YEARS <- NULL to see the in-sample frontier instead.
# - Every fit is wrapped in tryCatch; if a parameter value errors, the console logs
#   it and the sweep continues, and the run prints any configs that produced nothing.
# - Runtime scales with the number of configs x ntrees; trim the *_VALS vectors for
#   a quick pass. verbose = TRUE prints pre()'s progress for every fit, as requested.