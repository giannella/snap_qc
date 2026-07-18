# Reconstructed generating script for presentation_figures/winners_curse_raw_vs_lcb.png.
# The original was never committed and the raw-vs-lower-bound decay is not in any
# committed CSV, so this RE-MINES the any-error frame and sweeps the two selection
# rules through the shared pipeline (rule_mining_helpers.R):
#   - red  = keep rules whose RAW training precision clears the floor
#   - blue = keep rules whose 99% Wilson LOWER BOUND clears the floor
# At each floor we plot the kept union's actual precision on a held-out year. Raw
# selection over-promises (it keeps lucky rules); the lower bound keeps its promise.
# The dashed diagonal is "what was promised" (y = x).
#
# Fresh run on the current frame, so numbers may differ slightly from the original PNG.
# Needs reg_model_data.rds present. Takes a few minutes (1000 rounds + 1000 trees).
#   Rscript methods/draw_winners_curse_raw_vs_lcb.R
suppressMessages({library(ggplot2); library(dplyr); library(ranger); library(xgboost)})
source("rule_mining_helpers.R")

YEAR_COL <- "fiscal_year"; TRAIN_YEARS <- c("2022","2024"); HOLDOUT_YEARS <- c("2023")
TARGET_IS_ERROR <- quote(!is.na(over_threshold) & over_threshold != 0)
ERR_AMT_COL <- "total_error_amount"
HH_SIZE_COL <- "cert_HH_size_FS_n"; HH_LEVELS <- c("1","2-3","4+")
hh_group_of <- function(n){n<-suppressWarnings(as.numeric(as.character(n)))
  ifelse(is.na(n),NA_character_,ifelse(n<=1,"1",ifelse(n<=3,"2-3","4+")))}
features <- c("HH_size_n","children_i","elderly_disabled_i","total_deductions_by_hh_size",
  "expedited_i","cat_elig","rawben_rel_max","medical_deductions",
  "shelter_expenses_by_hh_size","utilities","married","homeless",
  "rawearn_by_hh_size","rawunearn_by_hh_size","rawgross_by_hh_size",
  "percent_abawd","unc_rawben_rel_max","months_since_cert_n","count_divisible_by_100")
XGB <- list(nrounds=1000,max_depth=4,eta=0.02,subsample=0.20)
RF  <- list(num_trees=1000,max_depth=4,mtry=2,min_node_size=20)
LCB_Z <- 2.326; MIN_TRAIN_FLAGGED <- 10; SIGNIF_DIGITS <- 3

reg_model_data <- readRDS("reg_model_data.rds")
frame_df <- reg_model_data %>%
  filter(error_status %in% c("earned_overissuance","unearned_overissuance",
                             "underissuance","other_error","no_error"),
         .data[[YEAR_COL]] %in% c(TRAIN_YEARS, HOLDOUT_YEARS))

pf <- prep_features(frame_df, features); fdf <- pf$data; pv <- pf$features
yr <- as.character(fdf[[YEAR_COL]])
train <- fdf[yr %in% TRAIN_YEARS,,drop=FALSE]; hold <- fdf[yr %in% HOLDOUT_YEARS,,drop=FALSE]
targets_of <- function(df){ie<-eval(TARGET_IS_ERROR,envir=df);ie[is.na(ie)]<-FALSE
  amt<-df[[ERR_AMT_COL]];amt[is.na(amt)]<-0;list(ie=ie,ed=ifelse(ie,abs(amt),0))}
tg_tr <- targets_of(train); tg_h <- targets_of(hold)
strata_tr <- lapply(setNames(nm=HH_LEVELS),function(h) which(hh_group_of(train[[HH_SIZE_COL]]) %in% h))
strata_h  <- lapply(setNames(nm=HH_LEVELS),function(h) which(hh_group_of(hold[[HH_SIZE_COL]]) %in% h))

rules_df <- mine_rule_vocabulary(
  train, setNames(list(list(rows=seq_len(nrow(train)), ie=tg_tr$ie)), "any_error"),
  strata_tr, pv, xgb=XGB, rf=RF, signif_digits=SIGNIF_DIGITS, seed=117)

idx_tr <- flags_for_rules(rules_df, train, strata_tr, label="train")
n_tr <- lengths(idx_tr); k_tr <- vapply(idx_tr,function(ix) sum(tg_tr$ie[ix]),numeric(1))
keep <- n_tr >= MIN_TRAIN_FLAGGED                       # support floor only, no raw>base screen
rules_df <- rules_df[keep,,drop=FALSE]; idx_tr <- idx_tr[keep]; n_tr <- n_tr[keep]; k_tr <- k_tr[keep]
drop_cov <- dedup_exact_coverage(rules_df, idx_tr)      # unbiased dedup only
rules_df <- rules_df[!drop_cov,,drop=FALSE]; idx_tr <- idx_tr[!drop_cov]
n_tr <- n_tr[!drop_cov]; k_tr <- k_tr[!drop_cov]

raw_tr <- k_tr / n_tr
lcb_tr <- wilson_lcb(k_tr, n_tr, LCB_Z)
idx_h  <- flags_for_rules(rules_df, hold, strata_h, label="holdout")

grid <- seq(0.05, 0.50, by = 0.05)
sw_raw <- precision_sweep(raw_tr, !is.na(raw_tr), idx_h, tg_h$ie, tg_h$ed, grid) %>%
  mutate(sel = "keep rules by their raw success rate")
sw_lcb <- precision_sweep(lcb_tr, !is.na(lcb_tr), idx_h, tg_h$ie, tg_h$ed, grid) %>%
  mutate(sel = "keep rules by a cautious (confidence-adjusted) estimate")
sweeps <- bind_rows(sw_raw, sw_lcb)

cols <- c("keep rules by a cautious (confidence-adjusted) estimate" = "#2E75B6",
          "keep rules by their raw success rate" = "#C0392B")
lim <- range(c(grid, sweeps$precision), na.rm = TRUE)

p <- ggplot(sweeps, aes(threshold, precision, colour = sel)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey55") +
  annotate("text", x = 0.42, y = 0.44, label = "what was promised",
           angle = 34, colour = "grey45", size = 4) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  scale_colour_manual(values = cols, name = NULL) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(xlim = c(0.05, 0.50), ylim = c(0.05, max(lim) + 0.02)) +
  labs(
    title = "Rules that look good on their own data disappoint on new data",
    subtitle = paste("Same set of candidate rules, two ways of choosing keepers. Choosing by raw",
                     "success rate keeps\nrules that got lucky, and they fall short on new data by",
                     "more as the bar rises. The cautious\nestimate keeps its promises, up to the",
                     "limit of what the data supports."),
    x = "Success rate required to keep a rule (on the data used to build it)",
    y = "Actual success rate on a year of new data") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top", panel.grid.minor = element_blank())

save_png(p, "presentation_figures/winners_curse_raw_vs_lcb.png", 10.8, 7.0)
cat(sprintf("wrote winners_curse_raw_vs_lcb.png (%d rules after dedup)\n", nrow(rules_df)))
