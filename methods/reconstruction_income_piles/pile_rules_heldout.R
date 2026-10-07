# Do rules that lean on income piles hold up on held-out years? (2026-10-07)
# Pool: the v2.7 benchmark's national pool mined on FY2017-19 (seed 117, final
# frame; methods/v270_cpi_benchmark/fy2022_cpi/cache), artifact-tagged rules
# dropped. A rule is pile-tagged when >= 25% of its training flags or errors
# are pile rows (income_pile_rows(), defined within each fiscal year). Every
# tagged rule and an equal-sized comparison sample of untagged rules with the
# same training-bound distribution (matched by bound decile) are scored on
# FY2022-24, which no rule saw: on all test cases, and on test cases that are
# not pile cases. The test years carry the same reconstruction, so the
# all-cases reading still includes the piles; the non-pile reading is the one
# that shows whether a rule's precision rests on them.
#   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" methods/reconstruction_income_piles/pile_rules_heldout.R
setwd("C:/Users/ericg/snap_qc_v27")
suppressMessages(library(dplyr))
source("rule_mining_helpers.R")
set.seed(117)
V <- c("HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size", "expedited_i",
       "bbce_state_i", "rawben_rel_max", "medical_deductions", "shelter_expenses_by_hh_size", "utilities_sua",
       "married", "homeless", "percent_abawd", "unc_rawben_rel_max", "months_since_cert_n",
       "count_divisible_by_100", "gross_by_hh_size", "earned_by_hh_size", "unearned_by_hh_size")
d0 <- readRDS("C:/Users/ericg/snap_qc/reg_model_data.rds")
d0 <- d0[d0$fiscal_year %in% c(2017:2019, 2022:2024), ]
d <- prep_features(d0, V)$data
pile <- income_pile_rows(d0)
ie <- !is.na(d$over_threshold) & d$over_threshold != 0
yr <- as.integer(as.character(d$fiscal_year))
n_ <- suppressWarnings(as.numeric(as.character(d$cert_HH_size_FS_n)))
hh <- ifelse(n_ <= 1, "1", ifelse(n_ <= 3, "2-3", "4+"))
tr <- yr <= 2019; te <- yr >= 2022
stopifnot(sum(tr) == 116060L, sum(ie[tr]) == 10920L)
p <- readRDS("C:/Users/ericg/snap_qc/methods/v270_cpi_benchmark/fy2022_cpi/cache/bench_national_117.rds")
p <- p[!(round(p$mm_n / p$n, 4) >= 0.25 | round(ifelse(p$k > 0, p$mm_k / p$k, 0), 4) >= 0.25), ]
st_of <- function(rows) lapply(setNames(nm = c("1", "2-3", "4+")), function(h) which(hh[rows] == h))

# pile counts on the training pile rows only (a rule's flags among them)
pr <- which(tr & pile)
sc <- reduce_flags_for_rules(p, d[pr, , drop = FALSE], st_of(pr), function(ix) c(length(ix), sum(ie[pr][ix])))
p$pile_n <- sc[, 1]; p$pile_k <- sc[, 2]
p$tag <- round(p$pile_n / p$n, 4) >= 0.25 | round(ifelse(p$k > 0, p$pile_k / p$k, 0), 4) >= 0.25
cat(sprintf("pool %d rules (artifact-tagged dropped); pile-tagged %d (%.1f%%); training pile rows %d (errors %d)\n",
            nrow(p), sum(p$tag), 100 * mean(p$tag), length(pr), sum(ie[pr])))

# comparison: untagged rules matched on the tagged rules' training-bound deciles
p$dec <- cut(p$lcb, breaks = unique(quantile(p$lcb[p$tag], 0:10 / 10)), include.lowest = TRUE)
tg <- p[p$tag, ]
ut <- p[!p$tag & !is.na(p$dec), ]
ut <- do.call(rbind, lapply(split(ut, ut$dec, drop = TRUE), function(g) {
  want <- sum(tg$dec == g$dec[1], na.rm = TRUE); g[sample(nrow(g), min(want, nrow(g))), ] }))
both <- rbind(tg, ut)
ter <- which(te)
sc2 <- reduce_flags_for_rules(both, d[ter, , drop = FALSE], st_of(ter), function(ix)
  c(length(ix), sum(ie[ter][ix]), sum(!pile[ter][ix]), sum(ie[ter][ix] & !pile[ter][ix])))
both$n_te <- sc2[, 1]; both$k_te <- sc2[, 2]; both$n_np <- sc2[, 3]; both$k_np <- sc2[, 4]
base <- tapply(ie[ter], hh[ter], mean); base_np <- tapply(ie[te & !pile], hh[te & !pile], mean)
cat(sprintf("FY2022-24 base rates by household size: all cases %s | non-pile cases %s\n",
            paste(names(base), round(base, 3), collapse = " "), paste(names(base_np), round(base_np, 3), collapse = " ")))
for (lab in c("pile-tagged", "untagged, matched")) {
  q <- if (lab == "pile-tagged") both[both$tag, ] else both[!both$tag, ]
  lift_all <- (q$k_te / pmax(q$n_te, 1)) / base[q$hh]; lift_np <- (q$k_np / pmax(q$n_np, 1)) / base_np[q$hh]
  cat(sprintf("%-18s %5d rules | training precision %.3f (bound median %.3f) | held out, all cases: %.3f, median lift %.2f | non-pile cases: %.3f, median lift %.2f | rules below base on non-pile cases %d (%.0f%%)\n",
              lab, nrow(q), sum(q$k) / sum(q$n), median(q$lcb),
              sum(q$k_te) / sum(q$n_te), median(lift_all, na.rm = TRUE),
              sum(q$k_np) / sum(q$n_np), median(lift_np, na.rm = TRUE),
              sum(lift_np < 1, na.rm = TRUE), 100 * mean(lift_np < 1, na.rm = TRUE)))
}
write.csv(both[, c("hh", "rule", "n", "k", "lcb", "pile_n", "pile_k", "tag", "n_te", "k_te", "n_np", "k_np")],
          "methods/reconstruction_income_piles/pile_rules_heldout_fy2022_2024.csv", row.names = FALSE)
cat("PILE RULES HELD-OUT DONE\n")
