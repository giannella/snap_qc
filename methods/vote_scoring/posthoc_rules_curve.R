# Post-hoc FY2019 check (2026-10-28, after the exploration choices): top-5% error dollars per reviewed
# case at R = 100..900 rules, representative vote, all three seeds. R = 400-900 were not declared candidates.
#   Rscript methods/vote_scoring/posthoc_rules_curve.R   -> fy2019/posthoc_rules_curve.csv
ex <- parse("C:/Users/ericg/snap_qc/methods/vote_scoring/explore_fy2019.R")
stop_at <- which(vapply(ex, function(e) grepl("^SV <- ", paste(deparse(e), collapse = "")), logical(1)))
for (e in ex[seq_len(stop_at - 1)]) eval(e, globalenv())
RS <- c(100L, 200L, 300L, 400L, 500L, 600L, 750L, 900L)
DEF <- "rep"; PFORM <- "vote"; SFORM <- "benefit_size_vote"
G <- readRDS(file.path(OUT, "explore_groups.rds"))
sim_f <- top_cases(simple, zero)
res <- list()
for (s in names(G)) {
  g <- G[[s]]; lead <- g[g$lead, ]
  fl <- flags_for_rules(lead, F$adf[te, , drop = FALSE], S_te)
  for (R in RS) {
    per <- R %/% 3L; keep <- lead$group <= per
    if (any(vapply(HH_LEVELS, function(l) sum(keep & lead$hh == l) < per, logical(1)))) next
    v <- votes_of(lead[keep, ], fl[keep], NT)
    sc <- oof_p("vote", v$vote) * oof_size(SFORM, v$vote)
    set.seed(7); f <- top_cases(sc, ben)
    res[[length(res) + 1]] <- data.frame(seed = s, R = R, reviewed = length(f), errors = sum(ie[f]), precision = mean(ie[f]),
      dollar_recall = sum(ed[f]) / sum(ed), per_case = per_case(f), gain = per_case(f) / per_case(sim_f) - 1,
      precision_gain = mean(ie[f]) / mean(ie[sim_f]) - 1,
      logloss = logloss(oof_p("vote", v$vote), ie), flagged = mean(v$vote > 0))
  }
}
cat(sprintf("simple ordering: %d reviewed, %d errors, precision %.4f, dollar recall %.4f, $%.2f per case; random $%.2f; FY2019 base rate %.4f
",
  length(sim_f), sum(ie[sim_f]), mean(ie[sim_f]), sum(ed[sim_f]) / sum(ed), per_case(sim_f), sum(ed) / NT, mean(ie)))
out <- bind_rows(res); write.csv(out, file.path(OUT, "posthoc_rules_curve.csv"), row.names = FALSE)
print(out %>% group_by(R) %>% summarise(seeds = n(), precision = mean(precision), dollar_recall = mean(dollar_recall), precision_gain = mean(precision_gain), per_case = mean(per_case), gain_min = min(gain), gain_max = max(gain), gain = mean(gain), logloss = mean(logloss), flagged = mean(flagged)) %>% as.data.frame(), digits = 4)
