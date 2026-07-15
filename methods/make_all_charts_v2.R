# Master chart runner: regenerates EVERY chart from its saved results CSVs
# (no mining -- seconds per script). Run after any benchmark refresh or chart
# convention change so figures never drift from the data or from each other.
# Convention (2026-07-12): state-by-state charts list states alphabetically.
#
#   Rscript methods/make_all_charts_v2.R

CHART_SCRIPTS <- c(
  "methods/visualize_budget_dotplots_v2.R",           # same-era budget dotplots
  "methods/visualize_loo_vs_nb_v2.R",                 # same-era LOO vs NB views
  "methods/visualize_deployment_own_vs_national_v2.R",# 2024-test dotplots + own-vs-national
  "methods/visualize_deployment_workshop_v2.R",       # 18-state workshop versions
  "methods/visualize_frozen_lists_v2.R",              # frozen vs batch panels
  "methods/visualize_blend_vs_regimes_v2.R",          # blend vs national vs own
  "methods/visualize_state_adaptation_v2.R",          # four adaptation arms per state
  "methods/visualize_mtry_frontier_v2.R",             # constrained-RF (mtry) frontier
  "methods/visualize_two_regime_choice_v2.R"          # two-regime best-of chart
)
for (f in CHART_SCRIPTS) {
  cat(sprintf("\n== %s ==\n", f))
  local(source(f, local = TRUE))
}
cat("\nall charts regenerated\n")
