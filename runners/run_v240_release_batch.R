# v2.4.0 release batch: regenerate all 49 states' blended delivery lists with
# the fresh-share walk (findings 33-34; SORT_WALK_USE_FRESH_SHARE = TRUE,
# SORT_WALK_MIN_FRESH_SHARE = 0.60, the builder defaults). MINING_FRAMES is
# pinned to any_error: the delivery vocabulary since findings 17, and the
# vocabulary of the committed lists this release supersedes. Pools come from
# the v3 caches, so this is a walk-only rebuild: same vocabulary, same
# consumed workload per state (the pass-zero construction), new rule
# composition where the floor binds.
reg_model_data <- readRDS("reg_model_data.rds")
MINING_FRAMES <- "any_error"
states <- sort(unique(as.character(reg_model_data$state)))
cat(sprintf("v2.4.0 release batch: %d states\n", length(states)))
for (DELIVERY_STATE in states) {
  cat(sprintf("\n########## %s ##########\n", DELIVERY_STATE))
  source("INCL_build_blended_delivery_list_v2.R")
}
cat("\n=== v2.4.0 release batch complete ===\n")
