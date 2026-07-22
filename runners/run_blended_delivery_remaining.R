# Runner: build blended 2022-24 delivery lists for the 30 states that did not
# yet have one, matching the committed recipe exactly (any-error vocabulary,
# fdr10 admission -- the builder defaults; existing plain filenames). The
# national pool is already cached (pool_national_anyerror_fdr10.rds), so each
# state here only mines its own small pool. Outputs -> state_delivery_lists/.
MINING_FRAMES <- "any_error"
reg_model_data <- readRDS("reg_model_data.rds")
for (DELIVERY_STATE in c("Alabama", "Arkansas", "Delaware", "Florida",
                         "Georgia", "Idaho", "Illinois", "Indiana", "Iowa",
                         "Kansas", "Kentucky", "Montana", "Nebraska", "Nevada",
                         "New Hampshire", "New Mexico", "New York",
                         "North Dakota", "Ohio", "Oklahoma", "Oregon",
                         "Pennsylvania", "Rhode Island", "South Carolina",
                         "South Dakota", "Utah", "Vermont", "West Virginia",
                         "Wisconsin", "Wyoming")) {
  cat(sprintf("\n########## %s ##########\n", DELIVERY_STATE))
  source("INCL_build_blended_delivery_list_v2.R")
}
cat("\n=== remaining-states delivery build complete ===\n")
