# Overnight: NB similarity -> benchmark (5 sims + LOO national) -> budgeted menu
reg_model_data <- readRDS("reg_model_data.rds")
source("methods/state_nb_similarity_v2.R")
source("methods/neighbor_transfer_benchmark_v2.R")
source("methods/budgeted_transfer_menu_v2.R")
cat("\n=== overnight NB + LOO run complete ===\n")
