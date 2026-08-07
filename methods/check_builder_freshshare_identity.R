# Identity check for the fresh-share knobs in the delivery builder
# (findings 33-34 release gate). With SORT_WALK_USE_FRESH_SHARE = FALSE, and
# separately TRUE with the threshold at 0, the builder must reproduce the
# committed delivery lists BYTE-IDENTICALLY: at floor 0 (or FALSE) the
# builder outputs its legacy pass-zero scan directly, so legacy behavior is
# the legacy code itself. Three states x two budgets x two modes;
# California's committed 5% list is national-only (its 10% list carries 16
# state buffer rules), the other two carry state rules throughout, so both
# pool compositions are exercised.
#
# Uses the existing pool caches (no mining) and builds into a temp directory;
# state_delivery_lists/ is never written. The committed lists are any-error
# vocabulary, so MINING_FRAMES is pinned to "any_error".
# Expects `reg_model_data`. Run:
#   Rscript -e "reg_model_data <- readRDS('reg_model_data.rds'); source('methods/check_builder_freshshare_identity.R')"

if (!exists("reg_model_data")) reg_model_data <- readRDS("reg_model_data.rds")

STATES <- c("Connecticut", "Michigan", "California")
MODES  <- list(use_false  = list(use = FALSE, thr = 0.60),  # FALSE ignores the threshold
               floor_zero = list(use = TRUE,  thr = 0))
TMP <- file.path(tempdir(), "freshshare_identity")
dir.create(TMP, recursive = TRUE, showWarnings = FALSE)

n_pass <- 0L; n_tot <- 0L
for (S in STATES) for (mn in names(MODES)) {
  cat(sprintf("=== building %s under %s ===\n", S, mn))
  e <- new.env(parent = globalenv())
  assign("DELIVERY_STATE", S, envir = e)
  assign("MINING_FRAMES", "any_error", envir = e)
  assign("SORT_WALK_USE_FRESH_SHARE", MODES[[mn]]$use, envir = e)
  assign("SORT_WALK_MIN_FRESH_SHARE", MODES[[mn]]$thr, envir = e)
  assign("out_dir", TMP, envir = e)
  source("INCL_build_blended_delivery_list_v2.R", local = e)
  for (b in c("05", "10")) {
    n_tot <- n_tot + 1L
    fn  <- sprintf("blended_delivery_%s_2022_2024_budget%s.csv", gsub(" ", "_", S), b)
    got <- readBin(file.path(TMP, fn), "raw", n = file.size(file.path(TMP, fn)))
    ref <- readBin(file.path("state_delivery_lists", fn), "raw",
                   n = file.size(file.path("state_delivery_lists", fn)))
    same <- identical(got, ref)
    if (same) n_pass <- n_pass + 1L
    cat(sprintf("  %-12s %-10s budget %s: %s\n", S, mn, b,
                ifelse(same, "byte-identical", "*** DIFFERS ***")))
  }
}
cat(sprintf("\nfresh-share identity: %d/%d rebuilt lists byte-identical to the committed lists\n",
            n_pass, n_tot))
unlink(TMP, recursive = TRUE)
if (n_pass != n_tot) stop("fresh-share identity check FAILED")
