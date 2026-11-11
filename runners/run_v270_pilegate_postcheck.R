# Post-step for runners/run_v270_pilegate_build.R (2026-10-07, issue #29).
#  1. Column order: the builders write pile_share_flags / pile_share_errors
#     beside the mm_ audit columns; VERSIONING.md says new output columns are
#     appended, so they move to the end of every shipped list. All other
#     columns and every row are asserted unchanged.
#  2. The gate is the only change (review advisory, 2026-10-07): a list whose
#     v2.7.0 version (git HEAD) carried no pile-tagged rule must come out
#     identical, rule for rule, in the same order and roles. Removing rules
#     the walk never took cannot change the walk, so any difference there
#     would mean something else changed. Tags for the v2.7.0 rules come from
#     methods/reconstruction_income_piles/rule_pile_share.csv, rounded to 4
#     places before the 0.25 comparison as the builders do.
# Writes methods/reconstruction_income_piles/list_changes.csv. Nothing is committed.
#   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" runners/run_v270_pilegate_postcheck.R
setwd("C:/Users/ericg/snap_qc")
suppressMessages(library(dplyr))
PILE <- c("pile_share_flags", "pile_share_errors")
files <- c(Sys.glob("state_delivery_lists/blended_delivery_*_2022_2024_budget*.csv"),
           Sys.glob("state_delivery_lists/national_delivery_*_2022_2024_budget*.csv"))
stopifnot(length(files) == 196L)

tags <- read.csv("methods/reconstruction_income_piles/rule_pile_share.csv")
tags$tag <- round(tags$pile_n / tags$n, 4) >= 0.25 |
            round(ifelse(tags$k > 0, tags$pile_k / tags$k, 0), 4) >= 0.25
tag_key <- paste(tags$unit, tags$hh, tags$rule)

out <- list()
for (fn in files) {
  x <- read.csv(fn, check.names = FALSE)
  stopifnot(all(PILE %in% names(x)))
  y <- x[, c(setdiff(names(x), PILE), PILE)]
  stopifnot(identical(x[, names(y)], y), nrow(y) == nrow(x))
  write.csv(y, fn, row.names = FALSE)

  old <- read.csv(pipe(sprintf('git show "HEAD:%s"', fn)), check.names = FALSE)
  st <- gsub("_", " ", sub("^(blended|national)_delivery_(.*)_2022_2024_budget\\d+\\.csv$", "\\2", basename(fn)))
  unit <- ifelse(old$pool == "state", st, "national")
  k <- match(paste(unit, as.character(old$hh), old$rule), tag_key)
  stopifnot(!anyNA(k))
  n_old_tagged <- sum(tags$tag[k])
  same <- nrow(old) == nrow(y) && all(old$rule == y$rule) && all(as.character(old$hh) == as.character(y$hh)) &&
          all(old$role == y$role)
  if (n_old_tagged == 0 && !same) stop("list with no tagged rule changed: ", fn)
  out[[length(out) + 1]] <- data.frame(
    file = basename(fn), rules_old = nrow(old), rules_new = nrow(y),
    core_old = sum(old$role == "core"), core_new = sum(y$role == "core"),
    tagged_rules_old = n_old_tagged, tagged_core_old = sum(tags$tag[k] & old$role == "core"),
    identical = same,
    kept_rules = sum(paste(y$hh, y$rule) %in% paste(old$hh, old$rule)))
}
res <- bind_rows(out)
write.csv(res, "methods/reconstruction_income_piles/list_changes.csv", row.names = FALSE)
cat(sprintf("columns moved to the end in %d lists | lists with no tagged rule in v2.7.0: %d, all identical | lists changed: %d (all had tagged rules) | v2.7.0 core rules tagged: %d of %d\n",
            nrow(res), sum(res$tagged_rules_old == 0), sum(!res$identical),
            sum(res$tagged_core_old), sum(res$core_old)))
cat("POSTCHECK DONE\n")
