# Frame identity gate for the CPI-inflation test: what did cpi_inflate()
# change, column by column? Run from the worktree root after build_frames.R.
#   Rscript methods/cpi_inflate_test/frame_diff.R
# Writes frame_diff_<target>.csv (one row per frame column) and
# frame_diff_vocab_<target>.csv (the mined features, by fiscal year), and
# stops if rows, row order, error flags or error dollars differ between the
# nominal and a CPI frame. Also checks the tail-only build path against the
# full build, and reports (no assert) how the staging nominal frame differs
# from the canonical frame in the main checkout (built 2026-08-23, before the
# 2026-08-30 SMD changes to the munging script).

suppressMessages(library(dplyr))
FR  <- "methods/cpi_inflate_test/frames"
OUT <- "methods/cpi_inflate_test/out"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
VOCAB19 <- c("HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size",
             "expedited_i", "bbce_state_i", "rawben_rel_max", "medical_deductions",
             "shelter_expenses_by_hh_size", "utilities_sua", "married", "homeless",
             "percent_abawd", "unc_rawben_rel_max", "months_since_cert_n",
             "count_divisible_by_100", "gross_by_hh_size", "earned_by_hh_size",
             "unearned_by_hh_size")

same <- function(a, b) isTRUE(all.equal(a, b, check.attributes = FALSE, tolerance = 0))
col_diff <- function(A, B) {
  stopifnot(nrow(A) == nrow(B))
  bind_rows(lapply(union(names(A), names(B)), function(v) {
    if (!(v %in% names(A) && v %in% names(B)))
      return(data.frame(column = v, status = "missing in one frame", n_changed = NA,
                        share_changed = NA, median_ratio = NA))
    a <- A[[v]]; b <- B[[v]]
    if (same(a, b)) return(data.frame(column = v, status = "identical", n_changed = 0,
                                      share_changed = 0, median_ratio = NA))
    if (is.factor(a)) a <- as.character(a); if (is.factor(b)) b <- as.character(b)
    ch <- xor(is.na(a), is.na(b)) | (!is.na(a) & !is.na(b) & a != b)
    ratio <- if (is.numeric(a) && is.numeric(b))
      suppressWarnings(median((b / a)[ch & !is.na(a) & a != 0], na.rm = TRUE)) else NA
    data.frame(column = v, status = "changed", n_changed = sum(ch),
               share_changed = round(mean(ch), 4), median_ratio = round(ratio, 4))
  }))
}

nominal <- readRDS(file.path(FR, "reg_model_data_nominal.rds"))

tc <- file.path(FR, "reg_model_data_nominal_tailcheck.rds")
if (file.exists(tc)) {
  d <- col_diff(nominal, readRDS(tc))
  cat(sprintf("tail-path check: %d of %d columns identical to the full build\n",
              sum(d$status == "identical"), nrow(d)))
  stopifnot(all(d$status == "identical"))
}

for (target in c(2019, 2024, 2022)) {
  fn <- file.path(FR, sprintf("reg_model_data_%d.rds", target))
  if (!file.exists(fn)) next
  cpi <- readRDS(fn)
  stopifnot(nrow(cpi) == nrow(nominal),
            same(nominal$state, cpi$state), same(nominal$fiscal_year, cpi$fiscal_year),
            same(nominal$over_threshold, cpi$over_threshold),
            same(nominal$total_error_amount, cpi$total_error_amount),
            same(nominal$cert_HH_size_FS_n, cpi$cert_HH_size_FS_n))
  d <- col_diff(nominal, cpi)
  write.csv(d, file.path(OUT, sprintf("frame_diff_%d.csv", target)), row.names = FALSE)
  cat(sprintf("\n== target %d: %d of %d columns changed ==\n", target,
              sum(d$status == "changed"), nrow(d)))
  print(d[d$status != "identical", ], row.names = FALSE)
  cat("\nmined features (VOCAB19) changed:",
      paste(intersect(VOCAB19, d$column[d$status == "changed"]), collapse = ", "), "\n")
  # by fiscal year, for each changed mined feature: share of rows changed and
  # the median ratio CPI / nominal among changed non-zero rows
  chv <- intersect(VOCAB19, d$column[d$status == "changed"])
  by_year <- bind_rows(lapply(chv, function(v) {
    a <- nominal[[v]]; b <- cpi[[v]]
    ch <- xor(is.na(a), is.na(b)) | (!is.na(a) & !is.na(b) & a != b)
    data.frame(fy = nominal$fiscal_year, ch = ch, pos = !is.na(a) & a != 0,
               ratio = ifelse(ch & !is.na(a) & a != 0, b / a, NA_real_)) %>%
      group_by(fy) %>%
      summarise(feature = v, rows = n(), share_nonzero = round(mean(pos), 4),
                share_changed = round(mean(ch), 4),
                share_changed_of_nonzero = round(sum(ch & pos) / max(sum(pos), 1), 4),
                median_ratio = round(median(ratio, na.rm = TRUE), 4), .groups = "drop")
  }))
  write.csv(by_year, file.path(OUT, sprintf("frame_diff_vocab_%d.csv", target)),
            row.names = FALSE)
  print(as.data.frame(by_year), row.names = FALSE)
  # rows whose calendar year IS the target take ratio 1: any change there is
  # floor() acting on a non-integer reconstructed value, not inflation
  if ("yrmonth" %in% names(nominal)) {
    at_target <- as.integer(substr(as.character(nominal$yrmonth), 1, 4)) == target
    chg <- d$column[d$status == "changed"]
    ft <- bind_rows(lapply(chg, function(v) {
      a <- nominal[[v]][at_target]; b <- cpi[[v]][at_target]
      if (is.factor(a)) a <- as.character(a); if (is.factor(b)) b <- as.character(b)
      ch <- xor(is.na(a), is.na(b)) | (!is.na(a) & !is.na(b) & a != b)
      data.frame(column = v, rows_at_target_year = sum(at_target), n_changed = sum(ch),
                 max_abs_change = if (is.numeric(a) && any(ch)) max(abs(b - a)[ch]) else 0)
    }))
    write.csv(ft, file.path(OUT, sprintf("frame_diff_ratio1_rows_%d.csv", target)), row.names = FALSE)
    cat("\nchanges among rows at ratio 1 (floor() only):\n"); print(ft, row.names = FALSE)
  }
  expect4 <- c("earned_by_hh_size", "medical_deductions", "shelter_expenses_by_hh_size",
               "unearned_by_hh_size")
  if (!setequal(chv, expect4))
    cat("\nNOTE: changed mined features differ from the four expected from reading the code:",
        paste(chv, collapse = ", "), "\n")
  rm(cpi); invisible(gc())
}

# the 2026-08-23 frame (the root frame was rebuilt 2026-09-21 and now equals the staging nominal frame)
canon <- "C:/Users/ericg/snap_qc/archive_data/reg_model_data_pre_smd_rebuild_2026-09-21.rds"
if (file.exists(canon)) {
  cf <- readRDS(canon)
  cat(sprintf("\n== staging nominal vs canonical frame (%d vs %d rows) ==\n",
              nrow(nominal), nrow(cf)))
  if (nrow(cf) == nrow(nominal)) {
    d <- col_diff(cf, nominal)
    write.csv(d, file.path(OUT, "frame_diff_nominal_vs_canonical.csv"), row.names = FALSE)
    print(d[d$status != "identical", ], row.names = FALSE)
  }
}
