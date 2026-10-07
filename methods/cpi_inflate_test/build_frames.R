# Frame builder for the CPI-inflation test (staging/cpi-inflate, 2026-09-20).
# Runs the munging script AS COMMITTED on this branch, expression by
# expression, overriding only its two CPI flags, so one code path still
# writes every frame. Run from the worktree root (here() must resolve to the
# worktree so the canonical frame in the main checkout is never touched).
#
#   Rscript methods/cpi_inflate_test/build_frames.R full  nominal
#   Rscript methods/cpi_inflate_test/build_frames.R tail  nominal   # identity check of the tail path
#   Rscript methods/cpi_inflate_test/build_frames.R tail  2019
#   Rscript methods/cpi_inflate_test/build_frames.R tail  2024
#
# mode "full": every expression (raw .sav -> frame), ~35 min.
# mode "tail": libraries, flags, `folder`, function definitions and
#   source("features.R") from the head, then everything from the final.rds
#   read onward (the CPI step sits after that checkpoint), ~1 min. Needs a
#   prior "full" run in this worktree.
# arm "nominal": cpi_inflate_vars <- FALSE. arm "<year>": TRUE with
#   modeling_target_year <- <year>.

args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2, args[1] %in% c("full", "tail"))
MODE <- args[1]; ARM <- args[2]
ARM_CPI    <- !identical(ARM, "nominal")
ARM_TARGET <- if (ARM_CPI) as.integer(ARM) else NA_integer_
stopifnot(!ARM_CPI || !is.na(ARM_TARGET))

SCRIPT  <- "1_data_munging_and_raw_variable_reconstruction_for_using_public_qc_data.R"
OUT_DIR <- "methods/cpi_inflate_test/frames"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
stopifnot(file.exists(SCRIPT),
          normalizePath(here::here()) == normalizePath(getwd()),
          !grepl("snap_qc$", normalizePath(getwd(), winslash = "/")))

exprs <- parse(SCRIPT, keep.source = FALSE)
is_assign_to <- function(e, nm)
  is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
  identical(as.character(e[[2]]), nm)
is_final_read <- function(e)
  is_assign_to(e, "mydata") && grepl("final.rds", paste(deparse(e), collapse = ""),
                                     fixed = TRUE) &&
  grepl("readRDS", paste(deparse(e), collapse = ""), fixed = TRUE)
is_head_keep <- function(e) {
  if (!is.call(e)) return(FALSE)
  f <- as.character(e[[1]])[1]
  if (f %in% c("library", "options")) return(TRUE)
  if (f == "source") return(TRUE)
  if (identical(e[[1]], as.name("<-")) && is.name(e[[2]])) {
    rhs <- e[[3]]
    if (is.call(rhs) && identical(rhs[[1]], as.name("function"))) return(TRUE)
    if (is.logical(rhs) || is.numeric(rhs)) return(TRUE)           # flags
    if (identical(as.character(e[[2]]), "folder")) return(TRUE)
  }
  FALSE
}
i_final <- which(vapply(exprs, is_final_read, logical(1)))
stopifnot(length(i_final) == 1)

n_override <- 0L
for (i in seq_along(exprs)) {
  e <- exprs[[i]]
  if (is_assign_to(e, "cpi_inflate_vars")) {
    cpi_inflate_vars <- ARM_CPI; n_override <- n_override + 1L; next
  }
  if (is_assign_to(e, "modeling_target_year")) {
    modeling_target_year <- if (ARM_CPI) ARM_TARGET else 2026
    n_override <- n_override + 1L; next
  }
  if (MODE == "tail" && i < i_final && !is_head_keep(e)) next
  eval(e, globalenv())
}
stopifnot(n_override == 2L, exists("reg_model_data"))
cat(sprintf("\narm %s (%s): cpi_inflate_vars = %s, modeling_target_year = %s\n",
            ARM, MODE, cpi_inflate_vars, modeling_target_year))

out <- file.path(OUT_DIR, sprintf("reg_model_data_%s%s.rds", ARM,
                                  if (MODE == "tail" && ARM == "nominal") "_tailcheck" else ""))
file.rename("reg_model_data.rds", out)
cat("frame ->", out, ":", nrow(reg_model_data), "rows\n")
