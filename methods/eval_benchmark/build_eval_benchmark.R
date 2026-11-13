# Builds the evaluation benchmark: one row per case of a frame, with the error label, the error dollars, and the
# flag that says whether held-out scoring counts the case (2026-11-12; Eric: "create a benchmarking dataset as a .rds
# file so that it's easy for us and others to be consistent about what we're using for evaluation").
#
# Scoring counts only "kept" cases: cases whose rebuilt inputs we trust. A case is dropped when any of these hold:
#   drop_gap               the benefit recreated from the rebuilt inputs misses the issued benefit (RAWBEN) by more
#                          than $5 (GAP_CUT)
#   drop_max_mismatch      the reconstruction mismatch: issued at or above the maximum allotment while the recreated
#                          benefit before the cap is below it
#   drop_income_pile       a reconstructed income pile (income_pile_rows() in rule_mining_helpers.R)
#   drop_prior_year_tables the case was rebuilt from prior-year tables (correctednotes)
# This is keep_of(filter = "gap") in methods/case_predictions/s46/s46_lib.R, the rule behind findings §46-§52 and the
# results map. check_eval_benchmark.R confirms that the file reproduces the published tables.
#
# Run from the repository root:
#   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" methods/eval_benchmark/build_eval_benchmark.R [TAG] [FRAME]
# TAG defaults to f1109 and FRAME to additional_data/reg_model_data.rds. Writes additional_data/eval_benchmark_<TAG>.rds.
# A rebuilt frame needs a new benchmark under a new tag; results stay tied to the file they were scored on.
args <- commandArgs(TRUE)
TAG <- if (length(args) >= 1) args[1] else "f1109"
FRAME <- if (length(args) >= 2) args[2] else "additional_data/reg_model_data.rds"
GAP_CUT <- 5
OUTF <- file.path("additional_data", sprintf("eval_benchmark_%s.rds", TAG))
stopifnot(file.exists("rule_mining_helpers.R"), file.exists(FRAME))
source("rule_mining_helpers.R")   # income_pile_rows()
sha <- digest::digest(file = FRAME, algo = "sha256")
if (TAG == "f1109") stopifnot(startsWith(sha, "08e0e825"))

a <- readRDS(FRAME)
yr <- as.character(a$fiscal_year); st <- as.character(a$state)
stopifnot(!anyNA(yr), !anyNA(st), !anyNA(a$hhldno), !anyDuplicated(paste(yr, st, a$hhldno)))

error <- !is.na(a$over_threshold) & a$over_threshold != 0
error_dollars <- ifelse(error, abs(ifelse(is.na(a$total_error_amount), 0, a$total_error_amount)), 0)
over <- error & a$bendiff > 0; under <- error & a$bendiff < 0
stopifnot(!anyNA(over), !anyNA(under), all(over | under | !error))

gap <- abs(a$rawben - a$raw_benefit_amount); gap[is.na(gap)] <- 0
mm <- a$rawben >= a$benmax & a$rawben_uncapped < a$benmax; mm[is.na(mm)] <- FALSE
pile <- income_pile_rows(a); pile[is.na(pile)] <- FALSE        # piles are defined within fiscal year and household size
notes <- as.character(a$correctednotes); notes[is.na(notes) | notes == ""] <- "no_change"
step7 <- grepl("prior_year_tables", notes)

bench <- data.frame(
  frame_row = seq_len(nrow(a)), fiscal_year = yr, state = st, hhldno = a$hhldno,
  error = error, error_dollars = error_dollars,
  error_direction = ifelse(over, "over", ifelse(under, "under", "none")),
  issued_benefit = a$rawben, recreated_benefit = a$raw_benefit_amount, gap = gap,
  drop_gap = gap > GAP_CUT, drop_max_mismatch = mm, drop_income_pile = pile, drop_prior_year_tables = step7,
  stringsAsFactors = FALSE)
bench$kept <- !(bench$drop_gap | bench$drop_max_mismatch | bench$drop_income_pile | bench$drop_prior_year_tables)

attr(bench, "frame_tag") <- TAG
attr(bench, "frame_file") <- FRAME
attr(bench, "frame_sha256") <- sha
attr(bench, "frame_rows") <- nrow(a)
attr(bench, "built") <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
attr(bench, "gap_cut") <- GAP_CUT
attr(bench, "keep_rule") <- paste(
  "kept = not drop_gap (|RAWBEN - recreated benefit| > $5), not drop_max_mismatch (RAWBEN >= max allotment and",
  "recreated uncapped benefit < max allotment), not drop_income_pile (income_pile_rows()), not",
  "drop_prior_year_tables (correctednotes contains 'prior_year_tables'); s46 keep_of(filter = 'gap')")
attr(bench, "flag_rule") <- paste(
  "At budget b, a state flags max(1, round(b x all of its cases in the score year)) cases (R's round, halves to",
  "even), filled with its highest-scored kept cases, ties broken by frame_row. Errors and error dollars are summed",
  "over flagged cases; the denominators (errors_all, dollars_all) are over the state's kept cases. See",
  "methods/eval_benchmark/score_ranking.R.")
saveRDS(bench, OUTF)

cat(sprintf("[%s] wrote %s: %d cases (frame %s, sha256 %s)\n", format(Sys.time(), "%H:%M:%S"), OUTF, nrow(bench), TAG,
            substr(sha, 1, 8)))
for (y in sort(unique(yr))) { b <- bench[bench$fiscal_year == y, ]
  cat(sprintf("  FY%s: %6d cases, %5d errors | kept %6d (%.1f%%), %5d errors | dropped: gap %d, max mismatch %d, pile %d, prior-year tables %d\n",
              y, nrow(b), sum(b$error), sum(b$kept), 100 * mean(b$kept), sum(b$error & b$kept), sum(b$drop_gap),
              sum(b$drop_max_mismatch), sum(b$drop_income_pile), sum(b$drop_prior_year_tables))) }
