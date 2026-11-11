# Munging review 2026-10-30: how many cases each new reconstruction path touches.
# Read-only on the frames; writes only to methods/munging_review_2026-10-30/scratch/.
#   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" methods/munging_review_2026-10-30/describe_paths.R
suppressMessages(library(dplyr))
setwd("C:/Users/ericg/snap_qc_v27")
out <- "methods/munging_review_2026-10-30/scratch/"
b <- readRDS("reg_model_data.rds")                                   # new (Ben's code)
a <- readRDS("archive_data/reg_model_data_f1027_eb989fda.rds")       # old (eb989fda)
fin <- readRDS("final.rds")                                          # new, pre-rename checkpoint
cor <- readRDS("corrected.rds")                                      # new, after Step 7/8, before smoothing / 97-26
cat("rows: old", nrow(a), " new", nrow(b), " final.rds", nrow(fin), " corrected.rds", nrow(cor), "\n")

err <- function(d) !is.na(d$over_threshold) & d$over_threshold != 0
b$err <- err(b); a$err <- err(a)

cat("\n== 1. Bound smoothing (imputed_at_bound) ==\n")
cl <- function(notes) { notes[is.na(notes)] <- ""
  m <- regmatches(notes, gregexpr("(^|\\+)(earn|unearn|rent|med|dep|cs)_(up|down)(?=\\+|$)", notes, perl = TRUE))
  vapply(m, function(x) if (length(x)) sub("^\\+", "", x[length(x)]) else NA_character_, character(1)) }
b$closing <- cl(b$correctednotes)
print(with(b, table(imputed_at_bound, closing, useNA = "no")))
print(with(b, table(imputed_at_bound, error = err, useNA = "ifany")))
cat("share of rows imputed, by FY:\n"); print(round(prop.table(table(b$fiscal_year, !is.na(b$imputed_at_bound)), 1) * 100, 2))
st <- b %>% group_by(state_name) %>% summarise(rows = n(), imputed = sum(!is.na(imputed_at_bound)),
        pct = round(100 * imputed / rows, 2), max = sum(imputed_at_bound %in% "max"),
        min = sum(imputed_at_bound %in% "min"), cap = sum(imputed_at_bound %in% "shelter_cap"), .groups = "drop") %>% arrange(desc(pct))
cat("by state (top 10 / bottom 5 by % imputed):\n"); print(head(st, 10)); print(tail(st, 5))
cat("state % imputed quantiles:\n"); print(quantile(st$pct, c(0, .1, .5, .9, 1)))
write.csv(st, paste0(out, "imputed_by_state.csv"), row.names = FALSE)
# what fraction of all "matched at max with income_down closing" and of all errors are imputed
cat("errors with an imputed value:", sum(b$err & !is.na(b$imputed_at_bound)), "of", sum(b$err), "\n")

# How far the draw moved the value (nominal): compare new frame to corrected.rds (pre-smoothing) by key
kf <- paste(fin$STATE, fin$fiscal_year, fin$HHLDNO); kc <- paste(cor$STATE, cor$fiscal_year, cor$HHLDNO)
mc <- match(kf, kc)
vars <- c(earn = "rawearn", unearn = "rawunearn", rent = "rawrent", med = "rawmedded", dep = "rawdepded", cs = "rawcsded")
fin$closing <- cl(fin$correctednotes)
cat("\nimputed moves vs the Step 7 stop point (nominal $):\n")
for (s in c("max", "min", "shelter_cap")) for (v in names(vars)) {
  r <- which(fin$imputed_at_bound %in% s & grepl(paste0("^", v, "_"), fin$closing))
  if (!length(r)) next
  d <- fin[[vars[[v]]]][r] - cor[[vars[[v]]]][mc[r]]
  qcv <- c(rawearn = "FSEARN", rawunearn = "FSUNEARN", rawrent = "RENT", rawmedded = "FSMEDDED", rawdepded = "FSDEPDED", rawcsded = "FSCSDED")[[vars[[v]]]]
  q <- as.numeric(fin[[qcv]][r]); stop <- cor[[vars[[v]]]][mc[r]]
  cat(sprintf("  %-11s %-6s n=%5d  move: median %7.0f p10 %7.0f p90 %7.0f | stop/QC median %.2f | new==0 %4.1f%% | stop==0 %4.1f%%\n",
              s, v, length(r), median(d, na.rm = TRUE), quantile(d, .1, na.rm = TRUE), quantile(d, .9, na.rm = TRUE),
              median(stop / q, na.rm = TRUE), 100 * mean(fin[[vars[[v]]]][r] == 0, na.rm = TRUE), 100 * mean(stop == 0, na.rm = TRUE)))
}

cat("\n== 2. AMOUNTi reporting (multi-element cases, new frame pre-NA via final.rds amount_pattern) ==\n")
mp <- fin[!is.na(fin$amount_pattern), ]
cat("multi-element cases:", nrow(mp), "\n")
print(round(prop.table(table(mp$fiscal_year, mp$amount_pattern), 1) * 100, 1))
print(table(mp$fiscal_year, mp$amount_pattern))
ps <- mp %>% group_by(state_name) %>% summarise(n = n(), all_zero = mean(amount_pattern == "all_zero"),
       amount1_only = mean(amount_pattern == "amount1_only"), copied = mean(amount_pattern == "copied"),
       even = mean(amount_pattern == "even_split"), clean = mean(amount_pattern == "clean_split"),
       other = mean(amount_pattern == "other"), .groups = "drop") %>% mutate(across(all_zero:other, ~ round(100 * .x)))
print(as.data.frame(ps %>% arrange(desc(n))), row.names = FALSE)
write.csv(ps, paste0(out, "amount_pattern_by_state.csv"), row.names = FALSE)
# within-state consistency across years: dominant pattern by state-year and share of the dominant one
sy <- mp %>% count(state_name, fiscal_year, amount_pattern) %>% group_by(state_name, fiscal_year) %>%
  mutate(share = n / sum(n), tot = sum(n)) %>% slice_max(share, n = 1, with_ties = FALSE) %>% ungroup()
write.csv(sy, paste0(out, "amount_pattern_dominant_state_year.csv"), row.names = FALSE)
chg <- sy %>% filter(tot >= 10) %>% group_by(state_name) %>% summarise(years = n(), patterns = n_distinct(amount_pattern),
        seq = paste(substr(amount_pattern, 1, 5), collapse = ","), .groups = "drop")
cat("states whose dominant pattern (state-years with >=10 multi-element cases) changes across years:",
    sum(chg$patterns > 1), "of", nrow(chg), "\n")
print(as.data.frame(chg %>% filter(patterns > 1)), row.names = FALSE)
# slot-level: share of present slots 2-9 with AMOUNT == 0, and AMOUNT1 == 0
el <- as.matrix(fin[paste0("ELEMENT", 1:9)]); am <- as.matrix(fin[paste0("AMOUNT", 1:9)])
pres <- !is.na(el); am[is.na(am)] <- 0
multi <- rowSums(pres) >= 2
cat(sprintf("multi-element: AMOUNT1 == 0 in %.1f%%; slots 2-9 present with AMOUNT 0: %.1f%%\n",
            100 * mean(am[multi, 1] == 0), 100 * sum(pres[multi, 2:9] & am[multi, 2:9] == 0) / sum(pres[multi, 2:9])))
single <- rowSums(pres) == 1 & err(fin)
cat(sprintf("single-element errors: AMOUNT1 == 0 in %.1f%%; |AMOUNT1 - AMTERR| <= 2 in %.1f%%\n",
            100 * mean(am[single, 1] == 0), 100 * mean(abs(am[single, 1] - fin$AMTERR[single]) <= 2)))
cat("multi-element cases by number of step-7 vs non-step-7 elements is in recreate path; pattern x cascade_type:\n")
print(table(fin$amount_pattern, fin$cascade_type, useNA = "ifany"))

cat("\n== 3. Reaching RAWBEN (old vs new frame) ==\n")
reach <- function(d, tol) (abs(d$rawben - d$raw_benefit_amount) <= tol) %in% TRUE
for (nm in c("a", "b")) { d <- get(nm)
  nel <- rowSums(!is.na(as.matrix(d[paste0("element", 1:9)])))
  grp <- ifelse(!d$err, "clean", ifelse(nel <= 1, "error_1el", "error_multi"))
  cat(nm, ": exact", round(100 * mean(reach(d, 0)), 2), "% | <=$5", round(100 * mean(reach(d, 5)), 2), "%\n")
  print(round(100 * tapply(reach(d, 0), list(grp, d$fiscal_year), mean), 2))
  cat("  <=$5 by group:\n"); print(round(100 * tapply(reach(d, 5), grp, mean), 2))
  cat("  _error notes:", sum(grepl("_error", d$correctednotes)), " of which errors:", sum(grepl("_error", d$correctednotes) & d$err), "\n")
}
# note: element slots are NA'd for no_action_required rows in the new frame, so 'error_multi' uses current elements

cat("\n== 4. Nature 97 / agency 26 ==\n")
print(table(FY = b$fiscal_year, required = b$no_action_required_i))
print(table(FY = b$fiscal_year, partial = b$no_action_partial_i))
ka <- paste(a$state, a$fiscal_year, a$hhldno); kb <- paste(b$state, b$fiscal_year, b$hhldno)
m <- match(ka, kb); ok <- !is.na(m)
ea <- a$err[ok]; eb <- b$err[m[ok]]
src <- ifelse(b$no_action_required_i[m[ok]], "required", ifelse(b$no_action_partial_i[m[ok]], "partial", "neither"))
cat("label transitions on matched rows by 97/26 status:\n")
print(table(src, transition = ifelse(ea & eb, "err->err", ifelse(ea & !eb, "err->clean", ifelse(!ea & eb, "clean->err", "clean->clean")))))
cat("no_action_required rows: old-frame label error:", sum(ea[src == "required"]), "; old status table:\n")
print(table(old_status = a$status[ok][src == "required"], old_amterr0 = (a$amterr[ok][src == "required"] == 0)))
# error dollars removed by the partial swap
pp <- ok & FALSE; pp[ok] <- src == "partial"
cat(sprintf("partial rows: AMTERR old sum %.0f -> new %.0f; |bendiff| old %.0f -> new %.0f\n",
            sum(a$amterr[pp], na.rm = TRUE), sum(b$amterr[m[pp]], na.rm = TRUE),
            sum(abs(a$bendiff[pp]), na.rm = TRUE), sum(abs(b$bendiff[m[pp]]), na.rm = TRUE)))
cat("partial rows, over_threshold old x new:\n"); print(table(old = ea[src == "partial"], new = eb[src == "partial"]))
# QC's own status for required rows: does QC already call them correct?
cat("no_action_required in final.rds: raw STATUS before clearing unavailable; corrected.rds STATUS:\n")
rq <- fin$no_action_required_i; mq <- match(kf[rq], kc)
print(table(STATUS = cor$STATUS[mq], over_thr = cor$over_threshold[mq], useNA = "ifany"))
cat("their pre-clear AMTERR > 0:", sum(cor$AMTERR[mq] > 0, na.rm = TRUE), "; |RAWBEN-FSBEN| > 0:", sum(abs(cor$RAWBEN[mq] - cor$FSBEN[mq]) > 0, na.rm = TRUE), "\n")
pq <- fin$no_action_partial_i; mpq <- match(kf[pq], kc)
cat("partial in corrected.rds: STATUS x over_threshold\n"); print(table(STATUS = cor$STATUS[mpq], over = cor$over_threshold[mpq], useNA = "ifany"))
cat("partial, new over_threshold:\n"); print(table(fin$over_threshold[pq]))
cat("partial: FSBEN changed:", sum(fin$FSBEN[pq] != cor$FSBEN[mpq], na.rm = TRUE), "; fell from over to under:",
    sum(cor$over_threshold[mpq] == 1 & fin$over_threshold[pq] == 0, na.rm = TRUE), "; rose under->over:",
    sum(cor$over_threshold[mpq] == 0 & fin$over_threshold[pq] == 1, na.rm = TRUE), "\n")
cat("required rows by state (top 10):\n"); print(head(sort(table(fin$state_name[rq]), decreasing = TRUE), 10))
