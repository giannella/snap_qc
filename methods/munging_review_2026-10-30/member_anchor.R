# Munging review 2026-10-30 (EXPLORATORY): a semi-independent anchor for the max-benefit earned-income draws.
# For real earn_down cases stuck at the maximum allotment (imputed_at_bound == "max"), the person-level earned
# income fields (reviewer side, sum to FSEARN) give candidate agency values of the form FSEARN minus one or more
# members' earnings ("the agency left a job out"). Where exactly one such candidate is consistent with the
# maximum benefit, compare the Step 7 stop point and the smoothing draw with it. Writes nothing.
setwd("C:/Users/ericg/snap_qc_v27")
load("methods/munging_review_2026-10-30/scratch/pre7_env.RData")
fin <- readRDS("final.rds"); cor <- readRDS("corrected.rds")
kf <- paste(fin$STATE, fin$fiscal_year, fin$HHLDNO); kc <- paste(cor$STATE, cor$fiscal_year, cor$HHLDNO); mc <- match(kf, kc)
cl <- function(notes) { notes[is.na(notes)] <- ""
  m <- regmatches(notes, gregexpr("(^|\\+)(earn|unearn|rent|med|dep|cs)_(up|down)(?=\\+|$)", notes, perl = TRUE))
  vapply(m, function(x) if (length(x)) sub("^\\+", "", x[length(x)]) else NA_character_, character(1)) }
r <- which(fin$imputed_at_bound %in% "max" & cl(fin$correctednotes) %in% "earn_down" & fin$correctednotes == "earn_down")
sub <- as.data.frame(fin[r, ])
E <- sapply(1:18, function(k) person_income(sub, earned_types, k))
fs <- as.numeric(sub$FSEARN)
stop_v <- as.numeric(cor$rawearn[mc[r]]); imp <- as.numeric(sub$rawearn)
nearn <- rowSums(E > 0)
cat("pure earn_down at max, imputed:", length(r), "; earners per case:", paste(names(table(pmin(nearn, 3))), table(pmin(nearn, 3)), sep = ":", collapse = " "), "\n")
cat("person earnings sum to FSEARN (within $1):", round(100 * mean(abs(rowSums(E) - fs) <= 1), 1), "%\n")
bi_all <- lapply(sub[benefit_inputs], as.numeric)
at_max <- function(i, val) { b <- lapply(bi_all, `[`, i); b$rawearn <- val; calculate_raw_benefits(b)$rawben_uncapped >= b$rawbenmax }
cand <- rep(NA_real_, length(r)); ncand <- integer(length(r))
for (i in seq_along(r)) {
  e <- E[i, E[i, ] > 0]
  if (!length(e)) next
  # one member's earnings left out, or all of them
  cs <- unique(c(fs[i] - e, 0)); cs <- cs[cs >= 0]
  ok <- cs[vapply(cs, function(v) at_max(i, v), TRUE)]
  ncand[i] <- length(ok)
  if (length(ok) == 1) cand[i] <- ok
}
u <- !is.na(cand)
cat("cases with exactly one consistent member-omission candidate:", sum(u), "( zero:", sum(cand[u] == 0), ", nonzero:", sum(cand[u] > 0), ")\n")
cat("multi-earner cases with a unique nonzero candidate:", sum(u & nearn >= 2 & cand > 0), "\n")
f <- function(sel, lab) {
  cat(sprintf("%-28s n=%4d | MAE stop %6.0f  imputed %6.0f | exact-hit stop %4.1f%% imputed %4.1f%% | imputed closer in %4.1f%% (ties %4.1f%%)\n", lab, sum(sel),
              mean(abs(stop_v[sel] - cand[sel])), mean(abs(imp[sel] - cand[sel])),
              100 * mean(stop_v[sel] == cand[sel]), 100 * mean(imp[sel] == cand[sel]),
              100 * mean(abs(imp[sel] - cand[sel]) < abs(stop_v[sel] - cand[sel])),
              100 * mean(abs(imp[sel] - cand[sel]) == abs(stop_v[sel] - cand[sel]))))
}
f(u, "unique candidate, all")
f(u & cand == 0, "unique candidate = $0")
f(u & cand > 0, "unique candidate > $0")
f(u & nearn == 1, "single earner")
f(u & nearn >= 2, "two+ earners")
