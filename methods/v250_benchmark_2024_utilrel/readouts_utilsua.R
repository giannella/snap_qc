# Post-run readouts for the utilities-SUA tier variant mine (design_note.md;
# run this AFTER the overnight benchmark finishes). Scores the three
# pre-registered readouts against their bars and prints a verdict block.
#   1. list non-inferiority vs the committed baseline benchmark
#   2. mechanism (residual-fragility) bar with exact binomial CI
#   3. family representation + per-state tier-1 mass (monitor)
suppressMessages(library(dplyr))

# ERA=2 (env var) switches every input to the era-2 replication arms
# (era2_design_note.md, constraint c): variant + baseline both from the
# fresh FY2017-18 mines, slices on FY2019, and the pre-set harmed-tail
# null bound reported beside the era-1 package-noise figure.
ERA <- Sys.getenv("ERA", "1")
if (ERA == "2") {
  VAR_DIR  <- "methods/v250_benchmark_2024_utilrel/era2_variant"
  VAR_CSV  <- file.path(VAR_DIR, "v250_benchmark_2024.csv")
  BASE_CSV <- "methods/v250_benchmark_2024_utilrel/era2_baseline/v250_benchmark_2024.csv"
  TEST_YR  <- "2019"
  HARM_BOUND <- c(`0.05` = 15L, `0.1` = 12L)   # independent-sampling null, FY2019 caseloads
} else {
  VAR_DIR  <- "methods/v250_benchmark_2024_utilrel"
  VAR_CSV  <- file.path(VAR_DIR, "v250_benchmark_2024.csv")
  BASE_CSV <- "methods/v250_benchmark_2024/v250_benchmark_2024.csv"
  TEST_YR  <- "2024"
  HARM_BOUND <- NULL
}
POOL_RDS <- file.path(VAR_DIR, "cache", "bench_national_117.rds")
HARM_THR <- -0.05

stopifnot(file.exists(VAR_CSV), file.exists(BASE_CSV), file.exists(POOL_RDS))

## ---- readout 1: paired non-inferiority --------------------------------------
v <- read.csv(VAR_CSV) %>% select(state, budget, precision, dollar_recall)
b <- read.csv(BASE_CSV) %>% select(state, budget, precision_b = precision,
                                   dollar_b = dollar_recall)
p <- inner_join(v, b, by = c("state", "budget")) %>%
  mutate(d_prec = precision - precision_b,
         d_doll = dollar_recall - dollar_b)
write.csv(p, file.path(VAR_DIR, "paired_vs_baseline.csv"), row.names = FALSE)
cat("== readout 1: variant vs baseline benchmark (paired per state) ==\n")
pass1 <- TRUE
for (bud in sort(unique(p$budget))) {
  s <- p %>% filter(budget == bud)
  bound <- if (is.null(HARM_BOUND)) NA_integer_ else HARM_BOUND[[as.character(bud)]]
  for (m in c("d_prec", "d_doll")) {
    md <- median(s[[m]]); mn <- mean(s[[m]]); hurt <- sum(s[[m]] < HARM_THR)
    # era 1 bar as pre-registered (zero harmed; recorded as mis-specified
    # in result_2026-08-22.md); era 2 bar: harmed <= the pre-set null bound
    ok <- md >= -0.005 && mn >= -0.01 &&
      (if (is.na(bound)) hurt == 0 else hurt <= bound)
    pass1 <- pass1 && ok
    cat(sprintf("  %s @ %d%%: median %+.4f | mean %+.4f | harmed %d of %d%s -> %s\n",
                m, round(100 * bud), md, mn, hurt, nrow(s),
                ifelse(is.na(bound), "",
                       sprintf(" (null bound %d; era-1 package noise %s)",
                               bound, ifelse(bud < 0.075, "10", "4"))),
                ifelse(ok, "ok", "FAIL")))
  }
}
if (ERA == "2") {
  small <- c("South Dakota", "Wyoming")      # under the 400-row / 20-error threshold
  s47 <- p %>% filter(!state %in% small)
  cat("  secondary (47 states, SD/WY excluded):\n")
  for (bud in sort(unique(s47$budget))) for (m in c("d_prec", "d_doll")) {
    s <- s47 %>% filter(budget == bud)
    cat(sprintf("    %s @ %d%%: median %+.4f | mean %+.4f | harmed %d of %d\n",
                m, round(100 * bud), median(s[[m]]), mean(s[[m]]),
                sum(s[[m]] < HARM_THR), nrow(s)))
  }
}
cat(sprintf("readout 1 (%s): %s\n\n",
            ifelse(is.na(bound), "bar: median >= -0.005, mean >= -0.01, zero harmed",
                   "bar: median >= -0.005, mean >= -0.01, harmed <= null bound"),
            ifelse(pass1, "PASS", "FAIL")))

## ---- readouts 2 + 3: per-rule on the variant national pool ------------------
pool <- readRDS(POOL_RDS)
COND_PAT <- "([A-Za-z_][A-Za-z0-9_]*)\\s*(>=|<=|>|<)\\s*(-?[0-9.]+)"
vars_of <- function(txt)
  unique(sub("\\s*(>=|<=|>|<).*$", "",
             regmatches(txt, gregexpr(COND_PAT, txt, perl = TRUE))[[1]]))
fam <- vapply(pool$rule, function(r) "utilities_sua" %in% vars_of(r), logical(1))
cat(sprintf("== readout 3a: family representation ==\n"))
cat(sprintf("  utilities_sua rules in the admitted national pool: %d of %d (%.1f%%) [baseline utilities: 26.9%%]\n\n",
            sum(fam), nrow(pool), 100 * mean(fam)))

d <- readRDS("reg_model_data.rds")
h <- d %>% filter(as.character(fiscal_year) == TEST_YR)
mode_pos <- function(x) {
  x <- round(x[x > 0]); as.numeric(names(sort(table(x), decreasing = TRUE))[1])
}
h <- h %>% group_by(state_name) %>%
  mutate(utilities_sua = ifelse(utilities <= 0, 0L,
                                ifelse(utilities < mode_pos(utilities) - 200,
                                       1L, 2L))) %>%
  ungroup() %>% as.data.frame()
hh_num <- suppressWarnings(as.numeric(as.character(h$cert_HH_size_FS_n)))
h$hh_group <- ifelse(hh_num <= 1, "1", ifelse(hh_num <= 3, "2-3", "4+"))
is_err <- !is.na(h$over_threshold) & h$over_threshold != 0

cat(sprintf("== readout 3b: per-state tier-1 mass (FY%s, positive-utilities cases) ==\n", TEST_YR))
t1 <- h %>% filter(utilities > 0) %>%
  group_by(state = as.character(state)) %>%
  summarise(tier1_share = mean(utilities_sua == 1), .groups = "drop")
cat(sprintf("  tier-1 share quartiles: %s | states with tier1 < 1%%: %d of %d\n\n",
            paste(round(quantile(t1$tier1_share, c(.25, .5, .75)), 3),
                  collapse = " / "),
            sum(t1$tier1_share < 0.01), nrow(t1)))
write.csv(t1, file.path(VAR_DIR, sprintf("tier1_mass_fy%s.csv", TEST_YR)),
          row.names = FALSE)

parse_conds <- function(txt) {
  parts <- regmatches(txt, gregexpr(COND_PAT, txt, perl = TRUE))[[1]]
  do.call(rbind, lapply(parts, function(pp) {
    mm <- regmatches(pp, regexec(COND_PAT, pp))[[1]]
    data.frame(var = mm[2], op = mm[3], thr = as.numeric(mm[4]))
  }))
}
band <- pool$n >= 30 & pool$n <= 300
sub <- pool[band, ]
fam_b <- fam[band]
vars_used <- sort(unique(unlist(lapply(sub$rule, vars_of))))
stopifnot(all(vars_used %in% names(h)))
X <- lapply(setNames(vars_used, vars_used), function(vv) {
  x <- h[[vv]]
  if (is.factor(x) || is.character(x)) x <- as.character(x) %in% c("TRUE", "1")
  suppressWarnings(as.numeric(x))
})
hh_vec <- h$hh_group
n24 <- integer(nrow(sub))
for (i in seq_len(nrow(sub))) {
  cc <- parse_conds(sub$rule[i])
  m <- hh_vec == as.character(sub$hh[i])
  for (j in seq_len(nrow(cc))) {
    x <- X[[cc$var[j]]]
    cm <- switch(cc$op[j], ">=" = x >= cc$thr[j], ">" = x > cc$thr[j],
                 "<=" = x <= cc$thr[j], "<" = x < cc$thr[j])
    cm[is.na(cm)] <- FALSE
    m <- m & cm
  }
  n24[i] <- sum(m)
}
fam_n <- sum(fam_b); ref_n <- sum(!fam_b)
fam_c <- sum(n24[fam_b] < 10); ref_c <- sum(n24[!fam_b] < 10)
fam_rate <- fam_c / max(fam_n, 1); ref_rate <- ref_c / max(ref_n, 1)
ci <- binom.test(fam_c, max(fam_n, 1))$conf.int
ratio <- fam_rate / max(ref_rate, 1e-9)
pass2 <- ratio < 1.5
cat("== readout 2: mechanism / residual fragility (train-n band [30,300]) ==\n")
cat(sprintf("  family (utilities_sua): %d rules, collapse %d (%.2f%%; exact 95%% CI %.2f-%.2f%%)\n",
            fam_n, fam_c, 100 * fam_rate, 100 * ci[1], 100 * ci[2]))
cat(sprintf("  reference: %d rules, collapse %d (%.2f%%)\n",
            ref_n, ref_c, 100 * ref_rate))
cat(sprintf("  ratio %.2fx (bar: < 1.5x; raw-encoding baseline ~3.0x) -> %s\n",
            ratio, ifelse(pass2, "PASS", "FAIL")))
cat(sprintf("  NB per the design note this is a residual-fragility check under\n"))
cat(sprintf("  the tier form, not a mechanism confirmation.\n\n"))
cat(sprintf("== VERDICT: readout1 %s | readout2 %s | family share %.1f%% (monitor) ==\n",
            ifelse(pass1, "PASS", "FAIL"), ifelse(pass2, "PASS", "FAIL"),
            100 * mean(fam)))
