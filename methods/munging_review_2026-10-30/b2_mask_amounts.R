# B2 (pre-registered in b2_mask_amounts_prereg.txt): mask AMOUNTs in report-well state-years and compare
# the greedy fallback (F0, current code) and two alternatives (F1 proportional split, F2 typical-largest-first)
# with the AMOUNT-guided reconstruction. Uses the pre-Step-7 snapshot's function copies. Writes only to scratch/.
#   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" methods/munging_review_2026-10-30/b2_mask_amounts.R
setwd("C:/Users/ericg/snap_qc_v27")
out <- "methods/munging_review_2026-10-30/scratch/"
load(paste0(out, "pre7_env.RData"))
mydata <- calculate_raw_benefits(mydata)
mydata$amount_pattern <- amount_pattern(mydata)
el <- slot_matrix(mydata, "ELEMENT"); am <- slot_matrix(mydata, "AMOUNT"); am[is.na(am)] <- 0
pres <- !is.na(el); nel <- rowSums(pres)
sy <- paste(mydata$state_name, mydata$fiscal_year)

# ---- 2. report-well state-years
multi <- nel >= 2
inf_other <- mydata$amount_pattern %in% "other" & rowSums(am > 0 & pres) == nel & abs(rowSums(am) - mydata$AMTERR) <= 2
cs <- mydata$amount_pattern %in% "clean_split"
n_multi <- tapply(multi, sy, sum); n_cs <- tapply(cs, sy, sum); n_inf <- tapply(cs | inf_other, sy, sum)
rw <- names(n_multi)[n_cs >= 10 & n_inf / pmax(n_multi, 1) >= 0.20]
cat("report-well state-years:", length(rw), "\n"); print(sort(rw))
cat("informative 'other' cases nationally:", sum(inf_other), "\n")

# ---- 3. evaluation cases and truth
vars <- unique(unname(step7_vars))
cls  <- function(codes) unname(step7_prefix[step7_vars[as.character(codes)]])
run_set <- function(rows, label) {
  sub <- mydata[rows, ]
  base <- recreate_elements(sub)
  ok <- base$cascade_type %in% "capped" & base$rawben_recreated == base$RAWBEN
  cat(label, ": clean_split", length(rows), "-> capped & reached", sum(ok), "\n")
  rows[ok]
}
ev_rw  <- run_set(which(cs & sy %in% rw), "report-well")
ev_all <- run_set(which(cs), "all clean_split (sensitivity)")

evaluate <- function(ev, folds_seed = 20261030) {
  sub <- mydata[ev, ]
  truth <- recreate_elements(sub)
  E <- el[ev, , drop = FALSE]; A <- am[ev, , drop = FALSE]; P <- pres[ev, , drop = FALSE]
  k <- rowSums(P)
  mix <- vapply(seq_len(nrow(E)), function(i) paste(cls(E[i, P[i, ]]), collapse = "|"), "")
  share <- A / rowSums(A)
  dir <- ifelse(sub$RAWBEN > sub$FSBEN, 2, 3)
  # folds by state-year cluster
  set.seed(folds_seed)
  cl <- paste(sub$state_name, sub$fiscal_year); u <- unique(cl)
  fold <- setNames(sample(rep(1:2, length.out = length(u))), u)[cl]
  arms <- list()
  mask <- function(type) {
    d <- sub
    for (j in 1:9) {
      a <- paste0("AMOUNT", j); f <- paste0("E_FINDG", j); pj <- P[, j]
      d[[a]] <- ifelse(!pj, NA, switch(type, zero = 0, copied = d$AMTERR, amount1_only = if (j == 1) d$AMTERR else 0))
      if (type == "copied") d[[f]] <- ifelse(pj, dir, d[[f]])
    }
    d$amount_pattern <- amount_pattern(d); d
  }
  for (t in c("zero", "copied", "amount1_only")) arms[[paste0("F0_", t)]] <- recreate_elements(mask(t))
  # F1 / F2 cross-fitted
  F1 <- F2 <- NULL
  d1 <- mask("zero"); d2 <- mask("zero")
  for (f in 1:2) {
    tr <- fold != f; te <- which(fold == f)
    # F1: mean slot shares by mix (>=10 training cases), else by slot position for that k
    mix_sh <- lapply(split(seq_len(nrow(E))[tr], mix[tr]), function(r) if (length(r) >= 10) colMeans(share[r, , drop = FALSE], na.rm = TRUE) else NULL)
    pos_sh <- lapply(split(seq_len(nrow(E))[tr], pmin(k[tr], 4)), function(r) colMeans(share[r, , drop = FALSE], na.rm = TRUE))
    # F2: mean share by variable class
    cl_vals <- unlist(lapply(unname(which(tr)), function(i) setNames(share[i, P[i, ]], cls(E[i, P[i, ]]))))
    # (run 1 used which(tr) with names, so class names were prefixed by state-year, every lookup missed and F2 = F0; fixed 20:31)
    cl_mean <- tapply(cl_vals, names(cl_vals), mean)
    for (i in te) {
      s <- mix_sh[[mix[i]]]; if (is.null(s)) s <- pos_sh[[as.character(min(k[i], 4))]]
      s <- s[1:k[i]]; s[is.na(s)] <- 0; if (sum(s) <= 0) s <- rep(1, k[i]); s <- s / sum(s)
      for (j in 1:k[i]) {
        d1[[paste0("AMOUNT", j)]][i] <- round(s[j] * sub$AMTERR[i], 2); d1[[paste0("E_FINDG", j)]][i] <- dir[i]
        cm <- cl_mean[cls(E[i, j])]; d2[[paste0("AMOUNT", j)]][i] <- if (is.na(cm)) 0 else round(1000 * cm)
      }
    }
  }
  d1$amount_pattern <- "clean_split"; d2$amount_pattern <- "other"
  arms[["F1_proportional"]] <- recreate_elements(d1)
  arms[["F2_typical_first"]] <- recreate_elements(d2)
  # metrics
  start <- calculate_raw_benefits(sub)$rawben_recreated
  gap <- sub$RAWBEN - start
  v1 <- unname(step7_vars[as.character(E[, 1])])
  s1 <- function(r) {
    b <- lapply(sub[benefit_inputs], as.numeric)
    for (i in seq_along(v1)) b[[v1[i]]][i] <- r[[v1[i]]][i]
    (calculate_raw_benefits(b)$rawben_recreated - start) / gap
  }
  s1_truth <- s1(truth)
  res <- lapply(names(arms), function(nm) {
    r <- arms[[nm]]
    dif <- sapply(vars, function(v) abs(r[[v]] - truth[[v]])); tol <- sapply(vars, function(v) pmax(10, 0.02 * abs(truth[[v]])))
    moved <- sapply(vars, function(v) r[[v]] != sub[[v]] | truth[[v]] != sub[[v]])
    s1a <- s1(r)
    data.frame(method = nm, cluster = cl, state = sub$state_name, k = k, mix = mix, hh = pmin(sub$rawusize, 4),
               truth_s1 = s1_truth, all_agree = rowSums(dif > tol) == 0, mae = rowSums(dif),
               var_agree_n = rowSums(moved & dif <= tol), var_moved_n = rowSums(moved),
               reached = r$rawben_recreated == r$RAWBEN, attr_err = abs(s1a - s1_truth),
               attr_agree = abs(s1a - s1_truth) <= 0.10, stringsAsFactors = FALSE)
  })
  do.call(rbind, res)
}

boot_ci <- function(df, stat_fun, B = 1000, seed = 1) {
  set.seed(seed); cl <- unique(df$cluster); idx <- split(seq_len(nrow(df)), df$cluster)
  bs <- replicate(B, { s <- sample(cl, length(cl), replace = TRUE); stat_fun(df[unlist(idx[s]), ]) })
  quantile(bs, c(.025, .975), na.rm = TRUE)
}
report <- function(R, label) {
  cat("\n================", label, "================\n")
  cat("cases:", length(unique(paste(R$cluster, seq_len(nrow(R)))))/length(unique(R$method)), "; clusters:", length(unique(R$cluster)), "\n")
  tab <- do.call(rbind, lapply(split(R, R$method), function(d) {
    ci <- boot_ci(d, function(x) mean(x$all_agree))
    data.frame(method = d$method[1], n = nrow(d), all_agree = round(100 * mean(d$all_agree), 1),
               ci_lo = round(100 * ci[1], 1), ci_hi = round(100 * ci[2], 1),
               var_agree = round(100 * sum(d$var_agree_n) / sum(d$var_moved_n), 1), mae = round(mean(d$mae)),
               reached = round(100 * mean(d$reached), 1), attr_agree = round(100 * mean(d$attr_agree, na.rm = TRUE), 1),
               attr_err = round(mean(d$attr_err, na.rm = TRUE), 3))
  }))
  print(tab, row.names = FALSE)
  base <- R[R$method == "F0_zero", ]
  for (m in c("F1_proportional", "F2_typical_first")) {
    x <- R[R$method == m, ]; stopifnot(identical(x$cluster, base$cluster))
    dd <- data.frame(cluster = x$cluster, d = x$all_agree - base$all_agree, dm = x$mae - base$mae)
    ci <- boot_ci(dd, function(z) mean(z$d)); cim <- boot_ci(dd, function(z) mean(z$dm))
    cat(sprintf("%s - F0_zero: all_agree %+.1f pts [%+.1f, %+.1f]; MAE %+.0f [%+.0f, %+.0f]\n", m, 100 * mean(dd$d),
                100 * ci[1], 100 * ci[2], mean(dd$dm), cim[1], cim[2]))
  }
  brk <- function(var, min_n = 1) {
    t <- tapply(R$all_agree, list(R[[var]], R$method), function(z) round(100 * mean(z), 1))
    n <- tapply(R$all_agree, list(R[[var]], R$method), length)[, 1]
    t <- cbind(n = n, t[, c("F0_zero", "F0_copied", "F0_amount1_only", "F1_proportional", "F2_typical_first")])
    print(t[t[, "n"] >= min_n, , drop = FALSE])
  }
  cat("\nall_agree by number of elements:\n"); brk("k")
  cat("\nall_agree by element mix (n >= 15):\n"); brk("mix", 15)
  cat("\nall_agree by state:\n"); brk("state")
  cat("\nall_agree by household size (4 = 4+):\n"); brk("hh")
  R$s1grp <- ifelse(R$truth_s1 >= 0.9, "slot1>=0.9", "slot1<0.9"); cat("\nall_agree by truth slot-1 share:\n"); brk("s1grp")
  cat("truth slot-1 share quantiles:\n"); print(round(quantile(R$truth_s1[R$method == "F0_zero"], c(.1, .25, .5, .75, .9), na.rm = TRUE), 2))
}
R_rw <- evaluate(ev_rw); report(R_rw, "REPORT-WELL STATE-YEARS (primary)")
R_all <- evaluate(ev_all); report(R_all, "ALL CLEAN_SPLIT (sensitivity)")
saveRDS(list(rw = rw, R_rw = R_rw, R_all = R_all), paste0(out, "b2_results.rds"))
