setwd("C:/Users/ericg/snap_qc_v27")
a <- readRDS("archive_data/reg_model_data_f1027_eb989fda.rds"); b <- readRDS("reg_model_data.rds")
ka <- paste(a$state, a$fiscal_year, a$hhldno); kb <- paste(b$state, b$fiscal_year, b$hhldno); m <- match(kb, ka)
r <- which(b$imputed_at_bound %in% "max" & b$correctednotes == "earn_down")
cat("new pure earn_down imputed at max:", length(r), "; matched in old:", sum(!is.na(m[r])), "\n")
cat("old notes for them:\n"); print(head(sort(table(a$correctednotes[m[r]]), decreasing = TRUE), 6))
oe <- a$rawearn_nominal[m[r]]; ne <- b$rawearn_nominal[r]; q <- b$fsearn[r]
cat(sprintf("rawearn == 0: old %.1f%%, new %.1f%% | median raw/FS: old %.2f new %.2f\n", 100*mean(oe == 0, na.rm=TRUE), 100*mean(ne == 0), median(oe/q, na.rm=TRUE), median(ne/q)))
cat("error label among them:", mean(b$over_threshold[r] == 1), "\n")
# same for all earn_down / unearn_down at max in each frame
for (nm in c("a","b")) { d <- get(nm); s <- d$correctednotes %in% c("earn_down") & d$raw_benefit_amount >= d$maximum_benefit_for_HH_size
  cat(nm, "earn_down at max:", sum(s), " zero share", round(mean(d$rawearn_nominal[s] == 0), 3), " distinct nonzero values / nonzero rows:",
      length(unique(d$rawearn_nominal[s & d$rawearn_nominal > 0])), "/", sum(s & d$rawearn_nominal > 0), "\n") }
