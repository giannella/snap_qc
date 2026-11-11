# Munging review 2026-10-30: label signatures at the maximum benefit, old frame vs new frame.
# Among cases paid the maximum allotment, how often does the uncapped reconstructed benefit sit right on the
# maximum (unc_rawben_rel_max in [1, 1.01)), for errors vs clean cases? A value that only error cases take is a
# reconstruction artifact the rule miner can use. Writes nothing.
setwd("C:/Users/ericg/snap_qc_v27")
for (f in c("archive_data/reg_model_data_f1027_eb989fda.rds", "reg_model_data.rds")) {
  d <- readRDS(f)
  err <- d$over_threshold %in% 1
  atmax <- (d$rawben >= d$maximum_benefit_for_HH_size) %in% TRUE
  u <- d$unc_rawben_rel_max
  edge <- (u >= 1 & u < 1.01) %in% TRUE
  cat(sprintf("%-50s at-max rows %6d | on-edge share: errors %5.1f%% (n=%5d) clean %5.1f%% (n=%6d) | error rate on-edge %5.1f%% vs at-max overall %5.1f%%\n",
              f, sum(atmax), 100 * mean(edge[atmax & err]), sum(atmax & err), 100 * mean(edge[atmax & !err]), sum(atmax & !err),
              100 * mean(err[atmax & edge]), 100 * mean(err[atmax])))
  # same at the minimum benefit
  atmin <- (d$rawben <= d$minimum_ben & d$minimum_ben > 0) %in% TRUE
  cat(sprintf("%-50s at-min rows %6d | error rate %5.1f%%\n", "", sum(atmin), 100 * mean(err[atmin])))
}
