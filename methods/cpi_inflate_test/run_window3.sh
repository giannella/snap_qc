#!/usr/bin/env bash
# Third window (added 2026-09-21): mine FY2017-19, score FY2022, CPI target
# 2022. Two arms at a time (memory), seed-117 pair first, interim readout,
# then the seed-118 pair and the final readout over all three windows.
#   bash methods/cpi_inflate_test/run_window3.sh          # full
#   SMOKE=1 bash methods/cpi_inflate_test/run_window3.sh  # tiny ensembles, 4 states
cd /c/Users/ericg/snap_qc_staging_cpi || exit 1
R="/c/Program Files/R/R-4.5.1/bin/Rscript.exe"
OUT=methods/cpi_inflate_test/out
[ "$SMOKE" = "1" ] && OUT=$OUT/smoke
mkdir -p "$OUT"
run_pair() {
  for arm in "$1" "$2"; do
    CPI_ERA=1719_22 CPI_ARM=$arm "$R" methods/cpi_inflate_test/cpi_inflate_oneyear_ahead_v2.R \
      > "$OUT/run_1719_22_${arm}.log" 2>&1 &
  done
  wait
  echo "pair $1 / $2 done: $(date)"
}
run_pair nominal cpi
"$R" methods/cpi_inflate_test/readout.R > "$OUT/readout_interim_w3.log" 2>&1
echo "interim readout done: $(date)"
run_pair nominal_seed2 cpi_seed2
"$R" methods/cpi_inflate_test/readout.R > "$OUT/readout.log" 2>&1
echo "=== DONE: readout done: $(date)"
