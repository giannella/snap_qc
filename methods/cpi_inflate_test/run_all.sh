#!/usr/bin/env bash
# Runner for the CPI-inflation test: four arms of one window in parallel,
# then the next window, then the readout. Needs the frames from
# build_frames.R and the frame diff from frame_diff.R.
#   bash methods/cpi_inflate_test/run_all.sh          # full run, ~2 h
#   SMOKE=1 bash methods/cpi_inflate_test/run_all.sh  # tiny ensembles, 4 states
cd /c/Users/ericg/snap_qc_staging_cpi || exit 1
R="/c/Program Files/R/R-4.5.1/bin/Rscript.exe"
OUT=methods/cpi_inflate_test/out
[ "$SMOKE" = "1" ] && OUT=$OUT/smoke
mkdir -p "$OUT"
for era in 1718_19 2223_24; do
  for arm in nominal cpi nominal_seed2 cpi_seed2; do
    CPI_ERA=$era CPI_ARM=$arm "$R" methods/cpi_inflate_test/cpi_inflate_oneyear_ahead_v2.R \
      > "$OUT/run_${era}_${arm}.log" 2>&1 &
  done
  wait
  echo "window $era done: $(date)"
done
"$R" methods/cpi_inflate_test/readout.R > "$OUT/readout.log" 2>&1
echo "readout done: $(date)"
