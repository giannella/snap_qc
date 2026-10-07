#!/usr/bin/env bash
# Two-at-a-time continuation of run_all.sh (2026-09-20 21:55). Each worker
# holds about 13 GB while scoring ~150k raw rules on the training years, so
# four at once exhausted this 60 GB host. The FY2017-18 nominal and cpi arms
# from the first launch are still running; this waits for them, then runs
# the remaining pairs: the seed-117 pair of the second window first (the
# primary contrast), then both seed-118 pairs. An interim readout follows
# the seed-117 pairs; the final readout follows everything.
cd /c/Users/ericg/snap_qc_staging_cpi || exit 1
R="/c/Program Files/R/R-4.5.1/bin/Rscript.exe"
OUT=methods/cpi_inflate_test/out
finished() { grep -q 'complete$\|Error\|Execution halted' "$OUT/run_$1.log" 2>/dev/null; }
until finished 1718_19_nominal && finished 1718_19_cpi; do sleep 30; done
echo "first-launch arms finished: $(date)"
run_pair() {
  for arm in "$2" "$3"; do
    CPI_ERA=$1 CPI_ARM=$arm "$R" methods/cpi_inflate_test/cpi_inflate_oneyear_ahead_v2.R \
      > "$OUT/run_$1_${arm}.log" 2>&1 &
  done
  wait
  echo "pair $1 $2 / $3 done: $(date)"
}
run_pair 2223_24 nominal cpi
"$R" methods/cpi_inflate_test/readout.R > "$OUT/readout_interim.log" 2>&1
echo "interim readout done: $(date)"
run_pair 1718_19 nominal_seed2 cpi_seed2
run_pair 2223_24 nominal_seed2 cpi_seed2
"$R" methods/cpi_inflate_test/readout.R > "$OUT/readout.log" 2>&1
echo "readout done: $(date)"
