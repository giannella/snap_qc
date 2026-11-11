#!/bin/bash
# §46 Run 2: real mode, phases in order, stop on first failure.
set -e
unset S46_SMOKE
cd /c/Users/ericg/snap_qc_v27/methods/case_predictions/s46
R="/c/Program Files/R/R-4.5.1/bin/Rscript.exe"
mkdir -p out
for p in "fit GC,E,D,F" "score"; do
  echo "=== $(date '+%F %T') start $p" >> out/run2_phases.txt
  "$R" s46_run.R $p >> out/run2_stdout.txt 2>&1
  echo "=== $(date '+%F %T') done $p" >> out/run2_phases.txt
done
echo "=== $(date '+%F %T') RUN 2 COMPLETE" >> out/run2_phases.txt
