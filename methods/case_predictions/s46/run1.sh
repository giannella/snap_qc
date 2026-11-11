#!/bin/bash
# §46 Run 1: real mode, phases in order, stop on first failure.
set -e
unset S46_SMOKE
cd /c/Users/ericg/snap_qc_v27/methods/case_predictions/s46
R="/c/Program Files/R/R-4.5.1/bin/Rscript.exe"
mkdir -p out
for p in "choices" "tuneB" "freeze" "methodB" "stage1" "freeze2" "fit A,B,G,C" "score"; do
  echo "=== $(date '+%F %T') start $p" >> out/run1_phases.txt
  "$R" s46_run.R $p >> out/run1_stdout.txt 2>&1
  echo "=== $(date '+%F %T') done $p" >> out/run1_phases.txt
done
echo "=== $(date '+%F %T') RUN 1 COMPLETE" >> out/run1_phases.txt
