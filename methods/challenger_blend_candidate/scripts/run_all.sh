#!/usr/bin/env bash
# Challenger benchmark: runs the learners one at a time, each under a time cap, then scores them (2026-11-12).
#   bash run_all.sh [learner ...]      default: xgb_tuned lgbm ebm_main ebm_pairs bart catboost tabnet
# A learner that fails or hits its cap is logged and the chain moves on; its finished seeds stay in scores/ and
# a rerun picks up where it stopped. Progress lines go to logs/status.log.
cd "$(dirname "$0")" || exit 1
PY=./.venv/Scripts/python.exe
RS="/c/Program Files/R/R-4.5.1/bin/Rscript.exe"
STATUS=logs/status.log
declare -A CAP=( [xgb_tuned]=45 [lgbm]=30 [ebm_main]=45 [ebm_pairs]=75 [bart]=90 [catboost]=90 [tabnet]=90 )   # minutes
say() { echo "[$(date '+%m-%d %H:%M:%S')] $*" | tee -a "$STATUS"; }
cleanup() {   # a capped job can leave worker processes behind: stop any Python still running from this folder's environment
  powershell.exe -NoProfile -Command "Get-Process python -ErrorAction SilentlyContinue | Where-Object { \$_.Path -like '*snap_qc_challenger_benchmark*' } | Stop-Process -Force -ErrorAction SilentlyContinue" >/dev/null 2>&1
}
LEARNERS=("$@"); [ ${#LEARNERS[@]} -eq 0 ] && LEARNERS=(xgb_tuned lgbm ebm_main ebm_pairs bart catboost tabnet)
say "CHAIN START: ${LEARNERS[*]}"
for l in "${LEARNERS[@]}"; do
  say "START $l (cap ${CAP[$l]} min)"
  if [ "$l" = bart ]; then timeout "$((CAP[$l] * 60))" "$RS" 03_fit_bart.R > "logs/run_$l.out" 2>&1
  else timeout "$((CAP[$l] * 60))" "$PY" -u 02_fit.py "$l" > "logs/run_$l.out" 2>&1; fi
  rc=$?; cleanup
  n=$(ls scores 2>/dev/null | grep -c "^${l}_W")
  if [ $rc -eq 0 ]; then say "DONE $l: $n score files"
  elif [ $rc -eq 124 ]; then say "CAPPED $l at ${CAP[$l]} min: $n score files so far"
  else say "FAILED $l (exit $rc): $n score files; last line: $(tail -n 1 "logs/run_$l.out" | cut -c1-200)"; fi
done
say "START scoring"
"$RS" 04_score.R > logs/04_score.log 2>&1 && say "DONE scoring" || say "FAILED scoring: $(tail -n 1 logs/04_score.log | cut -c1-200)"
say "CHAIN END"
