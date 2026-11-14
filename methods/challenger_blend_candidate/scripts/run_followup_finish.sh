#!/usr/bin/env bash
# Follow-up run (2026-11-13): held-back-year arms, promotion by the written rule, test-year fits of the promoted
# arms, one scoring read. See design_note_followup.md.   bash run_followup.sh
# Each stage runs under a time cap. A stage that fails or is capped is logged and the chain moves on, except that
# nothing is fitted on the test years unless the promotion step has written followup/promoted.json.
cd "$(dirname "$0")" || exit 1
PY=./.venv/Scripts/python.exe
RS="/c/Program Files/R/R-4.5.1/bin/Rscript.exe"
mkdir -p followup/logs; STATUS=followup/logs/status.log
say() { echo "[$(date '+%m-%d %H:%M:%S')] $*" | tee -a "$STATUS"; }
cleanup() {
  powershell.exe -NoProfile -Command "Get-Process python -ErrorAction SilentlyContinue | Where-Object { \$_.Path -like '*snap_qc_challenger_benchmark*' } | Stop-Process -Force -ErrorAction SilentlyContinue" >/dev/null 2>&1
}
stage() {   # stage <name> <cap minutes> <command...>
  local name=$1 cap=$2; shift 2
  say "START $name (cap $cap min)"
  timeout "$((cap * 60))" "$@" > "followup/logs/run_$name.out" 2>&1; local rc=$?; cleanup
  if [ $rc -eq 0 ]; then say "DONE $name"
  elif [ $rc -eq 124 ]; then say "CAPPED $name at $cap min"
  else say "FAILED $name (exit $rc); last line: $(tail -n 1 "followup/logs/run_$name.out" | cut -c1-200)"; fi
  return $rc
}
say "CHAIN RESUME at test_bart (promotion and the xgboost, CatBoost and blend test fits are kept as made)"
[ -f followup/promoted.json ] || { say "promoted.json is missing: nothing run. CHAIN END"; exit 1; }
say "promoted.json sha256 $(sha256sum followup/promoted.json | cut -c1-16); 06_bart_proper.R $(sha256sum 06_bart_proper.R | cut -c1-16)"
stage test_bart 210 "$RS" 06_bart_proper.R test
say "score files: $(ls followup/scores 2>/dev/null | sed 's/_W[123]_[0-9]*\.csv//' | sort | uniq -c | tr '\n' ';')"
stage score 120 "$RS" 07_score_followup.R
say "CHAIN END"
