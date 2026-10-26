#!/usr/bin/env bash
# 2026-10-22 night: the BBCE-limit feature exploration. Five arms (A: the
# shipped 19 features; B: + distance to the limit; C: + the BBCE limit; D: +
# both; E: + income as a percent of poverty + the limit), each mined on
# FY2017-19 with ranger alone at seed 117 and scored on FY2022, then the
# readout. See design_note.md.
#   bash methods/bbce_limit_features/run_chain.sh
# Two workers at most. A pool or state file on disk is kept, so a relaunch
# resumes. At 06:00 a sentinel stops this chain's R processes.
ROOT=/c/Users/ericg/snap_qc_v27
cd "$ROOT" || exit 1
R="/c/Program Files/R/R-4.5.1/bin/Rscript.exe"
D=methods/bbce_limit_features
[ "$SMOKE" = "1" ] && OD=$D/smoke || OD=$D
L=$OD/logs; mkdir -p "$L"
LOG=$L/chain.log; SENT=$L/CUTOFF; PIDS=$L/pids
export SR_ROOT=C:/Users/ericg/snap_qc_v27
rm -f "$SENT"; : > "$PIDS"
CUTOFF=${SR_CUTOFF:-0600}; CUTOFF_END=${SR_CUTOFF_END:-1200}
stamp() { echo "[$(date '+%F %T %Z')] $*" >> "$LOG"; }
stamp "=== bbce-limit chain start on branch $(git rev-parse --abbrev-ref HEAD) at $(git rev-parse --short HEAD)${SMOKE:+ [SMOKE]}"
stamp "study scripts: $(cat $D/common.R $D/run_arm.R $D/readout.R $D/run_chain.sh | sha256sum | cut -c1-16) | lookups: $(cat additional_data/poverty_guidelines.csv additional_data/bbce_income.csv | sha256sum | cut -c1-16)"
stamp "frame f1016: $(sha256sum /c/Users/ericg/snap_qc/archive_data/reg_model_data_f1016_2026-10-16.rds | cut -c1-16) (expected ba1f9626bd951f27)"

( while :; do
    now=$(date +%H%M)
    if (( 10#$now >= 10#$CUTOFF && 10#$now < 10#$CUTOFF_END )); then
      touch "$SENT"; stamp "CUT-OFF $CUTOFF: stopping this chain's R processes (pools on disk are kept; a relaunch resumes)"
      for p in $(cat "$PIDS"); do
        if kill -0 "$p" 2>/dev/null; then
          wp=$(cat "/proc/$p/winpid" 2>/dev/null); [ -n "$wp" ] && taskkill //F //T //PID "$wp" > /dev/null 2>&1; kill "$p" 2>/dev/null
        fi
      done
      exit 0
    fi
    sleep 20
  done ) &
WATCH=$!

step() {   # step <label> <logfile> <env assignments...> -- <script>
  local label=$1 logf=$2; shift 2
  local envs=(); while [ "$1" != "--" ]; do envs+=("$1"); shift; done; shift
  if [ -f "$SENT" ]; then stamp "$label not started (cut-off)"; return 1; fi
  stamp "$label start"
  env "${envs[@]}" "$R" "$1" > "$logf" 2>&1 &
  local pid=$!; echo "$pid" >> "$PIDS"
  if wait "$pid"; then stamp "$label done"; else stamp "$label FAILED or stopped (see $logf)"; return 1; fi
}
finish() { kill $WATCH 2>/dev/null; stamp "=== bbce-limit chain $1"; exit 0; }

unit() {   # unit <arm> [seed]
  local s=${2:-117} t=$1; [ "$s" != "117" ] && t="$1_seed$s"
  if [ -f "$OD/states_$t.csv" ]; then stamp "arm $t already on disk"; return 0; fi
  step "arm $t" "$L/arm_$t.log" ARM=$1 ARM_SEED=$s SMOKE=${SMOKE:-0} -- $D/run_arm.R; }
# A first on worker 1: the baseline every arm is read against; A at seed 118
# last on worker 2 (two arms against worker 1's three): ranger-only seed noise
( unit A; unit C; unit E ) & W1=$!
( unit B; unit D; unit A 118 ) & W2=$!
wait $W1 $W2   # never a bare wait: it would also wait for the cut-off watcher

step "readout ($OD/readout.md)" "$L/readout.log" SMOKE=${SMOKE:-0} -- $D/readout.R
if [ -f "$SENT" ]; then finish "CUT OFF (incomplete; relaunch to resume)"; else finish "DONE"; fi
