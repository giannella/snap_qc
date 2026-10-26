#!/usr/bin/env bash
# 2026-10-23 night 2 of the BBCE-limit feature exploration: arms A, D and E at
# mtry 3 and at mtry 4, arm A at mtry 4 and seed 118 (seed noise), and arm T1
# (the regimes plan's six light state options, mtry 2), each mined on FY2017-19
# with ranger alone and scored on FY2022, then readout_night2.R. Night 1's
# mtry-2 arms are read from disk. See design_note.md, "Night 2".
#   bash methods/bbce_limit_features/run_chain_night2.sh
# Two workers at most. A pool or state file on disk is kept, so a relaunch
# resumes. At 06:00 a sentinel stops this chain's R processes.
ROOT=/c/Users/ericg/snap_qc_v27
cd "$ROOT" || exit 1
R="/c/Program Files/R/R-4.5.1/bin/Rscript.exe"
D=methods/bbce_limit_features
[ "$SMOKE" = "1" ] && OD=$D/smoke || OD=$D
L=$OD/logs_night2; mkdir -p "$L"
LOG=$L/chain.log; SENT=$L/CUTOFF; PIDS=$L/pids
export SR_ROOT=C:/Users/ericg/snap_qc_v27
rm -f "$SENT"; : > "$PIDS"
CUTOFF=${SR_CUTOFF:-0600}; CUTOFF_END=${SR_CUTOFF_END:-1200}
stamp() { echo "[$(date '+%F %T %Z')] $*" >> "$LOG"; }
stamp "=== bbce-limit night-2 chain start on branch $(git rev-parse --abbrev-ref HEAD) at $(git rev-parse --short HEAD)${SMOKE:+ [SMOKE]}"
stamp "study scripts: $(cat $D/common.R $D/run_arm.R $D/readout_night2.R $D/run_chain_night2.sh $D/light_subgroups.csv | sha256sum | cut -c1-16) | lookups: $(cat additional_data/poverty_guidelines.csv additional_data/bbce_income.csv additional_data/snap_state_options_all_years.csv additional_data/standard_medical_deductions.csv additional_data/year_data.csv | sha256sum | cut -c1-16)"
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
finish() { kill $WATCH 2>/dev/null; stamp "=== bbce-limit night-2 chain $1"; exit 0; }

unit() {   # unit <arm> <mtry> [seed]
  local m=$2 s=${3:-117} t=$1
  [ "$m" != "2" ] && t="${t}_mtry$m"; [ "$s" != "117" ] && t="${t}_seed$s"
  if [ -f "$OD/states_$t.csv" ]; then stamp "arm $t already on disk"; return 0; fi
  step "arm $t" "$L/arm_$t.log" ARM=$1 ARM_MTRY=$m ARM_SEED=$s SMOKE=${SMOKE:-0} -- $D/run_arm.R; }
# each worker opens with its mtry's baseline (A); the light-options arm and the
# mtry-4 seed-noise arm go last, one per worker (four units each)
( unit A 3; unit D 3; unit E 3; unit A 4 118 ) & W1=$!
( unit A 4; unit D 4; unit E 4; unit T1 2 ) & W2=$!
wait $W1 $W2   # never a bare wait: it would also wait for the cut-off watcher

step "readout ($OD/readout_night2.md)" "$L/readout.log" SMOKE=${SMOKE:-0} -- $D/readout_night2.R
if [ -f "$SENT" ]; then finish "CUT OFF (incomplete; relaunch to resume)"; else finish "DONE"; fi
