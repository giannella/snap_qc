#!/usr/bin/env bash
# 2026-10-24 night 3 of the BBCE-limit feature exploration: three seeds (117,
# 118, 119) of arms A and E at mtry 2 and mtry 3, in two windows: mine
# FY2017-19 and score FY2022 (seven new units; the other five are on disk from
# nights 1-2), and mine FY2022-23 and score FY2024 (all twelve new, outputs in
# fy2024/). Then readout_night3.R. See design_note.md, "Night 3".
#   bash methods/bbce_limit_features/run_chain_night3.sh
# Two workers at most. A pool or state file on disk is kept, so a relaunch
# resumes. At 06:00 a sentinel stops this chain's R processes.
ROOT=/c/Users/ericg/snap_qc_v27
cd "$ROOT" || exit 1
R="/c/Program Files/R/R-4.5.1/bin/Rscript.exe"
D=methods/bbce_limit_features
[ "$SMOKE" = "1" ] && OD=$D/smoke || OD=$D
L=$OD/logs_night3; mkdir -p "$L"
LOG=$L/chain.log; SENT=$L/CUTOFF; PIDS=$L/pids
export SR_ROOT=C:/Users/ericg/snap_qc_v27
rm -f "$SENT"; : > "$PIDS"
CUTOFF=${SR_CUTOFF:-0600}; CUTOFF_END=${SR_CUTOFF_END:-1200}
stamp() { echo "[$(date '+%F %T %Z')] $*" >> "$LOG"; }
stamp "=== bbce-limit night-3 chain start on branch $(git rev-parse --abbrev-ref HEAD) at $(git rev-parse --short HEAD)${SMOKE:+ [SMOKE]}"
stamp "study scripts: $(cat $D/common.R $D/run_arm.R $D/readout_night3.R $D/run_chain_night3.sh | sha256sum | cut -c1-16) | lookups: $(cat additional_data/poverty_guidelines.csv additional_data/bbce_income.csv additional_data/snap_state_options_all_years.csv additional_data/standard_medical_deductions.csv additional_data/year_data.csv | sha256sum | cut -c1-16)"
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
finish() { kill $WATCH 2>/dev/null; stamp "=== bbce-limit night-3 chain $1"; exit 0; }

unit() {   # unit <window> <arm> <mtry> <seed>
  local w=$1 m=$3 s=$4 t=$2 od=$OD
  [ "$m" != "2" ] && t="${t}_mtry$m"; [ "$s" != "117" ] && t="${t}_seed$s"
  [ "$w" = "fy2024" ] && od=$OD/fy2024
  if [ -f "$od/states_$t.csv" ]; then stamp "$w arm $t already on disk"; return 0; fi
  step "$w arm $t" "$L/${w}_arm_$t.log" SR_WINDOW=$w ARM=$2 ARM_MTRY=$m ARM_SEED=$s SMOKE=${SMOKE:-0} -- $D/run_arm.R; }
# the FY2022 window first on both workers (the primary window; about 45 min a
# unit), then the FY2024 window (two training years; shorter units)
( unit fy2022 A 3 118; unit fy2022 E 3 118; unit fy2022 A 2 119; unit fy2022 E 2 119
  unit fy2024 A 2 117; unit fy2024 A 3 117; unit fy2024 A 2 118; unit fy2024 A 3 118; unit fy2024 A 2 119 ) & W1=$!
( unit fy2022 A 3 119; unit fy2022 E 3 119; unit fy2022 E 2 118
  unit fy2024 E 2 117; unit fy2024 E 3 117; unit fy2024 E 2 118; unit fy2024 E 3 118; unit fy2024 E 2 119; unit fy2024 E 3 119; unit fy2024 A 3 119 ) & W2=$!
wait $W1 $W2   # never a bare wait: it would also wait for the cut-off watcher

# the readout runs even after a cut-off (seconds, no mining): it reads
# whichever runs finished and says how many are on disk
stamp "readout ($OD/readout_night3.md) start"
SMOKE=${SMOKE:-0} "$R" $D/readout_night3.R > "$L/readout.log" 2>&1 && stamp "readout done" || stamp "readout FAILED (see $L/readout.log)"
if [ -f "$SENT" ]; then finish "CUT OFF (incomplete; relaunch to resume)"; else finish "DONE"; fi
