#!/usr/bin/env bash
# 2026-10-16 night: rebuild the frame on staging-v2.7 (Ben's lookup-table audit
# and reconstruction fixes), check it against the v2.7 final frame, mine it
# nationally on FY2022-23 at seeds 117 / 118 / 119, score each pool on FY2024
# (49 national-only state lists at 5% / 10%, and the top rules), then the
# readout against the final frame's results on record. See design_note_f1016.md.
#   bash methods/v270_seed_replicates/run_f1016.sh
# Two mine workers at most (about 13 GB each). A pool on disk is not re-mined,
# so a relaunch resumes; SKIP_REBUILD=1 skips the munging step. At 06:00 a
# sentinel stops this chain's R processes and no further step starts.
ROOT=/c/Users/ericg/snap_qc_v27
cd "$ROOT" || exit 1
R="/c/Program Files/R/R-4.5.1/bin/Rscript.exe"
D=methods/v270_seed_replicates
L=$D/logs_f1016; mkdir -p "$L" "$D/pools"
LOG=$L/chain.log; SENT=$L/CUTOFF; PIDS=$L/pids
FRAME_COPY=/c/Users/ericg/snap_qc/archive_data/reg_model_data_f1016_2026-10-16.rds
export SR_ROOT=C:/Users/ericg/snap_qc_v27
rm -f "$SENT"; : > "$PIDS"
CUTOFF=${SR_CUTOFF:-0600}; CUTOFF_END=${SR_CUTOFF_END:-1200}
stamp() { echo "[$(date '+%F %T %Z')] $*" >> "$LOG"; }
stamp "=== f1016 chain start on branch $(git rev-parse --abbrev-ref HEAD) at $(git rev-parse --short HEAD)"
stamp "study scripts: $(cat $D/common.R $D/mine_national_seed.R $D/score_pool.R $D/frame_check_f1016.R $D/readout_f1016.R $D/run_f1016.sh | sha256sum | cut -c1-16)"

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
finish() { kill $WATCH 2>/dev/null; stamp "=== f1016 chain $1"; exit 0; }

# 1. frame rebuild (writes reg_model_data.rds to this worktree's root)
if [ "$SKIP_REBUILD" != "1" ]; then
  touch "$L/rebuild_start"
  step "frame rebuild" "$L/munging.log" SR_DUMMY=1 -- 1_data_munging_and_raw_variable_reconstruction_for_using_public_qc_data.R || finish "STOPPED (rebuild failed)"
  grep -A1 "CHECK" "$L/munging.log" | head -20 >> "$LOG"
  [ reg_model_data.rds -nt "$L/rebuild_start" ] || finish "STOPPED (reg_model_data.rds was not rewritten)"
  cp reg_model_data.rds "$FRAME_COPY" || finish "STOPPED (frame copy failed)"
fi
FH=$(sha256sum /c/Users/ericg/snap_qc/reg_model_data.rds | cut -c1-16)
stamp "frames: f1016 $(sha256sum "$FRAME_COPY" | cut -c1-16) | final $FH (expected 1dbe8733ad0f30bc)"
[ "$FH" = "1dbe8733ad0f30bc" ] || finish "STOPPED (the final frame in the main checkout is not the v2.7 final frame)"

# 2. frame check (hard: same cases, same errors)
step "frame check" "$L/frame_check.log" SR_DUMMY=1 -- $D/frame_check_f1016.R || finish "STOPPED (frame check failed)"

# 3. mines and scoring, two workers
# a scoring output already on disk is kept too (2026-10-17 resume: seeds 117
# and 118 were scored in the daytime after the 10-16 run was cut by a restart)
unit() { step "mine f1016 seed $1" "$L/mine_seed$1.log" SR_FRAME=f1016 SR_SEED=$1 -- $D/mine_national_seed.R || return 1
         if [ -f "$D/states_f1016_seed$1.csv" ] && [ -f "$D/rules_f1016_seed$1.rds" ]; then stamp "score f1016 seed $1 already on disk"; return 0; fi
         step "score f1016 seed $1" "$L/score_seed$1.log" SR_FRAME=f1016 SR_SEED=$1 -- $D/score_pool.R; }
( unit 117; unit 119 ) & W1=$!
# worker 2 also re-scores the final frame's cached seed-117 pool with today's
# scripts; the readout checks it reproduces states_final_seed117.csv (the
# 10-02 run used the then-untracked study scripts)
( unit 118
  if [ -f "$D/states_final_rescore117.csv" ]; then stamp "re-score final seed 117 already on disk"
  else step "re-score final seed 117 (reproduction check)" "$L/rescore_final117.log" SR_FRAME=final SR_TAG=final_rescore117 SR_SKIP_RULES=1 SR_POOL=C:/Users/ericg/snap_qc/methods/v270_seed_replicates/pools/admitted_final_seed117.rds -- $D/score_pool.R; fi ) & W2=$!
wait $W1 $W2   # never a bare wait: it would also wait for the cut-off watcher

# 4. readout
step "readout ($D/readout_f1016.md)" "$L/readout.log" SR_DUMMY=1 -- $D/readout_f1016.R
if [ -f "$SENT" ]; then finish "CUT OFF (incomplete; relaunch to resume)"; else finish "DONE"; fi
