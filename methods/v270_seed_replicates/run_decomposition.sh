#!/usr/bin/env bash
# v2.7 seed replicates, night 2 (2026-10-03): the decomposition. See design_note.md.
#   bash methods/v270_seed_replicates/run_decomposition.sh          (full: about 7 h)
#   SMOKE=1 bash methods/v270_seed_replicates/run_decomposition.sh  (40-round ensembles, 4 states)
# 1. Mines and scores the two intermediate frames at seeds 117, 118, 119, one
#    worker per frame: f0928 (CPI keyed on fiscal year, annual-average values,
#    and the SUA tier on review-year amounts: step 1 carries both) and f0929
#    (October CPI values, no SUA tolerance). With night 1's 09-25 and
#    final pools this splits the frame difference into its three steps.
# 2. Readout (sections 1-5).
# Then the clean CPI-off frame (the final frame's own munging code with its CPI
# switch off, built 2026-10-03) and the older no-CPI frame (nominal,
# 2026-09-21) at the same three seeds. Nothing else runs: the
# pooled-seed and consensus lines are dropped
# (project lead, 2026-10-03). Only the decomposition.
# A pool on disk is not re-mined and a scored pool is not re-scored, so a
# relaunch resumes. Cut-off: at 06:00 a
# sentinel file is written, this chain's own R processes are stopped, and no
# further step starts. Every wait names its PIDs: a bare `wait` also waits for
# the cut-off watcher (the night-1 stall).
cd /c/Users/ericg/snap_qc || exit 1
R="/c/Program Files/R/R-4.5.1/bin/Rscript.exe"
D=methods/v270_seed_replicates
OUT=$D; [ "$SMOKE" = "1" ] && OUT=$D/smoke
mkdir -p "$OUT/logs" "$OUT/pools"
LOG=$OUT/logs/chain.log
SENT=$OUT/logs/CUTOFF; PIDS=$OUT/logs/pids
rm -f "$SENT"; : > "$PIDS"
CUTOFF=${SR_CUTOFF:-0600}; CUTOFF_END=${SR_CUTOFF_END:-1200}
stamp() { echo "[$(date '+%F %T %Z')] $*" >> "$LOG"; }
stamp "=== decomposition night start${SMOKE:+ (SMOKE)} on branch $(git rev-parse --abbrev-ref HEAD) at $(git rev-parse --short HEAD)"
stamp "frames: f0928 $(sha256sum archive_data/reg_model_data_pre_octcpi_2026-09-29.rds | cut -c1-16) | f0929 $(sha256sum archive_data/reg_model_data_pre_suatol_2026-09-29.rds | cut -c1-16) | nominal $(sha256sum archive_data/reg_model_data_pre_cpi_rebuild_2026-09-24.rds | cut -c1-16) | cpioff $(sha256sum archive_data/reg_model_data_cpioff_2026-10-03.rds 2>/dev/null | cut -c1-16)"
stamp "study scripts (untracked): $(cat $D/common.R $D/mine_national_seed.R $D/score_pool.R $D/score_pooled.R $D/readout.R $D/run_decomposition.sh | sha256sum | cut -c1-16)"

WATCH=""
if [ "$SMOKE" != "1" ] || [ -n "$SR_CUTOFF" ]; then
  (
    while :; do
      now=$(date +%H%M)
      if (( 10#$now >= 10#$CUTOFF && 10#$now < 10#$CUTOFF_END )); then
        touch "$SENT"
        stamp "CUT-OFF $CUTOFF: stopping this chain's R processes; no further step starts (pools on disk are kept; a relaunch resumes)"
        while :; do   # repeated passes until the chain ends and kills the watcher, so a step that slipped past the sentinel check is caught too
          for p in $(cat "$PIDS"); do
            if kill -0 "$p" 2>/dev/null; then
              wp=$(cat "/proc/$p/winpid" 2>/dev/null)
              [ -n "$wp" ] && taskkill //F //T //PID "$wp" > /dev/null 2>&1
              kill "$p" 2>/dev/null
            fi
          done
          sleep 10
        done
      fi
      sleep 20
    done
  ) &
  WATCH=$!
fi

step() {   # step <label> <logfile> <env assignments...> -- <script>
  local label=$1 logf=$2; shift 2
  local envs=(); while [ "$1" != "--" ]; do envs+=("$1"); shift; done; shift
  if [ -f "$SENT" ]; then stamp "$label not started (cut-off)"; return 1; fi
  env "${envs[@]}" "$R" "$1" > "$logf" 2>&1 &
  local pid=$!; echo "$pid" >> "$PIDS"
  if wait "$pid"; then stamp "$label done"; else stamp "$label FAILED or stopped (see $logf)"; return 1; fi
}
chain() {
  local fr=$1
  for s in 117 118 119; do
    step "mine $fr seed $s" "$OUT/logs/mine_${fr}_seed$s.log" SR_FRAME=$fr SR_SEED=$s -- $D/mine_national_seed.R || continue
    if [ -f "$OUT/states_${fr}_seed$s.csv" ] && [ -f "$OUT/rules_${fr}_seed$s.rds" ]; then stamp "score $fr seed $s already on disk"; continue; fi
    step "score $fr seed $s" "$OUT/logs/score_${fr}_seed$s.log" SR_FRAME=$fr SR_SEED=$s -- $D/score_pool.R
  done
}
# two workers; the decomposition frames first, then the clean CPI-off frame,
# then the older no-CPI (nominal) frame, so a cut-off costs the least
# important comparison first
chain_units() {   # chain_units <frame:seed> ...
  for u in "$@"; do
    local fr=${u%%:*} s=${u##*:}
    step "mine $fr seed $s" "$OUT/logs/mine_${fr}_seed$s.log" SR_FRAME=$fr SR_SEED=$s -- $D/mine_national_seed.R || continue
    grep -h "ANCHOR" "$OUT/logs/mine_${fr}_seed$s.log" | sed 's/^\[[0-9:]*\] //' | while read -r l; do stamp "$l"; done
    if [ -f "$OUT/states_${fr}_seed$s.csv" ] && [ -f "$OUT/rules_${fr}_seed$s.rds" ]; then stamp "score $fr seed $s already on disk"; continue; fi
    step "score $fr seed $s" "$OUT/logs/score_${fr}_seed$s.log" SR_FRAME=$fr SR_SEED=$s -- $D/score_pool.R
  done
}
chain_units f0928:117 f0928:118 f0928:119 cpioff:117 cpioff:119 nominal:118 & P1=$!
chain_units f0929:117 f0929:118 f0929:119 cpioff:118 nominal:117 nominal:119 & P2=$!
wait $P1 $P2

step "readout, decomposition ($OUT/readout.md)" "$OUT/logs/readout_decomposition.log" SR_DUMMY=1 -- $D/readout.R

[ -n "$WATCH" ] && kill "$WATCH" 2>/dev/null
if [ -f "$SENT" ]; then stamp "=== decomposition night CUT OFF at $CUTOFF (incomplete; relaunch to resume)"; else stamp "=== decomposition night DONE"; fi
