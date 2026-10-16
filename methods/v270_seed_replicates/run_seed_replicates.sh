#!/usr/bin/env bash
# v2.7 seed-replicate study: the whole chain. See design_note.md.
#   bash methods/v270_seed_replicates/run_seed_replicates.sh          (full: about 5.5 h)
#   SMOKE=1 bash methods/v270_seed_replicates/run_seed_replicates.sh  (40-round ensembles, 4 states)
# Two workers at a time, one per frame; each mines seed 117 (the anchor against
# the benchmark's cached pool), 118 and 119, scoring each pool on FY2024 as it
# lands. Then the pooled-seed arms, the pooled-pool state walks, and the readout.
# A pool on disk is not re-mined, so a relaunch resumes.
# Cut-off: at 06:00 a sentinel file is written, this chain's own R processes are
# stopped, and no further step starts (SR_CUTOFF / SR_CUTOFF_END override the
# window, HHMM, for testing).
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
stamp "=== seed replicates start${SMOKE:+ (SMOKE)} on branch $(git rev-parse --abbrev-ref HEAD) at $(git rev-parse --short HEAD)"
stamp "frames: final $(sha256sum reg_model_data.rds | cut -c1-16) | f0925 $(sha256sum archive_data/reg_model_data_pre_fycpi_2026-09-28.rds | cut -c1-16)"
stamp "study scripts (untracked): $(cat $D/common.R $D/mine_national_seed.R $D/score_pool.R $D/score_pooled.R $D/readout.R $D/run_seed_replicates.sh | sha256sum | cut -c1-16)"

WATCH=""
if [ "$SMOKE" != "1" ] || [ -n "$SR_CUTOFF" ]; then
  (
    while :; do
      now=$(date +%H%M)
      if (( 10#$now >= 10#$CUTOFF && 10#$now < 10#$CUTOFF_END )); then
        touch "$SENT"
        stamp "CUT-OFF $CUTOFF: stopping this chain's R processes; no further step starts (pools on disk are kept; a relaunch resumes)"
        for p in $(cat "$PIDS"); do
          if kill -0 "$p" 2>/dev/null; then
            wp=$(cat "/proc/$p/winpid" 2>/dev/null)
            [ -n "$wp" ] && taskkill //F //T //PID "$wp" > /dev/null 2>&1
            kill "$p" 2>/dev/null
          fi
        done
        exit 0
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
    grep -h "ANCHOR" "$OUT/logs/mine_${fr}_seed$s.log" | sed 's/^\[[0-9:]*\] //' | while read -r l; do stamp "$l"; done
    step "score $fr seed $s" "$OUT/logs/score_${fr}_seed$s.log" SR_FRAME=$fr SR_SEED=$s -- $D/score_pool.R
  done
}
chain f0925 & P1=$!
chain final & P2=$!
wait $P1 $P2

# wait on these steps' own PIDs: a bare `wait` also waits for the cut-off
# watcher, which is what stalled the 2026-10-02 run from 01:34 to 06:00
PP=""
for fr in f0925 final; do step "pooled arms $fr" "$OUT/logs/pooled_$fr.log" SR_FRAME=$fr -- $D/score_pooled.R & PP="$PP $!"; done
[ -n "$PP" ] && wait $PP   # never a bare wait: it would also wait for the cut-off watcher
PP=""
for fr in f0925 final; do
  step "state walk, three-seed pooled pool, $fr" "$OUT/logs/score_${fr}_pooled3.log" SR_FRAME=$fr SR_POOL=$OUT/pools/admitted_${fr}_pooled3.rds SR_TAG=${fr}_pooled3 SR_SKIP_RULES=1 -- $D/score_pool.R & PP="$PP $!"
done
[ -n "$PP" ] && wait $PP   # never a bare wait: it would also wait for the cut-off watcher
step "readout ($OUT/readout.md)" "$OUT/logs/readout.log" SR_DUMMY=1 -- $D/readout.R
[ -n "$WATCH" ] && kill $WATCH 2>/dev/null
if [ -f "$SENT" ]; then stamp "=== seed replicates CUT OFF at $CUTOFF (incomplete; relaunch to resume)"; else stamp "=== seed replicates DONE"; fi
