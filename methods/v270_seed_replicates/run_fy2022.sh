#!/usr/bin/env bash
# v2.7 frames, night 3 (2026-10-04): the FY2017-19 -> FY2022 window.
#   bash methods/v270_seed_replicates/run_fy2022.sh          (night 4: about 6.5 h)
#   SMOKE=1 bash methods/v270_seed_replicates/run_fy2022.sh  (40-round ensembles, 4 states)
# Same study scripts with SR_WINDOW=fy2022 (mine FY2017-19, score FY2022).
# Tonight, two workers: the CPI step alone at three seeds (final and CPI-off
# frames; final seed 117 is the cached benchmark pool, scored only), plus the
# 09-28 frame's cached seed 117 (scored only). FY2017-19 mines take about 2.4 h
# each with two workers, so the 09-25 and 09-29 frames (needed for every step
# contrast) do not fit and wait for a relaunch with those units (it resumes
# from the pools on disk). The readout runs at the end even after a cut-off
# (it only reads files).
# A pool on disk is not re-mined and a scored pool is not re-scored, so a
# relaunch resumes. Cut-off at 06:00: sentinel file, this chain's own R
# processes stopped, no further mining or scoring step starts. Every wait
# names its PIDs and is skipped when the list is empty.
cd /c/Users/ericg/snap_qc || exit 1
export SR_WINDOW=fy2022
R="/c/Program Files/R/R-4.5.1/bin/Rscript.exe"
D=methods/v270_seed_replicates
OUT=$D/fy2022; [ "$SMOKE" = "1" ] && OUT=$D/fy2022/smoke
mkdir -p "$OUT/logs" "$OUT/pools"
LOG=$OUT/logs/chain.log
SENT=$OUT/logs/CUTOFF; PIDS=$OUT/logs/pids
rm -f "$SENT"; : > "$PIDS"
CUTOFF=${SR_CUTOFF:-0600}; CUTOFF_END=${SR_CUTOFF_END:-1200}
stamp() { echo "[$(date '+%F %T %Z')] $*" >> "$LOG"; }
stamp "=== FY2022 window night start${SMOKE:+ (SMOKE)} on branch $(git rev-parse --abbrev-ref HEAD) at $(git rev-parse --short HEAD)"
stamp "frames: final $(sha256sum reg_model_data.rds | cut -c1-16) | cpioff $(sha256sum archive_data/reg_model_data_cpioff_2026-10-03.rds | cut -c1-16) | f0925 $(sha256sum archive_data/reg_model_data_pre_fycpi_2026-09-28.rds | cut -c1-16) | f0928 $(sha256sum archive_data/reg_model_data_pre_octcpi_2026-09-29.rds | cut -c1-16) | f0929 $(sha256sum archive_data/reg_model_data_pre_suatol_2026-09-29.rds | cut -c1-16)"
# refuse to start unless every frame is the one the cached pools and checks were built on
for pair in "reg_model_data.rds:1dbe8733ad0f30bc" "archive_data/reg_model_data_cpioff_2026-10-03.rds:2093ce1e7a817208"             "archive_data/reg_model_data_pre_fycpi_2026-09-28.rds:c4f715f59aef8291" "archive_data/reg_model_data_pre_octcpi_2026-09-29.rds:fdbc4508a2136370"             "archive_data/reg_model_data_pre_suatol_2026-09-29.rds:a0fd4f4dd643a0b2"; do
  f=${pair%%:*}; want=${pair##*:}; got=$(sha256sum "$f" | cut -c1-16)
  if [ "$got" != "$want" ]; then stamp "ABORT: $f has hash $got, expected $want; no mining started"; exit 1; fi
done
stamp "frame hashes checked"
stamp "study scripts (untracked): $(cat $D/common.R $D/mine_national_seed.R $D/score_pool.R $D/readout_window.R $D/run_fy2022.sh | sha256sum | cut -c1-16)"

WATCH=""
if [ "$SMOKE" != "1" ] || [ -n "$SR_CUTOFF" ]; then
  (
    while :; do
      kill -0 $$ 2>/dev/null || exit 0   # the chain is gone: stop watching
      now=$(date +%H%M)
      if (( 10#$now >= 10#$CUTOFF && 10#$now < 10#$CUTOFF_END )); then
        touch "$SENT"
        stamp "CUT-OFF $CUTOFF: stopping this chain's R processes; no further mining or scoring starts (pools on disk are kept; a relaunch resumes)"
        while :; do
          for p in $(cat "$PIDS"); do
            if kill -0 "$p" 2>/dev/null; then
              wp=$(cat "/proc/$p/winpid" 2>/dev/null)
              [ -n "$wp" ] && taskkill //F //T //PID "$wp" > /dev/null 2>&1
              kill "$p" 2>/dev/null
            fi
          done
          sleep 10
          kill -0 $$ 2>/dev/null || exit 0
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
chain_units() {   # chain_units <frame:seed> ...
  for u in "$@"; do
    local fr=${u%%:*} s=${u##*:}
    step "mine $fr seed $s" "$OUT/logs/mine_${fr}_seed$s.log" SR_FRAME=$fr SR_SEED=$s -- $D/mine_national_seed.R || continue
    if [ -f "$OUT/states_${fr}_seed$s.csv" ] && [ -f "$OUT/rules_${fr}_seed$s.rds" ]; then stamp "score $fr seed $s already on disk"; continue; fi
    step "score $fr seed $s" "$OUT/logs/score_${fr}_seed$s.log" SR_FRAME=$fr SR_SEED=$s -- $D/score_pool.R
  done
}
# night 4 (2026-10-05): the step frames. Night 3's units are on disk and are
# skipped. Order: complete seed 117 (all three steps), then seed 118, then the
# 09-29 frame at seed 119 (completes step 3, the tolerance, at three seeds).
chain_units final:117 cpioff:117 final:118 f0928:117 f0925:117 f0925:118 f0928:118 & P1=$!
chain_units cpioff:118 final:119 cpioff:119 f0929:117 f0929:118 f0929:119 & P2=$!
wait $P1 $P2

# the readout only reads files, so it runs even after a cut-off, outside the
# watcher's PID list
if "$R" $D/readout_window.R > "$OUT/logs/readout.log" 2>&1; then stamp "readout done ($OUT/readout.md)"; else stamp "readout FAILED (see $OUT/logs/readout.log)"; fi
[ -n "$WATCH" ] && kill "$WATCH" 2>/dev/null
if [ -f "$SENT" ]; then stamp "=== FY2022 window night CUT OFF at $CUTOFF (incomplete; relaunch to resume)"; else stamp "=== FY2022 window night DONE"; fi
