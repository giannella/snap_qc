#!/usr/bin/env bash
# Vote-table refit (design note ../design_note_table_refit_2026-11-12.md) and the §52 counting correction
# (../dedup/), 2026-11-12. Two lanes, because a list run peaks near 13 GB:
#   lane 1: refit_lists.R 117, then 119
#   lane 2: refit_lists.R 118, then list_flags_dedup.R map, then y2y
# then refit_fit_score.R and score_dedup.R. Needs refit_votes.R for all three seeds and vote_keys_s52.R for map and
# y2y first. Do not edit any script while this runs: R reads a script from disk as it goes.
#   bash methods/included_rules_vote/refit/run_refit_and_dedup.sh
set -u
cd /c/Users/ericg/snap_qc_v27
R="/c/Program Files/R/R-4.5.1/bin/Rscript.exe"; export R_MAX_VSIZE=10G
F=methods/included_rules_vote/refit; P=methods/included_rules_vote/dedup
for f in $F/votekeys_seed117.rds $F/votekeys_seed118.rds $F/votekeys_seed119.rds $P/votekeys_map.rds $P/votekeys_y2y.rds; do
  [ -f "$f" ] || { echo "missing $f"; exit 1; }
done
stamp() { echo "[$(date '+%H:%M:%S')] $*" | tee -a $F/run_progress.txt; }
lane1() {
  for sd in 117 119; do stamp "lists $sd start"; "$R" $F/refit_lists.R $sd > $F/lists_seed$sd.log 2>&1 || { stamp "lists $sd FAILED"; return 1; }; stamp "lists $sd done"; done
}
lane2() {
  stamp "lists 118 start"; "$R" $F/refit_lists.R 118 > $F/lists_seed118.log 2>&1 || { stamp "lists 118 FAILED"; return 1; }; stamp "lists 118 done"
  for s in map y2y; do stamp "dedup $s start"; "$R" $P/list_flags_dedup.R $s > $P/2_list_flags_dedup_$s.log 2>&1 || { stamp "dedup $s FAILED"; return 1; }; stamp "dedup $s done"; done
}
: > $F/run_progress.txt
lane1 & p1=$!; lane2 & p2=$!
wait $p1; r1=$?; wait $p2; r2=$?
[ $r1 -eq 0 ] && [ $r2 -eq 0 ] || { stamp "a lane failed (lane 1 exit $r1, lane 2 exit $r2)"; exit 1; }
stamp "score_dedup start"; "$R" $P/score_dedup.R > $P/3_score_dedup.log 2>&1 && stamp "score_dedup done" || stamp "score_dedup FAILED"
stamp "fit_score start"; "$R" $F/refit_fit_score.R > $F/fit_score.log 2>&1 && stamp "fit_score done" || stamp "fit_score FAILED"
stamp "ALL DONE"
