#!/bin/bash
# §46 Amendment 2 night run (modeling_findings_detailed.md, §46 "Amendment 2"). One job at a time.
# Phases log to out_a2/run_a2_phases.txt; R output to out_a2/run_a2_stdout.txt.
# The s46 chain stops on its first failure. The mines and wb_ref.R (WB only) record a failure and skip WB.
unset S46_SMOKE SMOKE SR_SEED
export S46_OUT=out_a2 SR_ROOT=C:/Users/ericg/snap_qc_v27 SR_FRAME=f1030
ROOT=/c/Users/ericg/snap_qc_v27; S46=$ROOT/methods/case_predictions/s46; O=$S46/out_a2
R="/c/Program Files/R/R-4.5.1/bin/Rscript.exe"
mkdir -p $O
ph() { echo "=== $(date '+%F %T') $*" >> $O/run_a2_phases.txt; }
run() { ph "start $1"; shift; if "$@" >> $O/run_a2_stdout.txt 2>&1; then ph "done"; return 0; else ph "FAILED"; return 1; fi; }
cd $S46
run "a2 setup" "$R" s46_a2.R setup || exit 1
run "freeze" "$R" s46_run.R freeze || exit 1
cd $ROOT
WB=ok
run "mine fy2019 (FY2017-18) seed 117" env SR_WINDOW=fy2019 SR_SEED=117 "$R" methods/v270_seed_replicates/mine_national_seed.R || WB=no
run "mine fy2024 (FY2022-23) seed 117" env SR_WINDOW=fy2024 SR_SEED=117 "$R" methods/v270_seed_replicates/mine_national_seed.R || WB=no
if [ $WB = ok ]; then
  for w in fy2019 fy2022 fy2024; do run "wb votes $w" env SR_WINDOW=$w "$R" $S46/wb_ref.R votes || { WB=no; break; }; done
fi
[ $WB = ok ] && run "wb score" env SR_WINDOW=fy2022 "$R" $S46/wb_ref.R score
cd $S46
run "a2 tuneB" "$R" s46_a2.R tuneB || exit 1
run "stage1" "$R" s46_run.R stage1 || exit 1
run "freeze2" "$R" s46_run.R freeze2 || exit 1
run "fit all" "$R" s46_run.R fit A,B,G,C,GC,E,D,F || exit 1
run "score" "$R" s46_run.R score || exit 1
run "a2 extra (B0, WB)" "$R" s46_a2.R extra
if grep -q "choice B (" $O/s46_log.txt; then run "deploy B" "$R" s46_post.R deploy
else ph "deploy skipped: the rerun did not choose B (waits for Eric)"; fi
ph "RUN A2 COMPLETE"
