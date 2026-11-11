#!/bin/sh
# Smoke test of the f1030 chain (40-tree models; outputs under */smoke/).
cd /c/Users/ericg/snap_qc_v27
R="/c/Program Files/R/R-4.5.1/bin/Rscript.exe"
L=methods/vote_scoring/deploy/logs
export SMOKE=1 SR_ROOT=C:/Users/ericg/snap_qc_v27 SR_FRAME=f1030
SR_WINDOW=deploy SR_SEED=117 "$R" methods/v270_seed_replicates/mine_national_seed.R > $L/smoke_mine_deploy.log 2>&1 || { echo FAIL mine deploy; exit 1; }
for sd in 117 118 119; do
  SR_WINDOW=fy2022 SR_SEED=$sd "$R" methods/v270_seed_replicates/mine_national_seed.R > $L/smoke_mine_fy2022_$sd.log 2>&1 || { echo FAIL mine fy2022 $sd; exit 1; }
done
SR_WINDOW=fy2022 "$R" methods/vote_scoring/deploy_table.R > $L/smoke_table.log 2>&1 || { echo FAIL table; exit 1; }
SR_WINDOW=deploy "$R" methods/vote_scoring/export_vote_rules.R > $L/smoke_export.log 2>&1 || { echo FAIL export; exit 1; }
echo SMOKE VOTE OK
