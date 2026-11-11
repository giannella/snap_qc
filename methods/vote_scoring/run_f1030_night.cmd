@echo off
rem Everything re-mined on the staging frame f1030 (sha256 e3714039; Ben's
rem reconstruction rewrite merged at 6a63a4d; Eric, 2026-10-30), on staging-v2.7
rem only. Copied from run_f1027_night.cmd; also refreshes the WA/DC/MI frame exports:
rem   1. the 196 delivery lists, fresh FY2022-24 mine (runners\run_f1030_build.R),
rem      shipped into this worktree's state_delivery_lists\
rem   2. the vote rules: one national FY2022-24 mine at seed 117
rem   3. the vote table: FY2017-19 mines at seeds 117-119, fitted on FY2022,
rem      checked on FY2023 (deploy_table.R)
rem   4. the 300 vote rules exported from step 2
rem   5. the WA, DC and MI vote workbooks rebuilt on steps 1, 3 and 4
rem Steps 2-4 run even if step 1 fails; step 5 needs all of them.
rem Logs: methods\vote_scoring\deploy\logs\  End: ALL_DONE.txt or FAILED.txt there.
cd /d C:\Users\ericg\snap_qc_v27
set SMOKE=
set SR_ROOT=C:/Users/ericg/snap_qc_v27
set SR_FRAME=f1030
set R="C:\Program Files\R\R-4.5.1\bin\Rscript.exe"
set L=C:\Users\ericg\snap_qc_v27\methods\vote_scoring\deploy\logs
if not exist %L% mkdir %L%
del /q %L%\ALL_DONE.txt %L%\FAILED.txt %L%\LISTS_FAILED.txt 2>nul
echo started %date% %time% > %L%\STARTED.txt

%R% runners\run_f1030_build.R > %L%\f1030_lists.log 2>&1
if errorlevel 1 echo lists failed > %L%\LISTS_FAILED.txt

set SR_WINDOW=deploy
set SR_SEED=117
%R% methods\v270_seed_replicates\mine_national_seed.R > %L%\mine_deploy_f1030_seed117.log 2>&1
if errorlevel 1 goto failed

set SR_WINDOW=fy2022
set SR_SEED=117
%R% methods\v270_seed_replicates\mine_national_seed.R > %L%\mine_fy2022_f1030_seed117.log 2>&1
if errorlevel 1 goto failed
set SR_SEED=118
%R% methods\v270_seed_replicates\mine_national_seed.R > %L%\mine_fy2022_f1030_seed118.log 2>&1
if errorlevel 1 goto failed
set SR_SEED=119
%R% methods\v270_seed_replicates\mine_national_seed.R > %L%\mine_fy2022_f1030_seed119.log 2>&1
if errorlevel 1 goto failed
set SR_SEED=

%R% methods\vote_scoring\deploy_table.R > %L%\deploy_table_f1030.log 2>&1
if errorlevel 1 goto failed

set SR_WINDOW=deploy
%R% methods\vote_scoring\export_vote_rules.R > %L%\export_vote_rules_f1030.log 2>&1
if errorlevel 1 goto failed

if exist %L%\LISTS_FAILED.txt goto failed
rem the workbook build caches each state's frame export; rebuild them on f1030
del /q C:\Users\ericg\snap_qc_v27\methods\excel_rules_for_states\.frames\wa_frame.csv C:\Users\ericg\snap_qc_v27\methods\excel_rules_for_states\.frames\dc_frame.csv C:\Users\ericg\snap_qc_v27\methods\excel_rules_for_states\.frames\mi_frame.csv 2>nul
set SNAP_VOTE_RULES_CSV=C:\Users\ericg\snap_qc_v27\methods\vote_scoring\deploy\vote_rules_fy2022_2024.csv
cd /d C:\Users\ericg\snap_qc_v27\methods\excel_rules_for_states
"C:\Users\ericg\AppData\Local\Python\pythoncore-3.14-64\python.exe" make_vote_state.py WA DC MI > %L%\workbooks_f1030.log 2>&1
if errorlevel 1 goto failed
echo done %date% %time% > %L%\ALL_DONE.txt
goto :eof
:failed
echo failed %date% %time% > %L%\FAILED.txt
