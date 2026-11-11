@echo off
rem Everything re-mined on the staging frame f1104 (Ben's donor matching,
rem ben/state-options to 7d9fe2d, merged at cc051b4; Eric, 2026-11-04), on
rem staging-v2.7 only. Copied from run_f1030_night.cmd, with two changes: the
rem lists are NOT shipped into state_delivery_lists\ (they stay in
rem methods\f1104_lists\ and the workbooks read them there through
rem SNAP_LIST_DIR), and the f1030 vote table and WA/DC/MI vote workbooks are
rem archived first.
rem   0. archive the f1030 deploy CSVs and vote workbooks; xgboost probe f1030 vs f1104
rem   1. the 196 delivery lists, fresh FY2022-24 mine (runners\run_f1104_build.R)
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
set SR_FRAME=f1104
set V250_HEAD_GATE=report
set R="C:\Program Files\R\R-4.5.1\bin\Rscript.exe"
set L=C:\Users\ericg\snap_qc_v27\methods\vote_scoring\deploy\logs
if not exist %L% mkdir %L%
del /q %L%\ALL_DONE.txt %L%\FAILED.txt %L%\LISTS_FAILED.txt 2>nul
echo started %date% %time% > %L%\STARTED.txt

set A=C:\Users\ericg\snap_qc_v27\methods\vote_scoring\deploy\v270_f1030
if not exist %A% mkdir %A%
copy /y C:\Users\ericg\snap_qc_v27\methods\vote_scoring\deploy\*.csv %A%\ > nul
if errorlevel 1 goto failed
for %%s in (washington district_of_columbia michigan) do (
  if not exist C:\Users\ericg\snap_qc_v27\custom_one_off\%%s\f1030 mkdir C:\Users\ericg\snap_qc_v27\custom_one_off\%%s\f1030
  copy /y C:\Users\ericg\snap_qc_v27\custom_one_off\%%s\*_vote.xls* C:\Users\ericg\snap_qc_v27\custom_one_off\%%s\f1030\ > nul
)
%R% methods\v270_seed_replicates\probe_f1104.R > %L%\probe_f1104.log 2>&1

%R% runners\run_f1104_build.R > %L%\f1104_lists.log 2>&1
if errorlevel 1 echo lists failed > %L%\LISTS_FAILED.txt

set SR_WINDOW=deploy
set SR_SEED=117
%R% methods\v270_seed_replicates\mine_national_seed.R > %L%\mine_deploy_f1104_seed117.log 2>&1
if errorlevel 1 goto failed

set SR_WINDOW=fy2022
set SR_SEED=117
%R% methods\v270_seed_replicates\mine_national_seed.R > %L%\mine_fy2022_f1104_seed117.log 2>&1
if errorlevel 1 goto failed
set SR_SEED=118
%R% methods\v270_seed_replicates\mine_national_seed.R > %L%\mine_fy2022_f1104_seed118.log 2>&1
if errorlevel 1 goto failed
set SR_SEED=119
%R% methods\v270_seed_replicates\mine_national_seed.R > %L%\mine_fy2022_f1104_seed119.log 2>&1
if errorlevel 1 goto failed
set SR_SEED=

%R% methods\vote_scoring\deploy_table.R > %L%\deploy_table_f1104.log 2>&1
if errorlevel 1 goto failed

set SR_WINDOW=deploy
%R% methods\vote_scoring\export_vote_rules.R > %L%\export_vote_rules_f1104.log 2>&1
if errorlevel 1 goto failed

if exist %L%\LISTS_FAILED.txt goto failed
rem the workbook build caches each state's frame export; rebuild them on f1104
del /q C:\Users\ericg\snap_qc_v27\methods\excel_rules_for_states\.frames\wa_frame.csv C:\Users\ericg\snap_qc_v27\methods\excel_rules_for_states\.frames\dc_frame.csv C:\Users\ericg\snap_qc_v27\methods\excel_rules_for_states\.frames\mi_frame.csv 2>nul
set SNAP_LIST_DIR=methods/f1104_lists
set SNAP_VOTE_RULES_CSV=C:\Users\ericg\snap_qc_v27\methods\vote_scoring\deploy\vote_rules_fy2022_2024.csv
cd /d C:\Users\ericg\snap_qc_v27\methods\excel_rules_for_states
"C:\Users\ericg\AppData\Local\Python\pythoncore-3.14-64\python.exe" make_vote_state.py WA DC MI > %L%\workbooks_f1104.log 2>&1
if errorlevel 1 goto failed
echo done %date% %time% > %L%\ALL_DONE.txt
goto :eof
:failed
echo failed %date% %time% > %L%\FAILED.txt
