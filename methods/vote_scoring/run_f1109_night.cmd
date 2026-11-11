@echo off
rem Everything re-mined on the staging frame f1109 (Ben's 2026-11-09 munging:
rem over_threshold and total_error_amount from AMTERR, nature 97 / agency 26
rem flagged not removed; staging 8b8d99f; Eric, 2026-11-09), on staging-v2.7
rem only. Copied from run_f1104_night.cmd, with these changes: no probe; the B
rem case-prediction bundle is refitted (step 5); the benefit check's national
rem estimate is recomputed (step 6); and only sample workbooks are built (DC,
rem MI, WV; WA is open in Excel), so they can be looked at before all 49 are
rem rebuilt. prorated_i is not used anywhere (Eric, 2026-11-09).
rem   0. archive the f1104 deploy CSVs and the DC/MI/WV vote workbooks
rem   1. the 196 delivery lists, fresh FY2022-24 mine (runners\run_f1109_build.R)
rem   2. the vote rules: one national FY2022-24 mine at seed 117
rem   3. the vote table: FY2017-19 mines at seeds 117-119, fitted on FY2022,
rem      checked on FY2023 (deploy_table.R)
rem   4. the 300 vote rules exported from step 2
rem   5. B's deploy bundle refitted on f1109 (s46_deploy_f1109.R), then checked
rem   6. the benefit check's national estimate (benefit_check_national.R)
rem   7. the DC, MI and WV vote workbooks rebuilt on steps 1, 3, 4 and 6
rem Steps 2-6 run even if step 1 fails; step 7 needs 1-4 and 6. A failed step 5
rem is marked B_FAILED.txt and does not stop the chain.
rem Lists are NOT shipped into state_delivery_lists\ (they stay in
rem methods\f1109_lists\, read by the workbooks through SNAP_LIST_DIR).
rem Logs: methods\vote_scoring\deploy\logs\  End: ALL_DONE.txt or FAILED.txt there.
cd /d C:\Users\ericg\snap_qc_v27
set SMOKE=
set SR_ROOT=C:/Users/ericg/snap_qc_v27
set SR_FRAME=f1109
set V250_HEAD_GATE=report
set R="C:\Program Files\R\R-4.5.1\bin\Rscript.exe"
set L=C:\Users\ericg\snap_qc_v27\methods\vote_scoring\deploy\logs
if not exist %L% mkdir %L%
del /q %L%\ALL_DONE.txt %L%\FAILED.txt %L%\LISTS_FAILED.txt %L%\B_FAILED.txt 2>nul
echo started %date% %time% > %L%\STARTED.txt

set A=C:\Users\ericg\snap_qc_v27\methods\vote_scoring\deploy\v270_f1104
if exist %A% goto failed
mkdir %A%
copy /y C:\Users\ericg\snap_qc_v27\methods\vote_scoring\deploy\*.csv %A%\ > nul
if errorlevel 1 goto failed
rem the vote-rule characterization is not rebuilt tonight; its f1104 copy is in the archive
if exist %A%\vote_rules_fy2022_2024_char.csv del /q C:\Users\ericg\snap_qc_v27\methods\vote_scoring\deploy\vote_rules_fy2022_2024_char.csv
for %%s in (district_of_columbia michigan west_virginia) do (
  if not exist C:\Users\ericg\snap_qc_v27\custom_one_off\%%s\f1104 mkdir C:\Users\ericg\snap_qc_v27\custom_one_off\%%s\f1104
  copy /y C:\Users\ericg\snap_qc_v27\custom_one_off\%%s\*_vote.xls* C:\Users\ericg\snap_qc_v27\custom_one_off\%%s\f1104\ > nul
)

%R% runners\run_f1109_build.R > %L%\f1109_lists.log 2>&1
if errorlevel 1 echo lists failed > %L%\LISTS_FAILED.txt

set SR_WINDOW=deploy
set SR_SEED=117
%R% methods\v270_seed_replicates\mine_national_seed.R > %L%\mine_deploy_f1109_seed117.log 2>&1
if errorlevel 1 goto failed

set SR_WINDOW=fy2022
set SR_SEED=117
%R% methods\v270_seed_replicates\mine_national_seed.R > %L%\mine_fy2022_f1109_seed117.log 2>&1
if errorlevel 1 goto failed
set SR_SEED=118
%R% methods\v270_seed_replicates\mine_national_seed.R > %L%\mine_fy2022_f1109_seed118.log 2>&1
if errorlevel 1 goto failed
set SR_SEED=119
%R% methods\v270_seed_replicates\mine_national_seed.R > %L%\mine_fy2022_f1109_seed119.log 2>&1
if errorlevel 1 goto failed
set SR_SEED=

%R% methods\vote_scoring\deploy_table.R > %L%\deploy_table_f1109.log 2>&1
if errorlevel 1 goto failed

set SR_WINDOW=deploy
%R% methods\vote_scoring\export_vote_rules.R > %L%\export_vote_rules_f1109.log 2>&1
if errorlevel 1 goto failed

set S46_OUT=out_f1109
%R% methods\case_predictions\s46\s46_deploy_f1109.R > %L%\s46_deploy_f1109.log 2>&1
if errorlevel 1 (echo B refit failed > %L%\B_FAILED.txt) else (
  set PC_MODE=check
  set PC_BUNDLE=methods/case_predictions/s46/out_f1109/deploy_B_bundle.rds
  %R% methods\case_predictions\predict_cases.R > %L%\predict_check_f1109.log 2>&1
  if errorlevel 1 echo B check failed > %L%\B_FAILED.txt
)
set S46_OUT=
set PC_MODE=
set PC_BUNDLE=

%R% methods\excel_rules_for_states\benefit_check_national.R > %L%\benefit_check_f1109.log 2>&1
if errorlevel 1 goto failed

if exist %L%\LISTS_FAILED.txt goto failed
rem the workbook build caches each state's frame export; rebuild them on f1109
del /q C:\Users\ericg\snap_qc_v27\methods\excel_rules_for_states\.frames\dc_frame.csv C:\Users\ericg\snap_qc_v27\methods\excel_rules_for_states\.frames\mi_frame.csv C:\Users\ericg\snap_qc_v27\methods\excel_rules_for_states\.frames\wv_frame.csv 2>nul
set SNAP_LIST_DIR=methods/f1109_lists
set SNAP_VOTE_RULES_CSV=C:\Users\ericg\snap_qc_v27\methods\vote_scoring\deploy\vote_rules_fy2022_2024.csv
cd /d C:\Users\ericg\snap_qc_v27\methods\excel_rules_for_states
"C:\Users\ericg\AppData\Local\Python\pythoncore-3.14-64\python.exe" make_vote_state.py DC MI WV > %L%\workbooks_f1109.log 2>&1
if errorlevel 1 goto failed
echo done %date% %time% > %L%\ALL_DONE.txt
goto :eof
:failed
echo failed %date% %time% > %L%\FAILED.txt
