@echo off
rem The workbook vote score (WB) on f1109, test 3, by state, for the results map. Eric, 2026-11-11; scheduled for 14:30.
rem 1. WB's test-3 scorer from the existing f1109 FY2017-19 pools (seeds 117-119); stops unless it reproduces the
rem    shipped vote_table.csv. Run first because it is short and catches setup errors early.
rem 2. A national FY2022-23 mine on f1109 at seed 117 (about 30-60 min).
rem 3. Votes on FY2024 from those rules; expected dollars; by-state table beside B_f1109_by_state_5_10pct.csv.
rem Progress: out_f1109_wb\phases.txt. Ends with DONE.txt, or FAILED.txt naming the step.
cd /d C:\Users\ericg\snap_qc_v27
set SR_ROOT=C:/Users/ericg/snap_qc_v27
set SR_FRAME=f1109
set S46_OUT=out_f1109_wb
set S46_SMOKE=
set SMOKE=
set SR_SEED=
set R="C:\Program Files\R\R-4.5.1\bin\Rscript.exe"
set O=C:\Users\ericg\snap_qc_v27\methods\case_predictions\s46\out_f1109_wb
if not exist %O% mkdir %O%
echo %date% %time% start >> %O%\phases.txt

echo %date% %time% wb votes fy2022 (scorer) >> %O%\phases.txt
set SR_WINDOW=fy2022
%R% methods/case_predictions/s46/wb_ref_f1109.R votes > %O%\1_wb_votes_fy2022.log 2>&1 || (echo wb votes fy2022 > %O%\FAILED.txt & exit /b 1)

echo %date% %time% mine fy2024 (FY2022-23) seed 117 >> %O%\phases.txt
set SR_WINDOW=fy2024
set SR_SEED=117
%R% methods/v270_seed_replicates/mine_national_seed.R > %O%\2_mine_fy2024_seed117.log 2>&1 || (echo mine fy2024 > %O%\FAILED.txt & exit /b 2)
set SR_SEED=

echo %date% %time% wb votes fy2024 >> %O%\phases.txt
%R% methods/case_predictions/s46/wb_ref_f1109.R votes > %O%\3_wb_votes_fy2024.log 2>&1 || (echo wb votes fy2024 > %O%\FAILED.txt & exit /b 3)

echo %date% %time% wb score >> %O%\phases.txt
set SR_WINDOW=fy2022
%R% methods/case_predictions/s46/wb_ref_f1109.R score > %O%\4_wb_score.log 2>&1 || (echo wb score > %O%\FAILED.txt & exit /b 4)

echo %date% %time% by state >> %O%\phases.txt
cd /d C:\Users\ericg\snap_qc_v27\methods\case_predictions\s46
%R% s46_f1109_wb_states.R > %O%\5_states.log 2>&1 || (echo by state > %O%\FAILED.txt & exit /b 5)

echo %date% %time% done >> %O%\phases.txt
echo %date% %time% > %O%\DONE.txt
