@echo off
rem Resume of run_f1104_night.cmd after the list build halted at the head check (2026-11-05).
rem Eric's ruling (ledger section 47): V250_HEAD_GATE=report, tagged artifact rules still dropped.
rem Steps 2-4 (vote mines, vote table, vote rules) finished on f1104 and are NOT rerun; the
rem archive step is NOT rerun (it would overwrite deploy\v270_f1030\ with the f1104 tables).
rem   1. the 196 delivery lists from the cached f1104 national pool (runners\run_f1104_build.R)
rem   5. the WA, DC and MI vote workbooks
cd /d C:\Users\ericg\snap_qc_v27
set SMOKE=
set SR_ROOT=C:/Users/ericg/snap_qc_v27
set SR_FRAME=f1104
set V250_HEAD_GATE=report
set R="C:\Program Files\R\R-4.5.1\bin\Rscript.exe"
set L=C:\Users\ericg\snap_qc_v27\methods\vote_scoring\deploy\logs
del /q %L%\ALL_DONE.txt %L%\FAILED.txt %L%\LISTS_FAILED.txt 2>nul
echo resumed %date% %time% > %L%\RESUMED.txt
if exist %L%\f1104_lists.log move /y %L%\f1104_lists.log %L%\f1104_lists_halted.log > nul
%R% runners\run_f1104_build.R > %L%\f1104_lists.log 2>&1
if errorlevel 1 goto failed
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
