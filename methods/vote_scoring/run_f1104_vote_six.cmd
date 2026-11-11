@echo off
rem Rebuild of the six states that failed or needed the override on 2026-11-05/06, after setting
rem prior_year_tables in states.py for CT IL NM RI TX VA (Eric, 2026-11-06). No validation override.
set SNAP_LIST_DIR=methods/f1104_lists
set SNAP_VOTE_RULES_CSV=C:\Users\ericg\snap_qc_v27\methods\vote_scoring\deploy\vote_rules_fy2022_2024_char.csv
set SNAP_ALLOW_VALIDATION_MISMATCH=
set PY="C:\Users\ericg\AppData\Local\Python\pythoncore-3.14-64\python.exe"
set L=C:\Users\ericg\snap_qc_v27\methods\vote_scoring\deploy\logs\vote_six
if not exist %L% mkdir %L%
del /q %L%\*.txt %L%\*.log 2>nul
cd /d C:\Users\ericg\snap_qc_v27\methods\excel_rules_for_states
for %%s in (CT IL NM RI TX VA) do (
  echo %%s start %time% >> %L%\progress.txt
  %PY% make_vote_state.py %%s > %L%\workbook_%%s.log 2>&1
  if errorlevel 1 (echo %%s >> %L%\FAILED_STATES.txt) else (echo %%s done >> %L%\progress.txt)
)
echo done %date% %time% > %L%\SIX_DONE.txt
if exist %L%\FAILED_STATES.txt type %L%\FAILED_STATES.txt >> %L%\SIX_DONE.txt
