@echo off
rem Vote workbooks on f1104 for the 46 states other than WA, DC and MI (Eric, 2026-11-05).
rem Same build as WA/DC/MI in run_f1104_resume.cmd: lists from methods\f1104_lists (SNAP_LIST_DIR),
rem the f1104 vote rules, each state's frame export rebuilt on f1104. One python call per state, so a
rem failure stops only that state. Output: custom_one_off\<state>\SNAP_flagging_rules_<ABBR>_vote.xlsx/.xlsb
rem (gitignored). Logs: methods\vote_scoring\deploy\logs\vote_all\  End: VOTE_ALL_DONE.txt (lists failures).
set SNAP_LIST_DIR=methods/f1104_lists
set SNAP_VOTE_RULES_CSV=C:\Users\ericg\snap_qc_v27\methods\vote_scoring\deploy\vote_rules_fy2022_2024.csv
set PY="C:\Users\ericg\AppData\Local\Python\pythoncore-3.14-64\python.exe"
set L=C:\Users\ericg\snap_qc_v27\methods\vote_scoring\deploy\logs\vote_all
set FR=C:\Users\ericg\snap_qc_v27\methods\excel_rules_for_states\.frames
if not exist %L% mkdir %L%
del /q %L%\VOTE_ALL_DONE.txt %L%\FAILED_STATES.txt 2>nul
echo started %date% %time% > %L%\STARTED.txt
cd /d C:\Users\ericg\snap_qc_v27\methods\excel_rules_for_states
for %%s in (AL AZ AR CA CO CT DE FL GA ID IL IN IA KS KY LA ME MD MA MN MS MO MT NE NV NH NJ NM NY NC ND OH OK OR PA RI SC SD TN TX UT VT VA WV WI WY) do (
  call :one %%s
)
echo done %date% %time% > %L%\VOTE_ALL_DONE.txt
if exist %L%\FAILED_STATES.txt type %L%\FAILED_STATES.txt >> %L%\VOTE_ALL_DONE.txt
goto :eof
:one
del /q %FR%\%1_frame.csv 2>nul
echo %1 start %time% >> %L%\progress.txt
%PY% make_vote_state.py %1 > %L%\workbook_%1.log 2>&1
if errorlevel 1 (echo %1 >> %L%\FAILED_STATES.txt) else (echo %1 done %time% >> %L%\progress.txt)
goto :eof
