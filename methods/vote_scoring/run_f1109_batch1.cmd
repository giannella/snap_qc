@echo off
rem Vote workbooks on f1109, first batch of 18 states after the DC/MI/WV samples (Eric, 2026-11-10: "whatever
rem set you can do in the next hour"). Same build as run_f1109_night.cmd step 7, with the AMTERR ERROR_AMOUNT
rem column. CT is left out (its f1104 build failed Excel verification). Each state's f1104 workbooks are copied
rem to custom_one_off\<state>\f1104\ first. One python call per state, so a failure stops only that state.
rem Logs: methods\vote_scoring\deploy\logs\vote_f1109\  End: BATCH1_DONE.txt (lists failures).
set SNAP_LIST_DIR=methods/f1109_lists
set SNAP_VOTE_RULES_CSV=C:\Users\ericg\snap_qc_v27\methods\vote_scoring\deploy\vote_rules_fy2022_2024.csv
set PY="C:\Users\ericg\AppData\Local\Python\pythoncore-3.14-64\python.exe"
set L=C:\Users\ericg\snap_qc_v27\methods\vote_scoring\deploy\logs\vote_f1109
set FR=C:\Users\ericg\snap_qc_v27\methods\excel_rules_for_states\.frames
set C=C:\Users\ericg\snap_qc_v27\custom_one_off
if not exist %L% mkdir %L%
del /q %L%\BATCH1_DONE.txt %L%\FAILED_STATES.txt 2>nul
echo started %date% %time% > %L%\STARTED.txt
cd /d C:\Users\ericg\snap_qc_v27\methods\excel_rules_for_states
call :one AL alabama
call :one AZ arizona
call :one AR arkansas
call :one CA california
call :one CO colorado
call :one DE delaware
call :one FL florida
call :one GA georgia
call :one ID idaho
call :one IN indiana
call :one IA iowa
call :one KS kansas
call :one KY kentucky
call :one LA louisiana
call :one ME maine
call :one MD maryland
call :one MA massachusetts
call :one MN minnesota
echo done %date% %time% > %L%\BATCH1_DONE.txt
if exist %L%\FAILED_STATES.txt type %L%\FAILED_STATES.txt >> %L%\BATCH1_DONE.txt
goto :eof
:one
if exist %C%\%2\~$* (echo %1 skipped: open in Excel >> %L%\FAILED_STATES.txt & goto :eof)
if not exist %C%\%2\f1104 mkdir %C%\%2\f1104
copy /y %C%\%2\*_vote.xls* %C%\%2\f1104\ > nul
del /q %FR%\%1_frame.csv 2>nul
echo %1 start %time% >> %L%\progress.txt
%PY% make_vote_state.py %1 > %L%\workbook_%1.log 2>&1
if errorlevel 1 (echo %1 >> %L%\FAILED_STATES.txt) else (echo %1 done %time% >> %L%\progress.txt)
goto :eof
