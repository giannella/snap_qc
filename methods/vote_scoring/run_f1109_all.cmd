@echo off
rem Vote workbooks on f1109 for all 49 states (Eric, 2026-11-10: "rebuild all the workbooks for all states"),
rem after the day's workbook changes: AMTERR ERROR_AMOUNT column; missing EXPEDITED / HOMELESS_FLAG /
rem MONTHS_SINCE_CERT left blank; benefit-check rows after the deployed rules, live on Step 2 except the
rem SAMPLE- rows, national figures in the description; Step 5.2.b columns H, M, R widened.
rem A state's f1104 workbooks are copied to custom_one_off\<state>\f1104\ only if that folder does not exist
rem yet (states built earlier today already have it; a second copy would overwrite it with f1109 files).
rem Each state's frame export is rebuilt. One python call per state, so a failure stops only that state.
rem Logs: methods\vote_scoring\deploy\logs\vote_f1109\all\  End: ALL_DONE.txt (lists failures).
set SNAP_LIST_DIR=methods/f1109_lists
set SNAP_VOTE_RULES_CSV=C:\Users\ericg\snap_qc_v27\methods\vote_scoring\deploy\vote_rules_fy2022_2024.csv
set PY="C:\Users\ericg\AppData\Local\Python\pythoncore-3.14-64\python.exe"
set L=C:\Users\ericg\snap_qc_v27\methods\vote_scoring\deploy\logs\vote_f1109\all
set FR=C:\Users\ericg\snap_qc_v27\methods\excel_rules_for_states\.frames
set C=C:\Users\ericg\snap_qc_v27\custom_one_off
if not exist %L% mkdir %L%
del /q %L%\ALL_DONE.txt %L%\FAILED_STATES.txt 2>nul
echo started %date% %time% > %L%\STARTED.txt
cd /d C:\Users\ericg\snap_qc_v27\methods\excel_rules_for_states
call :one AL alabama
call :one AZ arizona
call :one AR arkansas
call :one CA california
call :one CO colorado
call :one CT connecticut
call :one DE delaware
call :one DC district_of_columbia
call :one FL florida
call :one GA georgia
call :one ID idaho
call :one IL illinois
call :one IN indiana
call :one IA iowa
call :one KS kansas
call :one KY kentucky
call :one LA louisiana
call :one ME maine
call :one MD maryland
call :one MA massachusetts
call :one MI michigan
call :one MN minnesota
call :one MS mississippi
call :one MO missouri
call :one MT montana
call :one NE nebraska
call :one NV nevada
call :one NH new_hampshire
call :one NJ new_jersey
call :one NM new_mexico
call :one NY new_york
call :one NC north_carolina
call :one ND north_dakota
call :one OH ohio
call :one OK oklahoma
call :one OR oregon
call :one PA pennsylvania
call :one RI rhode_island
call :one SC south_carolina
call :one SD south_dakota
call :one TN tennessee
call :one TX texas
call :one UT utah
call :one VT vermont
call :one VA virginia
call :one WA washington
call :one WV west_virginia
call :one WI wisconsin
call :one WY wyoming
echo done %date% %time% > %L%\ALL_DONE.txt
if exist %L%\FAILED_STATES.txt type %L%\FAILED_STATES.txt >> %L%\ALL_DONE.txt
goto :eof
:one
if exist %C%\%2\~$* (echo %1 skipped: open in Excel >> %L%\FAILED_STATES.txt & goto :eof)
if not exist %C%\%2\f1104 (mkdir %C%\%2\f1104 & copy /y %C%\%2\*_vote.xls* %C%\%2\f1104\ > nul)
del /q %FR%\%1_frame.csv 2>nul
echo %1 start %time% >> %L%\progress.txt
%PY% make_vote_state.py %1 > %L%\workbook_%1.log 2>&1
if errorlevel 1 (echo %1 >> %L%\FAILED_STATES.txt) else (echo %1 done %time% >> %L%\progress.txt)
goto :eof
