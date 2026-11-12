@echo off
rem v2.8.1 workbooks (Eric, 2026-11-12: "let's just make it 2.8.1. Rebuild the workbooks when you're ready"):
rem all 49 vote workbooks rebuilt with the included rules voting (Step 3 Vote? follows Include? for list rules)
rem and "Start Here" leading with the 5.2.b ranking (findings section 52). Same inputs as the v2.8.0 build
rem (run_f1109_all.cmd): the f1109 lists in state_delivery_lists, the characterized FY2022-24 vote rules, and the
rem f1109 frame exports in excel_rules_for_states\.frames (written 2026-11-10 by that build, after the frame file;
rem reused, not re-exported). Each state's v2.8.0 workbooks are first copied to custom_one_off\<state>\v2.8.0\
rem unless that folder exists. One python call per state, so a failure stops only that state.
rem Logs: methods\vote_scoring\deploy\logs\vote_f1109\v281\  End: ALL_DONE.txt (lists failures).
set SNAP_LIST_DIR=state_delivery_lists
set SNAP_VOTE_RULES_CSV=C:\Users\ericg\snap_qc_v27\methods\vote_scoring\deploy\vote_rules_fy2022_2024_char.csv
set SNAP_VOTE_DEST_DIR=
set PY="C:\Users\ericg\AppData\Local\Python\pythoncore-3.14-64\python.exe"
set L=C:\Users\ericg\snap_qc_v27\methods\vote_scoring\deploy\logs\vote_f1109\v281
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
if not exist %C%\%2\v2.8.0 (mkdir %C%\%2\v2.8.0 & copy /y %C%\%2\*_vote.xls* %C%\%2\v2.8.0\ > nul)
echo %1 start %time% >> %L%\progress.txt
%PY% make_vote_state.py %1 > %L%\workbook_%1.log 2>&1
if errorlevel 1 (echo %1 >> %L%\FAILED_STATES.txt) else (echo %1 done %time% >> %L%\progress.txt)
goto :eof
