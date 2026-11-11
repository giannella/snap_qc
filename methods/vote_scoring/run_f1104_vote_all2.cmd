@echo off
rem Vote workbooks on f1104 for the 48 states other than DC (Eric, 2026-11-05, second run; the first was
rem cancelled at Mississippi to add the changes below). DC was rebuilt alone as the test of the changes.
rem Changes from run_f1104_vote_all.cmd: rule names ("Rule 12 (vote 4.013)", "Vote rule 4.060"), the vote
rem rules with characterization columns (vote_rules_fy2022_2024_char.csv, characterize_vote_rules.py), and
rem 200 sampled FY2024 cases on Step 5.1, "no flags" in Expected error $ by case (relaunched 22:15 after
rem the old 22:00 run collided with this one). Frame exports: the Oct 7 caches were deleted beforehand and the
rem ones rebuilt on f1104 today are reused. CT and IL run last with SNAP_ALLOW_VALIDATION_MISMATCH=1
rem (Eric, 2026-11-05: "override CT and IL but diagnose them"; unc_rawben_rel_max matched 99.4%/99.5%).
rem Logs: methods\vote_scoring\deploy\logs\vote_all\  End: VOTE_ALL_DONE.txt (lists failures).
set SNAP_LIST_DIR=methods/f1104_lists
set SNAP_VOTE_RULES_CSV=C:\Users\ericg\snap_qc_v27\methods\vote_scoring\deploy\vote_rules_fy2022_2024_char.csv
set SNAP_ALLOW_VALIDATION_MISMATCH=
set PY="C:\Users\ericg\AppData\Local\Python\pythoncore-3.14-64\python.exe"
set L=C:\Users\ericg\snap_qc_v27\methods\vote_scoring\deploy\logs\vote_all
if not exist %L% mkdir %L%
del /q %L%\VOTE_ALL_DONE.txt %L%\FAILED_STATES.txt 2>nul
echo started %date% %time% > %L%\STARTED.txt
cd /d C:\Users\ericg\snap_qc_v27\methods\excel_rules_for_states
for %%s in (AL AZ AR CA CO DE FL GA ID IN IA KS KY LA ME MD MA MN MS MO MT NE NV NH NJ NM NY NC ND OH OK OR PA RI SC SD TN TX UT VT VA WV WI WY WA MI) do (
  call :one %%s
)
set SNAP_ALLOW_VALIDATION_MISMATCH=1
call :one CT
call :one IL
set SNAP_ALLOW_VALIDATION_MISMATCH=
echo done %date% %time% > %L%\VOTE_ALL_DONE.txt
if exist %L%\FAILED_STATES.txt type %L%\FAILED_STATES.txt >> %L%\VOTE_ALL_DONE.txt
goto :eof
:one
echo %1 start %time% >> %L%\progress.txt
%PY% make_vote_state.py %1 > %L%\workbook_%1.log 2>&1
if errorlevel 1 (echo %1 >> %L%\FAILED_STATES.txt) else (echo %1 done %time% >> %L%\progress.txt)
goto :eof
