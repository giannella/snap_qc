@echo off
rem Vote score, shipped rules: one national FY2022-24 mine on the v2.7 frame
rem (1dbe8733) at seed 117, then the 300 vote rules exported from it, then the
rem WA, DC and MI vote workbooks rebuilt on those rules. See
rem methods\vote_scoring\design_note.md ("Confirmation result and decision").
rem Pool: methods\vote_scoring\deploy\pools\  Logs: methods\vote_scoring\deploy\logs\
cd /d C:\Users\ericg\snap_qc
set SMOKE=
set SR_WINDOW=deploy
set SR_FRAME=final
set SR_SEED=117
if not exist methods\vote_scoring\deploy\logs mkdir methods\vote_scoring\deploy\logs
"C:\Program Files\R\R-4.5.1\bin\Rscript.exe" methods\v270_seed_replicates\mine_national_seed.R > methods\vote_scoring\deploy\logs\mine_final_seed117.log 2>&1
if errorlevel 1 goto failed
"C:\Program Files\R\R-4.5.1\bin\Rscript.exe" methods\vote_scoring\export_vote_rules.R > methods\vote_scoring\deploy\logs\export_vote_rules.log 2>&1
if errorlevel 1 goto failed
set SNAP_VOTE_RULES_CSV=C:\Users\ericg\snap_qc\methods\vote_scoring\deploy\vote_rules_fy2022_2024.csv
cd /d C:\Users\ericg\snap_qc\methods\excel_rules_for_states
"C:\Users\ericg\AppData\Local\Python\pythoncore-3.14-64\python.exe" make_vote_state.py WA DC MI > C:\Users\ericg\snap_qc\methods\vote_scoring\deploy\logs\workbooks.log 2>&1
if errorlevel 1 goto failed
echo done > C:\Users\ericg\snap_qc\methods\vote_scoring\deploy\logs\ALL_DONE.txt
goto :eof
:failed
echo failed > C:\Users\ericg\snap_qc\methods\vote_scoring\deploy\logs\FAILED.txt
