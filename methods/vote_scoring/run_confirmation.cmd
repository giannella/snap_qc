@echo off
rem Vote-scoring study, confirmation: three years ahead (FY2017-19 rules -> FY2022,
rem decides) and one year ahead (FY2022-23 rules -> FY2024, must agree), seeds
rem 117-119, then the readout. Specification frozen in design_note.md
rem ("Exploration result", 2026-10-28). Logs: methods\vote_scoring\confirm\logs\
cd /d C:\Users\ericg\snap_qc
set VS_SMOKE=
set SMOKE=
set SR_FRAME=final
if not exist methods\vote_scoring\confirm\logs mkdir methods\vote_scoring\confirm\logs
for %%W in (fy2022 fy2024) do (
  set SR_WINDOW=%%W
  "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" methods\vote_scoring\confirm_window.R > methods\vote_scoring\confirm\logs\confirm_%%W.log 2>&1
)
"C:\Program Files\R\R-4.5.1\bin\Rscript.exe" methods\vote_scoring\confirm_readout.R > methods\vote_scoring\confirm\logs\readout.log 2>&1
echo done > methods\vote_scoring\confirm\logs\ALL_DONE.txt
