@echo off
rem Vote-scoring study, exploration window: national FY2017-18 mines on the
rem v2.7 frame (1dbe8733) at seeds 117-119, one after another (~2.4 h each).
rem Pools: methods\vote_scoring\fy2019\pools\  Logs: methods\vote_scoring\fy2019\logs\
rem See methods\vote_scoring\design_note.md.
cd /d C:\Users\ericg\snap_qc
set SMOKE=
set SR_WINDOW=fy2019
set SR_FRAME=final
if not exist methods\vote_scoring\fy2019\logs mkdir methods\vote_scoring\fy2019\logs
for %%S in (117 118 119) do (
  set SR_SEED=%%S
  "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" methods\v270_seed_replicates\mine_national_seed.R > methods\vote_scoring\fy2019\logs\mine_final_seed%%S.log 2>&1
)
echo done > methods\vote_scoring\fy2019\logs\ALL_DONE.txt
