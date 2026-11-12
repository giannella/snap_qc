@echo off
rem Overnight chain, 2026-11-11 (Eric). Waits for the f1109 rule-list benchmark to finish (methods\f1109_list_bench\
rem DONE.txt; stops if that run wrote FAILED.txt), then runs one step at a time so no two large R jobs share memory:
rem   0. the f1109 kept-case scoring for the map (runners/run_f1109_list_bench_kept.R). A failure here is recorded in
rem      methods\f1109_list_bench\KEPT_FAILED.txt and does not stop the stability check, which does not use it.
rem   The year-to-year stability check (national rules vs votes; trained FY2017-19 + FY2022, scored FY2023 and FY2024):
rem   1. one national mine, seed 117 (methods/v270_seed_replicates/mine_national_seed.R, SR_WINDOW=stab23)
rem   2. the vote score on both years (votes_stab.R; first reproduces the FY2024 vote file)
rem   3. national rule lists, scored on both years (rules_stab.R). After the votes because the recipe's artifact
rem      gate can stop this step, and the votes do not depend on it.
rem   4. the readout (readout_stab.R)
rem Progress: methods\stability_fy2324\phases.txt. Ends with DONE.txt, or FAILED.txt naming the step.
cd /d C:\Users\ericg\snap_qc_v27
set R="C:\Program Files\R\R-4.5.1\bin\Rscript.exe"
set O=C:\Users\ericg\snap_qc_v27\methods\stability_fy2324
set B=C:\Users\ericg\snap_qc_v27\methods\f1109_list_bench
set SR_ROOT=C:/Users/ericg/snap_qc_v27
set SMOKE=
set SEL_STATES=
set R_MAX_VSIZE=
if not exist %O% mkdir %O%
echo %date% %time% waiting for the f1109 benchmark >> %O%\phases.txt

:wait
if exist %B%\FAILED.txt (echo the f1109 benchmark failed; nothing run > %O%\FAILED.txt & exit /b 9)
if exist %B%\DONE.txt goto go
ping -n 121 127.0.0.1 > nul
goto wait

:go
echo %date% %time% f1109 kept scoring >> %O%\phases.txt
%R% runners/run_f1109_list_bench_kept.R > %B%\3_kept.log 2>&1 || (echo kept scoring failed, see 3_kept.log > %B%\KEPT_FAILED.txt)

echo %date% %time% mine FY2017-19+2022 seed 117 >> %O%\phases.txt
set SR_FRAME=f1109
set SR_SEED=117
set SR_WINDOW=stab23
%R% methods/v270_seed_replicates/mine_national_seed.R > %O%\1_mine.log 2>&1 || (echo mine > %O%\FAILED.txt & exit /b 1)

echo %date% %time% votes >> %O%\phases.txt
%R% methods/stability_fy2324/votes_stab.R > %O%\2_votes.log 2>&1 || (echo votes > %O%\FAILED.txt & exit /b 2)

echo %date% %time% rules >> %O%\phases.txt
%R% methods/stability_fy2324/rules_stab.R > %O%\3_rules.log 2>&1 || (echo rules > %O%\FAILED.txt & exit /b 3)

echo %date% %time% readout >> %O%\phases.txt
%R% methods/stability_fy2324/readout_stab.R > %O%\4_readout.log 2>&1 || (echo readout > %O%\FAILED.txt & exit /b 4)

echo %date% %time% done >> %O%\phases.txt
echo %date% %time% > %O%\DONE.txt
