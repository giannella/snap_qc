@echo off
rem Held-out test of the rule lists on f1109 (runners/run_f1109_list_bench.R): stage 1 bench (~4 h), stage 2 gate. 2026-11-11.
cd /d C:\Users\ericg\snap_qc_v27
set R="C:\Program Files\R\R-4.5.1\bin\Rscript.exe"
set O=C:\Users\ericg\snap_qc_v27\methods\f1109_list_bench
set SMOKE=
set SEL_STATES=
set SEL_TAG=
echo %date% %time% start >> %O%\phases.txt
echo %date% %time% bench >> %O%\phases.txt
set BENCH_STAGE=bench
%R% runners/run_f1109_list_bench.R > %O%\1_bench.log 2>&1 || (echo bench > %O%\FAILED.txt & exit /b 1)
echo %date% %time% gate >> %O%\phases.txt
set BENCH_STAGE=gate
%R% runners/run_f1109_list_bench.R > %O%\2_gate.log 2>&1 || (echo gate > %O%\FAILED.txt & exit /b 2)
echo %date% %time% done >> %O%\phases.txt
echo %date% %time% > %O%\DONE.txt
