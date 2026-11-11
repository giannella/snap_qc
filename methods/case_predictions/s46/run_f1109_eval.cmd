@echo off
rem B's performance on f1109 (s46_f1109.R setup, fit, score; then s46_f1109_states.R). Eric, 2026-11-10.
cd /d C:\Users\ericg\snap_qc_v27\methods\case_predictions\s46
set SR_ROOT=C:/Users/ericg/snap_qc_v27
set S46_OUT=out_f1109_eval
set R="C:\Program Files\R\R-4.5.1\bin\Rscript.exe"
if not exist out_f1109_eval mkdir out_f1109_eval
%R% s46_f1109.R setup > out_f1109_eval_setup.log 2>&1 || exit /b 1
%R% s46_f1109.R fit > out_f1109_eval_fit.log 2>&1 || exit /b 2
%R% s46_f1109.R score > out_f1109_eval_score.log 2>&1 || exit /b 3
%R% s46_f1109_states.R > out_f1109_eval_states.log 2>&1 || exit /b 4
