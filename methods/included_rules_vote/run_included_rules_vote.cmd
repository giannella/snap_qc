@echo off
rem Included rules as voting rules (2026-11-12; Eric: "included rules are voting rules"). Held-out test of letting
rem a workbook's list rules vote, in the map setting (FY2022-23 rules, FY2024 cases) and the year-to-year setting
rem (FY2017-19 + FY2022 rules, FY2023 and FY2024 cases). The two list-flag steps take about 25 and 35 minutes and
rem can run side by side; the vote step takes 2 minutes. Logs beside this file.
cd /d C:\Users\ericg\snap_qc_v27
set R="C:\Program Files\R\R-4.5.1\bin\Rscript.exe"
set R_MAX_VSIZE=10G
set O=methods\included_rules_vote
%R% %O%\votes_cases_y2y.R > %O%\1_votes_cases_y2y.log 2>&1 || exit /b 1
%R% %O%\list_flags_f1109.R > %O%\2_list_flags_f1109.log 2>&1 || exit /b 2
%R% %O%\list_flags_y2y.R > %O%\3_list_flags_y2y.log 2>&1 || exit /b 3
%R% %O%\score_variants.R > %O%\4_score_variants.log 2>&1 || exit /b 4
