# Design note: the 2026-10-16 frame vs the v2.7 final frame, one year ahead

A technical measurement of a data revision, not a shipping test; no adoption
bar is set. Requested by the project lead on 2026-10-16 ("frame rebuild
tonight, performance numbers using FY22-23 train and FY24 test"). Chain:
`run_f1016.sh`; readout: `readout_f1016.R` -> `readout_f1016.md`.

## 1. The question

On FY2022-23 mined and FY2024 scored, do the national-only state lists catch
more or fewer FY2024 errors on the frame rebuilt with Ben's 2026-10-16
commits than on the v2.7 final frame, beyond what a change of seed moves?

## 2. What varies, what is held fixed

| Component | Setting |
|---|---|
| **Frame (varies)** | v2.7 final frame (`reg_model_data.rds` of 2026-09-29, hash 1dbe8733) vs the 2026-10-16 rebuild on staging-v2.7 b53f340. The new frame bundles Ben's commits efcc378..4557479: lookup tables corrected against the FY2016-24 QC Technical Documentation (`state_sua.csv` lower value on mid-year changes, 17 state-years, and FY2027 rounded down; standard medical deductions for IA, NH, GA; FY2016/2020 error thresholds; FY2024 element and nature codes), the Illinois FY2024 standard-deduction offset (7 -> 4), and two reconstruction fixes (no excess shelter deduction with the homeless shelter deduction; child support counted only where the file allowed it). This run does not separate them. |
| Seed (replicate, paired) | 117, 118, 119 on the new frame; the final frame's three pools at the same seeds are on record (2026-10-02 run, `states_final_seed11*.csv`). Same-seed pairs. |
| Mining years / test year | FY2022-23 / FY2024 |
| Engines | xgboost 1000 rounds, eta 0.02, subsample 0.20; ranger 1000 trees, mtry 2; depth 4 |
| Vocabulary | the 19 features of the shipped lists, `utilities_sua` included |
| Strata | household size 1 / 2-3 / 4+ |
| Admission / ordering | one BH pass at FDR 10% vs the stratum base rate and n >= 30 / 99% Wilson LCB |
| Artifact handling | the mismatch-share tag at 0.25, tagged rules dropped before the walk (as in the final-frame run; the 2026-10-07 income-pile gate is NOT applied, so both arms match) |
| State lists | national pool only, the shipped fill walk (fresh-share 0.50, buffer to 3x), 5% and 10% budgets |

Code is the same as the final-frame run: `rule_mining_helpers.R` has gained
only an additive 25-line pile helper since 2026-10-02, and the study scripts
gained an `SR_ROOT` switch and the `f1016` frame entry (absolute frame paths).
The 2026-10-02 run showed seed-117 pools reproduce exactly at a fixed seed,
so the final frame is not re-mined.

## 3. Support

Nothing is split beyond the shipped recipe. `frame_check_f1016.R` stops the
chain unless the new frame has the same 231,619 cases in the same order with
identical error flags, so these counts carry over:

- Training: 76,031 rows, 8,397 errors (FY2022-23). Size 1: 45,165 rows /
  3,423 errors; size 2-3: 20,162 / 2,898; size 4+: 10,704 / 2,076.
- Test: 39,528 rows, 4,764 errors (FY2024).
- State walks: each state's own FY2022-23 rows (about 1,550) for the fill
  and its FY2024 rows (about 800) for scoring. The median state's 5% list is
  about 42 FY2024 cases (binomial SE about 0.07 on one state's precision);
  pooled over 49 states about 1,950 flags at 5% (SE about 0.011) and 3,930
  at 10% (about 0.007).

## 4. What the record already says

- Ledger row 119 (§41): the three refinements between the 09-25 and final
  frames each moved errors caught at 10% by less than a seed change (within-
  frame seed ranges of 5-57 errors), with no consistent sign. Final-frame
  10% pooled precision by seed: 0.2913 / 0.2967 / 0.2985; 5%: 0.3391 /
  0.3412 / 0.3391. A data revision of this size is expected to land inside
  that range; the readout says so if it does.
- Ledger row 118 (§41): one year ahead is not the primary window; three years
  ahead (FY2017-19 -> FY2022) matches deployment. This run is one year ahead
  only, at the project lead's request; a three-years-ahead run would need
  FY2017-19 mines (~2.4 h each).
- Ledger row 40 (§31): budget-depth lists are seed-unstable (errors-caught
  overlap 0.53 at 5%, 0.67 at 10%), so per-state changes are read against the
  final frame's own seed-to-seed paired changes (readout section 3).
- Known constraints (munging): a change to `state_sua.csv` changes what
  `utilities_sua` rules mean; the shipped lists and workbooks were walked on
  the final frame's tiers. This run measures the new frame; it rebuilds no
  delivery list or workbook.
- Ledger row 114 (§38): the residual mismatch rows (571-572 in FY2022-24)
  are an error magnet; the frame check reports whether the reconstruction
  fixes move that count.

Readout carries the shipping companions (median, mean, harmed / helped
tails at 0.05), pooled precision and errors caught, dollar recall, states
below base rate, and the top-200 / 1,000 / 3,000 rule precision on FY2024.
The target is any error, so frame-relative and any-error precision coincide.
