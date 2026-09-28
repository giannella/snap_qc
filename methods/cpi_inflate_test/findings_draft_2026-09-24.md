# Draft finding: CPI adjustment of the dollar fields (2026-09-24)

DRAFT for project-lead review before it enters `methods/modeling_findings.md`
(next free section) and `methods/modeling_findings_detailed.md`, with a ledger
row. The test was a technical exploration requested 2026-09-20: no bars were
set and none are applied here. The CPI step ships in v2.7 as a
data-processing change, on the judgment that dollar fields in one year's
prices are truer to their era; this record says what the step measurably
did, not whether it earned its place.

**Takeaway (data).** Inflating the training years' dollar fields to the test
year's prices makes rules' dollar cutoffs carry to later years better (the
typical affected rule's reach drift fell 14 to 26 percent in three windows,
at two seeds each), while one-year-ahead precision moved within what a
change of random seed moves. Three years ahead (FY2017-19 to FY2022, the
realized 2022 CPI) the precision gains were of one sign at both seeds and
about twice the seed movement. The one cost, seen in every window: more
rules with few training cases nearly stop firing in the test year.

## What was tested

- **Code.** `cpi_inflate()` in `features.R` at commit ccdd682 (Ben,
  2026-09-08), merged into `staging/cpi-inflate`. It multiplies `rawearn`,
  `rawunearn`, `rawmedded`, `rawdepded`, `rawcsded` and `rawrent` by
  CPI(target year) / CPI(calendar year of the review month), floors the
  result, and leaves a medical deduction equal to the state's standard
  medical deduction unchanged. The munging script applies it after
  `add_features()`. In this test the target year was the test year of each
  window, so training values landed in the test year's prices.
- **Windows.** FY2017-18 mined, FY2019 scored (values raised 2-4%);
  FY2022-23 mined, FY2024 scored (3-7%); and, added 2026-09-21, FY2017-19
  mined, FY2022 scored three years ahead across the excluded FY2020-21
  (14-19%).
- **Arms.** Four per window: nominal frame and CPI frame at seed 117, and
  both again at seed 118. Both frames were rebuilt from the same munging
  code, and `frame_diff.R` asserted identical rows, row order, error flags
  and error dollars. The seed-118 minus seed-117 contrast on one frame is
  the yardstick: it shows what a plain re-mine moves.
- **Held fixed.** The shipped recipe: one national any-error mine per window
  (no state mines), the 19-feature vocabulary with `utilities_sua`, xgboost
  plus ranger, joint BH FDR 10% and n >= 30 admission, 99% Wilson LCB
  ordering, artifact tag, fresh-share walk (0.50), cap-walk scoring.
- **What the step reached.** At ccdd682 four of the 19 mined features
  changed: `earned_by_hh_size`, `unearned_by_hh_size`,
  `medical_deductions`, `shelter_expenses_by_hh_size` (rent only).
  `gross_by_hh_size` and `total_deductions_by_hh_size` stayed nominal
  because they were summed before the step (reported to the author; fixed
  in 51d69ef and 4fa5fcc, which recompute gross income and total deductions
  after the step and add the standard and homeless deductions to the
  total). Rules were split by effect: "affected" if at least one cutoff
  flags different cases on the CPI frame than on the nominal frame (about
  68% of admitted rules), "not affected" otherwise (about 30%).

## Measurements

Reach drift is the median over rules of |ln(test-year reach / training
reach)|. "Under the bound" is the share of rules whose held-out precision
fell below their own 99% LCB. All figures are CPI minus nominal at the same
seed, with the seed-only movement beside them.

| window | affected rules' reach drift, nominal to CPI | seed-only movement | median rule precision change (seeds 117 / 118) | seed-only |
|---|---|---|---|---|
| FY2017-18 to FY2019 | 0.072 to 0.062 | 0.001 or less | +0.0000 / +0.0003 | 0.0004 to 0.0007 |
| FY2022-23 to FY2024 | 0.133 to 0.105 | 0.002 or less | +0.0019 / +0.0009 | 0.0017 to 0.0027 |
| FY2017-19 to FY2022 | 0.155 to 0.115 | 0.004 or less | (see third window) | |

- **Not-affected rules did not move.** No precision measure changed by more
  than 0.0006; 79-84% of them are word for word identical between the
  nominal and CPI runs at the same seed (9% across seeds).
- **Under the bound.** FY2024 window: affected rules under their bound fell
  9.8% to 9.2% (seed 118: 0.46 pt), against seed-only movement of 0.05-0.07
  pt. FY2019 window: no change beyond seed movement. FY2022 window: -0.4 /
  -0.3 pt.
- **Top 1,000 by LCB, flag-weighted precision of affected rules.** FY2019:
  +0.004 / +0.002 (seed-only -0.002 to +0.000). FY2024: +0.008 / +0.006
  (seed-only +0.004 to +0.006). FY2022: +0.006 / +0.009 (seed-only +0.003 to
  +0.006). Same sign in all six comparisons; the size is not separable from
  a re-mine in the one-year windows.
- **National-only lists walked per state, 49 states, 5% and 10% budgets.**
  One year ahead: median paired precision change 0.0000 in six of eight
  cells, mean +0.003 to +0.015 in all eight, harmed (worse than -0.05) 2-7
  states against seed-only harmed tails of 1-13. Three years ahead, scored
  on the test rows as recorded: mean +0.008 to +0.014 in all four cells,
  24-28 states up against 12-19 down, helped 5-11 against harmed 2-5;
  seed-only means -0.002 to +0.004 with helped and harmed about equal.
- **By field.** Unearned income and shelter cost showed the largest and
  steadiest reach-drift reduction (21-27%); earned income none in the
  FY2019 window and 18% in FY2024; medical deductions gained least, with
  37-45% of its nonzero training values exempt (equal to the standard
  medical deduction). At the top of the FY2024 ranking, earned income is
  the one field whose precision gain (+0.008 / +0.011) exceeds seed
  movement; medical deductions lost at both seeds (-0.004 / -0.021).
- **Cost.** Affected rules flagging fewer than 10 test-year cases: 0.2% to
  0.5% (FY2019), 0.4% to 0.6% (FY2024), 1.3% to 2.4% (FY2022). The rise sits
  in rules with 30-99 training cases (FY2022: 13% to 19%); rules with 300 or
  more training cases stayed under 0.1%. Cause not traced.
- **Scoring convention.** The step as tested keyed on the calendar year of
  the review month, so the test year's October-December cases were inflated
  too (1.8-2.9% one year ahead, 8% in the FY2022 window). One year ahead,
  scoring the CPI rule sets on the recorded test rows instead changed nothing
  to four decimals; the FY2022 window reports the as-recorded reading as its
  headline. v2.7 keys the step on the fiscal year (next section).

## Limits

- One national mine per arm, no state pools, so the list-level numbers are
  national-only lists, not the blended deliverable. The v2.7 measurement
  (`methods/v270_cpi_benchmark/`, FY2024 window 2026-09-24 night, FY2019
  window) repeats the contrast with the full blended recipe on the shipped
  v2.7 frame, and is the number that belongs in the evolution record beside
  the v2.5.0 and v2.6.0 benchmarks.
- The three windows differ in horizon, training size and test year at
  once; the larger FY2022 effect is not a dose-response reading.
- The FY2022 window was read once, after the first two, at the project
  lead's request.
- The frame under test predates 51d69ef and 4fa5fcc. The v2.7 frame also
  redefines total deductions, so the v2.7 measurement reads a bundle (CPI
  plus that redefinition), not the CPI step alone.
- Benefits stay nominal by design (`skip_benefits = TRUE` after inflation):
  `rawben_rel_max` and `unc_rawben_rel_max` are unchanged by the step.

## Where the step stands in v2.7

Shipped in v2.7 with `modeling_target_year = 2026`, keyed on each case's
fiscal year: the income, deduction, rent and utility amounts in 2026
dollars, with the 2026 standard deduction, shelter cap, homeless standard,
SUA and standard medical deduction. The two benefit ratios and the SUA tier
stay in each review year's own terms. The state workbooks take pasted
amounts in their review year's dollars and repeat the same step
(`MODELING_YEAR` in `make_input_workbook.py`, FederalTables L3 and K4), so
every workbook feature equals the frame's column; the build's validation
gate checks this on every row.

## Artifacts

- Scripts and design note: `methods/cpi_inflate_test/` (`design_note.md`,
  `build_frames.R`, `frame_diff.R`, `cpi_inflate_oneyear_ahead_v2.R`,
  `readout.R`, the three `run_*.sh`).
- Readouts: `methods/cpi_inflate_test/out/` (`readout.log`,
  `readout_contrasts.csv`, `readout_family*.csv`, `readout_feature.csv`,
  `readout_lists*.csv`, `readout_sweep.csv`, per-arm `support_*`, `sweep_*`,
  `lists_*` and `run_*.log`; the frames and pools are gitignored `.rds`).
- Write-up with the full tables: https://claude.ai/artifact/A5aUsLDP5wdyEtSA5D5gdF
  (2026-09-21).

## Proposed ledger row

| CPI-inflate the training years' dollar fields to the target year's prices (features.R `cpi_inflate`) | reach drift of affected rules down 14-26% in three windows at two seeds; one-year-ahead precision within seed noise; three years ahead, gains of one sign about twice seed movement; more n 30-99 rules go near-silent | SHIPPED IN v2.7 as data processing (project-lead judgment, 2026-09-24); the blended-recipe measurement on the v2.7 frame is pending (methods/v270_cpi_benchmark) | national-only mines, 3 windows x 2 seeds, 49 states walked; exploratory, no bars | this draft; methods/cpi_inflate_test/ |
