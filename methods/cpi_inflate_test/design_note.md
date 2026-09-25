# Design note: one-year-ahead test of `cpi_inflate()` (2026-09-20)

A technical exploration requested by the project lead (test the CPI code on
the staging branch, both train/test windows). It ends in a measurement and a
judgment call. No bars, no verdict language, no ledger row from this run.

**The code under test.** Commit ccdd682 (ben/state-options), merged into
`staging/cpi-inflate`. `cpi_inflate()` in `features.R` multiplies six raw
fields (`rawearn`, `rawunearn`, `rawmedded`, `rawdepded`, `rawcsded`,
`rawrent`) by CPI(target year) / CPI(calendar year of the review month),
floors the result, and leaves `rawmedded` alone where it equals the state's
standard medical deduction (`smd_amt`). The munging script applies it after
the `final.rds` checkpoint and after `add_features()`, with
`modeling_target_year` (2026 as committed).

Which inflated fields reach a mined feature (read from the munging script;
confirmed by `frame_diff.R` before launch). Four of the 19 mined features
change: `earned_by_hh_size`, `unearned_by_hh_size`, `medical_deductions`,
and `shelter_expenses_by_hh_size` (the rent part only; utilities stay
nominal). `rawdepded` and `rawcsded` reach no mined feature, because
`total_deductions` is summed before the CPI step. `gross_by_hh_size` comes
from `rawgrinc`, which is not on the inflated list, so on the CPI frame
gross income stays nominal while its two components are inflated (gross no
longer equals earned plus unearned). The by-field readout therefore covers
four features, and rules on the two dollar features left nominal are a
comparison group but not a clean control: they can stand in for the two
adjusted income features. These are reported to the code author; nothing
is patched inside this study. `floor()` moves a value by under $1.

**Question (one sentence).** When rules are mined on training years whose
six fields are inflated to the test year's dollars, do the rules that
condition on those fields hold up better on the held-out year than the same
recipe mined on nominal dollars, and which fields account for the change?

**What varies, with exactly one component varying.** The frame's CPI step:
`cpi_inflate_vars` FALSE (nominal arm) vs TRUE with
`modeling_target_year` = the test year (2019 for the FY2017-18 -> FY2019
window; 2024 for FY2022-23 -> FY2024). Both frames come from the staging
branch's munging script, unmodified, through `build_frames.R` (which
overrides only those two assignments). The canonical frame in the main
checkout (built 2026-08-23) is NOT the baseline: the munging script changed
on 2026-08-30 (SMD reconstruction), so the nominal arm is rebuilt from the
same code as the CPI arm. `frame_diff.R` asserts identical rows, row order,
error flags and error dollars between arms and lists every changed column.

Held fixed: rows, error definition, the shipped 19-feature vocabulary
(with `utilities_sua`, the encoding in the delivery lists since 2026-08-23,
as in `methods/v250_build_staged_lists_utilsua_v2.R`; both arms share it, so
this run says nothing about the utilities encoding), engines and
hyperparameters, seed 117, joint BH FDR 10% +
n >= 30 admission, 99% Wilson LCB ordering, the artifact tag (tagged rules
dropped from every readout), the fresh-share walk (0.50), cap-walk scoring.

Two more arms repeat both frames at seed 118 (`nominal_seed2`,
`cpi_seed2`). The frame contrast (CPI minus nominal) is then read four
times, at two seeds in each of two windows, for sign and size (ledger
process rule of 2026-08-09: attribution readouts carry per-arm seed
spread and same-sign counts). The seed contrasts (118 minus 117 on one
frame) show what a re-mine with no design change moves (section 31:
budget-depth lists are not seed-stable; section 39: a same-recipe re-mine
moved 10 states past -0.05 and 7 past +0.05 at the 5% budget). They
overstate the noise in a same-seed frame contrast, where the two arms
share their random draws, so they are an upper reference.

This is the any-error frame: frame-relative precision and any-error
precision are the same number (section 6).

**Scope limit.** National pool only: no state mines, no blend. The
list-level readout is the national-only list per state (the shipped
deliverable for 43 states at 5% / 36 at 10%), not the blended list.

**Support after the split (computed on the staging nominal frame by the
driver's `support_*.csv`; identical in every arm).**

| window | split | rows | errors | hh 1 / 2-3 / 4+ rows | hh 1 / 2-3 / 4+ errors |
|---|---|---|---|---|---|
| FY2017-18 -> FY2019 | train | 77,905 | 7,048 | 41,671 / 23,462 / 12,772 | 2,544 / 2,554 / 1,950 |
| | test | 38,155 | 3,872 | 21,393 / 10,914 / 5,848 | 1,525 / 1,334 / 1,013 |
| FY2022-23 -> FY2024 | train | 76,031 | 8,397 | 45,165 / 20,162 / 10,704 | 3,423 / 2,898 / 2,076 |
| | test | 39,528 | 4,764 | 23,553 / 10,356 / 5,619 | 1,932 / 1,611 / 1,221 |

Every mined unit is a national stratum; the smallest carries 1,950
training errors. No state-scale mining occurs, so the n >= 30 collapse hazard
(ledger, Virginia 2026-07-06) does not arise.

**Size of the adjustment.** CPI ratios to the target year: 2016 -> 2019
1.065, 2017 -> 2019 1.043, 2018 -> 2019 1.018; 2021 -> 2024 1.158,
2022 -> 2024 1.072, 2023 -> 2024 1.030. The key is the calendar year of
the review month, so the October-December cases of each fiscal year take
the prior calendar year's ratio, and the test year's own October-December
cases are inflated too (1.018 in FY2019, 1.030 in FY2024).

**Two limits on interpretation.** (1) Each CPI frame uses the test year's
realized CPI. A deployment targets a year whose CPI is not yet known (the
committed code targets 2026 with a projected value), so this test shows
the adjustment at its best-informed. (2) The adjustment is about twice as
large in the FY2022-23 -> FY2024 window (1.030-1.158) as in the
FY2017-18 -> FY2019 window (1.018-1.065); the two windows are not
equal-size replicates.

**Readouts (descriptive).**
1. Frame diff: which mined features the step changes, share of rows, median
   ratio by fiscal year.
2. Rule level, per window and arm, for admitted untagged rules. AFFECTED
   is defined by effect: a rule is affected when at least one of its
   conditions flags a different set of cases on the CPI frame than on the
   nominal frame (a zero-vs-positive cut flags the same cases on both and
   does not count). Rules that name a CPI-changed feature only through
   such insensitive cuts are their own group. NOT AFFECTED is split:
   conditions on a dollar feature the step leaves nominal / no dollar
   feature. Affected is split: two-sided interval on a CPI-changed
   feature (section 40's fragile class) / one-sided cuts only. Per group:
   count and pool share; median train
   precision, 99% LCB and held-out precision; flag-weighted held-out
   precision; median held-out minus train precision (decay); median
   held-out minus LCB (margin) and the share of rules below their LCB;
   share flagging fewer than 10 held-out cases; median absolute log ratio
   of held-out reach to train reach (reach drift). Scopes: all admitted,
   LCB >= 0.20, top 1,000 by LCB; the all-admitted table is repeated
   within train-n buckets (30-99 / 100-299 / 300+) so a change in which
   rules are admitted is not read as a change in how rules carry over.
   Contrasts: CPI minus nominal at each seed, beside seed 118 minus 117
   on each frame.
3. The same by feature, for each CPI-changed feature (rules with a
   sensitive condition on it; and rules for which it is the only
   sensitive feature).
4. Pool level: union held-out precision / recall / dollar recall by LCB
   floor, arms side by side (an error caught by several rules counts once).
5. List level: national-only list per state at 5% and 10%, paired against
   the reference arm: median, mean, counts above and below zero, count
   below -0.05 and above +0.05, for precision and dollar recall.
6. Sensitivity: each CPI pool is also scored on the nominal frame's
   test-year rows (only the test year's October-December cases differ).

**What the ledger and findings already say.**
- Vocabulary changes have not moved budget-list performance beyond seed
  noise (sections 35-37, exploratory: "different vocabularies re-describe
  the same errors"); the section 35 percentile features were already built
  on CPI-deflated dollars. Expect small list-level differences.
- Narrow dollar intervals are the fragile tail one year ahead (section 40:
  8-36% flag fewer than 10 held-out cases at matched train n); a dollar cut
  that travels with the price level is the mechanism this test probes, as
  the SUA tier did for utilities (ledger, `utilities_sua` row: reach-collapse
  ratio 3.0x -> 0.16x).
- Rank and filter on the LCB, never on held-out results (sections 1, 20,
  22): every held-out number here is a readout, not a selector.
- Seed-to-seed: deep coverage stable, budget-depth lists not (section 31).

**Review.** Fresh-context statistician review 2026-09-20: APPROVE WITH
CHANGES, no recipe or indexing bug found. Applied: effect-based
"affected", the seed-118 CPI arm and four-cell reading, the vocabulary
correction, the two interpretation limits, an explicit resume flag with a
frame-md5 check, the mismatch-row assert, train-n buckets, the ratio-1
floor check in the frame diff, the nominal-test-rows sensitivity.

**Runtime.** Eight arm-runs (2 windows x 4 arms), about 40 min mine + score
and about 20 min of state walks each; run four at a time, about 2-2.5 h
wall. Mining itself (the loud part) is about 3 min per arm.

## Addendum 2026-09-21: a third window, FY2017-19 -> FY2022

Requested by the project lead after the first readout: project rules mined
on FY2017-19 forward to FY2022 with the CPI, against the same rules mined
on recorded dollars, at the same two seeds.

**Question.** With a price adjustment of 14 to 22 percent (against 2-4 and
3-7 percent in the first two windows), does the CPI step change how rules
mined on FY2017-19 perform on FY2022?

**What varies.** As before, only the frame's CPI step
(`modeling_target_year` = 2022). Four arms: nominal and CPI at seeds 117 and
118. Same recipe, same driver (`CPI_ERA=1719_22`), same readout. Two
engineering changes, neither to the recipe: the training window is three
years, and rules are scored per household-size stratum on that stratum's
rows only (`score_by_stratum`), which gives the same counts from a fraction
of the memory; the smoke run asserts identity with the all-rows path.

**What is different about this window, declared before the run.**
- It is a three-year projection across the excluded FY2020-21, not one year
  ahead. Held-out levels will sit below the one-year-ahead windows for
  reasons common to both arms (error threshold $37-38 -> $48, base-rate
  shift, whatever else changed over the gap). Only the paired contrasts
  between arms are read.
- CPI ratios to 2022: calendar 2016 1.220, 2017 1.194, 2018 1.166, 2019
  1.145. The test year's October-December 2021 cases are themselves
  inflated by 1.080 under the calendar-year key, a quarter of the test
  year. In the first two windows that ratio was 1.018-1.029 and scoring on
  the nominal test rows changed nothing; here the same sensitivity readout
  (CPI pools scored on the nominal frame's FY2022 rows) is a primary
  companion, not a footnote.
- FY2022 is a training year of the second window. Nothing from that window
  informs this one (no selection on held-out results anywhere), so reading
  FY2022 here spends no test bed that another study depends on.

**Support (computed by the driver's `support_1719_22_*.csv`).** Train
FY2017-19: 116,060 rows, 10,920 errors (smallest stratum 4+: 18,620 rows,
2,963 errors). Test FY2022: 36,851 rows, 3,985 errors (household size 1 /
2-3 / 4+: 21,930 / 9,729 / 5,192 rows; 1,632 / 1,364 / 989 errors); 9,188
of the 36,851 test rows (24.9%) carry the 1.080 ratio on the CPI frame.

**Pre-run review (fresh-context statistician, 2026-09-21): APPROVE WITH
CHANGES, applied.** `score_by_stratum` asserts no NA and the smoke run
checks identity with the all-rows path on the test call as well as the
train call. Every held-out readout (rule level, bound-threshold union,
state lists) is produced twice for CPI arms: on the CPI frame's test rows
(a state runs its live cases through the step) and on the recorded-dollar
test rows (the as-shipped situation: CPI-mined rules applied to cases as
recorded); for nominal arms the two coincide. List contrasts are also
reported without Louisiana, Virginia, Mississippi and Indiana, whose
`bbce_state_i` differs between FY2017-19 and FY2022 (common to both arms).
Reading rules: paired contrasts only; no dose-response reading across
windows (horizon, training size and test year change together with the
adjustment size); every headline says "with the realized 2022 CPI" and
"the step as committed". FY2017-19 -> FY2022 has now been read once.

**Readouts.** Identical to the first two windows, all tables extended with
this window; the nominal-test-rows sensitivity is reported beside every
held-out number it could move.
