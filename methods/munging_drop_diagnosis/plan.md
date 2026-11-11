# Why did the vote score fall on the f1030 frame? Diagnosis plan

Agreed with Eric 2026-10-31. Staging only, uncommitted.

## What fell

The FY2023 vote check (`methods/vote_scoring/deploy_table.R`: rules mined on
FY2017-19, curve and size model fitted on FY2022, scored on FY2023, top 5% and
10% of each state's cases):

| | f1027 (eb989fda) | f1030 (e3714039) |
|---|---|---|
| Vote score $ per flagged case, 5%, seeds 117-119 | 68.2-71.6 | 58.6-61.8 |
| Vote score precision, 5% | 0.316-0.319 | 0.261-0.275 |
| Simple ordering $ per flagged case, 5% | 53.5-54.3 | 52.4-54.6 |
| Gain over simple ordering, 5% | 28-34% | 7-18% |

"Simple ordering" is issued benefit x the household-size group's training
error rate, not a list. It held steady, so the loss is in the vote score.

In-sample (FY2022-24), the shipped blended lists lost less: median state
precision 0.271 vs 0.285 at 5%, 0.234 vs 0.241 at 10%.

## What changed between the frames

Ben's 27 recreation commits, ec4d190..2570391 (merged at 6a63a4d). On FY2022-24:

- Labels: 26 error -> clean, 32 clean -> error, of 13,164 errors.
- Inputs of error cases changed far more than those of clean cases:
  total_deductions_by_hh_size 30% vs 4%, unc_rawben_rel_max 29% vs 4%,
  gross_by_hh_size 23% vs 3%, shelter 15% vs 2%, earned 13% vs 2%,
  unearned 11% vs 2%, rawben_rel_max 11% vs 1%, utilities_sua 4%,
  HH_size_n 4%.

Main hypothesis: the smoothing commits (02ca411 leftover small dollars,
419044a values stuck at the max/min benefit, 5f6690a small raises from zero,
2570391 rent at the shelter cap) removed marks that recreation left on error
cases. A state's own pre-review data has no such marks, so if the old rules
leaned on them, the old figures were inflated (as in exp106) and f1030 is the
honest one. The alternative: some change made recreated values less like real
pre-review values, which would be a real loss.

## Steps

1. **Old vote rules on the new frame (2x2).** Rule sets (the 300 allotted
   rules from the f1027 or the f1030 FY2017-19 pools, seeds 117-119) crossed
   with data (f1027 or f1030 for curve fit on FY2022 and check on FY2023).
   Rules-driven: f1027 rules hold up on f1030 data. Data-driven: both rule sets
   fall on f1030 data. Script: `vote_cross.R`.
2. **xgboost probe on both frames.** The miners' xgboost settings, per
   household-size group, VOCAB19; FY2017-19 -> FY2023 (the vote check's years)
   and FY2022-23 -> FY2024. National top 5% / 10% and within-state top 5% / 10%.
   If the probe falls like the vote score, it stands in for mining in steps
   3-4. Script: `xgb_probe.R`.
3. **Feature swaps.** f1030 with one changed feature (or group) restored to
   its f1027 values, scored by the probe (or by mined rules if step 2 shows
   the probe does not move).
4. **Commit-group bisection.** Rebuild the frame after each group of Ben's
   commits (about 4 min per munging run), probe each:
   - nature 97/26 and direction (5a5a312, 64d8bd4, 0f9b028, 91c2c53, f91a6ce)
   - elements 2-9: HH size, utilities, homeless (8783d49, cd40e2f, a4d3f82,
     f3b3668, 22d3af2, 7e2e22d, 819430f)
   - multi-element (aea4033, 7fd983a)
   - element 150 nature 13 / excluded members (6fc15d5, 0dff35a, a8cdf05, fe47028)
   - 520/361 and medical deductions (95ef77a, db2dfda)
   - smoothing (02ca411, 419044a, 5f6690a, 2570391)
   - plumbing (0aad422, 44a6033, da765e1, ed7e932, 6006cba): should not move results
5. **Artifact check.** On each frame, how well do recreation marks alone
   (exact cap values, small leftovers, rent at the shelter cap, rawben_rel_max
   exactly 1) separate error from clean cases, and how much of the old rules'
   precision came from cases with those marks.

Order: 1 and 2 first (decide whether 3-4 can use the probe), then 3 and 4,
with 5 alongside to interpret.

## Separate, after the diagnosis: model vs blended lists per state

Plan drafted 2026-10-31; Eric wants to review the modeling choices before it
starts. FY2022-23 training for both sides (blended lists re-mined on FY2022-23
following `methods/anyerror_blended_holdout_2024`), each state's FY2024 cases,
top 5% and 10% per state. Arms: stack and xgboost alone, each by P(error) and by
expected $; blended list; national-only list. Per-state precision, $ per flag,
dollar recall; median state, states better/worse, paired bootstrap CIs.
Pre-register as a findings section before running.

## Results

(filled in as steps finish)

### Step 1: vote rules x frames (2026-10-31, vote_cross.csv; mean of seeds 117-119)

| Rules from | Data from | Precision 5% | $/flag 5% | Gain 5% | Gain 10% |
|---|---|---|---|---|---|
| f1027 | f1027 | 0.318 | 70.5 | 31% | 22% |
| f1027 | f1030 | 0.243 | 55.4 | 3% | 2% |
| f1030 | f1027 | 0.264 | 60.4 | 12% | 6% |
| f1030 | f1030 | 0.267 | 60.5 | 12% | 7% |

The f1027 cell reproduces the shipped check (68-72). The old rules hold up
only on old data; on f1030 data they fall below the new rules. The new rules
score the same on either frame. The drop is data-driven, and the same pattern
as the probe: f1027 carried something, in mining and test years alike, that
the old rules used and f1030 lacks.

### Step 5: recreation marks (2026-10-31, artifacts_*.csv; FY2022-24, base 11.4%)

| Mark | f1027: share of errors / clean | f1027 error rate with mark | f1030: share of errors / clean | f1030 error rate with mark |
|---|---|---|---|---|
| uncapped benefit at the max (within $1) | 3.3% / 1.1% | 27.5% | 0.4% / 0.5% | 9.4% |
| uncapped benefit at the min, 1-2 persons | 1.6% / 0.6% | 26.3% | 0.1% / 0.2% | 7.3% |
| shelter at the cap (within $5) | 0.2% / 0.2% | 13.1% | 0.2% / 0.2% | 12.1% |
| an amount of $1-25 | 2.3% / 2.6% | 10.2% | 2.7% / 2.4% | 12.8% |

(The "recreated" note is set on every row, so it is dropped.)

- The two limit marks were real artifacts in f1027 (errors 2.4x the base rate),
  and Ben's smoothing (419044a) removed them. The leftover-amount and
  shelter-cap marks carried no signal in either frame.
- But they explain little of the drop. In f1027, 12% of the probe's top-5%
  errors carry a mark (7% of all errors). On the 37,552 FY2024 cases with no
  mark on either frame, the gap stays: AUC 0.763 vs 0.733, top-5% precision
  0.369 vs 0.324, $/flag 81.0 vs 62.5.
- So whatever f1027 carried is broader than values sitting on a limit. Steps 3
  and 4 say where.

### Step 2: xgboost probe (2026-10-31, xgb_probe.csv)

| Window | Trained on | Scored on | AUC | Nat. precision 5% | Nat. $/flag 5% | Nat. precision 10% |
|---|---|---|---|---|---|---|
| FY2017-19 -> FY2023 | f1027 | f1027 | 0.756 | 0.353 | 69.2 | 0.307 |
| | f1027 | f1030 | 0.689 | 0.272 | 50.8 | 0.255 |
| | f1030 | f1027 | 0.721 | 0.295 | 58.0 | 0.272 |
| | f1030 | f1030 | 0.717 | 0.299 | 58.4 | 0.274 |
| FY2022-23 -> FY2024 | f1027 | f1027 | 0.762 | 0.391 | 82.2 | 0.333 |
| | f1027 | f1030 | 0.701 | 0.301 | 60.3 | 0.280 |
| | f1030 | f1027 | 0.725 | 0.321 | 61.3 | 0.293 |
| | f1030 | f1030 | 0.725 | 0.331 | 64.7 | 0.299 |

- The probe falls as the vote score did (AUC -0.04; top-5% precision -5 to -6
  points; $/flag -11 to -18), so it can stand in for mining in steps 3-4.
- A model trained on f1030 scores the same on either frame's test data. A
  model trained on f1027 does well only on f1027 data and worst of all on f1030
  data. So f1027 carried a pattern, in both its training and test years, that
  separates errors and that f1030 no longer has. Recreation touches mainly
  error cases, so a regularity it leaves behind is a function of the label;
  that favors the artifact reading, but step 5 is needed to show it.

### Step 3: feature swaps (2026-10-31, feature_swaps.csv; FY2022-23 -> FY2024, seeds 117-119)

Changed inputs (VOCAB19): HH_size_n, total_deductions_by_hh_size,
rawben_rel_max, medical_deductions, shelter_expenses_by_hh_size, utilities_sua,
percent_abawd, unc_rawben_rel_max, count_divisible_by_100, gross_by_hh_size,
earned_by_hh_size, unearned_by_hh_size. f1027 has 3 FY2022-24 rows that f1030
lacks; they are dropped.

| Arm | AUC | Precision 5% | $/flag 5% |
|---|---|---|---|
| f1027 | 0.760 | 0.386 | 80.7 |
| f1030 | 0.725 | 0.329 | 64.7 |
| f1030 with the f1027 label | 0.725 | 0.328 | 63.9 |
| f1027 with the f1030 label | 0.759 | 0.378 | 79.1 |
| f1030 with all 12 changed inputs from f1027 | 0.765 | 0.414 | 88.6 |
| f1027 with all 12 changed inputs from f1030 | 0.731 | 0.357 | 71.9 |
| f1030 with f1027 unc_rawben_rel_max only | 0.778 | 0.462 | 102.5 |
| f1027 with f1030 unc_rawben_rel_max only | 0.770 | 0.456 | 106.3 |

- The label changes explain none of the drop. The 12 changed inputs, taken
  together, explain all of it: each frame's inputs carry their own score
  regardless of which label goes with them.
- Single-feature and group swaps cannot be read. Splicing one frame's value
  of a feature into the other leaves it out of line with the inputs it is
  computed alongside (income, deductions, benefit ratio), and since
  recreation changed mostly error cases, the mismatch itself marks errors. So
  both directions of a swap beat both frames (0.46 against 0.39 and 0.33).
  The swaps that involve the benefit ratios or gross income inflate most.
- Replaced by step 3b: drop each changed input from both frames and see
  whether the gap closes (`drop_features.R`). Step 4 (bisection) has no such
  problem, since each checkpoint frame is built whole.

### Step 3b: drop each changed input from both frames (2026-10-31, drop_features.csv)

| Dropped | Precision 5%, f1027 / f1030 | Gap | $/flag 5%, f1027 / f1030 | Gap |
|---|---|---|---|---|
| nothing | 0.386 / 0.329 | 5.7 pts | 80.7 / 64.7 | $16.0 |
| unc_rawben_rel_max | 0.351 / 0.332 | 1.9 | 72.1 / 65.2 | $6.9 |
| both benefit ratios | 0.342 / 0.323 | 1.9 | 71.6 / 64.8 | $6.8 |
| income (earned, unearned, gross) | 0.370 / 0.327 | 4.2 | 78.0 / 67.3 | $10.7 |
| income and both ratios | 0.307 / 0.298 | 0.9 | 65.3 / 62.9 | $2.4 |
| deductions (4 inputs) | 0.374 / 0.310 | 6.4 | 80.7 / 65.7 | $15.0 |
| any other single input | | 5.0-6.0 | | $13.7-17.7 |

- One input carries most of the gap: the uncapped benefit relative to the
  maximum (unc_rawben_rel_max). Dropping it costs f1027 3.5 points and
  f1030 nothing (0.332 vs 0.329), and the gap falls by two thirds. In f1030
  it adds no information beyond the other inputs; in f1027 it did.
- Income carries most of the rest. Without income and the two ratios the
  frames differ by under a point and $2.4 per flag.
- Deductions, household size and utilities carry none of it.
- The limit marks of step 5 (uncapped benefit exactly at the max or min)
  explained only 12% of f1027's top-5% errors, so the old uncapped benefit
  told errors apart in a broader way than sitting on a limit. Step 4 should
  name the commit that took it away.

### Step 4: bisection over Ben's commits (2026-10-31, bisect_probe.csv; FY2022-23 -> FY2024, seeds 117-119)

Frames rebuilt in the detached worktree `C:/Users/ericg/snap_qc_bisect`. The
rebuild at ec4d190 reproduces f1027 (eb989fda) and the rebuild at 2570391
reproduces f1030 (e3714039), so the builds are deterministic.

| Checkpoint | Adds | AUC | Precision 5% | $/flag 5% |
|---|---|---|---|---|
| ec4d190 | start (f1027) | 0.761 | 0.390 | 81.0 |
| 0f9b028 | nature 97/26, direction codes | 0.759 | 0.384 | 79.9 |
| 0aad422 | elements 2-9 household size and utilities, refactor | 0.759 | 0.375 | 78.1 |
| 7e2e22d | homeless, utilities data fix, more household-size elements | 0.759 | 0.378 | 78.4 |
| 0dff35a | element 150 nature 13, multi-element method | 0.760 | 0.376 | 79.4 |
| 95ef77a | 97 post-recreation, SMD, excluded members, tie-break, 520/361 | 0.758 | 0.376 | — |
| 02ca411 | no leftover amounts | 0.759 | 0.375 | 79.0 |
| **419044a** | **smoothing at the max/min benefit** | **0.725** | **0.328** | **64.5** |
| 5f6690a | no small raises from zero | 0.725 | 0.327 | 64.0 |
| 2570391 | rent at the shelter cap (f1030) | 0.725 | 0.329 | 64.7 |

The first nine commits together cost about 1.5 points. All of the rest of the
drop comes from one commit, 419044a.

What 419044a does (`at_419044a.R`). Step 7 recreation moves one input at a
time until the recreated benefit reaches the issued benefit. When the issued
benefit is the maximum allotment (or the minimum), it stops at the first value
that reaches the limit, so these cases pile up with an uncapped benefit right
on the limit. Before 419044a the frame rescaled some of them to the mean of
unchanged cases and set large cuts to $0. 419044a instead redraws the
last-moved input from donor cases with the same correction that were not on a
limit, far enough past the stop point to leave it; the benefit itself cannot
change. It redrew 1,670-1,950 cases a year (1,472 / 1,672 / 1,755 at the
maximum and about 200 at the minimum in FY2022-24); the inputs of 5,116 changed, 2,400 over-tolerance
errors and 2,716 under-tolerance cases.

| Uncapped benefit / maximum | Before 419044a: share of errors / clean, error rate | After |
|---|---|---|
| 0.995-1.005 | 4.2% / 1.3%, 30.1% | 0.7% / 0.7%, 11.8% |
| 1.005-1.10 | 9.8% / 8.2%, 13.3% | 7.8% / 8.4%, 10.7% |

On the 37,717 FY2024 cases the commit left alone, the frames before and after
it score the same: AUC 0.748 vs 0.744, top-5% precision 0.293 vs 0.300, $/flag
55.0 vs 56.5. The whole drop is in the redrawn cases.

## Conclusion

The precision drop is the removal of a recreation artifact, not a loss of
real signal. Before 419044a, error cases issued the maximum or minimum benefit
had an uncapped benefit sitting on the limit, because that is where the
recreation stopped. A state's own pre-review data would not show that: the
agency's inputs for a case issued the maximum put its uncapped benefit
wherever they put it, with no pile at the limit. The models found the pile
(the step 5 marks at exactly $1 caught only part of it; the 0.5% band around
the maximum held 4.2% of errors, at a 30% error rate) and the old figures
counted it as skill. This is the same kind of inflation as exp106 in the other
repo, smaller in size.

So the f1030 figures are the honest ones: a vote-score gain of 7-18% over
simple ordering at 5%, not 28-34%, and the re-mined lists' lower in-sample
precision. Figures produced on f1027 (the shipped vote check, the f1027 list
evaluations) overstate what a state would
see, by roughly 5-6 points of top-5% precision. The donor redraw is itself a
guess at where these cases' inputs were, but it removes the pile, and the
cases it left alone show no change. Main's frame (1dbe8733) predates Ben's
commits and very likely carries the same pile, as would results built on it;
not yet checked.
