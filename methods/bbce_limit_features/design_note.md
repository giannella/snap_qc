# Design note: BBCE limit and income-to-limit features, three years ahead

A technical exploration of a feature-set choice, not a research claim: the
output is a measurement and a judgment call, with no bar and no verdict
language (formality rule, findings ledger process notes, 2026-08-11).
Requested by the project lead on 2026-10-22: one seed tonight; three seeds
only if an arm is much better than the shipped features. Chain:
`run_chain.sh`; readout: `readout.R` -> `readout.md`.

## 1. The question

Mined on FY2017-19 and scored on FY2022, do the national-only state lists
catch more FY2022 errors when the forest can split on the state's BBCE gross
income limit, on the household's distance to that limit, on both, or on
income as a percent of poverty together with the limit, than with the
shipped 19 features alone?

## 2. What varies, what is held fixed

| Component | Setting |
|---|---|
| **Features (varies)** | A: the shipped 19 (which already include the BBCE yes/no flag `bbce_state_i`). B: A + `dist_to_limit_pts`. C: A + `bbce_limit_pct`. D: A + both. E: A + `gross_pov_pct` + `bbce_limit_pct`. |
| Engine | ranger alone, 1000 trees, depth 4, mtry 2, min node 20 (the shipped forest settings). The same for every arm, including A, so no arm is compared with the xgboost + ranger pools on record. A winning arm is re-mined with both engines before anything ships. |
| Seed | 117 for every arm. Arm A is also run at seed 118, as the only direct reading of ranger-only seed noise. A shared seed does not pair the arms: a different feature list changes ranger's random draws. |
| Frame | the 2026-10-16 staging-v2.7 frame (`f1016`, hash ba1f9626) |
| Mining years / test year | FY2017-19 / FY2022 (three years ahead, the deployment gap) |
| Strata | household size 1 / 2-3 / 4+ |
| Admission / ordering | one BH pass at FDR 10% against the stratum base rate and n >= 30 / 99% Wilson LCB (the benchmark's `admit_rank`, by name) |
| Artifact handling | the shipped national-only builder's two gates (`methods/build_national_only_lists_v2.R`): the mismatch-share tag at 0.25 and the income-pile tag at 0.25 (a rule with a quarter or more of its training flags or errors on reconstruction income-pile rows); tagged rules dropped before the walk. The pile gate matters here in particular: the one-person pile ($589 / $622 / $663 earned, FY2022-24) sits at 54.6-54.9% of poverty every year, so the new features line the piles up across years. |
| Scoring | every list scored twice on FY2022: on all cases, and on the cases that are not pile cases (427 pile rows with 135 errors removed, the budget a share of the rest), as `methods/reconstruction_income_piles/gate_removal_test.R` does. The non-pile reading stands in for a state's own file, which has no piles. |
| State lists | national pool only, the shipped fill walk (fresh-share 0.50, buffer to 3x), 5% and 10% budgets |

The features, computed in `common.R` from review-year amounts (the frame's
`rawearn_nominal + rawunearn_nominal`; the frame's own dollar columns are
CPI-adjusted to FY2026):

- `gross_pov_pct`: reconstructed gross income as a percent of the HHS poverty
  guideline for the reported unit size (`additional_data/poverty_guidelines.csv`,
  new, 48 states and DC / Alaska / Hawaii, 2016-2026). Fiscal year t uses the
  guideline of calendar year t-1. Paired that way, the QC file's TPOV is
  reproduced within 1 point on 99.96% or more of cases in every year; paired
  with the same calendar year, on 24-73%. On review-year amounts the
  reconstructed ratio matches TPOV within 1 point on 88% of error-free cases
  (TPOV is computed after review).
- `bbce_limit_pct`: the state's BBCE gross income limit that fiscal year, %
  of poverty, from Ben's `additional_data/bbce_income.csv` (the general
  limit; the higher value where it changed mid-year); 130 where the state has
  no BBCE.
- `dist_to_limit_pts` = `gross_pov_pct` - `bbce_limit_pct`.

Known simplifications, kept on purpose: the general limit is used for every
household. In 5 to 8 BBCE states a year, elderly/disabled households have a
higher limit (200%; FY2024: IL, NY, OH, RI, SC; Ben advised against the
elderly/disabled field), and outside BBCE they face no gross test at all; the
trees can pair the features with `elderly_disabled_i`. New Hampshire's BBCE
covers households with children only (the QC file shows 4-10% of its
childless non-elderly cases categorically eligible); one state, left as is.

## 3. Support

Nothing is split beyond the shipped recipe.

- Training (FY2017-19): 116,060 rows, 10,920 errors. Size 1: 63,064 /
  4,069; size 2-3: 34,376 / 3,888; size 4+: 18,620 / 2,963.
- The limit levels in training (rows / errors), size 1 / 2-3 / 4+:
  - no BBCE (28 state-years): 10,851 / 468, 6,510 / 557, 4,100 / 502;
  - 130 (31): 13,547 / 662, 8,945 / 817, 5,229 / 736;
  - 160 (6): 2,752 / 232, 1,391 / 207, 771 / 149;
  - 165 (12): 5,273 / 427, 2,715 / 389, 1,529 / 299;
  - 185 (24): 10,873 / 848, 4,634 / 645, 2,158 / 401;
  - 200 (46): 19,768 / 1,432, 10,181 / 1,273, 4,833 / 876.
  The thinnest cell, a 160% limit in households of 4 or more, holds 149
  errors.
- Within 30 points below the limit or above it: 4,709 / 242, 2,912 / 319,
  1,439 / 203. Income above 130% of poverty: 3,583 / 217, 2,083 / 260,
  500 / 88.
- Test (FY2022): 36,851 rows, 3,985 errors; within 30 points of the limit
  or above, 1,884 / 130, 926 / 155, 465 / 87. State lists: about 1,820
  flags pooled at 5% and 3,660 at 10% (seed-replicate study, this window).
- Income-pile rows: FY2017-19 1,232 rows with 320 errors; FY2022 427 with
  135.
- Seed yardstick in this window: the three-seed range of errors caught on
  xgboost + ranger pools was 25 / 6 (CPI-off frame), 18 / 57 (09-29) and
  21 / 43 (final) at 5% / 10% (`methods/v270_seed_replicates/fy2022/readout.md`);
  none was measured on f1016 or on ranger alone. The thresholds are the
  largest of these, 25 at 5% and 57 at 10%, or arm A's own seed-118 minus
  seed-117 difference if that is larger.

## 4. What the record already says

- Ledger, vocabulary section: `bbce_state_i` replaced `cat_elig` as the
  regime-level BBCE feature (§39); 127 of 2,824 delivered rules use it. The
  v2.5.0 vocabulary is the per-size 19-feature set (§39); the 2026-08
  vocabulary comparisons behind it are exploratory (§§35-37).
- Ledger, engines: the xgboost + ranger pair, depth 4, mtry 2, is settled
  (§2, §4, §13). mtry 2 beat 1 and 4 once (0.223 vs 0.214 and 0.219, 500
  trees, 90% bound); this run keeps mtry 2.
- Ledger, states: similar-state transfer pools are retired (§12, §14). This
  is not that design: the pool stays national and a state-level feature
  only limits a rule's scope, as the elderly/disabled feature did against an
  elderly/disabled stratum (§8).
- Hazard from the regimes plan: a rule limited to a high-limit regime can
  pass admission on that regime's higher base rate alone (FY2017-19 error
  rates: 0.071 with no BBCE, 0.080 at 130, 0.103-0.120 above). `run_arm.R`
  checks every admitted rule with a condition on `bbce_state_i` or
  `bbce_limit_pct` against its own regime's training error rate in its
  household-size group and counts those whose bound does not exceed it,
  overall and among the rules the state lists use. It applies to arm A too
  (`bbce_state_i`).
- Ledger §41 (three years ahead is the deployment window): this run uses it.

## Readout

Each arm against A, paired by state, on all cases and on non-pile cases:
errors caught and pooled precision at 5% and 10%; the within-state median
with the mean and the count of states worse / better by more than 0.05
beside it; mean dollar recall. Two named subgroups: states in minority
regimes in FY2022 (no BBCE, or BBCE at 130%), and states whose limit or BBCE
status in FY2022 differs from some FY2017-19 year (KY, LA, ME, MS, NE, OR,
VA), where a regime-limited rule chosen on training rows can lose its flags.
Rule level: how many visible and top-1,000 rules use each new feature and
their FY2022 precision; the regime check.

**Second night.** A shared seed does not pair the arms, so each difference
from A is a difference of two independent draws, and picking the best of
four arms selects a maximum. The trigger is a screen, not evidence: three
seeds for an arm (then both engines) if it beats A in errors caught on all
cases by more than 25 at 5% and 57 at 10% (or arm A's own seed difference,
if larger), its non-pile change has the same sign at both budgets, its
median and mean changes are positive, and it harms no more states than it
helps. A difference inside the thresholds is no evidence either way.

## Cost

Six ranger-only mines on FY2017-19 (five arms, plus A at seed 118), two
workers, launched at 22:00 with a 06:00 cut-off; a relaunch resumes from the
files on disk. The 10-16 chain's xgboost + ranger mine on FY2022-23 produced
148,694 raw rules, took 29 minutes to score them on the training rows and
30 minutes to walk and score 49 states. Ranger alone produces about half the
rules on 1.5 times the rows, so about 45-50 minutes per arm: about 2.5 hours
of wall time, done near 00:30.

## Night 2 (2026-10-23): mtry 3 and 4, and the light state options

Requested by the project lead on 2026-10-23 after night 1's readout (no arm
passed the screen; `readout.md`). Same frame, window, admission, gates, walk,
scoring and readout conventions as night 1; chain `run_chain_night2.sh`,
readout `readout_night2.R` -> `readout_night2.md`. Night 1's arms A, A at
seed 118, D and E are read from disk as the mtry-2 references.

### 1. The questions

(a) With ranger trying 3 or 4 features at each split instead of 2, do the
limit features (arm D: distance + limit; arm E: income as a percent of
poverty + limit) catch more FY2022 errors than the shipped 19 features at the
same mtry, and than the shipped setting (mtry 2)? The reasoning: at mtry 2 a
split rarely sees both the BBCE flag and the distance, so pairing them is
rare. (b) The regimes plan's Tier 1: do the six light state options, added
together as one arm (T1) at mtry 2, catch more FY2022 errors than arm A?

### 2. What varies, what is held fixed

| Comparison | Varies | Fixed |
|---|---|---|
| A at mtry 3 / 4 vs A at mtry 2 | mtry | the shipped 19 features |
| D, E at mtry m vs A at mtry m (m = 3, 4) | features | mtry |
| D, E at mtry 3 / 4 vs A at mtry 2 | features and mtry together (read as "the candidate setting against what ships", not as either effect alone) | |
| T1 vs A, both mtry 2 | features (eight columns: SMD gap in dollars, SMD flag, ESAP, standard CAP, modified CAP, county administration, self-employment by another method, TBA) | mtry 2 |
| A at mtry 4, seed 118 vs seed 117 | seed | the only seed-noise reading at mtry 4 |

Everything else as night 1: ranger alone, 1000 trees, depth 4, min node 20;
seed 117; FY2017-19 -> FY2022; strata 1 / 2-3 / 4+; BH at FDR 10% and n >= 30
with 99% LCB ordering; mismatch and income-pile gates at 0.25; national-only
lists at 5% and 10% with the shipped fill; scored on all cases and on non-pile
cases.

The light options are built in `common.R` (`add_light_features`) from the
staging-v2.7 tables. Each edition gives the options in effect on October 1 of
the year before its label, so FY2017 rows take the 2017 edition and FY2018
the 2018 edition; FY2019 rows take the 2018 edition (options at the start of
FY2018) and FY2022 rows the 2023 edition (options at the start of FY2023),
since there is no 2019 or 2022 edition. A state that changed an option
between those dates carries the later value. Every raw value must map to a
flag or the run stops. SMD is Ben's deduction table (0 where none), and the gap is
SMD minus (Part B premium minus $35), Part B keyed on the fiscal year as
`year_data.csv` is. The options report and the deduction table disagree on
SMD for Oregon FY2017-19 (the report lists none) and Louisiana FY2022; the
table is used. Known limits, kept: no state is below the SMD line in
FY2017-19. In training the gap runs from 1 to 111 dollars in SMD states and
-101 to -99 in states with none (the no-SMD value is minus the line, so it
drifts with the premium: -135 in FY2022). A split learned in the empty
interval between them acts as the SMD flag, so FY2022's eight below-the-line
states (-34 to -10) fall on the SMD side, with the low-gap SMD states; a split
among SMD states at a low gap groups them with the lowest training SMDs. The
eight
columns enter at mtry 2 among 27 features, so each is tried less often than
any of night 1's additions.

### 3. Support

mtry splits nothing. For the light options (`light_support.R`, which also
writes the readout's subgroup lists to `light_subgroups.csv`), the thinner
level of every flag is the "has the option" side, and its thinnest
household-size group is always 4 or more. Training rows / errors there:

| Flag = 1, households of 4+ | Rows | Errors |
|---|---|---|
| standard CAP | 2,310 | 296 |
| ESAP | 3,376 | 470 |
| modified CAP | 4,047 | 600 |
| county administration | 3,410 | 607 |
| SMD | 7,854 | 1,151 |
| self-employment by another method | 8,608 | 1,234 |
| TBA | 7,662 | 1,330 |

The full table (each level by group 1 / 2-3 / 4+) prints from
`light_support.R`; the "no option" side has at least 1,633 errors in every
group. The SMD gap ranges are given above. Subgroups in FY2022: the 20 states
with a light option that differs from some FY2017-19 year hold 14,988 rows
and 1,700 errors; the eight states below the SMD line hold 5,712 rows and 695
errors. Training and test rows as night 1.

### 4. What the record already says

- Ledger, engines: mtry 2 is part of a settled row (§2, §4, §13); the one grid
  on record (500 trees, 90% bound) had mtry 1 0.214, 2 0.223, 4 0.219. This
  night does not reopen the shipped setting: it asks whether the new features
  need a larger mtry to be used. Any change to mtry would need both engines
  and three seeds first.
- Night 1 (this folder): no limit arm passed the screen at mtry 2; rules using
  the new features were slightly less precise on FY2022 (0.260-0.270 vs
  0.270-0.274 for the other top-1,000 rules).
- Regimes plan (shared with Ben): Tier 1 gets one seed and one arm; it stops
  if its change is inside the seed spread.
- The regime-rate check applies to the light options too (they are
  state-level; `REGIME_FEATURES`).

### Screen and cost

Thresholds: the larger of 25 / 57 errors (5% / 10%) and arm A's seed gap at
mtry 2 (6 / 9) and at mtry 4 (tonight); there is no seed-noise reading at
mtry 3. More seeds for an arm only if it passes night 1's screen against arm
A at mtry 2, the setting that ships, since that is the decision; the
comparisons against A at the same mtry are read to explain a result, not to
trigger more seeds. Night 1's two regime subgroups are repeated for every
arm. Eight units, about an hour each on night
1's timing (mining takes two minutes; scoring the ~76,000 raw rules and the
walk take the rest), two workers: about 4 hours, launched 22:00, done near
02:00, 06:00 cut-off.

## Night 3 (2026-10-24): three seeds of A and E at mtry 2 and 3, two windows

Requested by the project lead on 2026-10-24 after night 2's readout
(`readout_night2.md`): settle whether mtry 3 helps and whether arm E is an
improvement, and add a second time period. Night 2's one lead was E at mtry 3
against A at mtry 2: +10 errors at 5% and +70 at 10% (non-pile +8 / +54), one
draw each; at mtry 4 a second seed of A alone moved 20 / 44. Chain
`run_chain_night3.sh`, readout `readout_night3.R` -> `readout_night3.md`.

### 1. The questions

(a) Over three seeds, does E (the shipped 19 + income as a percent of poverty
+ the BBCE limit) at mtry 3 catch more test-year errors than A (the shipped 19)
at mtry 2, which ships? (b) Is any gain from mtry, the features, or both: the
2 x 2 of A / E by mtry 2 / 3. Both in two windows: mine FY2017-19, score FY2022
(three years ahead, the deployment gap and the primary window); mine FY2022-23,
score FY2024 (one year ahead, the window the shipped benchmark was built in).

### 2. What varies, what is held fixed

| Comparison | Varies | Fixed |
|---|---|---|
| E3 vs A2 | features and mtry together (the candidate against what ships) | window, seeds |
| A3 vs A2 | mtry | the shipped 19 |
| E2 vs A2, E3 vs A3 | features | mtry |
| (E3 - A3) - (E2 - A2) | the interaction (descriptive) | |
| each cell, seeds 117 / 118 / 119 | seed | the reading of noise for that cell |

Everything else as nights 1-2: ranger alone, 1000 trees, depth 4, min node
20; strata 1 / 2-3 / 4+; BH at FDR 10% and n >= 30 with 99% LCB ordering;
mismatch and income-pile gates at 0.25; national-only lists at 5% and 10%
with the shipped fill; scored on all cases and on non-pile cases. The second
window comes from the seed-replicate study's loader (`SR_WINDOW=fy2024`, its
default: FY2022-23 training, FY2024 test, the benchmark's expected counts);
outputs go to `fy2024/`. In the FY2022 window, five of the twelve runs are on
disk (A at seeds 117 and 118, E, E at mtry 3, A at mtry 3, all seed 117);
seven are new. In the FY2024 window all twelve are new. The light options are
still computed by the loader (they map for FY2022-24 without a stop) but no arm
tonight uses them.

### 3. Support

mtry splits nothing. FY2024 window: training 76,031 rows / 8,397 errors
(FY2022-23), test 39,528 / 4,764 (FY2024). Pile rows: FY2022 427 / 135, FY2023
467 / 172, FY2024 500 / 162. E's limit takes five values in training; the
thinnest limit by household-size cell is 160% in households of 4+ (436 rows,
77 errors), the next 185% in 4+ (852 / 183); the 130% / no-BBCE level holds
24,254 rows. Four states' limits in FY2024 differ from FY2022 or FY2023
(Connecticut, Minnesota, New Hampshire, Pennsylvania). FY2022 window as night 1.

### 4. What the record already says

- Ledger, engines row (settled, §2 / §4 / §13): mtry 2 with xgboost + ranger.
  This night does not reopen it; it asks whether a ranger-alone gain at mtry
  3 is worth a both-engine test.
- Ledger, CPI step rows (§41): the same two windows at three seeds each,
  within-frame seed ranges of 5-57 errors at 10%. That is the size of noise to
  expect here.
- Night 2: ranger-alone seed gaps of 6 / 9 (mtry 2) and 20 / 44 (mtry 4), one
  pair each.

### Reading rule and cost

A contrast holds in a window when, at both budgets, the seed-mean gain in
errors caught is positive and larger than twice its standard error, the
non-pile gain is positive, the median and mean of the state-by-state
differences (each state's precision averaged over its seeds) are positive,
and no more states are worse by more than 0.05 than better. The standard
error is the larger of two: from each cell's own three seeds (two degrees of
freedom per cell), and from the seed variance pooled over the A and E cells
at the same mtry (four), since two degrees of freedom can come out small by
chance and seed noise differs by mtry.

Selection. Night 2 picked E at mtry 3 as the lead because its seed-117 draw
was the best of seven arms against A at mtry 2. That draw is one of the three
FY2022 seeds tonight, so the FY2022 three-seed reading carries some of its
luck. The readout therefore also gives every FY2022 contrast on seeds 118 and
119 alone, and the FY2024 window, wholly new, is the only reading free of the
selection. E at mtry 3 (or A at mtry 3) goes on to an xgboost + ranger test
only if it holds in both windows and its FY2022 gain on seeds 118-119 alone
is positive at both budgets; anything less is reported as such and changes
nothing. This is an exploration: the rule says what would justify the next
run, not whether to ship.

The readout also gives the change in mean dollar recall, and repeats nights
1-2's two subgroups in each window: the minority regimes in the test year (no
BBCE, or BBCE at 130%), and the states whose limit or BBCE status in the test
year differs from a training year.

Nineteen units on two workers: seven of about 45 min (FY2017-19) and twelve
shorter (two training years), about 5.5 h, 22:00 to about 03:30. The 06:00
cut-off stays; a relaunch resumes from files on disk. The readout runs even
after a cut-off and reads whichever runs finished.

Review (fresh senior statistician, 2026-10-24): REVISE on the readout and
this note only (the selection point above), the mining chain unchanged. Its
optional suggestions were taken: the pooled standard error, the interaction
left unjudged, dollars, the subgroups, a flag when a budget fails only on a
median of exactly 0, the readout after a cut-off, and a smoke check that
today's edits leave arm A's output unchanged.

## Outcome and close (2026-10-25)

A Windows Update restart at 23:29 on 2026-10-24 stopped the night-3 chain
after two of its nineteen units (A at mtry 3, seeds 118 and 119). E at mtry 3,
seeds 118 and 119, had finished mining; their state lists were walked and
scored by hand from the pools on disk on 2026-10-25 (logs_night3/*_resume.log).
The FY2024 window never ran.

FY2022, errors caught (seed means): A mtry 2 501 / 907 (two seeds), A mtry 3
495 / 915, E mtry 3 499 / 941 (seeds 514/485/498 and 972/909/941). E minus A
at mtry 3: +3.7 (SE 8.4) at 5%, +25.7 (SE 19.8) at 10%; neither clears 2 SE.
On seeds 118-119 alone: -3.0 at 5%, +17.5 at 10%, so the go-on condition
(positive at both budgets) fails, and the FY2024 window cannot change the
outcome under the rule above. E at mtry 3 minus A at mtry 2, by hand: -2 / +34.

We closed the study on 2026-10-25: mtry stays at 2 and the BBCE-limit features
are not added. The night-2 lead (+10 / +70) was mostly its seed-117 draw.
Readout: readout_night3.md (9 of 24 runs on disk).
