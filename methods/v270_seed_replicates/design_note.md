# Design note: v2.7 seed replicates (2026-10-02)

A technical exploration to diagnose a measurement, not a shipping test. No
adoption bar is set. Scheduled for 22:00 on 2026-10-02 at the project lead's
instruction.

## 1. The question

On the FY2024 window, is the difference between the 09-25 frame and the final
frame larger than what re-mining with a different random seed produces?

A second reading comes from the same mines at no extra mining cost: does
pooling the candidates of two or three seeds (a more exhaustive search) change
held-out precision at the top of the ranking, or how well the top rules hold
to their confidence bounds?

## 2. What varies, what is held fixed

| Component | Setting |
|---|---|
| **Frame (varies)** | 09-25 frame (annual-average CPI keyed on the review month's calendar year) or final frame (October CPI keyed on fiscal year, $10 SUA-tier tolerance). Same 231,619 cases in the same order. |
| **Seed (varies)** | 117, 118, 119, crossed with the frame: six national mines. |
| Mining years / test year | FY2022-23 / FY2024 |
| Engines | xgboost 1000 rounds, eta 0.02, subsample 0.20; ranger 1000 trees, mtry 2; depth 4 |
| Vocabulary | the 19 features of the shipped lists, `utilities_sua` included |
| Strata | household size 1 / 2-3 / 4+ |
| Admission | one BH pass at FDR 10% against the stratum base rate, and n >= 30 |
| Ordering | one-sided 99% Wilson lower confidence bound of training precision |
| Artifact tag | mismatch-share tag at 0.25, tagged rules dropped before the walk |
| State lists | national pool only, the shipped fill walk (fresh-share 0.50, buffer to 3x), 5% and 10% budgets |
| Pooled-seed arms (second reading) | candidates of 1, 2 or 3 seeds on one frame; admission re-run in one BH pass over the pooled candidates; same ordering |

Two components vary, crossed, because the question is their relative size.
"Frame" is itself a bundle of three changes (fiscal-year keying of the CPI,
October CPI values, the $10 SUA-tier tolerance on 593 FY2022-24 rows); this
run does not separate them.

The constants and the admission, fill-walk and scoring functions are read by
name from the shipped benchmark script
(`methods/v250_benchmark_2024_utilrel_v2.R`). The frame block, the artifact
tag and the national mining block are copies of the benchmark's; they match
it as of 2026-10-02, and the two anchors below are the check. The state walk
runs over the full visible pool, as the benchmark's does (no ranked window).

State pools are left out, so the results do not carry to the blended
benchmark's numbers. In the two v2.7 benchmark arms the state pools supply
22 of 1,459 and 17 of 1,531 core rules across all 49 states at the 5%
budget, and 67 of 2,664 and 69 of 2,735 at 10%.

## 3. Support

Nothing is split or subsampled beyond the shipped recipe.

- Training: 76,031 rows, 8,397 errors (FY2022-23). By stratum, as the
  benchmark arms logged: size 1, 45,165 rows and 3,423 errors; size 2-3,
  20,162 and 2,898; size 4+, 10,704 and 2,076.
- Test: 39,528 rows, 4,764 errors (FY2024).
- State walks use each state's own FY2022-23 rows for the fill (about 1,550
  per state) and its FY2024 rows for scoring (about 800), as the benchmark
  does. Rule support is national (n >= 30 on the national training rows).
- What a state list rests on: the median state's 5% list is 42 FY2024 cases
  (Wyoming's is 13), so one state's precision carries a binomial standard
  error of about 0.07 and the 0.05 harmed / helped cut sits inside it.
  Pooled over 49 states the lists hold about 1,950 flags at 5% (standard
  error about 0.011) and 3,930 at 10% (about 0.007).
- Draws: if the seed-117 anchors reproduce the benchmark's pools, seed 117
  is the draw that raised the question and only seeds 118 and 119 are new.

## 4. What the record already says

- Ledger row 40 (finding 31): "Seed-to-seed variation of the shipped
  pipeline: deep coverage is seed-stable ... but budget-depth lists are not
  (errors-caught overlap 0.531 at 5%, 0.666 at 10%); the instability is
  ordering, not vocabulary". Status settled; scope one era, national pools.
- Ledger row 83 (finding 5): "Mine big, filter stringently: a big pool at the
  99% bound matches a small pool's operating point with a much longer usable
  list". Status settled.
- Ledger row 51 (finding 26): "Raising the support floor above 30 helps a
  bigger search" is retired. The pooled-seed arms keep n >= 30.
- CPI test (2026-09-21, `methods/cpi_inflate_test/out/readout_lists.csv`),
  this window, national-only lists: changing only the seed moved the mean
  state precision by +0.007 to +0.011, with harmed / helped counts of 7/9,
  7/11, 1/6 and 5/7.
- Diagnosis of 2026-10-02 (`custom_one_off/v27_fy2024_shift_diagnosis_2026-10-02/`):
  at seed 117 the national union caught 674 (09-25 frame) and 633 (final
  frame) errors at 5% of FY2024 cases flagged, and 1,118 and 1,114 at 10%.
- Pooled seeds on the August seed-stability mines (older frame and
  vocabulary; local scratch analysis of 2026-10-02, not in the repo:
  `custom_one_off/v27_fy2024_shift_diagnosis_2026-10-02/pooled_seeds.R` and
  `.log`): precision when the union reached 5% of FY2024 cases was 0.309
  (one seed, mean of three), 0.307 (two seeds) and 0.306 (three seeds); at
  10%, 0.271, 0.279 and 0.281; at 1%, 0.432, 0.414 and 0.401.

## Checks built into the run

- **Pool anchor.** Seed 117 is re-mined on each frame and compared with the
  pool the v2.7 benchmark arm cached (expected: final 54,283 admitted,
  09-25 54,596). It is reported, not a stop: if the engines are not
  reproducible at a fixed seed, the re-mine is one more draw. At smoke scale
  a same-seed re-mine reproduced its pool exactly.
- **Frame identity.** The 09-25 frame's hash (c4f715f59aef8291) is the one
  the 09-25 chain logged (`v270_release_chain.log`); the final frame's
  (1dbe8733ad0f30bc) is the one the 09-30 chain logged.
- **Walk anchor (passed 2026-10-02, full-pool walk).** For the four states
  whose own pool is empty in the final-frame benchmark (Delaware, South
  Dakota, Vermont, Wyoming), the national-only walk in `score_pool.R`
  reproduced the benchmark's core count, buffer count, flagged cases and
  errors caught in 8 of 8 state-budget cells. It checks that the walk and
  scoring are the benchmark's; it covers four small states on one frame.
- **Single-seed admission.** In `score_pooled.R` each single-seed arm must
  re-admit exactly its own admitted pool, rule for rule.
- **Cut-off.** At 06:00 the chain stops its own R processes and starts no
  further step (tested at smoke scale); a relaunch resumes from the pools
  on disk.

## How it will be read

**Primary statistic, named before the run:** the pooled precision of the 49
national-only state lists at the 10% budget, direction final below 09-25
(the blended benchmark's largest gap was at 10%: 0.2996 against 0.2908).
It is called "beyond seed movement" only if all three final-frame values
lie below all three 09-25-frame values. With no frame difference that
happens 1 time in 20; because seed 117 is already known to be ordered that
way in the blended benchmark, the new information is seeds 118 and 119 and
the chance is nearer 1 in 10.

Everything else is descriptive, with the usual companions: paired state
median, mean, states up and down, and harmed / helped counts at both
budgets, pooled precision, dollar recall, and the national union curve
read on precision at matched share.

Three values against three is a coarse test. A result short of full
separation is weak evidence that there is no frame effect: it says the gap
seen at seed 117 is of the size seeds produce, not that the frames are
equal. One test year is the other limit.

## Review

A fresh senior-statistician review (2026-10-02) returned "revise". Applied
before the run: the state walk now uses the full pool instead of a
20,000-rule window; the 06:00 cut-off now holds (sentinel file, this
chain's processes only); the readout names one primary statistic, carries
states-up / states-down counts, reads the union curve on precision, labels
the fixed top-K table as not like-for-like across pooled arms, and reports
tagged rules and fill gaps per pool; the anchor block cannot fail a mine.

## Outputs

`methods/v270_seed_replicates/`: `pools/` (gitignored `.rds`),
`states_*.csv`, `rules_*.rds`, `pooled_*.csv`, `readout.md`,
`readout_*.csv`, `logs/`. Nothing is written to `state_delivery_lists/`.

## Night 2 (2026-10-03): the decomposition

Scheduled 22:00 at the project lead's instruction (Task Scheduler
`snapqc_v270_decomp`, `run_decomposition.sh`). Night 1 ran its mining,
scoring and pooled-seed arms (22:00-01:34) and then stalled on a bare
`wait` until the 06:00 cut-off, which skipped the pooled-pool state lists
and the readout. Every wait now names its PIDs and is skipped when the list
is empty (tested at smoke scale, with the cut-off watcher running, both
with work to do and with every step already on disk).

Two intermediate frames are mined at seeds 117, 118 and 119 and scored the
same way, so the 09-25-to-final difference splits into three ordered steps.
Each step is measured with the earlier changes in place, so the steps add up
to the total.

| Step | Frame (hash) | What changes in the mined features |
|---|---|---|
| start | f0925 (c4f715f59aef8291) | annual-average CPI keyed on the review month's calendar year |
| 1 | f0928 (fdbc4508a2136370) | two changes: CPI keyed on fiscal year (dollar features of October-December rows), and the SUA tier computed on review-year amounts: 439 FY2022-24 rows tier 2 to 1, 229 of them training rows, 207 of them among step 3's rows |
| 2 | f0929 (a0fd4f4dd643a0b2) | October CPI values: the six CPI-adjusted dollar features only |
| 3 | final (1dbe8733ad0f30bc) | $10 SUA-tier tolerance: 593 FY2022-24 rows tier 1 to 2, with utilities, shelter and total deductions on those rows |

The section is descriptive: no step has a primary statistic or a bar. It
prints nine separation readings, and with no effect each shows full
separation about 1 time in 10. Beside each step it prints how many of the
top 1,000 rules the two frames share at the same seed, against the share
two seeds share: where a change is small, same-seed pools are nearly the
same pool. If step 1 carries the gap, separating its two changes needs a
fifth frame on a later night.

A second fresh review (2026-10-03) returned "revise" on labelling (step 1
was named as the keying alone) and readout clarity; all items applied
before the run. After the decomposition and its readout, the chain runs the
pooled-pool state lists night 1 skipped, then the readout again.

**Added 2026-10-03, at the project lead's request: the no-CPI comparison.**
The nominal frame (built 2026-09-21 on main, hash ff92ee2054515f26) is mined
at the same three seeds after the decomposition frames, and enters the
readout as step 0 (09-25 minus nominal) and as a nominal-to-final total.
Step 0 is a bundle, not the CPI step alone: dollar amounts inflated to 2026
(calendar-year key, annual-average CPI, 2026 deduction tables), total
deductions redefined to include the standard and homeless deductions, the
ABAWD share over the reconstructed unit size (81 rows), and the 09-25
frame's SUA tier on inflated amounts (439 rows). Its seed-117 pool is
checked against the cached nominal benchmark pool (55,275 rules). The
consensus-filter and pooled-seed lines are dropped from tonight's run.

**Added 2026-10-03 evening: the clean CPI-off frame.** The tracked munging
script already has a switch for the CPI step (`cpi_inflate_vars`). A copy
with only that switch set to FALSE, and its outputs redirected to
`archive_data/cpioff_build_2026-10-03/` so the live frame is never touched,
builds the final frame's code without the CPI step:
`archive_data/reg_model_data_cpioff_2026-10-03.rds`. The frame check
(`frame_check_cpioff.R`, log `archive_data/frame_check_cpioff_2026-10-03.log`)
requires everything outside the CPI block to be identical to the final frame
and the CPI-off raw dollar inputs to equal the final frame's review-year
copies. The readout then adds two clean contrasts that add up to the
nominal-to-final total: "final minus CPI-off" (the CPI step alone) and
"CPI-off minus nominal" (every other change since the 09-21 frame). Mining
order tonight: the decomposition frames, then CPI-off, then nominal, about
7 hours from 22:00.

## Night 3 (2026-10-04): the FY2017-19 -> FY2022 window

Same study scripts with `SR_WINDOW=fy2022` (`common.R` sets the years and
expected counts as `runners/run_v270_cpi_bench.R` set them for the
FY2017-19 benchmark script, whose recipe is otherwise identical to the one
read by name: same vocabulary, national mining block, admission, artifact
tag, fill walk and scoring, checked by diff and by a fresh review).
Outputs in `methods/v270_seed_replicates/fy2022/`; chain `run_fy2022.sh`;
readout `readout_window.R`.

- **Question:** on the three-year-ahead window, what does the CPI step
  alone do (final minus CPI-off, same munging code), and then the three
  ordered steps from the 09-25 frame to the final frame?
- **Support:** training FY2017-19, 116,060 rows and 10,920 errors
  (household size 1: 63,064 / 4,069; 2-3: 34,376 / 3,888; 4+: 18,620 /
  2,963); test FY2022, 36,851 rows and 3,985 errors. The smallest state's
  FY2022 sample (Delaware, 126 rows) gives lists of 6 cases at 5% and 12 at
  10%.
- **Reuse:** the seed-117 pools for the final and 09-28 frames are the cached
  FY2022-window benchmark pools (identical recipe and frames, verified by
  hash), so they are scored, not re-mined.
- **Time:** FY2017-19 mining runs about 1.6 times as long as FY2022-23, about
  2.4 hours per mine-and-score with two workers. Tonight covers the CPI
  comparison at all three seeds plus the 09-28 frame's cached seed 117. The
  09-25 and 09-29 frames (needed for every step contrast) do not fit in one
  night and wait for a relaunch, which resumes from the pools on disk.
- **Guard:** the chain refuses to start unless every frame file's hash
  matches the frame the cached pools and checks were built on.
