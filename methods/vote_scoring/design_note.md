# Case scoring by rule votes: pre-run design note

Issues #16 and #21. Plan: "Case scoring by rule votes: proposed plan"
(https://claude.ai/artifact/1rXfbMoE652LGuVCFyB1Td, revised 2026-10-22 after
Ben's review).

Decisions of 2026-10-27 (Eric):
- The primary reading moves to each state's top 5%.
- The study runs on the v2.7 frame and reuses its pools.
- The workbook carries at most 300 rules, allotted in fixed shares by
  household-size group.
- Two ways of counting a group's vote are both candidates.
- The curve is tested for carry-over from one rule list to the next.
- Out of this round: typed-pool votes, the underpayment column, and issue
  #21's unreported-income tilt and 1.085 multiplier.

Revised the same day after statistical review. FY2019 was opened on
2026-10-28 and the specification is fixed (see "Exploration result"). Both
confirmation windows were opened on 2026-10-28. Bar 1 failed; Eric decided on
2026-10-29, after the readout, to ship on the interval above zero (see
"Confirmation result and decision").

## 1. The question

Does ranking each state's cases by expected error dollars, built from how
many groups of national rules flag the case, find at least 20% more error
dollars per reviewed case in each state's top 5% than ranking by issued
benefit times the household-size group's error rate? The test is on a year
the rules and the score never saw, with a vote-to-error curve carried
unchanged from an earlier rule list, as it would be at deployment.

## 2. What varies

One component varies: the order in which each state's cases are reviewed.
Cases, budgets, frame, error definition and rule pools are the same in every
arm.

**Frame.** The study uses the v2.7 final frame, sha256 `1dbe8733ad0f30bc`
(main). It does not use the staging frame with Ben's 2026-10-27
reconstruction fixes (`eb989fda`).

**Windows and pools.** Every window uses seeds 117, 118 and 119.

| Window | Rules mined on | Fitted or scored on | Role | Pools |
|---|---|---|---|---|
| Exploration | FY2017-18 | FY2019 (fit) | chooses the vote definition, the P(error) form, the error-size model and the number of rules; fits the curve and the error-size model, which are then frozen | new: three national mines |
| Three years ahead | FY2017-19 | FY2022 (score) | decides; the frozen curve is applied to this pool's votes | existing (`v270_cpi_benchmark/fy2022_cpi` seed 117; `v270_seed_replicates/fy2022/pools` seeds 118-119) |
| One year ahead | FY2022-23 | FY2024 (score) | must point the same way; the frozen curve is applied to this pool's votes | existing (`v270_cpi_benchmark/fy2024_cpi` seed 117; `v270_seed_replicates/pools` seeds 118-119) |

- **Test years:** FY2022 and FY2024. FY2023 is never a test year; it is a
  mining year for the one-year-ahead pool.
- **FY2017-18 rules:** used only for fitting. Each test year is scored with
  its own window's rules, and the curve and the error-size model carry over
  unchanged from FY2019. That mirrors deployment, where a curve fitted on
  one list is used on a newer one.
- **FY2019:** the fitting year and also a mining year of the FY2017-19
  pool. The FY2022 test does not touch it again.

- FY2020-21 stay out.
- FY2019 was used to tune the one-state score in issue #21, so it serves
  only for exploration.
- Before any confirmation reading, every pool is checked: its training
  years must match its window.

### The score

The score is fixed on the exploration window, then applied unchanged.

**Rule groups.**
- Groups are built only from each pool's mining-year flags.
- Within each household-size group (1 / 2-3 / 4+), walk the pool's
  admitted rules in 99% LCB order, taking the top 1,000 rules in that
  group.
- A rule joins the first existing group whose leading rule shares more than
  half of their combined flagged training cases (Jaccard > 0.5). Otherwise
  it starts a new group.
- Groups are ranked by their leading rule's LCB.

**Allotment.** The workbook carries at most 300 rules (Eric, 2026-10-27).
- The rules are allotted in fixed, equal shares by household-size group: a
  total of R rules gives R/3 to each group, for R = 100, 200 or 300.
- Fixed shares keep the vote scale comparable across pools. Allotted in
  global LCB order, the top 300 rules split 223/37/40 by size group in the
  FY2017-19 pool and 176/84/40 in the FY2022-23 pool (statistical review,
  computed from `bench_national_117.rds`).

**Vote: two candidates.** A case's vote is the number of its size group's
allotted groups that flag it.
- *Any member:* a group flags the case when any of its member rules does.
  Groups are taken in rank order while their member rules total at most
  R/3. Member rules count toward the cap.
- *Representative:* a group flags the case only when its leading rule does.
  The top R/3 groups each contribute one rule.

**P(error).**
- The error rate by vote within each size group, fitted on FY2019 cases
  with the FY2017-18 groups.
- Isotonic, so it can only rise with the vote.
- Vote bins with fewer than 50 FY2019 errors in a size group are pooled
  into the next lower bin.
- The curve is never fitted on mining years: in the one-state work its
  slope ran about twice as steep there as out of sample.
- It is carried unchanged to the confirmation pools' votes.

**Error size.** A gamma regression (log link) on FY2019 error cases.

**Expected dollars.** P(error) times expected error size, multiplied only at
the end.

**Seeds.**
- The specification is chosen on seed 117.
- The chosen curve and error-size model are then refit on each exploration
  seed's pool.
- Each fit is applied to the confirmation pool of the same seed number.

### Choosing on exploration

All candidates are declared here. Choices are made in this order, on
out-of-fold predictions: FY2019 split into five folds grouped by state, with
bins pooled within each fold's fitting part.

1. **Vote definition.** Any member vs representative, at R = 300, judged by
   log-loss of the vote curve.
2. **P(error) form.** Judged by log-loss.
   - the vote curve;
   - the highest LCB among the case's flagging rules (isotonic);
   - the size group's base rate alone, as a floor.

   If the LCB form wins, it becomes the full score's P(error), and the
   study tests that form. If the base rate wins, the study stops at
   exploration and records why.
3. **Error-size model.** Judged by gamma deviance.
   - size-group means;
   - log issued benefit plus exact household size (6+ pooled);
   - the same plus the vote.
4. **Number of rules.** Choose the smallest R of 100 / 200 / 300 whose
   out-of-fold top-5% error dollars per reviewed case is within 5% of
   R = 300.
   - This is the only choice read on dollars. It compares three nested
     sizes, not rival forms.

Then measure the paired standard error of the gain on FY2019. Use
out-of-fold scores against the simple ordering, with 2,000 within-state
resamples. Write the minimum detectable gain into this note before either
confirmation window is opened.

### Exploration result (FY2019, opened 2026-10-28)

Script: `methods/vote_scoring/explore_fy2019.R` (with `vote_groups.R`).
Output: `methods/vote_scoring/fy2019/explore_*.csv`, log in `fy2019/logs/`.

**The specification carried to confirmation:**
- **Vote:** representative. The leading rules of the top 100 groups in each
  size group, 300 rules in all.
- **P(error):** the vote curve, isotonic by size group.
- **Error size:** gamma (log link) on log issued benefit, exact household
  size (6+ pooled) and the vote.
- **Frozen fits:** refit on all of FY2019 for each seed in
  `fy2019/frozen_seed{117,118,119}.rds`. Each one carries the curve's bins and
  rates and the error-size model.

**The choices (seed 117, out of fold over five folds of states).**

| Step | Candidates | Result | Chosen |
|---|---|---|---|
| 1. Vote definition, R = 300 (log-loss) | any member / representative | 0.3133 / 0.3091 | representative |
| 2. P(error) form (log-loss) | vote curve / best flagging LCB / base rate | 0.3091 / 0.3103 / 0.3217 | vote curve |
| 3. Error-size model (gamma deviance, 3,872 errors) | size-group means / benefit + size / benefit + size + vote | 1,379 / 1,309 / 1,302 | benefit + size + vote |
| 4. Number of rules (top-5% $ per reviewed case) | R = 100 / 200 / 300 | $37.15 / $41.26 / $44.96 (-17%, -8%, 0) | 300 |

- **Representative vs any member.** Under "any member", whole groups count
  toward the cap, so the cap is never filled. At R = 300, any member used 237-257
  rules in 47-64 groups. At R = 100 it used 67-86 rules, and the 2-3 group
  held only 2-5 groups, because one group in that size group has 30 member
  rules among the top 1,000.
- **Groups.** The top 1,000 rules per size group form 1,046-1,080 groups
  per seed (312-406 per size group).
- **Reach.** The representative vote at R = 300 flags 10.5-11.8% of FY2019
  cases.

**The fitted curve (seed 117, FY2019 error rate by vote).**

| Size group | Vote 0 | Rising bins | Top bin |
|---|---|---|---|
| 1 | 0.056 (19,671 cases) | 0.134 (1), 0.150 (2-4), 0.225 (5-6), 0.302 (7-9), 0.403 (10-12), 0.487 (13-20) | 0.853 at 21+ (61 cases, 52 errors) |
| 2-3 | 0.112 (9,865) | 0.178 (1), 0.198 (2-5) | 0.370 at 6+ (181 cases) |
| 4+ | 0.151 (4,618) | 0.228 (1-2) | 0.319 at 3+ (408 cases; raw 0.354 at 3-4 and 0.276 at 5+, pooled by the isotonic fit) |

The top bins are not reconstruction artifacts. They hold 0, 1 and 3 artifact
rows.

**Gain on FY2019 and the minimum detectable gain.**
- **Gain over the simple ordering.** Out-of-fold top-5% error dollars per
  reviewed case:
  - seed 117: $44.84 against $29.13, a gain of 54%;
  - seeds 118 and 119: 47% and 46%;
  - seed mean: 49%.
- **For scale.** Random order gives $12.39.
- **Without artifact rows.** Dropping them, the gains are 51%, 49% and 47%.
  Artifact rows supply 1.2-2.6% of the score's top-5% dollars, against 5.6%
  of the simple ordering's.
- **Paired bootstrap SE of the gain** (seed 117, 2,000 within-state
  resamples, scores held fixed): 9.7 points (95% range 30% to 68%). The
  between-seed SD is 4.3 points.
- **Minimum detectable gain against the 20% bar:**
  - 39% at 50% power;
  - 47% at 80% power;
  - 51% at 80% power, counting between-seed variance as the readout does.

**What this means for confirmation.**
- **Size of the gain.** The confirmation can detect a true gain about as
  large as the one seen here, and not much smaller. FY2019 is one year past
  its rules. The deciding window is three years past, and its curve is
  carried rather than refit. Both should shrink the gain.
- **Optimism.** The FY2019 figure is also optimistic in one respect: the
  curve and error-size model were fitted on FY2019 itself, out of fold by
  state but within the same year.
- **The likely readings.** A three-year-ahead gain well above 0 but short
  of bar 1 is a real outcome to expect. The readout records it as a real
  gain under the bar.

**Departures from the text above, made in the script.**
- **Artifact-tagged rules.** These rules (`MM_TAG_SHARE`; 98 of 53,402 in
  seed 117) are dropped before the top 1,000 are taken, as the benchmark
  drops them from the visible pool.
- **Binning the LCB form.** The best-flagging-LCB form used the same
  50-error pooling on its sorted values as the vote curve.
- **Ties.** Score ties are broken by issued benefit and then at random.
  The simple ordering's ties are broken at random, as in the noise check.
- **Folds.** States were assigned to the folds with seed 20261028.

**Post-hoc check of larger workbooks (2026-10-28, FY2019 only).** R = 400 to
900 were not candidates. They were scored after the choices above, on all
three seeds (`fy2019/posthoc_rules_curve.csv`).
- The seed-mean gain rises from 28% at R = 100 to 39% at 200 and 45% at 300.
  It then stays at 46-50% through 900.
- Seed-to-seed swings at a given R are 3-7 points, so the rise after 300 is
  within noise.
- More rules extend reach (11% of cases flagged at 300, 31% at 900) and
  keep improving log-loss (0.3085 to 0.3012). At the top 5% they change
  little.
- The specification stays at R = 300.

**Pool check (2026-10-28).** For every confirmation pool, a sample of 300
rules was re-flagged on the expected mining years. In all six pools, n and k
reproduce exactly on all 300:
- three years ahead, FY2017-19: 57,718 / 57,126 / 56,854 rules;
- one year ahead, FY2022-23: 54,283 / 54,701 / 54,385 rules.

### Arms at confirmation

Each arm is an ordering of every case in the state:
- random order;
- the comparison: issued benefit times the group's base rate, with base
  rates from the window's mining years;
- the vote alone, ties broken by benefit;
- the LCB of the best flagging rule;
- the full score.

References are descriptive and carry no bar:
- a ridge regression on one indicator per rule group;
- a boosted model on the 19 features, P(error) times expected error
  dollars. This is snap_qc's production xgboost by household-size stratum,
  as in the 2026-10-27 comparison with the PER-experiments repo.

## 3. Support after the split (computed on 1dbe8733)

| Window | Mining cases / errors | Mining errors by size 1 / 2-3 / 4+ | Test cases / errors | Test errors by size 1 / 2-3 / 4+ | Top-5% reviews nationally (median state, min) |
|---|---|---|---|---|---|
| Exploration | 77,905 / 7,048 | 2,544 / 2,554 / 1,950 | 38,155 / 3,872 | 1,525 / 1,334 / 1,013 | 1,911 (41, 14) |
| Three years ahead | 116,060 / 10,920 | 4,069 / 3,888 / 2,963 | 36,851 / 3,985 | 1,632 / 1,364 / 989 | 1,838 (40, 6) |
| One year ahead | 76,031 / 8,397 | 3,423 / 2,898 / 2,076 | 39,528 / 4,764 | 1,932 / 1,611 / 1,221 | 1,981 (42, 13) |

- The curve and the error-size model are fitted on FY2019's 3,872 errors,
  at least 1,013 per size group.
- The number of vote bins that reach 50 errors per group is known only
  after the FY2017-18 mine.

**Noise check (step 1 of the plan; done 2026-10-27, simple ordering only).**
Base rates are from FY2022-23, and FY2024 cases are resampled within state
2,000 times. Error dollars per reviewed case:

| Each state's top | Reviewed nationally | Per reviewed case | SE (relative) | 95% range | Random | Perfect |
|---|---|---|---|---|---|---|
| 1% | 395 | $68.9 | $9.0 (13%) | $51-86 | $22.5 | $516 |
| 2% | 793 | $62.8 | $6.2 (9.9%) | $52-76 | $22.5 | $413 |
| 5% | 1,981 | $57.9 | $3.5 (6.0%) | $51-65 | $22.5 | $291 |
| 10% | 3,955 | $53.1 | $2.2 (4.2%) | $48-57 | $22.5 | $200 |

- At the top 5%, the bar needs a true gain of roughly 30-38%. That
  estimate leaves out seed and calibration variance, so the real figure is
  higher. It is re-measured on FY2019 before confirmation.
- At the top 2%, a median state reviews 17 cases. The decision is
  therefore national, as in issue #21.
- Script and output: `methods/vote_scoring/noise_check.R` and
  `noise_check_v27_1dbe8733.csv`.

**Reconstruction-artifact rows.** These are cases issued at or above the
maximum benefit whose rebuilt benefit falls below it (`load_frame`'s
`mm`).
- There are 155 / 147 / 236 of them in FY2019 / FY2022 / FY2024.
- Their error rates are 0.70-0.78, and they hold 4.4-7.1% of each year's
  error dollars.
- 666 of the top 1,000 FY2017-19 rules use `rawben_rel_max` or
  `unc_rawben_rel_max`, and the rule-level 0.25 artifact tag removes none
  of them.
- In FY2024, the simple ordering's top 5% holds 31 of these rows, which
  give 12.3% of its dollars. This was computed on the simple ordering only,
  which the noise check had already opened.

## 4. What the ledger and findings already say

- **Lists at budget depth are not stable across seeds** (ledger; §31).
  Coverage at depth 20,000 is stable (Jaccard 0.959-0.965). Overlap in
  errors caught is 0.531 at 5% and 0.666 at 10%, and about 0.65-0.70 at
  100-1,000 rules. A 300-rule score sits in the unstable range, hence three
  seeds.
- **Out-of-fold ordering of the national pool is retired at public national
  scale** (ledger; §30). It scored -0.0044 at 5% against a +0.010 bar, so
  the mining-year LCB orders rules here and is not cross-fitted.
- **Typed pools lost as members of the delivered lists** (ledger; §17;
  §3). They are out of this round.
- **A feature beat a stratum** (ledger; §8). The size groups stay strata
  here because the curve and the allotment are within them.
- **Always compute support after any split** (ledger; §30). See item 3.
- **Reconstruction artifacts** (ledger hazard rows; §28, §38). Hence the
  second reading below.
- **The issue #21 one-state score.** On FY2017-19 the error rate rose with
  the number of flags: 0.065 / 0.17 / 0.26 / 0.40 at 0 / 1-2 / 3-5 / 6-10.
  Dollars per error stayed between $117 and $146. Its constants were chosen
  on FY2017-19, and issue #21 notes they "hold only for the one list they
  were measured on". That is the carry-over this study tests.

**Disclosures.**
- On 2026-10-27, comparing this repo with the PER-experiments repo, boosted
  models were scored on FY2022 and FY2024. That included the two-part
  P(error) times expected dollars form, which raised dollar recall in 30-40
  of 49 states by 0.7-2.8 points. No constant of the vote score was chosen
  from those runs, and the two-part form was in the plan doc of
  2026-10-17.
- The noise check and the artifact-row count scored the simple ordering on
  FY2024.
- The v2.7 CPI step was adopted on FY2022 and FY2024 readings of these same
  pools (§41: +14-16% error dollars three years ahead). It changes rule
  features but not the simple ordering's within-state order, so it may
  favor the rule-based arms on the deciding year.
- The plan doc's figure of 364 groups from 1,000 rules was computed on
  FY2022-24 flags. In the study, grouping uses mining-year flags only.

## Readout and bars

**Primary.** Three years ahead (FY2017-19 to FY2022). Each state takes its
top 5%, and the flagged cases are pooled nationally. The contrast is error
dollars per reviewed case, full score against the simple ordering, as a
ratio minus 1.
- The decision rests on the seed-mean gain over seeds 117-119.
- Its interval combines the within-seed bootstrap variance with (1 + 1/3)
  times the between-seed variance.
- Within each seed, the bootstrap resamples cases within state in both the
  test year and FY2019, 2,000 draws. Inside each draw, it refits the curve
  and the error-size model on the resampled FY2019. The rule pools are
  fixed.
- The per-seed gains are reported beside the mean.

**Ships as a general workbook feature only if all of these hold:**

1. The seed-mean gain is at least 20%, and its 95% low end also clears 20%.
2. Bar 1 also holds on a second reading that drops the reconstruction-
   artifact rows from both arms, from ranking and from the totals. Each
   arm's top-5% share of dollars from those rows is reported.
3. One year ahead (FY2022-23 to FY2024), the seed-mean gain is above 0.
4. **Harm check.** The harm count is the number of states whose dollar
   recall under the score is worse than under the simple ordering by more
   than 0.05, at the 5% and at the 10% budget.
   - The null comes from 200 permutations of the vote term within size
     group × issued-benefit decile. This keeps the benefit information and
     destroys the vote information.
   - The observed count must be at or below the null's 95th percentile at
     both budgets.
5. **Companions, binding.** Beside the national figure: the within-state
   mean change in dollar recall at 5% (score minus simple ordering), the
   harm count, and states up and down.
   - A national win contradicted by both a negative within-state mean and a
     harm count above the null's 95th percentile does not ship (ledger
     process rule of 2026-08-07).
   - This study decides nationally, not on the median state, because at
     5% a median state reviews about 40 cases. That is a departure from the
     usual practice, stated here.

**Other outcomes.**
- A gain whose 95% low end is above 0 but which misses bar 1 is recorded as
  a real gain under the bar. It does not ship.
- A gain in one window only is recorded as an era result.

**Secondaries.** These are Holm-adjusted, family alpha 0.05, two tests.
1. The vote curve rises with the vote. A Cochran-Armitage trend test on the
   error rate across vote bins, within each size group, in both
   confirmation windows, pooled by seed mean.
2. The vote adds to error size: the coefficient on the vote in a gamma
   model on test-year errors, within exact household size, with state fixed
   effects.

**Descriptive.**
- the between-state variance of the error rate per vote bin, beyond
  binomial noise;
- the training-year curve against observed test-year rates by bin;
- the same contrast at the top 1%, 2% and 10%;
- calibration of expected dollars by score decile;
- the perfect ordering;
- the two references.

## Confirmation result and decision (run 2026-10-28; decided 2026-10-29)

Run: `confirm_window.R` per window, `confirm_readout.R`; outputs in
`confirm/fy2022/`, `confirm/fy2024/` and `confirm/readout.txt`. Full run,
2,000 bootstrap draws and 200 permutations per seed.

| Bar | Result | |
|---|---|---|
| 1. Three years ahead, gain >= 20% with 95% low end > 20% | 27.6% (seeds 30.2 / 21.3 / 31.5%); 95% 5.9% to 49.4% | fail |
| 2. Same, artifact rows dropped | 35.7%; 95% 6.0% to 65.3% | fail |
| 3. One year ahead, gain > 0 | 41.9%; 95% 23.8% to 60.1% | pass |
| 4. Harm count at or below the null's 95th percentile | FY2022 4.7 vs 24.7 (5%), 6.3 vs 27.3 (10%) | pass |
| 5. Companions do not contradict | within-state mean change in dollar recall at 5% +0.035 to +0.058; 31-34 of 49 states up | pass |

Secondaries both hold (Holm p 1.4e-9 for the trend, 1.6e-6 for the vote in
the size model).

Under the rules above, the score does not ship and is recorded as a real gain
under the bar. **Decision (Eric, 2026-10-29, after the readout):** ship it,
because the 95% interval of the deciding gain lies above zero. This replaces
bar 1's 20% low end with 0 after the results were seen. The record keeps both
readings: by the pre-set rule it does not ship; by the project lead's decision
it does.

What the readout says about deployment:
- **Relative risks carry over; levels do not.** The vote-0 error rate rose
  from 5.5% (FY2019) to 6.1% (FY2022) and 6.6% (FY2024) in one-person
  households, with similar rises in the other groups. The ratio of the
  vote-1 bin to vote 0 held (1.4-2.5 in FY2019, 1.4-2.2 three years ahead).
  The top one-person bin was overstated three years ahead (relative risk 15
  in FY2019, 9 in FY2022, 12 in FY2024).
- **Expected dollars run low.** Observed error dollars per case were 1.37
  times the carried score's expectation in FY2022 and 1.70 times in FY2024.
  Shipping relative risks with the level set from each state's FNS payment
  error rate addresses this.
- **States differ in level more than in shape.** Between-state variance of
  the vote-0 rate is 2-6 times binomial noise; in flagged bins with 20 or
  more states it is a median 1.3-1.5 times.

## Cost

- **Three national mines on FY2017-18 (seeds 117-119) on 1dbe8733.** At
  about 2.4 hours each, this is one night, using
  `methods/v270_seed_replicates/mine_national_seed.R`.
- **Required change first.** `common.R` accepts only the fy2024 and fy2022
  windows. Simply adding a FY2017-18 window would put its pools in the
  folder that already holds `admitted_final_seed117.rds` from FY2022-23,
  and the miner would exit with "pool already on disk" before its
  row-count checks run. The window needs its own pool folder, the training
  years stored as a pool attribute, and an assert on them when a pool is
  read.
- Grouping, curve fitting, bootstraps and permutations take minutes to
  hours per window.
- No writes to `state_delivery_lists/` and no CHANGELOG entry.

## Not in this round (Eric, 2026-10-27)

- **Typed-pool votes as error-size predictors** (plan item 4, issue #16),
  and the underpayment vote column. No typed pools exist on the v2.7 frame,
  and each would cost at least one more mine per window.
- **Issue #21's unreported-income tilt and 1.085 multiplier.** These are
  not in the plan doc's revision and are not tested here. Issue #21 should
  say so when this note is filed.

## Next

1. Add the exploration window to `common.R`, with its own pool folder and a
   training-year assert. Done 2026-10-27.
2. Mine FY2017-18 at seeds 117-119. Done 2026-10-27/28, about 50 minutes per
   seed.
3. Explore on FY2019, write the one specification and the minimum
   detectable gain into this note, and check every pool's training years.
   Done 2026-10-28.
4. Open the two confirmation windows. Done 2026-10-28; decided 2026-10-29
   (above).
5. Ship: fix the relative-risk table at deployment age (FY2017-19 rules
   fitted on FY2022, checked on FY2023), mine the shipped FY2022-24 rules,
   and replace the workbook's in-sheet bins with the fixed table and the
   state's FNS payment error rate.
