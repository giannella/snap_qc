# Follow-up review: tuning, noise, and the design of a next run

Senior-statistician review, fresh context, 2026-11-13. Requested by Eric after the challenger benchmark of 2026-11-12.

Scope: (A) whether CatBoost and BART were tuned badly, (B) how large the noise is and what a multi-seed test would buy, (C) a design for trying more specifications, (D) specifications I would try. The principal data scientist leads on model structure; my list in D is short on purpose.

Each claim is marked **[checked]** (I ran it or read the evidence) or **[judged]** (my expectation).

What I read: `findings_ledger.md`, `known_constraints.md`, `design_note.md`, `README.md`, `02_fit.py`, `03_fit_bart.R`, `04_score.R`, every file in `tuning/`, the logs, `results/`, and `s46_lib.R` / `s46_models.R`.

What I ran: scripts and outputs are in `reviews/scratch_statistician/`. About 26 minutes of model fitting, at most 6 threads. Every new fit was judged on the held-back last training year. No test-year label was used to choose or rank a new specification. Test-year labels were read only to recompute standard errors and to re-weight the rankings the benchmark had already scored.

## Verdicts

1. **CatBoost was not materially mis-tuned.** In W3 five variations (depth 10, two regularisation levels, column sampling, half the learning rate) all land within 0.0004 of the chosen setting on the held-back criterion, and the best of them is ahead by less than one seed standard deviation. In W2 a wider grid finds a setting 0.0005 better, which is real but worth under 0.1 point at 10%. [checked; the 0.1 point is judged]
2. **BART was under-run.** Its held-back log loss was still falling at sweep 1,500, a single chain is clearly worse than the four pooled, and its size model is 8% low with a slope of 1.14. BART has headroom that CatBoost does not. [checked in W1 for P and W3 for size; the headroom is judged]
3. **The half point cannot be confirmed by more seeds.** The paired bootstrap over test cases is 93% of the variance of the CatBoost difference at 10% and 91% of BART's. With 30 seeds the standard error goes from 0.55 to 0.54 for CatBoost and from 0.75 to 0.70 for BART. A true +0.5 would read z = 0.9 and 0.7. [checked]
4. **The half point is probably noise.** CatBoost gets it from California, New York and Florida; BART from California and Florida. Without those three states CatBoost is -0.15 and BART -0.30. With the same rankings and states weighted equally, CatBoost is -0.35 and BART -0.30; pooled over sample cases, -0.03 and -0.39. [checked numbers; the reading is judged]
5. **A multi-seed evening is not worth running as a test of the half point.** If an evening is spent, spend it on the three arms in section C, with the expectation stated there: these three tests can confirm a gain of about 1.5 points at 10%, and nothing on the list is expected to give that. [judged]

## A. Was the tuning off?

### A.1 Held-back-year criteria, every learner against xgboost

From `tuning/`. P is the within state-year log loss, size is the gamma deviance, both on the held-back last training year. Lower is better. [checked]

| Window (held-back year) | xgboost, B's settings | xgboost re-tuned | LightGBM | CatBoost | BART |
|---|---|---|---|---|---|
| P, W1 (FY2018) | 0.27410 | 0.27393 | 0.27386 | 0.27411 | 0.27521 |
| P, W2 (FY2019) | 0.27983 | 0.27970 | 0.27992 | 0.28097 | 0.28243 |
| P, W3 (FY2023) | 0.30306 | 0.30286 | 0.30237 | 0.30263 | 0.30650 |
| Size, W1 | 0.3223 | 0.3210 | 0.3243 | 0.3205 | 0.3275 |
| Size, W2 | 0.2932 | 0.2932 | 0.2977 | 0.2922 | 0.3015 |
| Size, W3 | 0.3117 | 0.3117 | 0.3114 | 0.3091 | 0.3205 |

The yardstick: three CatBoost seeds at one setting in W3 give 0.30263, 0.30296, 0.30271, a seed standard deviation of 0.00017. Two xgboost seeds differ by 0.0001 to 0.0002. So a gap of 0.0005 is real and a gap of 0.0002 is not. [checked]

Reading the table:

- CatBoost's P equals xgboost's in W1, is behind by 0.0011 to 0.0013 in W2, and is ahead by 0.0002 to 0.0004 in W3. Its size model has the lowest deviance of any learner in all three windows. [checked]
- BART's P is behind xgboost by 0.0011, 0.0026 and 0.0034. That is 6, 15 and 20 seed standard deviations. Its size deviance is the worst of the five in every window. [checked]
- BART's P on the held-back year is at the level of the EBM with pair terms (0.27489, 0.28238, 0.30744). [checked]

### A.2 CatBoost, item by item

| Item | What the record shows | Cost |
|---|---|---|
| Grid edge, P depth | Depth 8 (top of 4/6/8) chosen in W1 and W3; depth 4 (bottom) in W2. In W3 the criterion was still falling from depth 6 to 8. I ran depth 10 in W3: 0.30267 against 0.30263. The curve is flat past 8. [checked, W3 only] | None in W3. W1 not probed. |
| Grid edge, size depth | Depth 4 (bottom of 4/6) chosen in W1 and W3, where xgboost prefers depth 2. A depth 2 or 3 CatBoost size model was never tried. [checked from the files] | Small. CatBoost's size already beats xgboost's. [judged] |
| Early stopping | Largest best round is 1,695 of a 4,000 cap. No fit hit the cap. [checked] | None. |
| Learning rate 0.06 | W3: 0.03 gives 0.30260 against 0.30263. W2: 0.28107 against 0.28153. [checked] | None in W3. About 0.0005 in W2 at depth 8 with indicators. |
| Not tuned: L2 on leaves (fixed at 3), column sampling (off), minimum leaf size | xgboost and LightGBM each had 16 P settings, with column sampling and minimum child weight in the grid. CatBoost had 6. W3: L2 = 10 gives 0.30265, L2 = 30 gives 0.30249, column sampling 0.5 gives 0.30298. W2: L2 = 10 gives 0.28108, L2 = 30 gives 0.28100, column sampling 0.28116, and learning rate 0.03 with L2 = 10 and column sampling 0.5 gives 0.28050 against the chosen 0.28097. [checked] | W3: none. W2: 0.0005 left on the table. CatBoost stays 0.0007 behind xgboost in W2 after it. |
| State as target statistics or indicators | Chosen by the criterion per model. Target statistics won 5 of 6. For P the gaps between the two encodings are 0.0003 to 0.0009, above seed noise. Target statistics without feature combinations (W2, depth 6) gives 0.28119, about the same as with them. [checked] | None. |
| Tweedie 1.9 in place of gamma | The Tweedie 1.9 score equation is the gamma equation with each case weighted by its fitted mean to the power 0.1. CatBoost's held-back gamma deviance is the lowest of all learners in all three windows. [checked] | None. |
| Rounds rescaled by all rows over fit rows | Same rule as B's (`round_scale` in `s46_lib.R`). The factor is 1.95 in W1, 1.49 in W2, 1.26 in W3. On the W1 tuning fit, CatBoost at 1.96 times its best round loses 0.00064 and xgboost at 1.95 times loses 0.00070. That is an upper bound, because the refit has 1.95 times the rows. [checked] | The same for both learners. Not a source of a CatBoost disadvantage. |
| Thread count | The depth 8 fits reproduce the tuning file to five decimals on 6 threads. One depth 6 fit differed by 0.0003 (0.28197 against 0.28171). CatBoost is not bit-reproducible across thread counts for every setting. [checked] | Treat as seed noise. |

Verdict: CatBoost's grid was thin, and it did not matter. A wider grid is worth doing only as part of an arm that is going forward anyway. [judged]

### A.3 BART, item by item

| Item | What the record shows | Cost |
|---|---|---|
| Burn-in | I refit the W1 P model from a cold start and scored each block of 250 sweeps on the held-back year, four chains pooled: 0.27592, 0.27564, 0.27556, 0.27541, 0.27532, 0.27519. It falls in every block. The benchmark kept sweeps 1,001 to 1,500 (0.27521, which reproduces the tuning file). The chain had not settled at sweep 1,000 or at 1,500. [checked, W1 only] | Real. The fall over the last 1,000 sweeps is 0.0004 and has not stopped. The gap to xgboost in W1 is 0.0011. |
| Mixing across chains | One chain scores 0.27591 on the benchmark's kept sweeps; the four pooled score 0.27521. Chain-to-chain score correlation on the test rows is 0.947 to 0.962 (logged), against 0.992 between two xgboost seeds. On the scored test years a single chain reads 29.34 at 10% and the four-chain bag 29.97; at 5%, 16.93 and 17.53. B's single seed and its 10-seed bag read the same (29.46, 29.47). [checked] | Real. Four chains of 500 draws leave Monte Carlo noise in the score. Fitting 1 over the number of chains to the test-year levels puts an unlimited bag about 0.2 above the four-chain bag. [judged from 4 points] |
| Training size | The held-back gap to xgboost grows with the fit rows: 0.0011 at 41,014, 0.0026 at 80,071, 0.0034 at 156,990. The sweeps were fixed at 1,500. [checked numbers] | Consistent with slower mixing on more rows. [judged] |
| Priors: k, power, base, trees | All at the library defaults (k = 2, power 2, base 0.95, 200 trees). Only the size offset was tuned. [checked from the code] I did not vary them; the budget ran out. [not checked] | Unknown. |
| Probit link | A monotone change of scale against logit. [judged] | Negligible for ranking. |
| Lognormal retransformation of size | W3 size model refit, 4,455 held-back error rows. Deviance as scored: 0.32055. After one constant rescale: 0.31425; the predictions are 8.1% low. A gamma regression of dollars on the log fit has slope 1.140 (se 0.033). With level and slope refit the deviance is 0.31288, against xgboost's 0.3117. The log-scale residual standard deviation rises with the fit: 0.42, 0.53, 0.59, 0.65 by quartile. One sigma for all cases is wrong; the smearing factor is 1.16 in the lowest quartile and 1.32 in the highest. [checked] | The level error costs nothing for ranking. The slope does: BART's sizes are too compressed, so its score leans on P more than it should. Small. [judged] |
| Zero-filled blanks | 81, 123 and 210 blank cells in windows of 1.7 to 3.5 million input cells. [checked count] | None. [judged] |

Verdict: BART's figure in the benchmark is a lower bound on what BART can do here. Longer chains, more of them, and a recalibrated size model are the fixes. How much they are worth is in A.4.

### A.4 Does the tuning criterion line up with the test metric?

It lines up loosely, and only for large gaps.

- Eight CatBoost P fits in W3 with held-back log loss between 0.30249 and 0.30298 read between 28.9% and 30.0% on the held-back year's share at 10%, with one size model common to all. A three-seed bag read 28.2%, 0.8 to 1.0 below each of its own seeds. [checked]
- An xgboost P model with no state term is worse on the W3 held-back log loss by 0.0023 (0.30547 against 0.30313) and reads 0.8 points higher at 10% on that year. [checked]
- Across learners on the test years, the additive EBM is 0.003 to 0.0085 worse on held-back log loss and 1.4 to 2.3 points lower on the sample-pooled share at 10%. BART is 0.001 to 0.003 worse and -0.4, +0.2, -0.9 points. As a rough rule, 0.001 of log loss is 0.2 to 0.4 points at 10%. [checked arithmetic; the rule is judged]

Consequences:

- The log loss and the gamma deviance are the right things to tune on. They use every case and resolve 0.0005. The share at 10% on one held-back year moves by a point between settings that are the same model. It cannot be used to choose anything. [checked]
- By the rough rule, CatBoost's untaken 0.0005 in one window is worth under 0.1 point overall. BART's gap of 0.001 to 0.003 is worth 0.2 to 1 point if longer runs close it. [judged]

## B. How big is the noise, and what would a multi-seed test buy?

### B.1 Decomposition

`04_score.R` adds three variances per test and weights the tests 0.2 / 0.4 / 0.4: the paired bootstrap of the difference (cases resampled within state, bags held fixed), the challenger's single-fit seed variance over its number of fits, and B's over 10. I recomputed each part with 1,000 bootstrap draws (`01_noise.R`, `01b_noise_tables.txt`). [checked]

| Difference from B at 10% | Reported se | Bootstrap over cases | Challenger seeds | B seeds |
|---|---|---|---|---|
| CatBoost, 5 fits | 0.54 (mine 0.55) | 0.53 (93% of variance) | 0.13 (5%) | 0.06 (1%) |
| BART, 4 chains | 0.73 (mine 0.75) | 0.71 (91%) | 0.22 (9%) | 0.06 (1%) |
| LightGBM, 10 fits | 0.50 (0.49) | 0.48 (95%) | 0.09 (3%) | 0.06 (2%) |
| xgboost refit, 10 fits | 0.35 (0.35) | 0.34 (93%) | 0.07 (4%) | 0.06 (3%) |

The last row matters. Two 10-seed bags of the same xgboost recipe, with a rank correlation of 0.998, have a paired standard error of 0.34 points at 10%. That is the floor for any comparison on these three tests. The bootstrap part grows as the two rankings differ more: 0.34 for the same recipe, 0.48 for LightGBM, 0.53 for CatBoost, 0.71 for BART. [checked]

Why the floor is high: the map weighting gives California 13% of the weight, New York 9%, Florida 7%. The effective number of states is 18 to 21 of 48. California's 10% list is 73 to 84 cases. Between learners its share of error dollars caught swings by 5 to 10 points (W2: B 22.4%, CatBoost 31.9%, BART 25.8%). [checked]

### B.2 What more seeds give

More challenger seeds shrink the challenger's seed part and, slightly, the bootstrap part (the bag gets closer to its limit). I measured the bootstrap part for bags of 1 to 5 CatBoost fits (0.566, 0.561, 0.550, 0.545, 0.535) and 1 to 4 BART chains (0.743, 0.718, 0.706, 0.711), and extrapolated. [checked]

| Standard error at 10%, against the stored 10-seed B | CatBoost | BART |
|---|---|---|
| Now (5 fits / 4 chains) | 0.55 | 0.75 |
| 10 fits | 0.55 | 0.72 |
| 20 fits | 0.55 | 0.71 |
| 30 fits | 0.54 | 0.70 |
| Unlimited fits, for B too | 0.54 | 0.69 |
| z of a true +0.5 at 30 fits | 0.9 | 0.7 |
| True gain that reads z = 2 at 30 fits | 1.1 | 1.4 |
| True gain with an 80% chance of reading z >= 2 | 1.5 | 2.0 |

One correction, which makes the case for more seeds weaker still. The script's seed term assumes a bag's seed variance is the single-fit variance over the number of fits. It falls more slowly. For B, the seed standard deviation of the 10% share is 0.21 for one seed and 0.17 for a bag of five, where the formula says 0.10. xgboost refit, LightGBM and CatBoost show the same. The cause is the cutoff: a small change in scores swaps cases across the 10% line, and the dollars at stake in a swap do not shrink with the size of the change. With the measured rate the present standard errors are about 0.59 (CatBoost) and 0.8 (BART), and 30 fits give 0.57 and 0.74. [checked for bags up to 5; the extrapolation to 30 is judged]

### B.3 Can a true +0.5 ever clear 2 standard errors on these three tests?

No. Plainly: with unlimited seeds on both sides the standard error at 10% is 0.54 for CatBoost and 0.69 for BART. A true half point reads z = 0.9 or 0.7. The chance that it reads z >= 2 is 14% for CatBoost and 10% for BART. [checked]

Other routes, on the same rankings (bootstrap part only, `01c_noise_extra.txt`) [checked]:

| Statistic | CatBoost: difference, se, z | BART: difference, se, z |
|---|---|---|
| Share at 10%, map weights (the lead metric) | +0.54, 0.53, +1.0 | +0.46, 0.71, +0.6 |
| Mean of the shares at 5% and 10%, map weights | +0.11, 0.38, +0.3 | -0.06, 0.55, -0.1 |
| Share at 10%, sample cases pooled | -0.03, 0.38, -0.1 | -0.39, 0.45, -0.9 |
| Mean of pooled 5% and 10% | 0.00, 0.26, 0.0 | -0.21, 0.34, -0.6 |
| Share at 10%, states weighted equally | -0.35, 0.42, -0.8 | -0.30, 0.53, -0.6 |

- **Pairing** is already in the standard error. Nothing more to gain.
- **Pooling 5% and 10%** lowers the standard error to 0.38 but the gain is all in the 5 to 10% band, so the pooled difference is +0.11.
- **Other weightings** are less noisy (0.38 pooled, 0.26 for pooled 5% and 10%). On those the half point is not there.
- **More test years.** Five tests of this size would give 0.41 for CatBoost; six, 0.38. A true +0.5 would read z = 1.2 to 1.3. FY2018 and FY2023 are the only candidates, and both are the held-back years the settings were tuned on. FY2025, when published, would make four tests and 0.46.
- **A pre-registered direction** changes the bar from 1.96 to 1.645. A true +0.5 clears that 24% of the time.

### B.4 Where the half point comes from

State contributions to the map-weighted difference at 10%, three tests combined (`01b_noise_tables.txt`, section 6). [checked]

| | California | New York | Florida | All other 45 states | Total |
|---|---|---|---|---|---|
| CatBoost | +0.35 | +0.21 | +0.15 | -0.15 | +0.54 |
| BART | +0.48 | -0.03 | +0.32 | -0.30 | +0.46 |

- By test, CatBoost is +1.09, +1.17, -0.35 and BART is +0.07, +1.16, -0.05. The sign fails on FY2024.
- Both are largest on FY2022, the three-years-ahead test the ledger treats as closest to deployment. That gain is two states. California alone gives CatBoost +1.25 of its +1.17 there, and the other 45 states give -0.60. California and Florida give BART +1.09 of its +1.16. With sample cases pooled, the FY2022 differences are -0.18 and +0.16. (`01d_state_check.txt`)
- States ahead and behind B at 10%: CatBoost 29/20, 23/25, 21/28. BART 24/23, 23/25, 20/29.
- The within-state mean change for CatBoost at 10% is negative in all three tests (-0.026, -0.059, -0.030 times random), and 4 to 9 states are harmed. The project's shipping rule (ledger, 2026-08-07) says a headline gain contradicted by the within-state mean and the harmed tail does not ship.
- The CatBoost and BART differences are correlated 0.35 to 0.50 across bootstrap draws. They are not two independent confirmations.

No mechanism would make a learner better in large states. QC samples are about the same size everywhere. The simplest reading is that about 80 flagged cases in California moved. [judged]

### B.5 What could settle it

Nothing on the public file can settle a half point. What can be settled:

- A gain of 1.5 points or more at 10% (80% power at z >= 2), or about 1.1 for an xgboost variant, whose paired standard error is 0.38.
- Whether a learner is worse. The additive EBM was, at -1.9 points.
- A state's own case file, at 40,000 to 100,000 rows, would resolve that state's list far more finely than its 800 QC cases a year. That is where a half-point question can be answered. [judged]

## C. Design of a follow-up

Three facts set the design. The test years resolve about 1 to 1.5 points. The held-back log loss and gamma deviance resolve 0.0005, and those criteria are cheap. The three test years chose B from eight candidates and have now scored eight more learners.

**1. Name the gain that would change the decision before running anything.** I suggest 1 point at 10% with no loss at 5%. A smaller gain cannot be confirmed here and would not justify a second library in the state package. The ledger's rule applies: if the answer would not change a decision, it is not worth a study.

**2. Develop on the training years only. No limit on arms.**

- Each arm is tuned as the benchmark tuned: within state-year log loss for P, gamma deviance for size, on the held-back last training year of each window.
- Arms that change how P and size combine are judged on a smooth dollar criterion: the Poisson deviance of error dollars against the score with one scale per state-year (`pois_dev` in `s46_lib.R`), on the held-back year's kept cases. Measure its seed standard deviation first with five B-recipe seeds; that costs two minutes.
- The share at 5% and 10% on the held-back year is printed and never used to choose.
- An arm is promoted when it beats its reference by more than three seed standard deviations on the criterion. Promotion decisions use FY2018 and FY2023 only. FY2019 is W2's held-back year and also W1's test year, so it stays in W2's own tuning and out of any decision that crosses windows.
- At most one arm per family is promoted, and at most three families.

**3. Freeze before the test years are read.** Appended to `design_note.md`: the promoted arms, the hashes of their tuning files, the number of fits, the unchanged `04_score.R`, and the reading rule below.

**4. One read.** `04_score.R` runs once, for all promoted arms.

- Lead: the difference from the stored 10-seed B at 10%, with the paired standard error. Use the measured seed variance of a bag in place of single-fit variance over the number of fits.
- Ahead: z of at least 1.96 for one arm, 2.24 for two, 2.39 for three (Bonferroni, one-sided 0.025). Also: not more than one standard error below B at 5%, the precision guard as now, and the sample-pooled share at 10% not below B's.
- Otherwise not distinguishable. An arm that is not distinguishable is not rerun with more seeds or new settings.
- State the power in the note. At a standard error of 0.54 and three arms, a true +1.5 clears the bar 65% of the time, +1.0 30%, +0.5 7%.
- An "ahead" is a candidate until FY2025 or a state's file confirms it. That is the benchmark's own rule and it should stand.

**5. Compute.** Five fits per arm is enough for boosted trees; seeds are 5% of the variance. BART needs chains for its level, not for the standard error: 8 chains, 4,000 burn-in sweeps. The benchmark's BART run took 46 minutes for 1,500 sweeps and 4 chains over the three windows, tuning fits included (`logs/03_fit_bart.log`). Three times the sweeps is 2.3 hours; twice the chains on 8 cores adds up to as much again. Allow 3 to 4.5 hours. The whole run fits in one evening.

This is deliberately light. It has one page of pre-commitment and one scoring run.

## D. Specifications I would try

Ranked by expected gain per hour of compute. Gains are at 10% on the lead metric and are judged unless marked.

| Rank | Specification | Reason | Expected gain | Compute | Risk |
|---|---|---|---|---|---|
| 1 | **Calibrated combination: score = P x size^b**, b estimated per window on the held-back year | A quasi-Poisson regression of held-back error dollars on log P and log size, with state intercepts, gives a log P coefficient of 0.97 (se 0.03) and a size exponent relative to P of 1.22 (se 0.08) on FY2023 and 1.26 (se 0.12) on FY2018. Together 1.23 (se 0.07). B's size term is too compressed. [checked, xgboost with B's settings, one seed] | +0.1 to +0.3. On the held-back years b = 1.25 moved the share at 10% by +0.65 and -0.14, and at 5% by -0.48 and +0.41. | Minutes | It may trade 5% for 10%. It changes B, so it is the principal's call. |
| 2 | **Average of B's and CatBoost's scores** (and BART's once rerun) | Rank correlation with B is 0.966 to 0.983 for CatBoost and 0.95 to 0.96 for BART; the errors are partly independent. A three-seed CatBoost bag gained 0.0004 in log loss over one seed; a cross-learner average usually gains more. | +0.2 to +0.5 | 20 minutes for the held-back check; the test scores exist | Two libraries in the package. One more read of the test years. |
| 3 | **BART run properly**: 4,000 burn-in sweeps, 8 or more chains, size recalibrated for level and slope by a gamma regression on held-back predictions | A.3: not converged, Monte Carlo noise in the score, size slope 1.14. [checked] | +0.2 to +1.0, wide | 3 to 4.5 hours | It may land where B is. Mixing on 197,060 rows may need more than 4,000 sweeps; check the block-by-block log loss before scoring. |
| 4 | **CatBoost with a wider grid**: L2 3/10/30, column sampling 0.5/1, learning rate 0.03, size depth 2/3 | A.2: 0.0005 available in W2, nothing in W3. [checked] | Under +0.1 | 1 hour | None. Do it only inside an arm that is going forward. |
| 5 | **Best component from each learner**: xgboost or LightGBM P with CatBoost size | CatBoost's size has the lowest held-back deviance in all three windows, by 0.0005 to 0.0026; its P does not lead. [checked] | Under +0.2 | Minutes | Small gain, one more read. |

Tried here on the held-back years and not worth pursuing:

- **Training weights that match the map's state weights.** Weighting each case by its state's map weight over its training rows, to the power 0.5 or 1, made the within-state log loss worse on FY2023 and FY2018, on the unweighted criterion (0.30313 to 0.30381 and 0.30557; 0.27422 to 0.27478 and 0.27619) and on the same criterion with states weighted as the map weights them (0.3409 to 0.3423 and 0.3450; 0.3024 to 0.3036 and 0.3042). The national model pools across states; upweighting a few states costs effective sample (71% and 34% left) and buys nothing. [checked]
- **A random intercept or other partial pooling of the state's level** (for example `rbart_vi`). Lists are cut within state, so a state's level does not move its ranking. What the state term buys is state-by-input structure: dropping it costs 0.0023 on FY2023 and nothing on FY2018, where the fit has one year. Tree splits on 49 indicators already supply that. [checked for xgboost]

Low prior on the record:

- **Per-state adjustment, strata, and direction-split models.** Findings 46 tried a per-state boosted adjustment (F), household-size strata (D), and a three-class over/under model with a size model per direction (E), all on xgboost. None was chosen over B on these tests. Rebuilding them on CatBoost or BART starts from that result. [read]
- **The engine is worth about a point** (ledger, findings 2, 4, 13, for rule mining). Eight learners here land within about a point of B at 10%, the additive EBM excepted. That is the same finding. [read]

## What I did not check

- BART's priors (k, power, base, number of trees). No fit varied them.
- BART's burn-in trajectory in W2 and W3. W1 only.
- CatBoost in W1, CatBoost's grow policy, and minimum leaf size.
- The size exponent in W2, and on any learner but xgboost.
- The score average on a held-back year.
- One BART probe failed on an indexing error after its fit and was rerun; its 6 minutes are in the compute total. The rerun's fit took 7 minutes, over the 5-minute guide, because the machine was shared.

## A note for `04_score.R`

The seed term (single-fit variance over the number of fits) understates a bag's seed variance by a factor of about 2 at five fits (B.2). The effect on the standard errors in the benchmark is 5 to 10%. No reading changes. For the next run, estimate a bag's seed variance from sub-bags, or state the understatement.

## Files

All under `reviews/scratch_statistician/`.

| File | Content |
|---|---|
| `01_noise.R`, `01b_noise_tables.R`, `01b_noise_tables.txt` | Decomposition of the standard error, seed law, bootstrap by bag size, state contributions |
| `01c_noise_extra.R`, `01c_noise_extra.txt` | Other poolings of the same rankings, power, rolled tests |
| `01d_state_check.R`, `01d_state_check.txt` | State contributions by test |
| `probe_lib.py` | Loader and criteria for held-back probes; drops the test year at load |
| `02_catboost_probe.py`, `02_catboost_probe_W2.log`, `02_catboost_probe_W3.log` | CatBoost P variations |
| `03_bart_probe.R`, `03_bart_P_W1_base.log`, `03_bart_S_W3.log` | BART burn-in and chains; size retransformation |
| `04_structure_probe.py`, `04_structure_probe_W1.log`, `04_structure_probe_W3.log` | State term, state weights, the combination of P and size |
| `05_rounds_probe.py`, `05_rounds_probe_W1.log` | The rounds rescaling |
