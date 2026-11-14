# The xgboost and CatBoost blend: is it worth carrying forward, and how to test it once more

Principal data scientist review, 2026-11-14. Fresh read. Nothing here is a finding until the project lead logs it.

Read first: `methods/findings_ledger.md` and `known_constraints.md` in `snap_qc_v27`; findings 46, 48, 51; this folder's `README.md`, `design_note_followup.md`, `followup/promoted.json`, `followup/logs/run_score.out`, `05_followup.py`, `07_score_followup.R`.

Scripts and output: `reviews/scratch_blend_principal_ds/` (`common.py`, `01_decompose_test.py`, `02_disagree.py`, `03_bench_coverage.R`, and their `.out` files). No model was fitted. Every arm below is rebuilt from the saved per-seed P and size.

What I did and did not read. Test years (FY2019, FY2022, FY2024): descriptive work with labels; my script reproduces the scorer's figures for B, the twin, `xgb_rev`, `cb_rev` and the blend to two decimals. Held-back years (FY2018, FY2023): predictions, state and the kept flag only. I computed no share of error dollars, error rate or deviance there, and I did not open the share columns that already exist for CatBoost (see section 5).

Marks: **(computed)** is from my scripts or the run log; **(inferred)** is my judgment.

## 1. Short answer

- The blend improves the fit, and that part is established. It is not established that the better fit moves the share of error dollars at the top of the list. The gain from adding CatBoost to the same xgboost fits is about +0.3 to +0.7 points at 10%, at 1 to 2 standard errors, with nothing at 5% (computed).
- The headline of +0.72 against B overstates CatBoost's part. The blend as scored is four changes at once. About +0.28 of the +0.72 comes from the xgboost revision alone (computed).
- There is a conceptual reason to expect a small gain. My expectation is a few tenths of a point at 10%, not 0.7 (inferred).
- It is reasonable to keep it as an untested option. It is not reasonable to expect any test on the years available to confirm it. A second set of three years has the same standard error as the first.

## 2. What the blend is, and what part of the gain is CatBoost

The scored blend averages `xgb_rev` and `cb_rev`. So it carries ratio inputs in the P model, the era indicator (FY2024 test), the size exponent of 1.153, and CatBoost. Three tests combined 0.2 / 0.4 / 0.4, map weights (all computed):

| Arm | At 5% | At 10% | Mean of 5/7.5/10% | 10% without big three | Sample-pooled 10% |
|---|---|---|---|---|---|
| B, stored | 18.12 | 29.51 | 23.87 | 29.56 | 30.07 |
| Twin of B (`xgb_es`) | 18.34 | 29.56 | 24.05 | 29.97 | 30.18 |
| Revised xgboost, no exponent (`xgb_rev_b1`) | 18.20 | 29.79 | 24.06 | 30.21 | 30.57 |
| Revised xgboost with exponent (`xgb_rev`) | 18.22 | 29.47 | 23.89 | 30.07 | 30.56 |
| CatBoost alone (`cb_rev`) | 18.06 | 30.12 | 24.26 | 30.01 | 30.29 |
| Blend, no exponent | 18.42 | 29.79 | 24.35 | 30.37 | 30.70 |
| **Blend as scored** | 18.26 | 30.22 | 24.33 | 30.55 | 30.89 |
| Plain blend: twin + last night's CatBoost, no exponent | 18.30 | 29.76 | 24.11 | 29.90 | 30.22 |

What CatBoost adds to the same xgboost fits (paired case bootstrap, 400 draws, no seed term, so the standard errors are slightly low):

| Contrast | At 5% | At 10% | Mean share | Without big three | Pooled 10% |
|---|---|---|---|---|---|
| Blend minus `xgb_rev` (same exponent) | +0.04 (0.32) | +0.75 (0.40), z 1.9 | +0.45 (0.23), z 1.9 | +0.48 (0.35) | +0.33 (0.28) |
| Blend minus `xgb_rev_b1` | +0.05 (0.34) | +0.43 (0.41), z 1.1 | +0.27 (0.25), z 1.1 | +0.33 (0.37) | +0.32 (0.29) |
| Plain blend minus twin | -0.04 (0.30) | +0.20 (0.40), z 0.5 | +0.06 (0.22), z 0.3 | -0.08 (0.36) | +0.04 (0.26) |
| Blend minus CatBoost alone | +0.20 (0.42) | +0.10 (0.45) | +0.07 (0.31) | +0.54 (0.41) | +0.60 (0.30), z 2.0 |

Readings:

- **The two robustness figures quoted for the blend are mostly the xgboost revision.** Without the three largest states the blend is +0.99 against B; `xgb_rev_b1` alone is +0.65. Sample-pooled, +0.83 against +0.51. CatBoost's own part is about +0.3 on each, at about one standard error.
- **The exponent moves the 10% share in opposite directions in the two arms**: -0.32 for xgboost, +0.43 for the blend. The mean share does not move (24.35 and 24.33). The 10% figure is the noisier read, and the blend's +0.72 depends on it.
- **Averaging two xgboost fits adds nothing.** A two-xgboost average built the way the blend is built scores the same as the xgboost bag (difference 0.00 at 10%, SE 0.03). Seed noise is already gone from a ten-fit bag. Whatever the blend adds is not averaging alone.
- **The gain is in P, not size.** Averaging P only: +0.55 at 10% and +0.53 on the mean share (z 2.3) against the same xgboost fits. Averaging size only: +0.04 and +0.07.
- **The fit gain is real.** Dollar deviance against the two-xgboost average: -0.146 (z -3.1); against CatBoost alone, z -7.1. P log loss 0.30105 against 0.30140 (xgboost) and 0.30239 (CatBoost).
- **A mixing weight between the two is best near one half** on all three bases (CatBoost weight 0, 0.25, 0.5, 0.75, 1: 29.39, 29.69, 30.22, 30.06, 30.03 at 10%; pooled 30.62, 30.88, 30.89, 30.62, 30.21). The margins over the better single learner are 0.1 to 0.3.
- **By year, against `xgb_rev` at 10%**: FY2019 +1.00 (0.60), FY2022 +1.58 (0.74), FY2024 -0.21 (0.62). The three do not differ by more than chance (chi-square 3.8 on 2 df). FY2024 is the test with the most training rows and the one nearest the deployed fit.
- **By state**: 29 up and 15 down in FY2019, 23 and 20 in FY2022, 21 and 23 in FY2024. Leaving out one state, the combined gain against `xgb_rev` runs from +0.44 (without California) to +0.82; against B, +0.54 to +0.83. A state's gain in one year does not predict its gain in another (correlations +0.11, +0.16, -0.35).

## 3. Is there a conceptual reason to expect the blend to rank better?

**How the two learners differ as configured.**

- Trees. xgboost grows depth-wise trees of depth 4 to 6 with a different split at every node, on 50 to 80% of columns. CatBoost grows symmetric trees (one split per level), depth 8, 4 and 8 for P in the three tests. Symmetric trees are a stronger constraint and give a smoother P.
- States. xgboost gets 49 indicators, so a state-specific pattern costs one split per state. CatBoost used ordered target statistics for the state in the P model of the FY2019 and FY2022 tests and in every size model, which lets one split group states by their error rate. In the FY2024 test its P model chose indicators, like xgboost.
- Regularisation and sampling. xgboost: lambda 5, row sampling 0.8, eta 0.03. CatBoost: default leaf penalty, Bayesian bootstrap, random score noise. CatBoost's size model uses a Tweedie loss, not gamma.

**Where they disagree (computed).**

- More than seeds do, by a wide margin. Two-fit bags of xgboost share 92 to 94% of the top 10% flags; xgboost and CatBoost share 76 to 81% (71 to 76% at 5%). Within-state rank correlation 0.956 to 0.969 against 0.997. The same holds, label-free, on FY2018 and FY2023 (75% and 81% in common at 10%).
- In P. The within-state SD of log(CatBoost score / xgboost score) is 0.20 to 0.30; the P part is 0.18 to 0.28 and the size part 0.07 to 0.10. Seed noise is 0.045.
- Everywhere in the list. The SD is no larger in the top 5% than in the bottom half.
- Not on an identifiable kind of case. No input has a rank correlation above 0.16 with the disagreement (homeless and the utility tier in FY2019; under 0.11 in the other years). CatBoost pulls xgboost's extremes in a little (correlation -0.27 to -0.33 with xgboost's own log P; SD of logit P about 5% smaller).
- Neither is better calibrated at the top. In the top 5% by P, predicted and observed error rates are 0.314 / 0.351 (xgboost) and 0.290 / 0.342 (CatBoost) in FY2019, 0.331 / 0.321 and 0.332 / 0.314 in FY2022, 0.385 / 0.388 and 0.381 / 0.387 in FY2024.

**The argument for.** Two learners of nearly equal accuracy (P log loss 0.3014 and 0.3024; share at 10% 29.8 and 30.1) that disagree three times as much as seeds do is the standard case in which an average beats both. Each carries errors that come from its own functional form, a bag of one learner cannot remove them, and the average halves the part they do not share. The data fit this: the deviance falls below both components, the best mixing weight is interior, and same-learner averaging gives zero. The state coding is a real difference in what each learner can express with 49 states and as few as 30 training errors in a state.

**The argument against.**

- The objective is the top 5 to 10% within a state, and the criterion that improved covers the whole list. In this benchmark a better fit has not moved the share: `xgb_rev` improved the deviance by z -3.2 and moved the share at 10% by -0.03. Findings 46 already stacked a second learner on B (candidate C, with a ranger forest): Q 3.498 against B's 3.502. The ledger's long-standing reading is that the engine is worth about a point and the leverage is elsewhere.
- The disagreement is not concentrated at the top and is not tied to a kind of case, so it looks like functional-form noise spread over the list, not one learner seeing something the other cannot. Averaging that helps a little everywhere.
- At 10% the blend replaces about 400 of 3,900 flags a year (9.5 to 11.2%). Over three years the cases entering hold 269 error cases and those leaving 220, on 1,233 swaps. At 5% the sign reverses: 149 entering, 184 leaving, on 749 swaps. That is a move of error cases from the top 5% into the 5 to 10% band plus a gain at the 10% line, the same pattern CatBoost and BART showed alone on the first night. It is what a smoother P would do: xgboost's most confident cases are right more often than the blend's replacements, and the blend is better where both are unsure.
- The gain is absent in FY2024, where CatBoost coded states as xgboost does, the training set is largest and the two learners agree most (81% of flags in common). The deployed fit uses 231,602 rows and looks like that test. One year cannot settle this, but it points to a smaller gain at deployment.

**My reading (inferred).** A real, small gain in P is likely. On the share at 10% I would expect +0.2 to +0.4 points, with little or nothing at 5%. The +0.72 is the top of that range plus the xgboost revision plus a favourable exponent interaction.

## 4. Which further held-out test, and what should count as promising

**What the benchmark file and scorer cover (computed, `03_bench_coverage.R`).** `eval_benchmark_f1109.rds` has every case of all six years with the same kept rule: FY2017 41,014 cases (40,057 kept, 3,207 kept errors), FY2018 39,057 (37,944; 3,256), FY2023 40,070 (39,061; 3,980). All have 49 states, 48 with a map weight; FNS error-rate files exist for all six years. `score_ranking()` takes any year. So any of the three unspent years can be scored exactly as the three tests were. There is no stored B for them; the twin (`xgb_es` recipe) is the stand-in, and it matched B within +0.05 on the three tests.

**Which contrast.** Read two on any new year: blend minus twin (the headline as before) and blend minus the same xgboost fits without CatBoost (what the second learner adds). Judge "promising" on the second. Reasons: the lead's decision is whether to carry a second learner; and on FY2023 the first contrast is not clean, because the era indicator inside the blend's xgboost half was chosen with FY2023's share in view (the earlier review reports +1.8 points at 10% on held-back FY2023 for era handling). Both sides of the second contrast carry that piece, so it cancels.

**The candidates.**

| Candidate | What it shows | What it does not | Comparable to B's tests? |
|---|---|---|---|
| **FY2023 forward, new fits**: train FY2017-19 + FY2022, rounds and CatBoost settings chosen with FY2022 held back, xgboost at B's FY2024-test settings | One year ahead, pooled eras, 156,990 training rows. This is B's FY2024 test moved back a year | One year only: SE of the CatBoost contrast about 0.65 at 10%, 0.38 on the mean share. FY2023 labels chose B's FY2024 settings, the era piece, the exponent and the blend's promotion (on deviance, not share) | **Yes.** Same protocol, file, scorer and weights |
| Existing held-back predictions, FY2023 | Nearly the same thing as the row above: same training rows, same scored cases | Rounds were early-stopped on FY2023 itself, CatBoost's settings were chosen on it, and the xgboost pieces were promoted on it. Reading it spends FY2023 for the clean version | Partly. It is the row above done less cleanly |
| FY2018 forward or its held-back predictions | A small-data regime: 41,014 training rows, one year, as few as 16 errors in a state | B is never fitted on one year and its settings method needs two. A regularised learner should help most here, so it would flatter the blend against deployment | No |
| FY2017 scored backward: train FY2018-19 + FY2022-24 | The only year never scored or held back by anything here. About 196,500 training rows, near the deployed fit | The project tests forward only; inflation and policy drift run one way. Shows that the gain repeats on unseen cases, not that it carries forward | No, but clean |
| FY2018 leave-one-year-out: train the other five years | A second clean-fit year at deployment scale (about 198,500 training rows) | Interpolation between years, which is easier than a forward test. FY2018 labels were used in promotion (deviance) | No |

**Recommendation.** Run FY2023 forward as the comparable test. In the same run, fit FY2017 backward and FY2018 leave-one-year-out as companions, with settings frozen (B's FY2024-test settings, CatBoost's FY2024-test settings, rounds scaled by rows) so nothing is chosen on a scored year. Do not read the existing held-back shares first; the FY2023 refit costs under an hour and is the same test without the early stopping on the scored year. Ten xgboost and five CatBoost seeds, as before. Arms: twin; plain blend (twin fits plus CatBoost, no exponent); the promoted form with and without CatBoost at the frozen exponent of 1.153.

**What one more year can and cannot do.** From the test years, the CatBoost contrast in a single year has SE 0.60 to 0.75 at 10% and 0.34 to 0.44 on the mean share (computed). A true gain of +0.3 on the mean share is under one standard error in one year. Three new years together have SE about 0.38 at 10% and 0.22 on the mean share (inferred from the per-year figures), the same as the three spent tests. No test on these data will confirm a gain of this size at two standard errors. A new test can contradict the result, or fail to.

**Proposed rule, written before the read.** Statistic: the CatBoost contrast in the promoted form (blend minus the same xgboost fits), map weights, mean of the shares at 5%, 7.5% and 10%, paired case bootstrap as in `07_score_followup.R`.

- **Promising**: the mean-share contrast is at least +0.2 points (about half of what the three tests showed), the contrast at 10% is not negative, and the guards hold (5% not more than 1.645 SE below; sample-pooled 10% not below). The plain blend against the twin must not be negative on the mean share.
- **Supported** (the level the lead asked to keep): promising, and the contrast pooled over all years read, spent and new, is at least 1.645 SE above zero on the mean share.
- **Not promising**: the mean-share contrast is zero or negative. Leave the blend in this folder.
- Between 0 and +0.2: the lead's call; report it as unresolved.

Error rates of the "promising" line (inferred, normal approximation): with the three new years pooled with equal weights, an arm with no gain passes about 18% of the time, a true +0.3 about 67%, a true +0.45 about 87%. With FY2023 alone: about 30%, 60% and 75%. The bar is low on purpose. If the lead will accept only the comparable test, FY2023 alone is a weak filter and should be read as such.

## 5. Things to check

- **CatBoost's shares on FY2018 and FY2023 are already written down in three places**: the table near line 106 of `reviews/followup_principal_ds_review.md`, `followup/logs/05_followup_hb_cb.log` ("printed only"), and the `map5`, `map10`, `pooled5`, `pooled10` columns of `followup/heldback/criteria_cb_W1.csv` and `_W3.csv`. I did not read the values. The blend's own share there has not been computed. If whoever writes the rule has seen CatBoost's figures, those years are not unread for the blend's CatBoost half. FY2017 has no such problem.
- **The README's blend line** reads as CatBoost's contribution. It should say that the arm includes the xgboost revision and the exponent, and give the contrast against `xgb_rev` (+0.75 at 10%, z +1.7 in the scorer) and against `xgb_rev_b1` (+0.43).
- **Which blend would be kept.** The promoted form brings ratio inputs (the ledger's zero-fill and pile cautions apply), the era indicator and the exponent, none of which earned a place alone (`xgb_rev` -0.03). The plain form (B's fits plus CatBoost) is the one that could sit beside B without them, and on the test years it is +0.25 against B at 10% (z 0.6) and +0.06 against the twin on the mean share.
- **CatBoost's state coding differs by test** (target statistics in two, indicators in FY2024), chosen on the held-back year. A deploy fit would need the choice fixed.
- My standard errors omit the seed term; the scorer's are 5 to 15% larger (0.48 against my 0.43 for blend minus B at 10%).

## 6. Cost of carrying it

- **In /methods.** B's code there is R (`case_predictions/s46`), and the benchmark is Python. Keeping the blend means a new study folder with a fitting script, a pinned CatBoost (1.2.10 here), the 17-digit export or the rds (the ledger's CSV hazard), scoring through `eval_benchmark/score_ranking.R`, and a README that says "option under test". Under `known_constraints.md` a new study script needs the four-item design note and a fresh statistician review. As an exploratory result it gets an open-work line in the ledger, not a claim row.
- **Fit time is not a cost.** CatBoost took 1 to 3 minutes a fit here; xgboost 12 to 14 seconds.
- **The state package is the cost.** `snap_case_model` installs one CRAN package (xgboost) and has been checked to reproduce the repo's scores in 49 states. A blend adds a second model format and a second runtime; CatBoost's R package is not on CRAN as far as I know (not checked today). The missing-column test of findings 51 (fill with 0) was run for xgboost only and would have to be redone. The explanation to an agency grows from one model to an average of two.
- **Against that**: a gain I would put at +0.2 to +0.4 points at 10% and none at 5%, on a base of 29.5. B's lead over the workbook vote score is 3.8 points at 10%. I would keep the blend as a documented option for others to test and would not put it in the state package on this evidence.
