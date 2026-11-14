# Review of the xgboost and CatBoost blend, and a design for one more held-out test

Senior-statistician review, fresh context, 2026-11-14. Requested by the project lead after the follow-up run.

Each claim is marked **[computed]** (I ran it from the saved files) or **[inferred]** (my reading). Scripts and outputs are in `reviews/scratch_blend_statistician/`. No model was fitted. Nothing outside that folder and this file was written.

What I read: `README.md`, `design_note_followup.md`, `followup/promoted.json`, `followup/logs/run_score.out`, `05_followup.py`, `07_score_followup.R`, `02_fit.py` (loader), `tuning/catboost_W*.json`, `reviews/followup_statistician_review.md`.

What I did not do: I did not compute the blend's or CatBoost's share of error dollars on FY2018 or FY2023. On those years I computed only how far the two learners' lists agree, without labels. One exception is reported in section 5.

My scorer reproduces the run's levels to the printed digit (B 30.42 / 28.72 / 29.84 at 10% by test; blend 32.06 / 29.85 / 29.68) and its difference (+0.72; bootstrap SE 0.44 without seed terms, against the run's 0.48 with them). [computed]

## 1. Is there a reason to expect the blend to rank better?

### The argument for

- Averaging fits of one learner removes only the randomness of row and column sampling. Two 5-fit xgboost bags share 95 to 96% of the 10% list. An xgboost bag and a CatBoost bag share 77 to 81% on the test years and 75 to 80% on FY2018 and FY2023. Among cases in either learner's top 20%, the within-state rank correlation is 0.51 to 0.63 between learners and 0.98 between xgboost bags. [computed]
- CatBoost builds symmetric trees, orders its boosting, and codes the state by target statistics in most of its models. Its errors are partly its own. When two models of similar accuracy disagree this much, their average fits better. That is arithmetic for a convex loss.
- The files show it. Against an average of two xgboost fits with the same inputs and exponent, the blend's dollar deviance is lower by 0.158 (z -3.2). The log loss of P is lower in all three test years (0.28031 to 0.27979, 0.29851 to 0.29804, 0.31484 to 0.31468), although CatBoost alone is worse than xgboost in all three. The same holds on B's own inputs without any promoted piece (deviance z -3.6). [computed]

So the better fit is real and is due to the second learner.

### The argument against

- The objective is not fit. It is the dollars on the cases above each state's cut line. In this benchmark fit has bought little share. Across the 12 arms scored on the test years, 0.001 of log loss goes with 0.19 points at 10% and 0.29 on the mean share; 0.1 of dollar deviance goes with 0.06 to 0.09 points. [computed] The blend's log loss is 0.0004 below matched xgboost and 0.0008 below the twin. At that rate the expected gain is 0.1 to 0.25 points, not 0.7. [inferred]
- The structural xgboost arm is the precedent. It fitted better (deviance z -3.2) and its share at 10% did not move (-0.03).
- Across bootstrap resamples of cases, the blend's deviance advantage and its share advantage at 10% are nearly unrelated: correlation -0.11 to -0.15 in each test year. [computed] A sample in which the blend fits better is barely more likely to be one in which it ranks better at the top.
- Both learners are boosted trees on the same 18 to 21 inputs. Much of their disagreement is estimation noise, which more training rows also remove. Agreement between the two rises with training rows: on the held-back years 75% of the 10% list at 41,014 rows and 80% at 156,990; on the test years 77% at 80,071 and at 119,310 rows and 81% at 197,060. The blend's difference by test runs the same way: +1.00, +1.57, -0.21. The deployed model trains on more rows than any test. [computed numbers; the trend rests on three points]

### What the saved files show

All figures are the blend minus an average of two xgboost fits with the same inputs and exponent, so the blend gets no credit for the number of fits. Three tests combined 0.2 / 0.4 / 0.4; bootstrap SE over cases; seed terms would add 7 to 9%. [computed]

| Question | Figure |
|---|---|
| Does the blend beat averaging two fits of one learner? | At 10%: +0.74 (SE 0.40, z 1.9). Mean share at 5 / 7.5 / 10%: +0.44 (0.23, z 2.0). At 5%: +0.04. Sample-pooled at 10%: +0.33 (0.27). States weighted equally: +0.19 (0.34). Averaging two xgboost fits instead of bagging their scores changes nothing (0.00). |
| Is it in P, in size, or in the product? | Blended P with xgboost size: +0.46 at 10%, +0.47 on the mean share (z 2.1). Xgboost P with blended size: 0.00 and +0.02. What there is sits in P. |
| Is it spread over years? | FY2019 +1.00 (0.59), FY2022 +1.57 (0.74), FY2024 -0.21 (0.59). Nothing in the test with the most training rows. |
| Is it spread over states? | 27 of 48 states contribute a positive amount. California gives +0.31 of the +0.74. Without the three largest contributors it is +0.18. A state's difference in one test does not predict its difference in another (correlations +0.10, +0.16, -0.35). States up / down: 29/15, 23/20, 21/23. |
| How many cases carry it? | The blend swaps 9.5 to 11% of the 10% list, 385 to 439 cases a year. Swapped in against swapped out, share that are errors: 22.1% against 15.0% (FY2019), 22.5% against 17.1% (FY2022), 20.8% against 22.1% (FY2024). Error dollars: $13,623 against $8,690; $16,008 against $13,608; $15,915 against $17,227. |
| Does it survive small changes to the recipe? | No. Same fits with exponent 1 in place of 1.153: 0.00 at 10% (SE 0.39), +0.29 on the mean share. B's inputs with exponent 1: +0.20 and +0.08. B's inputs with exponent 1.153: +0.18 and +0.09. Mean of the four variants: +0.28 at 10% (SE 0.29), +0.23 on the mean share (0.17), +0.09 sample-pooled (0.19). The as-scored figure differs from the exponent-1 figure by 0.74 (SE 0.34, z 2.2). |
| Mixing weight | CatBoost weight 0, 0.25, 0.5, 0.75, 1: at 10%, 29.48, 29.73, 30.26, 30.12, 30.04; sample-pooled, 30.57, 30.87, 30.99, 30.74, 30.19; states weighted equally, 30.80, 30.96, 31.15, 30.76, 30.05. The middle is highest on all three. On the map weights CatBoost alone is also above xgboost (+0.56); on the other two bases it is below (-0.38, -0.74). |

### Reading

- A blend of two different learners should fit better, and it does. That part is settled. [computed]
- Whether it ranks better at the top is not settled. The evidence leans positive: for the recipe as scored the sign is positive against matched xgboost on every basis at 10%, the middle mixing weight is best on every basis, and the swapped-in cases are more often errors in two years of three. [computed]
- The size is probably well under the headline. The +0.72 was the largest of the run's arms, it depends on the exponent, FY2024 shows none of it, and the fit improvement predicts 0.1 to 0.25 points. My estimate of the true gain at 10% is 0.2 to 0.4 points. [inferred]
- Two of the README's supporting figures are not the blend's alone. Against B the blend is +0.99 without the three largest states and +0.83 sample-pooled (z 2.3 each). The promoted xgboost pieces alone give +0.51 and +0.49 of those. Against matched xgboost the blend adds +0.47 (z 1.3) and +0.33 (z 1.2). The lead figure at 10% is the reverse: the xgboost pieces give -0.03 and the second learner gives all of it. [computed]

## 2. The best additional held-out test

### The candidates

| Candidate | How clean | Cost | Verdict |
|---|---|---|---|
| **FY2017, backward.** Train FY2018-19 (78,296 rows), score FY2017 (41,014 rows). | No FY2017 label has chosen anything. Settings, rounds, promoted pieces, the exponent and the blend's promotion all used FY2018, FY2019 or FY2023 as the judged year. Time runs backward within the pre-pandemic era, so it says nothing about drift. It does test whether a second learner ranks unseen cases better. | New fits at the first test's size: 5 xgboost and 5 CatBoost. Minutes for xgboost, about 15 for CatBoost. | Use. The only unselected year. |
| **FY2018 and FY2023, the saved held-back predictions.** Forward: trained on FY2017 (41,014 rows) and on FY2017-19 plus FY2022 (156,990 rows). | Those years' labels chose the rounds (first seed, both learners), CatBoost's settings (6 P and 8 size settings) and, by `02_fit.py`'s header, B's own, the promoted pieces, the exponent, and the blend itself. Each of these either touches both arms alike or was judged on fit. Promotion on deviance says almost nothing about the share (case-level correlation -0.11 to -0.15), so the bias from having passed promotion is under 0.05 points [inferred]. The exponent was estimated on these two years from xgboost's own P and size; the direction of any bias from that is unclear. | None. Files exist. | Use. Not pristine, but the leaks are small beside an SE of 0.4 to 0.5 and do not favor one arm. |
| **FY2018 and FY2023, new forward fits with rounds chosen elsewhere.** | Removes the early-stopping leak only. Settings, pieces and exponent still come from those years. FY2018 has no earlier year to stop on. | About an hour of CatBoost, and new choices about rounds. | Not worth it. [inferred] |
| **Leave-one-year-out.** | For FY2018 and FY2023 it has the same selection history and loses the forward direction. For FY2019, FY2022 and FY2024 it reuses spent labels. For FY2017 it is the backward test, with post-pandemic training years added for no reason. | New fits on five years each. | No. |

### Recommended design

One read of three years: FY2017 (new backward fits), FY2018 and FY2023 (saved predictions).

- **Arms, in every year.** Blend: bag of 5 members; member i averages P and size over xgboost fit i and CatBoost fit i; score P x size^1.153. Reference: the same with xgboost fits i and i+1 (mod 5). This is the contrast `promote()` used.
- **Files.** FY2018: P from `xgb_W1_ratio.npz`, size from `xgb_W1_base.npz`, CatBoost from `cb_W1_base.npz`. FY2023: P from `xgb_W3_ratio_era.npz`, size from `xgb_W3_era.npz`, CatBoost from `cb_W3_era.npz` (the recipe scored on FY2024).
- **FY2017 fits.** Rows from `data/W1.csv`: train on FY2018 and FY2019, score FY2017. The first test's settings for both learners (ratio inputs for xgboost P, B's inputs for size, CatBoost at `tuning/catboost_W1.json`). Rounds: the first test's test-fit round counts times 78,296 / 80,071. No early stopping. Seeds 117 to 121. The percentile inputs are ranks within state and year, so they do not depend on the window. [computed from `s46_lib.R`]
- **Statistic.** Map-weighted share of error dollars, each year's own FNS error rates times FY2024 issuance, 48 states. Blend minus reference at 10%, and on the mean of the shares at 5%, 7.5% and 10%.
- **Weights.** FY2017 0.25, FY2018 0.25, FY2023 0.5. FY2023 has the training size and era nearest to deployment, and FY2024 is where the test years showed nothing.
- **Standard error.** Paired bootstrap of cases within state, plus half-bag seed terms, as in `07_score_followup.R`.

### Power

Per-year bootstrap SD of the blend-minus-xgboost difference at 10% on the test years is 0.59, 0.74 and 0.59. With a 15% allowance on the variance for seeds: [computed from the test years; the transfer to the validation years is inferred]

| Design | SE at 10% | SE, mean share |
|---|---|---|
| One year alone | 0.69 | 0.40 |
| FY2018 + FY2023, weights 1/3 and 2/3 | 0.51 | 0.30 |
| FY2017 + FY2018 + FY2023, equal weights | 0.40 | 0.23 |
| FY2017 + FY2018 + FY2023, weights 0.25 / 0.25 / 0.5 | 0.42 | 0.25 |

The three-year design is about as precise as the three test years were. Adding FY2017 takes the SE from 0.51 to 0.42.

### Reading rule

Write this into the design note before the read.

> **Promising:** over the three validation years, the blend's share at 10% is at least 0.25 points above the matched xgboost arm's, and the mean of its shares at 5%, 7.5% and 10% is above the matched arm's. **Not promising:** anything else.

Why 0.25: it is the midpoint between no gain and half a point. A result at or above it is more likely under a true half-point gain than under none. The mean-share condition screens out a difference that sits only on the cases at the 10% line.

Chance the rule reads "promising", by the true gain at 10% (simulated from the test years' bootstrap covariance; the true gain on the mean share taken as 0.6 of the gain at 10%, the test years' ratio): [computed under those assumptions]

| True gain at 10% | 0 | 0.25 | 0.5 | 0.75 | 1.0 |
|---|---|---|---|---|---|
| Proposed rule, three years | 23% | 45% | 69% | 87% | 96% |
| Proposed rule, FY2018 + FY2023 only | 25% | 44% | 64% | 81% | 92% |
| Looser: above 0 at 10% and on the mean share, three years | 36% | 61% | 82% | 94% | 99% |
| z of 1 at 10%, three years | 16% | 34% | 57% | 78% | 92% |
| z of 1.645 at 10%, three years | 5% | 15% | 32% | 55% | 77% |
| z of 1.96 at 10% (the run's bar), three years | 2% | 9% | 22% | 43% | 66% |

The proposed rule passes a true half point about 7 times in 10 and a null arm about 1 time in 4. If the project lead would rather pass a true quarter point 6 times in 10, the looser rule does that and passes a null arm 36% of the time. Choose between them before the read. The power figures move by 5 points or less if the mean-share gain is 0.4 or 0.8 of the gain at 10%.

Report beside the reading, with no bearing on it: each year's difference (FY2023 first); the same contrast with exponent 1; the blend against the B-recipe twin (`xgb_W*_base.npz`); the sample-pooled share at 10%; the difference at 5%.

### What the result can and cannot establish

- **A pass** means the sign and rough size held on years that did not produce the +0.72. With a validation result of +0.25 the precision-weighted estimate over all six years is +0.50 (SE 0.30); with +0.50 it is +0.62. [computed] That z overstates the case, because the test-year half was the best of the run's arms. A pass is grounds to keep the blend in `/methods` as a candidate. It is not confirmation.
- **A fail** does not show there is no gain. A true half point fails 3 times in 10. With the test years at +0.74 and a validation result of 0, the six-year estimate is +0.37 (SE 0.30).
- **Neither outcome** covers deployment conditions: training through FY2024 and scoring three years ahead. FY2025 or a state's own file is still the confirmation.
- No test on these years can resolve a gain of 0.2 to 0.4 points, which is my estimate of what is there. The rule sorts "about half a point or more" from "about nothing".

## 3. Things to check

1. **The exponent.** The blend's +0.74 against matched xgboost is 0.00 with exponent 1 on the same fits (difference 0.74, SE 0.34). The exponent lowers xgboost's own share at 10% by 0.31 and raises the blend's by 0.43. I found this after looking at many contrasts, so it may be chance. If it is not, the blend and the exponent are one recipe and should be kept and tested together. The exponent was estimated on FY2018 and FY2023; only FY2017 is free of that.
2. **FY2024.** The blend is -0.21 (SE 0.59) against matched xgboost and -0.15 against B in the test with the most training rows. FY2023 is the validation year most like it. Read its figure first.
3. **Held-back shares already on disk.** `followup/heldback/criteria_*.csv` holds map-weighted and pooled shares on FY2018 and FY2023 for each xgboost and CatBoost arm (bag rows), and the held-back logs print them. The blend's were never computed. FY2018 and FY2023 are unread for the blend, not for CatBoost alone.
4. **The README's "z +2.3" figures.** Half of each comes from the promoted xgboost pieces (section 1, last bullet). The blend-specific figures are z 1.3 and 1.2.

## 4. Files

In `reviews/scratch_blend_statistician/`:

| File | What it holds |
|---|---|
| `lib.py` | Shares, deviance, log loss and the within-state bootstrap, following `07_score_followup.R` |
| `01_decompose.py`, `.txt` | Arms rebuilt from the saved fits; differences with 2,000-draw bootstrap SEs; by test; log loss |
| `02_overlap.py`, `.txt` | Agreement of the two learners' lists on held-back and test years; the swapped cases on test years |
| `03_fit_vs_share.py`, `.txt` | Fit against share across the 12 arms scored on the test years |
| `04_by_state.py`, `.txt` | State contributions |
| `05_power.py`, `.txt` | Variant sensitivity, validation SEs, and the rules' operating characteristics |

## 5. One disclosure

While checking the layout of `followup/heldback/criteria_cb_W1.csv` I printed its first rows, which include the CatBoost base bag's shares on FY2018 (one row). I saw no xgboost figure and no blend figure for either held-back year, and I did not use the number. It does not bear on the blend-minus-xgboost difference the rule reads.
