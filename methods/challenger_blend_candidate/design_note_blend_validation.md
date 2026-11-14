# Blend validation: one more held-out test of the xgboost and CatBoost blend

2026-11-14. Written before any validation figure was computed. Follows `design_note_followup.md`.

## 1. Question

The follow-up run's blend arm (P and size averaged over one xgboost fit and one CatBoost fit, score = P x size^1.153) was +0.72 points at 10% against B on the three test years (SE 0.48, z +1.5). It did not clear the bars set before that run. Eric asked whether there is a conceptual reason to expect the combination to rank better, and asked for a check that it is "at least promising on at least one other held-out test" before the blend is kept in `/methods` for others to test.

## 2. What the two reviews said

`reviews/blend_review_statistician.md` and `reviews/blend_review_principal_ds.md` (the principal data scientist read the findings ledger first). Both worked from saved files and fitted nothing.

- There is a reason to expect a small gain. An xgboost bag and a CatBoost bag share 76 to 81% of the top-10% list; two xgboost bags share 92 to 96%. The two learners are about equally accurate and disagree three times as much as seeds do, which is the case where an average helps. The blend fits better than an average of two xgboost fits (dollar deviance z -3.2); averaging two xgboost fits adds 0.00 to the share. The gain is in P, not size.
- The +0.72 overstates it. Against the same xgboost fits the blend is +0.74 at 10% (SE 0.40) and +0.04 at 5%. By test year: +1.00, +1.57, -0.21 (FY2024). With exponent 1 in place of 1.153 on the same fits: 0.00. On B's own inputs: +0.20. Better fit has bought little share in this benchmark. Both reviewers expect a true gain of 0.2 to 0.4 points at 10% and nothing at 5%.

## 3. Design (the statistician's, adopted as written)

One read of three years. In every year the contrast is the one the promotion step used: the blend minus a matched two-fit xgboost average, so the blend gets no credit for the number of fits.

- **Arms.** Blend: bag of 5 members; member i averages P and size over xgboost fit i and CatBoost fit i; score P x size^1.153. Reference: the same with xgboost fits i and i+1 (mod 5).
- **FY2017, backward, new fits.** Rows from `data/W1.csv`: train on FY2018 and FY2019 (78,296 rows), score FY2017 (41,014 rows). The first test's settings for both learners (ratio inputs for the xgboost P model, B's inputs for size, CatBoost at `tuning/catboost_W1.json`). Rounds: the first test's test-fit round counts times 78,296 / 80,071. No early stopping. Seeds 117 to 121. No FY2017 label has chosen a setting, a round count, a promoted piece, the exponent or the blend.
- **FY2018 and FY2023, saved held-back predictions.** FY2018: P from `xgb_W1_ratio.npz`, size from `xgb_W1_base.npz`, CatBoost from `cb_W1_base.npz`. FY2023: P from `xgb_W3_ratio_era.npz`, size from `xgb_W3_era.npz`, CatBoost from `cb_W3_era.npz`. These years chose the rounds, the settings, the promoted pieces, the exponent and the blend (on dollar deviance). Each of those either touches both arms alike or was judged on fit, not on the share; the statistician puts the bias on the contrast under 0.05 points.
- **Statistic.** Map-weighted share of error dollars, each year's own FNS error rates times FY2024 issuance, 48 states, B's flag rule. Blend minus reference at 10%, and on the mean of the shares at 5%, 7.5% and 10%.
- **Weights.** FY2017 0.25, FY2018 0.25, FY2023 0.5.
- **Standard error.** Paired bootstrap of cases within state (2,000 draws) plus a half-bag seed term on the difference.

The principal data scientist would have refitted FY2023 forward with rounds chosen on FY2022. The statistician judged that this removes only the early-stopping leak, which touches both arms. Not adopted here; it can be run afterwards as a separate check.

## 4. Reading rule (fixed before the read)

> **Promising:** over the three validation years, the blend's share at 10% is at least 0.25 points above the matched xgboost arm's, and the mean of its shares at 5%, 7.5% and 10% is above the matched arm's.
> **Positive but small:** both differences are above zero and the difference at 10% is under 0.25 points.
> **Not promising:** anything else.

Chance of each reading by the true gain at 10% (statistician's simulation from the test years' bootstrap covariance; expected SE at 10% about 0.42):

| True gain at 10% | 0 | 0.25 | 0.5 | 0.75 | 1.0 |
|---|---|---|---|---|---|
| Reads promising | 23% | 45% | 69% | 87% | 96% |
| Reads promising or positive but small | 36% | 61% | 82% | 94% | 99% |

The middle grade is added so that a small real gain is not read as a failure. It was not in the statistician's rule; the statistician offered the looser threshold as an alternative to be chosen before the read.

Reported beside the reading, with no bearing on it: each year's difference (FY2023 first); the same contrast with exponent 1; the blend against the B-recipe twin; the sample-pooled share at 10%; the difference at 5%.

## 5. What the result can and cannot establish

- A pass means the sign and rough size held on years that did not produce the +0.72. It is grounds to keep the blend in `/methods` as a candidate. It is not confirmation.
- A fail does not show there is no gain. A true half point reads "not promising or small" 3 times in 10.
- No test on these years can resolve a gain of 0.2 to 0.4 points.
- FY2017 is a backward test within the pre-pandemic years. It says whether a second learner ranks unseen cases better. It says nothing about drift.

## 6. What had been seen before this note was written

- The blend's share and the matched xgboost arm's share on FY2017, FY2018 and FY2023 have not been computed by anyone.
- Single-arm shares on the held-back years exist on disk (`followup/heldback/criteria_*.csv`, the held-back logs, the morning principal data scientist review). During the run the session printed the CatBoost arms' FY2023 shares, and the statistician printed one CatBoost row for FY2018. The author of this note does not recall the matching xgboost figures. These do not give the blend-minus-xgboost contrast.

## 7. Files

`08_blend_validation.py` (stages `dry`, `fit17`, `score`); outputs in `followup/blend_validation/`. The scoring functions are the statistician's `reviews/scratch_blend_statistician/lib.py`, which reproduces the run's figures. The `dry` stage runs the scoring code on the three spent test years as a check of the machinery; it reads nothing new.
