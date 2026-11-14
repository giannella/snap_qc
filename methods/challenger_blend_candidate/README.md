# Candidate: a blend of xgboost and CatBoost for the per-case model

Status, 2026-11-14: **documented candidate, not adopted, not a finding.** Nothing here changes model B, the state package or any published figure. The folder is kept so that others can test the idea. Promotion is a project-lead decision.

## The idea

Model B ranks cases by expected error dollars: P(error over tolerance) x error size, both from xgboost. The candidate averages B's two components with the same two components from CatBoost, a second boosted-tree learner, before ranking.

Recipe as tested:

- **xgboost side.** B's settings, with eight ratio inputs added to the P model and, where the training years span both eras (the FY2024 test), an indicator for FY2022 and later in both models.
- **CatBoost side.** Settings in `scripts/tuning/catboost_W*.json` (depth and state coding chosen per test on a held-back training year; learning rate 0.06; the size model uses a Tweedie loss with variance power 1.9), with the same era indicator.
- **Blend.** Each bag member averages P over one xgboost fit and one CatBoost fit, and averages size the same way. Score = P x size^1.153. A bag is 5 members.

## Why it might help

- The two learners disagree far more than two fits of one learner do. An xgboost bag and a CatBoost bag share 76 to 81% of the top-10% list; two xgboost bags share 92 to 96%.
- They are about equally accurate, so their average fits better than either: against an average of two xgboost fits the blend's dollar deviance is lower (z -3.2), and its P log loss is lower in all three test years.
- Averaging two xgboost fits adds 0.00 to the share, so the gain is not from the number of fits. It is in P, not size.

Both reviewers expect a true gain of 0.2 to 0.4 points in the share of error dollars at 10%, and none at 5%.

## Evidence

Share of error dollars caught, map state weights, B's flag rule, scored with `methods/eval_benchmark/score_ranking.R` on `additional_data/eval_benchmark_f1109.rds`.

**Against B, three test years (FY2019, FY2022, FY2024; weights 0.2 / 0.4 / 0.4).** B is 18.12% at 5% and 29.51% at 10%.

| | At 5% | 5-10% band | At 10% | Diff at 10% (SE) | z |
|---|---|---|---|---|---|
| Blend | 18.26 | 11.97 | 30.22 | +0.72 (0.48) | +1.5 |

This did not clear the bars set before that run (z 2.39 at 10%). How the 0.72 splits depends on the size exponent: with it, the xgboost changes alone are -0.03 against B; without it, +0.28.

**Against a matched two-fit xgboost average, six years.** This contrast gives the blend no credit for the number of fits. Difference at 10%, points:

| Test years | FY2019 | FY2022 | FY2024 | Combined (SE) |
|---|---|---|---|---|
| | +1.5 | +1.5 | -0.3 | +0.82 (0.45) |

| Validation years | FY2017 (backward) | FY2018 | FY2023 | Combined (SE) |
|---|---|---|---|---|
| | +2.2 | +0.2 | -0.1 | +0.53 (0.55) |

On the validation years the mean of the shares at 5, 7.5 and 10% is +0.57 (SE 0.32) and the sample-pooled share at 10% is +0.49 (SE 0.33). By the rule written before the read (at least +0.25 at 10% and above zero on the mean share) the validation reads **promising**.

## Qualifications

- The validation figure at 10% is carried by FY2017, where the matched xgboost arm was itself 1.5 points below a plain refit of B's recipe. On FY2018 and FY2023 alone the difference at 10% is 0.0 and the mean-share difference is +0.2.
- FY2023 and FY2024, the years with the most training data, show no gain at 10% (-0.20 together, SE 0.51). Both are one-year-ahead tests. FY2022, the only three-years-ahead test, reads +1.53 at 10% (SE 0.85).
- All six years combined, weighted by precision: +0.65 at 10% (SE 0.32), +0.47 on the mean share (SE 0.19), +0.12 at 5% (SE 0.31); +0.38 at 10% without the size exponent (SE 0.34). The test-year half was the best of several arms.
- On the test years the gain at 10% appears with the size exponent and not without it (0.00 with exponent 1 on the same fits). On the validation years it does not depend on the exponent (+0.70 with exponent 1).
- The sample-pooled difference at 10% is positive in five of the six years.
- No test on these data can resolve a gain of 0.2 to 0.4 points. Standard errors are 0.45 to 0.55.
- All six years have now been read on this contrast. Further work on them is descriptive.

## How to test it

1. **New data is the only clean test.** When the FY2025 QC file is released, fit both arms on FY2017-19 and FY2022-24, score FY2025 once, and read the blend minus a matched two-fit xgboost average by a rule written first. One year has a standard error near 0.7 at 10%, so one year alone will not settle a gain of a few tenths.
2. **Independent replication.** Rerun the scripts from a clean checkout and check that they reproduce the figures above. This tests the code, not the idea.
3. **What not to do.** Do not tune the blend (mixing weight, exponent, CatBoost settings) on the six years and report the best. The statistician's review shows the mixing weight is flat near one half; a search would fit noise.

## Cost of carrying it

- Fit time is small: one to three minutes per CatBoost fit on a laptop.
- It adds CatBoost, which is not on CRAN (its R package installs from a GitHub release binary), and a second learner to explain to state agencies. The benchmark code in this folder is Python.
- The principal data scientist recommends keeping it out of the state package unless a clean test confirms a gain.

## Files

| File | What it is |
|---|---|
| `design_note_followup.md` | Design, reading rules and review record of the run that produced the blend (sections 4 to 7; section 11 records two stops and a change to the BART stage) |
| `design_note_blend_validation.md` | Design and reading rule of the validation, written before the read |
| `reviews/blend_review_statistician.md`, `reviews/blend_review_principal_ds.md` | The two reviews of the blend: conceptual basis, decomposition, validation design |
| `reviews/followup_design_review_*.md` | The two pre-run reviews of the follow-up design |
| `scripts/01_export.R` | Exports B's rows, inputs, settings and stored scores from the frozen s46 code |
| `scripts/02_fit.py`, `scripts/05_followup.py` | Loader and fits; `05_followup.py` builds the blend (`test_cb`) and holds the promotion rule |
| `scripts/07_score_followup.R` | Scorer for the three test years |
| `scripts/08_blend_validation.py`, `scripts/lib.py` | The validation (stages `dry`, `fit17`, `score`) and its share functions |
| `scripts/settings_B.csv`, `scripts/tuning/` | B's settings by test, and CatBoost's |
| `results/promoted.json` | What the held-back years promoted |
| `results/run_score.out`, `results/top_metrics.csv` | Full scoring output of the follow-up run |
| `results/blend_validation_result.txt` | The validation read |

The scripts are copies as run. They ran from a working folder outside the repo (`C:\Users\ericg\snap_qc_challenger_benchmark`), which also holds the exported data, the per-fit score files and the other learners tested (LightGBM, EBM, TabNet, BART; none ahead of B). To rerun elsewhere, set `REPO` at the top of `01_export.R`, `05_followup.py`, `07_score_followup.R` and `lib.py` to the checkout, keep the scripts in one folder with `tuning/` beside them, and run `01_export.R` first to build `data/`. `08_blend_validation.py` expects `lib.py` under `reviews/scratch_blend_statistician/` and the run's outputs under `followup/`. Versions used: Python 3.14.4, xgboost 3.4.1, catboost 1.2.10, numpy 2.5.4, pandas 3.1.1, R 4.5.1.

## State-by-state reading (added 2026-11-14)

The figures above weight states by error dollars. Read state by state (each state's share of its own error dollars caught, 49 states), the blend is positive on both sets of years. This reading was made after the fact, with no rule written for it.

| At 10% | Median paired state change | Mean paired state change | States harmed by more than 5 points, by year |
|---|---|---|---|
| Test years (FY2019, FY2022, FY2024), against B | +0.34 (SE 0.21) | +0.61 (SE 0.35) | 4, 1, 2 |
| Validation years (FY2017, FY2018, FY2023), against a refit of B's recipe | +0.86 (SE 0.29) | +0.69 (SE 0.40) | 3, 4, 3 |

The validation rows are against a refit of B's recipe, and FY2018 and FY2023 chose the xgboost changes inside the blend; those changes alone read -0.05 (mean) on the same years. At 5% the paired changes are +0.22 (median) and +0.50 (mean) on the test years, +0.29 and +0.66 on the validation years. Scripts `09_state_medians.py` and `09b_state_medians_validation.py` are in `scripts/`.

## A trial in a state: pinned (2026-11-14)

The project lead asked whether the blend should be offered to a state as an experimental option that takes whatever columns the state has, such as transaction variables. Two reviews answered (`reviews/inclusion_review_statistician.md`, `reviews/inclusion_review_principal_ds.md`). Decision: wait until a state asks for it. Nothing was designed or built.

What the reviews advise, for whoever picks this up:

- Do not add a second pre-trained model. A pre-trained model cannot use a column it was not trained on, so a trial with a state's own variables means code the state fits on its own cases.
- Make xgboost on the state's own columns the main arm, the blend an optional arm, and model B the reference. One state can resolve a gain from new variables (3 to 5 points needs about 400 to 2,000 test errors). It cannot resolve the blend's 0.2 to 0.5 points, so blend results would be pooled across states.
- States hold tens of thousands of labeled secondary-review cases, which would be the basis for a trial. If those reviews were targeted, a result holds for reviewed cases only.
- Hold out the latest year or more in time order, fix the arms beforehand, and compare with model B only on cases outside its training years (FY2017-19, FY2022-24).
- Proposed home: `methods/case_predictions/state_fit_kit_experimental/`, released as a pre-release zip, with one "Experimental" block in the repo README.
- The project's rules require a design note and a fresh senior-statistician review before any code.
