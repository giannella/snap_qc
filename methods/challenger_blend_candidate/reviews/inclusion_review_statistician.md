# Should the xgboost and CatBoost blend be offered to a state? Review of section 54 and advice on a state trial

Senior-statistician review, fresh context, 2026-11-14. Requested by the project lead after section 54 was drafted.

Marks: **[computed]** means I ran it from the saved files or checked it against a results file. **[inferred]** is my judgment. No model was fitted. No existing file was changed. Scratch scripts and outputs: `C:\Users\ericg\snap_qc_challenger_benchmark\reviews\scratch_inclusion_statistician\` (`01_state_scale.py`, `02_summarise.py`, their `.txt` outputs, `state_scale_sd.csv`).

Read: the senior-statistician skill; `methods/findings_ledger.md` (whole); the new-study and routing sections of `known_constraints.md`; section 54 in both findings files; this folder's `README.md`, `design_note_blend_validation.md`, `results/`, the two blend reviews; `08_blend_validation.py` and `lib.py`; the state package's `README.md` and `MODEL_CARD.md`.

## 1. Verdict on the write-up

Section 54 is supported by the files. Its headline is not overstated. Nine corrections follow; items 1 to 3 change what a reader takes away.

### What I checked

- **`results/first_run_top_metrics.csv`**: all nine rows of the step 1 table (levels, differences, SE, z, sample-pooled); B from 5 seeds at -0.03 (SE 0.22); pooled precision 27.4% and the 1.2-point spread. All match. [computed]
- **`results/top_metrics.csv`**: all nine rows of the step 3 table; the blend's +0.99 without the three largest states and +0.83 sample-pooled (z 2.3 each); deviance z -5.6 and log loss z -4.8; the primary arm's z -3.2 and -2.7; FY2024 alone +0.06 and -0.37; BART's log loss z +6.2. All match. [computed]
- **`results/blend_validation_result.txt` and `_dry_run.txt`**: every cell of the validation table, the levels, the exponent-1 line (+0.70, SE 0.60), the test-year contrast (+0.82, SE 0.45; +1.54, +1.53, -0.25 by year). All match. [computed]
- **`results/state_medians.csv` and `state_medians_validation.csv`**: the blend rows, the harmed-state ranges, the +0.82 (z 3.4) and +0.05 (z 0.4) for the xgboost changes. All match. [computed]
- **`results/promoted.json`**: every t statistic and exponent quoted in step 3. All match. [computed]
- **From the per-fit score files**, with my own script: B at 10% by test 30.42 / 28.72 / 29.84; blend 32.06 / 29.85 / 29.68; combined 29.51 and 30.22 (+0.72); at 5% 18.12 and 18.26; blend minus matched xgboost +0.82. All reproduce. [computed]
- Not checked: the list-overlap figures (76 to 81%, 92 to 96%), which come from the earlier reviews and not from `results/`.

### Corrections

1. **The deployment-gap companion is missing, and "nearest to deployment" is half right.** Plain and detailed section 54 ("Where the blend shows nothing"; the takeaway) and `README.md` line 50 call FY2023 and FY2024 the years nearest to deployment. They are nearest in training size and era. They are one-year-ahead tests. The ledger's rule of 2026-10-06 makes three years ahead the primary window, and FY2022 is the only such test. There the blend is +1.53 at 10% (SE 0.85) and +0.97 on the mean share (SE 0.46) against matched xgboost, and 29.85 against B's 28.72. It should say "the two years with the most training data" (the ledger row already does) and add the FY2022 figures, with their own limits: one year, +0.22 sample-pooled (SE 0.60), +0.30 at 10% with exponent 1 (SE 0.81). [computed from the dry-run file]
2. **"About 0.3 of the +0.72 belongs to the xgboost changes"** (plain section 54, blend bullet; detailed step 4; `README.md` line 33). This holds only without the exponent (+0.28). The blend's own xgboost half, with the exponent, is -0.03 against B, and the same paragraph then reports +0.74 to +0.82 against matched xgboost, which is more than +0.72. It should say: "The split depends on the exponent. With it, the xgboost half is -0.03 against B and the second learner accounts for all of the +0.72. Without it on either arm, the xgboost changes are +0.28 and the blend adds nothing at 10% on the same fits (both 29.79). The blend as scored is +0.43 above the xgboost arm without the exponent." [computed, except the blend without the exponent, which is the principal data scientist's figure]
3. **No six-year figure is given.** The precision-weighted combination of the six per-year contrasts against matched xgboost is +0.65 at 10% (SE 0.32), +0.47 on the mean share (SE 0.19), +0.43 sample-pooled (SE 0.23) and +0.12 at 5% (SE 0.31). With exponent 1: +0.38 (SE 0.34) and +0.32 (SE 0.20). Without FY2017: +0.54 at 10% (SE 0.34). FY2023 and FY2024 alone: -0.20 (SE 0.51). Heterogeneity across years is within chance (Q 7.1 on 5 df). Add these beside the two three-year figures, with the caveat that the test-year half was the best of several arms. [computed from the printed per-year figures]
4. **"A gain has to be about 1.5 points to clear two standard errors"** (plain section 54; detailed step 2; the ledger's Evaluation machinery row). An observed difference clears 2 SE at 1.0 to 1.2 points. The 1.5 is the true gain detected four times in five (the follow-up statistician's wording). Say that.
5. **Step 1 standard errors.** The plain table and the detailed step 1 table carry the first scorer's SEs. Detailed step 2 says they are "5 to 10% low". The four arms rescored in `top_metrics.csv` read 0.41 (twin, was 0.34), 0.43 (re-tuned, 0.38), 0.58 (CatBoost, 0.54) and 0.81 (BART, 0.73): 7 to 21% larger. It should say "7 to 21% low" or footnote the tables. No reading changes. [computed]
6. **The rule's false-pass rate.** "About one time in four" was computed for a planned SE of 0.42. The realised SE was 0.55. At that SE a null arm passes about 3 times in 10. [inferred, normal approximation, correlation 0.7 between the two statistics]
7. **State-by-state validation row.** The +0.86 and +0.69 are against the twin, and FY2018 and FY2023 chose the xgboost pieces inside the blend. Say so where they are quoted (plain section 54, ledger row, `README.md` table). Add that the xgboost pieces alone read +0.21 and -0.05 on the same years, so the pieces do not explain it. The +0.86 (z 2.9) is the largest z in the blend's record and comes from a reading with no rule.
8. **"Raised the median state by 0.8 points at 5%"** (plain section 54; ledger row on structural changes). The +0.82 is the median paired state change. The median state share moved from 17.51 to 17.74. Use "median paired state change".
9. **Year order.** Detailed section 54 lists the validation years as -0.1, +0.2, +2.2 without labels (FY2023, FY2018, FY2017). The plain section and the ledger use FY2017, FY2018, FY2023. Label them.

## 2. How much evidence that the blend beats xgboost alone

- **Fit: established.** The blend's dollar deviance and P log loss are lower than a matched xgboost average in every year. [computed by the earlier reviewers; I checked the z values in `top_metrics.csv`]
- **Share of error dollars at 10%: suggestive.** Six years, +0.65 points (SE 0.32), positive in four of six. On the mean of the shares at 5, 7.5 and 10%: +0.47 (SE 0.19), positive in five of six. These z values of 2.0 and 2.4 are nominal. Three things discount them. The test-year half was the largest of the run's arms. The size depends on the exponent (+0.38 at 10% with exponent 1). The reference arm is not the best xgboost on record: on the tests the exponent costs xgboost 0.3 points at 10%, and on FY2017 the reference was 1.5 points below a plain refit. [computed; the discounting is inferred]
- **Share at 5%: nothing.** +0.12 (SE 0.31) over six years.
- **By window.** The one three-years-ahead test shows the gain (+1.5, SE 0.85). The two tests with the most training data show none (-0.20, SE 0.51).
- **My reading.** A real gain at the 10% workload is more likely than not, about 4 chances in 5. Its size is probably 0.3 to 0.5 points on a base of 29.5, which is 1 to 2% more error dollars found. On the project's vocabulary I would call it supported, not confirmed, between 5% and 10%, and not shown at 5%. [inferred]

## 3. Recommendation

**Do not add the blend to the pre-trained state package. If the project builds code that a state fits on its own cases, ask the state first to try xgboost alone with its extra variables, and carry the blend in the same code as a second, clearly labeled arm whose result is collected and pooled across states, not read in any one state.**

Reasons:

- **A state cannot test the blend.** On one state-year of QC cases (median 836 cases, 76 errors) the paired difference between the blend and matched xgboost has a standard deviation of 2.5 points at 10% per 100 test errors. The expected gain is 0.3 to 0.5. Detecting half a point four times in five needs about 20,000 to 28,000 test errors. [computed; section 4]
- **A state can test its extra variables.** A gain of 5 points at 10% needs about 400 to 700 test errors. That is in reach of a state's own review data and out of reach of one QC year. [computed]
- **The variables are the larger lever.** Across eight arms on B's inputs the share at 10% runs from 27.6 to 30.2, and LightGBM, CatBoost, BART and the blend are all within 0.7 points of B. B's lead over the workbook vote score, which differs in how it uses the same information, is 3.8 points. The ledger holds that the engine is worth about a point (sections 2, 4, 13). New information about a case (transactions, interview channel, change reports) can move the ranking by more than any learner swap has. That it will is not shown; the ledger row "Hybrid internal + national blend at a real state" is open and untested. [inferred from computed figures]
- **The blend is the only route to new evidence before FY2025 public data.** Ten states returning 1,000 test errors each would give a pooled SE near 0.25 to 0.3 on the blend contrast. That is the reason to carry the arm at all. [computed constant; the pooling is inferred]
- **A caution on "improved xgboost".** Section 54 shows the revised xgboost (ratio inputs, era indicator, exponent) is not improved on the share: -0.03 at 10%. B is still the model. The blend as tested contains those pieces. If a blend is offered, decide which xgboost half it has.

**Honest label for the blend:** "Experimental. On public QC data it found 0.5 to 0.8 points more of error dollars than xgboost alone at a 10% workload (standard error about 0.5), nothing more at 5%, and nothing more in the two most recent years. This is not a confirmed gain. Expect it to rank cases about as well as xgboost alone. Your own data cannot tell the two apart unless your test set holds several thousand error cases."

## 4. A sound state trial

### How noisy one state's comparison is [computed]

Bootstrap of cases within each state, 147 state-years from the three tests, share of the state's own error dollars caught. The standard deviation falls with the square root of the number of test errors (fitted slopes -0.36 to -0.58), so SD = c / sqrt(test errors), in points.

| Quantity at a 10% workload | Lists in common | c | SD at 100 test errors | SD at 1,000 |
|---|---|---|---|---|
| One ranking's own share | | 54 | 5.4 | 1.7 |
| Blend minus matched xgboost | 90% | 25 | 2.5 | 0.8 |
| CatBoost minus xgboost (a moderately different ranking) | 79% | 36 | 3.7 | 1.2 |
| Main-effects EBM minus B (a very different ranking) | 68% | 48 | 4.8 | 1.5 |

At 5% the constants are 45, 23, 32 and 41. The national sample-pooled SE implies c near 30 for the blend contrast, so the extrapolation to thousands of errors is, if anything, slightly optimistic. A model with new variables will differ from one without them at least as much as CatBoost differs from xgboost, so use c of 36 to 48 for that contrast. [the last sentence is inferred]

Test errors needed to detect a true gain D four times in five (two-sided 5%): (2.8 x c / D) squared.

| Contrast | Gain to detect | Test errors | Test cases at 10% in error |
|---|---|---|---|
| Blend against xgboost, same inputs | 0.5 points | 20,000 to 28,000 | 200,000 or more |
| Blend against xgboost, same inputs | 1.0 | 5,000 to 7,000 | 50,000 to 70,000 |
| Extra variables against none | 3 | 1,100 to 2,000 | 11,000 to 20,000 |
| Extra variables against none | 5 | 400 to 700 | 4,000 to 7,000 |
| Extra variables against none | 10 | 100 to 180 | 1,000 to 1,800 |
| Any ranking against random review (gap about 20 points) | 20 | under 100 | one QC year |

One QC year (about 1,000 cases, 100 errors) can show that a ranking beats random review and can show a gain of 10 to 13 points or more. It cannot show a gain of 3 to 5 points, and it flags about 42 cases at 5%, of which about 13 are errors.

### Design

- **Hold out by time.** Train on earlier sample months, test on the latest 12 months or more. No random split. No household in both sets. Use the gap the state will run with: one year if it refits yearly.
- **To get more test errors for the within-state contrasts**, roll the origin: train through year t, test year t+1, for three or more test years, and pool. Three QC years give about 300 test errors (SD 2.1 to 2.8 for the variables contrast).
- **Arms, named before the test labels are opened**, all scored on the same test rows:
  1. the state's current practice, at the workload it actually reviews;
  2. model B's pre-trained ranking;
  3. xgboost fitted on the state's cases with B's inputs only;
  4. xgboost with B's inputs plus the state's variables (the primary arm);
  5. the blend with the same inputs as arm 4 (secondary).
- **Primary contrast: arm 4 minus arm 3**, which isolates the variables. Arm 4 minus arm 2 answers whether a state fit is worth having over the shipped model. Arm 5 minus arm 4 is reported with its SE and sent back for pooling.
- **Metrics.** Share of error dollars at 5%, added from 5% to 10%, and at 10%. Beside each: cases flagged, errors caught, error dollars caught, and a paired bootstrap SE over cases (over households if they repeat).
- **Training size.** B learns from 237,557 cases. A state QC file of five years is about 5,000 cases and 500 errors. A fit on that will probably rank worse than B on B's inputs, and the extra variables have to make up the difference. The ledger's record on own-state rule mining (boom or bust, section 14) points the same way. Below about 300 training errors I would not fit at all. [inferred; nothing in the benchmark measures a 5,000-row fit]

### Ways the trial misleads, and the guard for each

| Hazard | Guard |
|---|---|
| **B has already seen the state's public QC cases.** The shipped B trained on FY2017-19 and FY2022-24, which include the state's QC sample. On those rows B's ranking is in-sample and will look too good. | Compare against B only on cases B did not train on: the state's FY2025 and later QC cases, or non-QC reviews. The code should take the review fiscal year and refuse the B comparison on FY2024 and earlier QC rows. Do not feed B's score in as an input unless the training rows are also unseen by B. |
| **Targeted reviews select the labels.** If cases were reviewed because current practice flagged them, the test set is not the caseload. A share computed there says how well a model re-ranks cases already chosen. It cannot be compared with current practice, which chose all of them. | The code asks whether the test cases are a random sample of the caseload (the QC sample is) or carries sampling weights. If neither, it prints "within reviewed cases only" on every figure and drops the current-practice row. Train on selected labels only with that label attached. |
| **Leakage from fields written after the review.** Corrected income, claim or recoupment flags, post-review benefit, a case-action date after the sample month, transaction totals that run past the sample month. | Instruction: every value as it stood in the sample month, before any review. Code: flag any single input whose own ranking catches more than half the error dollars at 10% (B catches 30%); warn when precision at 5% exceeds 0.6 (B's is 0.31); list the top inputs by gain for a person to read; flag inputs blank far more often for one outcome than the other. |
| **A winner picked on the test rows.** With five arms and an SD of 4 to 5 points per contrast at 100 errors, the best arm will lead by several points by chance. | Arms and the primary contrast are written to a file before test labels load. Tuning uses training rows only, with an inner time split. The test is scored once; the code refuses a second run without a logged override. Every arm is printed, not the best. |
| **Small counts at 5%.** About 42 flags and 13 errors in a QC year. | Print counts beside every share. Suppress a workload with fewer than 30 flagged cases or 10 errors caught. Print "cannot tell apart" when a difference is under 2 SE. |
| **A different error definition.** State reviews may record an error that was fixed before issuance, or no dollar amount. | Require the outcome as payment error dollars over the federal tolerance for the review year, or label the run as a different outcome. |
| **Coding changes in the state's own variables** (a system migration is the state's version of the `cat_elig` recode in the ledger). | The time split catches part of it. The code prints each added variable's mean and blank rate by year. |

### What a state's result can and cannot tell the project

- **Can:** whether a state fit with extra variables beats pre-trained B on that state's cases, which is the first measurement for the ledger's open hybrid row; which variables carry signal; whether the code runs on a real file.
- **Can, pooled:** the blend contrast, if the code writes a standard results file (paired difference, SE, counts) and several states return it.
- **Cannot:** show in one state that the blend beats xgboost; say anything about the blend inside the pre-trained national model, which trains on about 50 times the rows with other inputs; give a caseload figure from selected labels; be added to section 54's six years.

## 5. Things to check

1. Whether a partner state can rebuild its extra variables as of the sample month for past years. If transaction history is kept for only a year or two, the training set shrinks to that.
2. Whether the state holds FY2025 QC cases with final findings. They are the only QC rows on which B and a state fit can be compared fairly.
3. The size and selection rule of any non-QC review file before promising a result from it.
4. Which xgboost half the offered blend has: B's recipe, or the revised one with the exponent. The test-year gain appears with the exponent and not without it on the same fits.
5. Fit-on-your-own-data code is a new study script under `known_constraints.md`: it needs the four-item design note and a fresh review before a state is pointed at it.
6. The nine corrections in section 1, before section 54 is committed.
