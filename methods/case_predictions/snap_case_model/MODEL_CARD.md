# Model card: SNAP payment error model (package v2.8.0)

## What it predicts

For each case, from 18 inputs built from the case's own fields plus the state:

- **P**: the probability of a payment error over the federal QC tolerance for the fiscal year.
- **Size**: the error amount if there is an error.
- **Expected error dollars**: P x size. Cases are ranked by this within the file.

## How it is built

- **P model.** Boosted trees (xgboost, binary logistic), depth 6, minimum child weight 5, column sample 0.5, 1,507 rounds.
- **Size model.** Boosted trees (xgboost, gamma), depth 4, minimum child weight 25, 850 rounds. It is fitted on error cases only, to the error amount adjusted for inflation by year. Its starting value for each case comes from a gamma regression on the case's recomputed benefit: log(size) = 3.894 + 0.227 x log(1 + benefit). The trees then adjust that value up or down.
- **State.** Both models include one indicator column per state, so each case is scored with its own state's term.
- **Averaging.** Each model is fitted ten times with different random seeds (117-126), and the ten predictions are averaged.
- **Settings.** Chosen from a grid by fit on FY2024 cases held back from training. The final fit then used all six years.

## Training data

- **Cases.** 237,557 public SNAP QC cases: FY2017-19 and FY2022-24, 48 states and the District of Columbia (frame f1109, sha256 08e0e825).
- **Values.** The inputs are reconstructed pre-review values, meaning the values as they stood before the QC review corrected them.
- **Outcome.** The outcome is QC's error amount over the year's tolerance.
- **Where it was built.** In the snap_qc repository, by `methods/case_predictions/s46/s46_deploy_f1109.R` (commit 8b8d99f).

## The 18 inputs

`R/build_features.R` builds them from the 21 input columns the same way the state Excel workbooks do. Dollar amounts are inflated to FY2026 dollars. The benefit ratios and the utility tier use the case's own fiscal-year federal and state tables. The inputs are:

- household size and composition: size, children, elderly or disabled members, married couple, homeless, ABAWD share;
- program fields: expedited service, the state's broad-based categorical eligibility, months since certification;
- income per person: earned, unearned and gross;
- shelter and deductions: shelter expenses per person, the utility tier against the state's standard utility allowance, medical deduction, total deductions per person;
- benefit ratios: the recomputed benefit relative to the maximum allotment, before and after the minimum and maximum caps.

## Test results

Each test reran the same fitting procedure on earlier years only, re-choosing the settings on those years, and scored a later year. So the tested models share the shipped model's design, but not its exact settings. Test 1 also used within-state percentile inputs, which this package does not build. Each state's top 5% or 10% of cases was flagged, and results were pooled over 49 states. "Times random" is the share of error dollars caught divided by the share of cases reviewed.

| Test (training years, scored year) | Budget | Flags | Precision | At random | Error $ per flag | Share of error $ caught | Times random, median state (10th-90th pct) | States below their base rate |
|---|---|---|---|---|---|---|---|---|
| FY2017-18, FY2019 | 10% | 3,926 | 0.221 | 0.093 | $32.67 | 29.5% | 3.08 (1.89-3.85) | 0 |
| FY2017-19, FY2022 | 5% | 1,883 | 0.253 | 0.100 | $51.96 | 16.6% | 3.37 (2.08-5.14) | 1 |
| FY2017-19, FY2022 | 10% | 3,765 | 0.243 | 0.100 | $46.47 | 29.7% | 2.85 (2.15-4.22) | 2 |
| FY2017-19 + FY2022-23, FY2024 | 5% | 2,027 | 0.309 | 0.111 | $73.20 | 18.3% | 3.65 (2.35-5.10) | 1 |
| FY2017-19 + FY2022-23, FY2024 | 10% | 4,049 | 0.272 | 0.111 | $61.35 | 30.7% | 2.94 (2.12-4.06) | 0 |

**Notes on the table.**

- **Precision** is the share of flagged cases with an error over the tolerance.
- **Error $ per flag** is in the scored year's own dollars.
- **Cases set aside.** About 2.5% of each test year's cases were set aside. These are cases whose reconstructed public-data inputs are known to be unreliable:
  - income amounts stacked on one value by the reconstruction;
  - cases issued the maximum benefit whose reconstructed inputs give a lower one;
  - benefits recreated on last year's tables;
  - large gaps between the recreated and issued benefit.

  The model was trained on all rows, including these.
- **Weakest states at 10%.**
  - FY2024: North Dakota, at 1.69 times random.
  - FY2022: Wyoming, at 0.32 times random (1 error among 28 flags), and Delaware, at 0.98 times random (13 flags).
- **Against the workbook.** On the FY2022 and FY2024 tests, at each state's top 5% of cases, the model caught 3.42 and 3.52 times its share of error dollars; the vote score in the state Excel workbooks caught 3.01 and 3.24 (findings §46, on the earlier frame f1030).
- **Optimism.** The same three tests chose this model from eight candidates (findings §46), so the figures carry some optimism. §46 reports a leave-one-test-out check.
- **Sources.** Snap_qc repo: `methods/case_predictions/s46/out_f1109_eval/B_f1109_summary_5_10pct.csv` and `B_f1109_by_state_5_10pct.csv`. The method is in findings §46 and §48.

## Checks of this package

Run in the repo by `methods/case_predictions/validate_case_model_package.R`; outputs are in `methods/case_predictions/case_model_validation/`.

- **Model file.** `model/snap_case_model.rds` holds the deploy bundle's twenty fitted models byte for byte.
- **Feature build.** Built from the input columns, the 18 inputs match the training frame's for 99.92% of FY2022-24 cases and 99.77% of FY2017-19 cases. The differences are all in the two benefit ratios. They are cases where the frame's reconstruction recomputes the benefit in ways the input columns do not carry.
- **Scores.** Across all 49 states, each state's top 10% of FY2022-24 cases scored this way overlaps the frame-scored top 10% by at least 98.5%.
- **Agreement with the repo's scoring script.** On Washington's FY2024 cases, the package reproduces `predict_cases.R` exactly: probabilities, dollars, ranks and the rules listed.

## Limits

- **Public QC cases only.** The tests used public QC cases with reconstructed pre-review values. The model has not been tested on a state's own case records, which carry no reconstruction.
- **Visible errors only.** Public QC files omit households found ineligible: in them, each state shows 43-81% of its error cases (snap_qc `methods/state_error_accounting/`). The model learned only from the errors it could see.
- **Levels are not calibrated.** P and the dollar amounts are not set to a state's published error rate. On each state's FY2024 cases, a year the model was trained on, the FNS FY2025 payment error rate was a median 1.89 times the model's dollar error rate, with both in FY2024 dollars (range 0.92 to 3.28). The gap has two parts:
  - **Public file against published rate.** The FNS rate was a median 1.53 times the error dollars the public file shows for the same cases (0.96 to 3.17). The gap was larger in states whose public files show a smaller share of their error cases (Spearman -0.34 across 49 states, against the FY2022-24 share visible). If the missing error cases were the same average size as the visible ones, the missing cases alone would give a median of 1.37. The FNS rate is also for FY2025 and weighted by the QC sample design; those differences are not separated here.
  - **Model against public file.** The public file shows a median 1.20 times the error dollars the model predicts (0.74 to 1.83), on a year the model was trained on. The cause has not been found.

  Use the ranking. Source: `methods/case_predictions/rate_gap_fy2024.R`, output in `case_model_validation/rate_gap_fy2024.csv`.
- **Years covered.** The lookup tables run through FY2027. Later years, and FY2020-21 (not in training), are flagged in the run summary.
- **Dollars are in FY2024 dollars.**
- **Blank inputs.** The model saw almost no blank inputs in training. README.md, under "What you need", gives the measured effect of a column a state does not record.
- **Coverage.** Alaska and Hawaii are not covered.

## Version record

`model/model_info.txt` gives the model file's sha256, the bundle it came from, the training frame, the commit, and the R and xgboost versions used to build it (R 4.5.1, xgboost 3.1.2.1).
