# Predict each case's payment error with a pre-trained model

## What this is for

The model in this folder is already trained, on public SNAP Quality Control (QC) cases from FY2017-19 and FY2022-24. You give it one row per case from your own system. For each case it returns:

- the probability of a payment error over the federal QC tolerance;
- the predicted error amount if the case has an error;
- the expected error dollars, which is the first times the second;
- the case's rank within your file by expected error dollars.

The model is not retrained on your data. For each case the output also lists which of your state's flagging rules (the rules in your state's Excel workbook) the case meets, to help explain its score. MODEL_CARD.md describes how the model was built and tested.

## What you need

- **R 4.3 or newer**, from https://cloud.r-project.org. The setup step below installs the one package the model needs, xgboost.
- **This folder.** The model and everything it uses come as one zip file, `snap_case_model_v2.8.0.zip`, on the repo's releases page. Unzip it anywhere. Every command below runs in the unzipped folder.
- **A CSV of your cases**, one row per case, with the 21 columns in `templates/input_columns.csv`. These are the same columns as Step 2 of your state's Excel workbook, and extra columns are ignored, so a file prepared for the workbook works here.
  - Give amounts as monthly dollars. The model adjusts for inflation.
  - Use the values on the case before any review or correction takes place.

**Columns you do not have.** In training the model saw almost no blank values: under 0.1% of cases for any input. A column left blank for every case changes the ranking a good deal, so the script stops and says what to do:

| If your system does not record | Do this | How much of the top 10% of high-risk cases would stay the same (median state) |
|---|---|---|
| NUM_ELDERLY, NUM_DISABLED, NUM_ABAWD, MARRIED_FLAG, EXPEDITED, HOMELESS_FLAG, MEDICAL_DEDUCTION, DEPENDENT_CARE_DEDUCTION, CHILD_SUPPORT_EXPENSES, HOMELESS_DEDUCTION | Fill the column with 0 | 94% to 100% filled with 0; 39% to 88% if left blank |
| NUM_CHILDREN, CATEGORICALLY_ELIGIBLE, MONTHS_SINCE_CERT, EARNED_INCOME, UNEARNED_INCOME, RENT, UTILITY_COSTS | Do not use the model without it | 32% to 92% even with 0 filled |

REVIEW_FISCAL_YEAR and HOUSEHOLD_SIZE are needed in every row. A few blank cells elsewhere are fine; the run summary warns when more than 1% of cases are blank in any column.

## How to use it

1. **Set up, once.** From a command prompt in this folder, run `Rscript install_packages.R`. It installs xgboost and runs a self-check on a sample file. The last line should read `check passed: 710 example cases (Washington) reproduce the expected predictions`. On Windows, if `Rscript` is not found, use its full path, for example `"C:\Program Files\R\R-4.5.1\bin\Rscript.exe"`.

2. **Score your file.** This writes `my_cases_predictions.csv` next to the input; `--output` names another file, and `--state` takes a name or a two-letter code.

   ```
   Rscript predict.R --state WA --input my_cases.csv
   ```

   From R or RStudio, in this folder:

   ```r
   source("predict.R")
   pred <- predict_error(read_cases("my_cases.csv"), state = "Washington")
   ```

   Use `read_cases` rather than plain `read.csv`, which would turn a case ID of 000123 into 123.

3. **Read the run summary.** It prints the number of cases scored, the eligibility setting it read, any blank inputs, and the average predicted error. Lines that begin `Check:` need attention: the tables in `lookups/` run through FY2027, and FY2020-21 were left out of training. The script stops, rather than scoring, on a missing column, a column blank for every case, text in a numeric column, or a state the model does not cover (Alaska and Hawaii).

## What you get

One row per case, highest expected error dollars first.

| Column | Meaning |
|---|---|
| `CASE_ID` | Your identifier |
| `priority_rank` | 1 = highest expected error dollars in the file. Review from the top. |
| `expected_error_dollars` | Probability of an error times the error amount if there is one, in FY2024 dollars |
| `p_error` | Probability of a payment error over the federal tolerance |
| `error_size_if_error` | Predicted error amount if the case has an error, in FY2024 dollars |
| `rank_by_p_error` | Rank by probability alone, for finding the cases most likely to have any error, whatever its size |
| `hh_group` | Household size group: 1, 2-3 or 4+ |
| `issued_benefit` | `ORIGINAL_BENEFIT_AMOUNT`, as given |
| `inputs_blank` | How many of the model's 18 inputs were blank for this case |
| `list_rules_flagging` | Your state's workbook rules that this case meets, by their rank in the state's rule list. This explains a score; it does not feed it. |

**Use the ranks, not the levels.** The probabilities and dollars are the model's own, learned from public QC cases, and they run below published error rates.

## Before you rely on it

We refit the model on FY2017-19 and FY2022-23 and scored FY2024 cases, which it had not seen, reviewing each state's top 5% or 10% of cases:

| Cases reviewed | Reviewed cases with an error (at random) | Share of all error dollars caught | Median state, times random (10th-90th percentile) |
|---|---|---|---|
| Top 5% | 31% (11%) | 18% | 3.7 (2.3-5.1) |
| Top 10% | 27% (11%) | 31% | 2.9 (2.1-4.1) |

MODEL_CARD.md has the full results, including the weakest states and a comparison with the workbook.

| File or folder | What it holds |
|---|---|
| `predict.R` | The script you run |
| `install_packages.R` | One-time setup and self-check |
| `R/` | Input checks, features, scoring and rule matching |
| `model/` | The trained model and its version record |
| `lookups/`, `rules/` | Federal and state tables; each state's workbook rules |
| `templates/`, `example/` | The input columns; a sample file and its expected output |
| `MODEL_CARD.md` | How the model was built and tested |

If the self-check fails, send the output of `Rscript predict.R --check` and `model/model_info.txt` to the contact who gave you this folder.
