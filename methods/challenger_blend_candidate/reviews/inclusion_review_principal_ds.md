# Should an experimental xgboost + CatBoost option be offered to a state, and in what form

Principal data scientist review, 2026-11-14. Fresh read; I did not take part in the benchmark or the earlier reviews. Nothing here is a finding until the project lead logs it. No model from the project was fitted and no existing file was changed.

Read first: `methods/findings_ledger.md` (whole), the principal-data-scientist skill, `known_constraints.md` (new study scripts, routing), section 54 in both findings docs, this folder's `README.md`, design notes, `results/` and the earlier blend review, the state package (`methods/case_predictions/snap_case_model/`: `README.md`, `MODEL_CARD.md`, `install_packages.R`, `make_case_model_package.R`), the repo `README.md`, `VERSIONING.md`, `CHANGELOG.md`, `check_doc_consistency.sh`, and `INCL_mine_internal_and_optionally_blend_with_national_v2.R`.

Marks: **(verified)** means I compared it with a file or ran a small check today. **(inferred)** is my judgment or arithmetic from figures in the files.

## 1. Short answer

- **The write-up is accurate.** I traced more than 200 figures from section 54, the ledger rows and this folder's README to the files in `results/` and found no wrong number. Seven corrections are listed in section 2; none changes a reading.
- **Recommendation: option (b), narrowed.** Build a small trainable kit in R that a state fits on its own reviewed cases with its own columns. xgboost is the main arm. The xgboost + CatBoost blend is an optional arm that runs only if CatBoost is installed. The shipped model B is the reference arm where the state can supply its 21 columns. Do not ship a second pre-trained model (option a).
- **The second learner is not the right main question for a state.** On public data the blend is worth an expected 0.2 to 0.5 points at 10% and nothing at 5%. A state file cannot resolve that. What a state can test, and what the project has never tested, is whether a model fitted on the state's own columns and labels beats the shipped model on the state's own held-back cases. Ask that; carry the blend along as a free second line in the output.
- **It should live at `methods/case_predictions/state_fit_kit_experimental/`**, with a zip on a GitHub pre-release, and one short "Experimental" paragraph in the repo README. Model B, the model card, the v2.8.0 package zip, the workbooks, GUIDANCE and the published figures do not change.

## 2. Review of the section 54 write-up

### What I checked (verified)

Against `results/first_run_top_metrics.csv`, `top_metrics.csv`, `promoted.json`, `blend_validation_result.txt`, `state_medians.csv` and `state_medians_validation.csv`:

- **Plain table, all 14 rows**: share at 5%, band, share at 10%, difference and SE. All match (for example B 18.12 / 11.39 / 29.51; CatBoost 17.80 / 12.25 / 30.05, +0.54 (0.54); EBM main effects 15.75 / 11.87 / 27.63, -1.88 (0.80); blend 18.26 / 11.97 / 30.22, +0.72 (0.48)).
- **Detailed step 1 table**: differences and SEs at 5%, z at 10%, sample-pooled at 10% for all nine rows; B on 5 seeds (-0.03, SE 0.22); pooled precision at 5% within 1.2 points of 27.4%. All match.
- **Detailed step 3 table**: all nine rows, every column including the mean share, sample-pooled difference, states harmed, and the quoted z values for deviance and log loss (-5.6, -4.8, -3.2, -2.7, +6.2). All match. FY2024 alone for the primary arm (+0.06 at 10%, -0.37 at 5%) matches.
- **Promotion record**: every t value and exponent quoted in step 3 matches `promoted.json` (for example P ratio -4.9 and -27.4; size era -46.5; exponent 1.153 from 1.222 and 1.115; blend -6.9 and -3.8; CatBoost regularisation -3.6 and -1.8).
- **Validation table**: all 20 cells, the three sets of levels, and the exponent-1 figure (+0.70, SE 0.60) match. "FY2018 and FY2023 alone: 0.0 at 10% and +0.2 on the mean share" reproduces from the per-year figures and the 0.25 / 0.5 weights (-0.01 and +0.22).
- **State-by-state tables**: all 13 test-year rows and all 4 validation rows match.
- **Ledger rows**: the five new rows match section 54.

I did not rescore anything from the per-fit score files in the working folder. The check is of the write-up against the result files, not of the result files against the fits.

### Corrections

1. **`modeling_findings.md` section 54 table, SE column; `modeling_findings_detailed.md` step 2, "its standard errors are 5 to 10% low".** The table mixes two scorers. The step 1 arms carry the first run's SEs; `top_metrics.csv` rescored four of them with the corrected seed term: twin 0.41 (table says 0.34), re-tuned xgboost 0.43 (0.38), CatBoost 0.58 (0.54), short BART 0.81 (0.73). The first run was 7 to 15% low on these four, not 5 to 10%. Use the corrected SEs in the plain table, or say in one line which rows use which scorer. No z crosses a line.
2. **`findings_ledger.md`, structure-changes row, and the plain "State by state" bullet: "+0.82 points, z +3.4".** That is the arm without the size exponent. The row's claim lists the exponent among the changes; the arm with all promoted pieces reads +0.52 (z +2.3). Say "+0.82 without the exponent (z +3.4), +0.52 with it". In the plain bullet, "raised the median state by 0.8 points" should read "the median paired state change was +0.8".
3. **`findings_ledger.md`, blend row: "the pre-set bars of 2.39 at 10% and 2.13 on the mean share were not met".** Next to "+0.72 points" these read as points. They are z values. Say "z +1.5 against bars of z 2.39 at 10% and z 2.13 on the mean share".
4. **Both findings docs and this folder's README: "eight ratio inputs" is never defined.** `design_note_followup.md` names them: earned share of income, shelter over gross, deductions over gross, uncapped minus capped benefit ratio, gross minus deductions, total gross, any medical deduction, zero income. Add the list once. The same note carries two cautions that the write-up dropped and that apply to the blend as tested, because its xgboost half uses these inputs: a zero-filled gross column sets the zero-income flag for every case (the section 51 fill rule was never tested with them), and a ratio of two reconstructed amounts could mark a reconstructed case. The second is partly covered because scoring is on kept cases, which exclude pile rows; say so, since the ledger's pile row asks feature studies for a non-pile reading.
5. **This folder's README, "Cost of carrying it": "adds a Python and CatBoost dependency".** CatBoost has an R package. It is not on CRAN (verified today on both R 4.5.1 and 4.6.1 against cloud.r-project.org). The CatBoost GitHub release v1.2.10 carries a prebuilt Windows binary, 90 MB. I installed it into a scratch library with `R CMD INSTALL`, with no compiler; it loads on R 4.5.1 and 4.6.1 and fits a toy model with a factor column and missing values (verified). The cost is a second package from outside CRAN, not Python.
6. **This folder's README, "State-by-state reading": "the median state share at 10% is 30.98% for the blend and 29.32% for B".** A reader will take the 1.67-point gap as the gain. The same statistic is +1.39 and +1.41 for CatBoost alone and +1.08 for LightGBM, whose mean paired state changes are -0.41, -0.27 and +0.06 (`state_medians.csv`). The gap between two medians is not a paired figure. Drop the sentence or follow it with "the paired change, +0.34, is the figure to read". The same paragraph says the `09` scripts are in the working folder; they are in `scripts/`.
7. **All four documents: FY2023 and FY2024 as "the nearest to deployment".** They are the years with the most training data. By the ledger's 2026-10-06 rule the deployed gap is three or more years, and the only three-years-ahead test is FY2022, where the matched contrast is +1.57 (SE 0.74). So one reading of the six years is "no gain with the most training data" and another is "a gain on the one long-gap test". The files cannot separate them. Say "the years with the most training data" and add the FY2022 clause. This cuts in the blend's favour and does not change the reading.

Minor, at the author's discretion:

- "76 to 81%" overlap is the earlier principal data scientist's figure. The file kept here (`reviews/blend_statistician_02_overlap.txt`) gives 77 to 81% on the three tests and 75 and 80% on the held-back years.
- The ledger's 2026-08-09 rule asks attribution readouts for same-sign state counts. They exist (`top_metrics.csv`: blend ahead / behind at 10% is 30/17, 26/20, 24/23; validation 19/23, 29/17, 30/19) and are not quoted.
- The plain entry never says why its 29.5% differs from the model card's 30.7%. One clause would do: the card's figure is FY2024 alone, sample-pooled.
- This folder's README file table omits `design_note_first_run.md`, `benchmark_README.md`, the `09` scripts and the two state-median files.

### Altitude and completeness

- The plain entry is at the right level for a state reader. The takeaway carries the levels, the size of the nearest gain, its standard error and the limit of the data.
- The companions the project requires are present: within-state mean, harmed-tail counts, 5% and 10% workloads, power.
- The status of the three ledger rows is right. "Retired for gains of about 1.5 points or more" states the resolution limit and does not file an unresolvable question as refuted.
- **One figure the write-up does not give, in the blend's favour (inferred).** The matched contrast at 10% is +0.82 (SE 0.45) on the tests and +0.53 (SE 0.55) on the validation years. Weighted by inverse variance the six years give about +0.70 (SE 0.35), z about 2.0. The test half is flattered (largest of several arms; the gain there depends on the exponent), so I would not log the pooled figure. It does mean "0.2 to 0.4" may be a little low. I use 0.2 to 0.5 below.

## 3. The decision

### What a state file can and cannot resolve (inferred from the result files)

- On one public year of about 40,000 cases, the paired SE of blend minus matched xgboost on the sample-pooled share at 10% is 0.47 to 0.66 points (`blend_validation_result.txt`). The SE grows with the square root of 40,000 over the number of held-back labeled cases.
- Held-back cases 10,000: SE about 1.0 to 1.3. Held-back cases 2,000: 2.1 to 3.0. Held-back cases 1,000, about one year of a state's QC sample: 3.0 to 4.2.
- The expected blend gain is 0.2 to 0.5 points. To put two standard errors under 0.4 points a state would need roughly 300,000 held-back labeled cases. No state has that. Five states returning results would not get there either.
- Two rankings that differ more (a state-fitted model against model B) have a larger paired SE, roughly 1 point per 40,000 cases (from the TabNet and EBM rows). At 5,000 held-back cases that is about 3 points, so a difference of 6 points would show. B's lead over the workbook vote score is 3.8 points at 10%, for scale.
- So a state can detect a large gain from its own columns. It cannot detect the blend.

### The options

**(a) A second pre-trained model, same 21 inputs. Not recommended.**

- No deploy fit of the blend exists. The tested recipe has CatBoost settings that differ by test (depth 8, 4 and 8; state coded two ways), ratio inputs whose zero-fill behaviour is untested, and an exponent. A deploy fit would need each fixed, and nothing held out would have tested the fixed form.
- The package's checks would all be redone for a second model: byte parity of the model file, 49-state score parity, the missing-column table of section 51.
- It adds a second install from outside CRAN to a package whose selling point is one CRAN dependency.
- The gain is expected at 10% only, and it is zero in the two tests with the most training data, which is what the deploy fit most resembles.
- A state handed two pre-trained rankings has no way to tell which is better on its own cases (see the SEs above). The project would be asking colleagues to try something whose result cannot come back.
- The ledger records the lead's decision of today that the blend is not in the state package. Nothing since then changes the evidence.

**(b) A trainable kit. Recommended, with the blend as an optional arm.**

- It answers a question the project cannot answer from public data. The model card's first limit is that B has never been tested on a state's own records. The ledger has carried "hybrid internal + national at a real state" as open and blocked on an engagement.
- It is the only form that can use transaction variables or any other column the public file lacks.
- There is a precedent in the repo for rules and none for the per-case model (verified). `INCL_find_inclusion_rules_by_hh_size_v2.R` takes an editable `features` vector and `TARGET_IS_ERROR`, numeric, logical or two-level factor columns only. `INCL_mine_internal_and_optionally_blend_with_national_v2.R` mines a state's internal file with the fixed 19 features. Nothing fits a per-case model on a state's file. `predict.R` ignores extra columns.
- The cost is real: it is a new state-facing product with no held-out validation of its own. Section 6 says what the project's rules require for that.

**(c) Both. Not recommended.** Option (a) adds cost and no information.

**Neither.** Defensible on the blend evidence alone. I do not recommend it because the kit's main arm, xgboost on a state's own columns, is worth testing whether or not the blend exists.

### Is the second learner the right thing to ask a state to test

No, not as the main question (inferred).

- The public-data gain from the second learner is a few tenths of a point. The state term alone was worth 3.3% to B (section 46). Columns the public file lacks, and labels that include ineligible cases (public files show a state 43 to 91% of its errors), could be worth several points. That is where a state's effort should go, and xgboost is one CRAN package.
- The project has already stacked a second learner on B once: candidate C, a ranger forest, Q 3.498 against B's 3.502 (section 46). The blend is the second result of the same kind.
- There is one honest reason CatBoost might do more on a state file than on the public frame. B's 18 inputs are numeric, plus 49 state indicators. A state file may carry categorical columns with many levels (office, case type, transaction codes), and CatBoost's handling of those is its main difference from xgboost. That is a conjecture. It justifies an optional arm, not a headline.
- Small state files are the larger risk. The ledger's hazard row says single-state mining at public scale collapses, and the national ranking beat own-state mining for the median state. A model fitted on a few thousand state rows will probably lose to B unless it borrows from B. The simplest way to borrow needs no new code: the state runs `predict.R` first and lists `p_error` and `error_size_if_error` among its input columns. The kit README should recommend that as the default wherever the 21 columns exist.

## 4. What the kit should be (proposal; not built)

**Language.** R only, base R plus xgboost from CRAN, as in the state package. No Python: the benchmark's environment is Python 3.14 with 38 packages, and a state analyst who has R for the package should not need a second runtime.

**CatBoost.** Optional. `fit_and_compare.R` checks `requireNamespace("catboost")` and prints "second learner not installed; blend arm skipped" if it is absent. A separate `install_catboost_optional.R` installs from a local `.tgz` the state's IT has approved, or from the CatBoost release page if the machine can reach GitHub. Pin 1.2.10, the version benchmarked. Expect many locked-down machines to allow a CRAN mirror and block GitHub downloads; the kit must be fully useful without it (inferred).

**Inputs, in one edited block.**

- the case file (CSV, read as `read_cases` does, so IDs keep leading zeros);
- a label column (1 = payment error over tolerance), or an error-amount column plus a tolerance from which the kit builds the label;
- an error-amount column (0 or blank when no error);
- a date or fiscal-year column for the split;
- the list of input columns, any number and any names;
- optional: a case ID, the issued benefit, and how the labeled cases were chosen (random sample or targeted review).

**Split and fitting.**

- Time split only: fit on the earlier period, hold back the latest (default the last quarter of cases by date, or a cut-off date the state gives). Never a random split.
- Rounds chosen by early stopping on the last part of the fitting period, never on the held-back period. One fixed setting per learner, no grid: B's settings for xgboost (P: depth 6, minimum child weight 5, column sample 0.5; size: gamma, depth 4, minimum child weight 25); CatBoost at the benchmark's learning rate with its defaults otherwise. Five seeds per arm.
- Score = P x size. No exponent: the 1.153 was chosen on public years and bought nothing there.
- Blend arm: the average of P and of size over the two learners, on identical columns, so exactly one thing varies.
- Support is printed before any fit: rows and errors in the fitting and held-back periods. Stop under 200 fitting errors or 30 held-back errors. Under 300 held-back errors, run and print "descriptive only".
- With few fitting errors the size model is weak. Under about 300, rank by P times a one-variable gamma regression on the issued benefit (B's offset form), or by P alone if no benefit column is given.

**Mixed column types and missing values.**

- Numeric columns pass through. Dates become months before the case date. Text and factor columns become factors with levels fixed on the fitting period; a level seen fewer than 30 times is pooled into "other"; an unseen level in the held-back period becomes missing. Both learners take factor columns directly (verified in toy fits: xgboost 3.1.2.1 in R accepts a data frame with a factor column and missing values; so does CatBoost 1.2.10).
- Missing values stay missing. This differs from the pre-trained package, which fills with 0 because B saw almost no blanks in training. Here the model learns from the state's own pattern of blanks.
- Constant columns and the ID are dropped with a message. A column with more than about 50 levels (a worker ID) is dropped unless the state lists it as allowed.

**Leak guards.** With arbitrary columns the main hazard is a field written during or after the review. Section 46 closed exactly this kind of leak on the public data. The kit should refuse the label and amount columns as inputs, stop on any single column that separates errors from non-errors almost perfectly on the fitting period (single-column AUC above 0.90) unless the state lists it as allowed, and print the ten most important columns so a person can spot a post-review field. These are design requirements, not failure modes to be judged afterwards.

**What it prints.** Plain counts first, at the 5% and 10% workloads, then the difference with its interval. Illustrative layout (invented numbers):

```
Held back: Oct 2025 to Mar 2026. 4,812 cases, 498 with an error (10.3%), $61,240 in error dollars.
Fitted on 14,377 earlier cases, 1,466 with an error.

If you had reviewed the top 10% (481 cases):
                                   errors found   share of error $   times random
  at random                              50             10%              1.0
  shipped model (21 columns)            121             29%              2.9
  your columns, xgboost                 140             34%              3.4
  your columns, xgboost + CatBoost      143             35%              3.5
(the same table for the top 5%, and the share added between 5% and 10%)

Your columns (xgboost) minus the shipped model, share of error $ at 10%:
  +5 points, 95% interval -1 to +11.
With 4,812 held-back cases a difference under about 6 points cannot be told from chance.
```

The three figures per arm are the project's: share of error dollars at 5%, the share added from 5% to 10%, and the share at 10%. The interval is a paired bootstrap of held-back cases. The last line states the power for the state's own file. If the labeled cases came from targeted reviews, the output says the figures describe ranking among reviewed cases only.

**How results come back.** The kit writes `summary_to_share.csv` and `run_info.txt`: row and error counts by period, the tables above with intervals, the number and types of input columns, package versions and the kit version. No case rows and no IDs. Column names and their importance are the most useful thing a state could send and are included only if the state sets a switch. The state emails the two files to the contact who sent the kit. The kit sends nothing by itself.

## 5. Where it should live

```
methods/case_predictions/state_fit_kit_experimental/
  README.md                      what it is and is not; setup; how to read the output; what to send back
  fit_and_compare.R              the one script a state runs
  config_template.R              the block a state edits (file, label, amount, date, columns)
  install_packages.R             xgboost from CRAN, then the self-check
  install_catboost_optional.R    CatBoost from a local .tgz or the CatBoost release page
  R/                             read_input.R, split.R, fit_xgb.R, fit_catboost.R, score.R, report.R
  example/                       one state's public QC cases as an example file, and the expected summary
  DESIGN.md                      the four-item design note and the reading rule for returned results
methods/case_predictions/make_state_fit_kit.R    builds dist/snap_case_fit_kit_experimental_v0.1.zip
```

- **In the repo.** Under `methods/`, beside the package source. `VERSIONING.md` already says everything under `methods/` is research code with no compatibility promise, so no new rule is needed. The folder name carries the word "experimental".
- **Release asset.** The zip goes on a GitHub pre-release tagged `fit-kit-v0.1-experimental`. A pre-release is not what `releases/latest` serves, so the README's workbook link keeps pointing at v2.8.1 (GitHub behaviour as I know it; not checked today). The link the lead sends a colleague is the pre-release page or the folder's README.
- **README.** One short block under "Where to start", after the two alternate routes, headed "Experimental: fit a case model on your own cases", two sentences and the link. Not in the "State agencies" line at the top, which should keep sending states to the workbook.
- **GUIDANCE.** Nothing. The doc checker requires every GUIDANCE bullet to cite a findings section, and no finding supports the kit.
- **Evidence.** `methods/challenger_blend_candidate/` stays as the record of the blend. The kit README links to it and to section 54.
- **Label.** "Experimental" in the folder name, the zip name, the README title, the first line of console output and the header of `summary_to_share.csv`.

**What must not change.** `methods/case_predictions/snap_case_model/` and the v2.8.0 zip; `MODEL_CARD.md`; the deploy bundle; the workbooks and `state_delivery_lists/`; the README map and its figures; the target of `releases/latest`; the ledger rows for B. The package README should not gain a pointer to the kit, because that would change the zip.

**The paragraph a state colleague reads first.**

> **Experimental: fit a case-ranking model on your own cases.** This kit is a test, not a recommended tool. The shipped model (`snap_case_model`) was trained on public Quality Control cases and uses 21 columns. This kit fits a model on your own reviewed cases instead, with whatever columns you have, including ones the public data lack. It sets aside your most recent period, fits on the earlier cases, and reports how many errors and error dollars each ranking would have found in the top 5% and 10% of the cases set aside: your columns with xgboost; the same with a second learner (CatBoost) averaged in, if you have installed it; and the shipped model, if you can supply its 21 columns. It has not been tested on any state's records. On public data the second learner added a few tenths of a point at most, too little to measure. Nothing leaves your machine. If you are willing, send us the two summary files it writes; they hold counts and percentages, no case rows.

## 6. What the project's rules require before it goes to a state

1. **Design note to the lead** (four items): the question in one sentence; what varies, one component at a time (columns, then learner); support after the split, with the stops above; what the ledger says, cited (the single-state hazard and the national-default rows, sections 9 and 14; the section 46 leak; the section 51 fill rule; section 54).
2. **Written under the principal-data-scientist skill, then a fresh senior-statistician review** of the script and the note before it is committed (`known_constraints.md`, routing). The kit is a new script under `methods/`. The reviewer should look hardest at the split, the early stopping, the leak guards, and what the output claims when the labeled cases are not a random sample.
3. **A dry run on public data, as a state would run it.** For each of the 49 states, treat the state's public rows as its file: fit through FY2023, hold back FY2024, three arms plus B. It takes minutes and stays well under 8 GB. It checks the plumbing against the benchmark file and tells the lead what to expect; my expectation is that own-state fits lose to B without B's score as an input (inferred). It is a new study script, so items 1 and 2 cover it.
4. **A reading rule for returned results, written before any come back**, as was done for the pile test now out with a state partner. Three levels (ahead, supported, not distinguishable), with the power stated for the state's number of held-back cases, so that a small real gain is not read as a failure and a noisy one is not read as a win.
5. **A self-check** on the example file that reproduces an expected summary within a tolerance (training is not byte-identical across xgboost versions; CRAN is now at 3.2.1.1, the package was built with 3.1.2.1).
6. **Logging.** The dry run is logged as a findings section marked exploratory. The ledger gets an Open-work row ("per-case model fitted on a state's own file: kit at this path, untested on state records, awaiting returns"), not a claim row. The blend row's "not in the state package" stands; add "offered as an optional arm of the experimental kit" if the lead decides so.
7. **CHANGELOG and version.** No version bump. A one-line "Added (experimental)" entry under Unreleased is consistent with how the evaluation benchmark and section 53 were entered, and is the lead's decision. Run `methods/check_doc_consistency.sh` after any doc change.
8. **The state-facing README** goes through the content-designer skill, as the package README did (inferred from the skill being in the repo; not confirmed).

Nothing in the rules lets the kit enter the recommended workflow. "Nothing enters the recommended workflow without held-out-year validation" applies, and the kit has none on state records. That is what the experimental label is for.

## 7. Things to check

- **What labels the colleague's state has.** A QC sample of about 1,000 cases a year gives about 100 held-back errors per year held back. That can show a very large gain and nothing else. Targeted internal reviews give more cases and a selected sample. Ask before building: number of labeled cases per year, whether error amounts are recorded, how cases were chosen for review.
- **Whether the state's IT allows a package from outside CRAN.** If not, the blend arm is skipped and nothing is lost.
- **Branch.** Section 54 and this folder are uncommitted on `staging-v2.7`. A colleague can only be pointed to `main` or a pre-release.
- **What I did not do.** I did not rescore from the per-fit files. I did not test the CatBoost install on a locked-down machine or through `remotes::install_url`; I downloaded the release `.tgz` and ran `R CMD INSTALL` into a scratch library, leaving the user's R libraries untouched. I did not read the statistician's blend review in full. The power figures in section 3 are scaled from public-data standard errors and assume a state error rate near 10%.
