# Challenger benchmark for the per-case model

Started 2026-11-12. This folder benchmarks other learners against model B, the per-case xgboost model in `snap_qc` (findings 46, 48, 51). It sits outside the repositories, reads the staging checkout (`C:/Users/ericg/snap_qc_v27`) and writes nothing to it. Nothing here is a finding until the project lead logs it.

Read `design_note.md` first: the question, what varies, the support, the reading rule set before scoring, and the two reviews that preceded the run.

## Files

| File | What it does |
|---|---|
| `design_note.md` | The design, the reading rule and the review record |
| `01_export.R` | Exports B's training and test rows, B's frozen settings and B's stored scores, using the frozen s46 code; checks the test rows against `eval_benchmark_f1109.rds` case by case |
| `02_fit.py` | Fits the Python learners: `xgb_ref`, `xgb_tuned`, `lgbm`, `catboost`, `ebm_main`, `ebm_pairs`, `tabnet` |
| `03_fit_bart.R` | Fits BART (dbarts) |
| `04_score.R` | Scores every learner with `score_ranking()` and the findings 46 statistics, against B's stored scores |
| `run_all.sh` | Runs the learners one at a time under time caps, then scores |
| `data/` | The export (CSV at 17 significant digits, rds copy, column hashes, `settings_B.csv`, `support.csv`) |
| `tuning/` | Each learner's chosen settings and its held-back-year diagnostics, per test |
| `scores/` | One file per learner, test and seed: P, size and expected error dollars for each test case |
| `results/` | `Q.csv`, `by_window.csv`, `by_state.csv` |
| `logs/` | Run logs, `status.log`, and the scripts that applied the review changes |
| `reviewed_v1/`, `reviewed_v2/` | The scripts as each review read them |
| `superseded_before_review/` | The first `xgb_ref` run, made on the 15-digit export |
| `requirements_frozen.txt` | The Python environment (`.venv`) |

To rerun from scratch: `Rscript 01_export.R`, then `.venv/Scripts/python.exe 02_fit.py xgb_ref`, then `bash run_all.sh`. A rerun skips any score file that exists.

## The libraries, and what running each takes on this machine

The machine: Ryzen 7 7840U (8 cores, 16 threads), 60 GB memory, integrated Radeon graphics, no NVIDIA GPU. Timings are on the FY2024 test's 197,060 training rows.

| Library | Runs here | Measured cost | Fits B's structure |
|---|---|---|---|
| xgboost 3.4.1 (reference) | yes | 15 s for P, 3 s for size | yes |
| LightGBM 4.7.0 | yes | 7 s for P, 1 s for size | yes: binary and gamma losses, offset, native categorical |
| CatBoost 1.2.10 | yes | 2 s per 150 rounds with state indicators; 24 s with target statistics | partly: no gamma loss (Tweedie 1.9 used), offset through `baseline` |
| EBM (interpret-core 0.7.8) | yes | 82 s for P with main effects only; 155 s with pair terms; 23 s for size | yes: log loss and gamma deviance, offset through `init_score`, nominal state term |
| BART (dbarts 0.9-32, already installed in R) | yes | 0.2 s per sweep at 200 trees | partly: probit for P; size on log dollars |
| TabNet (pytorch-tabnet 4.1.0, CPU torch 2.15) | yes | 10 s per epoch | partly: no gamma loss or offset; size on log dollars |
| TabPFN | no | not run | see below |
| Synthefy Nori | no | not run | see below |

### TabPFN (github.com/PriorLabs/TabPFN)

Not run. What it would take:

- **A GPU.** On CPU the current models refuse more than 5,000 training rows (1,000 for older versions). An override exists, but the three tests train on 80,071, 119,310 and 197,060 rows. The project page says about 8 GB of GPU memory works for most data and 16 GB is needed for some large data.
- **A license decision.** The code is Apache 2.0. The weights for TabPFN-2.5 and later are non-commercial: testing, evaluation and research are allowed; production use, client deliverables and outputs used in business decisions are not, without a commercial license from Prior Labs. Benchmarking here falls inside the license. Shipping a TabPFN model to state agencies would need the commercial license. Only TabPFN-2 is under an Apache-style license, and it is limited to 10,000 training rows.
- **A login.** The first use opens a browser to accept the license; a token (`TABPFN_TOKEN`) covers unattended runs.
- **Row limits by version:** TabPFN-2, 10,000; 2.5 and 2.6, 100,000; 3 and 3.5, 1,000,000. Only version 3 and later cover the FY2022 and FY2024 tests whole.
- **A hosted option.** `tabpfn-client` sends the data to Prior Labs' servers under a daily quota. The QC data are public, so that is possible; the same license terms apply to outputs, and the limits are returned by the service, not published.
- **Fit to the harness.** It has a classifier and a regressor with the scikit-learn interface, so it would slot into `02_fit.py` as P x size with the size model on log dollars, like TabNet. It has no gamma loss and no offset.

### Synthefy Nori (github.com/Synthefy/synthefy-nori)

Not run. What it would take:

- **A GPU.** The model attends across all context rows, so cost grows with the square of the training rows. Its published runs used H100 and H200 GPUs with up to 50,000 context rows; above 50,000 rows a policy picks which rows form the context. A CPU fallback exists with no published timings.
- **Regression only.** There is no classifier. B's structure would need P fitted as a regression on the 0/1 label, or a single model fitted to error dollars directly (findings 46 candidate G's structure, which B beat).
- **License and access.** Code and weights are Apache 2.0, public on Hugging Face, no login. This is the easier of the two to adopt if it worked.
- **Version pin.** It is tested on torch 2.8 to 2.13; this environment has 2.15, so it needs its own environment.

A CPU run of either model on a few thousand sampled rows per state is possible but would test a cut-down version of the model, not the model. It was left out for that reason.

## Results

Run 2026-11-12 21:51 to 2026-11-13 01:18. Every stage finished; none hit its time cap. Full tables: `results/top_metrics.csv`, `results/by_window.csv`, `results/by_state.csv`, and the printout in `logs/04_score.log`.

**The measure.** National share of error dollars caught, computed as the results map computes its nationwide line: each state's share weighted by its official error dollars, 48 states. The three tests (FY2019, FY2022, FY2024) are combined with weights 0.2 / 0.4 / 0.4. Differences are in points against the stored 10-seed B; z is the difference over its paired standard error.

| Learner | Fits | At 5% | 5-10% band | At 10% | Difference at 10% (z) | Difference at 5% (z) | Precision at 5% | Reading |
|---|---|---|---|---|---|---|---|---|
| B, stored scores | 10 | 18.12 | 11.39 | 29.51 | | | 27.4 | |
| xgb_ref (B's recipe refit here) | 10 | 18.28 | 11.19 | 29.46 | -0.05 (-0.1) | +0.15 (+0.5) | 27.6 | same as B: harness check passes |
| xgb_tuned (xgboost re-tuned on f1109) | 10 | 18.12 | 11.12 | 29.24 | -0.27 (-0.7) | -0.00 (-0.0) | 27.4 | same as B |
| LightGBM | 10 | 17.77 | 11.55 | 29.32 | -0.19 (-0.4) | -0.35 (-0.8) | 28.0 | not distinguishable |
| CatBoost | 5 | 17.80 | 12.25 | 30.05 | +0.54 (+1.0) | -0.32 (-0.7) | 27.8 | not distinguishable |
| BART | 4 chains | 17.53 | 12.44 | 29.97 | +0.46 (+0.6) | -0.59 (-0.9) | 27.3 | not distinguishable |
| EBM with pair terms | 3 | 16.99 | 12.01 | 29.00 | -0.50 (-0.8) | -1.13 (-1.9) | 26.7 | not distinguishable |
| EBM, main effects only | 3 | 15.75 | 11.87 | 27.63 | -1.88 (-2.4) | -2.37 (-3.6) | 26.2 | behind B at 10% and at 5%, at the stated settings |
| TabNet | 3 | 16.76 | 11.55 | 28.31 | -1.19 (-1.4) | -1.36 (-1.9) | 27.3 | not distinguishable |

What the run supports:

- **No learner is ahead of B.** None is 2 standard errors above B on any of the three metrics.
- **At 5%, no challenger's figure is above B's.** The gaps run from -0.3 points (CatBoost, LightGBM) to -2.4 (EBM main effects).
- **CatBoost and BART are about half a point above B at 10%**, all of it from the 5-10% band (+0.9 and +1.1 points) against a loss at 5%. Both are inside one standard error.
- **The additive EBM is behind.** With main effects only it catches 27.6% at 10% against B's 29.5%. Adding pair terms recovers most of the gap at 10% (29.0%) but not at 5% (17.0% against 18.1%).
- **Re-tuning xgboost on f1109 changed nothing measurable** (-0.27 points at 10%, z -0.7), so B's carried-over settings are not costing it.
- **What the run cannot see.** The standard error of a difference at 10% is 0.3 to 0.4 points for the xgboost arms, 0.5 for LightGBM and CatBoost, and 0.6 to 0.8 for BART, EBM and TabNet. A true gain under about one point for those learners would not register. The tests are the three that chose B, and there is no FY2025 file to confirm on.

State-level companions (changes against B; harmed = a state whose share of error dollars caught falls by more than 0.05). Counts are the range over the three tests and two workloads:

| Learner | States harmed |
|---|---|
| B from its first 5 seeds (seed noise alone) | 0-1 |
| xgb_ref | 0-4 |
| xgb_tuned | 1-5 |
| LightGBM | 1-5 |
| CatBoost | 3-9 |
| BART | 7-9 |
| EBM with pair terms | 6-12 |
| EBM, main effects only | 11-16 |
| TabNet | 5-14 |

A different learner moves individual states even where the national figure is unchanged. Pooled precision at 5% stays within 1.2 points of B's 27.4% for every learner.

Companions on other bases are in `logs/04_score.log`: the share with QC sample cases pooled over 49 states (the model card's basis; B 30.07% at 10%, LightGBM 30.22%, CatBoost 30.03%, EBM main effects 28.18%), and findings 46's Q (B 3.453, LightGBM 3.462, CatBoost 3.464, EBM main effects 3.134).

Settings each learner chose on the held-back training year are in `tuning/`. CatBoost chose target statistics for the state in five of its six models; TabNet chose width 8 in all three tests; BART and EBM took the size offset only where B did, except EBM main effects in FY2022.

## Follow-up reviews, 2026-11-13

Two fresh reviews looked at whether CatBoost and BART were tuned well and whether a different specification could do better: `reviews/followup_statistician_review.md` and `reviews/followup_principal_ds_review.md`, with their scripts in `reviews/scratch_statistician/` and `reviews/scratch_principal_ds/`. New fits in those folders were judged on held-back training years only. Nothing in them is a finding.

- The half-point gain of CatBoost and BART at 10% is carried by California, New York and Florida on the map weights. Without those three states the same rankings read -0.15 (CatBoost) and -0.30 (BART) against B; with sample cases pooled, -0.03 and -0.39.
- More seeds cannot settle it. The paired bootstrap over cases is 91 to 93% of the variance of the difference; 30 seeds would move the standard error from 0.54 to about 0.54 (CatBoost) and 0.73 to 0.70 (BART).
- CatBoost's tuning was adequate. BART was under-run: its P model was at default priors and still improving at sweep 1,500, and its size model reads 8% low.
- `04_score.R` understates a bag's seed variance by about a factor of 2 at five fits (the seed term falls more slowly than one over the number of fits). Standard errors in the table above are 5 to 10% low. No reading changes.
- Leads from held-back years, none specific to CatBoost or BART: weighting FY2022-on training rows up or adding an era indicator (FY2024 test only); score = P x size^1.2; ratio inputs.

## Follow-up run, 2026-11-13 to 14

The leads from the follow-up reviews were tested: changes to the structure of B (ratio inputs, an era indicator or era weights, a size exponent), CatBoost twins, a longer BART run, and a blend of xgboost and CatBoost. Design, reading rules and the two pre-run reviews are in `design_note_followup.md`; scripts `05_followup.py`, `06_bart_proper.R`, `07_score_followup.R`; outputs in `followup/` (promotion in `promoted.json`, tables in `results/`, full scoring output in `logs/run_score.out`). Not logged as a finding.

**Result: no arm is distinguishable from B.** Pieces were promoted on held-back training years (FY2018, FY2023) by a written rule, then fitted once on the test years and scored in one read. National share of error dollars caught, map state weights, three tests weighted 0.2 / 0.4 / 0.4; B is 18.12% at 5%, 11.39 in the 5-10% band, 29.51% at 10%:

| Arm | At 5% | 5-10% band | At 10% | Diff at 10% (SE) | z |
|---|---|---|---|---|---|
| xgboost, all promoted pieces (primary) | 18.22 | 11.26 | 29.47 | -0.03 (0.49) | -0.1 |
| the same without the size exponent | 18.20 | 11.59 | 29.79 | +0.28 (0.48) | +0.6 |
| blend of xgboost and CatBoost | 18.26 | 11.97 | 30.22 | +0.72 (0.48) | +1.5 |
| CatBoost with the era indicator | 18.06 | 12.07 | 30.12 | +0.62 (0.59) | +1.0 |
| BART, k = 1, 8 chains x 4,500 sweeps | 17.30 | 12.44 | 29.73 | +0.23 (0.79) | +0.3 |
| the same with its size exponent | 17.35 | 12.42 | 29.77 | +0.26 (0.83) | +0.3 |
| xgboost twin of B (same recipe refit) | 18.34 | 11.22 | 29.56 | +0.05 (0.41) | +0.1 |

- Promoted on held-back years: ratio inputs and the era indicator for the xgboost P model, the era indicator for the size model, a size exponent of 1.153, the era indicator for CatBoost, the blend, BART at k = 1 with exponent 1.330. Not promoted: ratio inputs for the size model, CatBoost regularisation, CatBoost ratio inputs, the CatBoost exponent.
- The promoted xgboost pieces fit the test years better (dollar deviance z -3.2 against B, P log loss z -2.7 against the twin) but that did not move the share at the top of the list. With sample cases pooled the primary arm is +0.49 (z +1.4).
- The blend is the nearest to a gain: +0.72 at 10% (z +1.5), +0.46 on the mean of the shares at 5, 7.5 and 10% (z +1.5), +0.99 without the three largest states (z +2.3), +0.83 with sample cases pooled (z +2.3), all guards met. It falls short of the bars set before the run (2.39 at 10% and 2.13 on the mean share, for one of three secondary arms).
- The longer BART run did not help: 29.73 against 29.97 last night at 10%, behind B at 5% by 0.8 points, and a worse fit than the xgboost twin (P log loss z +6.2).
- Power: with a standard error near 0.5 the run would show a gain of 1.5 points about 85 to 90% of the time, 1.0 point about 55 to 60%, 0.5 point about 17 to 20%. A real gain of half a point is not ruled out.

Run notes. The run was stopped twice for low memory. `dbarts::bart` holds about 2 x training rows x sweeps x chains x 8 bytes during burn-in (38 to 94 GB at 8 chains x 4,000 sweeps here); the test stage was changed to make the burn-in in pieces of 250 sweeps, which gives the same draws to 1e-14 and peaked at 19.4 GB. That change was made after promotion was frozen and was not reviewed; it does not touch promotion, the other arms or the scorer (section 11 of the design note).

## Blend validation, 2026-11-14

Eric asked whether there is a conceptual reason for the xgboost and CatBoost blend to rank better, and for one more held-out test before the blend is kept as a candidate. Two reviews (`reviews/blend_review_statistician.md`, `reviews/blend_review_principal_ds.md`), a design note with the reading rule written before the read (`design_note_blend_validation.md`), script `08_blend_validation.py`, outputs in `followup/blend_validation/`. Not logged as a finding.

**Conceptual answer.** There is a reason to expect a small gain. An xgboost bag and a CatBoost bag share 76 to 81% of the top-10% list; two xgboost bags share 92 to 96%. The two learners are about equally accurate, so their average fits better (dollar deviance z -3.2 against two xgboost fits). The gain is in P. Both reviewers expect 0.2 to 0.4 points at 10% and nothing at 5%; the +0.72 against B also credited the promoted xgboost pieces.

**Validation.** Blend minus a matched two-fit xgboost average, on FY2017 (scored backward from new fits on FY2018-19), FY2018 and FY2023 (saved held-back predictions), weights 0.25 / 0.25 / 0.5. Rule: promising if at least +0.25 at 10% and above zero on the mean of the shares at 5, 7.5 and 10%.

| | Combined (SE) | FY2023 | FY2018 | FY2017 |
|---|---|---|---|---|
| At 5% | +0.59 (0.59) | +0.18 | -0.27 | +2.29 |
| At 10% | +0.53 (0.55) | -0.13 | +0.24 | +2.15 |
| Mean of 5 / 7.5 / 10% | +0.57 (0.32) | +0.37 | -0.09 | +1.64 |
| Sample-pooled at 10% | +0.49 (0.33) | +0.51 | +0.57 | +0.38 |

**Reading: promising by the written rule**, with two qualifications. The figure at 10% is carried by FY2017, where the matched xgboost arm was itself 1.5 points below the B-recipe twin; on FY2018 and FY2023 alone the difference at 10% is 0.0 and the mean-share difference is +0.2. Across all six years the difference at 10% is +1.5, +1.5 and -0.3 on the test years (FY2019, FY2022, FY2024) and -0.1, +0.2 and +2.2 on the validation years: the two years with the most training data, FY2023 and FY2024, show none. The sample-pooled difference is positive in five of the six years.

## State-by-state reading, 2026-11-14

The national figures weight states by error dollars. `09_state_medians.py` asks whether an approach raises the typical state: the median over 49 states of the state share of error dollars caught, and the median and mean of the paired state changes against B, on the three test years (`followup/results/state_medians.csv`). `09b_state_medians_validation.py` repeats the paired changes on FY2017, FY2018 and FY2023 against the B-recipe twin (`state_medians_validation.csv`). Descriptive: no rule was written for this reading, and it covers 13 arms.

- No learner clearly raises the typical state. EBM lowers it (main effects: median paired change -1.6 points at 5%, -2.5 at 10%). BART and CatBoost alone harm 4 to 11 states per test by more than 5 points; the xgboost arms and the blend harm 1 to 4.
- The promoted xgboost pieces raised the typical state at 5% on the test years (median paired change +0.82 without the exponent, z +3.4; mean +0.74) although the national figure was flat. On the validation years the same contrast is +0.05 (z +0.4), mean +0.35 (z +1.0). Not replicated.
- The blend is positive on both sets. Test years, against B: median state share at 10% 30.98 against 29.32; median paired change +0.34 (z +1.6), mean +0.61 (z +1.7). Validation years, against the twin: median +0.86 (z +2.9), mean +0.69 (z +1.7) at 10%; +0.29 and +0.66 at 5%.
