# Follow-up review: specifications for CatBoost and BART

Principal data scientist, fresh context, 2026-11-13. Written for the project lead. A senior statistician is reviewing tuning adequacy, the noise decomposition and the follow-up's statistical design in parallel; this review leads on how to structure the models and their inputs.

Every claim is marked **checked** (I ran it or read the evidence) or **judged** (my expectation). Nothing here is a finding until the project lead logs it.

## 1. Summary

1. **The half-point gain is not a learner effect that a better specification can build on (checked).** It sits in the P model, in a few large states, and only on the map's state weights. On the sample-pooled basis CatBoost is -0.03 points from B at 10% and BART is -0.39. The unweighted mean change over 48 states is negative for CatBoost in all three tests. The states where CatBoost beats B in one test are not the states where it beats B in the next (correlations +0.09, +0.16, -0.25). On FY2024 CatBoost is behind B (-0.35).
2. **The band gain is mostly the 5% loss counted again (checked).** CatBoost's top-10% lists hold 79-84% of B's cases. A learner that picks nearly the same top 10% but orders it less sharply loses at 5% and gains the same amount between 5% and 10%. The figure that carries information is the 10% share: +0.54 with a standard error near 0.5.
3. **Tuning was adequate for CatBoost and thin for BART (checked on held-back years).** CatBoost matches re-tuned xgboost on the held-back log loss in two of three splits. Two settings last night did not search (`l2_leaf_reg`, `rsm`) improve it slightly. BART's P model was run at defaults and trails xgboost on the held-back log loss in all three splits by more than ten times the seed-to-seed spread.
4. **The leads I found are changes to B's structure, not properties of CatBoost or BART (checked on held-back years, single window for the first).** In order:
   - weighting the post-pandemic training years up, or adding an era indicator (applies to the FY2024 test and the deploy fit only): +1.8 points at 10% on held-back FY2023, three seeds, z 2.3; +1.2 on the sample-pooled read, z 2.6;
   - ratio inputs built from the 18 permitted fields: lower held-back log loss in both years I ran; +0.5 to +1.2 at 10%, inside noise on its own;
   - a three-class label that separates errors at or below the tolerance from no error: small gain in log loss; carries a validity hazard (section 5, S3).
5. **Blends, ranking losses, direct dollar models and a wider CatBoost tree shape showed nothing consistent on the held-back years (checked).** Stratified models, direction splits and per-state second stages already lost in findings 46; nothing about CatBoost or BART changes those answers (judged).
6. **A multi-seed rerun of last night's settings cannot confirm half a point (judged from last night's standard errors).** Two xgboost arms with ten seeds each still differ with a standard error of 0.38 at 10%. Most of the error is which cases the test years sampled, and more seeds do not reduce that.
7. **Recommended evening:** a held-back-year stage that reads no test label (xgboost and CatBoost structure arms, a wider CatBoost grid, BART's k), a promotion rule fixed in advance, then at most four specifications on the three tests with xgboost twins, then the seed top-up. About 5.5 hours of the 8 (section 7).

## 2. Where the band gain came from

All figures in this section use test-year labels and last night's score files. They describe; they were not used to choose anything. Script: `reviews/scratch_principal_ds/a_where_gain.R`, output `a_where_gain.log`, per-state table `a_state_contributions.csv`.

**It is in P, not in size (checked).** I multiplied each learner's seed-averaged P by another learner's seed-averaged size. xgb is `xgb_ref`, B's recipe refit, because B's stored scores hold only the product. Combined over the three tests, national share, percent:

| P from | Size from | At 5% | 5-10% band | At 10% |
|---|---|---|---|---|
| xgboost | xgboost | 18.36 | 11.12 | 29.48 |
| xgboost | CatBoost | 18.28 | 11.12 | 29.40 |
| xgboost | BART | 18.53 | 10.70 | 29.23 |
| CatBoost | xgboost | 17.55 | 12.56 | 30.10 |
| CatBoost | CatBoost | 17.76 | 12.30 | 30.06 |
| BART | xgboost | 17.89 | 11.98 | 29.88 |
| BART | BART | 17.53 | 12.44 | 29.97 |

Swapping the size model moves the 10% share by 0.1 points or less for CatBoost. The whole pattern (lower at 5%, higher in the band) follows the P model. Any specification work should go to P.

**It is in a few large states and only on the map's weights (checked).**

| | FY2019 | FY2022 | FY2024 |
|---|---|---|---|
| CatBoost minus B at 10%, map weights | +1.09 | +1.17 | -0.35 |
| of which the six largest-weight states (sum of rounded state contributions) | +0.77 | +1.06 | -0.06 |
| CatBoost minus B, unweighted mean of 48 states | -0.37 | -0.41 | -0.28 |
| States up / down at 10% | 28 / 20 | 23 / 24 | 21 / 27 |
| BART minus B at 10%, map weights | +0.07 | +1.16 | -0.05 |
| BART minus B, unweighted mean of 48 states | -0.14 | +0.36 | -1.04 |

- In FY2022 California alone contributes +1.25 of CatBoost's +1.17 (its share rises 9.5 points on 73 flags) and Florida +0.53. In FY2024 California contributes -0.52.
- California's change for CatBoost across the three tests: +2.1, +9.5, -3.9 points. Florida's: -3.3, +9.7, -0.7.
- The weights are concentrated: the effective number of states is 17 to 20 of 48. A state-cluster standard error of the weighted difference at 10% is 0.66, 1.41 and 0.69 points by test for CatBoost, 0.64 combined. That gives z near 0.8, in line with the reported 1.0.
- Sample-pooled (the model card's basis, `logs/04_score.log`): CatBoost -0.03 at 10%, BART -0.39.

**It does not persist (checked).** The correlation over 48 states of (CatBoost minus B at 10%) between tests is +0.09 (FY2019-FY2022), +0.16 (FY2022-FY2024) and -0.25 (FY2019-FY2024). For BART: +0.07, +0.21, +0.05. For the band alone the correlations run -0.24 to +0.18.

**The lists differ at the margin, and the swapped cases are of equal quality (checked).**

| | FY2019 | FY2022 | FY2024 |
|---|---|---|---|
| Share of B's top-10% cases also on the list of: B's recipe refit | 0.94 | 0.95 | 0.94 |
| CatBoost | 0.79 | 0.80 | 0.84 |
| BART | 0.71 | 0.71 | 0.74 |
| Error rate, cases only CatBoost flags / only B flags | 0.197 / 0.181 | 0.195 / 0.198 | 0.199 / 0.210 |
| Error dollars per case, only CatBoost / only B | 28 / 24 | 33 / 34 | 38 / 42 |

One case in five differs, and the cases traded are worth the same. That is what two noisy rankings of the same signal look like.

**Why the band and not the top (checked in part, judged in part).** Ranked by P alone, xgboost catches 16.4% at 5%, CatBoost 15.8% and BART 15.7% (checked). CatBoost's and BART's P order the top of the list less sharply and the next tranche about as well. Because the 10% lists mostly coincide, a deficit at 5% reappears as a surplus in the band. I judge the band gain to be the mirror of the 5% loss plus noise, not a second signal.

**What this implies for specifications (judged).** There is no state group, household type or score range where CatBoost or BART is reliably ahead, so there is nothing for a stratified or staged CatBoost or BART to exploit. Eight structures in findings 46 (Q from 3.39 to 3.53) and six learners last night (28.3% to 30.1% at 10%, the additive EBM aside) all land in the same place on the same 18 inputs. The ceiling comes from the inputs and the training data, not from the learner.

## 3. Tuning (brief; the statistician leads)

**CatBoost: adequate, with two unsearched settings worth a little (checked).**

Held-back within state-year log loss at the chosen settings (`tuning/*.json`):

| Split (held-back year) | xgboost re-tuned | CatBoost | BART |
|---|---|---|---|
| W1 (FY2018) | 0.27393 | 0.27411 | 0.27521 |
| W2 (FY2019) | 0.27970 | 0.28097 | 0.28243 |
| W3 (FY2023) | 0.30286 | 0.30263 | 0.30650 |

- Seed-to-seed spread of this criterion is about 0.0001 (three xgboost seeds on W3's split: 0.30312, 0.30325, 0.30302). The whole xgboost depth grid spans about 0.002.
- CatBoost ties xgboost in W1 and W3 and trails in W2 by 0.0013. W2 is the test where CatBoost's test-year gain was largest (+1.17). The held-back year and the test year disagree about where CatBoost is good.
- The grid was six P settings (depth by state encoding) with everything else at defaults. Depth 8 was the edge of the grid in W1 and W3. I probed past it on the W1 and W3 splits (`d_cb_arms.py`): depth 10 gives 0.27437 and 0.30267, no better than depth 8 (0.27409, 0.30263). The edge was not binding.
- `l2_leaf_reg` 10 with `rsm` 0.5 gives 0.27363 and 0.30254, better in both splits. Lossguide trees give 0.27474 and 0.30397, worse in both.
- Not checked: learning rate 0.03, ordered boosting, `border_count`, `random_strength`. I expect under 0.0003 from any of them (judged).
- State encoding: target statistics won five of six choices. For P the two encodings differed by 0.0003 to 0.0009 in held-back log loss, and indicators won in W3. Target statistics cost 12 times the fit time (24 s against 2 s per 150 rounds).

**BART: the P model was not tuned, and it is behind on the held-back years (checked).** The only choice made was the size offset. BART trails re-tuned xgboost by 0.0013, 0.0027 and 0.0036 in log loss, and in held-back lift (3.06 / 3.40 / 3.31 against 3.14 / 3.46 / 3.55). My short-chain refits on W1's and W3's splits agree (0.27566, 0.30747). The chain-to-chain score correlation was 0.947 to 0.962, so 500 kept sweeps per chain leave each chain a noisy draw. BART's `k` (2) and tree count (200) are the settings to search. BART's test-year standing (29.97% at 10%) is better than its held-back standing predicts; I read that as the test's noise, not as a hidden strength (judged).

## 4. What the held-back years say

I ran small screens on the split `02_fit.py` uses: fit on the earlier training years, read on the held-back last training year. W1's split reads FY2018 (41,014 fit rows). W3's split reads FY2023 (156,990 fit rows). Neither is a test year. Test-year rows were not loaded. Scripts: `common.py`, `c_xgb_arms.py`, `d_cb_arms.py`, `e_bart_val.R`, `i_hurdle.py`, `j_recency.py`, `f_read.py`; outputs `read_W1.log`, `read_W3.log`, `j_recency.log`.

How to read them:

- Each arm changes the P model only. All arms share one xgboost size model at B's settings, because section 2 shows size does not matter.
- Shares are on the benchmark's flag rule and kept cases. "Map" weights each state by that year's FNS error rate times FY2024 issuance; "pooled" pools the sample cases.
- Standard errors are a paired bootstrap of cases within state (300 draws), fits held fixed. A single fit on a single year has a standard error of 0.6 to 1.5 points on the map weights. A second xgboost seed differs from the first by -0.01 and -0.13 with standard errors of 0.76 and 0.71. **These dollar reads cannot rank arms that differ by less than about 1.5 points.** The log loss can: its seed spread is 0.0001.
- New settings were early-stopped on the held-back year, which flatters them slightly. Known settings used last night's rounds scaled back to the fit rows.

**Single fits.** Change from xgboost at B's settings in the share at 10%, points.

| Arm | Log loss FY2018 | Log loss FY2023 | FY2018 map (SE) / pooled | FY2023 map (SE) / pooled |
|---|---|---|---|---|
| xgboost, B's settings | 0.27429 | 0.30312 | 26.16% / 27.56% | 28.08% / 29.63% |
| xgboost, second seed | 0.27441 | 0.30325 | -0.01 (0.76) / -0.31 | -0.13 (0.71) / +0.17 |
| xgboost + ratio inputs | 0.27392 | 0.30238 | +0.99 (0.97) / +0.67 | +0.53 (0.72) / +0.14 |
| xgboost, FY2022 rows weighted 3x | n/a | 0.30263 | n/a | +1.21 (0.73) / +1.31 |
| xgboost, three-class tolerance label | 0.27408 | 0.30257 | +0.72 (0.85) / +0.02 | +0.24 (0.63) / +0.02 |
| xgboost, hurdle: P(any error) x P(over, given any) | 0.27452 | 0.30267 | +0.76 (1.10) / -0.06 | +0.55 (0.77) / +0.35 |
| CatBoost, last night's settings | 0.27409 | 0.30263 | -0.40 (1.28) / +0.21 | +0.82 (0.86) / +0.40 |
| CatBoost + ratio inputs | 0.27402 | 0.30321 | -0.39 (1.21) / -0.71 | +0.47 (0.88) / +0.51 |
| CatBoost, l2 10 and rsm 0.5 | 0.27363 | 0.30254 | +2.00 (1.12) / +1.24 | +1.06 (0.93) / +1.34 |
| CatBoost, depth 10 | 0.27437 | 0.30267 | +1.39 (1.46) / +0.20 | +0.97 (1.01) / +1.08 |
| CatBoost, Lossguide | 0.27474 | 0.30397 | -0.55 (1.15) / -0.20 | +0.12 (0.94) / +0.03 |
| BART, k 2, 200 trees, short chains | 0.27566 | 0.30747 | -0.25 (1.34) / -0.15 | -0.01 (1.12) / -0.05 |
| CatBoost YetiRank on error dollars, state-year groups | | | -0.26 (1.46) / -0.02 | +1.44 (1.16) / -0.03 |
| CatBoost QuerySoftMax on error dollars | | | -1.44 (1.59) / -0.21 | -0.31 (1.10) / -1.20 |
| CatBoost Tweedie on all rows | | | -0.40 (1.11) / +0.22 | +0.06 (0.97) / +0.09 |
| CatBoost, log loss weighted by error dollars | | | +0.16 (1.39) / +0.16 | +1.06 (0.95) / +0.34 |
| Mean of xgboost and CatBoost P | | | -0.68 (0.86) / +0.34 | +0.11 (0.63) / -0.01 |
| Mean of xgboost, CatBoost and BART P | | | +0.04 (1.11) / +0.60 | +0.22 (0.75) / +0.32 |
| xgboost, two-seed bag | | | +0.23 (0.51) / -0.02 | -0.40 (0.46) / -0.13 |

YetiRank loses at 5% in both years (-1.2 and -1.4 points) and lowers pooled precision at 5% by 1.6 and 2.1 points, which would breach the 1.5-point guard.

**The era question, three seeds, FY2023 only** (`j_recency.py`). xgboost at B's settings; W3's split fits on FY2017-19 plus FY2022 and reads FY2023.

| Arm (3-seed bag) | Log loss | At 10%, map: change (SE, z) | At 10%, pooled: change (SE, z) | At 5%, map / pooled |
|---|---|---|---|---|
| Base | 0.30298 | 27.27% | 29.63% | 17.59% / 18.15% |
| FY2022 rows weighted 2x | 0.30245 | +0.76 (0.54, 1.4) | +0.69 (0.43, 1.6) | -0.68 / +0.27 |
| FY2022 rows weighted 3x | 0.30252 | +1.80 (0.78, 2.3) | +1.22 (0.47, 2.6) | -0.95 / +0.37 |
| Era indicator (1 from FY2022) | 0.30239 | +1.11 (0.46, 2.4) | +0.83 (0.37, 2.2) | -0.49 / -0.17 |
| Ratio inputs | 0.30205 | +1.16 (0.71, 1.6) | -0.14 (0.50, -0.3) | +0.39 / +0.76 |
| Ratio inputs + 3x weights | 0.30192 | +2.77 (0.81, 3.4) | +1.56 (0.54, 2.9) | -0.05 / +0.31 |

Every one of the three 3x fits is above every one of the three base fits at 10% on both reads (map 28.69-29.29 against 27.54-28.08; pooled 30.59-30.94 against 29.63-29.83).

Limits of this table: one held-back year; three seeds; the bootstrap holds the fits fixed; I ran about 25 arms across these screens, so a z of 2.3 to 2.6 is a lead, not a result. The 3x weight was my first guess and was not searched. In this split FY2022 is 24% of the fit rows; in the real FY2024 test the post-pandemic years are 39% of the training rows, so the gain there should be smaller (judged).

## 5. Ranked specifications

Rank is by how much I expect the 10% share to move, weighed by the evidence above. "Twin" means the same structure applied to xgboost; where a twin exists it is the fair comparison, and a gain shared by the twin is a reason to change B, not to change learner.

Cost figures are for this machine at 16 threads, from last night's logs: xgboost about 18 s per seed on the FY2024 test; CatBoost with state indicators about 2 s per 150 rounds (10 to 20 s a fit at depth 8); CatBoost with target statistics 24 s per 150 rounds (1.4 to 4 min a fit); BART 0.2 s per sweep at 200 trees.

### Shared structure (CatBoost, BART and xgboost alike)

**S1. Era handling: weight the post-pandemic training years up, or add an era indicator.** Expected: up to +1 point at 10% on the FY2024 test, so up to +0.4 on the combined metric; nothing on FY2019 and FY2022, whose training years are one era. The only idea here I expect to move a figure visibly.

- What: in the FY2024 test and the deploy fit, give FY2022-23 rows (FY2022-24 at deployment) a case weight of 2 to 3, or add one input that is 1 from FY2022 on.
- Mechanism: the FY2024 test trains on 119,310 pre-pandemic rows and 77,750 post-pandemic rows and scores a post-pandemic year. Between the two eras the tolerance rose from $37-38 to $48-56 and the frame's error rate from 8.6% (FY2017) to 11.9% (FY2024) (checked, frame counts). Pooling treats a FY2017 case as equal evidence about FY2024 as a FY2023 case. Weights keep the older rows for the stable structure and let the newer rows set the rest. The indicator lets trees fit era-specific splits; at scoring every case has it set to 1.
- Record: findings 46 (E3) chose between FY2022 alone and pooled with no era column, on held-back FY2023 lift: pooled won by 5.9%. Weights and an indicator are the middle ground it did not test. My screen is the table above (checked).
- Tuning: weight in {1, 2, 3, 5} or the indicator, chosen on held-back FY2023 by log loss and the 10% share, five seeds. No test label.
- Cost: nothing beyond a normal fit.
- Risks: it rests on one held-back year, the same year that already made the pooled-years choice. It lost at 5% on the map weights in the screen (-0.95) while gaining on the pooled read (+0.37); the 5% guard needs watching. It is not a CatBoost or BART idea: the xgboost twin is the natural home, and CatBoost and BART take case weights the same way (dbarts through `weights`).
- Comparability: FY2019 and FY2022 results do not change, so the combined metric moves by 0.4 times the FY2024 change. For deployment (FY2027 cases, three years ahead), the FY2024 test is the closest analogue but is one year ahead.

**S2. Ratio inputs built from the 18 permitted fields.** Expected: +0.2 to +0.6 at 10% (judged). Likely positive, likely below what the tests can resolve alone.

- What: eight added columns: earned share of income, shelter over gross, deductions over gross, uncapped minus capped benefit ratio, gross minus deductions, total gross (per-person gross times household size), any medical deduction, zero income. Definitions in `common.py` (`ratios`).
- Mechanism: a tree splits on one input at a time and needs many splits to approximate a ratio of two inputs. If error risk varies with a ratio (shelter relative to income, the earned share of income), handing the ratio to the tree saves it those splits (judged; the log loss below is the evidence that it does).
- Record: xgboost's held-back log loss falls in both years (0.27429 to 0.27392; 0.30312 to 0.30238, three-seed bag 0.30298 to 0.30205), four to nine times the seed spread. Shares at 10% rise +0.99 and +0.53 on single fits, +1.16 on three seeds for FY2023, but the pooled read on three seeds is -0.14 (checked). CatBoost does not benefit: 0.27409 to 0.27402 and 0.30263 to 0.30321 (checked). The ledger's vocabulary studies (findings 35-37) found that re-describing the same inputs did not move rule lists; those were rule mines, not this model.
- Tuning: in or out, on held-back log loss in all three splits, five seeds; the same way B's percentile inputs were chosen.
- Cost: none.
- Risks: the ledger warns that a feature which lines up the reconstruction income piles across years raises the pile risk (reconstruction-piles row). Ratios of income fields can do that. Run findings 46's point-mass detector and the non-pile reading beside any gain. No new column is asked of a state: all eight are arithmetic on the 18.
- Assignable to: structure. Twin required. On this evidence it helps xgboost and not CatBoost.

**S3. A three-class label: no recorded error, error at or below the tolerance, error over the tolerance.** Expected: 0 to +0.3 (judged). Small, with a validity hazard.

- What: B's label puts cases with a recorded error at or below the tolerance in with the clean cases. In the frame, 26 to 27% of cases each year have such an error (median $9 to $15), against 9 to 12% over the tolerance (checked; `g_below_tolerance.R`, counts on all six years including the test years, no model). Fit a three-class model and score with P(over tolerance); the size model is unchanged. CatBoost `MultiClass`, xgboost `multi:softprob`.
- Mechanism: the over-tolerance model sees 3.7 times as many cases with something wrong and can learn what separates "an error" from "a large error". The tolerance also moved ($37-38 in FY2017-19, $48, $54, $56 in FY2022-24), so a fixed over/not-over label means different things across training years.
- Record: P(over) from the three-class model has lower held-back log loss in both years (0.27429 to 0.27408; 0.30312 to 0.30257); shares +0.72 and +0.24 on the map weights and +0.02 on both pooled reads (checked). The two-stage hurdle is mixed (0.27452, 0.30267). Findings 46's candidate E split errors by direction, not by size, and trailed B (3.495 against 3.526); this is a different split and is not in the ledger.
- Tuning: B's grid on the three-class log loss, held-back year.
- Cost: three times a P fit.
- Risks, and they are serious: in FY2017, FY2018 and FY2023, 95.8% of cases with an at-or-below-tolerance error and 98.6% of over-tolerance cases have recreated inputs; no clean case does (checked; `k_recreated.R`). A model of "any recorded error" is close to a model of "inputs were recreated". On the held-back years xgboost predicts "any recorded error" with a rank AUC of 0.82, against 0.74 to 0.75 for B's own label, and B's P separates over-tolerance from at-or-below-tolerance errors with an AUC of only 0.62 to 0.63 (checked; `l_auc.py`). I did not test whether that 0.82 comes from case characteristics or from marks the recreation leaves. Until that is known, a gain from this label on the public tests could be recreation read better. It needs the point-mass detector, the path-robust reading and, in the end, a state's own file. I would not ship it on public tests alone.
- Assignable to: structure. Twin required.

**S4. Averaging P across learners (B's xgboost, CatBoost, BART).** Expected: 0 to +0.3 (judged).

- What: average the three P models, multiply by B's size model. Equal weights, fixed in advance.
- Mechanism: the rankings agree at 0.94 to 0.98 within state yet trade one top-10% case in five for cases of equal quality (section 2). Averaging learners of different shape reduces that margin noise more than extra seeds of one learner (a 5-seed and a 10-seed B differ by 0.03 points).
- Record: findings 46's stack of xgboost with ranger (C, 3.526) and with per-state adjustments (F, 3.531) equalled B (3.526). On the held-back years the xgboost-CatBoost mean is -0.68 and +0.11 at 10%, the three-way mean +0.04 and +0.22, pooled +0.34 / -0.01 and +0.60 / +0.32 (checked). Nothing visible. I did not score any blend on a test year: that would rank a new specification on test labels.
- Tuning: none (equal weights). A fitted meta-model is findings 46's C and bought nothing.
- Cost: the fits already exist for last night's settings.
- Risks: three libraries to ship and maintain for a state package, for a gain I expect to be unmeasurable. The fair control is an xgboost bag of the same number of fits, and xgboost with LightGBM.

**S5. Objectives aligned with the metric.** Expected: 0, with a loss at 5% for the ranking losses (judged from the screens). Long shot.

- What: YetiRank or QuerySoftMax on error dollars with state-year groups; Tweedie on all rows (findings 46's G, in CatBoost); log loss with error rows weighted by their dollars.
- Mechanism claimed: lists are cut within state, so only the within-state order of expected dollars matters, and these losses train on that directly.
- Record: none is ahead in both held-back years (table in section 4). YetiRank loses 1.2 to 1.4 points at 5% and 1.6 to 2.1 points of precision. QuerySoftMax is behind. Tweedie equals the base. Dollar-weighted log loss is +0.16 and +1.06 on the map read, +0.16 and +0.34 pooled, with a loss at 5%. Findings 46: G lost to B and failed the calibration guard (checked).
- Why it does not help here (judged): a state-year holds about 800 cases and 80 errors, and a few large errors dominate a dollar-weighted loss. P times size uses every row for P and every error row for size, which is the better-conditioned estimate at this sample size. B's tuning criterion already removes the state-year level.
- Note: `QueryCrossEntropy`, the log loss with a free shift per group, is GPU-only in CatBoost 1.2.10 (checked: it raised an error).
- Assignable to: structure; xgboost has `rank:pairwise` and `rank:ndcg`.

**S6. A second-stage model on the first stage's top 20%, or boosting on B's score.** Expected: 0 ± 0.3 (judged). Not run. Long shot.

- Mechanism claimed: the metric only reads the top 10% of each state, so a model trained on the cases near that cut specialises where it matters.
- Record: findings 46's F (a boosted adjustment per state on the stacked margin) and arm 3 (state trees on an out-of-fold margin) added nothing. Boosting already spends its later trees on the cases the earlier trees get wrong.
- What it costs: out-of-fold first-stage scores (five extra fits), or the second stage trains on a selection made by in-sample scores. The training set falls to about 31,000 rows on W3's split.
- If run at all: CatBoost with B's out-of-fold logit as `baseline`, trained on each state-year's top 20%, depth 4, early-stopped on the held-back year. Twin: xgboost with `base_margin`.

### CatBoost

**C1. A regularisation grid.** Expected: +0.1 to +0.4 over last night's CatBoost (judged).

- What: depth {6, 8} by `l2_leaf_reg` {3, 10, 30} by `rsm` {0.5, 1.0}, state indicators, learning rate 0.06.
- Record: l2 10 with rsm 0.5 lowered held-back log loss in both splits and read +2.00 and +1.06 at 10% against the xgboost base on single fits (+1.24 and +1.34 pooled) (checked). Against CatBoost's own base that is +2.4 and +0.2. The standard errors are near 1.
- Tuning: held-back log loss, as last night. Cost: 12 fits a split, about 10 minutes for all three splits with indicators.
- Risk: it is a tuning change, so it only matters if CatBoost is a candidate at all, and section 2 says the evidence for that is weak.

**C2. State indicators everywhere, not target statistics.** Expected: no change in the share; 12 times less compute (judged). The two encodings differed by 0.0009 or less in held-back log loss with the sign changing by test. Indicators free the evening's budget for seeds. CatBoost's feature-combination statistics (state crossed with tree splits) are already what the target-statistics arm used; they did not separate from indicators.

**C3. The shared structures with a CatBoost arm**: S1 weights, S3 `MultiClass`, each beside its xgboost twin. Ratio inputs (S2) did not help CatBoost on either held-back year; carry them only as a grid switch.

**C4. Not worth a slot (checked or judged as marked).** Depth 10 (checked: no gain). Lossguide (checked: worse). Ranking losses (checked: S5). Ordered boosting, lower learning rate, `border_count` (judged: under 0.1 point; ordered boosting helps small data and costs several times the fit time). Monotone constraints (judged: no input here has a relationship with error known to be monotone; a wrong constraint costs fit).

### BART

**B1. Tune k and the number of trees on the held-back log loss.** Expected: closes part of BART's held-back gap; +0.0 to +0.5 at 10% over last night's BART (judged).

- What: `k` in {1, 2, 3} at 200 trees; then 100 or 400 trees at the best `k`. P only.
- Mechanism: BART's prior shrinks each tree's contribution, and `k` sets how hard. The default of 2 was not chosen for 80,000 to 197,000 rows. BART's weakness is at the top of the list (5%), where shrinkage of the highest probabilities costs most (judged).
- Record: BART is behind on held-back log loss in all three splits (section 3) (checked). No `k` was tried.
- Cost: about 17 minutes per setting for the three splits at 1,000 burn-in and 500 kept sweeps.
- Risk: BART ends as good as xgboost and CatBoost, no better. Its harmed-state count was the highest of the three (7 to 9).

**B2. BART for P only, with xgboost's size model.** Expected: no change in the share; halves BART's run (checked on test labels, section 2: BART's P with xgboost's size reads 29.88, with its own 29.97; xgboost's P with BART's size reads 29.23). It also removes the lognormal size model, the part of BART least like B.

**B3. More chains and sweeps.** Expected: lower chain noise, no shift in the mean (judged). Eight chains of 1,000 kept sweeps. BART's single-chain spread at 10% was 0.5 to 1.1 points against 0.3 to 0.4 for a B seed.

**B4. Not worth a slot (judged).**
- State random intercepts (`rbart_vi`): a state main effect cannot change a within-state list except through the probit curve. Findings 46's B-over-A gain came from the state term in a tree model, where it interacts with inputs; a random intercept has no interactions.
- State-centred or percentile inputs: B's own choice was percentiles in for FY2019 and out for FY2022 and FY2024.
- Zero-filled blanks: 81, 123 and 210 cells in the three windows. Too few to matter.
- Logit link, heteroskedastic size: dbarts has no logit link, and size is not where the learners differ.
- Separate BARTs by household type or benefit level: see section 6.

## 6. Considered and not recommended

| Idea | What the record says | Would CatBoost or BART change it? |
|---|---|---|
| Separate models by household size | Findings 46, candidate D: 3.447 against B's 3.526. Findings 11's "coarse strata never lose" is about rule mining | No (judged). Both learners form interactions; splitting cuts each model's data by two thirds |
| Separate models by elderly/disabled | Findings 8: a feature, not a stratum | No (judged) |
| Overpayment and underpayment models, error type first | Findings 46, candidate E: 3.495 against 3.526; arms 4a and 4b. Findings 17: typed delivery retired | No (judged). CatBoost `MultiClass` is the same structure |
| Per-state second stage | Findings 46, candidate F: 3.531, no better than B, most complex | No (judged). Median state has 313 training errors after the tuning split |
| State groups, similar-state pools | Findings 12, 14: retired on a future year | No (judged) |
| Certification position or benefit-level strata | Not tested as strata. `months_since_cert_n` and the benefit ratios are inputs | No reason to expect a gain (judged): last night's EBM result shows pair interactions matter (27.6% to 29.0%) and full trees recover the rest (29.5%); nothing points to an interaction the trees miss |
| One model of error dollars (Tweedie) | Findings 46, candidate G: behind, calibration slope 0.7995 | No: CatBoost Tweedie on all rows equalled the base on both held-back years (checked) |
| Within state-year percentile inputs | B's choice: in for FY2019, out for FY2022 and FY2024. Findings 36: percentiles cost a moderate amount in rule mines | No (judged) |

Inputs the ledger rules out, for the record: `lben` as a tree input (findings 46 E1; offset only), `count_divisible_by_100` (built from reviewer fields), `cat_elig` (recoded in FY2024), `second_element_i`, `initial_month_i` (not a model input by decision; `prorated_i` is pre-registered in findings 50 and not yet run). BBCE-limit features and the six state options are open and deprioritised (findings 42).

The frame holds case fields a state has before review that are not among the 18, for example the length of the certification period, the reporting system and the type of the most recent action (checked: columns exist, `b_frame_cols.log`). Given that every learner and structure lands at the same ceiling, new information is where I would expect a larger gain than from any item in section 5 (judged). Each would need the leak check findings 46 applied to the member-record fields and a decision by the project lead. That is outside this request and I have not tested any of them.

## 7. What one evening should run

Principle: read the test years as little as possible. The three tests chose B and have now scored at least sixteen models (eight candidates in findings 46, eight learners last night). Most of the evening should be spent on the held-back years, with a promotion rule fixed in advance; only what passes is scored on the tests, each beside its xgboost twin.

One caution on the held-back years: W2's held-back year is FY2019, which is W1's test year. B's own tuning used it the same way. Stage 1 results on W2's split should be labelled as such.

**Stage 0. Freeze and harness check (10 min).** Design note and scripts hashed. Refit `xgb_ref` seed 117 on each test and assert the scores equal last night's files.

**Stage 1. Held-back years only; `04_score.R` is not run (cap 150 min).** Five seeds per arm for xgboost and CatBoost with indicators. Read: held-back log loss of the five-seed bag, and held-back shares at 5% and 10% on the map weights and pooled, with the paired bootstrap.

| Order | Block | Arms | Minutes |
|---|---|---|---|
| 1a | xgboost, B's settings, rounds by early stopping | base; S1 weights 2x, 3x, 5x and the era indicator (W3 only); S2 ratio inputs; S2 + S1; S3 three-class | 25, cap 40 |
| 1b | CatBoost, indicators | C1 grid (12 settings, one seed, three splits), then at the chosen setting five seeds of: base, S1 weights (W3), S3 `MultiClass`, ratio switch | 30, cap 50 |
| 1c | Blends from the 1a and 1b held-back fits | S4: mean P of xgboost and CatBoost; with BART when 1d ends; control: xgboost 10 seeds | 5 |
| 1d | BART, P only (B2) | B1: `k` 1 and 3 at 200 trees, 4 chains, 1,000 + 500 sweeps, three splits | 35, cap 45 |
| 1e | Only if time remains | S6 second stage with CatBoost on B's out-of-fold margin, W3's split; S5 dollar-weighted log loss, five seeds | cap 20 |

**Promotion rule, written before the run.** A specification goes to stage 2 only if, against the same learner's base:

1. its held-back log loss is lower in every split where it applies; and
2. its five-seed share at 10% is higher on both the map and the pooled read in at least two of the three splits. For arms that apply to W3 only: higher on both reads by at least one bootstrap standard error; and
3. its pooled precision at 5% on the held-back years is not more than 1.5 points below the base.

At most four specifications are promoted, taken in this review's order (S1, S2, C1, S3, B1, S4). If none passes, stage 2 is skipped.

**Stage 2. The three tests, last night's reading rule unchanged (cap 90 min).** Each promoted specification is refit on all training years with 10 seeds, beside its xgboost twin where the structure is not learner-specific. xgboost: about 6 minutes per arm for the three tests. CatBoost with indicators: about 10. `04_score.R` with the bootstrap: about 10. "Ahead" still needs 2 standard errors over B, over the matched bag and over `xgb_tuned`, and now also over the xgboost twin for the claim to belong to the learner. For S1 the reading that matters is FY2024 alone; say so before the run.

**Stage 3. The seed top-up the project lead proposed (cap 70 min).** CatBoost seeds 122-126 at last night's settings (about 35 minutes: 1.7, 3.9 and 1.4 minutes a seed by test). BART, four more chains (about 30 minutes). This equalises CatBoost's bag with B's ten seeds. It will not settle the half point (summary, item 6): expect the standard error at 10% to fall from about 0.5 to about 0.45.

Total at the caps: 320 minutes, leaving about 2.5 hours for overruns and one rerun.

**What I expect the evening to show (judged).** S1 passes stage 1 and gains on FY2024 by less than it did on FY2023. S2 passes on log loss and is borderline on the shares. C1 improves CatBoost by a few tenths without separating it from B. Nothing reads "ahead of B" at 2 standard errors on the combined metric. If S1 holds on FY2024 for xgboost, the result is a change to B's recipe and to the deploy fit, and CatBoost and BART remain alternatives of equal quality with more states moved.

## 8. Disclosures and limits

- **Test-year labels.** Section 2 and the BART and CatBoost rows of section 5 that cite it use test-year labels on last night's score files, for description only. The counts of at-or-below-tolerance errors by year (S3) were taken on all six years, test years included, with no model. No new fit was scored on a test year. No blend was scored on a test year.
- **Held-back screens.** Single fits unless marked, one or two held-back years, standard errors of 0.6 to 1.5 points, about 25 arms tried. They support log-loss comparisons and the era lead; they do not rank the other arms by dollars.
- **Not done.** W2's split; any stage-2 style refit; BART `k`; a second-stage model; a test of whether recreated inputs carry marks a model can read.
- **Compute.** About 35 minutes of wall time. Two runs went over five minutes while the machine was shared: BART on W3's split (about 6) and the three-seed era run (5.8). Threads were held to 6 for Python and 4 for BART.
- **Files.** Everything I wrote is in `reviews/scratch_principal_ds/` and this file. Nothing was written to `snap_qc_v27` or `snap_qc`, no existing benchmark file was changed, and nothing was committed.
