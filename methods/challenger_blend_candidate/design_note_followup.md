# Follow-up run: structural changes to B, CatBoost twins, and BART run properly

Design note for the project lead, 2026-11-13, written before anything is fitted. It follows `design_note.md` (last night's benchmark) and the two reviews in `reviews/`. Nothing here is a finding until the project lead logs it.

## 1. The question

Last night no learner was ahead of B. The two reviews then found, on held-back training years only, three changes to B's structure that lowered the held-back criteria, and found that BART had been under-run. Tonight's question:

**Does a revised B (xgboost with the changes that survive the held-back years) catch more error dollars than B on B's three tests? And does CatBoost or a properly run BART, given the same chance, do better than that?**

Eric approved the proposed arms on 2026-11-13 and added one instruction: "Be sure we're not excluding real improvements by setting the bars too high." Section 6 is written to that instruction.

## 2. What is unchanged

- The three tests (train FY2017-18, test FY2019; FY2017-19, FY2022; FY2017-19 plus FY2022-23, FY2024), the frame (f1109), the export in `data/`, and B's stored 10-seed scores.
- The scoring: `eval_benchmark_f1109.rds` through `score_ranking()`; the three top metrics (share of error dollars caught at 5%, added between 5% and 10%, at 10%); the map's state weights (48 states); the tests combined 0.2 / 0.4 / 0.4; the paired bootstrap of cases within state (2,000 draws, seed 4646).
- The fitting scripts load `02_fit.py`'s and `03_fit_bart.R`'s loaders verbatim, so the test year's labels are blanked at load. Only `07_score_followup.R` reads them, once.

## 3. The arms

All arms keep B's structure: score = P(error over tolerance) x size.

**xgboost at B's own settings** (depth, minimum child weight, column sampling, offset; rounds by early stopping on the held-back year, scaled to the full training rows as `xgb_tuned` did):

| Piece | What it is | Where it applies |
|---|---|---|
| Ratio inputs | Eight columns built from inputs B already has: earned share of income, shelter over gross, deductions over gross, uncapped minus capped benefit ratio, gross minus deductions, total gross, any medical deduction, zero income | All three tests |
| Era handling | Either a case weight of 2, 3 or 5 on training rows from FY2022 on, or an indicator that is 1 from FY2022 on | FY2024 test only (the other tests train on one era) |
| Size exponent | Score = P x size^b. A regression of held-back error dollars on log P and log size gave b near 1.2 in the review: B's size term is too compressed | All three tests |

**CatBoost at last night's settings**, with the same pieces offered: heavier regularisation (`l2_leaf_reg` 10, `rsm` 0.5), ratio inputs, era handling (weight 3 or the indicator), and its own exponent.

**BART run properly**: prior scale k chosen from 1, 2, 3; 8 chains of 4,000 burn-in and 500 kept sweeps for P (last night: 4 chains, 1,000 and 500, k fixed at 2); its own exponent, which also corrects the size slope the review measured (1.14).

**Blend**: P and size each averaged over one xgboost fit and one CatBoost fit, scored with xgboost's exponent. On the held-back years it is judged against the same average over two xgboost fits, so it gets no credit for averaging alone.

Left out, with reasons:
- The three-class tolerance label. In the training years 96 to 99% of error cases have recreated inputs and no clean case does, so a gain could be the model reading recreation (principal data scientist, checked).
- Strata, direction splits and per-state second stages. Findings 46 tested them and neither reviewer saw why a different learner would change the answer.
- Ranking losses and direct-dollar losses. None was ahead on the held-back years and YetiRank breached the precision guard there.
- State-level pooling and map-weighted training. The statistician tested both on held-back years; neither helped.

## 4. Stage 1: the held-back years decide what goes forward

No test-year label is read in this stage. Each window's last training year is held back, as in last night's tuning: FY2018 for the FY2019 test, FY2019 for the FY2022 test, FY2023 for the FY2024 test.

**Criteria.** P is judged on within state-year log loss, size on gamma deviance, and anything that changes how they combine (the exponent, the blend) on the Poisson deviance of error dollars against the score with one scale per state (`pois_dev` in `s46_lib.R`). These criteria resolve differences of about 0.0005; the held-back share at 10% has a standard error of 0.6 to 1.5 points and cannot rank arms. The shares are printed and never used to choose. This follows the senior statistician and sets aside the principal data scientist's wish to also require a higher held-back share.

**Promotion rule**, applied by code (`05_followup.py promote`), not by hand:

- A piece is promoted when its seed-paired mean difference from its reference is on the good side by more than 2 standard errors, in FY2018 and in FY2023. Five seeds per arm (four chains for BART).
- FY2019 is not used in any promotion decision, because FY2019 is also the first test's scored year. It gives the second test its number of rounds, as it did for B and for every learner last night, and nothing else: no criterion or share is computed or printed for it.
- Era handling exists only in the FY2024 test's training years, so it is decided on FY2023 alone: the best of the forms by mean criterion, then the 2 standard error rule against B's own inputs. If ratio inputs are also promoted, the era form is stacked on them unless the stacked arm's criterion is worse than the ratio inputs alone.
- Ratio inputs and era handling are decided separately for the P model and the size model.
- The exponent b is estimated on FY2018 and on FY2023. It is promoted when the b estimated on one year lowers the dollar deviance on the other year, both ways, by the same rule. The b used on the test years is the precision-weighted mean of the two.
- BART's k is the value with the lowest summed log loss over FY2018 and FY2023. This is tuning, with no bar.

The statistician's morning proposal was "three seed standard deviations". The rule here is 2 standard errors of the seed-paired mean, which is far easier to pass. The reason is the stacked era piece: in the morning's screen, 3x weights on top of ratio inputs moved the log loss by only 0.0001 while adding 1.6 points to the held-back share in all three seeds. A bar of 3 would pass that piece 16 to 53% of the time, a bar of 2 36 to 80% (statistician's pre-run review). Promotion is cheap to get wrong in this direction: a promoted piece with no real effect adds nothing to the primary arm but a little variance, and the test years are still read once.

**Freeze.** `promoted.json` records every decision with its numbers. The chain logs its hash and the hashes of the three scripts before any test-year fit starts.

## 5. Stage 2: the test-year fits

Only what was promoted is fitted on the test years' training rows.

| Arm | Fits | What it is |
|---|---|---|
| `xgb_es` | 10 seeds | B's inputs and settings, rounds by the same early stopping as the revised arm: the twin |
| `xgb_rev` | 10 seeds | Every promoted xgboost piece together: **the primary arm** |
| `xgb_rev_b1`, `xgb_exp` | 10 seeds | The revised fits without the exponent; the exponent alone. Written only when both kinds of piece were promoted. For decomposition, not claims |
| `cb_rev` | 5 seeds | CatBoost with its promoted pieces. Not fitted if nothing was promoted |
| `bart_long` / `bart_rev` | 8 chains | BART run properly; `bart_rev` adds its exponent if promoted |
| `blend` | 5 fits | Only if promoted |

If no xgboost piece is promoted there is no primary arm, and the result is that no structural lead survived the held-back years.

## 6. The one read, and where the bars sit

`07_score_followup.R` runs once. For every arm it prints the three top metrics, each difference from the stored 10-seed B with its paired standard error, the same on FY2024 alone, against the twin (`xgb_es`) and against the primary arm (`xgb_rev`), the sample-pooled share, the share at 10% without the three largest states, the count of states up and down, precision at 5%, and the harmed states.

**Standard errors.** As last night, with one change the statistician asked for: a bag's seed variance is estimated from random splits of its own fits into two half-bags and used as an upper bound for the full bag. Last night's formula understated it.

**Three levels, so that a real gain below what the 10% share can resolve is not thrown away:**

| Reading | What it takes |
|---|---|
| **Ahead of B** | Share at 10% above B's by 1.96 standard errors for the primary arm; for the secondary arms (CatBoost, BART, blend) the Bonferroni bar over however many are fitted: 1.96 for one, 2.24 for two, 2.39 for three. Guards met |
| **Supported, not confirmed** | Share at 10% above B's, guards met, and the mean of the shares at 5%, 7.5% and 10% above B's by 1.645 standard errors (primary), or 1.645 / 1.96 / 2.13 for one, two or three secondary arms |
| **Not distinguishable** | Neither of the above |
| **Behind B** | Share at 10% below B's by 1.96 standard errors |

Guards, the same for both positive readings: not more than 1.645 standard errors below B at 5%; pooled precision at 5% no more than 1.5 points below B's; sample-pooled share at 10% not below B's. Harmed states, the share without the three largest states, and the states up and down are reported beside each reading and weighed by the project lead (findings 53; the ledger's rule that a headline gain contradicted by the within-state mean and the harmed tail does not ship).

**Fallback, named now.** If `xgb_rev` misses a guard and the same fits without the exponent (`xgb_rev_b1`) do not, `xgb_rev_b1` is read at the primary bar and reported as the fallback.

**Why the bars sit here.**

- **One primary arm, tested once, without a multiplicity penalty.** The morning proposal was a bar of 2.39 with three promoted arms. Stacking the promoted xgboost pieces into one named arm asks one question (can B be improved?) and keeps the bar at 1.96. The statistician's pre-run review judged this sound, because the screening used no test-year label.
- **No minimum effect size for changes inside xgboost.** The morning review suggested naming one point as the gain that would change a decision. That fits a second library, which adds a dependency to the state package. A weight, eight derived columns and an exponent add little to maintain, so a confirmed gain of any size is worth taking.
- **The second level rests on a figure with half the noise.** The share at 10% turns on the cases at each state's cut line: its standard error is 0.4 to 0.5 points for an xgboost arm. The mean of the shares at 5%, 7.5% and 10% has a standard error near 0.21 (statistician, from last night's scores). With it, an arm that gains nothing reads "supported" at most 5% of the time, and an arm with a true half point spread over the top tenth reads it 55 to 75% of the time. This mean counts a state's top 5% three times, as Q did five times; it is used only for this level and is not a fourth top metric.
- **Why not the dollar deviance.** The first draft used the Poisson dollar deviance for this level. Both pre-run reviews rejected it: the exponent is fitted to lower that deviance, so an arm carrying the exponent would have read "supported" about one time in three with no gain. The deviance (on P x size, without the exponent) and the log loss of P are still printed, as diagnostics.
- **The 5% guard was loosened from 1 to 1.645 standard errors.** At 1 it would fail 16% of the time when nothing had changed.
- **The promotion bar is 2 standard errors, not 3** (section 4).

**Power, stated in advance.** For the primary arm, with a standard error of 0.45 to 0.5 points at 10%: a true gain of +1.5 points reads ahead about 85 to 90% of the time, +1.0 about 55 to 60%, +0.5 about 17 to 20%. For a secondary arm the figures are lower (standard error 0.55 to 0.75, bar up to 2.39). The "supported" level is there for the gains in the lower rows.

**What is not allowed after the read.** An arm that is not distinguishable is not rerun with more seeds or new settings on these tests. A reading of ahead or supported is a candidate until FY2025 or a state's file confirms it (last night's rule).

**Assignability.** A CatBoost, BART or blend arm that is ahead of B but not ahead of `xgb_rev` is a gain from the structure, not from the learner. The print shows z against `xgb_rev` for that purpose.

## 7. Limits known before the run

- The three tests chose B and have now scored eight learners. Tonight adds one primary and up to three secondary readings.
- The era pieces rest on one held-back year (FY2023), the year that also made B's pooled-years choice (findings 46, E3), and they can move only the FY2024 test: a gain of one point there is 0.4 on the combined metric. The FY2024-alone figures are printed for that reason.
- The exponent is estimated from models fitted on fewer rows than the final fits (41,014 of 80,071 for the first test). If the compression it corrects shrinks with more training rows, the carried b is too large. The cross-year check guards against chance, not against this.
- In the principal data scientist's screen the 3x weight lost 0.95 points at 5% on the map weights while gaining at 10%. The 5% guard is there for that.
- The dollar deviance rewards a better fit across all cases, not only the top tenth. It supports a reading; it does not replace the share.
- **Deployment gap.** The ledger (2026-10-06) makes the three-years-ahead window the primary one, because lists fitted through FY2024 are applied to FY2027 cases. The era pieces can be read only one year ahead (FY2024). A weight also means different things as newer years accumulate: 3x gives the newer era 49% of the weight in the held-back fit, 66% in the FY2024 test fit and 75% in a fit through FY2024. The indicator has no such parameter.
- **Ratio inputs and reconstruction piles.** The ledger records that reconstructed incomes pile at shared values. Kept cases exclude pile rows, but a ratio of two reconstructed amounts could still mark a reconstructed case. A path-robust reading is needed before any ratio-input result is logged as a finding.
- **Ratio inputs in the state package.** The ledger's rule is that a column a state lacks is filled with 0. A zero-filled gross column sets the zero-income flag for every case and blanks two ratios. Untested here.
- **The blend and the exponent.** The blend uses xgboost's exponent on averaged components; CatBoost's own exponent is not used in it.
- **Same-fit comparisons.** `xgb_rev` against `xgb_rev_b1`, and `xgb_exp` against `xgb_es`, share fits; the scorer adds both seed terms, so those component z values are understated.
- BART at 197,060 rows may not have converged at 4,000 sweeps. The held-back runs log the log loss by block of 500 sweeps; the final runs log the spread between chains.

## 8. Compute

Stages run one at a time under caps (minutes): held-back xgboost 40, CatBoost 55, BART 75; promotion 10; test-year xgboost 30, CatBoost 90, BART 170; scoring 120. The worst case is 9.8 hours; the expected run is about 5 to 6. A capped stage is logged and the chain moves on. No test-year fit is made unless the promotion step exits cleanly and writes `promoted.json`. An arm with fewer than two fits in any test is not read.

## 9. Files

`05_followup.py` (Python stages), `06_bart_proper.R`, `07_score_followup.R`, `run_followup.sh`; outputs under `followup/` (`heldback/`, `promoted.json`, `scores/`, `results/`, `logs/`). Last night's files are not modified.


## 10. Pre-run reviews (2026-11-13, evening)

Two fresh reviews read this note and the scripts before the run: `reviews/followup_design_review_statistician.md` (required by `known_constraints.md`) and `reviews/followup_design_review_principal_ds.md` (read the findings ledger first).

**Senior statistician: REVISE, then GO.** Validity holds: no test-year label reaches a fit, a promotion decision, a carried round count or the exponent; FY2019 enters only the second test's rounds. Changes made:

1. "Supported" rested on the dollar deviance, which the exponent lowers by construction. Replaced by the mean share at 5%, 7.5% and 10% (section 6).
2. The CatBoost test cap was too short (45 minutes against 35.5 for the same fits last night). Now 90. Arms with a single fit are not read.
3. Secondary arms are now also compared with `xgb_rev`.
4. Recommended, adopted: promotion bar 3 to 2; 5% guard 1 to 1.645 standard errors.
5. Optional, adopted: test stages are gated on the promotion step's exit status; BART files are written at 17 digits; the second test's held-back year (FY2019) no longer has any criterion computed or printed; the power table uses a standard error of 0.45 to 0.5.

**Principal data scientist: comparable; NOT YET ready, five fixes.** Rows, benchmark file, flag rule, kept cases, state weights and B's stored scores are last night's; `xgb_es` is a fair twin. Changes made:

1. The era form had to beat the ratio arm on log loss, which the morning's screen says it would probably fail while adding to the share. It is now judged against B's inputs and stacked unless the stack is worse (section 4).
2. "Supported": as the statistician's item 1. The deviance is now computed on P x size and is a diagnostic only.
3. Scoring cap raised from 40 to 120 minutes, and the scorer's diagnostics were sped up.
4. z against `xgb_rev` added (statistician's item 3).
5. The BART branch of promotion no longer falls back silently: it falls back only when the held-back stage did not finish, and stops on an unreadable file. The branch now runs in the smoke test.
6. Optional, adopted: the blend is built from averaged components on one scale; the fallback arm is named; the secondary bar follows the number of secondary arms fitted; the share without the three largest states and the states up and down are printed; the ledger's deployment-gap, pile and zero-fill rules are cited in section 7.

Where the reviewers differed: on the "supported" criterion the principal data scientist would have kept the deviance, computed without the exponent; the statistician's replacement was adopted because it measures the top of the list, which is the objective, and has a stated false-positive rate.


## 11. Stop and restart (2026-11-13, night)

The first launch (18:46) finished the xgboost and CatBoost held-back stages and the first test's BART P fits. At about 19:53 Claude Code ended the run because system memory was low. The likely cause was a second BART process (`06b_bart_hb_W3k3.R`) started beside the chain at 19:48 to keep the stage inside its cap; each process holds about 9 GB because every sweep is kept. No promotion and no test-year fit had been made.

Eric authorized a restart with up to 30 GB for BART. Changes at restart, to the run script only:

- `hb_bart` cap raised from 75 to 120 minutes and `test_bart` from 170 to 210, so BART finishes as designed without a side process.
- No process runs beside the chain. `06b_bart_hb_W3k3.R` is not used.

The model, promotion and scoring scripts are unchanged (same hashes as the first launch). The chain skips the held-back files already written. The run script as restarted is `reviewed_followup/run_followup_restart.sh` (sha256 42f6bdb0d87b76a3).

**Second stop (about 23:07) and the change to the BART test stage.** The restart finished the BART held-back stage, the promotion step (`promoted.json`, file sha256 7b2eae4bca3009be) and the xgboost, CatBoost and blend test-year fits. During the first test's BART fit Claude Code again ended the run for low memory; the BART process had peaked at 40.4 GB against the 30 GB allowance. No BART test file was written and nothing was scored.

Cause, measured on synthetic data: `dbarts::bart` runs its burn-in as one stored run and holds about 2 x training rows x sweeps x chains x 8 bytes while it does, whatever `keeptrainfits` says. At 8 chains x 4,000 burn-in sweeps that is 38 GB for the first test (80,071 training rows), 57 GB for the second and 94 GB for the third. The design as written could not run on this machine. The held-back stage's 25.9 GB peak is the same mechanism.

Change (`06_bart_proper.R`, sha256 now 2f27ba95c129eca1, copy in `reviewed_followup/06_bart_proper_restart2.R`): the P model's burn-in is made in pieces of 250 sweeps and each piece is dropped, through `bart`'s own sampler and code path (`bart_chunked`). The model, prior, chains, sweeps and seed are unchanged. Checks: on synthetic data the kept draws equal the one-call draws to 1e-14, with peak memory 459 MB against 2,594 MB; on the smoke data the patched script reproduces the earlier smoke scores (P to 3e-16, size exactly). Expected peak is now about 13 GB on the third test. The stage also logs its peak memory after each P fit. This change was made after promotion was frozen and has not had a fresh review; it does not touch promotion, the other arms or the scorer.
