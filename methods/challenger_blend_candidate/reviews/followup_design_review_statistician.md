# Pre-run review of the follow-up design and code

Senior statistician, fresh context, 2026-11-13 evening. Reviewed: `design_note_followup.md`, `05_followup.py` (as patched 18:25), `06_bart_proper.R` (18:25), `07_score_followup.R` (18:13), `run_followup.sh` (18:14), against `02_fit.py`, `03_fit_bart.R`, `04_score.R`, `s46_lib.R`, the two morning reviews, `known_constraints.md` and the ledger.

## Verdict

**REVISE, three small fixes, then GO.** No test-year label reaches a fit, a promotion decision or a carried setting. The three fixes are in the scorer and the chain, not in the fits. Two further changes are recommended because they would change a reading; they are the lead's call.

What I ran: two read-only scripts in `reviews/scratch_statistician2/`. `02_bars.R` bootstraps last night's stored B, `xgb_ref` and `catboost` on the test years (400 draws) to size the noise of the reading rule. It touches no new arm. `01_cb_size_check.py` checks the smoke CatBoost size scale on training years.

## A. Validity

Checked and sound:

- **Label blanking.** `05_followup.py` lines 14-17 exec the head of `02_fit.py`; `Win` blanks `y`, `d`, `dd` on test rows (02 line 38) after the column hash check. `06_bart_proper.R` lines 12-14 eval the head of `03_fit_bart.R`; `load_win` blanks the same columns (03 line 35). Test rows are used for inputs only (`W.te` in `test_xgb`, `test_cb`, and 06 line 65).
- **Promotion.** `better()` (05 line 200) and `exponent()` (line 221) read FY2018 and FY2023 only (`DECIDE`, line 27). W2's rows enter `promote()` only through `rounds` (line 293 on), for W2's own test fit.
- **Rounds.** Each window's rounds come from its own held-back year and are scaled as `xgb_tuned` was.
- **Exponent.** Estimated on the held-back bags of FY2018 and FY2023, checked across the two years, pooled by precision. No test year enters.
- **Scorer.** The only reader of test labels. Bootstrap seed and draw order are last night's.

Two things to know, neither a leak in the code:

- W2's held-back split (fit FY2017-18, read FY2019) is W1's test configuration. `hb_xgb` logs FY2019 criteria and shares for `base` and `ratio` at five seeds, and `test_cb` logs a FY2019 log loss (05 line 342), before the one read. The code ignores them. Nobody should open the W2 lines before the scorer has run.
- The CatBoost `reg` setting was partly suggested by the morning probe on W2's held-back FY2019 (statistician review, A.2). Promotion uses FY2018 and FY2023 only, so the effect is on the candidate list, not on the decision.

## B. Correctness

Read line by line and found correct: weight normalisation (`era_wt`, mean 1, as the morning screen did); the era indicator at scoring (1 for FY2024 rows); rounds scaling; `qpois_b` (IRLS, robust variance, delta method for the ratio); the cross-year exponent check; `cb_fit_S` against `cb_score` (same baseline handling, same offset; the smoke size deviance of 8.4 was a bug fixed at 18:14 and now reads 0.313); the BART block and chain handling; `pdev_of` and `wll_of` against `pois_dev` and `wll` in `s46_lib.R`; `cmp`; the arm-writing logic in `test_xgb` for all six promotion outcomes; the half-bag seed term. On the last: for `xgb_ref` minus B at 10% the half-bag seed part is 0.21 points against a bootstrap part of 0.34, total 0.40. It is a mild upper bound (about 5% on the standard error), not a doubling.

### Must fix

1. **`07_score_followup.R` line 117 and design note section 6: the "supported" level reads noise as support about one time in three.** The deviance leg is close to automatic for any arm that carries the exponent. The exponent is promoted because it lowers that same deviance, and the miscalibration it corrects is stable (size coefficient 1.18 and 1.22, each about 2.7 standard errors from 1 on one year; smoke cross-year t of -12 and -57). The level then rests on the share being above zero on both weightings with the guards met. For two bags of the same recipe that happens 33% of the time (39% before the 5% guard; the two share differences correlate 0.67). A true +0.5 reads "supported" 78% of the time. 78 against 33 is weak evidence.
   Fix: make the sensitive leg a rank-based top-of-list measure with a stated error rate. Use the mean of the map-weighted shares at 5%, 7.5% and 10% (all three are already in `BUD`). Its paired standard error is about half that of the 10% share: 0.21 against 0.40 for `xgb_ref` minus B, 0.38 against 0.56 for CatBoost minus B.
   - Line 21: add `"m510"` to `SHARES`.
   - Lines 30-34: `num <- c(0, 0, 0)`; accumulate `got[3:5]`; return `dr5 = dr[1], band = dr[3] - dr[1], dr10 = dr[3], m510 = mean(dr)`.
   - Line 117: replace `TOP$pdev_z <= -2` with `TOP$m510_z >= ifelse(TOP$role == "primary", 1.645, 2.13)`.
   - Restore the check in `04_score.R` lines 66-68 (weighted figure against `score_ranking()`'s state table), since `shares_of` is edited.
   - Print the deviance and the log loss as diagnostics, not as legs.
   Result: no gain reads "supported" at most 5% of the time; a +0.5 gain spread over the top tenth, 55 to 75%; a gain confined to the 10% line, 25 to 35%. If the lead will not touch `shares_of` tonight, the fallback is one token: `TOP$dr10_z >= 1` in place of `TOP$dr10_minus_B > 0` (no gain 11%, +0.5 60%).
2. **`run_followup.sh` line 31 and `07_score_followup.R` lines 89-91: a capped CatBoost stage can stop the one read.** Last night the same 15 CatBoost test fits took 35.5 minutes. Tonight adds three early-stopped refits and possibly slower regularised fits, under a 45-minute cap. A cap during W3 leaves `cb_rev` with a short bag. With one fit, `sterm()` calls `stop()` and the scorer dies after `by_window.csv` is written.
   Fix: raise the cap to 90. In line 89 keep in `full` only arms with at least two fits in every window, and name the dropped arms in the existing NOTE.
3. **`07_score_followup.R` line 99: the learner comparison is against the wrong arm.** The note (sections 1 and 6) asks whether CatBoost, BART or the blend beats `xgb_rev`. The code prints z against `xgb_es`, which has no ratio inputs, era handling or exponent. A `cb_rev` that carries those pieces could look ahead of the twin on structure alone.
   Fix: for `cb_rev`, `bart_long`, `bart_rev` and `blend`, also compute and print `cmp(l, "xgb_rev", s)` when `xgb_rev` exists.

### Recommended; each can change a reading (lead decides)

4. **`05_followup.py` line 30: lower `TBAR` from 3 to 2.** Promotion is a screen. A piece promoted in error dilutes the primary arm; it cannot create a false claim, because the test read has its own bar. With five fits the t has 4 degrees of freedom. Chance of passing one year, by true effect in standard errors of the mean:

   | True effect | Bar 3 | Bar 2 |
   |---|---|---|
   | 0 | 0.02 | 0.06 |
   | 1.5 | 0.18 | 0.38 |
   | 3 | 0.56 | 0.82 |
   | 4.5 | 0.88 | 0.98 |

   Ratio inputs on P (0.00037 in FY2018, 4 to 8 standard errors) pass either bar. Era handling on top of ratio inputs measured 0.00013 in log loss this morning (0.30205 to 0.30192), 1.4 to 2.9 standard errors: 16 to 53% at bar 3, 36 to 80% at bar 2. As written, the piece the principal data scientist expected to move a figure most will probably not reach the test years. At bar 2 a two-year piece with no effect passes 0.3% of the time; an era form (one year, best of four) up to about 20%.
5. **`07_score_followup.R` line 112: the 5% guard at -1 standard error fails 16% of the time when nothing changed at 5%.** At -1.645 it fails 5% and still catches a true loss of 0.95 points about 75% of the time. If fix 1 is taken, a loss at 5% already lowers the new measure.

### Optional

- `run_followup.sh` line 28: gate the test stages on the promote stage's exit status, not on the file, so a stale `promoted.json` cannot be used on a rerun. Line 12: `cleanup` stops Python only; a capped `Rscript` may leave its child running.
- `06_bart_proper.R` lines 67-68 write 15 significant digits; `03_fit_bart.R` wrote `%.17g`. No effect on a ranking; match it for consistency with the CSV ruling.
- `05_followup.py` line 359: the blend is a plain mean of scores on different scales when the exponents differ (size^1.23 is about three times size). It is roughly a 3:1 blend. Promotion tests the same formula, so it is consistent, but say so.
- CatBoost pieces are each tested against the base and then stacked untested (lines 256-259). xgboost tests era on top of ratio.
- Promotion log loss is on all held-back rows, about 2.5% of which are not kept (piles, mismatches). The test shares are on kept cases, so the ledger's non-pile reading holds by construction. A kept-rows log loss beside it would cost one line.
- The deviance z uses a normal approximation on a dollar-weighted statistic. If it stays in the print, add the share of bootstrap draws below zero.

## C. The bars

- **Promotion (3 standard errors, FY2018 and FY2023).** Not too low: seed noise passes 2% a year. Too high in one place: the era piece after ratio inputs (item 4). Requiring both years is the real protection and should stay.
- **Ahead (1.96 primary, 2.39 secondary).** Sound. Screening on held-back years used no test label, so it does not inflate the error rate of a single named arm on the test years. No multiplicity penalty is needed for the primary arm. The tests chose B, which works against a challenger, not for it.
- **Supported.** Not defensible as defined for an arm with the exponent (item 1). The dollar deviance is the wrong sensitive criterion for the reading: it rewards calibration over all cases, and CatBoost last night was +0.54 at 10% with a worse deviance than B (+0.05, z +0.6). The test-year log loss of P has the same limit. The mean share over 5%, 7.5% and 10% is rank-based, sits on the three top metrics, and halves the standard error.
- **Power in the note.** The 0.4 standard error is the floor for two bags of one recipe. With new inputs and an exponent expect 0.45 to 0.5 (LightGBM read 0.50). At 0.47, +1.0 reads ahead 57% of the time, not 72%; +0.5, 15%.
- The note names FY2025 as the confirmation. That file does not exist yet, so a "supported" reading has no scheduled confirmation. That is a further reason to give it a stated error rate.

## D. Comparability

- B is the stored 10-seed bag; pooled shares are asserted against `score_ranking()`; `xgb_es` uses B's design and column order, differing in rounds only; `cb_rev`'s size model is last night's.
- Standard errors differ from last night's by design (half-bag seed term): `xgb_ref` 0.40 against 0.35, CatBoost 0.56 against 0.54. Last night's arms will show slightly smaller z tonight. Say so beside the table.
- The era weight is chosen where post-pandemic rows are 24% of the fit rows and applied where they are 39%. Add this to section 7.

## Not checked

- The BART promotion path failed in the 18:25 smoke on an assertion from a smoke-only row split, patched at 18:25 (`load`, 05 lines 43-44). The 18:29 re-smoke ran it: k chosen, exponent line printed. I read that log; I did not re-read the test-stage smoke. If the path fails in the real run, the code falls back to k 2 and no exponent and says so.
- dbarts memory in the held-back stage (4 chains x 2,000 kept sweeps x about 40,000 rows is about 2.6 GB) and whether a capped `Rscript` leaves a child process.
- I did not run any fit.
