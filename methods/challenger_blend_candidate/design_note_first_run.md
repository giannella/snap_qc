# Challenger benchmark: other learners against the per-case model (B)

Written 2026-11-12, before any challenger was scored on a test year. Requested by Eric ("examine what it would take to benchmark them against our current xgboosted model"; then "you can keep running things all night until 6 am ... skip anything that genuinely requires a GPU").

This folder sits outside the repositories. It reads the staging checkout (`C:/Users/ericg/snap_qc_v27`) and writes nothing to it. Nothing here is a finding until the project lead logs it.

## 1. The question

Does any of six other learners rank cases by expected error dollars better than model B's boosted trees (xgboost), on B's three held-out tests, with everything but the learner held as B had it?

## 2. What varies

One component: the learner that fits P (error over tolerance) and the learner that fits size (error dollars given an error). Held as B had them (findings 46, 48, 51; `methods/case_predictions/s46/`, outputs in `out_f1109_eval`):

- Frame f1109 (sha256 08e0e825), the evaluation benchmark `additional_data/eval_benchmark_f1109.rds`, and `score_ranking()`.
- The three tests: FY2017-18 scored on FY2019 (W1); FY2017-19 on FY2022 (W2); FY2017-19 + FY2022-23 on FY2024 (W3).
- B's frozen per-test choices: the 18 inputs, plus the three within-state percentile inputs in W1 only; the FY2017-19 rows in W3's training set.
- The score: P x size. Size is fitted on training error rows to error dollars deflated by year.
- Training on all rows, including the cases set aside at scoring.
- Tuning: settings chosen on the last training year held back, by B's criteria (within state-year log loss for P, gamma deviance for size), then refit on all training years. The test year's labels are read only by `04_score.R`.

The learners, each run the way its own documentation recommends:

| Learner | P | Size | State | Settings chosen on the held-back year | Fits averaged |
|---|---|---|---|---|---|
| xgb_ref (harness check, not a challenger) | xgboost, B's frozen settings | xgboost gamma, B's frozen settings | 49 indicators | none | 10 seeds |
| xgb_tuned (learner-only reference, not a challenger) | xgboost | xgboost gamma, lben offset on or off | 49 indicators | B's own grid, re-tuned on this frame: depth 3/4/5/6 x min child weight 5/25 x column sample 0.5/0.8 (P); depth 2/3/4 x min child weight 5/25 x offset (size) | 10 seeds |
| lgbm | LightGBM binary | LightGBM gamma, lben offset on or off | native categorical | leaves 8/16/32/64 x min hessian 5/25 x column sample 0.5/0.8 (P); leaves 4/8/16 x min hessian 5/25 x offset (size) | 10 seeds |
| catboost | CatBoost log loss, learning rate 0.06 (B's is 0.03) | CatBoost Tweedie, variance power 1.9 (it has no gamma loss), offset on or off | target statistics or indicators | depth 4/6/8 x state encoding (P); depth 4/6 x encoding x offset (size) | 5 seeds |
| ebm_main | Explainable Boosting Machine, main effects only | EBM gamma deviance, main effects only | nominal term | offset on or off | 3 seeds, each a bag of 14 |
| ebm_pairs | EBM with its default pair terms | EBM gamma deviance with pair terms | nominal term | offset on or off | 3 seeds, each a bag of 14 |
| bart | dbarts probit BART, 200 trees, quantile cut points | BART on log error dollars, lognormal mean per draw | 49 indicators | offset on or off | 4 chains, 1,000 burn-in + 500 kept sweeps each; each chain is scored as one fit |
| tabnet | TabNet classifier (pytorch-tabnet, CPU); inputs passed through sign(x) log(1 + abs(x)) | TabNet regressor on centred log error dollars, one rescaling factor | learned embedding | width 8 or 24 (P); offset on or off; epochs | 3 seeds |

**A second thing varies between B and the challengers: the frame the settings were tuned on.** B's shipped settings were tuned on frame f1030 and carried to f1109 unchanged (findings 48 and 51; `s46_f1109.R`). Every challenger is tuned on f1109's own held-back year. Findings 48 records the direction: carried-over settings can only disadvantage the later frame. That tilts toward the challengers by an unmeasured amount. The `xgb_tuned` arm removes it: xgboost on B's own grid, tuned on f1109 through the same path as the challengers. A challenger's reading against `xgb_tuned` is the learner-only comparison.

Things that differ with the learner and cannot be held fixed:

- **Fits averaged.** B averages 10 seeds. Slower learners get fewer. `04_score.R` therefore also scores B from its first 5, first 3 and first 1 stored seeds, and reports each challenger against the B bag of matching size as well as against the shipped 10-seed B.
- **Blank inputs.** xgboost, LightGBM, CatBoost and EBM take blanks as blanks. BART and TabNet get 0. There are 81, 123 and 210 blank cells in the three windows.
- **Size loss.** Gamma where the library has it (xgboost, LightGBM, EBM). CatBoost: Tweedie 1.9. BART and TabNet: log dollars.
- **EBM** keeps its own internal validation split and bagging, so no round count is carried from the tuning fit.
- **Grid size.** B was tuned over 16 (P) and 12 (size) settings. LightGBM gets the same count; CatBoost 6 and 8; EBM and BART tune one switch; TabNet two widths and one switch. A learner that reads behind is behind at these settings.
- **Library versions.** xgb_ref runs xgboost 3.4.1 under Python; B was fitted with xgboost 3.1.2.1 under R. CatBoost's boosting type is its default at this size (Plain).
- **Data path.** B read the frame's rds. The Python learners read a CSV written at 17 significant digits and assert each column's sha256 against the export; BART reads an rds copy.

Not run: TabPFN and Synthefy Nori. Both need a GPU at these training sizes (TabPFN refuses more than 5,000 rows on CPU; Nori's attention cost grows with the square of the context rows, and its published runs used H100 GPUs). TabPFN's current weights also carry a non-commercial license and need a login.

## 3. Support

From `data/support.csv`, written by `01_export.R`:

| Test | Training rows | Training errors | Tuning fit rows / errors | Held-back rows / errors | Test rows / errors / kept | Fewest training errors in a state (median) |
|---|---|---|---|---|---|---|
| W1 (FY2019) | 80,071 | 7,122 | 41,014 / 3,512 | 39,057 / 3,610 | 39,239 / 3,926 / 38,302 | 30 (148) |
| W2 (FY2022) | 119,310 | 11,048 | 80,071 / 7,122 | 39,239 / 3,926 | 37,680 / 4,022 / 36,788 | 44 (224) |
| W3 (FY2024) | 197,060 | 19,525 | 156,990 / 15,070 | 40,070 / 4,455 | 40,497 / 4,820 / 39,471 | 76 (392) |

No learner is fitted per state, so no split leaves a state pool as its unit. The size models train on the error rows (3,512 at the smallest tuning fit). After the tuning split, the fewest errors in a state (median) are: tuning fit 16 (73) in W1, 30 (148) in W2, 61 (313) in W3; held-back year 14 (75), 13 (76), 10 (91). These matter only to the state term each learner fits.

## 4. What the record already says

- Findings 46: B was chosen from eight candidates built from xgboost and ranger on these same three tests. The spread was small: Q ran 3.39 (A) to 3.51 (GC), and B's margin over A was 0.113 with a paired standard error of 0.048. The ranger stacks (C, GC, F) did not clear B under the selection rule.
- Findings 51: B on f1109 catches 18.3% of error dollars at 5% and 30.7% at 10% on FY2024 (16.6% / 29.7% on FY2022; 17.8% / 29.5% on FY2019).
- No ledger row covers LightGBM, CatBoost, EBM, BART, TabNet or a tabular foundation model (`grep` of `findings_ledger.md`, `modeling_findings.md`, `known_constraints.md`, 2026-11-12).
- The three test years chose B from eight candidates, so B's figures carry selection optimism that a challenger's do not. That tilts the comparison toward B. Tuning on f1109 tilts the other way (above); `xgb_tuned` measures that part.
- There is no FY2025 case file, so no unread public year exists to confirm an "ahead" on. As in findings 46, the per-test L values are written so leave-one-test-out gains can be quoted.
- Ledger hazard, CSV consumers ("must parse round-trip or read the rds"): met by the 17-digit export and the column hashes.

## The reading, set before scoring

**Superseded in part on 2026-11-12, before any challenger was scored: see "Decision statistic changed before scoring" at the end. The top metrics are now the share of error dollars caught at 5%, the share added between 5% and 10%, and the share caught at 10%. L and Q below are companions only.**

- **Decision statistic** (findings 46): L, the mean over the 2%, 3%, 5%, 7.5% and 10% workloads of the mean state's error dollars per flag, over the mean error dollars per kept case; and Q = 0.2 L(W1) + 0.4 L(W2) + 0.4 L(W3).
- **Standard error**: the paired bootstrap of findings 46 (cases resampled within state, 2,000 draws, the same draws for B and the challenger), plus the seed variance of each bag.
- **A challenger is ahead of B** when Q(challenger) - Q(B) is at least 2 standard errors above 0, against the 10-seed B, the B bag of matching size, and `xgb_tuned`; and its pooled precision at 5% (weights 0.2 / 0.4 / 0.4) is no more than 1.5 points below B's, the findings 46 guard. **Behind (at the stated settings)** when it is 2 standard errors below against both. Otherwise **not distinguishable**. Six challengers are read one-sided, so the family threshold (Bonferroni at 0.025) is 2.64 standard errors; an "ahead" between 2 and 2.64 is weak evidence.
- **Seed term.** A bag's seed variance is the variance of its single fits' L over the number of fits. For B's smaller bags the single-fit variance comes from all ten stored seeds. BART's four chains are its fits.
- **Reported beside it**, for every learner, test and the 5% and 10% workloads: share of error dollars caught (the objective), precision (the guard), median-state times random, the within-state median and mean change from B in times random, and the harmed tail: the count of states whose share of error dollars caught falls by more than 0.05 against B (the findings 53 definition). The median will read 0 in many cells, because a median state flags about 42 cases at 5% and many states tie; the mean and the ahead and behind counts carry the state reading. Each challenger is also shown against xgb_ref, because per-test gaps between xgb_ref and B (up to 0.075 in L, 0.4 points of error dollars) mark what this harness cannot resolve.
- **xgb_ref** must land within B's seed noise (Q within 2 standard errors of B). If it does not, the export or the harness is wrong and no challenger figure is read until it is fixed. Its rank correlation with B's stored scores is reported beside it.
- A crash, misaligned rows, or a time cap is an engineering fault: it is fixed or reported, and the rerun is noted. A learner's settings are not changed after its test-year score has been seen.
- This benchmark does not choose a model. A learner that reads "ahead" is a candidate for a pre-registered confirmation, not a replacement.

## Compute

One job at a time on this laptop (Ryzen 7 7840U, 8 cores, 60 GB, no usable GPU), each under a time cap. Measured on the W3 training rows before the run: xgboost 18 s per seed; LightGBM 8 s; CatBoost 2 s per 150 rounds with indicators and 24 s with target statistics; EBM 82 s (main effects) and 155 s (pairs) per P fit; BART 0.2 s per sweep at 200 trees; TabNet 10 s per epoch.

## Review before the run

A fresh-context senior-statistician review (the routing rule in `known_constraints.md`) read this note and the four scripts on 2026-11-12, before any challenger had a test-year score. Verdict: revise. All five required changes and the optional ones were made before the run:

1. LightGBM's size model was passed an offset of zeros when the offset was off, which switched off its starting average and made those six settings unusable. It now gets no offset in that arm. Every grid row's criterion and best round are saved, and a fit that stops at round 1 is an error.
2. The export wrote 15 significant digits. It now writes 17, the Python reader parses round-trip and checks a sha256 of every column, and BART reads an rds copy.
3. The "ahead / behind" reading in `04_score.R` now requires 2 standard errors against both the 10-seed B and the matched bag, as stated above.
4. BART's four chains are written and scored as four fits, so its seed term is no longer zero. B's smaller bags take their seed variance from all ten seeds.
5. TabNet's inputs pass through a signed log, and its size target is centred when the offset is off.

Also made: test-year labels are blanked when the data are loaded; each score file carries the state and is checked against the benchmark's rows; CatBoost's no-offset arm starts from the log mean; the grid sizes, library versions and data path are disclosed above.

A first xgb_ref run (21:19 to 21:25) on the 15-digit export preceded the review. Its files are in `superseded_before_review/`; the scripts as reviewed are in `reviewed_v1/`. xgb_ref was rerun on the 17-digit export. No challenger had been scored.

## Second review before the run

At the project lead's request, a fresh-context principal-data-scientist review read the findings ledger first and then this design, with one question: will these runs be comparable to what the project already publishes for B? Verdict: not yet comparable, two fixes. Both were made before any challenger was fitted on full data:

1. The harmed-tail count was on the times-random scale (a drop of 0.05). The project's definition (findings 53) is a state whose share of error dollars caught falls by more than 0.05. `04_score.R` now uses that.
2. B's settings were tuned on f1030 and the challengers' on f1109. The `xgb_tuned` arm was added, and "ahead" must now clear it as well as B.

The review confirmed the rest as like-for-like: rows, labels, inputs and the kept flag come from the frozen s46 code and are asserted case by case against `eval_benchmark_f1109.rds`; B's row reproduces the published figures of findings 51; flag counts, L, Q, the weights and the bootstrap are the s46 code; every challenger tunes on the held-back last training year only.

## Decision statistic changed before scoring

Decided by the project lead on 2026-11-12, while the chain was running and before any challenger's test-year score had been read (only the two xgboost reference arms had been scored). Eric: "three top metrics: dollar recall at 5, additive dollar recall between 5-10, dollar recall at 10"; on Q, "reporting Q is fine but I agree it makes little sense and would prefer to drop it in the long run."

**Why Q was set aside.** L averages error dollars per flag over five workloads (2%, 3%, 5%, 7.5%, 10%), and those five lists are nested. A case in a state's top 2% is counted in all five, and each list is divided by its own length. The weight on an error dollar, relative to one found between 7.5% and 10%, is 12.7 in the top 2%, 7.7 between 2% and 3%, 4.3 between 3% and 5%, and 2.3 between 5% and 7.5%. Half of L's weight sits on the top 2% of a state's cases, about 16 cases in a median state. L also weights states equally, where the map's figures pool cases.

**The three top metrics**, each pooled over the 49 states on kept cases with `score_ranking()`'s flag rule, and combined over the three tests with the weights 0.2 / 0.4 / 0.4:

- share of error dollars caught at each state's top 5% of cases;
- share added by the cases between 5% and 10% (the 10% share minus the 5% share), so the 10% figure is not the 5% figure counted again;
- share caught at each state's top 10%.

**Standard errors.** As before: the paired bootstrap (cases resampled within state, 2,000 draws, the same draws for every learner) plus each bag's seed term, now computed for each of the three metrics.

**The reading.**

- Per metric, a challenger is ahead when it is at least 2 standard errors above both the 10-seed B and the B bag of matching size; behind when 2 below both; otherwise the same.
- The overall call rests on the 10% share. "Ahead of B" also needs: 2 standard errors above `xgb_tuned` at 10%; pooled precision at 5% no more than 1.5 points below B's (27.4%, so a floor of 25.9%); and not behind B at 5%.
- The harness gate is now on these metrics: `xgb_ref` must be within 2 standard errors of B at 5% and at 10%.
- Six challengers and three metrics are read, so a single reading at 2 standard errors is weak evidence.

Everything else in "The reading" stands: the companions (precision, median state, the within-state mean and median change, the harmed tail), the engineering-fault rule, and that this benchmark does not choose a model.

**Reference arms, read at the time of this change.** Against the 10-seed B (17.53% at 5%, 12.53% between 5% and 10%, 30.07% at 10%): `xgb_ref` +0.09, -0.08, +0.01 points; `xgb_tuned` -0.04, +0.02, -0.02 points. Neither is distinguishable from B, so re-tuning xgboost on f1109 moved nothing measurable, and the standard error of a difference at 10% is about 0.3 points.

## State weighting changed before scoring

Decided by the project lead on 2026-11-12, about 22:15, before any challenger's test-year score had been read. Eric: "States should be treated the way they are for the map's national figure - they should contribute based on their actual cases and not be reweighted for balance."

**What the map does.** Its nationwide dollar line (`pooledDollarLift` in `snap-qc-results-map/map.js`) weights each state's share of error dollars caught by the state's official error dollars: the FNS payment error rate times issuance. California carries about 13% of the weight, New York 7 to 10%. The District of Columbia has no weight in the map's table, so the line covers 48 states.

**What the scoring did before.** It pooled the QC sample cases over 49 states (`summarise_ranking()`, the model card's basis). QC samples are of similar size in every state, so that counts states by sample size, close to equally.

**What the scoring does now.** The three top metrics are national figures on the map's weighting: each state's share of error dollars caught at 5%, between 5% and 10%, and at 10%, weighted by the state's official error dollars, 48 states.

- The weight for a test is the FNS payment error rate of the scored year times FY2024 issuance. FY2024 issuance is the only issuance year on file (`additional_data/fns_state_issuance_fy2024.csv`). For the FY2024 test the weights are the map's table exactly (largest gap 0 dollars).
- Check: B on FY2024 scores 18.52% at 5% and 29.84% at 10%, which is the map's 3.7x and 3.0x for the full model.
- The sample-pooled shares are still printed, as a companion, for the link to the model card (B: 18.3% and 30.7% on FY2024). Precision at 5% pools flag counts over the 49 states, as the map's precision line does.
- The standard error is the same paired bootstrap and seed term. It is larger on this weighting, about 0.3 points at 5%, 0.35 at 10% and 0.5 for the band, because a few large states carry much of the weight.

**Reference arms on the new weighting**, against the 10-seed B (18.12% at 5%, 11.39% between 5% and 10%, 29.51% at 10%): `xgb_ref` +0.15, -0.20, -0.05 points; `xgb_tuned` -0.00, -0.26, -0.27 points. Neither is distinguishable from B; the harness gate passes (z = +0.49 at 5%, -0.13 at 10%).
