# Pre-run review of the follow-up design: comparability, code, and the bars

Principal data scientist, fresh context, 2026-11-13, before any new arm is fitted on full data. Written for the project lead.

Read, in order: `findings_ledger.md` (whole), `known_constraints.md`, `design_note_followup.md`, `design_note.md`, `README.md`, the two morning reviews, then `05_followup.py` (as patched at 18:25), `06_bart_proper.R` (18:25), `07_score_followup.R` (18:13), `run_followup.sh`, `02_fit.py`, `03_fit_bart.R`, `04_score.R`, `s46_lib.R`, `s46_f1109.R`, and the smoke logs under `smoke/followup/logs/`.

Each claim is marked **checked** (I ran it or read the evidence) or **judged**. I fitted no model. Two small read-only scripts are in `reviews/scratch_principal_ds2/`; they use test-year labels only for arms scored last night (`xgb_ref`, `xgb_tuned`, `catboost`, `bart`). No new arm was read.

## Verdict

**The figures will be comparable. The run is NOT YET ready: five must-fix items, all small.** Two of them bear on the project lead's instruction about the bars. None requires a new fit before launch.

## Must-fix

**1. The era piece is gated on log loss on top of the ratio inputs, where the morning's own numbers show no log-loss gain. `05_followup.py` lines 243-248; note section 4, second bullet.**

- What the code does: if ratio inputs are promoted, the era forms are tested as `ratio_w2 / ratio_w3 / ratio_w5 / ratio_era` against `ratio`, at 3 standard errors on the P log loss.
- What the morning's log shows (`reviews/scratch_principal_ds/j_recency.log`, held-back FY2023, three seeds, checked): ratio against base, seed by seed: -0.00074, -0.00124, -0.00069. Ratio with 3x weights against ratio: -0.00023, +0.00014, -0.00016, mean -0.00008, t about -0.7. The 3x weight against base: -0.00049, -0.00045, -0.00027, t about -6.
- On dollars the same fits read the other way. Ratio with 3x weights against ratio: +1.61 points at 10% on the map weights and +1.70 pooled; all three stacked fits are above all three ratio fits on both.
- Consequence (judged): ratio is promoted, the weight fails against it, and the morning's largest dollar lead never reaches FY2024. The note's own argument for one stacked arm ("the pieces add: +2.77 where 3x alone read +1.80") is a dollar reading; on the note's promotion criterion the pieces do not add.
- Fix: decide the era form on B's inputs (best of `w2, w3, w5, era` against `base`, 3 standard errors, as now), then stack it if the stacked arm's mean criterion is not worse than the ratio arm's. No second 3-standard-error bar for stacking. If the author prefers not to change the rule, the smaller fix is to write the stacked-with-era fits as a component arm on FY2024 whenever the form passes against base, so the project lead sees the reading.

**2. "Supported, not confirmed" is close to automatic for any arm that carries the exponent. `07_score_followup.R` lines 114-118; note section 6 table.**

- The exponent is the coefficient that minimises the Poisson dollar deviance. It was 1.22 (se 0.08) and 1.26 (se 0.12) on the two held-back years. If that carries, the test-year deviance falls by 2 standard errors whether or not the top 10% changes (judged). On the held-back years b = 1.25 moved the 10% share +0.65 and -0.14 (statistician's review).
- The deviance and the share measure different things (checked, `a_pdev_instrument.R`): last night's BART is worse than B on the test-year deviance by z = +7.3 while its 10% share read +0.46.
- So for `xgb_rev` with the exponent, the tier reduces to "share above B on both bases and guards met". With no true gain that happens roughly a quarter to a third of the time (judged). The printed wording, "fits error dollars better on the test years", would then describe the exponent's calibration, not the ranking.
- Fix: for the reading, compute the deviance on p x size (exponent removed) whenever the score file carries p and size. The exponent's own evidence is the same-fit pair (`xgb_rev` against `xgb_rev_b1`, or `xgb_exp` against `xgb_es`); see optional item 1.
- With that fix the tier is sound for the fitted pieces (checked): against the stored B, `xgb_ref` and `xgb_tuned` read z = -0.9 and +0.2 on the deviance, so the instrument is not biased by the refit. The paired standard error of the P log loss between two xgboost bags on FY2024 is 0.00008, so a gain the size the held-back years showed (0.0004 to 0.0009) would read z of 5 or more.

**3. The scoring stage will probably hit its cap. `run_followup.sh` line 34 (cap 40 minutes).**

- Measured on FY2024 (`b_timing_and_wll.R`, machine shared with the smoke tests): one call costs 0.0065 s for the shares, 0.0085 s for the deviance and 0.049 s for the log loss. 2,000 draws by three tests is about 48 minutes with 9 arms and 74 with 13. Last night's scorer had neither the deviance nor the log loss.
- A capped scorer writes nothing, and the chain ends with every fit done and no table.
- Fix: cap 150. Or index the state shift by integer in `wll_of` (lines 40-42 use a character lookup inside the loop) and time it again.

**4. The note says assignability is read against `xgb_rev`; the code prints z against `xgb_es`. `07_score_followup.R` lines 96 and 99-102; note section 6, "Assignability".**

- For `cb_rev`, `bart_long`/`bart_rev` and `blend`, the question "learner or structure" needs the difference from `xgb_rev` with its standard error. It is not computed.
- Fix: add that comparison for the three secondary arms. Otherwise a second scoring run is needed, against the note's "one read".

**5. The BART branch of the promotion step has not run in smoke and falls back silently. `05_followup.py` lines 276-289.**

- Smoke log: "BART held-back files not usable (AssertionError)", then k 2 and no exponent. Cause (checked by reading): the smoke-only split for a window with one training year takes rows 0, 3, 6 in `02_fit.py` line 50 and rows 2, 5, 8 in `03_fit_bart.R` line 40. The real run does not use that split.
- The k choice and BART's exponent are therefore untested code, and any error tonight gives a BART at k 2 with one line in a log.
- Fix: outside smoke, raise the error. Promotion then stops and no test-year fit is made, which is the chain's rule already.

## A. Comparability

Comparable (checked unless marked):

- **Rows, file, flag rule, kept cases, weights.** `07_score_followup.R` reads the same benchmark file (hash asserted), B's stored 10-seed scores, the same state weights, the same bootstrap seed, and asserts its pooled shares against `summarise_ranking()`. Its share code is `04_score.R`'s with L removed. B's row will read 18.12 / 11.39 / 29.51 again.
- **Standard errors differ from last night's table** by the half-bag seed term, so z for last night's arms will be slightly smaller than in the README. The scorer reprints them. Using the half-bag as an upper bound overstates the standard error by about 3% (judged from the statistician's seed law).
- **The twin is fair.** `xgb_es` and `xgb_rev` share B's settings, the seeds 117-126, and the rounds rule (early stopping at seed 117 on the held-back year, scaled by rows). In a window with no promoted piece they are the same fits.
- **B's settings in the revised arms** can only handicap them: nothing is re-tuned for 26 inputs or for weights. Last night's re-tuning moved nothing measurable (-0.27, z -0.7), so the handicap is small (judged).
- **Ratio inputs** are arithmetic on the permitted inputs. No new field enters.
- **Precision guard, harmed states, within-state mean** are all printed for every arm. The exponent is not checked for precision on the held-back years; the guard acts only at the read (optional item 3).

Limits to state in the note:

- **Era pieces and deployment.** The ledger's deployment-gap rule (2026-10-06) makes the three-years-ahead window primary. The era pieces can be read only one year ahead (FY2024). A weight k also means different things as the newer years accumulate: 3x gives the newer era 49% of the weight in the held-back fit, 66% in the FY2024 test fit and 75% in a fit through FY2024 (checked arithmetic). The indicator has no such parameter.
- **Ratio inputs and the state package.** The ledger's rule is that a column a state lacks is filled with 0. A zero-filled gross column makes `r_zero_income` 1 for every case and leaves the two ratios over gross blank. Untested.

## B. Code against the note

Beyond the must-fix items:

- **Blend** (`05_followup.py` lines 267-270 and 359): the two scores are averaged as written, each with its own exponent. If xgboost's exponent is promoted and CatBoost's is not, the xgboost score is on a larger scale and the blend is mostly xgboost. Divide each score by its mean first, or blend at a common exponent.
- **CatBoost stacked arm** (lines 339-341): fitted on the held-back year once, for rounds only. Its log loss is logged and not compared with anything.
- **BART's k** is chosen at 1,500 burn-in sweeps and 4 chains; the test fit uses 4,000 and 8.
- Arms per window, weight normalisation (mean 1), the indicator at scoring (1 for FY2024), separate P and size decisions, the exponent's cross-year check and the `xgb_rev` / `xgb_rev_b1` / `xgb_exp` logic all match the note. The smoke run exercised the exponent-only case correctly.

## C. The bars

- **A true +1.0 point** at 10% reads "ahead" about 70% of the time for the primary arm; **+0.5** about 24%. Those figures in the note are right.
- **What reaches the project lead below that** depends on the two fixes above. As written, the era piece probably stops at promotion, and "supported" does not separate a real ranking gain from the exponent's calibration.
- **With the fixes** the category is useful and matches how the project has decided before: vote scoring shipped on an interval above zero after missing its bar (findings 43-44). The ledger's formality rule (2026-08-11) says feature-set membership is measured and decided, not bound to adoption bars. Print the share gain with its interval in the "supported" line.
- **Large states.** The only formal guard is the pooled share not below B's. Last night's CatBoost missed it by 0.03 points. `by_state.csv` already holds what is needed: say in the note now that the difference without the three largest-weight states and the count of states up and down will be read beside each positive reading.
- **Excluded and should stay out:** the three-class label, ranking losses, strata. **Nothing else promising is excluded** once item 1 is fixed.

## Optional

1. `07_score_followup.R` line 95 adds both seed terms even when two arms are the same fits (`xgb_exp` and `xgb_es`). Their z is understated. Print `xgb_rev` against `xgb_rev_b1` with the bootstrap term only.
2. Blend scale (section B).
3. Name the fallback now: if `xgb_rev` fails a guard that `xgb_rev_b1` meets, which one is read.
4. Set the secondary bar by the number of secondary arms in `promoted.json` (2.24 for two), fixed before the read.
5. Cite in the note: the pile hazard for ratio inputs (kept cases exclude pile rows; give a path-robust reading before anything is logged), the deployment-gap rule, and the zero-fill rule.
