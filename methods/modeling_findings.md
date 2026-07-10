# SNAP QC rule mining — key findings & syntheses (2026-07-04/05 runs)

Working notes for the presentation. Each section lists the supporting artifact
files. Methods details live in `methods/design_drop_pre_architecture.md`; all numbers
below are hold-out (train 2022+2024, test 2023) unless noted.

---

## 1. The winner's curse, diagnosed and addressed

Thresholding thousands of mined rules on raw train precision selects for lucky
rules: a "train precision >= 0.20" shortlist held only ~0.10 median on the
hold-out. Diagnosis showed this is almost pure selection noise, not model
overfit or year drift:

- among high-support rules with NO selection applied, train precision is
  essentially unbiased for hold-out precision (median gap -0.003, r = 0.83);
- the decay is symmetric (rules selected on HOLD-OUT >= 0.20 have median TRAIN
  precision 0.116) — textbook regression to the mean;
- era check: the same rules give ~3.9x lift on 2018-19 vs ~3.5x on 2023 —
  drift is secondary.

**Fix: threshold on the one-sided Wilson LOWER CONFIDENCE BOUND (LCB) of train precision**
instead of the point estimate. At matched deployed precision (~0.20), LCB
selection catches 12.8% of all errors vs 8.2% for absolute thresholds — strictly
better ranking, and trained precision became roughly calibrated to test precision.

*Artifacts: compare_models_by_HHsize_vs_pooled/ (rawstat_ vs unprefixed runs).*

**Calibration of floor definitions (2026-07-07, rebuilt frame, unearned rules):**
sweeping floors on raw trained precision overpromises even AFTER the LCB gate
removes the junk — among rules passing the 99% bound at 0.20, a raw floor of
0.40 delivers 0.33 union precision and a raw floor of 0.50 delivers 0.34 (the
highest raw values among survivors still belong disproportionately to
luck-inflated estimates). Floors on the LCB itself underpromise: an LCB floor
of 0.30 delivers 0.381, 0.40 delivers 0.509. The LCB is therefore the only
menu axis whose number reads "at least this". Side-by-side figure:
`presentation_figures/floor_definitions_educational.png`
(`methods/educational_floor_definitions_figure.R`).

## 2. Results of dropping {pre} r package in favor of rolling our own

Same rule quality, ~5x cheaper, ~3x smaller memory, and it unlocked analyses
pre() could not run:

- **Memory**: pre() peaked >40 GB on ONE frame (its internal lasso matrix —
  paid even when the lasso output is unused). v2 runs in a few GB; works on a
  16 GB laptop.
- **Compute**: one pre() frame ~40 min vs four v2 frames (incl. any-error
  scoring) in ~45 min.
- **Quality**: matched earned-frame comparison under identical LCB selection —
  pre: 68 rules, median hold-out 0.134; v2: 29 rules, 0.157 — parity at 1/5
  the trees.
- **Unlocked**: the any-error single model (pre's lasso matrix would be
  ~100+ GB), the other_error frame, 853k-rule head-to-heads, coverage-based +
  dominance dedup, checkpointed vocabularies, a 15-check regression test.
- What it did NOT buy: more signal. Best honest per-rule hold-out precision by
  frame: earned 0.31, underissuance 0.29, other_error 0.40, unearned 0.48.
- **Engine head-to-head (2026-07-05/06, identical pipeline, 1000 trees/rounds
  each, any-error frame)**: xgboost + ranger is the best pair — mean precision
  0.2216 at matched dollar recall and 54.8% dollar recall at the 0.20 floor,
  vs rpart + ranger 0.2157 / 53.1% and bagged rpart alone (pre's generator)
  0.2096 / 47.3% (reach capped at 94%). Both pairs beat all singles —
  vocabulary complementarity again. So pre's CART engine was competitive but
  not its pipeline's problem; the engines add ~+1pp precision / +7pp dollar
  recall, while stringent filtering and any-error scoring supply the larger gains.
  *Artifacts: methods/compare_engines_v2/ (engine_ and combo_ sweeps + summaries).*

*Artifacts: rule_mining_helpers.R, test_rule_mining_helpers.R,
methods/design_drop_pre_architecture.md.*

## 3. Typed frames vs one any-error model (head-to-head)

Scored on ALL 2023 errors with identical machinery and selection:

- **Typed mining wins, but barely** (+0.5-1pp mean precision at matched
  recall). One all-errors model gets ~95% of the way at 1/4 the mining cost.
- **The vocabularies complement**: Combined beats both parents on recall at
  every FIXED filter floor (only ~7% cross-pool overlap). Best practice: mine
  both, pool, dedup — cheap on the v2 stack. Hold-out recall of all errors,
  typed-only vs combined, in both ensemble regimes:

  | floor | typed -> combined (300/500 trees) | typed -> combined (1000/2500) |
  |---|---|---|
  | 0.20 | 62.7% -> 66.0% | 68.8% -> 72.9% (dollars 71.2% -> 75.3%) |
  | 0.30 | 22.0% -> 27.6% | 29.7% -> 36.0% |
  | 0.35 | 10.9% -> 15.4% | 16.5% -> 21.5% |

  Precision cost at the same floors: ~0.7-2pp. HONEST CAVEAT: at MATCHED
  RECALL combined runs ~0.5-1pp below typed-only in both runs (at/near the
  noise band) — so "combine" wins for a state operating at a fixed filter
  floor (the standard workflow) and roughly ties for a state targeting a
  precision level. The floor-level recall gain is near-guaranteed mechanically
  (adding a vocabulary can only grow the union); the measured question was the
  precision price, which is small. Evidence grade: SOLID as of 2026-07-06 —
  the year-swap replication (train 2022+2023, test 2024) reproduced both the
  ordering and the magnitudes on a disjoint test year: at the 0.30 floor,
  typed 26.9% -> combined 35.6% recall (+8.7pp vs +6.3pp in the original); at
  0.20, +4.7pp (vs +4.1pp); matched-recall deltas again within the noise band.
  Artifacts: methods/compare_anyerror_vs_typed_v2/yearswap_train2223_test24/.
- Robust to ensemble size (300/500 vs 1000/2500 trees: same ordering).

*Artifacts: methods/compare_anyerror_vs_typed_v2/ (dollar- and counts-basis plots,
sweep + summary CSVs; small-ensemble run preserved as xgb300rf500_).*

## 4. Engine tuning: what matters and what doesn't

19-config one-at-a-time grid (any-error frame, frontier = mean hold-out
precision at matched dollar recall):

- **ranger: mtry = 2 beats mtry = 1** (0.223 vs 0.214) — the "pure randomness
  for diversity" premise was wrong. (Tested once, at 500 trees / 90% LCB;
  adopted and unchallenged since.) Tree count is a plateau, not a peak: 250
  trees 0.206, 500 trees 0.214, 1000 trees 0.213 (a 0.001 gap, within noise),
  2500 trees 0.210 — more trees add inventory and reach, not matched-recall
  precision, AT A FIXED 90% z. Per §5 we nonetheless adopted 1000 trees: with
  stringent filtering (z = 2.326) the bigger pool keeps its reach without the
  precision dilution.
- **xgboost: eta 0.02 > 0.05, and low subsample (0.15-0.30) beats high
  (0.60-0.80)** — echoing the old rpart sampfrac finding; values within
  0.15-0.30 are statistically indistinguishable. Both results are independent
  of the filter setting; they are the production defaults (eta .02,
  subsample .20).
- **Round count only looks like it matters at a loose filter.** At a fixed
  90% LCB, 100 rounds beat 1000 on the frontier (0.217 vs 0.198) — but most of
  that gap is the selection-multiplicity dilution that §5 shows is
  correctable: at each pool's appropriate stringency (small @ 90%, big @ 99%)
  the two trace essentially the SAME hold-out frontier. What mining big buys
  is the MENU behind each operating point — ~2.6x the rules pass any floor in
  this experiment (~5x at production scale with both engines), each with a
  stiffer per-rule guarantee — not extra portfolio precision. Production:
  1000 rounds — "mine big, filter stringently."
- Depth 4~5 >> 3. Inventory (shortlist size) and frontier quality often
  DISAGREE — e.g. subsample 0.75 gives more rules but a worse frontier.

*Artifacts: methods/parameter_tuning_v2/v2_tuning_{ranger,xgboost}.png, summary CSVs,
v2_subsample_fine.*

## 5. "Mine big, filter stringently" — the flexible LCB

More mining extends recall reach but dilutes matched-recall precision via
selection multiplicity (more lucky rules clear any floor). The z-sweep showed
the dilution is mostly CORRECTABLE in order to keep the potential for greater recall:

- On the 1000-round pool, raising z (80%->99%) recovers precision cleanly and
  monotonically; on the 100-round pool z barely matters — the multiplicity
  signature.
- **1000 rounds @ z=2.33 lands on the same 0.20-floor operating point as 100
  rounds @ z=1.28 (55% recall @ 17% precision), with 2,026 vs 789 filtered
  rules behind it.** Honest framing: the two recipes trace the same union
  frontier — the big pool's gain is rule inventory (substitutes for
  expert-driven removal) and per-rule guarantee stringency, not portfolio
  precision or reach. (Figure: presentation_figures/
  mine_big_filter_stringently.png.)
- Residual gap (~1/3 of the dilution) is intrinsic marginal-rule quality; no
  z fixes it.

Production recipe adopted: xgb 1000 rounds / eta .02 / subsample .20, ranger
1000 trees / mtry 2, z = 2.326. Result: **1,535 filtered-in rules** (vs 834
under small ensembles at 90%), with better median hold-out quality per frame
(e.g. other_error 0.212 vs 0.197; unearned 432 rules at 0.284).

*Artifacts: methods/parameter_tuning_v2/v2_lcbz_sweep.png + v2_lcbz_summary.csv;
inclusion_rules_by_hh_size_v2/ (run1_small_ensembles_z90/ preserved).*

## 6. Frame-relative vs deployed (any-error) performance

A rule mined for one error type flags cases whose OTHER errors count as wins
in deployment. Any-error precision runs ~2-2.7x the frame-relative number
(e.g. earned union at the 0.20 floor: 0.080 frame vs 0.178 any-error). All v2
outputs carry both views; quote the any-error numbers to states.

## 7. other_error: the largest, previously unmodeled category

other_error (deductions, shelter, household composition; 1,377 of 2,994 total
2023 errors — more than any typed category) had never been mined. It produced
the single largest filtered-in block (1,082 rules, median hold-out 0.212) —
heterogeneous or not, it has learnable structure.

## 8. ESAP / elderly-disabled: feature suffices, and why

Decision: NO fourth stratum or separate model. The models carved the caseload
themselves:

- elderly/disabled HHs are 49.8% of caseload, 48.2% of error cases, 40.9% of
  error dollars — NOT more error-prone.
- Their error MIX is what differs: 64% other_error + 18% unearned (the two
  most detectable types) vs other households' 45% earned (the least
  detectable). Detection asymmetry is compositional.
- The unearned frame became a de facto elderly model on its own: 91.8% of its
  flags are elderly HHs; all 96 of its indicator-using rules REQUIRE
  elderly/disabled. The earned frame is the mirror image (82% non-elderly
  flags; its indicator rules require NOT-elderly).
- Union recall: 26.7% of elderly-HH errors vs 7.2% of other-HH errors (dollar
  recall 28.2% vs 7.4%); precision slightly HIGHER inside elderly flags
  (0.219 vs 0.188).
- The real gap is non-elderly working households (earned-income volatility) —
  this could lack of signal issue, but also is an area I'm continuing to explore. 

*Artifacts: methods/check_esap_coverage_v2.R (rerun anytime).*

## 9. States: a clean two-regime deployment rule

Seven states, independent grid search (thresholds +/-10-25%) on state
2022+2024, tested on state 2023. Qualification: >=20 train flags at >=0.20 raw
precision; tuned variant maximizes train dollar recall. Not recommendations, 
just a reflection of how this is working right now. 

| state | qualified | tuned: prec @ recall ($) | national as-is: prec @ recall ($) |
|---|---|---|---|
| Connecticut | 35 | 0.209 @ 43.0% (49.1%) | 0.228 @ 24.4% (31.8%) |
| Arizona | 32 | 0.211 @ 20.2% (24.5%) | 0.467 @ 11.8% (9.7%) |
| North Carolina | 11 | 0.133 @ 26.2% | 0.250 @ 7.7% |
| Michigan | 6 | 0.140 @ 9.5% | 0.280 @ 9.5% |
| Virginia | 3 | 0.105 @ 3.3% | 0.158 @ 5.0% |
| Washington | 3 | 0.048 @ 2.3% | 0.364 @ 9.1% (11.8%) |
| Louisiana | 3 | 0.000 @ 0% | 0.182 @ 5.3% |

**Criterion comparison (2026-07-06, three qualification/selection schemes,
all with >= 5 train flags):** (a) 90% LCB qualify + LCB-max select, (b) simple
support floor (>= 20 flags @ >= 0.20 raw) + dollar-max select, (c) hybrid: 90%
LCB qualify + dollar-max select. 2023 test unions in the three states where
rules qualify:

| state | (a) LCB+LCBmax | (b) floor+$max | (c) hybrid |
|---|---|---|---|
| Connecticut | 0.217 @ 32.6% | **0.209 @ 43.0%** | 0.215 @ 33.7% |
| Arizona | 0.290 @ 15.1% | 0.211 @ 20.2% | **0.247 @ 20.2%** |
| North Carolina | 0.159 @ 15.4% | 0.133 @ 26.2% | 0.170 @ 23.1% |

Column definitions — each scheme is a QUALIFICATION bar (which threshold
variants of a rule are eligible at all, judged on state training data) plus a
SELECTION objective (which single qualifying variant the state deploys). Cells
show the deployed union's precision @ recall on the state's 2023 test year.

- **(a) LCB+LCBmax** — qualify: 90% Wilson lower confidence bound of the
  variant's train precision >= 0.20 (>= 5 cases flagged); select: the variant
  with the HIGHEST LCB (the most statistically defensible version — tends to
  pick tight, small-footprint variants).
- **(b) floor+$max** — qualify: variant flags >= 20 train cases at >= 0.20 raw
  precision (the simple, transparent criterion); select: the variant capturing
  the most error DOLLARS on train among qualifiers (the widest-reaching
  version that still clears the bar).
- **(c) hybrid** — qualify as in (a) (LCB-based, careful gate); select as in
  (b) (dollar-maximizing, aggressive pick).

No scheme dominates: (b) wins the largest state on recall; (c) dominates (b)
in Arizona (equal recall, +3.6pp precision) and is the precision-recall middle
ground in NC. The qualification bar drives the trade (LCB admits fewer
marginal-support rules -> higher precision, less reach); the selection
objective drives reach. Differences are a few points on small test samples —
within noise for a per-state ranking. DEFAULT (decided 2026-07-06): the hybrid
(c) — QUALIFY_MODE = "lcb" — dominating in AZ, middle ground in NC, and close
behind in CT; it also keeps the qualification logic consistent with the
national pipeline's LCB filtering. QUALIFY_MODE = "support_floor" remains
available for states wanting the simpler-to-explain criterion or maximum
reach. In the four small states every scheme loses to national-as-is
(criterion-invariant).

**Rule of thumb: tune locally if ~30+ rules qualify on state train; otherwise
deploy the national selection at national thresholds unchanged.** Small-sample
tuning is winner's-curse territory (Louisiana's tuned rules went 0-for-6 on
test; Washington's collapsed to 5% precision while national-as-is delivered
36% at 9% recall). Where tuning works it works well: Connecticut catches 43%
of errors / 49% of error dollars at 21% review precision.

National selection sent to states: up to 60 rules per frame by national train
LCB (earned admitted at a relaxed 0.15 floor; 186 rules total).

**THREE-WAY comparison (2026-07-06; train 2022+2024, test 2023):** adding
"mine on the state's own data" (raw >= 0.30 @ n >= 30) as option (c) beside
national-as-is and national-tuned. Where data is deep, own-mining dominates
recall by a wide margin at moderate precision (AZ: 86% of error dollars at
0.221; VA 81% at 0.138; NC 71% at 0.114; CT 70% at 0.181); in the thin states
it collapses to a handful of rules (LA and WA: 8 rules each). IMPORTANT
CAVEAT: the 2023 test year sits BETWEEN the training years — temporal
interpolation flatters all options and own-mining most; the year-split
extrapolation checks (below) are the honest forward-deployment expectation.
Artifacts: methods/compare_state_options_v2/.

**Same-era NEIGHBOR TRANSFER — the thin-state recipe (2026-07-06, Louisiana):**
train on the state's fire-rate-similar neighbors (cosine on sqrt rule fire
rates; for LA: IN, OK, AL, NM, KY) using the SAME years, exclude the state
entirely, test on all of the state's rows. Result for LA: 913 rules; of the
386 firing in LA, median precision 0.33 (neighbor-train) -> 0.18 (LA), 48%
holding >= 0.20, union 0.141 @ 49% of LA errors (2.3x lift) — versus LA-alone
mining's collapse and national-as-is at 5% recall. An earlier pooled attempt
that mixed eras failed (median -> 0.00): the era match, not just the neighbor
choice, is load-bearing. FY25 adds temporal drift on top, so quote below-0.18
expectations. Deliverable rule: hand the state the full neighbor-trained list
ranked by NEIGHBOR-train precision (selecting "rules that held in the state's
test" would be a fresh winner's curse).
Artifacts: methods/state_similarity_v2/ (repo) + custom_one_off/louisiana/.

**Single-state MINING (2026-07-06, one state, year-split validated):** when
national rules cover too little of a state's error mix, mining directly on
state data works ONLY with a hard support floor. At LCB >= 0.30 with n >= 5,
the mined list collapsed out-of-year (median holdout precision 0.000; 59% of
rules caught nothing). At raw precision >= 0.30 with n >= 30, it held: median
train 0.33 -> holdout 0.21 (~1/3 deflation), only ~1% of rules at zero, 57%
holding >= 0.20, union ~60% recall. Deflation expectations, not the training
numbers, are what to hand the state. Also: two-sided ladder rules
(a < x <= b) mean the dominance dedup's nesting assumption bites less at
state scale — the ladder-collapse post-filter matters more there.
(State-specific artifacts live outside the repo in custom_one_off/.)

*Artifacts: state_rules_v2/ (per-state rule CSVs with national + tuned
thresholds side by side; state_union_summary.csv; LCB-criterion run preserved
in run1_lcb_criterion/).*

## 10. Data visibility: how much of a state's error population the public
## data can even show (2026-07-07)

Two pipeline defects were found and fixed, then the remaining gap quantified:

- **Stale single-element frame**: reg_model_data.rds descended from a build
  with the multi-element drop active — every result before 2026-07-07 was
  mined on ~69% of true errors (multi-element cases, 31% of errors, excluded).
  Fixed: multi-element cases kept (second_element_i tracks them; NOT a mining
  feature — states report second elements too inconsistently). The frame now
  saves from the script directly, so it cannot silently drift again.
- **Deduction-NA drops**: states like WA/MS/MN leave optional deduction fields
  unrecorded in blocks; those rows are now zero-filled (ded_fields_imputed
  flag) instead of dropped. Recovered ~16% of WA's caseload.
- **BENMAX filter: exonerated** (drops zero rows in the real pipeline; an
  earlier circumstantial attribution to it was wrong).

Post-fix VISIBILITY (frame errors / [raw over-threshold errors + ineligible
exclusions], FY22-24): national 71%; WA/VA/LA now 78-81%. The floor is
INELIGIBLE CASES, which the public file excludes entirely and which are
100%-of-benefit errors: NJ sees only 43% of its error population, TN 51%,
AR/MO/UT ~53%. **Guidance: states below ~60% visibility should treat
national/public rules as a supplement and run the mining pipeline on internal
data, which contains their ineligible determinations.** Artifacts:
methods/state_error_accounting/ (per-state-year raw error counts, FYWGT-weighted
error dollars, exclusions; visibility_by_state_2022_2024.csv).

Rule-content changes from data revisions are tracked with
methods/compare_rule_sets_v2.R (exact / threshold-shifted / coverage-overlap /
dropped / new classification, plus a check of where new rules' catches
concentrate). The measured effects of the 2026-07-07 rebuild on the mined
rules — ~3x inventory, old set 93% preserved, higher LCB-floor reach, and the
finding that the new rules are NOT multi-element specialists — are documented
in `methods/effects_of_munging_options.md`.

## 11. Household-size stratification: split, but split coarsely

Established under the pre-era methodology (June 2026, earned income, greedy
nets) and the reason the pipeline uses 1 / 2-3 / 4+:

- **1/2-3/4+ stratification: mean precision 0.148 vs pooled (no split) 0.101**
  at matched recall — a ~47% relative precision gain from splitting at all;
- the coarse 3-way grouping also beat the standard 5-way 1/2/3/4/5+ (0.127)
  and 1/2/3-4/5+ (0.139): finer strata thin the training data faster than
  they add homogeneity;
- intuition: even dollar-scaled features (income/benefit relative to HH size) mean
  different things at different HH sizes; stratifying lets thresholds differ,
  while over-splitting starves rule support.

**v2-stack confirmation (2026-07-06).** With
production engines (mtry=2 ensembles, HH size available as a feature) and strict
LCB: pooled 0.2256 mean precision vs 1/2-3/4+ 0.2216 vs 5-way 0.2142. The
pre-era +47% gap does NOT replicate — like the ESAP finding, ensembles using restricted mtry (set to 2)
capture most of what stratification provided when the stratifier is a feature.
The 3-way split still wins where it matters operationally: **reach** (54.8% vs
48.4% dollar recall at the 0.20 floor) and **filtered rule inventory** (4,279 vs
809 rules — per-stratum filtering gives rules the within-size support to
clear the stiff bound). The 5-way split loses either way. 

*Artifacts: compare_models_by_HHsize_vs_pooled/strata_earn_inc_scheme_summary.csv
(pre-era); methods/compare_hh_strata_v2/ (v2 confirmation).*

## 12. Cross-state transfer vs honest national baselines (2026-07-09)

Leave-one-state-out benchmark: for 12 target states, any-error rules were
mined on donor pools that NEVER saw the target (2022-24 both sides), then
scored on the target under review-capacity budgets (rules added in
descending train-LCB order until the budget fills). Donor pools: top-5
neighbors under four similarity definitions — fire-rate cosine (sqrt),
inverse-frequency-weighted cosine (IDF), naive-Bayes/KL over rule-firing
profiles (NB), QC-derived policy vectors — plus a leave-one-state-out
NATIONAL pool (all 48 other states, same any-error recipe: the honest
version of the national baseline).

Median delivered precision / share of error dollars across the 12 states:

| budget | natl as-is* | natl LOO | fire | IDF | NB | policy |
|---|---|---|---|---|---|---|
| 5% of caseload | 0.336 / 16% | 0.309 / 12% | 0.264 / 12% | 0.273 / 11% | 0.270 / 12% | 0.246 / 10% |
| 10% of caseload | 0.307 / 30% | 0.245 / 22% | 0.270 / 24% | 0.240 / 22% | 0.278 / 24% | 0.236 / 21% |

*natl as-is = the production 5-frame shortlist, trained on 2022+2024
INCLUDING each target's own cases. Its edge over natl LOO conflates two
things — the in-sample advantage AND the richer 5-frame recipe (LOO pools
are any-error-only) — so the as-is-vs-LOO gap (0.03 at 5%, 0.06 at 10%) is
an UPPER bound on in-sample flattering at these budgets.

Findings (same-recipe comparisons, i.e. LOO vs similarity pools):

- **At a 5% budget, more data wins**: the 48-state LOO pool beats 5-neighbor
  pools in 9 of 12 states (median 0.309 vs 0.273 best-transfer).
- **At a 10% budget, similarity wins as often as size**: 5-neighbor pools
  match or beat the 48-state pool in 6 of 12 states (NB median 0.278 vs LOO
  0.245), with the transfer wins larger than the losses (Mississippi +0.066,
  Connecticut +0.054, Texas +0.038, Colorado +0.035). Five well-chosen
  states can out-teach forty-eight at moderate budgets.
- **NB ~ fire, both > policy-only**: NB's donor pools often coincide with
  fire's (its LA pool is identical); where they differ NB is equal or better
  (California 5%: 0.302 vs 0.233). IDF is the most conservative and wins
  where precision is the binding concern (CT 10%: 0.337). Policy-only pools
  are erratic; policy information helps only blended with fire rates.
- **Budget-filling fixes the workload problem**: at fixed LCB floors the
  same rule sets flagged 12-73% of caseloads; under budgets every approach
  delivers 0.16-0.45 precision. Mississippi, a total failure at fixed
  floors (rules stopped firing), is transfer's best budget result
  (0.346/16% at 5%) — the fixed-floor failure was a floor artifact.
- **Era-matched similarity is load-bearing and definitions converge**:
  LA's 2022-24 neighbor lists under fire/NB/policy agree closely with each
  other and with the donor pool that worked in the July transfer, and all
  differ sharply from the 2017-19 lists.

Deployment guidance (supersedes nothing; complements the two-regime rule in
section 9): the production national shortlist remains the best single list at
small budgets, but numbers quoted to a state from in-sample national training
overstate honest performance by up to the as-is-vs-LOO gap; at moderate
budgets (~10%), similarity-picked donor pools (fire or NB) are competitive
with any national option and are the honest choice where a state's own data
must stay out of training.

*Artifacts: methods/state_similarity_v2/transfer_benchmark/ (benchmark +
budgeted_menu_results.csv); methods/state_similarity_v2/similarity_*_2022_2024.csv
and _2017_2019.csv; methods/state_nb_similarity_v2.R; methods/neighbor_transfer_benchmark_v2.R;
methods/budgeted_transfer_menu_v2.R; overnight_nb_loo_run.log.*

## 13. Pre-registered year-swap replication of the model-selection studies (2026-07-09)

Every model-selection decision (engines, subsample, filter stringency,
ensemble size) had been judged on the same held-out year, 2023 — a year that
sits BETWEEN the training years, so the selection procedure itself risked
being tuned to one interpolated year (methods/pipeline_critique_2026-07-09.md, V2).
Guard: the four decisive selection claims were re-run with the year roles
swapped — train 2022+2023, test 2024, a year that never influenced any
design decision — with expectations and falsification criteria WRITTEN DOWN
BEFORE the run (`methods/yearswap_preregistration_2026-07-09.md`). Levels were
expected to shift (rebuilt frame, different year); orderings and margins
were what the original decisions rested on, so orderings and margins were
what was pre-registered.

**Claim 1 — engine pairing.** The claim: mining rules with xgboost and a
constrained random forest (ranger, mtry 2) TOGETHER yields more recall at
the 0.20 filter floor than either engine alone or a bagged-CART + ranger
pair, at a small precision cost, because the two engines contribute
complementary rule vocabularies. REPLICATED in ordering: the pair again
leads recall at the floor (0.794 vs 0.773 for xgboost alone, 0.757 ranger,
0.724 bagged CART) at a small precision cost (0.185 vs 0.189-0.202). The
pre-registered margin (>= 3pp over the best single engine) came in at
2.1pp — the pairing advantage is real but thinner than the 2023-judged
number, a direct measurement of how much adaptive selection flattered the
original margin.

**Claim 2 — low subsampling beats high.** The claim: showing each boosted
tree only 15-30% of the training data produces better rules than showing it
60-80%, so subsample belongs at 0.20. PREDICTION FAILED; finding RETIRED.
On 2024 the band structure vanishes: precision at the 0.20 floor spans
0.181-0.186 across ALL nine settings from 0.15 to 0.80 — one flat plateau,
with the predicted ordering (worst low-band member >= best high-band
member) failing, 0.182 < 0.186. Per the pre-registered decision rule, "low
subsample beats high" is no longer quotable; the surviving claim is only
that subsample barely matters in this range. Production stays at 0.20 (it
leads mean precision, 0.303, and nothing beats it meaningfully).

**Claim 3 — stringent filtering delivers more precision.** The claim: among
rules mined by big ensembles, raising the one-sided Wilson lower-bound
stringency from 80% to 99% (z = 0.84 -> 2.33) monotonically raises the
delivered (held-out) precision of the surviving union while costing recall
— the mechanism that makes "mine big" safe. REPLICATED: precision at the
0.20 floor again rises monotonically in z (0.169 / 0.175 / 0.179 / 0.188)
while recall falls (0.873 -> 0.776); the 99%-vs-80% advantage is 0.019
against a pre-registered >= 0.020 prediction and a 0.010 falsification
line.

**Claim 4 — big ensembles widen the menu, not the frontier.** The claim:
1000-round/1000-tree mining does not trace a better precision-recall
frontier than 100-round mining, but produces several times more distinct
filtered rules (the menu states need for vetoes and substitutes).
REPLICATED: inventory ratio 7.3-7.9x (26.6-29.1k vs 3.6-3.7k rules), with
the big pool's precision deficit at matched stringency only 0.020-0.022 and
+7pp recall at the floor.

Net: three of the four selection findings replicate on a year that never
judged any design decision, and the procedure produced one honest
retraction (Claim 2) — evidence the selection methodology was not
2023-luck, and that the pre-registration has teeth. No production setting
changes.

*Artifacts: methods/yearswap_preregistration_2026-07-09.md (predictions + results);
methods/compare_engines_v2/yearswap_train2223_test24/;
methods/parameter_tuning_v2/yearswap_train2223_test24/;
methods/run_selection_yearswap.R.*

## 14. Time-shifted deployment benchmark: own-state vs NB transfer vs national on 2024 (2026-07-10)

Section 12's transfer benchmark scored every pool on the same era it was
mined from, so its verdicts could lean on same-era correlation
(methods/pipeline_critique_2026-07-09.md, V6). Guard: the three deployable
options were re-run as a state would actually face them — rules mined on
2022+2023 only, scored on the target state's 2024 cases only, budgets
filled in descending train-LCB order. Approaches: the target's OWN 2022-23
data (own_state), a 5-neighbor donor pool picked by 2022-23 NB/KL
similarity (transfer_nb), the 48-other-state national pool
(national_loo), and the ALL-state national pool including the target
(national_all -- honest here because the test year is unseen, and the
list a state deploying "national rules" actually receives). Any-error
recipe throughout; 12 target states.

Median delivered precision / share of the state's 2024 error dollars:

| budget | own_state | transfer_nb | national_loo | national_all |
|---|---|---|---|---|
| 5% of caseload | 0.253 / 14% | 0.256 / 11% | 0.296 / 13% | 0.300 / 16% |
| 10% of caseload | 0.240 / 25% | 0.245 / 20% | 0.276 / 25% | 0.273 / 25% |

- **The national pool is the best time-shifted default.** It leads median
  precision at both budgets and ties own_state on dollars, and it is never
  the disaster case. Under national_all at 10% every state clears its base
  rate (precision 0.18-0.37 vs 8-17% bases, 1.5-3.4x lift over random
  review).
- **Including the target's own 2022-23 rows in the national pool changes
  little vs holding it out**: national_all minus national_loo precision at
  10% spans -0.081 (Michigan) to +0.062 (Washington) with no systematic
  direction (medians 0.273 vs 0.276). With a truly unseen test year there
  is no in-sample flattering to correct -- the same-era as-is-vs-LOO gap
  (section 12) came from scoring inside the training era, not from the
  state's rows being in the pool per se.
- **Section 12's 10%-budget transfer advantage did NOT survive the time
  shift.** Same-era, NB led LOO 0.278 vs 0.245 at 10%; on 2024 the order
  flips, LOO 0.276 vs NB 0.245, and NB trails on dollars at both budgets
  (20% vs 25% at 10%). Caveat: only NB was re-run (fire/IDF/blended were
  not), and the neighbor lists came from 2022-23 similarity, so this
  retires "similarity pools beat the national pool at moderate budgets" as
  a deployment claim while leaving open whether another definition
  transfers better across years.
- **Own-state mining is high-variance, exactly as the two-regime rule
  (section 9) predicts.** Its wins are the largest anywhere (Connecticut
  0.416, Virginia 0.371, Mississippi 0.355 at 10%) but its failures are
  total: Washington's own-state rules deliver 0.049-0.075 precision on
  2024 — BELOW the state's 8.5% base rate, i.e. worse than random review —
  and Louisiana's 0.161 trails both pooled options. The 10% LOO-minus-own
  precision gap spans -0.103 (Mississippi) to +0.173 (Washington).
  (Bullet judged against national_loo; the own-vs-national deployment
  charts use national_all, same story.)
- **NB transfer is insurance where own-state fails, not a first choice.**
  Where own_state collapses it holds up (Washington 0.247, Louisiana
  0.207 at 10%) and it wins outright only in Connecticut (0.500 at 5%);
  everywhere else it is second or third.

Deployment guidance: quote states the national_all numbers (national list,
time-shifted test) as the honest default; offer own-state mining only where
the state's own held-out year confirms it (it cannot be assumed — a
mid-size state like Washington can fail below base rate); keep NB transfer
as the fallback for states whose own mining fails and who want a smaller,
more tailored pool than the national list. The own_state rows double as
the per-state own-data appendix table.

*Artifacts:
methods/state_similarity_v2/transfer_benchmark_train2223_test24/deployment_menu_train2223_test24.csv;
methods/deployment_benchmark_train2223_test24.R;
methods/state_similarity_v2/transfer_benchmark_train2223_test24/deployment_benchmark_run.log;
methods/state_similarity_v2/similarity_nb_2022_2023.csv;
charts: methods/visualize_deployment_own_vs_national_v2.R ->
deploy_national_dotplot_budget05/10.png,
deploy_own_vs_national_budget05/10.png.*

## 15. Frozen per-state lists: the handable deliverable, priced (2026-07-10)

The deployment benchmark's budget fill (section 14) chooses rules against
the test year's realized caseload. A state, however, needs a list it can
hold in advance. Deliverable design (settled after one iteration): ONE
ranked list per state -- the national pool (mined on all states'
2022+2023) budget-filled against the state's own 2022-23 CASELOAD
COVARIATES only, to the target sizing (the core) and then onward to 3x the
target (the buffer). The state walks the list in rank order, activating
each rule while its flagged total fits capacity -- outcome-free, and it
lands on budget whichever way firing rates drift. The buffer is part of
the deliverable, not an option: a state never idles reviewers because a
list ran dry, so core-only numbers understate deployment (and a core that
over-fires must be trimmable the same way). Raw core drift on 2024 ran
2.3%-12.0% of caseload against 5/10% sizings; the walked list lands at
4.9-5.0% and 9.3-10.0% in all 18 states.

Median across 18 states, at identical review volume (frozen walked list
vs the full national pool batch-filled against the realized 2024
caseload): precision 0.294 vs 0.301 at 5% sizing, 0.270 vs 0.275 at 10%;
share of error dollars 12.3% vs 15.6% at 5%, 24.6% vs 25.2% at 10%.
Median deployed list: 23 rules (5%), 42 rules (10%). All 18 states clear
their base rate at both sizings.

- **Advance commitment costs almost nothing at matched workload**: the
  precision gap is under a point at both sizings; the dollar gap is ~3pp
  at 5% and under 1pp at 10%.
- **Cross-state content**: the 18 deployed lists resolve 898 rule-slots to
  297 distinct rules; ~1/3 of each state's list is unique to it, median
  pairwise Jaccard 0.13, and only 8 rules serve 10+ states (the
  high-deductions + benefits-near-maximum family). The national ranking
  personalizes itself through each state's case mix.

Deployment recipe this supports: hand each state its frozen list (mined
and sized on public data through the latest year), with the instruction to
validate internally on their own newer data before relying on it -- the
public files see only 43-81% of error cases (section 10), and this
experiment validates one year ahead only.

*Artifacts:
methods/frozen_list_experiment_v2.R ->
methods/state_similarity_v2/transfer_benchmark_train2223_test24/frozen_list_results.csv
+ frozen_lists/frozen_list_<state>.csv (the handable lists);
methods/contributing_rule_overlap_v2.R -> contributing_rules_by_state.csv,
contributing_overlap_jaccard_budget10.csv, contributing_overlap_summary.csv;
methods/count_contributing_rules_v2.R -> contributing_rules_summary.csv.*

## 16. Blending state and national rules on one confidence scale (2026-07-10)

Never previously run: merge each state's OWN mined pool into the national
pool and rank every rule by its own-training 99% Wilson LCB (national
rules bounded on national 2022-23 train; state rules on the state's own
2022-23). Both bounds read "at least this precision with 99% confidence",
so the merged ranking is coherent and the bound applies the certainty
discount to small-support state rules automatically. Same freeze/buffer/
walk protocol as section 15; a 98% variant was also run (immaterial,
within a point everywhere).

Median across 18 states, single recipe, no regime decision (precision /
error dollars): blended 0.324 / 15% at a 5% budget vs national-only
0.294 / 12%; at 10%, blended 0.262 / 24% vs national-only 0.270 / 25%
(tie). The two-regime best-of-two pick reads higher (0.337 / 0.285) but
selects its winner ON the test year; the blend needs no pick.

- **Where state rules clear the unified bar, interleaving beats either
  pool alone**: Arizona deploys 20 state rules and delivers 0.326 (vs
  0.291 for its best single regime), DC 0.495 (vs 0.464), Mississippi
  0.374 (vs 0.355), Missouri deploys 17 state rules. Half the states
  deploy at least one state rule at 10%.
- **The blind spot is transfer asymmetry**: a national rule's LCB is a
  tight bound on its precision in the NATIONAL mix and says nothing
  about transfer to this state; the state rule's LCB honestly prices its
  noise. The scale therefore over-trusts national rules exactly where
  the national mix fits worst: New Jersey's own rules never enter (their
  small-sample bounds cannot beat 45k tight national bounds) and the
  blend under-delivers there (0.161 at 10% vs 0.230 for NJ's own list).
  Relaxing to 98% does not rescue this.

Deployment guidance: the BLEND is the default shipped recipe (better at
5%, no worse at 10%, no regime decision to defend); the own-pool list is
kept as a FALLBACK, activated only where the state's own internal
validation shows the blend under-performing. In low-visibility states
this is the only honest arbiter -- New Jersey's public files show 43% of
its error cases (section 10), so the public data cannot establish which
option truly performs better there; the state's internal check decides.

*Artifacts: methods/blended_frozen_lists_v2.R ->
methods/state_similarity_v2/transfer_benchmark_train2223_test24/blended_frozen_results.csv;
blended_frozen_run.log (same folder); comparison inputs:
frozen_list_results.csv, frozen_own_list_results.csv.*
