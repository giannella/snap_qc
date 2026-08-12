# SNAP QC rule mining: findings: complete detailed record

> This is the **full evidence log**: every number, table, caveat, and artifact
> pointer, in the order the work happened. It is the source of truth and is kept
> deliberately dense. If you are here to learn the key points, read the shorter
> **[modeling_findings.md](modeling_findings.md)** instead. It carries the same
> plain-language takeaway for each section and links back here for the detail.

Working notes for the presentation. Each section lists the supporting artifact
files. Methods details live in `methods/design_drop_pre_architecture.md`. Sections
1-13 are hold-out numbers (train 2022+2024, test 2023) unless noted; the
deployment sections (14-16) train on 2022+2023 and test on 2024, a year that
never influenced any design decision. Current deployment guidance lives in
sections 14-16 (and the README): the blended frozen list is the default
deliverable; earlier per-state guidance in sections 9 and 12 is partially
superseded and carries notes where so.

---

## How to read this document

This is the running lab notebook for the rule-mining project: every experiment we
ran, what moved held-out performance, and what didn't. It was written as results
came off the console, so the sections are dense. To make it usable without having
watched the work happen, each numbered section now opens with a plain-language
**Takeaway**, and the recurring shorthand is defined once here.

**Two kinds of findings.** Each section is tagged as one of:

- **About the data**: something that appears to be true of SNAP QC data itself and
  would probably show up no matter how you built the model. Treat these as portable
  lessons.
- **About our pipeline**: a choice that made *our* system better on *our* tests.
  These earned their place with numbers, but they are engineering decisions, not
  laws of nature; don't assume they carry over to a different setup.

Some sections carry a little of both; the tag marks the main point and the takeaway
notes the rest.

**Glossary of recurring terms** (skip if you know them):

- **Error / "over threshold."** A QC case counts as an error when its payment was off
  by more than the tolerance threshold. Base rates are low: about 11% of cases
  nationally have any error; typed categories run 0.4-6%.
- **Precision.** Of the cases a rule flags, the share that are truly errors: "is
  flagging this worth a reviewer's time?"
- **Recall.** Of all the errors out there, the share our flags catch. **Dollar
  recall** weights each error by its dollar size.
- **Base rate / lift.** The error rate you'd get reviewing at random; lift is how many
  times better than that a rule does.
- **Rule / union.** A rule is one if-then condition (e.g. "deductions high AND benefit
  near maximum"). We flag a case if *any* selected rule fires; the combined flag set
  is the **union**, and an error caught by several rules counts once.
- **Frame.** A modeling dataset built around one target. We mine four **typed frames**
  (earned overissuance, unearned overissuance, underissuance, other) plus one
  **any-error frame** that pools all error types.
- **Frame-relative vs any-error (deployed) precision.** A rule mined for one error type
  often flags cases that have a *different* error too, which still counts as a good
  catch in real review. Frame-relative precision credits only the target type;
  any-error precision credits any error and runs ~2x higher. Quote states the
  any-error number (section 6).
- **Filter floor vs review budget.** A *filter floor* is a minimum precision we require
  of rules we keep. A *review budget* is the more realistic framing: a state can review
  only, say, 5% or 10% of its caseload, so fill that capacity with the best-ranked
  rules and see what you catch.
- **Stratum.** We split the caseload by household size (1 / 2-3 / 4+) and mine each
  group separately.
- **Holdout / year-swap / era.** We always test on data the model never saw. Sections
  1-13 train on 2022+2024 and test on 2023; sections 14 onward train on
  2022+2023 and test on 2024, a year that never influenced any design choice. A few
  claims were re-checked on a separate 2017-19 "era" as an independent second test.
- **Wilson lower confidence bound (LCB), and z.** Rather than trust a rule's raw
  training precision (which is optimistic, see section 1), we rank and filter on a
  statistical *lower* bound: "with high confidence, this rule's true precision is at
  least X." `z` sets how cautious the bound is; z = 2.326 is the 99% bound, our
  production setting. **Support (n)** is how many cases a rule flags in training; small
  n means an unreliable estimate, so we also require n >= 30.
- **Winner's curse.** If you pick the rules that scored best on the same data you
  scored them on, you are partly picking luck, and they look worse in deployment.
  Avoiding it is a running theme (sections 1, 9, 17, 19, 22).
- **Engines (xgboost, ranger).** We don't use these as predictors. We grow tree
  ensembles and harvest each branch as a candidate rule. They are just rule
  *generators*.

---

## 0. How the work unfolded (chronology + breadcrumbs)

One line per step, in the order it actually happened, so the story of what
we learned is recoverable. Each points to its numbered section.

- **07-04/05**: Diagnosed the winner's curse in raw-precision shortlists and
  adopted the Wilson LCB as the selection statistic (#1). Dropped {pre} for
  the v2 xgboost+ranger pipeline (#2). Typed-vs-pooled mining (#3), engine
  tuning grid (#4), and "mine big, filter stringently" (#5) followed on the
  same holdout (train 2022+2024, test 2023).
- **07-06**: Engine-pair studies settled xgboost+ranger (#2). Seven-state
  threshold grid search produced the original two-regime rule and the
  state-scale support-floor lesson (#9). Louisiana neighbor-transfer and
  single-state mining studies (#9). HH-strata v2 confirmation (#11).
- **07-07**: Rebuilt the modelling frame: multi-element error cases restored
  (~31% of errors), deduction NAs zero-filled; rule content survived
  (93%), inventory ~3x (effects_of_munging_options.md). Per-state data
  VISIBILITY accounting (#10). Floor-definition calibration figure (#1).
- **07-08**: State packages re-run on the rebuilt frame; exclusion pipeline
  moved to a relative safety standard; partition-aware threshold variants
  cut grid-search waste; lessons deck started.
- **07-09**: State-similarity program (fire-rate / IDF / policy / blended /
  NB definitions, per era) and the same-era transfer benchmark with the
  leave-one-out baseline (#12). Review-budget evaluation replaced floor-only
  reporting (#12). Senior-statistician critique (pipeline_critique_
  2026-07-09.md) prompted the pre-registered year-swap replication: 3 of 4
  selection claims replicated, subsample claim retired (#13).
- **07-10**: Deployment-grade benchmark (train 2022-23, test 2024):
  national_all is the default; same-era transfer advantage did not
  survive; own-state mining is high-variance (#14). Four state-adaptation
  schemes tested. None beat the national ordering on the median (#9 note,
  #14). Contributing-rules analysis: unions are built by dozens of rules,
  not thousands (#15). Frozen per-state lists + ranked buffer, walked to
  capacity (#15). Blend of state+national pools on the LCB scale becomes
  the default deliverable, own-pool list the fallback (#16). Delivery lists
  built for CT + 8 more states (custom_one_off/, gitignored).
- **07-12**: Guidance refreshed (README, CLAUDE.md, this header); chart
  conventions settled (states alphabetical; every results CSV gets a figure
  via methods/make_all_charts_v2.R); year-swap re-runs of the HH-strata
  study and the floor-definitions figure (this doc, #11 note when done).
  Deck-comment review round: floor-definitions replicated on 2024 (raw
  floors overpromise, LCB floors deliver 0.30 -> 0.336, 0.40 -> 0.475;
  methods/floor_definitions_2024_figure.R); era change in donor lists
  quantified (top-5 lists keep ~2 of 5 members across eras); LCB entry
  bars tabulated (0.30 floor needs 7/10 at n=10, 41/100 at n=100);
  hardcoded-year axis labels fixed in the anyerror-vs-typed and strata
  figure scripts; mtry frontier chart added
  (methods/visualize_mtry_frontier_v2.R).

- **07-29**: Tested the munging script's row exclusions by relaxing them and
  rebuilding the frame (#24): rejected. The one consequential filter is
  additive-only on the six years we use and the rows it excludes have both a
  circular error label and a failed pre-QC restoration; FY2020/FY2021 excluded by
  decision; the max-allotment filter removes only FY2021. No pipeline change.
- **08-03**: Tightened the admission false-discovery rate from 10% to 5%, floor held
  at n >= 30 (#25): no effect. 16 of 18 states identical at both review budgets,
  median within-state difference 0.000, because the highest-ranked rule the stricter rate
  removes sits at position 14,449 of 50,697 and a budget deploys the top 16 to 27.
  No pipeline change.
- **08-03**: Swept the support floor over seven shapes, flat and scaled to pool size
  (#26): the shipped n >= 30 wins. Raising the national floor to 66, 195 and 778 cost
  precision monotonically at the 5% budget (0.3345 to 0.3000, 0.2950, 0.2826) and
  dropping states to a flat 1% cost more (0.2558). Refutes the prediction that a
  larger search needs a larger floor. No pipeline change.
- **08-04**: Measured how far down the ranked pool the fill actually reaches (#27):
  median rank 1,544 at the 5% budget and 4,194 at the 10%, deepest 9,072, against
  137 and 283 rules delivered. Depth tracks rule width (-0.58), not weak fill
  (-0.31, -0.07). Lets evaluation run on a ranked window with a capacity
  certificate instead of the whole pool. No change to any delivered list.
  Verified the same day: on all 49 delivered lists a 20,000-rule window left zero
  slack and rebuilt every list identically to the shipped CSV (98 of 98), and on a
  200,000-rule research pool the pruned evaluation reproduced the unpruned results
  on all 70 comparisons while truncating every pool. 77 minutes to 1 per state.
- **08-04**: Measured the delivered exposure to a benefit-reconstruction artifact
  near the maximum benefit (#28, issue #1): 96.06% of truly-at-max households land
  on a ratio of exactly 1 but 2.39% land in [0.987, 1), and 1,134 of 19,316
  delivered rule instances (5.9%) draw at least half their flags from those
  mis-recreated rows, a median 6.3% of delivered cases at the 5% budget. Reading
  exposure off the rule text overstates it fivefold. Diagnostic only; no rule,
  feature or list changed.
- **08-04**: Characterized what each delivered rule finds so a state can pick
  its own rules (#29, issue #2): descriptive fields with Wilson intervals, no
  categories. What the error is ABOUT travels across states at the sampling
  floor (earned-income share reliability 0.94, split-half ratio 1.06); cause,
  timing and discovery carry real state variation (1.23 to 1.35). Two nature
  groups drift across eras only because codes 56/57/33/58 are new in FY2024.
  Also corrected: AGENCY 26 is a no-fault code, not undocumented, and coverage
  on error-case variances is near total.

- **08-05**: Pre-registered and ran the national-only cross-fit ordering test
  (#30): ranking the national pool by an out-of-fold bound does not beat the
  self-scored bound in the deliverable (-0.0044 within-state at the 5% budget
  against a pre-set +0.010 bar; +0.0000 at 10%). K-fold dropped, the blend
  stays shelved. The same run measured partition dependence: top-100 rules
  share 3.2% of signatures across partitions while the 5% lists catch 38.2%
  of the same errors; the pre-set case-overlap bar landed between its
  thresholds.
- **08-05**: Seed-stability study (#31), pre-registered with bars and run the
  same evening under the new routing rule (PDS-implemented, fresh
  senior-statistician review caught a checkpoint-provenance defect before
  launch): coverage is seed-stable at depth (pairwise error-case Jaccard
  0.959-0.965 at K=20,000; every seed's pool covers all 4,803 FY2024 errors;
  one mine saturates reach) while budget-depth lists are not (errors-caught
  overlap 0.531 at 5%, 0.666 at 10%). The instability is ordering, not
  vocabulary; the preference-ordering line of work opens.

- **08-06**: Measured the fill walk's residual adverse selection (#32, Eric's
  question): capacity-weighted marginal precision runs 3-4 points below the
  rules' own precision on holdout (0.314 vs 0.345 at the 5% budget); the
  zero-error marginal slices match their binomial expectation (median slice
  1-2 cases); an oracle re-walk gains +0.175 but the deployable version gains
  +0.000/+0.011, so marginal-quality-aware ordering is retired at public
  scale for lack of an estimable statistic. Priced into all quoted numbers.

- **08-06**: The fresh-share chain (#32-33, from Eric's redundant-finds
  question): measured the walk's adverse selection (3-4pp, unrecoverable by
  outcome-based reordering), raced six outcome-free dissimilarity instruments
  under a pre-registered plan (fresh share the sole SIGNAL; five failed the
  f < 0.99 bar), then a pre-registered two-pass fresh-share floor cleared the
  §30 bar (+0.0118 vs +0.010 at 5%, dollar recall held, workload identical by
  construction) - the first of five interventions to beat the plain LCB walk.
  Candidate pending the mandatory second-era replication. Side product: four
  off-by-one flag counts corrected in the §29 artifact; reg_model_data.rds
  established as the source of truth over the lossy CSV; the
  engineering-artifacts-are-not-failure-modes rule adopted and encoded.

- **08-06 (late)**: The fresh-share floor replicated on the second era (#34):
  era-2 confirmatory +0.0070 vs the +0.005 bar (47 states, FY2017-18 to
  FY2019), guard held, two-era pooled +0.0100; the bridge showed the effect
  indifferent to pool composition (+0.0118 national-only, matching blended to
  the fourth decimal; Eric's dilution hypothesis formally refuted by
  0.000019); the 0.60 challenger met all four pre-stated conditions and is
  the proposed shipping threshold. First walk change to achieve two-era
  validation; promotion is Eric's MINOR-bump call.

- **08-08**: Vocabulary attribution (#35; reclassified 2026-08-11 as a
  technical exploration, not a research finding): the 26-feature package
  (per-size income + frozen train-year percentiles) performed the same as
  the shipped 16 on the rebuilt frame (flat median, mean -0.0231 at 5%,
  MA/MI sign-consistent redistribution) despite 85.8% deployed usage;
  feature membership at equivalent performance is a judgment call on
  deployment cost, not a bar-verdict. Incidental artifact observation
  recorded with limits (strict band down to 2-3% of flags in both arms;
  correlate reliance not established; pre/post-fix level comparisons
  unpaired). Side product: the shipped vocabulary is 16 features, not 19
  (three finder-vector names never existed in the frame).

- **08-09**: The vocabulary factorial (#36, six arms, chained into the
  state re-mine; reframed 2026-08-11, EXPLORATORY - no ledger rows, informs
  only the open v2.5.0 vocabulary decision): replacing per-size with
  percentiles
  costs a moderate, sign-consistent amount (cross-state mean negative in
  every seed at both budgets, -0.026 to -0.045 at 5%; the 5% loss is a
  four-state harmed tail) - but on TEN evaluation states, a compute
  trade-off; the cached pools support a 49-state evaluation-only rerun.
  Per-size is table-free and the v2.5.0 candidate. The one-variable
  shelter_expenses_p 2x2 added nothing despite rank-1 admission and ~26%
  deployed share; half a night's mining on one variable is retrospectively
  judged not worth a study.

- **08-09 to 08-10**: A state-scale re-mine ran (runoff + 49 pools) but was
  DELETED 2026-08-11: its percentile arm used the frozen train-only
  construction, not Ben's pooled-years design, so the comparison did not
  test his idea. Artifacts removed; surviving pieces: the joint-BH admission catch (its review record,
  methods/state_remine_review_2026-08-09.md) and the per-size typed-mine
  caches. The replacement §37 - Ben's within-state pooled-years percentiles
  vs the per-size variables, exploratory, state scale - is queued.

- **08-11 (evening)**: The replacement #37 ran (EXPLORATORY, no ledger
  rows): Ben's as-built within-state percentiles, additive on the 19-var
  per-size vocabulary, vs that vocabulary alone; 48 paired states,
  any-error state mining, shipped fresh-share walk, single seed. Precision
  a wash at 5% (median +0.0000, harmed 9 / helped 10 - redistribution),
  slightly negative at 10% (-0.0041) with dollars slightly positive
  (+0.0062). The direct answer to the outlier framing: of 495 deployed
  `_p` conditions, 2 are high-tail (>= p90); the miner uses the features
  heavily (75% of deployed rules) as mid-scale income/rent encodings and
  zero/absence flags, not as outlier detectors. Review chain: B1
  stratum-evaluation bug caught and fixed pre-launch; the walk's
  exact-refill assertion shown empirical-only at state scale (tolerated,
  reported gaps; max 2 cases).

Charting/documentation conventions (2026-07-12): state-by-state charts list
states alphabetically; every benchmark CSV has a visualize_*_v2.R script
registered in methods/make_all_charts_v2.R so figures regenerate in one
command; findings sections carry explicit supersession notes rather than
silent rewrites.

## 1. The winner's curse, diagnosed and addressed

> **Takeaway: about the data (a statistical fact you'll hit too).** If you
> shortlist rules by their raw accuracy on the same data you measured them on, you
> reward luck: rules that looked ~20% accurate came in around ~10% on fresh data.
> The cure is to rank and filter on a cautious *lower* bound of each rule's precision
> (the Wilson bound) instead of the raw number. That one change made our training
> estimates that track held-out performance, and it will do the same for
> anyone mining rules this way.

Thresholding thousands of mined rules on raw train precision selects for lucky
rules: a "train precision >= 0.20" shortlist held only ~0.10 median on the
hold-out. Diagnosis showed this is almost pure selection noise, not model
overfit or year drift:

- among high-support rules with NO selection applied, train precision is
  essentially unbiased for hold-out precision (median gap -0.003, r = 0.83);
- the decay is symmetric (rules selected on HOLD-OUT >= 0.20 have median TRAIN
  precision 0.116): textbook regression to the mean;
- era check: the same rules give ~3.9x lift on 2018-19 vs ~3.5x on 2023.
  Drift is secondary.

**Fix: threshold on the one-sided Wilson LOWER CONFIDENCE BOUND (LCB) of train precision**
instead of the point estimate. At matched deployed precision (~0.20), LCB
selection catches 12.8% of all errors vs 8.2% for absolute thresholds: strictly
better ranking, and trained precision became roughly calibrated to test precision.

*Artifacts: methods/compare_models_by_HHsize_vs_pooled/ (rawstat_ vs unprefixed runs).*

**Calibration of floor definitions (2026-07-07, rebuilt frame, unearned rules):**
sweeping floors on raw trained precision overpromises even AFTER the LCB gate
removes the junk: among rules passing the 99% bound at 0.20, a raw floor of
0.40 delivers 0.33 union precision and a raw floor of 0.50 delivers 0.34 (the
highest raw values among survivors still belong disproportionately to
luck-inflated estimates). Floors on the LCB itself underpromise: an LCB floor
of 0.30 delivers 0.381, 0.40 delivers 0.509. The LCB is therefore the only
menu axis whose number reads "at least this". Side-by-side figure:
`presentation_figures/floor_definitions_educational.png`
(`methods/educational_floor_definitions_figure.R`).

## 2. Results of dropping {pre} r package in favor of rolling our own

> **Takeaway: about our pipeline.** Replacing the off-the-shelf `pre` R package with
> our own rule generator gave the same rule quality at a fraction of the cost: it
> mines all four typed frames in about the time `pre` took to mine one, and its peak
> memory drops from over 40 GB to a few GB. It also made analyses possible that `pre`
> simply could not fit: the pooled any-error model, 853k-rule comparisons, a
> regression test. Worth knowing: swapping the underlying tree engines barely moved
> precision (about 1 point). The real gains come from strict filtering and from
> scoring on any-error, not from the choice of algorithm.

Same rule quality, ~5x cheaper, ~3x smaller memory, and it unlocked analyses
pre() could not run:

- **Memory**: pre() peaked >40 GB on ONE frame (its internal lasso matrix,
  paid even when the lasso output is unused). v2 runs in a few GB; works on a
  16 GB laptop.
- **Compute**: one pre() frame ~40 min vs four v2 frames (incl. any-error
  scoring) in ~45 min.
- **Quality**: matched earned-frame comparison under identical LCB selection:
  pre: 68 rules, median hold-out 0.134; v2: 29 rules, 0.157. Parity at 1/5
  the trees.
- **Unlocked**: the any-error single model (pre's lasso matrix would be
  ~100+ GB), the other_error frame, 853k-rule head-to-heads, coverage-based +
  dominance dedup, checkpointed vocabularies, a 15-check regression test.
- What it did NOT buy: more signal. Best out-of-sample per-rule hold-out precision by
  frame: earned 0.31, underissuance 0.29, other_error 0.40, unearned 0.48.
- **Engine head-to-head (2026-07-05/06, identical pipeline, 1000 trees/rounds
  each, any-error frame)**: xgboost + ranger is the best pair: mean precision
  0.2216 at matched dollar recall and 54.8% dollar recall at the 0.20 floor,
  vs rpart + ranger 0.2157 / 53.1% and bagged rpart alone (pre's generator)
  0.2096 / 47.3% (reach capped at 94%). Both pairs beat all singles:
  vocabulary complementarity again. So pre's CART engine was competitive but
  not its pipeline's problem; the engines add ~+1pp precision / +7pp dollar
  recall, while stringent filtering and any-error scoring supply the larger gains.
  *Artifacts: methods/compare_engines_v2/ (engine_ and combo_ sweeps + summaries).*

*Artifacts: rule_mining_helpers.R, methods/test_rule_mining_helpers.R,
methods/design_drop_pre_architecture.md.*

## 3. Typed frames vs one any-error model (head-to-head)

> **Takeaway: about our pipeline.** Mining the four error-type datasets separately
> beats a single all-errors model, but only by about a percentage point, and one
> all-errors model gets ~95% of the way at a quarter of the cost. Because the two
> approaches find largely different rules, the best move is to mine both and pool
> them; at a fixed precision floor that can only add catches, and the precision cost
> is small. This held up on an independent test year.

Three arms are compared, all scored on ALL 2023 errors with identical machinery
and selection:
- **Typed**: rules mined from the four separate error-type frames, pooled.
- **Any-error**: rules from a single model whose target is *any* error.
- **Combined**: Typed and Any-error pooled together and de-duplicated.

- **Typed wins on precision, but barely**: mean precision at matched recall 0.177
  (Typed) vs 0.167 (Any-error), a ~1pp edge. The single Any-error model reaches ~95%
  of typed's performance at 1/4 the mining cost, and neither parent dominates on
  recall (Typed reaches more at loose floors, Any-error slightly more at strict
  floors).
- **The vocabularies complement**: Combined beats BOTH parents on recall at
  every FIXED filter floor (only ~7% cross-pool overlap). Best practice: mine
  both, pool, dedup: cheap on the v2 stack. Hold-out recall of all 2023 errors,
  all three arms:

  Large ensembles (1000/2500):

  | floor | typed | any-error | combined |
  |---|---|---|---|
  | 0.20 | 68.8% | 66.9% | 72.9% |
  | 0.30 | 29.7% | 29.1% | 36.0% |
  | 0.35 | 16.5% | 17.9% | 21.5% |

  (dollar recall at the 0.20 floor: typed 71.2% / any-error 69.7% / combined 75.3%.)

  Small ensembles (300/500):

  | floor | typed | any-error | combined |
  |---|---|---|---|
  | 0.20 | 62.7% | 59.0% | 66.0% |
  | 0.30 | 22.0% | 21.4% | 27.6% |
  | 0.35 | 10.9% | 12.9% | 15.4% |

  Precision cost at the same floors: ~0.7-2pp. CAVEAT: at MATCHED
  RECALL combined runs ~0.5-1pp below typed-only in both runs (at/near the
  noise band). So "combine" wins for a state operating at a fixed filter
  floor (the standard workflow) and roughly ties for a state targeting a
  precision level. The floor-level recall gain is near-guaranteed mechanically
  (adding a vocabulary can only grow the union); the measured question was the
  precision price, which is small. Evidence grade: SOLID as of 2026-07-06.
  The year-swap replication (train 2022+2023, test 2024) reproduced both the
  ordering and the magnitudes on a disjoint test year: at the 0.30 floor,
  typed 26.9% -> combined 35.6% recall (+8.7pp vs +6.3pp in the original); at
  0.20, +4.7pp (vs +4.1pp); matched-recall deltas again within the noise band.
  Artifacts: methods/compare_anyerror_vs_typed_v2/yearswap_train2223_test24/.
- Robust to ensemble size (300/500 vs 1000/2500 trees: same ordering).

*Artifacts: methods/compare_anyerror_vs_typed_v2/ (dollar- and counts-basis plots,
sweep + summary CSVs; small-ensemble run preserved as xgb300rf500_).*

## 4. Engine tuning: what matters and what doesn't

> **Takeaway: about our pipeline.** Most tuning knobs barely matter. The few that do:
> give the random forest a little signal to split on (mtry = 2, not 1), use slow,
> low-sample boosting, and grow trees to depth 4-5. More trees don't buy precision at
> a fixed filter setting; they buy a bigger *menu* of rules, which pays off only when
> you also filter more strictly (section 5).

19-config one-at-a-time grid (any-error frame, frontier = mean hold-out
precision at matched dollar recall):

- **ranger: mtry = 2 beats mtry = 1** (0.223 vs 0.214). The "pure randomness
  for diversity" premise was wrong. (Tested once, at 500 trees / 90% LCB;
  adopted and unchallenged since.) Tree count is a plateau, not a peak: 250
  trees 0.206, 500 trees 0.214, 1000 trees 0.213 (a 0.001 gap, within noise),
  2500 trees 0.210: more trees add inventory and reach, not matched-recall
  precision, AT A FIXED 90% z. Per §5 we nonetheless adopted 1000 trees: with
  stringent filtering (z = 2.326) the bigger pool keeps its reach without the
  precision dilution.
- **xgboost: slow eta, low subsample.** eta 0.02 (0.217) beats eta 0.1 (0.212);
  low subsample (0.15-0.30, e.g. 0.20 -> 0.218) beats high (0.60-0.80, e.g.
  0.75 -> 0.208), echoing the old rpart sampfrac finding; values within
  0.15-0.30 are statistically indistinguishable. Both results are independent
  of the filter setting; they are the production defaults (eta .02,
  subsample .20). (The low-vs-high subsample edge did NOT replicate on 2024;
  see §13. 0.20 stays as "as good as any," not proven best.)
- **Round count only looks like it matters at a loose filter.** At a fixed
  90% LCB, 100 rounds beat 1000 on the frontier (0.217 vs 0.198). But most of
  that gap is the selection-multiplicity dilution that §5 shows is
  correctable: at each pool's appropriate stringency (small @ 90%, big @ 99%)
  the two trace essentially the SAME hold-out frontier. What mining big buys
  is the MENU behind each operating point, ~2.6x the rules pass any floor in
  this experiment (~5x at production scale with both engines), each with a
  stiffer per-rule guarantee, not extra portfolio precision. Production:
  1000 rounds, "mine big, filter stringently."
- Depth 5 beats depth 3 clearly for ranger (0.203 -> 0.213) but only
  marginally for xgboost (0.210 -> 0.211); production uses depth 4. Inventory
  (shortlist size) and frontier quality often DISAGREE: e.g. subsample 0.75
  gives more rules but a worse frontier.

*Artifacts: methods/parameter_tuning_v2/v2_tuning_{ranger,xgboost}.png, summary CSVs,
v2_subsample_fine.*

## 5. "Mine big, filter stringently": the flexible LCB

> **Takeaway: about our pipeline.** Mining a big pool of rules and then filtering it
> hard lands on the same accuracy as mining a small pool and filtering it gently. The
> big pool's advantage isn't better numbers; it's a longer list of usable rules, so
> states have substitutes when they veto one on expert judgment. The strict filter
> (the 99% bound) is what keeps the big pool from drowning in lucky rules.

More mining extends recall reach but dilutes matched-recall precision via
selection multiplicity (more lucky rules clear any floor). The z-sweep showed
the dilution is mostly CORRECTABLE in order to keep the potential for greater recall:

- On the 1000-round pool, raising z (80%->99%) recovers precision cleanly and
  monotonically; on the 100-round pool z barely matters: the multiplicity
  signature.
- **1000 rounds @ z=2.33 lands on the same 0.20-floor operating point as 100
  rounds @ z=1.28 (55% recall @ 17% precision), with 2,026 vs 789 filtered
  rules behind it.** Framing: the two recipes trace the same union
  frontier. The big pool's gain is rule inventory (substitutes for
  expert-driven removal) and per-rule guarantee stringency, not portfolio
  precision or reach. (Figure: presentation_figures/
  mine_big_filter_stringently.png.)
- Residual gap (~1/3 of the dilution) is intrinsic marginal-rule quality; no
  z fixes it.

Production recipe adopted: xgb 1000 rounds / eta .02 / subsample .20, ranger
1000 trees / mtry 2, z = 2.326. Result: **11,036 filtered-in rules** (vs 834
under small ensembles at 90%), with better median hold-out quality per frame
(e.g. other_error 0.25 vs 0.197; unearned 896 rules at 0.314).

*Artifacts: methods/parameter_tuning_v2/v2_lcbz_sweep.png + v2_lcbz_summary.csv;
inclusion_rules_by_hh_size_v2/ (run1_small_ensembles_z90/ preserved).*

## 6. Frame-relative vs deployed (any-error) performance

> **Takeaway: about the data.** A rule you mined to catch one kind of error routinely
> flags cases that have some *other* error too, and in real review that still counts
> as a hit. So a rule's real-world precision runs about 2x its narrow, single-type
> precision. Always quote states the any-error number; the narrow one understates
> what they would actually see.

A rule mined for one error type flags cases whose OTHER errors count as wins
in deployment. Any-error precision runs ~2-2.7x the frame-relative number
(e.g. earned union at the 0.20 floor: 0.080 frame vs 0.178 any-error). All v2
outputs carry both views; quote the any-error numbers to states.

## 7. other_error: the largest, previously unmodeled category

> **Takeaway: about the data.** The biggest single category of SNAP errors is the
> "other" bucket (deductions, shelter, household composition), larger than any of the
> classic income-error types, and nobody had tried to model it. It turns out to have
> plenty of learnable structure. One caveat from the program side: many states treat
> these as small-dollar, low-priority errors, so "we can find them" is a completeness
> win, not a headline.

other_error (deductions, shelter, household composition; 2,007 of 4,460 total
2023 errors, more than any typed category) had never been mined. It produced
the single largest filtered-in block (1,700 rules, median hold-out 0.25).
Heterogeneous or not, it has learnable structure.

## 8. ESAP / elderly-disabled: feature suffices, and why

> **Takeaway: about the data.** Elderly and disabled households (ESAP = the Elderly
> Simplified Application Project population) are about half the caseload but are *not*
> more error-prone. What differs is the *mix* of their errors, mostly the
> easy-to-detect types, which is why our models catch far more of their errors (~19%
> vs ~8% at a 5% review budget). The hard, still-open problem is working households
> with volatile earned income. Practical upshot: this group did not need its own
> model; letting the ensemble see it as a feature was enough.

Decision: NO fourth stratum or separate model. The models carved the caseload
themselves:

- elderly/disabled HHs are 50.7% of caseload, 47.1% of error cases, 39.2% of
  error dollars: NOT more error-prone.
- Their error MIX is what differs: 63% other_error + 20% unearned (the two
  most detectable types) vs other households' 44% earned (the least
  detectable). Detection asymmetry is compositional.
- The unearned frame became a de facto elderly model on its own: 91.8% of its
  flags are elderly HHs; all 96 of its indicator-using rules REQUIRE
  elderly/disabled. The earned frame is the mirror image (82% non-elderly
  flags; its indicator rules require NOT-elderly).
- Union recall at a 5% review budget: 19.0% of elderly-HH errors vs 8.2% of
  other-HH errors (dollar recall 21.1% vs 9.0%); precision slightly HIGHER inside
  elderly flags (0.320 vs 0.252). At a 10% budget: 27.9% vs 19.2%, precision
  0.274 vs 0.240. (The old figures were at an unspecified, looser operating point;
  at the full LCB>=0.20 union the groups converge, so the edge is a
  tight-operating-point effect.)
- The real gap is non-elderly working households (earned-income volatility).
  This could lack of signal issue, but also is an area I'm continuing to explore. 

*Artifacts: methods/check_esap_coverage_v2.R (rerun anytime).*

## 9. States: a clean two-regime deployment rule

> **Takeaway: about our pipeline (with one portable caution).** When a state has
> enough of its own data (roughly 30+ rules clearing the bar), tuning rules on that
> state's data can pay off well; when it doesn't, tuning collapses to noise and you
> should just deploy the national rules unchanged. The portable part is the caution:
> at small sample sizes, tuning is pure winner's curse, so we enforce a hard support
> floor (n >= 30). *(Partly superseded: the current deployment recipe is the blended
> list in section 16.)*

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

Column definitions: each scheme is a QUALIFICATION bar (which threshold
variants of a rule are eligible at all, judged on state training data) plus a
SELECTION objective (which single qualifying variant the state deploys). Cells
show the deployed union's precision @ recall on the state's 2023 test year.

- **(a) LCB+LCBmax**: qualify: 90% Wilson lower confidence bound of the
  variant's train precision >= 0.20 (>= 5 cases flagged); select: the variant
  with the HIGHEST LCB (the most statistically defensible version, tends to
  pick tight, small-footprint variants).
- **(b) floor+$max**: qualify: variant flags >= 20 train cases at >= 0.20 raw
  precision (the simple, transparent criterion); select: the variant capturing
  the most error DOLLARS on train among qualifiers (the widest-reaching
  version that still clears the bar).
- **(c) hybrid**: qualify as in (a) (LCB-based, careful gate); select as in
  (b) (dollar-maximizing, aggressive pick).

No scheme dominates: (b) wins the largest state on recall; (c) dominates (b)
in Arizona (equal recall, +3.6pp precision) and is the precision-recall middle
ground in NC. The qualification bar drives the trade (LCB admits fewer
marginal-support rules -> higher precision, less reach); the selection
objective drives reach. Differences are a few points on small test samples,
within noise for a per-state ranking. DEFAULT (decided 2026-07-06): the hybrid
(c), QUALIFY_MODE = "lcb", dominating in AZ, middle ground in NC, and close
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

*(2026-07-10 update, partially superseded: re-run in the deployment setting
(train 2022-23, test 2024, review budgets; sections 14-16), per-state
re-filtering and re-tuning of the national pool did NOT beat deploying the
national ranking as-is for the median of 18 states; adaptation paid only
where the national list underperformed. The current deployment default is
the blended frozen list of section 16, with the state's own-pool list as
the fallback, judged by the state's internal validation. The support-floor
lesson here (n >= 30 at state scale) still stands and is baked into the
delivery builder.)*

National selection sent to states: up to 60 rules per frame by national train
LCB (earned admitted at a relaxed 0.15 floor; 186 rules total).

**THREE-WAY comparison (2026-07-06; train 2022+2024, test 2023):** adding
"mine on the state's own data" (raw >= 0.30 @ n >= 30) as option (c) beside
national-as-is and national-tuned. Where data is deep, own-mining dominates
recall by a wide margin at moderate precision (AZ: 86% of error dollars at
0.221; VA 81% at 0.138; NC 71% at 0.114; CT 70% at 0.181); in the thin states
it collapses to a handful of rules (LA and WA: 8 rules each). IMPORTANT
CAVEAT: the 2023 test year sits BETWEEN the training years. Temporal
interpolation flatters all options and own-mining most; the year-split
extrapolation checks (below) are the forward-deployment expectation.
Artifacts: methods/compare_state_options_v2/.

**Same-era NEIGHBOR TRANSFER, the thin-state recipe (2026-07-06, Louisiana):**
train on the state's fire-rate-similar neighbors (cosine on sqrt rule fire
rates; for LA: IN, OK, AL, NM, KY) using the SAME years, exclude the state
entirely, test on all of the state's rows. Result for LA: 913 rules; of the
386 firing in LA, median precision 0.33 (neighbor-train) -> 0.18 (LA), 48%
holding >= 0.20, union 0.141 @ 49% of LA errors (2.3x lift), versus LA-alone
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
state scale. The ladder-collapse post-filter matters more there.
(State-specific artifacts live outside the repo in custom_one_off/.)

*Artifacts: archive/state_rules_v2/ (per-state rule CSVs with national + tuned
thresholds side by side; state_union_summary.csv; LCB-criterion run preserved
in run1_lcb_criterion/).*

## 10. Data visibility: how much of a state's error population the public data can even show (2026-07-07)

> **Takeaway: about the data (and it matters a lot).** The public QC file does not
> show a state its whole error population: it excludes ineligible cases entirely, so
> a state sees only part of its own errors, from 43% (New Jersey) to 91% (Georgia). Any
> rule mined on public data is therefore blind to a large slice of reality. States
> below roughly 60% visibility should treat public/national rules as a *supplement*
> and run the pipeline on their own internal data.

Two frame changes preceded the measurement:

- **Multi-element cases restored**: every result before 2026-07-07 was
  mined on a single-element frame, ~69% of true errors (multi-element cases,
  31% of errors, excluded). The rebuild keeps them (second_element_i tracks
  them; NOT a mining feature: states report second elements too
  inconsistently). The frame now saves from the script directly, so the .rds
  always matches the munging code.
- **Deduction-NA drops**: states like WA/MS/MN leave optional deduction fields
  unrecorded in blocks; those rows are now zero-filled (ded_fields_imputed
  flag) instead of dropped. Recovered ~16% of WA's caseload.
- **BENMAX filter: exonerated** (drops zero rows in the real pipeline; an
  earlier circumstantial attribution to it was wrong).

Post-fix VISIBILITY (frame errors / [raw over-threshold errors + ineligible
exclusions], FY22-24): national 71%; WA/VA/LA now 78-81% (Georgia highest, at 91%). The floor is
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
rules (~3x inventory, old set 93% preserved, higher LCB-floor reach, and the
finding that the new rules are NOT multi-element specialists) are documented
in `methods/effects_of_munging_options.md`.

## 11. Household-size stratification: split, but split coarsely

> **Takeaway: about our pipeline.** Splitting the caseload by household size
> (1 / 2-3 / 4+) and mining each group separately reliably helps, or at worst never
> hurts, so it is our default. Splitting *finer* (a 5-way split) does not help and
> costs more compute; past a point, smaller groups just starve each rule of the cases
> it needs to clear the bar.

Established under the pre-era methodology (June 2026, earned income, greedy
nets) and the reason the pipeline uses 1 / 2-3 / 4+:

- **1/2-3/4+ stratification: mean precision 0.148 vs pooled (no split) 0.101**
  at matched recall: a ~47% relative precision gain from splitting at all;
- the coarse 3-way grouping also beat the standard 5-way 1/2/3/4/5+ (0.127)
  and 1/2/3-4/5+ (0.139): finer strata thin the training data faster than
  they add homogeneity;
- intuition: even dollar-scaled features (income/benefit relative to HH size) mean
  different things at different HH sizes; stratifying lets thresholds differ,
  while over-splitting starves rule support.

**v2-stack confirmation (2026-07-06).** With
production engines (mtry=2 ensembles, HH size available as a feature) and strict
LCB: pooled 0.2256 mean precision vs 1/2-3/4+ 0.2216 vs 5-way 0.2142. The
pre-era +47% gap does NOT replicate. Like the ESAP finding, ensembles using restricted mtry (set to 2)
capture most of what stratification provided when the stratifier is a feature.
The 3-way split still wins where it matters operationally: **reach** (54.8% vs
48.4% dollar recall at the 0.20 floor) and **filtered rule inventory** (4,279 vs
809 rules: per-stratum filtering gives rules the within-size support to
clear the stiff bound). The 5-way split loses either way. 

*Artifacts: methods/compare_models_by_HHsize_vs_pooled/strata_earn_inc_scheme_summary.csv
(pre-era); methods/compare_hh_strata_v2/ (v2 confirmation).*

**Year-swap re-test (2026-07-13, train 2022+2023, test 2024, PARTIAL
replication):** the 2023 verdict "pooling matches the split's precision"
did not hold on 2024. There the 1/2-3/4+ split wins mean precision at
matched recall (0.302 vs 0.262 pooled) while pooling wins reach at the
0.20 floor (0.844 vs 0.794 dollar recall). Consistent across both years:
the coarse split never loses, so it stays the default. NOT replicated:
"5-way is worse". On 2024 the 5-way ties the 3-way (0.304 vs 0.302) at
~1.6x the compute; the claim softens to "no better, costlier."
*Artifacts: methods/compare_hh_strata_v2/yearswap_train2223_test24/
(strata_summary.csv, strata_sweeps.png; methods/run_strata_yearswap.R).*

## 12. Cross-state transfer vs like-for-like national baselines (2026-07-09)

> **Takeaway: about our pipeline (now superseded).** In a same-year test, pools of a
> few "similar" states looked competitive with the full national pool at moderate
> review budgets. That advantage did *not* survive a proper future-year test (section
> 14), so it is recorded here for the trail but is no longer the guidance. The durable
> point: judge deployment at realistic review budgets (5-10% of caseload), not at
> abstract filter floors.

Leave-one-state-out benchmark: for 12 target states, any-error rules were
mined on donor pools that NEVER saw the target (2022-24 both sides), then
scored on the target under review-capacity budgets (rules added in
descending train-LCB order until the budget fills). Donor pools: top-5
neighbors under four similarity definitions: fire-rate cosine (sqrt),
inverse-frequency-weighted cosine (IDF), naive-Bayes/KL over rule-firing
profiles (NB), QC-derived policy vectors, plus a leave-one-state-out
NATIONAL pool (all 48 other states, same any-error recipe: the like-for-like
version of the national baseline).

Median delivered precision / share of error dollars across the 12 states:

| budget | natl as-is* | natl LOO | fire | IDF | NB | policy |
|---|---|---|---|---|---|---|
| 5% of caseload | 0.336 / 16% | 0.309 / 12% | 0.264 / 12% | 0.273 / 11% | 0.270 / 12% | 0.246 / 10% |
| 10% of caseload | 0.307 / 30% | 0.245 / 22% | 0.270 / 24% | 0.240 / 22% | 0.278 / 24% | 0.236 / 21% |

*natl as-is = the production 5-frame shortlist, trained on 2022+2024
INCLUDING each target's own cases. Its edge over natl LOO conflates two
things: the in-sample advantage AND the richer 5-frame recipe (LOO pools
are any-error-only). So the as-is-vs-LOO gap (0.03 at 5%, 0.06 at 10%) is
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
  (0.346/16% at 5%). The fixed-floor failure was a floor artifact.
- **Era-matched similarity is load-bearing and definitions converge**:
  LA's 2022-24 neighbor lists under fire/NB/policy agree closely with each
  other and with the donor pool that worked in the July transfer, and all
  differ sharply from the 2017-19 lists.

Deployment guidance *(SUPERSEDED by section 14 on 2026-07-10: re-tested with
a true temporal split, the 10%-budget similarity-pool advantage did not
survive. The national pool leads at both budgets on 2024, and with an
unseen test year the in-sample-flattering concern also dissolves. Kept as
originally written for the record)*: the production national shortlist
remains the best single list at small budgets, but numbers quoted to a
state from in-sample national training overstate held-out performance by up
to the as-is-vs-LOO gap; at moderate budgets (~10%), similarity-picked
donor pools (fire or NB) are competitive with any national option and are
the right choice where a state's own data must stay out of training.

*Artifacts: methods/state_similarity_v2/transfer_benchmark/ (benchmark +
budgeted_menu_results.csv); methods/state_similarity_v2/similarity_*_2022_2024.csv
and _2017_2019.csv; methods/state_nb_similarity_v2.R; methods/neighbor_transfer_benchmark_v2.R;
methods/budgeted_transfer_menu_v2.R; overnight_nb_loo_run.log.*

## 13. Pre-registered year-swap replication of the model-selection studies (2026-07-09)

> **Takeaway: about our pipeline (and the check we ran on it).** Every modeling
> choice above had been judged on one test year (2023). We wrote our predictions down
> in advance and re-ran the four big ones on a fresh year (2024): three held up, and
> one ("low subsampling helps") did not and was retired. The value here is the
> discipline: pre-committing to predictions is what lets you tell a real effect from
> a lucky one.

Every model-selection decision (engines, subsample, filter stringency,
ensemble size) had been judged on the same held-out year, 2023, a year that
sits BETWEEN the training years, so the selection procedure itself risked
being tuned to one interpolated year (methods/pipeline_critique_2026-07-09.md, V2).
Guard: the four decisive selection claims were re-run with the year roles
swapped (train 2022+2023, test 2024, a year that never influenced any
design decision) with expectations and falsification criteria WRITTEN DOWN
BEFORE the run (`methods/yearswap_preregistration_2026-07-09.md`). Levels were
expected to shift (rebuilt frame, different year); orderings and margins
were what the original decisions rested on, so orderings and margins were
what was pre-registered.

**Claim 1: engine pairing.** The claim: mining rules with xgboost and a
constrained random forest (ranger, mtry 2) TOGETHER yields more recall at
the 0.20 filter floor than either engine alone or a bagged-CART + ranger
pair, at a small precision cost, because the two engines contribute
complementary rule vocabularies. REPLICATED in ordering: the pair again
leads recall at the floor (0.794 vs 0.773 for xgboost alone, 0.757 ranger,
0.724 bagged CART) at a small precision cost (0.185 vs 0.189-0.202). The
pre-registered margin (>= 3pp over the best single engine) came in at
2.1pp. The pairing advantage is real but thinner than the 2023-judged
number, a direct measurement of how much adaptive selection flattered the
original margin.

**Claim 2: low subsampling beats high.** The claim: showing each boosted
tree only 15-30% of the training data produces better rules than showing it
60-80%, so subsample belongs at 0.20. PREDICTION FAILED; finding RETIRED.
On 2024 the band structure vanishes: precision at the 0.20 floor spans
0.181-0.186 across ALL nine settings from 0.15 to 0.80: one flat plateau,
with the predicted ordering (worst low-band member >= best high-band
member) failing, 0.182 < 0.186. Per the pre-registered decision rule, "low
subsample beats high" is no longer quotable; the surviving claim is only
that subsample barely matters in this range. Production stays at 0.20 (it
leads mean precision, 0.303, and nothing beats it meaningfully).

**Claim 3: stringent filtering delivers more precision.** The claim: among
rules mined by big ensembles, raising the one-sided Wilson lower-bound
stringency from 80% to 99% (z = 0.84 -> 2.33) monotonically raises the
delivered (held-out) precision of the surviving union while costing recall
: the mechanism that makes "mine big" safe. REPLICATED: precision at the
0.20 floor again rises monotonically in z (0.169 / 0.175 / 0.179 / 0.188)
while recall falls (0.873 -> 0.776); the 99%-vs-80% advantage is 0.019
against a pre-registered >= 0.020 prediction and a 0.010 falsification
line.

**Claim 4: big ensembles widen the menu, not the frontier.** The claim:
1000-round/1000-tree mining does not trace a better precision-recall
frontier than 100-round mining, but produces several times more distinct
filtered rules (the menu states need for vetoes and substitutes).
REPLICATED: inventory ratio 7.3-7.9x (26.6-29.1k vs 3.6-3.7k rules), with
the big pool's precision deficit at matched stringency only 0.020-0.022 and
+7pp recall at the floor.

Net: three of the four selection findings replicate on a year that never
judged any design decision, and the procedure produced one
retraction (Claim 2): evidence the selection methodology was not
2023-luck, and that the pre-registration has teeth. No production setting
changes.

*Artifacts: methods/yearswap_preregistration_2026-07-09.md (predictions + results);
methods/compare_engines_v2/yearswap_train2223_test24/;
methods/parameter_tuning_v2/yearswap_train2223_test24/;
methods/run_selection_yearswap.R.*

## 14. Time-shifted deployment benchmark: own-state vs NB transfer vs national on 2024 (2026-07-10)

> **Takeaway: about our pipeline (with one portable caution).** Tested the way a
> state would actually face it (rules built on past years, scored on a future year),
> the plain national rule list is the best default: the highest precision at both
> budgets among the lists a state can actually deploy, and never a disaster. Mining on
> a state's *own* data has the biggest upside but is high-variance and can fail below
> the random-review base rate (Washington did). Portable caution: don't assume a
> state's own rules beat the national ones; make the state confirm it on their own
> held-out year. ("NB transfer" = a pool of a few statistically similar states, picked
> by a naive-Bayes similarity measure.)

Section 12's transfer benchmark scored every pool on the same era it was
mined from, so its verdicts could lean on same-era correlation
(methods/pipeline_critique_2026-07-09.md, V6). Guard: the three deployable
options were re-run as a state would actually face them: rules mined on
2022+2023 only, scored on the target state's 2024 cases only, budgets
filled in descending train-LCB order. Approaches: the target's OWN 2022-23
data (own_state), a 5-neighbor donor pool picked by 2022-23 NB/KL
similarity (transfer_nb), the 48-other-state national pool
(national_loo), and the ALL-state national pool including the target
(national_all, usable here because the test year is unseen, and the
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
  is no in-sample flattering to correct. The same-era as-is-vs-LOO gap
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
  2024 (BELOW the state's 8.5% base rate, i.e. worse than random review),
  and Louisiana's 0.161 trails both pooled options. The 10% LOO-minus-own
  precision gap spans -0.103 (Mississippi) to +0.173 (Washington).
  (Bullet judged against national_loo; the own-vs-national deployment
  charts use national_all, same story.)
- **NB transfer is insurance where own-state fails, not a first choice.**
  Where own_state collapses it holds up (Washington 0.247, Louisiana
  0.207 at 10%) and it wins outright only in Connecticut (0.500 at 5%);
  everywhere else it is second or third.

Deployment guidance: quote states the national_all numbers (national list,
time-shifted test) as the default; offer own-state mining only where
the state's own held-out year confirms it (it cannot be assumed: a
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

> **Takeaway: about our pipeline.** A state can't wait for the test year; it needs a
> fixed list in hand. Freezing the national list and sizing it against the state's own
> caseload (with a buffer so reviewers never run dry) costs almost nothing versus an
> idealized after-the-fact list: under a point of precision. Each list personalizes
> itself through the state's own case mix, and about a third of every state's list is
> unique to it.

The deployment benchmark's budget fill (section 14) chooses rules against
the test year's realized caseload. A state, however, needs a list it can
hold in advance. Deliverable design (settled after one iteration): ONE
ranked list per state, the national pool (mined on all states'
2022+2023) budget-filled against the state's own 2022-23 CASELOAD
COVARIATES only, to the target sizing (the core) and then onward to 3x the
target (the buffer). The state walks the list in rank order, activating
each rule while its flagged total fits capacity, outcome-free, and it
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
validate internally on their own newer data before relying on it. The
public files see only 43-91% of error cases (section 10), and this
experiment validates one year ahead only.

*Artifacts:
methods/frozen_list_experiment_v2.R ->
methods/state_similarity_v2/transfer_benchmark_train2223_test24/frozen_list_results.csv
+ frozen_lists/frozen_list_<state>.csv (the handable lists);
methods/contributing_rule_overlap_v2.R -> contributing_rules_by_state.csv,
contributing_overlap_jaccard_budget10.csv, contributing_overlap_summary.csv;
methods/count_contributing_rules_v2.R -> contributing_rules_summary.csv.*

## 16. Blending state and national rules on one confidence scale (2026-07-10)

> **Takeaway: about our pipeline (the current default deliverable).** Put each
> state's own mined rules and the national rules on one comparable confidence scale (the
> 99% bound) and let them interleave. This "blend" is the recipe we now ship: better
> at a 5% budget, about even at 10% (0.262 vs 0.270 precision), and no case-by-case
> decision to defend. Its one blind spot (a national rule's bound says nothing about
> whether it *transfers* to a given state) is why we keep the state's own-rules list
> as a fallback where their internal validation shows the blend underperforming.

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
  about transfer to this state; the state rule's LCB prices its
  noise. The scale therefore over-trusts national rules exactly where
  the national mix fits worst: New Jersey's own rules never enter (their
  small-sample bounds cannot beat 45k tight national bounds) and the
  blend under-delivers there (0.161 at 10% vs 0.230 for NJ's own list).
  Relaxing to 98% does not rescue this.

Deployment guidance: the BLEND is the default shipped recipe (better at
5%, no worse at 10%, no regime decision to defend); the own-pool list is
kept as a FALLBACK, activated only where the state's own internal
validation shows the blend under-performing. In low-visibility states
this is the only arbiter that settles it. New Jersey's public files show 43% of
its error cases (section 10), so the public data cannot establish which
option truly performs better there; the state's internal check decides.

*Artifacts: methods/blended_frozen_lists_v2.R ->
methods/state_similarity_v2/transfer_benchmark_train2223_test24/blended_frozen_results.csv;
blended_frozen_run.log (same folder); comparison inputs:
frozen_list_results.csv, frozen_own_list_results.csv.*

## 17. Typed-frame delivery vocabulary: retired after three rescue attempts (2026-07-15/16)

> **Takeaway: about our pipeline.** Adding the four typed datasets to the delivery
> pool tripled the candidate rules but *lowered* delivered precision, and three
> attempts to rescue it failed. The reason is general enough to remember: when you must
> pick only 20-50 rules to fit a review budget, a bigger pool mostly adds small-sample,
> lucky-looking rules that crowd out the genuinely precise ones. The filter-floor advantage of
> pooling (section 3) is real but does not survive a tight budget.

Mining the delivery pools on the five frames (four typed + any_error) tripled
the candidate vocabulary (48,429 -> 159,245 national rules on 2022-24) and
LOST budget-filled precision on the train-2022-23/test-2024 benchmark:
median any-error precision 0.306 vs 0.324 at the 5% budget (0.277 vs 0.262
at 10%; dollars 13.9% vs 14.6% and 23.7% vs 24.2%). Three rescue attempts
failed to put it at-or-above the any-error baseline at the 5% budget:

- Stringency: no fixed z in {2.326, 2.576, 2.74, 3.09} x floors {30, 50,
  100} closed the gap (best five-frame 0.318 vs 0.335 for the best
  any-error configuration in the same sweep).
- Near-duplicate collapse (Jaccard 0.95, keep max support): lifted the
  five-frame 5% median 0.306 -> 0.319, still short of 0.324. The deployed-
  rule autopsy CONFIRMED the mechanism: uncollapsed, the five-frame walk
  deploys median-support-59 rules with 0.39 median train->2024 precision
  deflation vs support-77 / 0.23 for the any-error pool; collapse
  normalizes both (support 81, deflation 0.23).
- Shrinkage ranking (section 18) did not help either arm.

The typed+pooled union's filter-floor advantage (section 3) is real but does
not survive capacity-constrained selection: forced to CHOOSE ~20-50 rules on
a noisy statistic, the enlarged pool's extra small-support, high-raw-
precision candidates displace genuinely precise rules at the top. Ten five-frame lists
were briefly published (2.1.x) and replaced in v2.2.0.

*Artifacts: methods/state_similarity_v2/transfer_benchmark_train2223_test24/
blended_frozen_results_5frames.csv, stringency_vocabulary_sweep.csv,
neardup_collapse_sweep.csv (incl. the autopsy columns);
methods/design_selection_layers_v3.md.*

## 18. Shrinkage (empirical-Bayes) ranking: refuted on two eras (2026-07-16/17)

> **Takeaway: about our pipeline (with a portable statistical reason).** Ranking rules
> by a smoothed "posterior mean" precision did worse than our lower-bound ranking on
> two separate eras. The reason travels: filling a small review budget is a decision
> about the *top* of the list, and posterior-mean ranking floods the top with big,
> only-slightly-above-average rules, while a lower-bound statistic penalizes exactly
> the small-sample noise that piles up there.

Ranking rules by a beta-binomial posterior mean (prior fit per stratum)
instead of the Wilson lower bound degraded the production pool's 5%-budget
median from 0.324 to 0.259 on the 2024 benchmark (posterior 5% quantile:
0.298, tracks the bound, slightly worse). A repaired variant for the era
validation (prior fit on near-duplicate family representatives only,
Jaccard 0.95, max-support representative) was the worst ordering arm on
2019 as well: 0.201 at the 5% budget vs 0.219 for the z = 2.326 bound
(0.173 vs 0.221 at 10%). Interpretation that survives both eras: budget
fill is a tail decision; posterior-mean ordering floods the top with large-
support, mildly-above-average rules, while quantile-type statistics
penalize exactly the noise that concentrates there.

*Artifacts: methods/state_similarity_v2/transfer_benchmark_train2223_test24/
estimation_admission_sweep.csv;
methods/state_similarity_v2/era_validation_train1718_test19/
era_validation_results.csv (ordering comparison, famEB arm).*

## 19. Which rules to keep: a false-discovery-rate test plus a minimum-support floor (2026-07-16/17)

> **Takeaway: about our pipeline (with a portable statistical lesson).** Before we
> rank rules, we decide which ones to keep. A false-discovery-rate test
> (Benjamini-Hochberg at 10%) plus the n >= 30 support floor matched our old
> hand-tuned filter at the 5% budget on two eras, was never worse, and kept a smaller
> pool, so it is now the default. The two checks do different jobs and you need both:
> the test limits how many kept rules are flukes (whose true precision is no better
> than the base error rate), and the floor keeps out rules whose precision is measured
> from too few cases to trust. Drop the floor and those small-sample rules get
> deployed and underperform on the test year.

On raw unfiltered vocabularies (144,533 national candidates on 2022-23;
145,313 on 2017-18), keeping rules by Benjamini-Hochberg against the stratum base
rate PLUS the n >= 30 floor ("fdr10f") matched the hand-tuned production
filter (n >= 30, raw >= 0.05, raw > base) exactly at the 5% budget on both
eras and was never worse:

- 2024 test: fdr10f 0.335 / prod 0.335 at 5%; 0.275 / 0.262 at 10%, with a ~40%
  smaller kept pool (median 54,261 vs 93,869 rules).
- 2019 test: identical medians at both budgets (0.219 at 5%, 0.221 at 10%).

FLOORLESS BH was refuted on 2024: 0.284 (alpha .10) / 0.293 (alpha .05) at
the 5% budget, the same small-support collapse as the state-scale lesson
(section 13) and the displacement autopsy (section 17). The two checks answer
different questions. The false-discovery-rate test controls how many kept rules
are flukes, meaning rules whose true precision is no better than the base rate.
The support floor is a separate guard on estimation quality: a rule can be
genuinely above the base rate yet have a precision estimate too noisy to rank on
when it comes from very few cases. Dropping the floor let those small-sample rules
through, and they underperformed a year later, so this is a reliability failure,
not a failure of the fluke test (which was still running). The self-scaling worked
as intended: at alpha .10 the gate kept 54k of 145k nationally but only 1,336 of
67k for Michigan and 96 of 62k for Texas at alpha .05, with no hand-set constant
involved. Caveat: the first FDR audition (estimation_admission_sweep.csv) was
invalid because it applied BH on already-filtered pools, a no-op; only the
raw-vocabulary runs test this filtering.

*Artifacts: methods/state_similarity_v2/transfer_benchmark_train2223_test24/
fdr_admission_audition.csv;
methods/state_similarity_v2/era_validation_train1718_test19/
era_validation_results.csv (admission comparison);
methods/fdr_raw_vocabulary_mine_v2.R (raw caches regenerable, gitignored).*

## 20. Ordering stringency: z = 2.326 vindicated across eras; the 2024 bump did not replicate (2026-07-17)

> **Takeaway: about our pipeline.** A 2024 sweep hinted that filtering even more
> strictly than our 99% bound would help; pre-registered on a separate era, that hint
> did not replicate, so we kept z = 2.326. A useful reminder that a single year can
> whisper a false signal; the second era is what settled it.

The 2024 stringency sweep suggested raising z helps (orig pool: 0.335 at
z = 2.576 vs 0.324 at z = 2.326 at the 5% budget). Pre-registered on the
2017-19 era (expectation E2), the direction did NOT replicate: z = 2.576
0.216 vs z = 2.326 0.219 at 5% (z = 3.09: 0.223 at 5% but 0.200 at 10%).
No fixed z dominated 2.326 at both budgets on 2019. The competition-scaled
formula z(N) = qnorm(1 - 0.01 * 48429 / N) landed within 1pp of the best
fixed z at both budgets (E3), i.e. safe parity, no gain. Conclusion:
z = 2.326 stays; the "under-stringent" hint was era noise.

*Artifacts: methods/state_similarity_v2/era_validation_train1718_test19/
era_validation_results.csv (ordering comparison);
methods/preregistration_era_validation_2026-07.md (E2, E3);
methods/state_similarity_v2/transfer_benchmark_train2223_test24/
stringency_vocabulary_sweep.csv (the 2024 side).*

## 21. Dollar-yield ranking: direction consistent, magnitude era-unstable; not adopted (2026-07-16/17)

> **Takeaway: about the data (but not adopted).** A rule's average error *dollars per
> flagged case* carries over from one year to the next more reliably than its precision
> does; error size seems anchored to observable case traits. Ranking by dollars beat
> ranking by precision on dollar recall in 2024, but the gain shrank on the 2017-19
> replication and missed our pre-set bar, so we did not adopt it. Recorded as a real
> but era-unstable direction worth revisiting.

Groundwork: per-rule error dollars per flagged case persist train->test
MORE strongly than precision in every support band (train 2022-23 pools
scored on 2024; Spearman 0.560 / 0.699 / 0.789 / 0.677 for support bands
30-60 / 61-120 / 121-300 / 300+, vs 0.498 / 0.634 / 0.708 / 0.672 for
precision; 169,402 rule-state pairs). Error magnitude is anchored to
observable case characteristics. Ranking by dollars per flagged case (dpf)
then beat the precision bound on dollars at the 10% budget by +3.5pp on
2024 (27.8% vs 24.2%, precision 0.255 vs 0.262) but by only +1.0pp on the
2019 pre-registered replication (22.7% vs 21.6%, precision 0.191 vs
0.221), under the pre-set >= 2pp bar (E4). The log-scale lower-bound
variant (dpflb) beat the baseline on BOTH metrics at 10% on 2024 (0.278 /
25.0%) but not on 2019. Not adopted; recorded as a real but era-unstable
direction. A structure-anchored dollar statistic (size credited only as
far as flagged cases' benefit levels justify) is the untested follow-up.

*Artifacts: methods/state_similarity_v2/transfer_benchmark_train2223_test24/
dollaryield_audition.csv, dollar_persistence.csv (regenerable, gitignored;
methods/dollar_persistence_check_v2.R);
methods/state_similarity_v2/era_validation_train1718_test19/
era_validation_results.csv (dollar comparison).*

## 22. The winner's curse at the top, demonstrated directly (2026-07-17)

> **Takeaway: about the data (a clean demonstration).** Using the same data to both
> choose and rank rules inflates how good the top of the list looks. We isolated this
> by ranking one half of the data on rules mined from the *other* half: the clean
> ranking beat the self-scored one by ~1.6 points of precision at a 5% budget, and the
> gap was concentrated in the very top rules, exactly where a tight review budget
> lives. It is the winner's curse of section 1, shown directly at the point of the
> list that matters most.

Equal-footing cross-fit on 2017-18 (identical half-mined vocabulary,
identical admitted set of 66,540 rules): ordering by the UNTOUCHED half's
Wilson bound beats ordering by the MINING half's bound by +1.6pp median
precision at the 5% budget on 2019 (0.216 vs 0.200): the selection bias
of ranking on in-sample estimates, isolated from every other factor. At
the 10% budget the arms are within noise (0.192 vs 0.200 precision, 21.3%
vs 20.0% dollars), consistent with the curse concentrating in the extreme
tail. Caveat: the pre-registered cross-fit arm (E6) initially FAILED
because it confounded selection-free ordering with a half-sized mining
vocabulary; this equal-footing rerun is the diagnosis required by the
pre-registration's own decision rule.

*Artifacts: methods/state_similarity_v2/era_validation_train1718_test19/
era_xfit_diagnosis.csv, era_validation_results.csv (xfit comparison);
methods/era_xfit_mine_v2.R, methods/era_xfit_diagnosis_v2.R.*

## 23. Exclusion rules: cutting a review pile safely

> **Takeaway: about our pipeline.** The exclusion pipeline is the inclusion pipeline
> run in reverse: the same tree ensembles, but it keeps rules that identify
> very-likely-error-free cases (scored on the clean rate with a 95% lower bound) so a
> state can drop low-risk cases from an existing review pile. On a held-out year it
> traces a workload-cut vs error-dollar-retention curve, e.g. dropping the safest ~17%
> of the pile keeps ~96% of its error dollars. It is validated less deeply than the
> inclusion list: a single hold-out year (2023), with no multi-era or multi-state
> deployment test yet.

Two things differ from the inclusion pipeline:

- Rules are kept by a 95% Wilson lower bound on the *clean* rate (the share of flagged
  cases with no error), with a stiffer support floor (at least 25 training cases;
  exclusions warrant more support than inclusions).
- A *relative* safety standard: an excluded pocket must carry at most 1/5 of its
  stratum's base error rate, i.e. excluded cases are at least 5x safer than the pile
  average. Base error rates run about 8/15/20% by household size, so a relative bar is
  what makes exclusion meaningful in every stratum.

Held-out (2023) operating points from the clean-rate sweep (share of the pile dropped
/ share of error dollars kept): 10% / 98%, 17% / 96%, 37% / 85%, 58% / 69%.

Per-state adaptation exists (`EXCL_optimize_single_`, `EXCL_optimize_set_`), parallel
to the inclusion gridsearch, but has not been through the future-year, 18-state
validation the inclusion blended list has (sections 14-16, 20).

*Artifacts: exclusion_rules_by_hh_size_v2/ (exclusion_rules_all.csv,
exclusion_rules_highclean.csv, exclusion_lcb_sweep.csv/png); driver
EXCL_find_exclusion_rules_by_hh_size_v2.R.*

## 24. Munging row exclusions: tested by relaxing them, and kept (2026-07-29)

> **Takeaway: about the data.** We tested whether the munging script is throwing
> away usable rows, by re-running it with its row exclusions relaxed. It is not.
> Relaxing the one consequential filter adds 19,095 rows carrying 12,782 apparent
> errors, and both are artefacts of the same inconsistency that got those rows
> excluded: their error label is derived from a benefit discrepancy that the case's
> own reported error amount contradicts, and the pre-QC variable restoration fails
> on them (the recomputed benefit misses its target by a median $51 against $0 on
> the rows that pass). FY2020 and FY2021 are excluded by decision, not by
> measurement: the data is poor and misleading and state practices were
> qualitatively different. The useful by-product is a guarantee: on the six years
> we use, the filter is additive-only, reproducing the production frame's rows and
> errors exactly, year by year.

The modelling frame `reg_model_data.rds` is built by
`1_data_munging_and_raw_variable_reconstruction_for_using_public_qc_data.R`, which
drops rows in four places. The question asked here was whether any of that is
costing us usable data. Method: re-run the munging script with the exclusions
relaxed, write the result to a separate file, and compare. The script itself was
not edited or forked; the runner reads its text, applies named substitutions,
prints each one, and evaluates the result, so what changed is explicit. Both
`saveRDS` targets were redirected and the production frame's checksum was verified
unchanged afterwards.

This is a frame-composition check, not a model comparison. There is no train/test
split and no rule mining in it; the rule-level consequence is in the last
subsection.

**The four exclusions, and what was done with each.**

| # | exclusion | in the script | action here |
|---|---|---|---|
| 1 | `exclude_2020_2021 <- TRUE`: skip the FY2020 and FY2021 data files | line 14 | relaxed in the first run, then reinstated by decision (see below) |
| 2 | drop Alaska, Hawaii, Guam, Virgin Islands | line 145 | kept |
| 3 | keep only rows where `abs(absbendiff - AMTERR) <= 5` | line 157 | relaxed: recorded as an `amterr_reconciles` flag instead of dropping |
| 4 | keep only rows where `BENMAX == rawbenmax` | line 367 | kept (measured separately) |

Terms used below. `FSBEN` and `RAWBEN` are the two benefit figures the public file
carries per case; `absbendiff` is `abs(RAWBEN - FSBEN)`, and the pipeline calls a
case an error when that difference exceeds the fiscal year's threshold
(`over_threshold != 0`). `AMTERR` is the file's separately reported amount of
benefit in error, so the file states the same quantity twice by two routes.
`rawben_recreated` is the benefit the script recomputes from the restored pre-QC
fields, and `BENMAX` is the file's own maximum-allotment field against which the
script checks its `additional_data/max_allotments.csv` lookup (`rawbenmax`).

**Exclusion 2 was kept on validity grounds, not measured.** The max-allotment and
standard-deduction lookups in `additional_data/` are keyed by year and household
size only, i.e. they hold the 48-state values. Alaska and Hawaii have different
allotments, so keeping those rows would give them a wrong maximum benefit and
therefore a wrong `rawben_rel_max`. Adding rows that carry broken features is not
the same as keeping data.

**Frame totals.** Relaxing exclusions 1 and 3 (and zero-filling NA `RENT`/`UTIL`
rather than dropping them, which turned out to affect 0 rows) produced:

| frame | rows | errors | error dollars | states |
|---|---|---|---|---|
| production (`reg_model_data.rds`) | 237,391 | 24,334 | $3,684,635 | 49 |
| minimal-exclusion rebuild | 305,954 | 42,102 | $4,667,571 | 49 |

The difference decomposes exactly, with no residual:

| | rows | errors |
|---|---|---|
| production frame | 237,391 | 24,334 |
| + FY2020 rows that pass the AMTERR filter | +49,468 | +4,986 |
| + rows the AMTERR filter had dropped (all years) | +19,095 | +12,782 |
| = minimal-exclusion rebuild | 305,954 | 42,102 |

**The AMTERR filter is additive-only.** On the six years both frames contain
(FY2017-19, FY2022-24), the subset of the rebuild that passes the filter
reproduces the production frame exactly: 0 row mismatches and 0 error mismatches
across all six years, year by year. So the filter does not alter any row it keeps.
That is worth having on record independently of the verdict: it means we know
precisely what the filter removes and that it perturbs nothing else. It also means
every one of the 12,782 extra errors sits on a row the filter excluded.

**The excluded rows are not usable, for two independent reasons.** Restricting to
FY2022-24 (126,176 rows in the rebuild), where the delivery lists are built:

| FY2022-24 | rows | errors | error rate | error dollars |
|---|---|---|---|---|
| passes the AMTERR filter | 118,263 | 13,288 | 11.2% | $2,334,189 |
| excluded by it | 7,913 | 5,270 | 66.6% | $174,290 |

*First, the error label on the excluded rows is circular.* These are by
construction the rows where `absbendiff` and `AMTERR` disagree by more than $5. On
4,639 of the 7,913 (59%) `AMTERR` is exactly 0, i.e. the file reports no error,
while `absbendiff` on the same rows has a median of $93 (mean $137). Since the
pipeline's error test is `absbendiff` against the threshold, the disagreement that
excluded these rows is the same thing that makes them test positive. The 66.6%
error rate against 11.2% in the rest of the frame is therefore a restatement of
the inconsistency, not a property of the households, and the file's own error
amount says most of them are not errors at all. Nothing in the data resolves which
of the two statements is right.

*Second, the pre-QC restoration does not converge on them.* Measured within the
single rebuild run, comparing `abs(RAWBEN - rawben_recreated)` between the two
groups:

| FY2022-24 | rows | median residual | within $5 | off by more than $50 |
|---|---|---|---|---|
| passes the AMTERR filter | 118,263 | $0 | 95.5% | 2.1% |
| excluded by it | 7,913 | $51 | 29.3% | 50.0% |

So on the excluded rows the restored fields do not reproduce the benefit they were
restored against. Since the whole point of these features is that they are the
restored pre-QC values (it is why the Python state workbook was moved off its own
feature reconstruction and onto this frame, 2026-07-29), rows where the restoration
failed carry feature values we cannot describe.

**What did not discriminate.** The `correctednotes == "no_change"` share is
essentially identical in both groups, 64.5% on the excluded rows against 63.5% on
the rows that pass, so it carries no signal here. Most cases have no error element
to correct in the first place, so "no change" is the normal outcome. An earlier
reading of this number as evidence of failure was wrong; the residual is what
separates the groups.

**Exclusion 4 removes FY2021 and nothing else.** Measured by running the pipeline
only as far as that filter (`methods/measure_benmax_filter.R`), with exclusions 1
and 3 relaxed so all seven years reach it: 315,410 rows arrive, the filter drops
9,456 of them (3.0%), carrying 1,683 errors (3.8% of the 43,785 errors present).
Every one of those 9,456 rows is FY2021, which is 100% of that year, and the filter
drops 0 rows in every other year (FY2017, 2018, 2019, 2020, 2022, 2023, 2024: 0.0%
each). The reason is visible in the ratio `BENMAX / rawbenmax` on the dropped rows,
which takes only three values: 1.147 (5,576 rows), 1.15 (2,509) and 1.151 (1,371),
the spread being integer rounding of the pandemic 15% allotment increase that the
lookup table does not carry. So despite looking like a general data-quality guard,
in this data the filter functions as a switch that deletes FY2021. It would be
recoverable by using the file's own `BENMAX` as the denominator rather than the
lookup, which is a change to the script's logic rather than a flag.

**FY2020 and FY2021 are excluded by decision, not by this measurement**
(2026-07-29). Both years are not to be used: the data is poor and misleading, and
the practices states used were qualitatively different, so pooling them with
FY2017-19 and FY2022-24 mixes eras rather than adding data. This is why exclusion 1
was reinstated and why exclusion 4 has nothing left to remove. For scale, FY2021 in
the public files is only 9,832 rows covering July to September 2021, quality
control having been suspended for the rest of that year; FY2020 is 49,468 rows
passing the AMTERR filter, carrying 4,986 errors.

**Rule-level consequence, Washington FY2022-24.** The state whose workbook this was
checked against went from 2,356 rows / 223 errors to 2,464 rows / 289 errors. The
108 added rows are the inconsistent kind and 66 of them (61.1%) are labelled
errors. On the 2,356 shared rows the two frames agree: 0 error-flag disagreements,
and feature disagreements on at most 3 rows (`rawben_rel_max`,
`unc_rawben_rel_max` and `total_deductions_by_hh_size` on 3 of 2,356 each,
`medical_deductions` on 1; every other rule feature 0). So the rebuild is additive
rather than disruptive, which is the reassuring part: the check did not cast doubt
on the frame we use. Separately, 2,464 is exactly the Washington row count the
Python dashboard builder read before applying its own consistency filter and
landing on 2,356, confirming that filter was reproducing exclusion 3.

**Conclusion.** No pipeline change. `reg_model_data.rds` stands, and each exclusion
is a validity guard rather than conservatism. The falsifiable version of this
finding, for anyone tempted to revisit it: relaxing exclusion 3 would be justified
if the excluded rows' restoration residuals matched the retained rows', or if their
`AMTERR` and `absbendiff` agreed after some repair. Neither holds.

**Caveats.** The convergence comparison is within a single rebuild run, which is
the right comparison (same correction loops, two groups of rows) but does not tell
us how those rows would behave if the correction were redesigned around them. The
FY2020/FY2021 decision is a judgement about data quality and state practice, not a
measurement, and this entry does not test it. Exclusion 2 was reasoned about rather
than measured.

*Artifacts: `methods/munging_exclusion_check/` (`rebuild_run.log`,
`frame_comparison.log`, `benmax_filter.log`, `verification.log`). Regenerating
scripts: `methods/test_munging_exclusions_minimal.R` (the ~10-minute rebuild;
writes `reg_model_data_minexcl.rds`, ~60 MB, not committed),
`methods/measure_benmax_filter.R`, `methods/verify_munging_exclusions.R`
(re-derives every number above from the two frames in seconds).*

## 25. Admission stringency: tightening the false-discovery rate from 10% to 5% changes nothing (2026-08-03)

> **Takeaway: about our pipeline.** We hold rules to a false-discovery-rate test before
> they can enter a list. Making that test twice as strict (10% to 5%, with the n >= 30
> support floor left in place) changed nothing: 17 of 18 states delivered a
> bit-identical list at the 5% review budget and 16 of 18 at the 10% budget, and the
> median within-state difference in precision was 0.000 at both. The reason is positional.
> Tightening the rate removes rules from the middle and bottom of the ranking, and the
> highest-ranked rule it removed sat at position 14,449 of 50,697, while a review
> budget deploys the top 16 to 27 rules. The support floor is the guard that reaches
> the top of the list; the rate is not.

### What was compared

Two admission arms, identical in every other respect. Both were applied to the raw,
unfiltered 2022-2023 rule vocabularies (144,533 national candidates), which are the
same candidate sets the production pipeline mines.

- **fdr10f** (the shipped admission): a rule is admitted when a Benjamini-Hochberg
  test at a false-discovery rate of 10%, one-sided against its own household-size
  stratum base error rate, rejects "this rule is no better than the base rate", AND
  the rule flags at least 30 training cases. Admitted 50,697 of 144,533 national
  candidates.
- **fdr05f**: the same test at a false-discovery rate of 5%, with the same n >= 30
  floor. Admitted 46,963 of 144,533, which is 92.6% of what fdr10f admitted.

Everything downstream is the settled recipe and is identical across arms: rules are
ordered by the one-sided 99% Wilson lower confidence bound of their training
precision; each state's own mined pool is blended with the national pool on that one
scale; the list is filled to the review budget as "core" and out to 3x depth as
"buffer"; the frozen list is then scored on that state's FY2024 cases. FY2024 is a
true future year relative to the FY2022-2023 training window, not an interpolated
one; nothing from it touched mining, admission, ranking, or fill.

Two arms from the earlier admission audition (section 19) are the relevant prior
context, because they isolate what the floor does. Both were run without the support
floor: **fdr10** (rate 10%, no floor) reached 0.2840 median precision at the 5%
budget, and **fdr05** (rate 5%, no floor) reached 0.2931, against **fdr10f** at
0.3345. Tightening the rate while the floor was absent recovered less than a fifth of
the gap the floor itself was worth. Section 19 recorded that the floor is not
optional. This entry records that once the floor is present, the rate is inert.

### Result: 18 states, both review budgets

Median across the 18 states, with the base error rate given so lift is legible.

| | 5% budget, fdr10f | 5% budget, fdr05f | 10% budget, fdr10f | 10% budget, fdr05f |
|---|---|---|---|---|
| admitted rules (blended pool) | 54,261 | 49,360 | 54,261 | 49,360 |
| rules deployed | 16 | 18 | 27 | 27 |
| workload (share of caseload) | 0.0493 | 0.0493 | 0.0994 | 0.0994 |
| holdout precision | 0.3345 | 0.3471 | 0.2753 | 0.2770 |
| holdout dollar recall | 0.1461 | 0.1565 | 0.2484 | 0.2375 |
| base error rate | 0.1253 | 0.1253 | 0.1253 | 0.1253 |
| lift over base rate (median of the per-state ratio) | 2.48x | 2.56x | 2.15x | 2.09x |

The median precision row moves from 0.3345 to 0.3471 at the 5% budget, and that
apparent gain is an artifact of the median landing on a different state. The lift row
shows the same trap from the other side: at the 10% budget the 5% rate has the higher
median precision (0.2770 against 0.2753) but the lower median lift (2.09x against
2.15x). The within-state difference is the statistic that settles it, and **its median is
exactly 0.0000 at both budgets**, for precision and for dollar recall alike. At the 5% budget fdr05f
was better in 1 state and worse in 0; at the 10% budget better in 1 and worse in 1.

Per state, precision and dollar recall, fdr10f then fdr05f. The blended pool fell to
90-92% of its size for every state, so the pool did shrink; the delivered list did
not change.

| state | base rate | prec 5% | $rec 5% | prec 10% | $rec 10% |
|---|---|---|---|---|---|
| Arizona | 0.112 | 0.279 / 0.279 | 0.224 / 0.224 | 0.326 / 0.326 | 0.376 / 0.376 |
| California | 0.130 | 0.275 / 0.275 | 0.194 / 0.194 | 0.247 / 0.247 | 0.294 / 0.294 |
| Colorado | 0.121 | 0.525 / 0.525 | 0.201 / 0.201 | 0.362 / 0.362 | 0.259 / 0.259 |
| Connecticut | 0.145 | 0.386 / 0.386 | 0.160 / 0.160 | 0.371 / 0.371 | 0.274 / 0.274 |
| District of Columbia | 0.219 | 0.638 / 0.638 | 0.135 / 0.135 | 0.474 / 0.474 | 0.217 / 0.217 |
| Louisiana | 0.110 | 0.209 / 0.209 | 0.100 / 0.100 | 0.207 / 0.207 | 0.235 / 0.235 |
| Maine | 0.124 | 0.326 / 0.326 | 0.241 / 0.241 | 0.250 / 0.250 | 0.315 / 0.315 |
| Maryland | 0.144 | 0.353 / 0.353 | 0.086 / 0.086 | 0.357 / 0.357 | 0.230 / 0.230 |
| Massachusetts | 0.137 | 0.488 / 0.488 | 0.158 / 0.158 | 0.333 / 0.333 | 0.218 / 0.218 |
| Michigan | 0.131 | **0.279 / 0.372** | **0.138 / 0.159** | **0.279 / 0.233** | **0.257 / 0.194** |
| Mississippi | 0.123 | 0.415 / 0.415 | 0.155 / 0.155 | 0.374 / 0.374 | 0.257 / 0.257 |
| Missouri | 0.090 | 0.351 / 0.351 | 0.194 / 0.194 | 0.253 / 0.253 | 0.288 / 0.288 |
| New Jersey | 0.094 | 0.129 / 0.129 | 0.095 / 0.095 | 0.161 / 0.161 | 0.164 / 0.164 |
| North Carolina | 0.127 | 0.289 / 0.289 | 0.107 / 0.107 | 0.233 / 0.233 | 0.147 / 0.147 |
| Tennessee | 0.099 | 0.244 / 0.244 | 0.125 / 0.125 | 0.194 / 0.194 | 0.240 / 0.240 |
| Texas | 0.149 | 0.309 / 0.309 | 0.115 / 0.115 | **0.247 / 0.282** | **0.178 / 0.205** |
| Virginia | 0.168 | 0.343 / 0.343 | 0.106 / 0.106 | 0.329 / 0.329 | 0.201 / 0.201 |
| Washington | 0.085 | 0.375 / 0.375 | 0.230 / 0.230 | 0.272 / 0.272 | 0.351 / 0.351 |

**What did not move** is the finding. Sixteen states are identical on both metrics at
both budgets. Michigan moved in opposite directions at the two budgets (+0.093 at 5%,
-0.047 at 10%), which is what per-state sampling noise looks like rather than an
effect, and Texas moved at the 10% budget only (+0.035). No state changed by a
consistent sign across both budgets.

### Why the rate cannot reach the delivered list

Measured on the national 2022-2023 pool. The 5% rate removes 3,734 of the 50,697
rules fdr10f admits, which is 7.4% of the pool. Ordering that pool by the 99% Wilson
lower bound, the statistic the delivered list is filled from:

| position in the ranking | rules the 5% rate removes |
|---|---|
| top 50 | 0 |
| top 100 | 0 |
| top 500 | 0 |
| top 1,000 | 0 |
| top 5,000 | 0 |

The highest-ranked removed rule sits at **position 14,449 of 50,697**. A 5% review
budget deploys a median of 16 rules and a 10% budget 27, all drawn from the very top
of that ranking, so the removed rules were never candidates for delivery.

The removed rules are not low-precision; they are low-evidence. Median raw training
precision is 0.202 among the rules the 5% rate keeps and 0.202 among those it
removes, identical. What differs is support: median 1,163 cases flagged among the
kept against 360 among the removed, which is why their binomial p-values against the
base rate are weaker. Median 99% lower bound is 0.167 for kept and 0.142 for removed.

The support floor acts at the opposite end of the same ranking. In the same 2022-2023
national admitted pool:

| cases flagged | rules | median 99% LCB | median raw precision |
|---|---|---|---|
| 30-50 | 2,102 | 0.198 | 0.344 |
| 50-100 | 4,116 | 0.190 | 0.298 |
| 100-200 | 5,172 | 0.185 | 0.259 |
| 200-500 | 7,381 | 0.176 | 0.226 |
| 500+ | 31,926 | 0.157 | 0.174 |

Thinly-supported rules carry the *highest* bounds, so they sort to the top, and the
top of the ranking is almost entirely made of them: among the top 25 rules by lower
bound the median rule flags 50 cases and 96% flag fewer than 100; among the top 100
the median flags 68 and 81% flag fewer than 100; every one of the top 500 flags fewer
than 200. Median raw precision in the top 25 is 0.608. Section 1's selection
arithmetic puts the precision reachable by noise alone at n = 30, in a search of this
size, at about 0.34. So the rules a tight review budget deploys are the ones whose
apparent precision is least distinguishable from luck. That is a description of where
a floor would act, not a test of one; a floor sweep is a separate arm and is not
reported here.

### Caveats

- **Single era, not replicated.** One training window (FY2022-2023) and one test year
  (FY2024). Section 20 is the precedent for why that matters: a stringency effect
  visible on 2024 failed to replicate on 2019. A null result is less fragile than a
  positive one, but this has not been confirmed on the FY2017-2018 to FY2019 era.
- **Exploratory, not pre-registered.** No bar was set in advance.
- **18 states, not 49.** The audition harness runs the 18 benchmark states, because
  those are the states whose raw vocabularies were cached.
- **The harness re-fills against the test year.** This audition walks core AND buffer
  rules against each state's FY2024 caseload and stops at the FY2024 cap, so its
  workload equals the review budget by construction (median 0.0493 at the 5% budget,
  0.0994 at the 10%). That is what makes the arms comparable to each other, and it is
  also why these precision numbers are NOT comparable to the frozen-list scorecard in
  `methods/anyerror_blended_holdout_2024/`, where a list filled on FY2022-2023 is
  scored on FY2024 without refilling and carries a median 0.855 of its budgeted
  workload at the 5% budget. Fill ratios from the two designs measure different
  quantities and must not be quoted against each other.
- **Only the 0.05 to 0.10 band was tested by the scored arms.** The test is also not
  correctly specified: the right multiplicity denominator is the searched space, not
  the reported one. The addendum below substitutes denominators up to 100 million and
  finds the delivered list unchanged, so this caveat is now measured rather than open,
  but it is measured on admission only.

### Addendum (2026-08-03): the multiplicity denominator does not reach the delivered list either

The caveat above notes that the Benjamini-Hochberg step divides by the number of rules
the ensembles reported, not the number the trees searched, which makes the bar too
easy. Substituting larger denominators answers the question directly. The support
floor is held at 30 and the rate at 10%; only the denominator moves.

| denominator | national rules admitted | of top 25 by LCB kept | of top 100 | of top 1,000 |
|---|---|---|---|---|
| 144,533 (reported, today) | 50,697 | 25 | 100 | 1,000 |
| 1,000,000 | 41,650 | 25 | 100 | 1,000 |
| 5,700,000 | 36,204 | 25 | 100 | 1,000 |
| 34,000,000 | 31,882 | 25 | 100 | 993 |
| 100,000,000 | 29,807 | 25 | 100 | 973 |

At a denominator of 100 million, three orders of magnitude beyond what we correct
against now, the pool falls by 41% but every one of the top 100 rules survives and 973
of the top 1,000 do. A review budget deploys 16 to 27 rules. So no denominator anyone
would defend changes a delivered list: the top rules have p-values small enough that
the multiplicity correction never binds on them. This is the same positional result as
the rest of the section, in a stronger form, and it means the "wrong denominator"
weakness cannot be repaired, or even usefully probed, through admission.

Where the search size could still matter is the ORDERING, since the delivered list is
the top of a ranking rather than a set. A simultaneous bound over m candidates would
replace z = 2.326 with sqrt(2 log m), which is 4.87 at the reported denominator and
6.07 at 100 million: three orders of magnitude in the search move z by 1.2, so a
sweep over z in {2.326, 3, 4, 5, 6} covers every plausible search size and the exact
denominator never has to be pinned down. That sweep has NOT been run. Section 26 is
the reason to expect it to lose, since a larger z is a continuous version of the
higher support floors that cost precision there, but the two differ: a floor removes
rules from the pool and forces the fill deeper, whereas z only reorders and can still
deploy a thin rule when nothing better competes.

*Regenerating script: `methods/multiplicity_denominator_probe_v2.R` (reads the cached
raw vocabulary, no mining, runs in seconds).*

*Artifacts:
`methods/state_similarity_v2/transfer_benchmark_train2223_test24/fdr05f_audition.csv`
(72 rows: 18 states x 2 budgets x 2 arms). The console log is written beside it as
`fdr05f_audition_run.log` but is not tracked, since the repo ignores `*.log`.
Regenerating script: `methods/fdr_admission_alpha05_v2.R` (runner:
`runners/run_fdr_admission_alpha05.R`), which is `methods/fdr_admission_audition_v2.R`
with `adm_fdr05f <- adm_fdr05 & r$n >= 30` added and `ARMS <- c("fdr10f", "fdr05f")`;
it mines nothing and reads the cached raw vocabularies in
`.../fdr_raw_vocab/`. Prior arms (prod, fdr10, fdr05, fdr10f):
`.../fdr_admission_audition.csv`. The positional and support-band numbers in this
entry are re-derived from `.../fdr_raw_vocab/raw_national.rds`. The re-run of fdr10f
reproduced the recorded audition exactly across all 36 rows (maximum absolute
difference 0 in both precision and dollar recall), which is the check that the two
arms differ only in alpha.*

## 26. The support floor: raising it costs precision, and n >= 30 is near optimal from both directions (2026-08-03)

> **Takeaway: about our pipeline (a refuted prediction).** The n >= 30 support floor
> admits rules whose apparent precision noise alone could reach, so we expected raising
> it to help. It does not. Raising the national floor to 66, 195 or 778 cases lowered
> median holdout precision at the 5% review budget from 0.3345 to 0.3000, 0.2950 and
> 0.2826, monotonically, and lowering the state floor to about 15 (a flat 1% of a state
> caseload) lowered it further to 0.2558. Section 19 already showed that removing the
> floor entirely costs precision (0.2840 against 0.3345), so 30 is close to best from
> both directions. The floor is an estimation-quality guard, not a precision dial, and
> the prediction that a bigger search needs a bigger floor was wrong on this era.

### What was compared

Seven admission arms on the raw 2022-2023 vocabularies (144,533 national candidates,
national training caseload 77,806 cases; the 18 state caseloads run about 1,200 to
1,800 cases). The Benjamini-Hochberg false-discovery rate is held at 10% in every arm,
so the support floor is the only thing that moves. Each arm's floor is applied to the
pool that mined the rule, so national and state pools can get different floors. Rules
are then ordered by the 99% Wilson lower bound, blended, filled to the budget as core
plus buffer, and scored on FY2024, a true future year.

| arm | rule | national floor | state floor (typical) | national rules admitted |
|---|---|---|---|---|
| f30 | n >= 30 (shipped) | 30 | 30 | 50,697 |
| logeq | n >= 30 log(N)/log(2500) | 43 | 27-29 | 49,278 |
| p085 | n >= max(30, 0.085% N) | 66 | 30 | 47,142 |
| p25 | n >= max(30, 0.25% N) | 195 | 30 | 39,586 |
| log25 | n >= 25 log(N) | 282 | 178-187 | 36,653 |
| p100 | n >= max(30, 1% N) | 778 | 30 | 27,948 |
| pure100 | n >= 1% N, no backstop | 778 | 12-18 | 27,948 |

N is the pool's own training caseload. Because a state's public caseload is only about
1,500 cases, the max(30, ...) form leaves p085, p25 and p100 at 30 for every state, so
those three arms change the NATIONAL floor only. pure100 differs from p100 solely in
dropping the state backstop, which isolates the state floor. logeq is the shape that
equalising selection inflation implies (inflation goes as sqrt(2 log m), so equal
inflation means n proportional to log m, and log m barely differs between a 78k-case
pool and a 1.5k-case one); anchored at 30 for a median state it predicts a national
floor of 43, that is, nearly the flat floor we already use.

### Result: 18 states, both review budgets

Medians across the 18 states. "median within-state difference" is computed by taking
each state's own difference against f30 and then the median of those 18 numbers
(i.e., each state's performance is compared to itself); the median precision
column can move on which state lands in the middle.

Budget 5%:

| arm | admitted (blended) | rules deployed | precision | dollar recall | median within-state difference | beats f30 | loses | ties |
|---|---|---|---|---|---|---|---|---|
| f30 | 54,260 | 16.5 | 0.3345 | 0.1461 | 0.0000 | - | - | - |
| logeq | 52,914 | 13.0 | 0.3119 | 0.1484 | 0.0000 | 4 | 7 | 7 |
| p085 | 50,706 | 14.5 | 0.3000 | 0.1444 | 0.0000 | 5 | 8 | 5 |
| p25 | 43,150 | 8.0 | 0.2950 | 0.1314 | -0.0149 | 4 | 10 | 4 |
| p100 | 31,513 | 6.0 | 0.2826 | 0.1343 | -0.0223 | 3 | 12 | 3 |
| log25 | 37,554 | 14.0 | 0.2809 | 0.1211 | -0.0470 | 5 | 11 | 2 |
| pure100 | 32,058 | 8.0 | 0.2558 | 0.1425 | -0.0242 | 4 | 14 | 0 |

Budget 10%:

| arm | admitted (blended) | rules deployed | precision | dollar recall | median within-state difference | beats f30 | loses | ties |
|---|---|---|---|---|---|---|---|---|
| logeq | 52,914 | 26.5 | 0.2770 | 0.2510 | 0.0000 | 6 | 6 | 6 |
| f30 | 54,260 | 27.0 | 0.2754 | 0.2484 | 0.0000 | - | - | - |
| p085 | 50,706 | 24.0 | 0.2692 | 0.2527 | 0.0000 | 6 | 6 | 6 |
| log25 | 37,554 | 25.0 | 0.2679 | 0.2477 | 0.0000 | 6 | 8 | 4 |
| p25 | 43,150 | 19.0 | 0.2655 | 0.2489 | 0.0000 | 7 | 7 | 4 |
| pure100 | 32,058 | 14.0 | 0.2577 | 0.2347 | -0.0113 | 6 | 10 | 2 |
| p100 | 31,513 | 13.0 | 0.2474 | 0.2424 | -0.0155 | 6 | 12 | 0 |

The median base error rate across these states is 0.1253, so f30's 0.3345 at the 5%
budget is 2.48x lift and pure100's 0.2558 is 1.93x.

Two isolations are worth naming:

- **National floor only.** f30 (30) to p085 (66) to p25 (195) to p100 (778), with every
  state held at 30: 0.3345, 0.3000, 0.2950, 0.2826 at the 5% budget. Monotone decline.
- **State floor only.** p100 and pure100 share the 778-case national floor and differ
  only in whether states keep the 30-case backstop: 0.2826 with it, 0.2558 without.
  Lowering the state floor to roughly 15 costs 0.027.

At the 10% budget the picture is flatter. logeq, p085, log25 and p25 all have a median
within-state difference of exactly 0.0000 against f30, so floors up to about 200 nationally are a
wash there; only the 778-case floors lose (-0.0155 and -0.0113). The 5% budget is where
the floor matters, which is consistent with section 22: the top of the list is where
these effects live.

### Why raising it hurts

Higher floors admit only broader rules, and broader rules are less precise. In the
2022-2023 national admitted pool, median raw training precision falls from 0.344 for
rules flagging 30 to 50 cases to 0.174 for rules flagging 500 or more (the full band
table is in section 25). Filling a fixed review budget out of broader rules therefore
lands on lower-precision cases, and it takes fewer rules to do it: median rules
deployed falls from 16.5 at f30 to 6.0 at p100 at the 5% budget. The thin rules at the
top of the ranking, the ones whose apparent precision is closest to what noise can
reach, still carried their precision into FY2024 well enough to beat the broader rules
that replace them.

### The prediction this refutes

Section 1's selection arithmetic says that at n = 30, in a search of this size, noise
alone can reach a precision near 0.34, and section 25 showed the top 25 rules by lower
bound have a median raw precision of 0.608 with 96% of them flagging fewer than 100
cases. The natural inference, that these rules are largely luck and a higher floor
would improve the delivered list, is refuted here on this era: every arm that raised
the floor did worse at the 5% budget. The arithmetic bounds what noise COULD produce;
it does not establish that the rules we deploy ARE noise.

This also answers, in the direction of "no", the specific proposal that a pool mined
from a much larger caseload should carry a much larger floor. The national pool at
77,806 cases is roughly the scale of a state's internal QA data (40k to 100k cases),
and it is exactly the pool where raising the floor hurt most.

### Caveats

- **Single era, not replicated.** FY2022-2023 training, FY2024 test. Not confirmed on
  the FY2017-2018 to FY2019 era. Section 20 is the precedent for a stringency result
  that did not survive that check.
- **Exploratory, not pre-registered**, and 18 states rather than 49.
- **The harness re-fills against the test year**, so workload equals the budget by
  construction (see section 25's caveat). Arms are comparable to each other and to
  section 25's arms, not to the frozen-list scorecard.
- **Internal-scale deployment is not directly tested.** The national pool is a proxy
  for a large caseload, but a state running this on 40k to 100k of its own cases would
  also have a much larger OWN pool, which no arm here simulates.
- **The floors tested are coarse**, four national values between 30 and 778. Nothing
  here rules out a small improvement from a value between 30 and 66.

*Artifacts:
`methods/state_similarity_v2/transfer_benchmark_train2223_test24/support_floor_shape_audition.csv`
(252 rows: 18 states x 2 budgets x 7 arms). Regenerating script:
`methods/support_floor_shape_v2.R` (runner: `runners/run_support_floor_shape.R`), which
is `methods/fdr_admission_audition_v2.R` with the seven floor arms substituted for the
admission arms; it mines nothing and reads the cached raw vocabularies in
`.../fdr_raw_vocab/`. The f30 arm is the shipped admission and reproduces the fdr10f
figures recorded in section 25.*

## 27. How deep the fill reaches, and why that makes evaluation cheap (2026-08-04)

> **Takeaway: about our pipeline.** A delivered list holds about 137 rules at the 5%
> review budget, but building it examines far more of the ranked pool than that:
> a median of 1,544 rules at the 5% budget and 4,194 at the 10%, with the deepest state
> reaching rank 9,072. Depth tracks how WIDE a state's top rules are (correlation -0.58
> with cases flagged per rule), not whether the state struggles to fill its budget
> (-0.31 and -0.07). This matters for cost rather than for quality: evaluating rules is
> the expensive step, so a list can be built from a ranked window instead of the whole
> admitted pool, and because the walk consumes a fixed capacity in rank order there is a
> check that proves the window was big enough. A window of 20,000 covers the worst state
> observed by better than twice over. A fixed cap chosen without that check would be a
> mistake: at the 10% budget the median list needs rank 969 just for its core, so a cap
> at 1,000 would start cutting into delivered rules, not spare ones.

### Two different counts

A delivered list is built by walking the ranked pool from the top and taking a rule
whenever it flags cases that no higher-ranked rule already flagged. Rules that add
nothing new are skipped and cost nothing. Filling continues until the review budget is
full (those rules are the "core") and then out to three times that depth ("buffer"
rules, substitutes a state can swap in when it rejects one on expert judgment).

So there are two counts, and only the second one drives cost:

- **rules delivered**, what the state receives, and
- **rules examined**, how far down the ranking the walk had to look to find them.

Measured over the 49 shipped any-error lists (2022-24 pools, median pool size 39,807
rules after blending each state's own pool into the national one):

| | 5% budget | 10% budget |
|---|---|---|
| rules delivered (core + buffer) | 137 | 283 |
| of which core | 50 | 97 |
| rank reached, core only | median 359, max 1,622 | median 969, max 2,613 |
| rank reached, core + buffer | median 1,544, max 3,820 | median 4,194, max 9,072 |
| deepest state as a share of its pool | 9.7% | 23.1% |
| states past rank 1,000 | 35 of 49 | 46 of 49 |
| states past rank 5,000 | 0 | 17 |
| states past rank 20,000 | 0 | 0 |

The deepest five at the 10% budget are Arkansas (9,072), West Virginia (7,858), Alabama
(6,833), Tennessee (6,629) and Washington (6,292).

### What makes a state go deep

Rule width, and it is the stronger of the two candidate explanations by a wide margin.
Across the 49 states, scan depth correlates -0.58 with the median number of cases a
delivered rule flags, and -0.62 (5% budget) to -0.66 (10%) with the median number of
NEW cases a rule contributes at its rank. A state whose top-ranked rules each flag
twenty cases fills its budget in a few hundred rules; a state whose top rules flag two
needs thousands of them, and has to look past many more that add nothing.

The other candidate does not hold up. It would be natural to assume a deep scan is a
symptom of a list that struggles to reach its budget, but the correlation between scan
depth and the frozen-core fill ratio is only -0.31 at the 5% budget and -0.07 at the
10%. Tennessee and Washington appear among the deepest states and are also among the
weakest fillers, but that pairing does not generalise across the other 47.

### Why this is worth knowing: evaluation is the expensive step

Mining rules is not what costs the most in a study of this kind; scoring them is. In
the first cross-fitted ranking run (section 28 when it lands), the per-state budget
broke down as roughly 10 minutes of mining against 75 minutes of evaluation, because
every candidate rule has to be matched against every case to learn which cases it
flags. The national pool for that study reached 667,714 rules across five splits.

Since the walk only ever looks at rules in rank order and stops when capacity is full,
it never needs the bottom of the pool. Restricting evaluation to the top K rules by the
SAME statistic the walk sorts by turns an O(pool) cost into an O(K) one. On the numbers
above, K = 20,000 against a 39,807-rule pool is a 2x saving for the shipped lists and
roughly a 10x saving for a 200,000-rule research pool, with no change to any output.

### The check that makes it safe rather than merely convenient

Pruning is only sound if the window is provably big enough, and there is a natural
certificate. The walk consumes a fixed capacity, three times the review budget in
cases. Once that capacity is exactly full, no rule below can enter, because any rule
that adds cases would exceed it. So:

1. evaluate the top K rules by the arm's own ranking statistic,
2. run the walk and record the leftover capacity ("slack"),
3. if slack is zero, the result is identical to walking the entire pool. The first K
   steps are the same in both, and after them nothing else fits.

If slack is above zero the window was too small to be sure, and that arm is re-run
unpruned. The window must be ordered by the same statistic the walk uses, or it is not
a prefix of the walk order and the argument fails.

### What NOT to do

Capping the pool at a fixed rank as a matter of policy, rather than as a certified
optimisation, would change delivered lists. At the 10% budget the median state needs
rank 969 for its core alone, so a cap at 1,000 would truncate the core of roughly half
the states and the buffer of nearly all of them. Depth is a property of a state's
caseload, through the width of its rules, so a single global constant cannot be right
for every state.

### Caveats

- Measured on the shipped any-error lists built from the 2022-24 pools. Scan depth
  depends on pool size, the review budget and the buffer multiple, so the specific
  ranks do not transfer to a different configuration; the method of measuring does.
- The 20,000 window is validated against these 49 states at these two budgets only.
  Nothing guarantees a future pool behaves the same, which is exactly why the slack
  certificate is checked at run time rather than assumed.
- This is a statement about cost and about what a cap would do. It says nothing about
  whether the ranking itself is any good, which is section 28's question.

### Addendum (2026-08-04): the certificate tested, on the delivered lists and on a research pool

The section above argues that evaluation can run on a ranked window because the walk
exhausts a fixed capacity in rank order. Two runs test that argument rather than
resting on the rank counting.

**On the 49 delivered lists.** The real fill was re-run for every state at both review
budgets, restricted to the top 20,000 rules by the ranking statistic, and the result
compared rule for rule and role for role against the committed CSV in
`state_delivery_lists/`.

| | result |
|---|---|
| state and budget combinations | 98 (49 states x 2 budgets) |
| leftover capacity ("slack") zero | 98 of 98, maximum slack 0 |
| rebuilt list identical to the shipped list | 98 of 98 |
| pool size | median 39,807, maximum 45,374 |
| rules delivered | median 137 at the 5% budget (max 202), 283 at the 10% (max 405) |

So on the deliverable the window is not merely adequate, it is never even close to
binding, and pruning changes nothing a state receives. That is the claim that matters
for production: this is an evaluation optimisation, not a modelling change.

**On a research pool five times larger.** The cross-fitted ranking study (section 28
when it lands) blends five splits into pools of roughly 200,000 admitted rules per arm,
which is where the saving is worth having. The pruned, single-pass evaluation was run
against the unpruned version that had already been computed overnight for five states:

| | result |
|---|---|
| rows compared | 70 (5 states x 7 arms x 2 budgets) |
| identical admitted counts, deployed counts, workload, precision, dollar recall | 70 of 70 on every measure |
| slack zero | 70 of 70 |
| window actually used | 20,000 in every arm, i.e. the window truncated every admitted pool |

The last row is what makes this a real test rather than a vacuous one. Every arm's
admitted set exceeded 20,000 rules, so the window discarded rules in all 70 cases, and
the results were still identical, because the fill finished inside it. Had the pools
been smaller than the window, the check would have proved nothing.

The cost difference on that pool: Massachusetts took 77 minutes to evaluate unpruned
and about 1 minute pruned. Per state, flags are now built once over roughly 37,000
pooled rules plus 20,000 baseline rules, instead of fourteen passes (seven arms, train
and test) over 180,000 to 220,000.

**What is still not established.** Both runs sample the same era and the same
configuration. A window that suffices here need not suffice for a different budget, a
larger buffer multiple, or a pool whose top-ranked rules are much narrower, which is why
the slack certificate is evaluated at run time and a positive value triggers an
unpruned re-run rather than a warning that can be ignored.

*Artifacts: `methods/anyerror_blended_holdout_2024/certified_fill_check.csv` (98 rows:
pool size, window, core and buffer counts, slack, and whether the rebuild matched the
shipped list). Regenerating script: `methods/certified_fill_check_v2.R` (runner:
`runners/run_certified_fill_check.R`), about 15 minutes, no mining. The research-pool
comparison is `methods/crossfit_ranking_v2.R` run before and after the change; the
unpruned figures come from the 2026-08-03 overnight log.*

### Addendum (2026-08-05): the certificate over the completed cross-fit study

The full cross-fitted ranking run finished 2026-08-05 at 00:31 and gives the strongest
version of this check so far, because the window bound in every single case rather than
in some of them.

All **252 rows** (18 states, 7 arms, 2 budgets) came back with **slack zero**, and all
252 had their pool truncated by the window: the median admitted pool was **182,964
rules** against a 20,000-rule window, and the largest was 228,403. So there is no row
where the window happened to be larger than the pool and the certificate was trivially
satisfied. Pruning was real in 252 of 252 and provably exact in 252 of 252.

That answers the question left open when the window was chosen: no state needed more
than 20,000 rules, and we know it rather than assume it. The baseline arm, which reads
the shipped full-data vocabulary, ran on pools with a median of 54,260 rules and a
maximum of 59,477, also fully truncated and also slack zero.

*Artifacts: `methods/state_similarity_v2/crossfit_ranking_train2223_test24/crossfit_ranking.csv`
(252 rows, `slack` column). Regenerating script: `methods/crossfit_ranking_v2.R` via
`runners/run_crossfit_ranking.R`, about 2.5 hours when the national mines are cached.*

*Artifacts: `methods/anyerror_blended_holdout_2024/fill_scan_depth.csv` (one row per
state and budget: pool size, rules delivered, rank reached for core and for core plus
buffer, rule width). Regenerating script: `methods/fill_scan_depth_v2.R`, which reads
the delivered lists and the pool caches, evaluates no rules and runs in about a minute.
The timing figures come from `crossfit_ranking.log`.*

## 28. Rules that key on a benefit-reconstruction artifact near the maximum benefit (2026-08-04)

> **Takeaway: about the data.** Our reconstruction puts 96.06% of truly-at-max
> households on `rawben_rel_max` exactly 1, but 2.39% land just below it in [0.987, 1).
> A rule whose clauses confine the ratio to just below 1 therefore selects a
> reconstruction artifact, and in a state file where those households sit at exactly 1
> it would flag far fewer cases. The exposure is real and narrow: 1,134 of 19,316
> delivered rule instances (5.9%) draw at least half their flags from those
> mis-recreated households, and they account for a median 6.3% of the cases a state's
> 5% list actually delivers (16.3% at the worst state). Reading exposure off the rule
> text badly overstates it: 7,748 instances bound the ratio below 1, but 1,940 of the
> 2,028 distinct exposed rules take only 1.35% of their flags from artifact rows.

Raised by Ben Molin as issue #1 on the Alabama 5% list. This entry is the diagnostic
half only. It changes no feature definition and no rule.

### What the ratio does in our frame

`rawben_rel_max` is the reconstructed benefit over the maximum benefit for the
household size. Measured on the FY2022-24 modelling frame, 118,263 rows, the same
frame the mining runs report:

| quantity | value |
|---|---|
| rows with `rawben_rel_max` exactly 1 | 37.02% |
| rows truly at max (`rawben == benmax`) | 37.37% |
| rows truly at max but with ratio below 1 ("artifact rows") | 1,724 (1.46% of rows) |
| of truly-at-max households, share landing on exactly 1 | 96.06% |
| of truly-at-max households, share landing in [0.987, 1) | 2.39% |
| of truly-at-max households, share with `unc_rawben_rel_max > 1` | 95.72% |

The last row is a second, separate mechanism. Because 95.72% of truly-at-max
households have an uncapped ratio above 1, a clause such as
`unc_rawben_rel_max <= 0.997` also excludes at-max households rather than selecting
them.

### How the rules were classified

Rules are conjunctions of numeric comparisons, so the clauses on one feature imply an
interval. Terms, defined once:

- **excludes exact-1**: the implied interval for `rawben_rel_max` is bounded strictly
  below 1, so the rule can never flag a household whose ratio is exactly 1.
- **band-confined**: excludes exact-1 and the implied lower bound is at least 0.987, so
  among at-max households the rule can only ever match artifact rows.
- **unc-capped**: `unc_rawben_rel_max` bounded above by something below 1.
- **artifact-dependent**: measured rather than read off the rule text. At least half of
  what the rule flags in the frame is an artifact row.

### Exposure across the 98 delivered lists

19,316 rule instances, 2,028 distinct stratum-and-rule pairs once repeats across states
and budgets are removed.

| class | rule instances | share |
|---|---|---|
| mention `rel_max` at all | 15,398 | 79.7% |
| exclude exact-1 | 7,748 | 40.1% |
| unc-capped | 3,155 | 16.3% |
| band-confined | 625 | 3.2% |
| artifact-dependent (measured) | 1,134 | 5.9% |

The text-based and measured classes disagree, which is the reason to measure. Of the
2,028 distinct exposed rules, 1,940 draw only 1.35% of their flags from artifact rows;
they bound the ratio below 1 somewhere far from the band and are not touching this.
The concentration is in **88 distinct rules** that draw **76.7% of their flags (17,765
of 23,160) from artifact rows**. All 625 band-confined instances fall in that group.

Those 88 are not weak rules. On the frame they run at precision 0.3612 (8,366 errors on
23,160 flags), against 0.2339 for the other 1,940 exposed rules. They earned their rank
on flags that a state's own file may not contain.

### What reaches a delivered list

`n_new_at_rank` is the marginal new cases a rule contributed at its rank in the walk, so
summing it over a list partitions the list's cases (unlike per-rule flag counts, which
double-count cases that trip several rules).

| budget | median share of delivered cases from artifact-dependent rules | 90th pct | max |
|---|---|---|---|
| 5% | 6.3% | 11.0% | 16.3% (Massachusetts) |
| 10% | 4.1% | 8.2% | 9.0% (Massachusetts) |

Most exposed at 5%: Massachusetts 16.3%, Vermont 14.7%, Pennsylvania 14.3%, Rhode
Island 13.0%, Michigan 11.4%. Of the 1,134 artifact-dependent instances, 724 sit in the
core and 410 in the buffer.

This is an upper bound on the damage, not an estimate of it. It is the share of
delivered cases contributed by rules that mostly flag artifact rows. Whether those
cases disappear in a state's own file depends on that state's reconstruction, which
this repo cannot observe.

### The three rules from the issue

Frame counts are FY2022-24 within the rule's own household-size stratum.

| rule (abbreviated) | stratum | list rows | frame flags | errors | artifact rows | share artifact |
|---|---|---|---|---|---|---|
| `elderly_disabled_i > 0.5 & rawben_rel_max > 0.993 & total_deductions_by_hh_size > 348 & unc_rawben_rel_max <= 0.997` | 2-3 | 28 | 54 | 35 | 45 | 83% |
| `rawben_rel_max >= 0.987 & < 0.997 & shelter_expenses_by_hh_size >= 850 & utilities < 576` | 1 | 67 | 353 | 165 | 277 | 78% |
| `rawben_rel_max >= 0.987 & < 0.991 & total_deductions_by_hh_size >= 276 & utilities < 578` | 1 | 63 | 382 | 177 | 341 | 89% |

The first is the `unc_` mechanism: it puts no upper bound on `rawben_rel_max` at all, so
a text scan for a bound below 1 misses it. The other two are band-confined.

### What is not settled

Whether an `at_max_benefit` feature repairs this is untested. Adding a feature changes
the mining vocabulary and requires a full re-mine. Whether smoothing
`unc_rawben_rel_max` to exactly 1 is the right fix is also open, and it would be a
change to the reconstruction rather than to the pipeline.

*Artifacts: `methods/at_max_benefit_diagnostic/` (README.md with the full tables,
`rule_classification.csv` for all 19,316 instances, `affected_rule_eval.csv` for the
2,028 distinct exposed rules, `delivered_footprint.csv` per state and budget,
`ben_examples.csv`, `summary.json`). Regenerated by
`python runners/run_at_max_benefit_diagnostic.py`.*

## 29. Characterizing what each delivered rule finds, so a state can choose its own rules (2026-08-04)

> **Takeaway: about the data.** Each delivered rule can be described by what its errors
> are about and how they happened, well enough for a state to judge which rules suit what
> it can catch and fix. The description carries real spread: across 543 rules sorted into
> 7 element groups, a rule's earned-income share runs from 0.03 at the 10th percentile to
> 0.43 at the 90th, against a national mix of 0.33. Fields saying WHAT the error is about
> differ across halves of the country by no more than sampling alone would produce
> (earned income: gap 0.034 against a 0.032 floor). Fields saying WHO was coded as the
> cause, WHEN it arose and HOW it surfaced differ by 1.23 to 1.35 times what sampling
> explains, so a national figure on those three is indicative rather than exact for any
> one state.

**How to read the numbers below.** Three terms, used throughout.

- **Reliability** is the share of the spread across rules that is real difference between
  rules rather than sampling error. 0 means the apparent differences between rules are
  entirely noise; 1 means they are entirely real. It answers "does this number describe
  THIS rule?"
- **Split-half over states** takes the 49 states, splits them at random into two halves,
  computes the same rule's share on each half, and records the absolute gap. Repeated
  over 40 random splits. It answers "would a different set of states have told the same
  story?"
- The **sampling floor** is the gap you would expect from that same split if the rule's
  true share were *identical* in both halves and only sampling differed. It is not zero:
  at these counts, two draws from the same distribution differ by a few points. Formally
  the expected absolute difference of two binomial estimates.

The **ratio** of the observed gap to the floor is what carries meaning. A ratio near 1
means the field differs across halves of the country by exactly what noise predicts, so
there is nothing to explain. Above 1 means there is genuine variation between states on
top of noise. Below 1 is also noise, arriving slightly under expectation.

The point is not to label a rule for its own sake. A state deciding whether a rule is
worth using as a flagging criterion needs to know what it tends to surface, so it can
weigh that against what it can actually catch and fix. So this produces descriptive
fields and stops. It assigns no rule to a category, and it reports no conjunctions such
as "agency-caused AND resolvable at the desk"; combining fields is the state's
judgement. Every share ships with a Wilson interval, because a share on 20 variances and
a share on 400 are different objects.

**What we tried first, and why it was dropped.** Issue #2 asked for the modal
`ELEMENT1` next to each rule plus the share of that rule's errors carrying it, so we
built exactly that before building the sheet described here. Three things came out of it,
and only the third is a reason to drop the mode.

It was **not unstable across years**, which is worth stating plainly because it is the
opposite of what a summary of this history might suggest: a rule's modal element agreed
between FY2022-23 and FY2024 for 463 of 529 rules, 87.5% against 41.6% chance agreement.

It was **uninformative**. The modal element accounted for a median 34.9% of what a rule
caught on FY2024 and 32.7% on FY2022-23, only 9.6% of rules had a modal element above
half their errors, and 320 of the 529 rules carried the same one, wages and salaries. A
state told "this rule finds wages and salaries errors" would have been right about a
third of the time, and would have read the same sentence against three rules in five.

And a mode is a **fragile way to summarise a flat distribution**. Measured on the coarser
7-group scale used here, which is the most favourable case for it, the top group flips
between two random halves of the 49 states in **11.2% of 19,061 comparisons**, because
the top group leads the runner-up by a median of only 0.179 and by under 0.10 for 22.7%
of rules. The underlying shares are stable at the sampling floor over the same splits;
it is the act of collapsing them to a winner that is not.

So the mode is not reported. The full set of group shares with intervals carries the same
information without the knife edge, and a state that wants a headline can read the top
line of `element_groups_to_75`. The earlier framing also judged that label against
pass-or-fail criteria rather than characterizing the rule, and scored concentration
against 49 raw element codes rather than the groups a state reasons about.

**What the fields say.** Across 543 rules, median 203 variances and 126 error cases
each pooled over FY2022-24:

| field | median share | reliability | split-half over states | sampling floor | ratio |
|---|---|---|---|---|---|
| earned income | 0.330 | 0.94 | 0.034 | 0.032 | 1.06 |
| unearned income | 0.188 | 0.94 | 0.030 | 0.031 | 0.96 |
| shelter deduction | 0.182 | 0.92 | 0.030 | 0.038 | 0.79 |
| wrong amount, known item | 0.298 | 0.75 | 0.041 | 0.039 | 1.04 |
| wrong include/exclude decision | 0.287 | 0.93 | 0.039 | 0.046 | 0.86 |
| arose at certification | 0.604 | 0.85 | 0.057 | 0.046 | 1.23 |
| coded agency-caused | 0.566 | 0.72 | 0.062 | 0.047 | 1.32 |
| surfaced from the case record | 0.409 | 0.75 | 0.061 | 0.045 | 1.35 |

Reliability is the share of the spread across rules that is real between-rule difference
rather than sampling error. The split-half test computes each rule on two random halves
of the 49 states and compares the observed difference to what sampling alone would give.

**Why states, not years, are the primary test.** The temporal axis gives one comparison;
splitting states gives as many draws as you care to take. It also turned out to be the
only clean one. Two nature groups drift far above their sampling floor between eras
(wrong-amount 3.60x, wrong-include/exclude 1.68x) because **nature codes 56, 57, 33 and
58 are 0.00% of variances in FY2022 and FY2023 and appear only in FY2024**. Deduction
errors formerly coded 52 or 53 are now split into 56 and 57, moving wrong-amount from
14.3% to 23.3% and pulling wrong-include/exclude from 21.6% to 17.3%. The same two
fields score 1.04 and 0.86 on the state split-half, so they are stable; the era test was
measuring a codebook revision.

**Two things we learned about using these data.** The FY2024 technical documentation, in
`additional_data/`, defines every code the FY2023 one leaves undefined. `AGENCY` 26 is
not a fault code but "change was not required to be reported by the client or acted upon
by the State", and on error-case variances it is 1.4%, not the 27.3% an earlier note
reported from a population that included sub-threshold variances on non-error cases.
Coverage on error-case variances is near total: NATURE 1.000, AGENCY 1.000, DISCOV
0.992, TIMEPER 0.990, no state below 0.938. The cause split is agency 54.7% to client
41.9%.


### Design

543 distinct stratum-and-rule pairs, from 2,390 deployed instances across the 98
delivered lists. The flagged set is the refill walk against the FY2024 caseload, and all
98 state-and-budget combinations reproduce holdout_metrics.json on rules deployed, rules
available, cases flagged, errors and precision; the script asserts this and stops on
drift. Characterization is computed on the NATIONAL pool. Per-state characterization
does not work: at the 10% budget the median deployed (rule, state) pair flags 3 cases
and the 25th percentile is 1, and the median rule is deployed by exactly one state.

Code definitions come from `additional_data/FY-2024-Tech-Doc.pdf` and the element and
nature nesting in `additional_data/FNS380-1WithInstructions.pdf`. Element groups follow
the munging script's own reconstruction groups (lines 603-627), so the characterization
and the modelling frame agree on what an earned-income error is; 321 sits with earned
income. Nature groups are element-independent: FNS-380-1 nests natures under elements
only to say which are permitted where, and a nature code carries the same meaning
wherever it appears, verified across every element page. Every variance maps to a nature
group; there is no residual.

### The bound that shaped the design

For a rule truly at 50% with 200 train and 100 test error cases, the standard error on
the difference is 0.061, so a 95% band of plus or minus 0.120 and a median absolute
difference of 0.041 arise with nothing having changed. 179 of 543 rules reach that
support; the median rule has 84 and 45, giving a plus or minus 0.181 band. Detecting a
true 10-point drift at 80% power needs about 588 train and 294 test cases, which only 38
of 543 rules reach. Per-rule stability verdicts are therefore unavailable for most rules,
which is why the fields ship with intervals and the evidence is reported at field level.

### Full field table

| field | median share | 10th-90th | reliability | split-half obs | floor | ratio |
|---|---|---|---|---|---|---|
| earned income | 0.330 | 0.034-0.425 | 0.94 | 0.034 | 0.032 | 1.06 |
| unearned income | 0.188 | 0.088-0.379 | 0.94 | 0.030 | 0.031 | 0.96 |
| shelter deduction | 0.182 | 0.128-0.332 | 0.92 | 0.030 | 0.038 | 0.79 |
| utility allowance | 0.050 | 0.017-0.131 | 0.90 | 0.018 | 0.019 | 0.92 |
| medical deduction | 0.013 | 0.000-0.304 | 0.98 | 0.008 | 0.012 | 0.73 |
| dep care or child support deduction | 0.043 | 0.000-0.140 | 0.92 | 0.013 | 0.014 | 0.97 |
| wrong amount, known item | 0.298 | 0.164-0.352 | 0.75 | 0.041 | 0.039 | 1.04 |
| wrong include/exclude decision | 0.287 | 0.207-0.584 | 0.93 | 0.039 | 0.046 | 0.86 |
| unreported source of income | 0.104 | 0.039-0.153 | 0.70 | 0.026 | 0.025 | 1.05 |
| household composition | 0.059 | 0.000-0.102 | 0.79 | 0.012 | 0.016 | 0.79 |
| change in circumstances | 0.078 | 0.026-0.120 | 0.63 | 0.025 | 0.023 | 1.11 |
| arose at certification | 0.604 | 0.418-0.680 | 0.85 | 0.057 | 0.046 | 1.23 |
| arose after certification | 0.298 | 0.208-0.480 | 0.88 | 0.058 | 0.044 | 1.33 |
| coded agency-caused | 0.566 | 0.471-0.676 | 0.72 | 0.062 | 0.047 | 1.32 |
| coded client-caused | 0.395 | 0.277-0.493 | 0.75 | 0.061 | 0.047 | 1.30 |
| surfaced from the case record | 0.409 | 0.292-0.510 | 0.75 | 0.061 | 0.045 | 1.35 |
| overissuance, of directional error cases | 0.746 | 0.606-0.999 | 0.95 | | |
| error_status is other_error | 0.269 | 0.077-0.454 | 0.90 | | | |

Split-half uses 40 random halves of the 49 states, requiring 20 variances on each side.

### Era drift against the same floor

| field | median abs difference | sampling floor | ratio |
|---|---|---|---|
| wrong amount, known item | 0.138 | 0.038 | 3.60 |
| wrong include/exclude decision | 0.070 | 0.042 | 1.68 |
| arose after certification | 0.053 | 0.041 | 1.28 |
| unearned income | 0.036 | 0.031 | 1.16 |
| arose at certification | 0.051 | 0.044 | 1.15 |
| surfaced from the case record | 0.047 | 0.043 | 1.10 |
| coded client-caused | 0.045 | 0.045 | 1.00 |
| unreported source of income | 0.023 | 0.024 | 0.98 |
| earned income | 0.029 | 0.033 | 0.90 |
| coded agency-caused | 0.041 | 0.045 | 0.91 |
| overissuance, of directional error cases | 0.032 | 0.033 | 0.96 |
| error_status is other_error | 0.016 | 0.036 | 0.44 |
| change in circumstances | 0.021 | 0.023 | 0.89 |
| dep care or child support deduction | 0.012 | 0.014 | 0.89 |
| utility allowance | 0.015 | 0.019 | 0.80 |
| medical deduction | 0.009 | 0.011 | 0.79 |
| shelter deduction | 0.028 | 0.037 | 0.77 |
| household composition | 0.011 | 0.015 | 0.75 |

Nature shares by fiscal year, showing the codebook revision: nature 56 runs 0.00%,
0.00%, 6.78% across FY2022, FY2023 and FY2024, and nature 57 runs 0.00%, 0.00%, 3.45%.
Codes 33 and 58 behave the same way.

### Mutual information against a permutation null

Normalized mutual information (NMI) runs 0 when knowing the rule tells you nothing about
the error's group to 1 when it tells you everything. The permutation null is what NMI
would be if rule and group were unrelated: the group labels are shuffled across variances
and the statistic recomputed, 60 times.

| pairing | groups | NMI observed | NMI under the null | distance above null |
|---|---|---|---|---|
| rule and element group | 7 | 0.0435 | 0.0020 | 985 sd |
| rule and nature group | 10 | 0.0159 | 0.0096 | 233 sd |

Those two columns answer different questions and only one of them is about size. The
distance above the null says the association is not chance, and it is enormous only
because the null is tight: there are 259,958 (rule, variance) pairs behind it, so
detecting a small association is easy. **The sd distance is not an effect size.** The
effect size is NMI, which at 0.0435 on a 0-to-1 scale says a rule shifts the mix of
error types well short of determining it.

The usable magnitude is the spread across rules, not either statistic: a rule's
earned-income share runs 0.03 to 0.43 between the 10th and 90th percentiles, and 0.00 to
0.70 end to end, against a national mix of 0.33. That range is what a state acts on.

A case trips several rules, so the units are (rule, variance) pairs rather than a
partition; the null has the same structure, so the comparison holds.

### A rule's profile is only as distinctive as the rule is narrow

The characterization is least useful exactly where a state's list leans hardest.
Measuring each rule's element mix against the national mix as a total variation
distance, where 0 means identical:

| rule support | rules | median distance from the national mix |
|---|---|---|
| under 50 error cases | 125 | 0.264 |
| 50 to 200 | 189 | 0.235 |
| 200 to 1,000 | 194 | 0.115 |
| over 1,000 | 35 | 0.057 |

Support and distance correlate -0.446. This is structural rather than a defect: a rule
flagging 2,000 error cases covers a large slice of the caseload, so its error mix has to
approach the caseload's. The three highest-support rules in the sheet read almost
identically, at earned income 0.33, 0.35 and 0.34 with shelter deduction 0.23, 0.21 and
0.23.

So the rules whose profile actually distinguishes them are the narrow ones, which are
also the ones with the widest intervals. That is the concrete form of the mutual
information result above: the columns should be read next to `n_error_cases`. A state
should not expect the high-volume core of its list to differentiate on these fields, and
should treat a distinctive-looking narrow rule with the caution its interval implies.

### Caveats

Per-rule counts double-count cases that trip several rules and are not a partition of
the flagged total.

Direction (over versus under) is case-level and comes from the frame's `status` field
(2 overissuance, 3 underissuance), which is populated for every one of the 13,288 FY2022-24
error cases. It does NOT come from `E_FINDG`, which is populated for 57% of variances and
which three states do not report at all, and it does not come from `error_status`. An
earlier version of this section used `error_status` and put its `other_error` category in
the denominator, which was wrong twice over: `other_error` is a residual error TYPE that
carries both directions (3,830 overissuance and 2,209 underissuance cases), so the
resulting field correlated -0.90 with how little other_error a rule caught and only 0.21
with the actual direction split. It read 0.437 at the median where the directional split
reads 0.746. The other_error share is now reported as its own field. VERIF is excluded from every construct: it
records the evidence a QC reviewer needed to substantiate a finding after the fact,
which is QC's evidentiary burden rather than what a caseworker could resolve from the
file.

*Artifacts: [`methods/rule_error_profiles/`](https://github.com/giannella/snap_qc/tree/main/methods/rule_error_profiles) (README.md with the full tables,
`rule_characterization.csv` as the shippable sheet, `rule_profiles.csv` per era,
`deployed_rules.csv`, `stage1_checks.csv`, `characterization.md`). Regenerated by
`python runners/run_rule_error_profiles.py`.*

## 30. Out-of-fold ordering does not reproduce at deliverable scale, and how much the mining draw decides (2026-08-05)

> **Takeaway: about our pipeline.** Ranking the national pool by a bound computed on
> data the rules were not mined from does not beat ranking by the self-scored bound
> once the result is measured as delivered precision on an unseen year. The
> pre-registered bar was a +0.010 within-state median at the 5% review budget; the
> result was **-0.0044** across 49 states, and +0.0000 at the 10% budget, under both
> admission variants. Section 22's +1.6pp does not carry into the deliverable. What
> the same run does show is how much rides on the mining draw: two partitions'
> top-100 rules share **3.2%** of their signatures, yet the **5% budget lists** they
> produce catch **38.2%** of the same errors, twelve times the rule-level agreement
> and six times chance. Most of the rule churn is cosmetic. The remainder is not:
> only **one error in ten** is caught by all five partitions. Section 31, run later
> the same day, bounds the decomposition: under seed variation alone the same
> 5%-budget overlap is 0.531, though on mines of double the data (full FY2022-23
> against the halves here), which itself stabilizes lists. So the seed lottery in a
> single mine accounts for much of the churn seen here, with the data half and the
> data volume sharing the remainder in a split this pair of studies cannot separate.

Pre-registered in `methods/preregistration_national_only_xfit_2026-08-05.md` before
any result existed, including both bars and the decision rules.

### Why national-only

The 2026-08-04 study cross-fitted the national pool and every state's own pool at
once, which halved state pools to 48-140 errors and destroyed them; it is recorded
as an invalid design rather than a refuted idea. The obvious repair, cross-fitting
only the national pool, turns out not to be available either. Measured on the cached
mines at the top 1% of the national ranking (mean of self-scored minus out-of-fold
bound over the top 1% ranked by the self-scored bound), the self-scored Wilson bound
overstates the out-of-fold bound by **+0.106 for national rules**; the same
overstatement was recorded as larger still for state pools (+0.123) at the time,
but that computation was not preserved as an artifact and a post-hoc re-derivation
did not reproduce it exactly, so read the state-side figure as direction only.
Either way, blending a cross-fitted national pool with self-scored state pools on
one sorted scale would systematically promote state rules. The state pool is
therefore dropped, which section 14 already supports: the plain national list is
the best default a state can deploy.

### The ordering result

Five cached partitions of FY2022-23, each mining on 38,901 rows carrying ~4,242
errors and scoring on the complementary 38,905. Both arms read the same vocabulary;
only the ranking statistic differs. Evaluated by filling each of 49 states' training
caseloads to the review budget, freezing, and walking against that state's FY2024
cases.

| out-of-fold minus self-scored | 5% budget | 10% budget |
|---|---|---|
| per state the mean over 5 partitions, median across 49 states, common admission | -0.0044 | +0.0000 |
| states better / worse (ties from the 4-decimal precision column) | 18 / 28 (3 tied) | 23 / 24 (2 tied) |
| across all 245 state-partition pairs | 95 better / 118 worse (32 tied) | 111 / 106 (28 tied) |
| own-admission variant | -0.0053 | -0.0021 |

The aggregation is stated exactly because the two natural readings differ: taking
the MEDIAN over partitions within each state instead of the mean gives -0.0213 at
the 5% budget. Both readings fail the pre-registered +0.010 bar.

Self-scored admission passes about 11% more rules than out-of-fold admission
(mean over the five partitions 41,443 against 37,335, own-admission variant),
which is the same inflation appearing one stage earlier.

### How much the mining draw decides

The rules barely agree across partitions. Ranked by the out-of-fold bound, two
partitions' top 100 share a pairwise Jaccard of **0.032** by signature, with **zero**
signatures common to all five. By exact rule text the Jaccard is 0.000 to 0.006.
That is not a property of out-of-fold scoring; ranking by the self-scored bound
gives 0.066. It also sits against 72.9% of signatures being common to all five
partitions across the pool as a whole, so the pool is stable in composition while
its top is not.

The delivered lists agree far more than their rules do:

| | 5% budget | 10% budget |
|---|---|---|
| Jaccard, cases flagged | 0.325 | 0.435 |
| Jaccard, errors caught | 0.382 | 0.497 |
| chance baseline for errors | 0.063 | 0.126 |
| errors caught by all five partitions | 10.6% | 22.3% |

Ten states, both budgets, out-of-fold arm. The walk fills to the same capacity each
time, so the sets are near-equal in size and the comparison is of composition. One
machinery note for anyone comparing across studies: admission in these arms is BH
within stratum, where the shipped delivery builder pools BH across strata with
per-rule stratum base rates. Section 31 matches the builder, so its contrast with
this table carries that small admission difference on top of the intended one.

Two readings, both supported. Agreement at the case level is twelve times agreement
at the rule level, so most of the rule churn is different descriptions of the same
work. And two partitions still share under 40% of the errors they catch, with one
error in ten common to all five, so the lists are not interchangeable either.

The pre-registered bar did not resolve this: above 0.5 was to mean stable, below 0.3
was to mean the draw changes which errors a state finds, and the result landed
between them at both budgets. Section 31 then supplied most of the missing
decomposition. Under seed variation alone, with the shipped recipe on the full
77,806 rows (double the 38,901 each partition mined on, a difference that itself
stabilizes lists), the same ten-state readout gives 0.531 and 0.666, and coverage
converges to near-identity by depth 20,000 of the ranking (pairwise 0.959 to 0.965,
every seed's pool covering all 4,803 test errors). The summary of both studies
together: the reachable error set is stable, budget-depth lists are not, and the
instability is a property of ordering under any single draw, seed or data half
alike, not of what the mining can reach. How the residual gap (0.531 against 0.382)
splits between the data half and the doubled mining data is not separable from this
pair of studies.

### Scope, and what this does not say

Every figure here comes from mining on half the data. The shipped pipeline mines on
all of it, so this particular source of variation does not exist in the delivered
lists. What those lists do have is ensemble seed variation, which was untested when
this section was drafted; the pre-registration claimed a low overlap here would be a
finding about the currently shipped pipeline, and that claim reached too far and was
withdrawn. Section 31 then measured the shipped pipeline directly, seed varying and
nothing else, and found budget-list overlap of 0.531 at the 5% budget: the
withdrawn claim's conclusion was approximately right, but it needed its own
experiment rather than an extrapolation from half-data mining.

Delivered precision moved with the partition by a median standard deviation of
0.0443 at the 5% budget. Read that with the measurement in mind: a median state has
872 FY2024 cases, so a 5% budget flags about 44, and precision on 44 cases carries a
binomial standard error of 0.068 by itself. The partitions vary less than
independent draws would, which is the case-overlap result seen from another angle.

### Decision

The pre-registered rule fires: section 22 does not generalise to the deliverable, so
the line is dropped, K-fold cross-fitting is not warranted, and the blended version
stays shelved. No pipeline change.


### Verification notes and artifacts

Every number above was re-derived from the artifacts on 2026-08-05 before this
section was recorded. Notes a reader needs to reproduce them:

- The ordering table reads `methods/national_only_xfit/national_only_xfit.csv`
  (49 states x 5 partitions x 2 budgets x 2 admission variants; written by
  `methods/national_only_xfit_v2.R`, runner `runners/run_national_only_xfit.R`).
  Its `precision` column is rounded to 4 decimals, so within-state mean
  differences carry float residue; the better/worse counts treat |diff| under
  1e-12 as ties. Under common admission both arms share `n_admitted` by
  construction; the 41,443 vs 37,335 admission contrast comes from the
  own-admission rows.
- The case-overlap table reads
  `methods/national_only_xfit/partition_case_overlap.csv` (10 states; written
  by `methods/partition_case_overlap_v2.R`). The chance baseline is f/(2-f)
  with f the median errors-caught share per state; it reproduces as
  0.0630 / 0.1261.
- The signature numbers are logged in
  `methods/national_only_xfit/partition_signature_overlap.txt` (regenerated
  2026-08-05 by `methods/partition_signature_overlap_v2.R` against the cached
  mines). "Zero signatures common to all five" holds for the out-of-fold
  ranking's top 100; the self-scored top 100 has one. The whole-pool 72.9%
  figure is 486,799 of 667,714 signatures present in all five partitions,
  from the cached mines.
- The +0.106 top-1% overstatement is the mean of (self-scored minus
  out-of-fold bound) over the top 1% of out-of-fold-admitted national rules
  ranked by the self-scored bound, re-derived from the cached mines. The
  +0.123 state-pool companion was recorded during the study but its
  computation was not preserved and did not re-derive; it is reported as
  direction only. 8 of the 90 cached state mines have zero rules passing
  self-scored admission at all, consistent with the halved-pool collapse.
- All rows carry `slack` = 0 with `n_scanned` = 20,000, the section 27
  capacity certificate.
- Cached mines: `methods/state_similarity_v2/crossfit_ranking_train2223_test24/mines`
  (gitignored; regenerated by `methods/crossfit_ranking_v2.R`).

## 31. Seed stability: the deep pool covers the same errors; the top of the ranking does not (2026-08-05)

> **Takeaway: about our pipeline (pre-registered, bar cleared decisively).**
> Re-mining the national pool with different random seeds changes the rule
> text a lot and the errors reached almost not at all, once you go deep
> enough. By depth 20,000 of the ranking, two seeds' rules cover 96% the
> same FY2024 error cases (pairwise Jaccard 0.959 to 0.965 against a 0.705
> chance baseline at that reach), and every seed's full admitted pool covers
> ALL 4,803 test errors, so nothing in coverage is left on the table by
> mining once.
> At the depths a review budget actually deploys, the seed matters more:
> two seeds' 5% budget lists catch only about half the same errors (median
> 0.531). The pre-registered conclusion follows: the instability in our
> lists is an ORDERING phenomenon, not a vocabulary one, which opens the
> preference-ordering line of work (letting a state promote rules by what
> it can catch and fix, using the section 29 characterization fields)
> without fear of losing reach.

**Design** (pre-registered before any number existed, approved same day).
Three seeds, one component varying: seed A is the cached full-data mine
(seed 117), B (20260805) and C (31415) fresh mines, all on the full FY2022-23
frame (77,806 rows, 8,485 errors; no split), any-error frame, shipped
admission (pooled BH at FDR 10% + n >= 30) and ordering (99% Wilson LCB),
national pool only. Evaluated as FY2024 error-case coverage (4,803 errors,
40,457 rows, 49 states) at depths K = 100 / 200 / 1,000 / 20,000 / full.
The seed-A determinism anchor passed both parts (recomputed n/k identical to
the era cache for all 144,533 rules; 50,697 admitted, matching the era
artifact), so the machinery reproduces the shipped scoring exactly.

**Scale stability.** Raw pools: 144,533 / 144,933 / 144,488 rules. Admitted:
50,697 / 51,503 / 51,372 (within 1.6%).

**The primary result.** Median pairwise Jaccard of covered FY2024 error-case
sets, with the chance baseline at matched reach in parentheses:

| K | case Jaccard (chance) | signature Jaccard | all-3-seed intersection / union |
|---|---|---|---|
| 100 | 0.656 (0.029) | 0.150 | 0.549 |
| 200 | 0.698 (0.040) | 0.186 | 0.580 |
| 1,000 | 0.658 (0.127) | 0.240 | 0.541 |
| 20,000 | 0.961 (0.705) | 0.354 | 0.943 |
| full | 1.000 (1.000) | 0.399 | 1.000 |

The pre-registered bar (>= 0.80 at K = 20,000 for the ordering fork) was
cleared by every pair individually (0.9593, 0.9608, 0.9649), not just the
median. Dollar-weighted coverage tracks the case numbers everywhere (0.9665
to 0.9698 at K = 20,000). The pre-registered expectation (ordering fork) was
written down in advance and confirmed.

**Full pools saturate; one mine suffices for reach.** Every seed's full
admitted pool covers all 4,803 of 4,803 FY2024 errors, and the
union-accumulation curve is degenerate: seed A alone reaches 4,803, so seeds
B and C add zero new errors.

**Where the seed does matter: the top of the list.** From K = 100 to 1,000
the pairwise overlap plateaus at 0.65 to 0.70 (first crossing of 0.70 at
rank 63, but the curve is not monotone; it dips back to 0.658 at K = 1,000).
On the ten section 30 states, the budget readout gives median pairwise
errors-caught Jaccard of **0.531 at the 5% budget and 0.666 at 10%** (chance
0.057 / 0.126), with wide state spread (Texas 0.889, New Jersey 0.200 at 5%;
per-state counts are small, 3 to 32 errors caught, so individual state
numbers are noisy). Plainly: the shipped pipeline mines once with one
arbitrary seed, and a different draw would deliver a 5% list catching only
about half the same errors, while drawing from the same deep reservoir.
States should hear the constructive version: many near-equivalent orderings
exist, which is exactly what makes preference-based reordering viable.

**Text churn is not work churn, now shown at seed level.** Rule-signature
overlap stays low everywhere (0.15 at K = 100, 0.354 at K = 20,000) while
case-level agreement reaches 0.96: different seeds describe largely the same
underlying errors with largely different rules. Section 30 found this for
half-data partitions; it holds under seed variation alone.

**Contrast with section 30's partitions (data + seed vs seed alone).**
Errors-caught Jaccard on the same ten states: 0.531 vs 0.382 at 5%, 0.666
vs 0.497 at 10%. Seed variation alone accounts for much of the partition
study's churn. Three differences ride on the contrast, so read it as an
approximate decomposition, not an exact one: this study's mines use the full
77,806 rows against the partitions' 38,901 each, and the doubled data itself
stabilizes lists, so part of the gap is volume rather than the data half's
variation being removed; the section 30 scripts ran BH within stratum while
this study matches the shipped pooled-BH recipe (a discrepancy in section
30's machinery, surfaced by this study's pre-launch review); and section
30's numbers embed out-of-fold scoring.

**Incidental observation for section 28.** Two of the three seeds put a
`rel_max`-band rule at the very top of the national ranking (seed A rank 1:
`unc_rawben_rel_max` in (0.891, 0.991]; seed B rank 1: `rawben_rel_max` in
[0.984, 0.992)), independent draws landing in the reconstruction-artifact
band section 28 flagged. Strengthens the case for resolving the
at-max-benefit feature question before the next delivery-list build.

**Caveats.**
- The chance baseline at K = 20,000 is high (0.705) because each seed's
  top-20,000 already reaches about 83% of all errors; the claim is the
  excess over chance (0.961 vs 0.705) plus the all-pairs unanimity, not the
  raw Jaccard alone.
- One era (train FY2022-23, test FY2024), national pools only. State-pool
  seed stability is unmeasured (states' own rules enter blended lists; see
  the prereg's national-only caveat).
- No dedup in this machinery (matches the section 30 comparison and the
  50,697 anchor; differs from the shipped builder), so the depth-K numbers
  describe a redundant ranking and K* is conservative relative to the
  deduped shipped list.
- Whether preference-based reordering PRESERVES PRECISION is untested; this
  study only establishes that deep coverage is seed-stable, which is the
  necessary condition.

**Artifacts.** `methods/seed_stability_v2/seed_stability_*.csv` (473d304),
driver `methods/seed_stability_v2.R`, runner `runners/run_seed_stability.R`
(f30ac9c), log `seed_stability_run.log` (untracked), caches in
`methods/seed_stability_v2/cache/` (gitignored). Review record in commit
f30ac9c's message.

### Run record

Pre-registered in `methods/preregistration_seed_stability_2026-08-05.md`
(approved before implementation; both bars, the five-seed extension rule, and
the expected outcome written down in advance). Implemented under the
principal-data-scientist skill and reviewed pre-launch by a fresh
senior-statistician subagent under the routing rule: first verdict REVISE
(checkpoint files were keyed by seed name only, so a config edit could
silently resume another seed's artifacts and misstate provenance); fixed by
keying every checkpoint on the seed value; re-verified in the file; verdict
APPROVE. Run 2026-08-05 20:19 to 22:24 (2h05m against a 3.5h estimate),
detached at high priority, log `seed_stability_run.log` (untracked). The
seed-A determinism anchor passed both parts before any mining time was spent,
and the budget readout produced zero slack warnings (the section 27
certificate held). Every number in this section re-derives from
`methods/seed_stability_v2/seed_stability_*.csv`; the run configuration is in
`seed_stability_run_info.csv`. Implementation commit f30ac9c (which carries
the review record), results commit 473d304.

## 32. Marginal precision of delivered rules: the walk's adverse selection is real, about 3 to 4 points, and not recoverable at public scale (2026-08-06)

> **Takeaway: about our pipeline (a measured answer to a fair objection).** The
> delivery walk admits any rule whose flags include at least one not-yet-covered
> case, so a rule can enter on a marginal slice that is mostly false positives.
> Measured on the shipped machinery, the effect is real but modest and already
> inside every number we quote: weighting rules by the cases they actually
> contribute, marginal precision runs 3 to 4 points below the same rules' own
> precision on the held-out year (0.314 against 0.345 at the 5% budget, 0.273
> against 0.310 at 10%). And reordering cannot recover the gap: an oracle that
> peeks at realized outcomes gains +0.175, while the deployable version, ordered
> on training-year marginal precision and scored a year ahead, gains +0.000 at
> the 5% budget and +0.011 at 10%. The median marginal slice is 1 to 2 cases,
> so no marginal statistic has support to stand on at public-data scale; the
> lower-bound walk stays.

The question, raised by Eric 2026-08-06: the fill walk is overlap-aware in
selection (a rule enters only if it adds new cases, section 27) but the test is
existence, not quality, so a rule adding one error and many clean cases is
admitted. Does that lower delivered precision, and would a marginal-quality-aware
selection do better at the same budget?

**What was measured.** For every rule on every list, two precisions on the same
caseload: its own (errors among ALL cases it flags there) and its marginal
(errors among only the cases it newly contributed when walked in rank order).
Two bases: the 98 shipped lists on their 2022-24 build caseload (the train
basis), and the 98 bench lists built on FY2022-23 with marginal slices scored on
FY2024 (the holdout basis, the honest one). The machinery reproduced the shipped
artifacts exactly before any new number was read: recomputed new-case counts
matched `n_new_at_rank` at every rank of all 196 lists, and the FY2024 walk
reproduced the committed scorecard for every state and budget.

**The scenario, counted, and the binomial check that right-sizes it.** On the
holdout basis, 54.8% of deployed rules at the 5% budget (57.8% at 10%)
contribute a marginal slice below the state's base error rate, and those slices
carry 38.6% / 39.6% of budget capacity; 54.6% / 57.1% of deployed rules add zero
errors. But the median marginal slice is 1 to 2 cases, and a 2-case slice from a
rule whose true precision is 0.30 comes up empty 49% of the time by chance
alone. Summing that over the actual slice sizes and own-precisions, the expected
zero-error share is 54.1% / 55.2%, almost exactly what is observed. The mass of
empty slices is chance on tiny samples, not evidence of selectively bad
residuals. The genuine adverse selection is the capacity-weighted gap above:
about 3 points at the 5% budget and 4 at 10%.

**Do not read the train basis.** On the build caseload the own-vs-marginal gap
looks like 13 to 15 points in the deep ranks. Train-side own precision is
selection-inflated (rules were admitted and ranked for looking good on exactly
that data, section 1) while marginal slices were never selected on, so the
train-basis gap mostly measures the winner's curse in the own column, not the
residuals. On the train basis the observed zero-error share (0.63 to 0.69) also
exceeds its binomial expectation (0.50 to 0.53); on holdout it does not.

**Reordering does not recover the gap.**

| re-walk, same rules, same capacity | 5% budget | 10% budget |
|---|---|---|
| oracle: ordered by realized marginal precision (peeks at outcomes; upper bound, unachievable) | +0.175 | +0.176 |
| deployable: ordered by FY2022-23 marginal precision, scored on FY2024 | +0.000 | +0.011 |

The oracle's gain is what sorting on the answer key buys when slices are 1 to 2
cases: it is the same arithmetic that turns raw train precision 0.20 into
deployed 0.10 (section 1). The deployable version transfers essentially nothing,
because a 1-or-2-case estimate carries almost no information about next year's
residual quality. And the repo's own support discipline cannot help: the n >= 30
floor (sections 19, 26) cannot be applied to marginal slices without emptying
the list, since the walk's deduplication is precisely what makes slices small.
Marginal-quality-aware ordering at public-data scale fails for lack of anything
estimable to order on, not for lack of trying. A state's internal data
(40k-100k rows) scales slices up roughly 30 to 50x; the question stays open
there only.

**Follow-up idea recorded, not yet tested.** A rule's section 29
characterization profile is computed on its full error set (median 126 error
cases), so profile dissimilarity between rules IS estimable where marginal
precision is not, and could serve as a proxy for complementarity if profile
distance predicts case-level complementarity out of year. Two measured cautions:
profiles converge to the national mix as support grows (sections 29, 6), so the
signal lives in narrow rules; and every ordering intervention tested against the
plain lower bound has lost or failed to transfer (sections 18, 20, 30, and this
section). A pairwise diagnostic on existing artifacts precedes any arm.


### Run record and verification

Script `methods/marginal_precision_diagnostic_v2.R` (post-processing only, no
mining; full run 0.9 minutes). Per the routing rule it was written under the
principal-data-scientist framing and reviewed by a fresh senior-statistician
before running; the review's one required fix (the train-basis oracle must vary
only the walk order: same rule set, same capacity as the shipped comparator) was
applied, and the review added the totals assertion and the binomial
expected-zeros benchmark.

Hard assertions, all passed: 98/98 shipped lists reproduced `n_flagged_state`
and `n_new_at_rank` exactly at every rank on the 2022-24 caseload; 98/98 bench
lists reproduced `n_new_at_rank` on the FY2022-23 build caseload AND the
committed FY2024 scorecard (`methods/anyerror_blended_holdout_2024/holdout_metrics.json`)
for every state and budget, including the Michigan reference (19 rules, 86
cases, 24 errors, precision 0.2791). The capacity-weighted numbers in the
takeaway are sum(k_new)/sum(n_new) against sum(own_precision x n_new)/sum(n_new)
over holdout-deployed rules (879 rules at the 5% budget, 1,511 at 10%),
re-derived independently from `per_rule_marginal.csv` before recording.

Artifacts, all in `methods/marginal_precision_diagnostic/`: `per_rule_marginal.csv`
(21,706 rows: state, budget, basis, rank, role, n_new, k_new, marginal and own
precision, LCB), `adverse_selection_by_decile.csv` and
`adverse_selection_by_state_decile.csv` (medians are zero-inflated because the
median slice is 1-2 cases; read the paired within-rule gap and the
capacity-weighted numbers), `scenario_shares.csv` (the below-base and zero-error
shares with their binomial expectations), `headroom_oracle.csv` (per state:
shipped, oracle, and deployable re-walk), `state_base_rates.csv`. Inputs:
`state_delivery_lists/` (read-only), `methods/anyerror_blended_holdout_2024/`
bench lists, `reg_model_data.rds`.

## 33. A fresh-share floor in the delivery walk clears the pre-registered bar; a candidate pending second-era replication (2026-08-06)

> **Takeaway: about our pipeline (pre-registered, one shot, cleared; NOT yet
> shipped).** Requiring each rule on a delivered list to bring at least half
> new cases (fresh share f = new cases / flagged cases >= 0.50 at its turn in
> the walk, on both core and buffer), with skipped slots refilled from deeper
> ranks, raised delivered precision by a median **+0.0118** at the 5% review
> budget across 49 states on the held-out year, against a pre-registered
> +0.010 bar, at reviewer workload identical by construction and a median
> dollar-recall change of exactly 0.0000. It is the first of five
> ordering-or-selection interventions to beat the plain LCB walk (shrinkage
> §18, stricter z §20, out-of-fold §30, and outcome-based marginal reordering
> §32 all lost or failed to transfer). The margin over the bar is thin
> (+0.0018 against a ~0.008 standard error on the median), so the pre-stated
> second-era replication (train FY2017-18, test FY2019, 18 states) is
> mandatory before any shipping decision, which is Eric's call as a MINOR
> version bump.

**The chain that produced it, in one paragraph.** Section 32 priced the
problem: the walk admits any rule adding at least one new case regardless of
the quality of its marginal slice, costing 3 to 4 points of capacity-weighted
precision, concentrated on the ~43% of capacity where fresh share falls below
0.99, and unrecoverable by outcome-based reordering (marginal slices, median
1-2 cases, support no statistic). A pre-registered six-instrument diagnostic
(`methods/profile_distance_diagnostic_plan_2026-08-06.md`) then raced
outcome-free dissimilarity measures with a split-half stability certificate as
an eligibility gate and permutation nulls per instrument: flag-overlap fresh
share was the ONE SIGNAL (tercile gap in realized FY2024 marginal precision
+0.0876 at the 5% budget against a permutation 95th percentile of 0.0656;
bottom tercile 0.245, top 0.332; certificate 0.951). Spectral co-firing,
naive-Bayes feature divergence, profile TV distance, signature distance, and a
consensus all posted large all-capacity gaps that FAILED the pre-stated
restriction to f < 0.99 capacity: their apparent signal was the fresh-share
structure itself, not incremental information. The pre-committed consequence
was this stage-2 re-walk (`methods/stage2_freshshare_rewalk_plan_2026-08-06.md`),
one shot on the section 30 bar.

**The mechanism** is a two-pass walk: a priority pass keeps a rule only if its
sequential fresh share at its turn is at least 0.50 (the threshold is the
observed lower-tercile boundary among deployed 5%-budget core instances, so it
removes approximately the tercile the diagnostic measured at 0.245); a
completion pass then walks skipped rules under the shipped test until capacity
fills, so consumed capacity equals the baseline exactly by construction
(asserted, never judged: 588 of 588 walks exact). f uses flags only, computed
where `n_new_at_rank` is computed today, so the deliverable remains the same
outcome-free frozen list and states receive no new machinery.

**The result, against the pre-stated rules:**

| pre-stated test | result |
|---|---|
| primary: median paired within-state precision difference, 5% budget, 49 states | +0.0118 vs bar +0.010: CLEARED |
| secondary: 10% budget, directional | +0.0061, positive |
| guard: median dollar-recall change at 5% (bar -0.005) | 0.0000, held |
| states better / worse / tied at 5% | 29 / 16 / 4 |

**The sensitivity grid** (descriptive only, pre-stated to carry no verdict):
f_min of 0.25 does nothing (+0.0000), 0.40 gives +0.0130, 0.50 +0.0118, 0.60
+0.0261, 0.75 +0.0145 at the 5% budget, so the effect occupies the 0.40-0.75
band rather than a knife edge. The 0.60 peak is a note for the second-era
pre-registration, not a result of this run; its own caution travels with it:
at the 10% budget, 0.60 read +0.0000 precision with a -0.0158 dollar-recall
change, a signature to check before preferring it.

**Caveats.** One era for the confirmatory readout (FY2022-23 build, FY2024
score); the margin is one-quarter of a standard error above the bar, which is
exactly why the second-era gate exists; the second-era harness has 18 states,
not 49, so its bar must be set with its own power arithmetic; the fresh-share
floor interacts with list length (median walk depth grew from 1,213 to 1,710
at the 5% budget), which is compute at build time, not state-side complexity;
and nothing here touches the LCB ordering statistic, which remains settled
(§1, §20). The one-shot rule stands: had this failed, the line was
pre-committed to close at public scale, and no alternative mechanism or
threshold was to be tried on this outcome.


### Run record and verification

Every number above re-derived from `methods/freshshare_rewalk/per_state_paired.csv`
and `sensitivity_grid.csv` before recording; the verdict was read only from the
run's `summary.txt` per the plan's launch conditions. Anchors, all passed
before any counterfactual number existed: 98/98 bench lists rebuilt
rule-for-rule at F_MIN = 0; 98/98 FY2024 refills matching
`methods/marginal_precision_diagnostic/per_rule_marginal.csv` at every rank;
98/98 frozen-core unions matching the committed scorecard. 588/588 capacity
assertions exact; 54 windowed walks redone unpruned under the corrected
certificate; the completion pass was never needed on the walks actually used.

Process record: both the diagnostic and the re-walk ran under pre-registrations
written before any result existed, implemented blind (no primary outcome
serialized on any smoke path, by construction), each REVISE'd by a fresh
senior-statistician review before launch and fixed before any number was read.
The consequential catches, recorded because each would have produced plausible
wrong numbers silently: an anti-conservative permutation null in the diagnostic
(value-shuffle where the plan required identity-shuffle-and-recompute); signal
readings computed for certificate-ineligible instruments; a float-transport
divergence between evaluators (two frame rows 1-2 ULP above rule literals,
which also surfaced and corrected four off-by-one flag counts in the committed
section 29 artifact and established reg_model_data.rds as the source of truth);
and a windowed-certificate gap under which four smoke walks would have been
certified with completion-composed lists that the full pool fills from the
priority pass alone. Eric's engineering-artifacts rule (a design-preventable
outcome is a requirement, never a judged failure mode) was adopted mid-course
and rebuilt the capacity guard into a construction assertion; it is encoded in
the fresh-review rubric and `methods/known_constraints.md#new-study`.

Scripts: `methods/profile_distance_diagnostic_v2.R` +
`methods/profile_distance_variance_join.py` (the instrument race),
`methods/freshshare_rewalk_v2.R` (stage 2). Plans:
`methods/profile_distance_diagnostic_plan_2026-08-06.md`,
`methods/stage2_freshshare_rewalk_plan_2026-08-06.md`. Full run 26 minutes;
log `freshshare_rewalk_run.log` (untracked).

## 34. The fresh-share floor replicates on the second era; two-era validated, promotion pending (2026-08-06)

> **Takeaway: about our pipeline (pre-registered replication; cleared; promotion
> is a decision, not a result).** The fresh-share floor of section 33 replicated
> a decade away: on lists built from FY2017-18 and scored on FY2019, f >= 0.50
> delivered a median paired precision gain of **+0.0070** at the 5% budget
> across 47 states against a pre-registered +0.005 bar, with the dollar guard
> held at exactly 0.0000 and the 10%-budget secondary at **+0.0110**. The
> two-era pooled median is **+0.0100**. A bridge arm first showed the effect is
> indifferent to pool composition: national-only on era 1 it reads +0.0118 at
> the 5% budget, matching the blended result to the fourth decimal, so the
> benefit is a property of the walk, not of any pool. The 0.60 challenger met
> all four of its pre-stated conditions on era 2 (median +0.0175, strictly
> above 0.50's +0.0070; guard held at -0.0033; its era-1 warning signature did
> not reproduce), so 0.60 is the proposed shipping threshold, with 0.50 the
> conservative alternative. The floor is now validated on two eras; shipping
> it is a MINOR version bump and Eric's call.

Pre-registered in `methods/era2_freshshare_replication_plan_2026-08-06.md`
(approved with a +0.005 era-2 bar whose power arithmetic is in the plan: SE
~0.0108 on the 47-state median, 74% power against the era-1 effect, joint
two-stage false-positive rate ~5.5%; the shipping claim is therefore a
"+0.010-scale effect supported by two eras jointly", and the pooled +0.0100
lands on it).

**The bridge, and the dilution hypothesis.** Eric's pre-stated hypothesis was
that the state-rule slots DILUTED the floor's benefit, predicting a
national-only bridge effect of at least the blended +0.0118. The bridge read
+0.011781: formally REFUTED by the pre-stated rule (short of the bar by
0.000019), substantively answered as "no dilution and no amplification". Two
caveats attach: the bridge pool is the undeduped admitted cache (50,697
rules) while the stage-2 bench walk consumed its deduped derivative (32,633),
so the bridge-vs-blended comparison carries dedup status as a second
difference (dedup is not walk-neutral under an f-floor); and FY2024 was read
a third time by the bridge, which is why it carries no shipping verdict. The
bridge-vs-era-2 link the shipping gate rides on is undeduped on both sides.

**Era-2 integrity.** Support reconciliation matched the plan's pre-computed
table (national build 79,907 rows / 7,115 errors; test 39,221 / 3,931;
Wyoming and South Dakota excluded by the pre-stated floor; 47 eligible). The
cache determinism anchor recomputed n/k from the frame for all 145,313 cached
FY2017-18 rules and matched exactly. Admission was recomputed with the
shipped pooled-BH recipe, not the within-stratum variant the era scripts
carried. 192 baseline-identity anchors and 960 capacity assertions passed
exactly; 193 windowed walks were redone unpruned under the corrected
certificate; the completion pass was never needed on a walk actually used,
in this run or any before it.

**The challenger, condition by condition (pre-stated, evaluated only because
the confirmatory arm cleared):** era-2 5% median +0.0175 >= +0.005; strictly
beats 0.50's +0.0070; dollar guard -0.0033 >= -0.005; and the era-1 warning
signature (10% precision <= 0 AND dollar <= -0.010) did not reproduce (era-2
10%: precision +0.0071, dollar -0.0018). All four TRUE, so the compound rule
proposes 0.60. The era-2 grid is consistent with a broad optimum around
0.60-0.75 at the 5% budget (+0.0175 / +0.0146) with positive 10% readings
throughout; the era-1 grids favored 0.60 at 5% but showed its 10% weakness,
which era 2 did not.

**What did not move.** The dollar guard read 0.0000 at the shipped threshold
on every harness (blended era 1, bridge, era 2): the floor buys precision
without giving back error dollars at the 5% budget. At 0.60 the 10%-budget
dollar costs seen on era 1 (-0.0158 unweighted on the blended grid, -0.0184
on the bridge) shrank to -0.0018 on era 2.


### Run record

Implemented on the reviewed stage-2 machinery (walk2 and the certificate
verbatim); fresh senior-statistician review APPROVE with one required change
(the dedup caveat now printed in the bridge summary block) and a ruling that
declined the runtime shortcut in favor of the smoke-covered code (it also
caught that the WINDOW_TOPK-raise variant would have made the run slower).
All seven disclosed implementation choices accepted, including the
load-bearing one: the bridge walks the undeduped admitted cache so the
bridge-to-era-2 link varies era alone. Blinding by construction: the SMOKE
path serializes nothing and never computes the branch median; the
proceed/re-scope branch was machine-read per the plan. Full run 2026-08-06
19:47 to 21:59 (2h12m), exit 0, zero failures. Verdicts read only from the
run's summary.txt per the launch conditions; medians re-derived from
per_state_paired.csv before recording (+0.011781 bridge; +0.006979 era-2 at
0.50; +0.017512 at 0.60). Scripts: methods/freshshare_rewalk_era2_v2.R.
Plan: methods/era2_freshshare_replication_plan_2026-08-06.md. Log:
freshshare_era2_run.log (untracked).

### Addendum (2026-08-07): the fine grid and the threshold selection

The pre-stated fine-grid step (methods/remine_proposal_2026-08.md) added
f in {0.55, 0.65, 0.70} on all three harnesses; every previously recorded
grid cell reproduced exactly on the rerun (30/30 across the three grids).
Selection rule, fixed before the grid was read: maximize the minimum of the
two eras' 5%-budget medians subject to the dollar guard (>= -0.005) at the
5% budget on both eras, ties to the lower f. Result: **f = 0.60** (min
+0.0175; 0.65 posted min +0.0200 but failed the era-1 5% dollar guard at
-0.0053; 0.55 and 0.70 read +0.0107 and +0.0125). The mechanical rule and
section 34's pre-registered compound rule converge on 0.60. Shipped
estimate per the rule: two-era pooled median at 0.60 = +0.0185 at the 5%
budget (96 state-pairs) and +0.0037 at 10%; the era-2-only +0.0175 is the
conservative read (the one cell never involved in any selection). Builder
knobs (names decided by Eric): SORT_WALK_USE_FRESH_SHARE (TRUE default;
FALSE restores the legacy walk and ignores the threshold) and
SORT_WALK_MIN_FRESH_SHARE (default 0.60). Full 10%-budget companions for
state internal testing are in the committed sensitivity grids.

**Absolute benchmark medians at the selected threshold (recorded 2026-08-07
for the v2.4.0 release docs; era-1 bench harness, build FY2022-23, score
FY2024, 49 states, f = 0.60, from
`methods/freshshare_rewalk/per_state_paired.csv`):** at the 5% budget,
median delivered precision 0.3256 with the floor against 0.3137 without,
median unweighted dollar recall 0.1179 against 0.1198; at the 10% budget,
0.2632 against 0.2745 precision and 0.2183 against 0.2448 dollar recall.
The 5%-budget improvement and the 10%-budget give-back are the two sides of
the 0.60 selection, whose rule optimized the 5% budget by Eric's explicit
scoping (states are most likely capacity-bound at 5%); the 10% companions
are published for states that test deeper internally. Medians of levels and
medians of paired differences do not commute, so these level figures sit
beside, not in place of, the paired readouts above.

**Threshold adjudication (2026-08-07, Eric): v2.4.0 ships f = 0.50, not the
0.60 the median-lens rule selected.** The mandatory-companion review (means
and harmed-tail counts beside the decision median) found 0.60's case
confined to the single 5%-budget median column: on within-state MEANS, 0.60
never materially beats 0.50 on any harness at any budget (best margin
+0.0005) while 0.50 wins all three 10% readouts (+0.0092/+0.0062/+0.0128 vs
+0.0047/-0.0227-tranche/+0.0069 class) and era-2 5% by double (+0.0152 vs
+0.0072); on 10% MEDIANS 0.50 leads on all three harnesses; and the harmed
tail (states worse than -0.05 paired) roughly doubles under 0.60 on all six
harness-budget readouts (5%: 3 vs 6, 4 vs 8, 2 vs 7; 10%: 0 vs 3, 2 vs 7,
1 vs 4; worst state -0.215 at era-2 5% under 0.60). A state deploys one
list and cannot average across states. 0.50 is additionally the point that
cleared both pre-registered confirmatory bars. Standing rule adopted from
this adjudication (Eric): in every future shipping decision, the median
stays the pre-registered decision statistic, and the within-state MEAN and
the HARMED-TAIL count (paired change worse than -0.05) are mandatory
companions; a median win contradicted by both companions does not ship.

**Depth-indexed threshold: coherent, frozen.** The tranche decomposition
(5%-budget fill vs the 5-to-10% increment, paired medians) shows the floor's
returns decline with depth on era 1 (f=0.60 increment -0.0098 blended,
-0.0227 national-only) but NOT on era 2 (+0.0110), so any capacity-indexed
schedule calibrated today would hard-code era 1's profile - the section 20
"failed second-era hint" shape. The zero-new-parameter schedule (0.60 to the
5%-budget depth, 0.50 beyond) is recorded as a frozen pre-registered
candidate awaiting a genuinely unread test bed (the FY2025 public file when
it lands, or a state's internal data); with all three current harnesses
consumed by its construction, testing it now would be confirmatory theater.

## 35. Vocabulary attribution: the 26-feature package has the same performance as 16-feature (2026-08-08)

> **Takeaway: about our pipeline (a technical exploration, not a research
> finding - reclassified on Eric's 2026-08-11 review).** Replacing the
> 16-feature national any-error mining vocabulary with 26 features (the
> three per-size income features plus seven FROZEN train-year percentiles)
> left FY2024 budget-list performance unchanged beyond seed noise: median
> paired precision delta **+0.0000** at the 5% budget (mean -0.0231, 2 of
> 10 states worse than -0.05), both arms on the rebuilt frame. "Frozen"
> means the percentile cutoffs were fit once on the training years
> FY2022-23 (per state x household-size cell, on CPI-deflated dollars) and
> applied unchanged to FY2024 - a construction this study introduced as a
> leakage guard. It is NOT Ben's as-built `_p` columns, which rank a case
> within all six frame years pooled (2017-19 and 2022-24), and it is
> unrelated to the frozen delivery LIST of §15. The miner
> used the new features heavily (median 85.8% of deployed candidate rules)
> with no gain: different vocabularies re-describe the same errors. With
> performance equivalent, feature membership is a judgment call on validity
> and deployment cost (the percentile features require per-state frozen
> cutoff tables; the per-size features are table-free arithmetic), not a
> performance question. The pre-registered bar-and-verdict framing this
> study originally carried overstated what was at stake; the reclassified
> record keeps the measurement and drops the verdict. An incidental
> observation about the §28 artifact, never part of the design, is
> recorded below with its limits.

The question, pre-registered in
`methods/design_note_vocab_attribution_2026-08-08.md`: does the 26-feature
vocabulary change delivered budget-list performance on a true future year
beyond seed noise? Two arms, everything else fixed:

- **baseline**: the 16 features every v2 mine has actually used. The finder
  vectors list 19, but the three `raw*_by_hh_size` names never existed in the
  frame and `prep_features()` dropped them silently - established 2026-08-08
  (the Gate-1 session), an operational hazard now recorded in
  `methods/known_constraints.md`.
- **candidate**: those 16 plus `gross_by_hh_size`, `earned_by_hh_size`,
  `unearned_by_hh_size` (per-size income) and `rawgrinc_p`, `rawearn_p`,
  `rawunearn_p`, `rawrent_p`, `rawmedded_p`, `rawcsded_p`, `rawdepded_p`
  (percentiles). Percentiles are fit on FY2022-23 only and FROZEN (per-cell
  state x reported-HH-size empirical CDFs of CPI-deflated values, zeros
  pinned to 0), then applied unchanged to FY2024 - the frame's as-built `_p`
  columns rank across all years and would leak.

Design: rebuilt 2026-08-08 frame (231,619 rows; train FY2022-23 76,031 rows
/ 8,397 any-error events; test FY2024 39,528 / 4,764 - a true future year);
shipped engines, strata, BH FDR 10% + n >= 30 admission, 99%-LCB ordering;
the legacy §31 walk (identical across arms, chosen so §31's seed-noise
yardstick applies like-for-like); budgets 5% / 10%; the §30/§31 ten-state
panel (California, Texas, Michigan, Massachusetts, Arizona, Washington,
Louisiana, Maine, New Jersey, Mississippi); seeds 117 / 20260805 / 31415
paired across arms. All 120 walk cells valid (slack 0).

Pre-registered readout (within-state median paired delta, candidate minus
baseline, seed-mean, with the mandatory companions):

| budget | median | mean | harmed tail (< -0.05) | median d dollar-recall | seed-noise ref (base / cand) |
|---|---|---|---|---|---|
| 5% | +0.0000 | -0.0231 | 2 of 10 | -0.0082 | 0.0553 / 0.0741 |
| 10% | -0.0046 | -0.0050 | 1 of 10 | -0.0068 | 0.0415 / 0.0353 |

Every delta sits inside the within-arm across-seed spread: **the measured
answer is no performance difference.** (Process history, kept for the
record: the study ran under a pre-registered do-no-harm bar and the flat
median with negative companions read as "do not adopt"; Eric adopted the
package the next day on artifact-independence grounds as the factorial's
`cand` arm; §36's representation contest then mooted that. On 2026-08-11
Eric reclassified the whole question: feature-set membership at equivalent
performance is a technical exploration ending in a judgment call, and
binding it to adoption bars created commitments that then had to be
reasoned around. The measurement stands; the verdict language does not.)

**What the candidate vocabulary did do.** Median 85.8% of deployed
candidate-arm rules reference at least one new feature; all 60 state x seed
x budget cells deploy some; `rawrent_p` and `rawmedded_p` reached admitted
rank 1 in individual seeds. Heavy usage with zero performance change is the
§30-31 pattern again: text churn, not work churn.

**The results review corrected the null reading (PDS analysis + fresh
senior-statistician, 2026-08-09):**

- The flat median hides a sign-consistent REDISTRIBUTION: Massachusetts
  negative in 6 of 6 cells on both precision and dollars (10% precision
  deltas -0.145 / -0.092 / -0.145 across seeds), Michigan 6 of 6 negative,
  Mississippi 5 of 6 positive (+0.10 precision at 10% in all three seeds).
  Within-state moves of 0.10-0.20 in both directions netting to zero is a
  worse property than a true null for a state-facing deliverable.
- **Rarity-based variable exclusion is rejected on the run's own data**:
  window share predicts nothing (the rarest window features are the
  validated incumbents homeless 1.5%, children_i 1.6%, married 2.5% - a
  rarity rule would trim them before touching rawcsded_p at 3.9%), and a
  feature's share is not stable to which other features are present.
  Vocabulary membership belongs to validity/deployability grounds plus
  measured family-level delivered effect.
- **Massachusetts is attributable displacement**: base arm stable
  (0.553-0.579 at 5%, spread 0.026), candidate collapses in two of three
  seeds (0.263 / 0.289; 10-11 of 38 flags caught vs 21-22), negative in all
  six cells including dollars, ~3.5 SE. Trust the sign, not the -0.20
  magnitude (an extreme order statistic across ten states).
- **Eric's ruling on the MA baseline (2026-08-09)**: the 0.55-0.58 level was
  artifact-inflated (§28); with the artifact diminished, lower MA results
  are expected deflation toward truth, not harm, unless the level falls
  below 0.30 (two candidate seeds sit marginally below at 5%). The
  do-not-adopt verdict rests on the primary readout, not the harmed tail.
- **New standing companions for attribution readouts**, adopted into the
  factorial design: per-state same-sign paired-cell counts (would have
  caught the MA/MI structure the median/mean/harmed-tail trio missed) and
  per-arm seed spread (the candidate's 5% spread 0.0741 vs 0.0553 is
  suggestively wider; a vocabulary that widens §31 instability is a cost
  even at equal precision).

**An incidental observation about the artifact - not part of the design,
recorded in layers (narrowed on Eric's 2026-08-11 review; his initial win
framing of 2026-08-09 is in the study README and its scope is corrected
here).** The study has no pre/post-fix arm and was never designed to
measure the fix's effect; everything in this block is opportunistic
reading of levels. §28 measured 88 delivered rules
taking 76.7% of their flags from the reconstruction band just below max
benefit, and two of three §31 seeds put a band rule at national rank 1.
Layer by layer, from measured to not-established:

- MEASURED: the 2026-08-08 rebuild's $0-tolerance recreation removed most
  of the band (in-band errors 537 to 227), and the strict-band share of
  flagged cases in this run is 0.023-0.032 median in both arms, vs the §28
  median of 0.063. At most a small share of either arm's current
  performance runs through the band itself.
- INDICATIVE, NOT PAIRED: baseline medians on the rebuilt frame (0.325 at
  5% / 0.286 at 10%, ten-state panel) sit at the shipped benchmark levels
  (49-state medians 0.314 / 0.275, old frame). This is a cross-panel,
  cross-frame comparison; it cannot establish that the fix cost nothing,
  because pre-fix levels were measured with the artifact's help on a
  different frame and state set. Directionally (Eric, 2026-08-11): the
  artifact was selection on the dependent variable, so removing it SHOULD
  deflate measured precision - pre-fix levels were inflated by
  construction. A post-fix level that holds is therefore consistent with
  either legitimate replacement rules or residual correlate inflation
  propping the level up; this design cannot distinguish the two.
- NOT ESTABLISHED: that either arm's performance is free of artifact
  CORRELATES. Near-boundary rel_max shapes still top the baseline ranking
  in all three seeds, and the candidate's rank-1 rule in seed 117 is
  itself a rel_max shape carrying a new feature
  (`rawben_rel_max > 0.998 & rawrent_p > 0.973 & ...`) - so "85.8% of
  deployed rules reference a new feature" does not mean the deployed list
  is artifact-independent; a rule can reference both. Eric's MA ruling
  (below) already located the residual inflation in correlates rather
  than the strict band; that caveat applies to the win framing too.

Caveats: ten states x three seeds supports a recommendation, not a
promotion; the candidate bundles two feature families, so this run
attributes the package, not the families (the factorial's ps_pure and
pct_pure arms are the family-level follow-up, §36); the frozen train-only
percentile construction worked as designed and is the template for any
future percentile feature.

Run 2026-08-08 21:21 to 01:20, no errors. Review: APPROVE WITH FLAGS
(`methods/vocab_attribution_review_2026-08-08.md`; launch authority
delegated by Eric). Artifacts: `methods/vocab_attribution_v2/` (README.md
carries the full record; budget readout, paired deltas, feature usage,
window usage, seed noise, run info CSVs). Script:
`methods/vocab_attribution_v2.R` + `runners/run_vocab_attribution.R`.

## 36. The vocabulary factorial: the percentile representation costs a moderate, sign-consistent amount vs per-size; a one-variable shelter test added nothing (2026-08-09)

> **Takeaway: about our pipeline (EXPLORATORY - not an established finding
> and deliberately carrying no ledger row; it informs the open v2.5.0
> vocabulary decision and nothing else. Reframed on Eric's 2026-08-11
> review).** The exploratory reading: replacing the per-size
> (`_by_hh_size`) representation of the dollar fields with FROZEN
> train-year percentiles (`_p`; the frozen construction defined in §35 -
> cutoffs fit on FY2022-23 only, NOT Ben's pooled-years design) costs a
> moderate, sign-consistent
> amount in aggregate - measured on the TEN-STATE evaluation panel only (a
> compute-driven §35 design choice; the mining is national, the walk
> readout is not). The cross-state mean paired delta
> is negative in every seed separately (**-0.026 to -0.045** at the 5%
> budget, **-0.027 to -0.037** at 10%), the 10% per-seed medians are
> negative in all three seeds, and the effect survives dropping
> Massachusetts, the worst state (10% median -0.027, mean -0.019 without
> it). At 5% the loss is a harmed tail, not a level shift: four states
> lose 0.07-0.18 (seed-means MA -0.18, NJ -0.14, MI -0.10, TX -0.07)
> against modest gains elsewhere, and the 5% median is seed-fragile
> (-0.047 / 0.000 / +0.035 by seed). Only SINGLE-state readings sit inside
> the per-state seed spread (0.055-0.093); the aggregate does not - a
> genuinely null contrast (adding per-size income to the base) posts
> per-seed aggregate means of just -0.014 to +0.014. Per-size also needs
> no per-state frozen cutoff tables. The run's other half - a 2x2 on the
> single variable `shelter_expenses_p` - added nothing (median -0.0119 on
> the base at 5%; +0.0078 on the package with mean -0.0046) despite rank-1
> admission and a quarter of deployed slots. Retrospectively, half a
> night's mining on one variable was not worth a study (the
> formality-proportionality process rule, 2026-08-11).

Design (`methods/design_note_vocab_factorial_2026-08-09.md`; review
approved, launched 18:00, chained into §37's state re-mine): six arms on
the §35 machinery - same frame, engines, admission, ordering, legacy §31
walk, ten-state panel, seeds 117 / 20260805 / 31415, budgets 5% / 10%. 360
walk cells, all valid (slack 0). The base and cand pools are §35's cached
mines reused unchanged; tonight mined the other four arms (raw vocabularies
142k-152k rules; admitted pools 50k-60k).

| arm | features | vocabulary |
|---|---|---|
| base | 16 | the shipped-in-practice set |
| cand | 26 | 16 + the §35 package |
| ps_pure | 19 | 16 + per-size income: all five `_by_hh_size` fields, zero percentiles |
| pct_pure | 23 | 16 minus its two `_by_hh_size` features, plus percentile counterparts of all five dollar fields and the four remaining component percentiles; zero `_by_hh_size` |
| base_slt | 17 | 16 + `shelter_expenses_p` |
| cand_slt | 27 | 16 + package + `shelter_expenses_p` |

The pct_pure arm removes two incumbents and adds nine columns; that is one
component by design: the NORMALIZATION REPRESENTATION of the same dollar
fields (divide-by-household-size vs rank-within-state-x-size-cell). Eric
corrected an earlier additive design on exactly this point. Pre-stated
attribution guard: pct_pure also carries four component percentiles with no
per-size counterpart, so a pct win could be representation or granularity
and would be logged as "percentile package wins", never "representation
wins" - moot, because pct lost.

**Contrast 1-2, the shelter feature (positive bar: median paired 5% delta
> 0, not contradicted by mean and harmed tail, with real deployed usage -
positive rather than do-no-harm because the feature alone extends the
per-state frozen cutoff tables).** Its pre-screen had passed cleanly
(standalone `shelter_expenses_p > 0.99` on train: precision 0.214 vs base
0.110; FY2024 holdout 0.230 vs 0.121). Delivered, it failed:

| contrast | budget | median | mean | harmed tail | median d dollars |
|---|---|---|---|---|---|
| base_slt - base | 5% | -0.0119 | -0.0122 | 1 of 10 | -0.0020 |
| base_slt - base | 10% | -0.0091 | -0.0048 | 0 of 10 | -0.0080 |
| cand_slt - cand | 5% | +0.0078 | -0.0046 | 1 of 10 | +0.0069 |
| cand_slt - cand | 10% | -0.0097 | -0.0017 | 1 of 10 | +0.0095 |

Usage was real - the null is not non-deployment: shelter rules reached
admitted rank 1 in two of three base_slt seeds, every one of the 120 slt-arm
cells deployed at least one, and the median deployed share in base_slt was
25.3% at the 5% budget / 26.7% at 10% (cand_slt 9.8% / 11.6%). A
quarter of the list turned over to shelter rules and delivered precision
went nowhere or down. The feature is dropped. (Reclassified 2026-08-11: a
pre-registered bar on a single variable was disproportionate formality -
six of the night's twelve fresh mines went to these two arms. The record
keeps the measurement, adding the feature changed nothing, and drops the
verdict framing.)

**Contrasts 4-6, the representation contest (per-state seed-mean, then
across-state summary; sign counts over the 6 cells per state):**

| contrast | budget | median | mean | harmed tail | median d dollars |
|---|---|---|---|---|---|
| ps_pure - base | 5% | -0.0080 | -0.0061 | 1 of 10 | -0.0010 |
| ps_pure - base | 10% | +0.0019 | +0.0091 | 0 of 10 | -0.0105 |
| pct_pure - base | 5% | -0.0152 | -0.0426 | 4 of 10 | -0.0162 |
| pct_pure - base | 10% | -0.0270 | -0.0237 | 4 of 10 | -0.0148 |
| pct_pure - ps_pure | 5% | -0.0113 | -0.0366 | 4 of 10 | -0.0090 |
| pct_pure - ps_pure | 10% | -0.0375 | -0.0328 | 4 of 10 | -0.0237 |

Adding the per-size income features to base (ps_pure) is within noise and
harms nothing. The percentile replacement is not noise at the aggregate
level. Computing the ten-state aggregate separately within each seed (no
seed averaging) shows the sign is stable and the magnitude moderate:

| seed | 5% median | 5% mean | 10% median | 10% mean |
|---|---|---|---|---|
| 117 | -0.047 | -0.045 | -0.006 | -0.034 |
| 20260805 | 0.000 | -0.039 | -0.035 | -0.027 |
| 31415 | +0.035 | -0.026 | -0.038 | -0.037 |

The mean is negative in all six seed-budget cells; the 10% median in all
three seeds. Excluding Massachusetts (the worst state) still leaves the
10% aggregate at median -0.027 / mean -0.019, and the 5% mean at -0.021 -
though the 5% MEDIAN flips to +0.015 without MA, which is why the 5%-budget
loss is properly described as a harmed tail (four states lose 0.07-0.18 on
seed-means: MA -0.176, NJ -0.140, MI -0.095, TX -0.073; the largest gain is
Arizona +0.070), not a level shift. Calibration for what a null contrast
looks like on this panel: persize_on_base's per-seed aggregate means span
only -0.014 to +0.014. The per-state seed spread (0.055-0.093) is the
yardstick for a SINGLE state's reading, not for these aggregates.
(Per-seed aggregates derived 2026-08-11 from `factorial_paired_deltas.csv`
after Eric challenged the earlier "near seed-noise" framing, which had
applied the single-cell yardstick to the aggregate.) Sign consistency (the
§35 standing
companion): Massachusetts 5 of 6 cells negative (mean -0.1667), New Jersey
5 of 6 (all non-positive, -0.1048), Texas 5 of 6 (-0.0607), Michigan 5 of
6 (-0.0774); positive states are smaller (Arizona +0.0426, Washington
+0.0301). Absolute FY2024 medians across cells: base 0.325 / 0.285,
ps_pure 0.301 / 0.305, pct_pure 0.292 / 0.279, cand 0.302 / 0.286.
pct_pure also posts the widest 5% seed spread (0.093 vs base 0.0553).

Contrast 3 (cand - base) reproduced §35's numbers exactly - shared cached
pools re-walked, a machinery check, not an independent replication.

**Where this leaves the v2.5.0 national vocabulary**: the percentile
replacement tested HERE (frozen train-year fit) costs a moderate,
sign-consistent amount on the panel, so the per-size vocabulary (ps: 16 +
the three per-size income features, no percentiles) is the v2.5.0
candidate for now. Note the scope of what was tested: this arm used the
FROZEN train-only percentile construction, not Ben's pooled-years
within-state construction - the new §37 exploratory study tests his
actual design. The national production mine is Eric's call at regen time.

Scope, stated squarely: one era (train FY2022-23, test FY2024), TEN
evaluation states, three seeds. Ten states is thin for an effect whose 5%
form is a four-state harmed tail - the panel was a §35 compute trade-off
(a 49-state walk across all the pools would not fit the overnight window),
not a power calculation. All 18 factorial pools are cached, so a 49-state
walk readout of this contrast is an evaluation-only rerun, no mining
needed; Eric deferred it 2026-08-11 as not important enough right now. No
second era has been run.

Artifacts: `methods/vocab_factorial_v2/` (budget readout with 360 cells,
paired deltas, sign consistency, feature usage, seed noise, run info;
`run1_4arm/` archives the interim 4-arm pass, identical shared cells).
Script: `methods/vocab_factorial_v2.R` + `runners/run_vocab_factorial.R`.
Review: `methods/vocab_factorial_review_2026-08-09.md`.


## 37. Exploratory: Ben's within-state percentiles at state scale - precision a wash, and the miner does not use them as outlier detectors (2026-08-11)

> **Takeaway: about our pipeline (EXPLORATORY - no ledger rows; single
> seed, one era; informs the v2.5.0 vocabulary decision and nothing
> else).** Adding Ben's seven as-built within-state percentile features
> (pooled six-year fit, no override) to the 19-feature per-size vocabulary
> left within-state FY2024 budget precision a wash across 48 paired
> states: median paired delta **+0.0000** at the 5% budget (mean +0.0007,
> 9 states harmed worse than -0.05 vs 10 helped better than +0.05 - a
> redistribution, not equivalence), **-0.0041** at 10% (mean -0.0052),
> with dollar recall slightly favoring the percentile arm at 10% (median
> +0.0062). The direct answer to the outlier framing: of **495** `_p`
> conditions in deployed rules, exactly **2 (0.4%)** are high-tail
> conditions (> with threshold >= 0.90). The miner uses the features
> heavily - a median 75% of deployed rules reference one, and
> `_p`-rule-flagged cases run HIGHER precision than the non-`_p` rules
> beside them (0.281 vs 0.168 at 5%) - but as mid-scale income and rent
> encodings (median `>` threshold 0.254; half the conditions are
> low-side) and as zero/absence flags on the deduction variables, not as
> outlier detectors.

Design: `methods/design_note_state_pctl_runoff_2026-08-11.md` (with the
post-review addendum). Every design decision was Eric's, made
interactively 2026-08-11; this replaced the deleted 2026-08-10 state
re-mine, whose percentile arm used the frozen train-only construction
(defined in section 35) rather than Ben's design. Arms, additive by
Eric's ruling: **persize** = the 16 shipped-in-practice features + the 3
per-size income features (19); **benp** = persize + Ben's 7 as-built
`_p` columns (26; features.R construction, CPI-deflated dollars ranked
within state x household-size cells across all six frame years pooled,
zeros pinned to 0, used AS BUILT). Any-error frame x coarse HH strata
mined PER STATE on FY2022-23; joint BH FDR 10% with per-stratum base
rates AND n >= 30; 99%-LCB ordering; the shipped v2.4.0 fresh-share walk
(f = 0.50) filled FY2022-23 to 5%/10% core + 3x buffer, froze, walked
FY2024 in delivered order to that year's cap, outcome-free; seed 117,
single seed. Support: printed per state x stratum (roughly 300-1,200
rows, 5-110 events per stratum). Run 16:09-18:51 (2h42m), 49 states,
no errors.

**Paired readout (benp - persize; 48 paired states; Wyoming admits
nothing under either arm):**

| budget | median | mean | harmed (< -0.05) | helped (> +0.05) | d dollars median / mean |
|---|---|---|---|---|---|
| 5% | +0.0000 | +0.0007 | 9 | 10 | +0.0014 / +0.0030 |
| 10% | -0.0041 | -0.0052 | 5 | 2 | +0.0062 / +0.0084 |

Absolute within-state medians: persize 0.2945 at 5% / 0.2677 at 10%;
benp 0.2840 / 0.2624 (dollar recall 0.1118/0.2335 vs 0.1036/0.2408).
Per the pre-named rule this is REDISTRIBUTION: 19 states move more than
0.05 in magnitude at 5% (Delaware -0.2418 through Connecticut +0.1591),
netting to a flat median. The incremental-catch file shows the same
symmetry: at 5% benp uniquely catches 235 errors nationally, persize
uniquely catches 234, 288 shared; at 10%: 329 / 343 / 690.

**What the percentile rules actually catch (the flag-profile layer,
designed by the fresh senior-statistician review per Eric's brief):**

- Condition inventory (495 `_p` conditions across 361 deployed
  rule-instances): ops split 224 `>` / 32 `>=` / 212 `<=` / 27 `<`;
  median threshold for `>` conditions 0.254 (q25 0.108, q75 0.485);
  exactly 2 conditions are `>`-side with threshold >= 0.90. Variables:
  rawgrinc_p 142, rawrent_p 109, rawunearn_p 104, rawearn_p 95; the
  three deduction percentiles total 45, and cases flagged by rules using
  them sit at percentile 0 (the pinned-zero mass; median flagged-zero
  share 0.98-1.00) - absence-of-deduction encoding, not extremity.
- Flagged cases are not tail-concentrated: the median share of flagged
  cases above p90 is 0.00 for every variable except rawrent_p (0.16,
  against a caseload share of 0.087; error cases 0.157). benp's UNIQUE
  catches sit in the tails even less than its shared catches (rent 0.096
  vs 0.174).
- The `_p`-using rules do earn their slots on precision: median
  precision of `_p`-rule-flagged cases 0.281 vs 0.168 for non-`_p`
  deployed rules at 5% (0.270 vs 0.236 at 10%), carrying a median 94% of
  caught dollars at 5% - though `_p` rules are also the majority of the
  deployed list (median 3 of 4 rules at 5%).
- Interpretation hazard (carried verbatim from the readout notes): `_p`
  conditions are conjoined with non-percentile conditions, so "flagged
  by a rule using `_p`" is rule-level, not causal, attribution.

**Machinery record.** The fresh senior-statistician review (routing rule)
returned REVISE and both blocking items mattered: (B1) the evaluation
walk initially passed an "all" stratum list while rules are admitted per
stratum, so `flags_for_rules()` silently applied stratum rules
unrestricted - fixed before launch, evaluation strata now mirror mining
strata; (B2/B3) the smoke run showed the shipped walk's exact-refill
capacity assertion is an EMPIRICAL property of large blended pools, not
a guarantee - at state-pool scale the completion pass stranded a 1-case
remainder on the first smoke state. Treatment (reviewed): tolerate and
report per phase, never modify the walk, never crash. In the full run
the gaps were negligible: 3 of 96 cells at 5% and 1 of 96 at 10%
nonzero, max 2 cases (1.65% of its fill target), so the deviation
carried no comparability cost.

Caveats: single seed (state-level moves are inside mining seed noise; no
individual state's delta is readable), one era, and the information
asymmetry pre-stated in the design note ran in benp's favor (its pooled
percentile fit includes FY2024), which if anything leans the flat result
against the percentile arm. Small-cell degeneracy of the construction at
public state scale is quantified in the companion map
(`percentile_value_map_fy2024.csv`, n_distinct column).

Artifacts: `methods/state_percentile_runoff_v2/` (readout, paired table
for both budgets, condition inventory, p-rule catch, per-variable
profile, incremental catch, README_readout_notes.md, the FY2024
percentile-to-value map with distinct-value counts). Script:
`methods/state_percentile_runoff_v2.R` + `runners/run_state_percentile_runoff.R`.
Log: pctl_runoff.log (untracked). Review record: in the design note's
addendum.
