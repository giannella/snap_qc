# SNAP QC rule mining — findings: complete detailed record (July 2026 runs)

> This is the **full evidence log**: every number, table, caveat, and artifact
> pointer, in the order the work happened. It is the source of truth and is kept
> deliberately dense. If you are here to learn the key points, read the shorter
> **[modeling_findings.md](modeling_findings.md)** instead — it carries the same
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

- **About the data** — something that appears to be true of SNAP QC data itself and
  would probably show up no matter how you built the model. Treat these as portable
  lessons.
- **About our pipeline** — a choice that made *our* system better on *our* tests.
  These earned their place with numbers, but they are engineering decisions, not
  laws of nature; don't assume they carry over to a different setup.

Some sections carry a little of both; the tag marks the main point and the takeaway
notes the rest.

**Glossary of recurring terms** (skip if you know them):

- **Error / "over threshold."** A QC case counts as an error when its payment was off
  by more than the tolerance threshold. Base rates are low — about 11% of cases
  nationally have any error; typed categories run 0.4-6%.
- **Precision.** Of the cases a rule flags, the share that are truly errors — "is
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
  training precision (which is optimistic — see section 1), we rank and filter on a
  statistical *lower* bound: "with high confidence, this rule's true precision is at
  least X." `z` sets how cautious the bound is; z = 2.326 is the 99% bound, our
  production setting. **Support (n)** is how many cases a rule flags in training; small
  n means an unreliable estimate, so we also require n >= 30.
- **Winner's curse.** If you pick the rules that scored best on the same data you
  scored them on, you are partly picking luck, and they look worse in deployment.
  Avoiding it is a running theme (sections 1, 9, 17, 19, 22).
- **Engines (xgboost, ranger).** We don't use these as predictors. We grow tree
  ensembles and harvest each branch as a candidate rule — they are just rule
  *generators*.

---

## 0. How the work unfolded (chronology + breadcrumbs)

One line per step, in the order it actually happened, so the story of what
we learned is recoverable. Each points to its numbered section.

- **07-04/05** — Diagnosed the winner's curse in raw-precision shortlists and
  adopted the Wilson LCB as the selection statistic (#1). Dropped {pre} for
  the v2 xgboost+ranger pipeline (#2). Typed-vs-pooled mining (#3), engine
  tuning grid (#4), and "mine big, filter stringently" (#5) followed on the
  same holdout (train 2022+2024, test 2023).
- **07-06** — Engine-pair studies settled xgboost+ranger (#2). Seven-state
  threshold grid search produced the original two-regime rule and the
  state-scale support-floor lesson (#9). Louisiana neighbor-transfer and
  single-state mining studies (#9). HH-strata v2 confirmation (#11).
- **07-07** — Rebuilt the modelling frame: multi-element error cases restored
  (~31% of errors), deduction NAs zero-filled; rule content survived
  (93%), inventory ~3x (effects_of_munging_options.md). Per-state data
  VISIBILITY accounting (#10). Floor-definition calibration figure (#1).
- **07-08** — State packages re-run on the rebuilt frame; exclusion pipeline
  moved to a relative safety standard; partition-aware threshold variants
  cut grid-search waste; lessons deck started.
- **07-09** — State-similarity program (fire-rate / IDF / policy / blended /
  NB definitions, per era) and the same-era transfer benchmark with the
  honest LOO baseline (#12). Review-budget evaluation replaced floor-only
  reporting (#12). Senior-statistician critique (pipeline_critique_
  2026-07-09.md) prompted the pre-registered year-swap replication: 3 of 4
  selection claims replicated, subsample claim retired (#13).
- **07-10** — Deployment-grade benchmark (train 2022-23, test 2024):
  national_all is the honest default; same-era transfer advantage did not
  survive; own-state mining is high-variance (#14). Four state-adaptation
  schemes tested — none beat the national ordering on the median (#9 note,
  #14). Contributing-rules analysis: unions are built by dozens of rules,
  not thousands (#15). Frozen per-state lists + ranked buffer, walked to
  capacity (#15). Blend of state+national pools on the LCB scale becomes
  the default deliverable, own-pool list the fallback (#16). Delivery lists
  built for CT + 8 more states (custom_one_off/, gitignored).
- **07-12** — Guidance refreshed (README, CLAUDE.md, this header); chart
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
> estimates roughly honest about held-out performance, and it will do the same for
> anyone mining rules this way.

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

*Artifacts: methods/compare_models_by_HHsize_vs_pooled/ (rawstat_ vs unprefixed runs).*

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

> **Takeaway: about our pipeline.** Mining the four error-type datasets separately
> beats a single all-errors model, but only by about a percentage point, and one
> all-errors model gets ~95% of the way at a quarter of the cost. Because the two
> approaches find largely different rules, the best move is to mine both and pool
> them; at a fixed precision floor that can only add catches, and the precision cost
> is small. This held up on an independent test year.

Three arms are compared, all scored on ALL 2023 errors with identical machinery
and selection:
- **Typed** — rules mined from the four separate error-type frames, pooled.
- **Any-error** — rules from a single model whose target is *any* error.
- **Combined** — Typed and Any-error pooled together and de-duplicated.

- **Typed wins on precision, but barely** — mean precision at matched recall 0.177
  (Typed) vs 0.167 (Any-error), a ~1pp edge. The single Any-error model reaches ~95%
  of typed's performance at 1/4 the mining cost, and neither parent dominates on
  recall (Typed reaches more at loose floors, Any-error slightly more at strict
  floors).
- **The vocabularies complement**: Combined beats BOTH parents on recall at
  every FIXED filter floor (only ~7% cross-pool overlap). Best practice: mine
  both, pool, dedup — cheap on the v2 stack. Hold-out recall of all 2023 errors,
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

> **Takeaway: about our pipeline.** Most tuning knobs barely matter. The few that do:
> give the random forest a little signal to split on (mtry = 2, not 1), use slow,
> low-sample boosting, and grow trees to depth 4-5. More trees don't buy precision at
> a fixed filter setting — they buy a bigger *menu* of rules, which pays off only when
> you also filter more strictly (section 5).

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
- **xgboost: slow eta, low subsample.** eta 0.02 (0.217) beats eta 0.1 (0.212);
  low subsample (0.15-0.30, e.g. 0.20 -> 0.218) beats high (0.60-0.80, e.g.
  0.75 -> 0.208) — echoing the old rpart sampfrac finding; values within
  0.15-0.30 are statistically indistinguishable. Both results are independent
  of the filter setting; they are the production defaults (eta .02,
  subsample .20). (The low-vs-high subsample edge did NOT replicate on 2024;
  see §13 — 0.20 stays as "as good as any," not proven best.)
- **Round count only looks like it matters at a loose filter.** At a fixed
  90% LCB, 100 rounds beat 1000 on the frontier (0.217 vs 0.198) — but most of
  that gap is the selection-multiplicity dilution that §5 shows is
  correctable: at each pool's appropriate stringency (small @ 90%, big @ 99%)
  the two trace essentially the SAME hold-out frontier. What mining big buys
  is the MENU behind each operating point — ~2.6x the rules pass any floor in
  this experiment (~5x at production scale with both engines), each with a
  stiffer per-rule guarantee — not extra portfolio precision. Production:
  1000 rounds — "mine big, filter stringently."
- Depth 5 beats depth 3 clearly for ranger (0.203 -> 0.213) but only
  marginally for xgboost (0.210 -> 0.211); production uses depth 4. Inventory
  (shortlist size) and frontier quality often DISAGREE — e.g. subsample 0.75
  gives more rules but a worse frontier.

*Artifacts: methods/parameter_tuning_v2/v2_tuning_{ranger,xgboost}.png, summary CSVs,
v2_subsample_fine.*

## 5. "Mine big, filter stringently" — the flexible LCB

> **Takeaway: about our pipeline.** Mining a big pool of rules and then filtering it
> hard lands on the same accuracy as mining a small pool and filtering it gently — the
> big pool's advantage isn't better numbers, it's a longer list of usable rules, so
> states have substitutes when they veto one on expert judgment. The strict filter
> (the 99% bound) is what keeps the big pool from drowning in lucky rules.

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

> **Takeaway: about the data.** A rule you mined to catch one kind of error routinely
> flags cases that have some *other* error too, and in real review that still counts
> as a hit. So a rule's real-world precision runs about 2x its narrow, single-type
> precision. Always quote states the any-error number — the narrow one understates
> what they would actually see.

A rule mined for one error type flags cases whose OTHER errors count as wins
in deployment. Any-error precision runs ~2-2.7x the frame-relative number
(e.g. earned union at the 0.20 floor: 0.080 frame vs 0.178 any-error). All v2
outputs carry both views; quote the any-error numbers to states.

## 7. other_error: the largest, previously unmodeled category

> **Takeaway: about the data.** The biggest single category of SNAP errors is the
> "other" bucket (deductions, shelter, household composition) — larger than any of the
> classic income-error types — and nobody had tried to model it. It turns out to have
> plenty of learnable structure. One caveat from the program side: many states treat
> these as small-dollar, low-priority errors, so "we can find them" is a completeness
> win, not a headline.

other_error (deductions, shelter, household composition; 1,377 of 2,994 total
2023 errors — more than any typed category) had never been mined. It produced
the single largest filtered-in block (1,082 rules, median hold-out 0.212) —
heterogeneous or not, it has learnable structure.

## 8. ESAP / elderly-disabled: feature suffices, and why

> **Takeaway: about the data.** Elderly and disabled households (ESAP = the Elderly
> Simplified Application Project population) are about half the caseload but are *not*
> more error-prone. What differs is the *mix* of their errors — mostly the
> easy-to-detect types — which is why our models catch far more of their errors (~27%
> vs ~7% for other households). The hard, still-open problem is working households
> with volatile earned income. Practical upshot: this group did not need its own
> model; letting the ensemble see it as a feature was enough.

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

> **Takeaway: about our pipeline (with one portable caution).** When a state has
> enough of its own data (roughly 30+ rules clearing the bar), tuning rules on that
> state's data can pay off well; when it doesn't, tuning collapses to noise and you
> should just deploy the national rules unchanged. The portable part is the caution:
> at small sample sizes, tuning is pure winner's curse, so we enforce a hard support
> floor (n >= 30). *(Partly superseded — the current deployment recipe is the blended
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

*(2026-07-10 update — partially superseded: re-run in the deployment setting
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

*Artifacts: archive/state_rules_v2/ (per-state rule CSVs with national + tuned
thresholds side by side; state_union_summary.csv; LCB-criterion run preserved
in run1_lcb_criterion/).*

## 10. Data visibility: how much of a state's error population the public
## data can even show (2026-07-07)

> **Takeaway: about the data (and it matters a lot).** The public QC file does not
> show a state its whole error population — it excludes ineligible cases entirely, so
> a state sees only 43-81% of its own errors (New Jersey: 43%, Tennessee: 51%). Any
> rule mined on public data is therefore blind to a large slice of reality. States
> below roughly 60% visibility should treat public/national rules as a *supplement*
> and run the pipeline on their own internal data.

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

> **Takeaway: about our pipeline.** Splitting the caseload by household size
> (1 / 2-3 / 4+) and mining each group separately reliably helps, or at worst never
> hurts, so it is our default. Splitting *finer* (a 5-way split) does not help and
> costs more compute — past a point, smaller groups just starve each rule of the cases
> it needs to clear the bar.

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

*Artifacts: methods/compare_models_by_HHsize_vs_pooled/strata_earn_inc_scheme_summary.csv
(pre-era); methods/compare_hh_strata_v2/ (v2 confirmation).*

**Year-swap re-test (2026-07-13, train 2022+2023, test 2024 -- PARTIAL
replication):** the 2023 verdict "pooling matches the split's precision"
did not hold on 2024 -- there the 1/2-3/4+ split wins mean precision at
matched recall (0.302 vs 0.262 pooled) while pooling wins reach at the
0.20 floor (0.844 vs 0.794 dollar recall). Consistent across both years:
the coarse split never loses, so it stays the default. NOT replicated:
"5-way is worse" -- on 2024 the 5-way ties the 3-way (0.304 vs 0.302) at
~1.6x the compute; the claim softens to "no better, costlier."
*Artifacts: methods/compare_hh_strata_v2/yearswap_train2223_test24/
(strata_summary.csv, strata_sweeps.png; methods/run_strata_yearswap.R).*

## 12. Cross-state transfer vs honest national baselines (2026-07-09)

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

Deployment guidance *(SUPERSEDED by section 14 on 2026-07-10: re-tested with
a true temporal split, the 10%-budget similarity-pool advantage did not
survive — the national pool leads at both budgets on 2024, and with an
unseen test year the in-sample-flattering concern also dissolves. Kept as
originally written for the record)*: the production national shortlist
remains the best single list at small budgets, but numbers quoted to a
state from in-sample national training overstate honest performance by up
to the as-is-vs-LOO gap; at moderate budgets (~10%), similarity-picked
donor pools (fire or NB) are competitive with any national option and are
the honest choice where a state's own data must stay out of training.

*Artifacts: methods/state_similarity_v2/transfer_benchmark/ (benchmark +
budgeted_menu_results.csv); methods/state_similarity_v2/similarity_*_2022_2024.csv
and _2017_2019.csv; methods/state_nb_similarity_v2.R; methods/neighbor_transfer_benchmark_v2.R;
methods/budgeted_transfer_menu_v2.R; overnight_nb_loo_run.log.*

## 13. Pre-registered year-swap replication of the model-selection studies (2026-07-09)

> **Takeaway: about our pipeline (and our honesty check on it).** Every modeling
> choice above had been judged on one test year (2023). We wrote our predictions down
> in advance and re-ran the four big ones on a fresh year (2024): three held up, and
> one ("low subsampling helps") did not and was retired. The value here is the
> discipline — pre-committing to predictions is what lets you tell a real effect from
> a lucky one.

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

> **Takeaway: about our pipeline (with one portable caution).** Tested the way a
> state would actually face it — rules built on past years, scored on a future year —
> the plain national rule list is the best default: the highest precision at both
> budgets among the lists a state can actually deploy, and never a disaster. Mining on a state's *own* data has the biggest upside but is
> high-variance and can fail below the random-review base rate (Washington did).
> Portable caution: don't assume a state's own rules beat the national ones; make the
> state confirm it on their own held-out year. ("NB transfer" = a pool of a few
> statistically similar states, picked by a naive-Bayes similarity measure.)

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

> **Takeaway: about our pipeline.** A state can't wait for the test year — it needs a
> fixed list in hand. Freezing the national list and sizing it against the state's own
> caseload (with a buffer so reviewers never run dry) costs almost nothing versus an
> idealized after-the-fact list: under a point of precision. Each list personalizes
> itself through the state's own case mix — about a third of every state's list is
> unique to it.

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

> **Takeaway: about our pipeline (the current default deliverable).** Put each
> state's own mined rules and the national rules on one honest confidence scale (the
> 99% bound) and let them interleave. This "blend" is the recipe we now ship: better
> at a 5% budget, about even at 10% (0.262 vs 0.270 precision), and no case-by-case
> decision to defend. Its one
> blind spot — a national rule's bound says nothing about whether it *transfers* to a
> given state — is why we keep the state's own-rules list as a fallback where their
> internal validation shows the blend underperforming.

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

## 17. Typed-frame delivery vocabulary: retired after three rescue attempts (2026-07-15/16)

> **Takeaway: about our pipeline.** Adding the four typed datasets to the delivery
> pool tripled the candidate rules but *lowered* delivered precision, and three
> attempts to rescue it failed. The reason is general enough to remember: when you must
> pick only 20-50 rules to fit a review budget, a bigger pool mostly adds small-sample,
> lucky-looking rules that crowd out the honest ones. The filter-floor advantage of
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
precision candidates displace honest rules at the top. Ten five-frame lists
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
0.298 — tracks the bound, slightly worse). A repaired variant for the era
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
> whisper a false signal — the second era is what settled it.

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
> does — error size seems anchored to observable case traits. Ranking by dollars beat
> ranking by precision on dollar recall in 2024, but the gain shrank on the 2017-19
> replication and missed our pre-set bar, so we did not adopt it. Recorded as a real
> but era-unstable direction worth revisiting.

Groundwork: per-rule error dollars per flagged case persist train->test
MORE strongly than precision in every support band (train 2022-23 pools
scored on 2024; Spearman 0.560 / 0.699 / 0.789 / 0.677 for support bands
30-60 / 61-120 / 121-300 / 300+, vs 0.498 / 0.634 / 0.708 / 0.672 for
precision; 169,402 rule-state pairs) — error magnitude is anchored to
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
> gap was concentrated in the very top rules — exactly where a tight review budget
> lives. It is the winner's curse of section 1, shown directly at the point of the
> list that matters most.

Equal-footing cross-fit on 2017-18 (identical half-mined vocabulary,
identical admitted set of 66,540 rules): ordering by the UNTOUCHED half's
Wilson bound beats ordering by the MINING half's bound by +1.6pp median
precision at the 5% budget on 2019 (0.216 vs 0.200) — the selection bias
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
