# SNAP QC rule mining: key findings and syntheses

This file is the plain-language version of the findings; the full per-run numbers,
tables, and artifact paths for every section live in the companion
**[detailed record](modeling_findings_detailed.md)**. Methods details live in
`methods/design_drop_pre_architecture.md`. Sections 1-13 are hold-out numbers (train
2022+2024, test 2023) unless noted; the deployment sections (14-16) train on
2022+2023 and test on 2024, a year that never influenced any design decision.
Current deployment guidance lives in sections 14-16 (and the README): the blended
frozen list is the default deliverable; earlier per-state guidance in sections 9 and
12 is partially superseded and carries notes where so.

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
  nationally have any error, and typed categories run 0.4-6%.
- **Precision.** Of the cases a rule flags, the share that are truly errors ("is
  flagging this worth a reviewer's time?").
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
  training precision (which is optimistic; see section 1), we rank and filter on a
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

## 0. How the work unfolded

The blow-by-blow chronology (every step in the order it happened, with breadcrumbs
to each experiment) is kept in the companion
[detailed record](modeling_findings_detailed.md) (§0). In brief: we started by
diagnosing the winner's curse and adopting the confidence-bound filter (sections 1
and 5), built and tuned the v2 pipeline and its engines and strata (2-4, 11), then
spent the deployment phase (9, 12, 14-22) working out what to actually hand a state,
pre-registering replications along the way to keep ourselves honest.

## 1. The winner's curse, diagnosed and addressed

> **Takeaway: about the data (a statistical fact you'll hit too).** If you
> shortlist rules by their raw accuracy on the same data you measured them on, you
> reward luck: rules that looked ~20% accurate came in around ~10% on fresh data.
> The cure is to rank and filter on a cautious *lower* bound of each rule's precision
> (the Wilson bound) instead of the raw number. That one change made our training
> estimates roughly honest about held-out performance, and it will do the same for
> anyone mining rules this way.

Here is the evidence. Take the rules whose training precision was at least 0.20 and
look at how they did on a held-out year: the median came in around 0.10, half of
what the training number promised. We then checked *why*, and it is almost entirely
selection luck, not the model overfitting or the world changing:

- Rules examined *without* any precision filter show almost no train-to-holdout gap
  (median gap -0.003, correlation 0.83). The training estimate is honest until you
  start selecting on it.
- Select on the *holdout* instead, and the bias flips: those rules have median
  *training* precision 0.116. That symmetry is textbook regression to the mean.
- The same rules give similar lift across very different years (~3.9x on 2018-19,
  ~3.5x on 2023), so year-to-year drift is only a minor part of the gap.

The fix is to rank and filter on the one-sided Wilson lower confidence bound of a
rule's training precision (a cautious "at least this good" figure) rather than the
raw estimate. At matched deployed precision (~0.20) that catches 12.8% of all errors
versus 8.2% for a raw threshold, and it makes the training number roughly honest
about what the holdout will show.

One practical corollary for setting a cutoff: even after the bound weeds out the
junk, a floor written on *raw* precision still overpromises (a raw 0.40 floor
delivered 0.33), while a floor written on the *bound itself* reads honestly (a 0.30
bound-floor delivered 0.38). Set your floor on the bound.

*Full numbers and the calibration figure: [detailed record](modeling_findings_detailed.md), §1.*

## 2. Results of dropping {pre} r package in favor of rolling our own

> **Takeaway: about our pipeline.** Replacing the off-the-shelf `pre` R package with
> our own rule generator gave the same rule quality at a fraction of the cost: it
> mines all four typed frames in about the time `pre` took to mine one, and its peak
> memory drops from over 40 GB to a few GB. It also made analyses possible that `pre`
> simply could not fit: the pooled any-error model, 853k-rule comparisons, a
> regression test. Worth knowing: swapping the underlying tree engines barely moved
> precision (about 1 point). The real gains come from strict filtering and from
> scoring on any-error, not from the choice of algorithm.

The head-to-head, everything else held equal:

- **Quality is a wash.** On the earned frame, `pre` kept 68 rules at median holdout
  precision 0.134; our generator kept 29 rules at 0.157, matching pre's quality from
  a smaller rule set.
- **Cost is not.** `pre` peaked above 40 GB of memory on a single frame (it builds an
  internal lasso matrix even when we never use the output) and took ~40 minutes; our
  version runs all four frames in a few GB and ~45 minutes, on a 16 GB laptop.
- **It unlocked work `pre` could not fit:** the pooled any-error model, the
  `other_error` frame, 853k-rule comparisons, coverage-and-dominance de-duplication,
  and a regression test.

What the switch did *not* buy is more signal: the best honest per-rule holdout
precision by frame still tops out around 0.31-0.48. And when we later raced the tree
engines directly (xgboost+ranger vs bagged trees + ranger vs single engines), the
best pair only edged the alternatives by about a point of precision. The lesson: the
engine is not where the leverage is; strict filtering and any-error scoring are.

*Full engine sweep and numbers: [detailed record](modeling_findings_detailed.md), §2.*

## 3. Typed frames vs one any-error model (head-to-head)

> **Takeaway: about our pipeline.** Mining the four error-type datasets separately
> beats a single all-errors model, but only by about a percentage point, and one
> all-errors model gets ~95% of the way at a quarter of the cost. Because the two
> approaches find largely different rules, the best move is to mine both and pool
> them; at a fixed precision floor that can only add catches, and the precision cost
> is small. This held up on an independent test year.

There are three things being compared here, so let me name them:

- **Typed**: the rules mined from the four separate error-type frames, pooled.
- **Any-error**: the rules from a single model whose target is *any* error.
- **Combined**: Typed and Any-error pooled together and de-duplicated.

Scored against *all* 2023 errors, with identical machinery:

- **The two parents are close, and neither dominates.** Typed beats Any-error by only
  about a point of mean precision at matched recall (0.177 vs 0.167), and the single
  Any-error model reaches ~95% of typed's performance at a quarter of the mining cost.
  On recall they trade places: Typed reaches more errors at loose floors, Any-error
  slightly more at strict ones.
- **They find largely different rules (~7% overlap), so combining beats *both*
  parents at every floor.** Recall of all 2023 errors (large ensembles):

  | precision floor | typed | any-error | combined |
  |---|---|---|---|
  | 0.20 | 68.8% | 66.9% | 72.9% |
  | 0.30 | 29.7% | 29.1% | 36.0% |
  | 0.35 | 16.5% | 17.9% | 21.5% |

  (dollar recall at the 0.20 floor: 71.2% / 69.7% / 75.3%.)

The honest caveat: that floor-level gain is almost mechanical (adding rules can only
grow the union), and at *matched recall* the combined set runs about half a point
*below* typed-only, inside the noise. So combining clearly wins for a state working
at a fixed floor (the usual workflow) and roughly ties for one targeting a precision
level. Both the ordering and the magnitudes reproduced on an independent test year
(train 2022+2023, test 2024).

*Full table, both ensemble sizes, and the replication: [detailed record](modeling_findings_detailed.md), §3.*

## 4. Engine tuning: what matters and what doesn't

> **Takeaway: about our pipeline.** Most tuning knobs barely matter. The few that do:
> give the random forest a little signal to split on (mtry = 2, not 1), use slow,
> low-sample boosting, and grow trees to depth 4-5. More trees don't buy precision at
> a fixed filter setting; they buy a bigger *menu* of rules, which pays off only when
> you also filter more strictly (section 5).

A 19-setting grid, one knob changed at a time, scored on held-out precision at
matched recall:

- **Give the forest a little signal.** mtry = 2 beat mtry = 1 (0.223 vs 0.214); the
  "maximum randomness for diversity" intuition was wrong here.
- **Slow learning, low row-sampling.** A slow learning rate helped (eta 0.02 gave
  0.217 precision vs 0.212 at eta 0.1), and a low row-sample beat a high one
  (subsample 0.20 gave 0.218 vs 0.208 at 0.75); within 0.15-0.30 the choice does not
  matter, so production uses 0.20. (Honest footnote: the "low beats high" edge did
  *not* replicate on a later test year, see section 13, so treat 0.20 as "as good as
  anything," not proven best.)
- **Deeper trees help the forest, not the booster.** Depth 5 beat depth 3 clearly for
  ranger (0.203 to 0.213) but only marginally for xgboost (0.210 to 0.211); production
  uses depth 4.
- **More trees/rounds is a plateau, not a peak,** at a fixed filter. What extra trees
  buy is a bigger menu of rules clearing any floor, worth having only because we
  then filter more strictly (section 5). Note that "more rules" and "better rules"
  often disagree: some settings yield more rules but a worse precision-recall
  frontier.

*Full grid and figures: [detailed record](modeling_findings_detailed.md), §4.*

## 5. "Mine big, filter stringently": the flexible LCB

> **Takeaway: about our pipeline.** Mining a big pool of rules and then filtering it
> hard lands on the same accuracy as mining a small pool and filtering it gently. The
> big pool's advantage isn't better numbers; it's a longer list of usable rules, so
> states have substitutes when they veto one on expert judgment. The strict filter
> (the 99% bound) is what keeps the big pool from drowning in lucky rules.

Mining more rules extends how many errors you can *reach*, but it also dilutes
precision at matched recall, because more lucky rules clear any given floor. Sweeping
the confidence level shows that dilution is mostly correctable:

- On a big (1000-round) pool, tightening the bound from 80% to 99% recovers precision
  cleanly and monotonically. On a small (100-round) pool the bound barely matters,
  the tell-tale sign that the big pool's problem was selection multiplicity, not bad
  rules.
- Concretely, a big pool at the 99% bound and a small pool at the 90% bound land on
  the *same* operating point (about 55% recall at 17% precision), but the big pool has
  ~2,000 rules behind it versus ~800. Same performance, longer usable list.
- A residual ~1/3 of the dilution is just marginal-rule quality; no bound fixes it.

Production settings: xgboost 1000 rounds (eta 0.02, subsample 0.20) + ranger 1000
trees (mtry 2), 99% bound. That keeps about 11,000 rules, with better median holdout
quality per frame than the small-ensemble setup.

*Full sweep and figure: [detailed record](modeling_findings_detailed.md), §5.*

## 6. Frame-relative vs deployed (any-error) performance

> **Takeaway: about the data.** A rule you mined to catch one kind of error routinely
> flags cases that have some *other* error too, and in real review that still counts
> as a hit. So a rule's real-world precision runs about 2x its narrow, single-type
> precision. Always quote states the any-error number; the narrow one understates
> what they would actually see.

Concretely: on the earned frame at the 0.20 floor, frame-relative precision is 0.080
but any-error precision is 0.178. The rule was flagging plenty of real errors, just
not the *type* it was mined for. The ratio runs about 2-2.7x across frames. Every
output carries both numbers; the any-error one is what a reviewer actually
experiences.

*Detail: [detailed record](modeling_findings_detailed.md), §6.*

## 7. other_error: the largest, previously unmodeled category

> **Takeaway: about the data.** The biggest single category of SNAP errors is the
> "other" bucket (deductions, shelter, household composition), larger than any of the
> classic income-error types, and nobody had tried to model it. It turns out to have
> plenty of learnable structure. One caveat from the program side: many states treat
> these as small-dollar, low-priority errors, so "we can find them" is a completeness
> win, not a headline.

In 2023, `other_error` (deductions, shelter, household composition) accounted for
2,007 of 4,460 errors (more than any single income-error type), and no prior version
had mined it. It produced our single largest block of kept rules (1,700, median
holdout precision 0.25), so whatever heterogeneity it has, it is learnable. (Program
caveat, restated: many states treat these as small-dollar and low-priority, so this
is completeness, not a headline.)

*Detail: [detailed record](modeling_findings_detailed.md), §7.*

## 8. ESAP / elderly-disabled: feature suffices, and why

> **Takeaway: about the data.** Elderly and disabled households (ESAP = the Elderly
> Simplified Application Project population) are about half the caseload but are *not*
> more error-prone. What differs is the *mix* of their errors, mostly the
> easy-to-detect types, which is why our models catch far more of their errors (~19%
> vs ~8% at a 5% review budget). The hard, still-open problem is working households
> with volatile earned income. Practical upshot: this group did not need its own
> model; letting the ensemble see it as a feature was enough.

The numbers behind the takeaway:

- Elderly/disabled households are 50.7% of the caseload, 47.1% of error cases, and
  39.2% of error dollars, right about their share, not more.
- Their error *mix* differs: 63% `other_error` + 20% unearned (both easy to detect)
  versus 44% earned-income errors (the hardest to detect) for everyone else.
- The models split the caseload on their own: the unearned frame's flags are 91.8%
  elderly households, and every one of its indicator-using rules requires
  elderly/disabled; the earned frame is the mirror image.
- Result: at a 5% review budget the union catches 19.0% of elderly-household errors
  vs 8.2% for others (dollar recall 21.1% vs 9.0%), at higher precision inside the
  flags (0.32 vs 0.25); the edge holds at a 10% budget (27.9% vs 19.2%).

So there was nothing to gain from a separate elderly model. The real frontier is
non-elderly working households with volatile earned income, still an open problem.

*Detail: [detailed record](modeling_findings_detailed.md), §8.*

## 9. States: a clean two-regime deployment rule

> **Takeaway: about our pipeline (with one portable caution).** When a state has
> enough of its own data (roughly 30+ rules clearing the bar), tuning rules on that
> state's data can pay off well; when it doesn't, tuning collapses to noise and you
> should just deploy the national rules unchanged. The portable part is the caution:
> at small sample sizes, tuning is pure winner's curse, so we enforce a hard support
> floor (n >= 30). *(Partly superseded: the current deployment recipe is the blended
> list in section 16.)*

This was the first serious look at per-state deployment; the current recipe is the
blend in section 16, but the lesson here still holds. We grid-searched rule
thresholds on seven states (train on the state's 2022+2024, test on 2023), and the
pattern was sharp, depending on how much of its own data a state had:

- **Where a state had enough qualifying rules, local tuning paid off.** ("Qualifying"
  means a rule flags enough of the state's own training cases to be trusted: our
  support floor of n >= 30, at reasonable precision.) Connecticut had 35 such rules,
  and its tuned list caught 43% of its errors (49% of its error dollars) at 0.21
  review precision, versus 24% of errors at 0.23 precision from the untouched
  national list. So tuning nearly doubled the reach at essentially the same precision;
  it bought recall, it did not raise precision.
- **Where it did not, tuning collapsed.** Louisiana's tuned rules went 0-for-6 on the
  test year; Washington's fell to 5% precision, while the untouched national list held
  at 36% precision (9% recall).

That collapse is the winner's curse at state scale: with few cases, the "best" local
thresholds are mostly luck. The guard is a hard support floor: require at least 30
flagged training cases before trusting a rule, which turns a collapse into a gentle
~1/3 deflation. That floor is now built into the delivery builder.

We also tested several ways to *adapt* the national pool to a single state
(re-filtering, re-tuning, mining the state's own data, borrowing similar states'
rules). None beat simply deploying the national ranking for the median state on a
true future-year test (section 14); adaptation helped only where the national list
already underperformed.

*The seven-state table, the three tuning criteria, neighbor-transfer, and
single-state mining detail: [detailed record](modeling_findings_detailed.md), §9.*

## 10. Data visibility: how much of a state's error population the public data can even show

> **Takeaway: about the data (and it matters a lot).** The public QC file does not
> show a state its whole error population: it excludes ineligible cases entirely, so
> a state sees only part of its own errors, from 43% (New Jersey) to 91% (Georgia). Any
> rule mined on public data is therefore blind to a large slice of reality. States
> below roughly 60% visibility should treat public/national rules as a *supplement*
> and run the pipeline on their own internal data.

The headline gap comes with two pipeline fixes we made along the way (both now
permanent):

- An earlier frame had silently dropped multi-element error cases, about 31% of all
  errors. Fixed; the frame now saves straight from the build script so it cannot
  drift again.
- Optional deduction fields left blank by some states were being dropped as missing;
  they are now zero-filled, which recovered ~16% of Washington's caseload.

With those fixed, we measured how much of each state's true error population the
public file can even show, defined as frame errors divided by all over-threshold
errors plus ineligible exclusions, over FY22-24. Nationally it is 71%; the
best-covered states reach about 87-91% (Georgia highest, at 91%). The floor is set by *ineligible cases*, which the
public file omits entirely and which are 100%-of-benefit errors: New Jersey sees just
43% of its errors, Tennessee 51%, several states ~53%. Below ~60%, public rules are
only a supplement, and the state should mine its own internal data, which contains
those ineligible determinations.

*Per-state visibility table and rebuild effects: [detailed record](modeling_findings_detailed.md), §10.*

## 11. Household-size stratification: split, but split coarsely

> **Takeaway: about our pipeline.** Splitting the caseload by household size
> (1 / 2-3 / 4+) and mining each group separately reliably helps, or at worst never
> hurts, so it is our default. Splitting *finer* (a 5-way split) does not help and
> costs more compute; past a point, smaller groups just starve each rule of the cases
> it needs to clear the bar.

Two studies, same conclusion. Under the older pipeline, splitting into 1 / 2-3 / 4+
lifted mean precision from 0.101 (no split) to 0.148, and this coarse split also beat
finer 5-way splits (0.127); finer groups thin the training data faster than they add
homogeneity.

Under the current pipeline (where household size is also a feature the ensemble can
use) the precision gap largely closes (pooled 0.226 vs split 0.222), but the split
still wins on *reach* (54.8% vs 48.4% dollar recall at the 0.20 floor) and gives each
rule the within-group support to clear the strict bound. A year-swap re-test made the
case cleaner still: on 2024 the split won precision outright (0.302 vs 0.262). Across
both years the split never loses, so it stays the default. The one claim that did
*not* replicate: the 5-way split is not actively worse, just costlier for no gain.

*Both studies and the year-swap: [detailed record](modeling_findings_detailed.md), §11.*

## 12. Cross-state transfer vs honest national baselines

> **Takeaway: about our pipeline (now superseded).** In a same-year test, pools of a
> few "similar" states looked competitive with the full national pool at moderate
> review budgets. That advantage did *not* survive a proper future-year test (section
> 14), so it is recorded here for the trail but is no longer the guidance. The durable
> point: judge deployment at realistic review budgets (5-10% of caseload), not at
> abstract filter floors.

For 12 states we mined rules on donor pools that never saw the target state, then
scored them under review budgets. The comparison was between a full 48-state national
pool and small pools of 5 "similar" states (chosen four different ways). At a 5%
budget the big national pool won on median precision (0.309 vs 0.273 for the best
similar-state pool), because it had more data. At a 10% budget the small
similar-state pools looked competitive or better in about half the states (best pool
0.278 vs the national pool's 0.245).

That 10%-budget edge is exactly what did not survive a proper future-year test
(section 14), so it is not guidance. But the *method* it introduced is durable:
evaluate deployment at realistic review budgets (5-10% of caseload), not at abstract
precision floors. Several states that looked like total failures at fixed floors were
fine under a budget; the failure was a floor artifact.

*Full similarity definitions, the 12-state tables, and the honest-baseline
discussion: [detailed record](modeling_findings_detailed.md), §12.*

## 13. Pre-registered year-swap replication of the model-selection studies

> **Takeaway: about our pipeline (and our honesty check on it).** Every modeling
> choice above had been judged on one test year (2023). We wrote our predictions down
> in advance and re-ran the four big ones on a fresh year (2024): three held up, and
> one ("low subsampling helps") did not and was retired. The value here is the
> discipline: pre-committing to predictions is what lets you tell a real effect from
> a lucky one.

We had judged every modeling choice on one held-out year (2023), which sits *between*
our two training years, so the whole selection procedure could have been tuned to
one lucky, interpolated year. To check, we wrote predictions down in advance and
re-ran the four big decisions on 2024, a year that had never touched any design
choice:

- **Engine pairing (xgboost + ranger):** replicated. The pair still led recall at
  the floor, though the margin thinned from the 2023 number (2.1pp vs the 3pp we
  predicted), itself a clean measure of how much the original result was flattered.
- **Low subsampling helps:** *failed*. On 2024 subsample made no difference across
  0.15-0.80. Retired; we keep 0.20 because nothing beats it, not because low wins.
- **Stringent filtering raises precision:** replicated. Precision rose monotonically
  as the confidence level rose.
- **Big ensembles widen the menu, not the frontier:** replicated, with ~7x more rules
  at a nearly identical frontier.

Three of four held, and the procedure produced one honest retraction. That is the
point: pre-committing to predictions is what separates a real effect from a lucky
one. No production settings changed.

*The pre-registration and full numbers: [detailed record](modeling_findings_detailed.md), §13.*

## 14. Time-shifted deployment benchmark: own-state vs transfer vs national on 2024

> **Takeaway: about our pipeline (with one portable caution).** Tested the way a
> state would actually face it (rules built on past years, scored on a future year),
> the plain national rule list is the best default: the highest precision at both
> budgets among the lists a state can actually deploy, and never a disaster. Mining on
> a state's *own* data has the biggest upside but is high-variance and can fail below
> the random-review base rate (Washington did). Portable caution: don't assume a
> state's own rules beat the national ones; make the state confirm it on their own
> held-out year. ("NB transfer" = a pool of a few statistically similar states, picked
> by a naive-Bayes similarity measure.)

We re-ran deployment the way a state actually faces it: rules built only on
2022+2023, scored only on each state's 2024 cases, filling a review budget in rank
order. Four options, 12 states (median delivered precision):

| budget | own state | similar-state transfer | national |
|---|---|---|---|
| 5% of caseload | 0.253 | 0.256 | 0.300 |
| 10% of caseload | 0.240 | 0.245 | 0.273 |

(the "national" column is the list a state deploying "national rules" actually
receives. A held-out check, the same national pool with the target state's own past
cases removed, lands essentially identically, at 0.296 at 5% and 0.276 at 10%, so
including a state's own rows in the national pool neither flatters nor hurts it.)

- **National is the best default:** the highest precision among deployable lists at
  both budgets, and never a disaster. Every state clears its base rate (1.5-3.4x lift
  over random review).
- **Own-state mining is boom-or-bust:** the biggest wins anywhere (Connecticut 0.416,
  Virginia 0.371 at 10%) but also total failures. Washington's own rules landed
  *below* its 8.5% base rate, i.e. worse than reviewing at random. You cannot assume a
  state's own rules beat the national ones; make the state prove it on a held-out
  year.
- **Similar-state transfer is insurance, not a first choice:** it holds up where
  own-state mining fails (Washington 0.247, Louisiana 0.207 at 10%) but rarely wins
  outright.
- Section 12's 10%-budget transfer advantage flipped here, hence its retirement.

*Per-state detail and charts: [detailed record](modeling_findings_detailed.md), §14.*

## 15. Frozen per-state lists: the handable deliverable, priced

> **Takeaway: about our pipeline.** A state can't wait for the test year; it needs a
> fixed list in hand. Freezing the national list and sizing it against the state's own
> caseload (with a buffer so reviewers never run dry) costs almost nothing versus an
> idealized after-the-fact list: under a point of precision. Each list personalizes
> itself through the state's own case mix, and about a third of every state's list is
> unique to it.

A budget-filled benchmark chooses rules against the test year's realized caseload,
but a state needs its list *in advance*. So we froze one ranked list per state: the
national pool, sized against the state's own recent caseload to the target budget
(the "core"), then extended to 3x that depth (a "buffer") so reviewers never run dry.
The state walks the list in rank order, turning on rules until capacity fills, with no
outcomes needed, and it lands on budget however firing rates drift.

Committing in advance costs almost nothing: across 18 states, the frozen list came
within a point of precision of an idealized after-the-fact list (0.294 vs 0.301 at a
5% budget; 0.270 vs 0.275 at 10%), and every state cleared its base rate. Median list
size is 23 rules at 5%, 42 at 10%. And the lists individualize themselves: about a
third of each state's list is unique to it, and only 8 rules serve 10 or more states.

*List-construction detail and overlap analysis: [detailed record](modeling_findings_detailed.md), §15.*

## 16. Blending state and national rules on one confidence scale

> **Takeaway: about our pipeline (the current default deliverable).** Put each
> state's own mined rules and the national rules on one honest confidence scale (the
> 99% bound) and let them interleave. This "blend" is the recipe we now ship: better
> at a 5% budget, about even at 10% (0.262 vs 0.270 precision), and no case-by-case
> decision to defend. Its one blind spot (a national rule's bound says nothing about
> whether it *transfers* to a given state) is why we keep the state's own-rules list
> as a fallback where their internal validation shows the blend underperforming.

We put each state's own mined rules and the national rules on one scale, with every
rule ranked by its own 99% Wilson bound (national rules bounded on national data,
state rules on the state's own). Because both bounds mean "at least this precise, with
99% confidence," the merged ranking is coherent and it automatically discounts
small-support state rules.

Across 18 states, the blend beats a national-only list at a 5% budget (0.324 vs
0.294) and roughly ties at 10% (0.262 vs 0.270), with no per-state decision to make.
Where a state's own rules clear the shared bar, interleaving wins: Arizona deploys 20
of its own rules and reaches 0.326 (vs 0.291 for its best single-source list); DC,
Mississippi, and Missouri similarly.

The blind spot is transfer asymmetry: a national rule's bound describes its precision
in the *national* mix and says nothing about whether it carries to a given state, so
the scale over-trusts national rules exactly where the national mix fits worst. In
New Jersey, which sees only 43% of its errors in public data (section 10), the
state's own rules never clear the bar against tens of thousands of tight national
bounds, and the blend under-delivers (0.161 vs 0.230 for NJ's own list). That is why
the own-pool list stays as a fallback, chosen by the state's internal validation.

*Full numbers: [detailed record](modeling_findings_detailed.md), §16.*

## 17. Typed-frame delivery vocabulary: retired after three rescue attempts

> **Takeaway: about our pipeline.** Adding the four typed datasets to the delivery
> pool tripled the candidate rules but *lowered* delivered precision, and three
> attempts to rescue it failed. The reason is general enough to remember: when you must
> pick only 20-50 rules to fit a review budget, a bigger pool mostly adds small-sample,
> lucky-looking rules that crowd out the honest ones. The filter-floor advantage of
> pooling (section 3) is real but does not survive a tight budget.

Adding the four typed frames to the delivery pool tripled the candidate rules
(48k to 159k nationally) but *lowered* budget-filled precision on the 2024 test (0.306
vs 0.324 at a 5% budget). Three rescue attempts failed to recover it: no filter
stringency closed the gap; collapsing near-duplicate rules helped but not enough; the
shrinkage ranking of section 18 did not help either.

An autopsy of the deployed rules showed the mechanism directly: the enlarged pool's
extra small-support, high-raw-precision rules crowd out honest ones at the very top
of the list, exactly where a tight budget lives. The floor-level advantage of
pooling (section 3) is real but does not survive capacity-constrained selection. Ten
typed-frame lists were briefly shipped and then withdrawn.

*The sweeps and autopsy: [detailed record](modeling_findings_detailed.md), §17.*

## 18. Shrinkage (empirical-Bayes) ranking: refuted on two eras

> **Takeaway: about our pipeline (with a portable statistical reason).** Ranking rules
> by a smoothed "posterior mean" precision did worse than our lower-bound ranking on
> two separate eras. The reason travels: filling a small review budget is a decision
> about the *top* of the list, and posterior-mean ranking floods the top with big,
> only-slightly-above-average rules, while a lower-bound statistic penalizes exactly
> the small-sample noise that piles up there.

We tried ranking rules by a shrinkage estimate (a beta-binomial posterior mean that
smooths each rule toward its stratum's average) instead of the lower bound. It did
worse at a 5% budget on both test eras (0.259 vs 0.324 on 2024; 0.201 vs 0.219 on
2019). The reason generalizes: filling a small budget is a decision about the *top*
of the list, and a posterior mean rewards large-support, only-slightly-above-average
rules (flooding the top), while a lower bound penalizes the small-sample noise that
piles up there.

*Detail: [detailed record](modeling_findings_detailed.md), §18.*

## 19. Which rules to keep: a false-discovery-rate test plus a minimum-support floor

> **Takeaway: about our pipeline (with a portable statistical lesson).** Before we
> rank rules, we decide which ones to keep. A false-discovery-rate test
> (Benjamini-Hochberg at 10%) plus the n >= 30 support floor matched our old
> hand-tuned filter at the 5% budget on two eras, was never worse, and kept a smaller
> pool, so it is now the default. The two checks do different jobs and you need both:
> the test limits how many kept rules are flukes (whose true precision is no better
> than the base error rate), and the floor keeps out rules whose precision is measured
> from too few cases to trust. Drop the floor and those small-sample rules get
> deployed and underperform on the test year.

On the raw, unfiltered rule vocabularies (~145k candidates per era), the
false-discovery-rate test (Benjamini-Hochberg against the stratum base rate, at 10%)
plus the n >= 30 support floor matched our old hand-tuned filter exactly at the 5%
budget on both test eras, using a pool about 40% smaller. That is why it replaces the
hand-tuned filter as the default.

What the two checks catch is different, and the difference is the whole point. The
false-discovery-rate test controls how many of the rules we keep are flukes, meaning
rules whose true precision is no better than the base error rate. The support floor is
a separate guard on estimation quality: a rule can be genuinely above the base rate
yet have a precision *estimate* too noisy to rank on when it comes from only a handful
of cases. When we dropped the floor and kept only the test, those small-sample rules
were kept and then deployed, and their training precision did not hold up a year
later: median delivered precision at the 5% budget fell from 0.335 to 0.284. So this
is not a "the test let in more flukes" failure, because the test was still running; it
is a reliability failure, the same winner's curse as sections 13, 17, and 22.

The test also sizes itself to the data: it kept 54k of 145k candidate rules
nationally but only a few hundred for a small state, with no hand-set cutoff.

*Detail: [detailed record](modeling_findings_detailed.md), §19.*

## 20. Ordering stringency: z = 2.326 vindicated across eras; the 2024 bump did not replicate

> **Takeaway: about our pipeline.** A 2024 sweep hinted that filtering even more
> strictly than our 99% bound would help; pre-registered on a separate era, that hint
> did not replicate, so we kept z = 2.326. A useful reminder that a single year can
> whisper a false signal; the second era is what settled it.

A 2024 sweep hinted that filtering more strictly than our 99% bound (z = 2.326) would
help: z = 2.576 read 0.335 vs 0.324 at a 5% budget. We pre-registered the question
on the separate 2017-19 era, and the direction did not replicate (there, 2.576 read
*below* 2.326). No fixed stringency beat 2.326 at both budgets on the second era, and
a caseload-scaled formula only matched it. So 2.326 stays; the 2024 "under-stringent"
signal was era noise.

*Detail: [detailed record](modeling_findings_detailed.md), §20.*

## 21. Dollar-yield ranking: direction consistent, magnitude era-unstable; not adopted

> **Takeaway: about the data (but not adopted).** A rule's average error *dollars per
> flagged case* carries over from one year to the next more reliably than its precision
> does; error size seems anchored to observable case traits. Ranking by dollars beat
> ranking by precision on dollar recall in 2024, but the gain shrank on the 2017-19
> replication and missed our pre-set bar, so we did not adopt it. Recorded as a real
> but era-unstable direction worth revisiting.

Groundwork first: a rule's average error *dollars per flagged case* carries from one
year to the next more reliably than its precision does, in every support band
(Spearman rank correlations ~0.56-0.79 for dollars vs ~0.50-0.71 for precision,
across 169k rule-state pairs). Error size seems anchored to observable case traits.

So we tried ranking by dollars. It beat precision-ranking on dollar recall at a 10%
budget on 2024 (+3.5pp), but the gain shrank to +1.0pp on the 2017-19 replication and
missed our pre-set 2pp bar. A real direction with an unstable magnitude: not
adopted, but flagged for a future, more structure-anchored dollar statistic.

*Detail: [detailed record](modeling_findings_detailed.md), §21.*

## 22. The winner's curse at the top, demonstrated directly

> **Takeaway: about the data (a clean demonstration).** Using the same data to both
> choose and rank rules inflates how good the top of the list looks. We isolated this
> by ranking one half of the data on rules mined from the *other* half: the clean
> ranking beat the self-scored one by ~1.6 points of precision at a 5% budget, and the
> gap was concentrated in the very top rules, exactly where a tight review budget
> lives. It is the winner's curse of section 1, shown directly at the point of the
> list that matters most.

A clean, controlled demonstration of section 1's effect. On 2017-18 we mined one rule
vocabulary, then split the scoring: rank the rules by a bound computed on the *same*
half they were mined from, versus a bound computed on the *untouched* other half, with
everything else identical. The clean ranking beat the self-scored one by 1.6 points
of precision at a 5% budget on 2019 (0.216 vs 0.200), and the gap vanished at a 10%
budget. The curse lives in the extreme top of the list, exactly where a tight review
budget operates.

*Detail: [detailed record](modeling_findings_detailed.md), §22.*

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

*Detail and artifacts: [detailed record](modeling_findings_detailed.md), §23.*

## 24. Munging row exclusions: tested by relaxing them, and kept

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

The munging script drops rows in four places, and the question was whether any of
that costs us usable data. We re-ran it with the exclusions relaxed, into a
separate file, leaving the production frame untouched.

Relaxing them took the frame from 237,391 rows and 24,334 errors to 305,954 rows
and 42,102 errors. That difference decomposes exactly: 49,468 rows and 4,986 errors
from adding FY2020, and 19,095 rows and 12,782 errors from switching off one
filter, the one that keeps only rows where the file's two statements of the benefit
error agree within $5.

Those 19,095 rows look like a rich seam and are not one. In FY2022-24 they are
7,913 rows of which 66.6% are labelled errors, against 11.2% in the rest of the
frame. But they are by definition the rows where the two statements disagree, and
the pipeline's error test reads one of those two statements, so the 66.6% is a
restatement of the disagreement. On 59% of them the file's reported error amount is
zero while the benefit figures differ by a median of $93. Independently, the pre-QC
restoration fails on them: the benefit recomputed from the restored fields misses
its target by a median $51 against $0 on the rows that pass the filter, with 29.3%
landing within $5 against 95.5%. An untrustworthy label and unreconstructed
features.

FY2020 and FY2021 are excluded by decision rather than by this measurement: the
data is poor and misleading and state practices were qualitatively different. That
also settles the fourth exclusion, which turns out to drop 9,456 rows that are all
FY2021, 100% of that year and 0% of every other, because FY2021 carries the
pandemic 15% allotment increase the lookup table does not.

The by-product is worth more than the verdict. On the six years we use, the filter
is additive-only: the rows it keeps reproduce the production frame's rows and
errors exactly, year by year, 0 mismatches. So we know exactly what it removes and
that it changes nothing else. For Washington, the frame the state workbook reads is
unaffected: 0 error-flag disagreements on the 2,356 shared rows and feature
disagreements on at most 3 of them.

No pipeline change came out of this. Each exclusion is a validity guard rather than
conservatism.

*Full tables, the FY2021 diagnosis, caveats and artifacts:
[detailed record](modeling_findings_detailed.md), §24.*

## 25. Admission stringency: tightening the false-discovery rate from 10% to 5% changes nothing

> **Takeaway: about our pipeline.** We hold rules to a false-discovery-rate test before
> they can enter a list. Making that test twice as strict (10% to 5%, with the n >= 30
> support floor left in place) changed nothing: 17 of 18 states delivered a
> bit-identical list at the 5% review budget and 16 of 18 at the 10% budget, and the
> paired median difference in precision was 0.000 at both. The reason is positional.
> Tightening the rate removes rules from the middle and bottom of the ranking, and the
> highest-ranked rule it removed sat at position 14,449 of 50,697, while a review
> budget deploys the top 16 to 27 rules. The support floor is the guard that reaches
> the top of the list; the rate is not.

Two admission arms were compared on the raw 2022-2023 vocabularies and scored on each
state's FY2024 cases, a true future year. Both keep the n >= 30 support floor and
differ only in the false-discovery rate. The shipped setting, 10%, admitted 50,697 of
144,533 national candidates and delivered median holdout precision 0.3345 at the 5%
budget and 0.2753 at 10%, against a median base error rate of 0.1253, so lift of 2.48x
and 2.15x. The 5% setting admitted 46,963, which is 92.6% as many, and delivered
0.3471 and 0.2770. Those medians land on different states; the paired per-state
difference has a median of exactly 0.000 at both budgets, on precision and on dollar
recall alike, and the 5% rate was better in 1 state of 18 and worse in 1.

The explanation is that the rate and the floor act on opposite ends of the ranking.
The rules a stricter rate removes are not less precise, they are less well evidenced:
median raw training precision 0.202 whether kept or removed, but median 1,163 cases
flagged among those kept against 360 among those removed. None of them are anywhere
near the top of the list. Rules flagging 30 to 50 cases carry the highest lower bounds
(median 0.198, against 0.157 for rules flagging 500 or more) and therefore sort to the
top, where a review budget actually fills: among the top 25 rules by lower bound, 96%
flag fewer than 100 cases and median raw precision is 0.608. That is where a support
floor acts, and it is why section 19 found the floor worth 0.335 against 0.284 while
this section finds the rate worth nothing.

Caveats: one era only (FY2022-2023 to FY2024), 18 states, exploratory rather than
pre-registered, and only the 0.05 to 0.10 band was tested by the scored arms. The
multiplicity correction should arguably count the searched space rather than the
reported one, and an addendum to the detailed record substitutes denominators up to
100 million: the pool falls by 41% but 973 of the top 1,000 rules survive and all of
the top 100 do, so no defensible denominator changes a delivered list either. Where
the search size could still matter is the ordering, which has not been tested.

*Detail and artifacts: [detailed record](modeling_findings_detailed.md), §25.*

## 26. The support floor: raising it costs precision, and n >= 30 is near optimal from both directions

> **Takeaway: about our pipeline (a refuted prediction).** The n >= 30 support floor
> admits rules whose apparent precision noise alone could reach, so we expected raising
> it to help. It does not. Raising the national floor to 66, 195 or 778 cases lowered
> median holdout precision at the 5% review budget from 0.3345 to 0.3000, 0.2950 and
> 0.2826, monotonically, and lowering the state floor to about 15 (a flat 1% of a state
> caseload) lowered it further to 0.2558. Section 19 already showed that removing the
> floor entirely costs precision (0.2840 against 0.3345), so 30 is close to best from
> both directions. The floor is an estimation-quality guard, not a precision dial, and
> the prediction that a bigger search needs a bigger floor was wrong on this era.

Seven floors were compared on the same raw 2022-2023 vocabularies, with the
false-discovery rate held at 10% so the floor is the only thing that moves, and each
list scored on FY2024, a true future year. Because a state's public caseload is only
about 1,500 cases, a percentage floor written as max(30, q x caseload) leaves every
state at 30 and changes the national pool alone, which makes the comparison clean.
At the 5% review budget, against a median base error rate of 0.1253: floor 30 gave
0.3345 precision (2.48x lift), 66 gave 0.3000, 195 gave 0.2950, 778 gave 0.2826, and
dropping states to a flat 1% of their caseload (about 15 cases) gave 0.2558 (1.93x).
At the 10% budget floors up to about 200 are a wash, with paired median differences of
exactly 0.000; only the 778-case floors lose.

The mechanism is that a higher floor admits only broader rules, and broader rules are
less precise: median raw training precision runs 0.344 for rules flagging 30 to 50
cases against 0.174 for rules flagging 500 or more. Filling a fixed budget out of
broader rules lands on lower-precision cases and takes fewer rules to do it, with
median rules deployed falling from 16.5 to 6.0.

This also answers a specific proposal in the direction of "no": that a pool mined from
a much larger caseload should carry a much larger floor. The national pool holds 77,806
training cases, roughly the scale of a state's internal QA data, and it is exactly the
pool where raising the floor hurt most.

Caveats: one era (FY2022-2023 to FY2024), 18 states, exploratory rather than
pre-registered, coarse floor values, and the harness re-fills against the test year, so
these numbers compare to each other and to section 25 but not to the frozen-list
scorecard. A state running this on its own 40k to 100k internal cases would also have a
much larger own pool, which no arm here simulates.

*Detail and artifacts: [detailed record](modeling_findings_detailed.md), §26.*
