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
[detailed record](modeling_findings_detailed.md#0-how-the-work-unfolded-chronology--breadcrumbs) (§0). In brief: we started by
diagnosing the winner's curse and adopting the confidence-bound filter (sections 1
and 5), built and tuned the v2 pipeline and its engines and strata (2-4, 11), then
spent the deployment phase (9, 12, 14-22) working out what to actually hand a state,
pre-registering replications along the way.

## 1. The winner's curse, diagnosed and addressed

> **Takeaway: about the data (a statistical fact you'll hit too).** If you
> shortlist rules by their raw accuracy on the same data you measured them on, you
> reward luck: rules that looked ~20% accurate came in around ~10% on fresh data.
> The cure is to rank and filter on a cautious *lower* bound of each rule's precision
> (the Wilson bound) instead of the raw number. That one change made our training
> estimates that track held-out performance, and it will do the same for
> anyone mining rules this way.

Here is the evidence. Take the rules whose training precision was at least 0.20 and
look at how they did on a held-out year: the median came in around 0.10, half of
what the training number promised. We then checked *why*, and it is almost entirely
selection luck, not the model overfitting or the world changing:

- Rules examined *without* any precision filter show almost no train-to-holdout gap
  (median gap -0.003, correlation 0.83). The training estimate tracks held-out
performance until you
  start selecting on it.
- Select on the *holdout* instead, and the bias flips: those rules have median
  *training* precision 0.116. That symmetry is textbook regression to the mean.
- The same rules give similar lift across very different years (~3.9x on 2018-19,
  ~3.5x on 2023), so year-to-year drift is only a minor part of the gap.

The fix is to rank and filter on the one-sided Wilson lower confidence bound of a
rule's training precision (a cautious "at least this good" figure) rather than the
raw estimate. At matched deployed precision (~0.20) that catches 12.8% of all errors
versus 8.2% for a raw threshold, and it brings the training number close to what
held-out data shows
about what the holdout will show.

One practical corollary for setting a cutoff: even after the bound weeds out the
junk, a floor written on *raw* precision still overpromises (a raw 0.40 floor
delivered 0.33), while a floor written on the *bound itself* delivers what it says (a 0.30
bound-floor delivered 0.38). Set your floor on the bound.

*Full numbers and the calibration figure: [detailed record](modeling_findings_detailed.md#1-the-winners-curse-diagnosed-and-addressed), §1.*

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

What the switch did *not* buy is more signal: the best out-of-sample per-rule holdout
precision by frame still tops out around 0.31-0.48. And when we later raced the tree
engines directly (xgboost+ranger vs bagged trees + ranger vs single engines), the
best pair only edged the alternatives by about a point of precision. The lesson: the
engine is not where the leverage is; strict filtering and any-error scoring are.

*Full engine sweep and numbers: [detailed record](modeling_findings_detailed.md#2-results-of-dropping-pre-r-package-in-favor-of-rolling-our-own), §2.*

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

One caveat: that floor-level gain is almost mechanical (adding rules can only
grow the union), and at *matched recall* the combined set runs about half a point
*below* typed-only, inside the noise. So combining clearly wins for a state working
at a fixed floor (the usual workflow) and roughly ties for one targeting a precision
level. Both the ordering and the magnitudes reproduced on an independent test year
(train 2022+2023, test 2024).

*Full table, both ensemble sizes, and the replication: [detailed record](modeling_findings_detailed.md#3-typed-frames-vs-one-any-error-model-head-to-head), §3.*

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
  matter, so production uses 0.20. (Footnote: the "low beats high" edge did
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

*Full grid and figures: [detailed record](modeling_findings_detailed.md#4-engine-tuning-what-matters-and-what-doesnt), §4.*

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

*Full sweep and figure: [detailed record](modeling_findings_detailed.md#5-mine-big-filter-stringently-the-flexible-lcb), §5.*

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

**One limit on how far this can be used.** A rule's profile is only as distinctive as
the rule is narrow. Measured as the distance between a rule's element mix and the
national mix, rules under 50 error cases sit a median 0.264 away, rules over 1,000 only
0.057, and support correlates -0.446 with distance. A rule covering a large slice of the
caseload must look like the caseload. So the high-volume core of a state's list will not
differentiate on these fields, and the rules that do differentiate are the ones with the
widest intervals. Read the columns next to the rule's error-case count.

*Detail: [detailed record](modeling_findings_detailed.md#6-frame-relative-vs-deployed-any-error-performance), §6.*

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

*Detail: [detailed record](modeling_findings_detailed.md#7-other_error-the-largest-previously-unmodeled-category), §7.*

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

*Detail: [detailed record](modeling_findings_detailed.md#8-esap--elderly-disabled-feature-suffices-and-why), §8.*

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
single-state mining detail: [detailed record](modeling_findings_detailed.md#9-states-a-clean-two-regime-deployment-rule), §9.*

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

*Per-state visibility table and rebuild effects: [detailed record](modeling_findings_detailed.md#10-data-visibility-how-much-of-a-states-error-population-the-public-data-can-even-show-2026-07-07), §10.*

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

*Both studies and the year-swap: [detailed record](modeling_findings_detailed.md#11-household-size-stratification-split-but-split-coarsely), §11.*

## 12. Cross-state transfer vs like-for-like national baselines

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

*Full similarity definitions, the 12-state tables, and the baseline
discussion: [detailed record](modeling_findings_detailed.md#12-cross-state-transfer-vs-like-for-like-national-baselines-2026-07-09), §12.*

## 13. Pre-registered year-swap replication of the model-selection studies

> **Takeaway: about our pipeline (and the check we ran on it).** Every modeling
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

Three of four held, and the procedure produced one retraction. That is the
point: pre-committing to predictions is what separates a real effect from a lucky
one. No production settings changed.

*The pre-registration and full numbers: [detailed record](modeling_findings_detailed.md#13-pre-registered-year-swap-replication-of-the-model-selection-studies-2026-07-09), §13.*

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

*Per-state detail and charts: [detailed record](modeling_findings_detailed.md#14-time-shifted-deployment-benchmark-own-state-vs-nb-transfer-vs-national-on-2024-2026-07-10), §14.*

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

*List-construction detail and overlap analysis: [detailed record](modeling_findings_detailed.md#15-frozen-per-state-lists-the-handable-deliverable-priced-2026-07-10), §15.*

## 16. Blending state and national rules on one confidence scale

> **Takeaway: about our pipeline (the current default deliverable).** Put each
> state's own mined rules and the national rules on one comparable confidence scale (the
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

*Full numbers: [detailed record](modeling_findings_detailed.md#16-blending-state-and-national-rules-on-one-confidence-scale-2026-07-10), §16.*

## 17. Typed-frame delivery vocabulary: retired after three rescue attempts

> **Takeaway: about our pipeline.** Adding the four typed datasets to the delivery
> pool tripled the candidate rules but *lowered* delivered precision, and three
> attempts to rescue it failed. The reason is general enough to remember: when you must
> pick only 20-50 rules to fit a review budget, a bigger pool mostly adds small-sample,
> lucky-looking rules that crowd out the genuinely precise ones. The filter-floor advantage of
> pooling (section 3) is real but does not survive a tight budget.

Adding the four typed frames to the delivery pool tripled the candidate rules
(48k to 159k nationally) but *lowered* budget-filled precision on the 2024 test (0.306
vs 0.324 at a 5% budget). Three rescue attempts failed to recover it: no filter
stringency closed the gap; collapsing near-duplicate rules helped but not enough; the
shrinkage ranking of section 18 did not help either.

An autopsy of the deployed rules showed the mechanism directly: the enlarged pool's
extra small-support, high-raw-precision rules crowd out genuinely precise ones at the very top
of the list, exactly where a tight budget lives. The floor-level advantage of
pooling (section 3) is real but does not survive capacity-constrained selection. Ten
typed-frame lists were briefly shipped and then withdrawn.

*The sweeps and autopsy: [detailed record](modeling_findings_detailed.md#17-typed-frame-delivery-vocabulary-retired-after-three-rescue-attempts-2026-07-1516), §17.*

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

*Detail: [detailed record](modeling_findings_detailed.md#18-shrinkage-empirical-bayes-ranking-refuted-on-two-eras-2026-07-1617), §18.*

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

*Detail: [detailed record](modeling_findings_detailed.md#19-which-rules-to-keep-a-false-discovery-rate-test-plus-a-minimum-support-floor-2026-07-1617), §19.*

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

*Detail: [detailed record](modeling_findings_detailed.md#20-ordering-stringency-z--2326-vindicated-across-eras-the-2024-bump-did-not-replicate-2026-07-17), §20.*

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

*Detail: [detailed record](modeling_findings_detailed.md#21-dollar-yield-ranking-direction-consistent-magnitude-era-unstable-not-adopted-2026-07-1617), §21.*

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

*Detail: [detailed record](modeling_findings_detailed.md#22-the-winners-curse-at-the-top-demonstrated-directly-2026-07-17), §22.*

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

*Detail and artifacts: [detailed record](modeling_findings_detailed.md#23-exclusion-rules-cutting-a-review-pile-safely), §23.*

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
[detailed record](modeling_findings_detailed.md#24-munging-row-exclusions-tested-by-relaxing-them-and-kept-2026-07-29), §24.*

## 25. Admission stringency: tightening the false-discovery rate from 10% to 5% changes nothing

> **Takeaway: about our pipeline.** We hold rules to a false-discovery-rate test before
> they can enter a list. Making that test twice as strict (10% to 5%, with the n >= 30
> support floor left in place) changed nothing: 17 of 18 states delivered a
> bit-identical list at the 5% review budget and 16 of 18 at the 10% budget, and the
> median within-state difference in precision was 0.000 at both. The reason is positional.
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
0.3471 and 0.2770. Those medians land on different states; the within-state
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

*Detail and artifacts: [detailed record](modeling_findings_detailed.md#25-admission-stringency-tightening-the-false-discovery-rate-from-10-to-5-changes-nothing-2026-08-03), §25.*

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
At the 10% budget floors up to about 200 are a wash, with median within-state differences
of exactly 0.000; only the 778-case floors lose.

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

*Detail and artifacts: [detailed record](modeling_findings_detailed.md#26-the-support-floor-raising-it-costs-precision-and-n--30-is-near-optimal-from-both-directions-2026-08-03), §26.*

## 27. How deep the fill reaches, and why that makes evaluation cheap

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

A list is built by reading down the ranked pool and keeping a rule whenever it flags
cases that no better-ranked rule already flagged. Rules that add nothing new are passed
over and cost nothing to skip. That means two counts differ: what the state receives,
and how far down the ranking we had to read to assemble it.

Across the 49 shipped lists, from pools of about 39,800 rules:

| | 5% budget | 10% budget |
|---|---|---|
| rules delivered (core plus buffer) | 137 | 283 |
| of which core | 50 | 97 |
| rank reached for the core | 359 (max 1,622) | 969 (max 2,613) |
| rank reached in total | 1,544 (max 3,820) | 4,194 (max 9,072) |
| states past rank 1,000 | 35 of 49 | 46 of 49 |

What sends a state deep is the width of its rules, not any weakness in its list: depth
correlates -0.58 with how many cases a delivered rule flags, and -0.62 to -0.66 with
how many new cases a rule adds at its rank. A state whose top rules each flag twenty
cases finishes in a few hundred rules; one whose top rules flag two needs thousands.
The intuitive alternative, that deep scans signal a list struggling to fill its budget,
is not supported: that correlation is only -0.31 at the 5% budget and -0.07 at the 10%.

The practical significance is cost. Scoring rules, not mining them, is the expensive
step, because every candidate must be matched against every case: in the first
cross-fitted ranking run, each state took roughly 10 minutes to mine and 75 to
evaluate. Since the walk reads in rank order and stops once its capacity is full, the
bottom of the pool is never needed, so evaluation can be restricted to a window at the
top. That is a 2x saving on the shipped pools and roughly 10x on a large research pool,
with identical output.

Identical, not approximately identical, because there is a check. The walk can only
consume a fixed capacity, three times the review budget measured in cases. If the walk
over the window fills that capacity exactly, nothing below the window could have
entered, so the answer is the same one the full pool would have given. When it does not
fill, the window was too small and that case is redone unpruned.

What would be a mistake is capping the pool at a fixed rank as policy. At the 10%
budget the median state needs rank 969 for its core alone, so a cap at 1,000 would
truncate real delivered rules for about half the states. Depth is a property of each
state's caseload, and no single constant fits all of them.

Both halves of that were then tested rather than argued. Re-running the real fill on a
20,000-rule window for all 49 states at both budgets left zero leftover capacity in all
98 cases and rebuilt every list identically to the committed one, rule for rule. On a
research pool five times larger, where the saving actually matters, the pruned
evaluation reproduced the unpruned results on all 70 comparisons (5 states, 7 ranking
variants, 2 budgets) with the window truncating every pool, so the check was a real one
rather than a pool that happened to fit. The state that took 77 minutes to evaluate
unpruned took about 1 minute pruned.

*Detail and artifacts: [detailed record](modeling_findings_detailed.md#27-how-deep-the-fill-reaches-and-why-that-makes-evaluation-cheap-2026-08-04), §27.*

## 28. Rules that key on a benefit-reconstruction artifact near the maximum benefit

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

`rawben_rel_max` is the reconstructed benefit divided by the maximum benefit for the
household size. A household at the maximum should show 1. In our frame 37.37% of rows
are truly at the maximum (`rawben == benmax`), and 96.06% of them do show exactly 1.
The remaining 3.9% fall short, most of them into [0.987, 1). Those 1,724 rows are the
artifact. A separate clause has the same effect from the other side: 95.72% of
truly-at-max households have `unc_rawben_rel_max` above 1, so a cap like
`unc_rawben_rel_max <= 0.997` excludes at-max households instead of selecting them.

Whether a rule is exposed has to be measured, not read off its text. Of the 2,028
distinct rules in the delivered lists that exclude a ratio of exactly 1 by either
mechanism, 1,940 take just 1.35% of their flags from artifact rows; they bound the
ratio somewhere far from the band and are unaffected. The problem sits in 88 rules
that take 76.7% of their flags from artifact rows. Those 88 are above-average
performers on our frame, at precision 0.3612 against 0.2339 for the rest, which is the
uncomfortable part: they earned their rank on flags a state's own file may not have.

What that costs a delivered list:

| | 5% budget | 10% budget |
|---|---|---|
| median share of delivered cases from artifact-dependent rules | 6.3% | 4.1% |
| 90th percentile | 11.0% | 8.2% |
| most exposed state | 16.3% (Massachusetts) | 9.0% (Massachusetts) |

Read that as an upper bound on the damage rather than an estimate of it. It counts the
cases contributed by rules that mostly flag artifact rows; whether those cases vanish
in a state's own file depends on that state's reconstruction, which we cannot observe.

This is the diagnostic only. Whether an `at_max_benefit` feature repairs it is untested,
and adding a feature changes the mining vocabulary and needs a full re-mine.

*Detail and artifacts: [detailed record](modeling_findings_detailed.md#28-rules-that-key-on-a-benefit-reconstruction-artifact-near-the-maximum-benefit-2026-08-04), §28.*

## 29. Characterizing what each delivered rule finds, so a state can choose its own rules

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

*Detail: [detailed record](modeling_findings_detailed.md#29-characterizing-what-each-delivered-rule-finds-so-a-state-can-choose-its-own-rules-2026-08-04), §29. Artifacts, including the characterization sheet itself: [`methods/rule_error_profiles/`](https://github.com/giannella/snap_qc/tree/main/methods/rule_error_profiles).*

## 30. Out-of-fold ordering does not reproduce at deliverable scale, and how much the mining draw decides

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

*Detail and artifacts: [detailed record](modeling_findings_detailed.md#30-out-of-fold-ordering-does-not-reproduce-at-deliverable-scale-and-how-much-the-mining-draw-decides-2026-08-05), §30. Artifacts: [`methods/national_only_xfit/`](https://github.com/giannella/snap_qc/tree/main/methods/national_only_xfit).*

## 31. Seed stability: the deep pool covers the same errors; the top of the ranking does not

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

*Detail and artifacts: [detailed record](modeling_findings_detailed.md#31-seed-stability-the-deep-pool-covers-the-same-errors-the-top-of-the-ranking-does-not-2026-08-05), §31. Artifacts: [`methods/seed_stability_v2/`](https://github.com/giannella/snap_qc/tree/main/methods/seed_stability_v2).*

## 32. Marginal precision of delivered rules: the walk's adverse selection is real, about 3 to 4 points, and not recoverable at public scale

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

*Detail and artifacts: [detailed record](modeling_findings_detailed.md#32-marginal-precision-of-delivered-rules-the-walks-adverse-selection-is-real-about-3-to-4-points-and-not-recoverable-at-public-scale-2026-08-06), §32. Artifacts: [`methods/marginal_precision_diagnostic/`](https://github.com/giannella/snap_qc/tree/main/methods/marginal_precision_diagnostic).*
