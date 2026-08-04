# How a delivery list is built

An end-to-end read of the pipeline: what it optimises, every gate in the order it is
applied, and what we have measured about each one. Written for someone who builds models
and wants to know why this is shaped the way it is. It is the expanded companion to the
pipeline figure in the [README](README.md); that figure shows the same
flow but collapses each filter into the phrase "passed the filter".

Source: `INCL_build_blended_delivery_list_v2.R`, `rule_mining_helpers.R`. Counts are from
the any-error 2022-24 build. Numbers cite the findings section they come from; the
findings docs own every number and this document restates rather than originates them.

Last revised 4 August 2026.

---

## What the pipeline is optimising

A state agency can re-examine only a small share of its SNAP cases, call it 5% or 10% of
the caseload. The deliverable is therefore **not a score** and not a model artifact. It is
an ordered list of readable conditions, each of which a reviewer can apply, accept or
reject on its own merits, sized to the capacity the agency actually has.

That constraint drives every choice below. Gradient boosting and random forests appear
here purely as **rule generators**: every root-to-node path in every tree is harvested as
a candidate conjunction, and the fitted models are then discarded. Nothing is ever
predicted from them. What ships is the rules.

The central statistical hazard is selection. We generate on the order of 145,000 candidate
rules per pool and then choose the best few dozen, so the same data both *selects* a rule
and *measures* it. Rules whose training precision is 0.20 come in near 0.10 on a fresh year
if you rank on that raw number (findings section 1). Most of the machinery below exists to
keep that gap closed: a false-discovery test and a support floor to admit, and a **lower
confidence bound rather than a point estimate** to order.

Everything is judged by freezing a list on 2022-23 and scoring it on 2024, a year that
influenced no design decision, and always at a review budget rather than at a precision
threshold, because a threshold is not a thing an agency can staff.

| | |
|---|---|
| median holdout precision at a 5% budget, 49 states | **0.31** |
| lift over the caseload base error rate of about 12% | **2.6x** |
| rules delivered per state at that budget, core plus substitutes | **137** |

---

## Step one: two pools, one recipe

Two pools are mined separately and identically. Nothing about the recipe changes between
them, which is what later lets their rules share a single ranking scale, and is also the
source of the second open question at the bottom of this document.

| pool | what it holds | size |
|---|---|---|
| **National** | All states' public QC data, FY2022-2024. Mined once, cached, reused for every state. | 118,263 cases |
| **The state's own** | One state's rows from the same frame, or its internal case files, which include the ineligible determinations the public files omit. | about 1,000 to 4,000 cases |

Both pools then run stages 1 to 6 below, independently.

---

## Step two: the six stages, and what each one removes

Stages 2 and 5 apply a filter. Everything else transforms or measures.

### 1. Prepare the frame and split it into strata

Features must be numeric, logical or two-level; multi-level factors are rejected outright
rather than silently encoded. Constant and all-NA features are dropped. Cases are split
into three household-size strata, and every later stage runs inside a stratum.

Stratifying by household size, rather than adding it as a feature, is an empirical result
and not an assumption: the coarse three-way split never lost in testing, while a five-way
split bought nothing for about 1.6x the compute (findings 11). Elderly and disabled status
went the other way and stayed a feature.

`strata: 1 / 2-3 / 4+` &middot; `cert_HH_size_FS_n` &middot; `prep_features()`

### 2. Generate candidate rules from two tree ensembles

Every root-to-node path in every tree becomes one candidate rule. Two engines run per
mining frame and stratum, because they find complementary rules and the pair beats either
alone (findings 2). Depth 4 caps a rule at four conditions, which is about as much as a
reviewer will act on.

The two minimums are **cell** gates, not rule gates: a frame-by-stratum cell with too few
rows or too few errors is skipped entirely rather than mined thinly. They never remove an
individual rule, and on the any-error vocabulary they bind only for the two smallest
states.

```
xgboost   1000 rounds, depth 4, eta .02, subsample .20
ranger    1000 trees,  depth 4, mtry 2,  min node 20
GATE      min_rows   = 100   (per cell)
GATE      min_errors =  10   (per cell)
```

| | candidates generated |
|---|---|
| national | 146,764 |
| state (Alabama) | 81,702 |

### 3. Canonicalize, so identical rules print identically

Thresholds round to three significant digits. Repeated bounds on the same variable in the
same direction collapse to whichever one binds. Self-contradictory rules are dropped,
conditions are sorted, and rules with identical text collapse to one row that records
every engine and frame that produced it.

`SIGNIF_DIGITS = 3` &middot; dedup layer 1 of 3: exact text

### 4. Score every candidate on the any-error target

Each rule is scored over the pool's *full* caseload against *any* error, never against the
frame it was mined from. A mining frame records where a rule came from; it is never the
basis on which the rule is judged. This yields three numbers per rule: cases flagged,
errors among them, and error dollars among them.

`target: over_threshold != 0` &middot; scored on the whole pool, not the frame

### 5. Admit: the two filters that decide the candidate pool

A rule is admitted only if it passes **both** tests. They do different jobs and neither
substitutes for the other: the false-discovery test controls what share of admitted rules
are flukes, and the support floor keeps badly-measured rules out of the top of the ranking.
Drop the floor and keep only the test, and delivered precision falls from 0.335 to 0.284
(findings 19).

The FDR test is a one-sided binomial against that *stratum's* base error rate, with
Benjamini-Hochberg applied within each frame's candidate set. Setting
`ADMISSION <- "legacy"` restores the pre-v2.3.0 filter instead: raw precision at or above
0.05 and above the base rate.

How much these two constants matter has since been measured directly, and the answer
surprised us. See [Settled by measurement](#settled-by-measurement) below.

```
GATE   n >= 30                        (MIN_TRAIN_FLAGGED)
GATE   Benjamini-Hochberg, FDR 10%    (FDR_ALPHA)
```

| | admitted | cut |
|---|---|---|
| national | 58,235 | 60% |
| state (Alabama) | 11,788 | 86% |

### 6. Compute the statistics, then dedup twice more

Three statistics are recorded per admitted rule. The lower confidence bound is the one that
orders the list; raw precision is reported but never ranked on, because it carries a strong
winner's curse.

Then two further dedup layers run on the merged survivor set. Rules flagging an identical
set of cases collapse to the simplest one. A rule is dropped for dominance only when a
looser rule of the same shape provably contains it and scores at least as well within a
tolerance. Overlapping rules of *different* shape are kept on purpose, so an agency can
reject any rule on expert judgment and still have substitutes that catch the same errors.
This is why no joint sparsity penalty is ever applied to the pool: a lasso would delete
exactly the substitutes the deliverable depends on.

```
precision_train
precision_train_lcb     one-sided 99% Wilson, LCB_Z = 2.326
dollars_per_flag_train
dedup layer 2: exact coverage
dedup layer 3: same-structure dominance, stat_eps = 0.01
```

| | pool after dedup |
|---|---|
| national | 36,214 |
| state (Alabama) | 6,206 |

---

## Step three: blend, fill, deliver

From here the two pools are one list. No outcome data is used at any point below.

**Blend on one confidence scale.** National and state rules are stacked and sorted by
descending 99% lower bound. Where the same rule was mined by both pools, the copy with the
higher bound is kept. Ties break deterministically on stratum then rule text, so a rerun
reproduces the list exactly.

**Fill to the review budget.** Walking in rank order, a rule is taken only if it flags
cases no earlier rule already flagged, a greedy set cover against a capacity constraint.
Rules fill the **core** until the budget is reached, 5% or 10% of the state's caseload, and
then the **buffer** out to three times that depth, as named substitutes rather than extra
capacity.

The walk reads much further than it delivers, because rules that add no new cases are
skipped: a median of 1,544 ranks to deliver 137 rules at the 5% budget, and 4,194 to
deliver 283 at the 10%, with the deepest state reaching rank 9,072. Depth is set by how
*wide* a state's top rules are (correlation -0.58 with cases flagged per rule), not by any
weakness in its list (findings 27).

That bound is also what makes evaluation cheap. Since capacity is finite and read in rank
order, scoring only the top 20,000 rules gives the same answer whenever the walk exhausts
its capacity inside that window, a certificate checked at run time rather than an
assumption. Across all 49 states at both budgets it held in 98 of 98 cases and rebuilt
every list identically, while cutting a research-scale evaluation from 77 minutes per state
to about one.

**Deliver.** Thirteen columns per rule, ending in `rank` and `role`. Each row carries its
provenance: which pool mined it, which engines found it, which frames it came from, how
many training and state cases it flags, and how many *new* cases it adds at its position in
the walk.

---

## What the pipeline refuses to do

- Never rank on raw training precision
- Never rank on hold-out performance
- Never re-prune the pool with a joint lasso
- No greedy "nets", those existed only in v1
- Never let a mining frame become a scoring basis
- Never use outcomes when filling to budget

---

## Settled by measurement

Three questions this document previously raised have now been tested on a held-out year.
All three came back negative, and two of them refuted a prediction we had argued for from
theory. They are recorded here because the null results are what justify leaving the
constants where they are.

### No effect: making the false-discovery test twice as strict changes nothing

Tightening the rate from 10% to 5%, with the support floor held, left 16 of 18 states
delivering a bit-identical list at both budgets; the paired median difference in precision
was 0.000. The reason is positional. The stricter rate removes 3,734 of 50,697 national
rules, but the highest-ranked rule it removes sits at position **14,449**, and a review
budget deploys the top 16 to 27. Those rules are not less precise, only less well
evidenced: identical median raw precision of 0.202, but 360 cases flagged against 1,163.
(findings 25)

### No effect: correcting the multiplicity denominator does not reach the delivered list

The test divides by the roughly 145,000 rules the trees reported, while the trees searched
a far larger space; enumerating every depth-4 conjunction over decile cutpoints gives
millions per stratum. The bar really is too easy. But substituting denominators up to 100
million, three orders of magnitude past what we correct against now, shrinks the pool by
41% and still keeps **all of the top 100 rules** and 973 of the top 1,000. The top rules
carry p-values small enough that no defensible denominator binds on them.

Where a search-size correction could still matter is the *ordering*, not admission, and
that remains untested. A simultaneous bound over *m* candidates would replace z = 2.326
with sqrt(2 ln *m*): 4.87 at the reported denominator, 6.07 at 100 million. Three orders of
magnitude move z by 1.2, so a sweep of z from 2.3 to 6 covers every plausible search size
without anyone having to pin the denominator down. (findings 25)

### Refuted: raising the support floor makes the list worse, not better

At n = 30, in a search this size, noise alone can reach a precision near 0.34, which is
roughly what the rules at the top of the list report. The natural inference is that those
rules are largely luck and a higher floor would help. Tested across seven floor shapes,
flat and scaled to pool size, that inference is wrong. Raising the national floor to 66,
195 and 778 cases lowered median holdout precision from **0.335 to 0.300, 0.295 and
0.283**, monotonically; letting a flat 1% rule drop small states to about 15 lowered it to
0.256.

The mechanism is a trade the arithmetic does not capture: a higher floor admits only
broader rules, and broader rules are less precise, median raw precision 0.344 for rules
flagging 30 to 50 cases against 0.174 for those flagging 500 or more. Since removing the
floor entirely also costs precision, n = 30 sits near the optimum from both directions. The
bound on what noise *could* produce is not evidence about what the deployed rules *are*.
(findings 26)

---

## Known opportunities for improvement

Everything on the admission side is now measured and inert, which leaves the ordering as
the only lever with real leverage. Both items below concern it.

### The ranking statistic is measured on the rows that chose the rule

This is the one defect we have measured a price for. A rule's bound is computed from the
same rows that selected it out of about 145,000 candidates, so the bound at the top of the
list is inflated by selection. Ranking instead on an untouched half of the data, everything
else identical, beat the self-scored ranking by 1.6 points of precision at a 5% budget
(0.216 against 0.200), with the gap concentrated in the very top rules, exactly where a
tight review budget operates (findings 22).

A penalty constant cannot fix this; it can only trade one bias for another, which is what
the floor and stringency results above demonstrate. The fix under test is cross-fitting:
split each pool at random, mine one half, and score the candidates on the other, so the
ordering carries no selection inflation to correct. Rules rediscovered across independent
splits also get a stability signal for free, counted over rule *shape*, since exact
thresholds move with the split and text-level rediscovery runs near 2%.

### One scale spans two very different searches

The blend ranks a rule mined from 118,263 cases against a rule mined from perhaps 2,000,
using a bound that adjusts for how many cases the rule flagged but not for how hard its
pool was searched to find it. State rules therefore carry more selection inflation than
national ones at the same nominal bound, and the single scale treats them as equivalent.

A per-pool penalty looks like the obvious repair and is not. Selection inflation grows with
sqrt(2 ln *m*), and *m* differs far less between the pools than case counts do: z of 4.87
nationally against 4.70 for a typical state pool, a 4% difference against a 50x difference
in rows. A floor scaled that way was tested and lost (findings 26). Cross-fitting is the
more promising route precisely because it removes the inflation from both pools directly
rather than trying to price it.

This matters unevenly, and the split is sharp. At the 5% budget, 26 of the 49 states take
no core rules at all from their own pool, while three take more than half and the District
of Columbia takes all of them. The distortion is concentrated in that second group, because
locally mined rules are the inflated ones, and those are exactly the states that a change
to the national pool cannot reach. Any global lever we pull misses the states that need it
most.

---

## Notes

Counts are from the any-error 2022-24 build; the state column uses Alabama. Constants shown
are the defaults in `INCL_build_blended_delivery_list_v2.R`; each is overridable by
pre-setting it before `source()`.

A variant vocabulary that mines four typed error frames alongside the pooled one produces
roughly three times the candidates at every stage. It was built for all 49 states and
measured against this one: precision was a wash and dollar recall slightly worse, so it is
kept as a labelled option rather than the default (findings 17).

Two performance figures appear in this project and are not interchangeable. A list frozen
on 2022-23 and scored on 2024 without refilling carries only about 86% of its budgeted
workload, because a frozen list does not flag exactly 5% of a different year's caseload.
Re-walking core and buffer against the newer caseload fills the budget by construction.
Quote precision with the basis attached; the scorecard in
`methods/anyerror_blended_holdout_2024/` reports both.

Further reading: [`methods/modeling_findings.md`](methods/modeling_findings.md) for the
plain-language findings and [`methods/modeling_findings_detailed.md`](methods/modeling_findings_detailed.md)
for the full evidence log with artifact paths.
