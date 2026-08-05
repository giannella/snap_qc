# Pre-registration: national-only cross-fit ordering, and partition stability

**Written 2026-08-05, before any result existed.** The first run had been
launched shortly beforehand and had written no output file; the second had not
started. Nothing below was informed by a result, and no result has been
inspected. Everything here is a commitment, not a description.

## Why this exists

The 2026-08-04 cross-fitted ranking study could not answer its own question. It
cross-fitted the national pool AND each state's own pool, so every arm differed
from the baseline in two ways at once, and halving state pools left 48 to 140
errors to mine on, which destroyed them. Recorded in `RESUME.md` under "Invalid
designs: the QUESTION is still open" rather than with the retired ideas, because
a study whose design cannot answer its question says nothing about the question.

The obvious repair, cross-fitting the national pool while leaving each state's
own pool on the full-data recipe, is not available. Measured on the cached mines:
at the top 1% of the ranking, the self-scored Wilson bound overstates the
out-of-fold bound by **+0.105 for national rules and +0.123 for state rules**.
Blending those on one sorted scale would systematically promote state rules,
which are the ones the Virginia work already flags as fragile at this scale. So
the state pool is dropped entirely. Section 14 already makes the plain national
list the best default a state can deploy.

## Run 1: does out-of-fold ordering beat self-scored ordering?

**The question.** Ranking the national pool by a Wilson lower bound computed on
data the rules were not mined from, versus the same bound computed on the mining
data, measured as delivered precision in a frozen list walked against an unseen
year.

**What varies:** the ranking statistic. **Held fixed:** the vocabulary (both arms
read the same mine), the walk, buffer depth at 3x, both review budgets, FY2024 as
the test year, and no state pool in either arm.

Two admission variants, because they answer slightly different questions and cost
the same:

| variant | admission | what it isolates |
|---|---|---|
| `common` | both arms drawn from one set: out-of-fold BH at FDR 10% within stratum, plus `n_hon >= 30` | the ordering effect alone |
| `own` | each arm admits on its own statistic | closer to what each would do in production, but changes two things |

**Support, computed not estimated.** National FY2022-23 is 77,806 rows carrying
8,485 errors. Each of the five cached partitions mines on 38,901 rows (~4,242
errors) and scores on the complementary 38,905. Those mines produced 137,106 to
138,403 rules each, and section 27 measured that the fill reads a median rank of
1,544 at the 5% budget, so vocabulary is not the binding constraint. The test
year is 40,457 rows carrying 4,803 errors across 49 states.

**Replication.** 5 independent partitions x 49 states = 245 within-state
comparisons per budget per variant.

**Pre-registered bars.**

- **Primary:** the median within-state difference (out-of-fold minus self-scored)
  at the **5% budget** under `common` admission. **>= +0.010** counts as
  reproducing section 22's direction at deliverable scale. Between 0 and +0.010,
  real but too small to act on. **<= 0**, does not reproduce.
- **Secondary:** the 10% budget, treated as directionally uninformative, since
  section 22 found the arms within noise there.
- **Descriptive, no bar:** the spread of delivered precision across the five
  partitions, per state. This is the "does it matter which half we drew" question
  and we have no prior expectation to commit to.

**What we already know that bears on it.** Section 22: an equal-footing cross-fit
on 2017-18 national gave +1.6pp median precision at the 5% budget on 2019 (0.216
against 0.200), within noise at 10%. Section 20: ordering changes have a poor
record across eras. Measured this morning on these same mines: the top 100 rules
by out-of-fold bound share a pairwise Jaccard of 0.032 by signature across
partitions, with zero signatures common to all five, against 72.9% commonality
across the pool as a whole; and self-scored admission passes about 11% more rules
(41,400 against 37,300).

## Run 2: do different partitions catch the same errors?

**The question.** Given that the top of the ranking is nearly partition-specific,
do the resulting lists flag the same cases through different rules, or different
cases? Raised by Eric, and it decides whether the instability matters at all.

**Design.** Out-of-fold arm only, 10 states spanning a range of caseload sizes and
base rates, both budgets. Record the actual FY2024 cases flagged and errors caught
by each of the five partitions' lists, then compare composition pairwise. The walk
fills to the same capacity every time, so the sets are near-equal in size and
Jaccard compares composition only.

**Pre-registered bar.** Median pairwise Jaccard on **errors caught**:

- **above 0.5** the deliverable is stable in what it does, and the rule churn is
  cosmetic
- **below 0.3** partition choice changes which errors a state finds, which is a
  property of the currently shipped pipeline too, since it also draws one
  arbitrary sample, and states would need to be told

## Decision rules, set now

- Ordering effect clears +0.010 **and** case overlap is high: the mechanism is
  real and the deliverable is stable. K-fold then becomes worth running, to
  recover the vocabulary a single 50/50 split gives up, followed by a shipping
  decision.
- Ordering effect at or below 0: section 22 does not generalise to the
  deliverable. Drop the line. K-fold is not warranted and the blend study stays
  shelved.
- Case overlap low, whatever the ordering result: that is a reproducibility
  finding about the pipeline we ship today, and it is worth recording on its own.

## Explicitly out of scope

The blended deliverable with state pools, K-fold cross-fitting, any claim about
the 40k-100k internal case files a state holds, and any change to
`state_delivery_lists/`. Each is gated on the results above.

## Artifacts

`methods/national_only_xfit_v2.R` (runner `runners/run_national_only_xfit.R`),
`methods/partition_case_overlap_v2.R` (runner
`runners/run_partition_case_overlap.R`),
`methods/partition_signature_overlap_v2.R`. Outputs to
`methods/national_only_xfit/`. All three read the cached mines in
`methods/state_similarity_v2/crossfit_ranking_train2223_test24/mines`; none of
them mine.
