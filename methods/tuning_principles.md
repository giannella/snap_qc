# Principles for constraining state-side tuning of a delivery list

Written 2026-07-29, in answer to this question: we want to hand states the
blended delivery list plus a script that tunes it against their own internal
data (data that never helped build the rules). What keeps that from becoming a
fishing expedition?

This is a design ruling, drawn from the evidence already in
`modeling_findings.md` (sections 9, 10, 12, 14, 15, 16, 19) and from the
constraints already implemented in `state_threshold_gridsearch_v2.R`,
`methods/deployment_state_adaptation_v2.R`, and the v1
`INCL/EXCL_optimize_*_for_a_state.R` grid searches. No new runs were made for
it. The reference implementation of the tiers and guards below is
`custom_one_off/snap_dashboard/tuning.py`.

## The principle

Every degree of freedom you hand the state is a comparison you have to pay for.
The winner's curse does not care whether the search is run by us or by them.
What controls it is the number of *genuinely distinct* candidates evaluated and
the honesty of the statistic used to pick among them. So the design question is
not "how much tuning should we allow" but "how many distinct tests does this
script perform, and does the acceptance rule survive that count."

Our existing scripts answer that in three ways, and all three carry over.

## What we already constrain, concretely

**1. Structure frozen, thresholds only.** In both generations the parse fixes
the variables, the operators, the number of conditions, and the household-size
stratum. Only numeric thresholds move. `state_threshold_gridsearch_v2.R` scales
each threshold by `FACTORS_FINE = (0.75, 0.90, 1.00, 1.10, 1.25)` for rules with
three or fewer conditions and `FACTORS_COARSE = (0.90, 1.00, 1.10)` for deeper
ones, capped at `MAX_VARIANTS = 700` per rule. The bracket *narrows as the rule
gets more complex*: combinatorics grow exponentially in the condition count, so
the per-condition grid shrinks to hold the comparison count down. The v1 scripts
do the same thing with absolute lattices instead of multipliers (dollars step 50,
ratios 0.05, counts 1), bounded to the 2nd to 98th percentile of the state's own
data, `MAX_GRID_PTS = 20` per variable, `MAX_COMBOS = 5000` per rule.

This is the single biggest constraint. It converts unbounded rule discovery into
a small perturbation of an already-validated rule. A plus-or-minus 25 percent
bracket is defensible on its own terms: thresholds are rounded to three
significant digits from wherever a tree happened to split, so a nearby cut is
the same rule relocated inside its own estimation noise, not a new hypothesis.

**2. The shipped value is always in the grid, and wins ties.** Both generations
add the original threshold to the candidate set explicitly. The v2
partition-aware dedup goes further: a candidate is kept only if it induces a
*different partition of the state's observed values* (`findInterval` over sorted
unique values), cuts that can never fire in the state are dropped, binary
indicators collapse to one variant, and within a group of equivalent cuts the one
closest to the shipped value is kept. Two cuts with no data point between them
are one test, not two. Since the effective test count is what drives the curse,
deduping to distinct partitions is the correct accounting, not just a compute
saving.

**3. Qualification and selection are separate, and the choice set is enumerated
in advance.** Qualification asks whether a variant may deploy at all (90 percent
Wilson LCB of state-train precision at or above 0.20, plus a support floor);
selection asks which qualifying variant to deploy (dollar-max or LCB-max). The
gate controls false discovery; the objective controls reach. Findings section 9
tested three combinations and found no dominance, so the objective must be fixed
globally before the run, never chosen per rule.

The five-similar-states work constrained itself the same way: four similarity
definitions specified in advance, top-5 donors, and the deliverable ranked by
*neighbor*-train precision. The record is explicit that selecting "rules that
held in the state's test" would be a fresh winner's curse. The same discipline
applies here.

## Recommended structure: three tiers, defaulting to the lowest

**Tier 0, default: no tuning at all.** Re-fill the frozen list against the
state's own caseload, walking rank order until capacity fills. This is already
what the delivery list is built for, it uses no outcomes, and it cost under a
point of precision versus an idealized after-the-fact list (0.294 vs 0.301 at a
5 percent budget across 18 states, section 15). Most states should stop here.

**Tier 1: re-filter and re-rank, rule text untouched.** Score each shipped rule
on the state's internal data, admit by Benjamini-Hochberg at FDR 10 percent
against the *state's own* base rate with n at or above 30 flagged, re-rank by the
state's own 99 percent Wilson LCB. The search space is subsets and orderings of a
fixed list (median 23 rules at 5 percent, 42 at 10 percent), so the multiplicity
is exactly the list length and BH handles it directly. This is the `filtered` arm
of `methods/deployment_state_adaptation_v2.R`.

**Tier 2: threshold tuning inside the bracket.** Only for rules that already
clear Tier 1. Port `rule_variants()` as written: multiplicative bracket,
complexity-scaled grid, partition dedup, `MAX_VARIANTS` cap, shipped value
included and tie-favored. Qualify at the 90 percent LCB with n at or above 30 on
state data, select on the pre-declared objective. Because the number of distinct
variants per rule is known, pay for that within-rule search explicitly: set the
qualification z from `variant_gate_alpha / m_variants` (a Bonferroni adjustment
inside the rule family), which is the same idea as letting BH set the bar from the
number of candidates.

Anything beyond that (new variables, dropped conditions, flipped operators,
changed strata) is not tuning, it is mining, and it belongs in the own-pool
fallback with its own floor, not in a script we hand out.

## Guards that must be in the code, because we will not be there

- **n at or above 30 on the tuned variant's own state support.** Not the shipped
  rule's national support. This is the non-negotiable one. At n at or above 5
  with an LCB gate, single-state tuning collapsed: median holdout precision
  0.000, 59 percent of rules caught nothing. At n at or above 30 it deflates
  gently instead, median train 0.33 to holdout 0.21 (section 9).
- **Time-based split, never random.** Hold out the state's most recent year.
  Every validation in this repo is year-based, and section 14 exists precisely
  because same-era judging flattered the transfer result until we shifted the
  test forward.
- **The holdout decides one comparison per tier, not one per rule.** Compute
  each arm's union, fill to the review budget in rank order on the tuning years
  only, and compare arms on the held-out year under a decision rule written
  before the run. Per-rule holdout selection re-opens the curse at the last
  step. The implemented rule: promote a tier only if its holdout precision's 90
  percent Wilson LCB exceeds Tier 0's holdout point precision, with at least 30
  flagged holdout cases to compare on.
- **Refuse to tune when the data cannot support it.** Section 9's rule of thumb
  is roughly 30 or more rules qualifying on state training data; below that,
  Tier 2 is off and the script says so rather than producing a list.
- **Always carry the untuned arm.** Both existing R scripts report tuned and
  as-is side by side; a state-facing script should refuse to emit a tuned list
  without the Tier 0 benchmark next to it.
- **Print the comparison count.** Distinct variants actually evaluated, after
  dedup, per rule and in total. The validity argument depends on that number, so
  make it visible rather than implicit.
- **Any-error scoring.** State samples are too thin for typed evaluation, and
  reviews find whatever error is present.

## What to tell states to expect

Tuning buys reach, not precision. Connecticut's tuned list caught 43 percent of
errors (49 percent of error dollars) at 0.209 precision against the national
list's 24.4 percent at 0.228: nearly double the reach at essentially the same
precision. Where support was thin it failed outright, Washington falling to 0.048
while the untouched national list held 0.364 (section 9). Quote the deflation
expectation, roughly a third off state-train precision, not the training numbers.

One asymmetry worth naming to them: their internal data contains ineligible cases
the public file omits entirely, which is why public visibility runs from 43
percent (New Jersey) to about 91 percent, and 71 percent nationally (section 10).
Their tuning data is therefore a richer population than the one that mined the
rules. That makes it the honest test, and it also means some threshold movement
will be real signal rather than noise. It does not relax any of the floors above.
