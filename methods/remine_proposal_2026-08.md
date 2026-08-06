# Proposal: one re-mine to settle the open vocabulary questions

**Status: proposal seeking feedback from Eric and Ben (2026-08-05). Nothing
here is scheduled.** A run happens only after both have weighed in, the
four-item pre-run design note is approved, and the study script passes the
fresh senior-statistician review (`methods/known_constraints.md#routing`).

## Why one re-mine, and why now

Three open items each require a full re-mine, because each changes the mining
vocabulary, and a full national mine plus scoring costs a night. Riding them
together pays for that night once:

1. **The at-max-benefit feature** (issue #1, findings 28). The reconstruction
   puts 2.39% of truly-at-max households just below `rawben_rel_max` = 1, and
   88 delivered rules take 76.7% of their flags from those artifact rows.
   Findings 31 raised the priority: two of three independent mining seeds put
   a `rel_max`-band rule at the very top of the national ranking, so the
   artifact is not a tail problem, it sits at rank 1.
2. **Per-stratum outlier features** (issue #7). Ben's measurement: household
   size 3 cases above the within-stratum 99th percentile on shelter expense
   run 26.0% error (69 of 265) against the roughly 11% base rate, with
   19.6 to 23.8% on capped shelter, medical, earned and unearned income. The
   open question a re-mine answers: do explicit outlier indicators find
   pockets the depth-4 trees do not already reach by splitting on raw values?
3. **The A1-F1 finder upgrade** (rule_id on emitters, admit_bh helper,
   dollars-per-flag) already requires regenerating the vocabulary; it has
   been queued since 2026-07-22.

Prerequisite before any of it: the finder and EXCL scripts must get the
chunked-reducer treatment (`reduce_flags_for_rules()`), or the regen OOMs
this box (ledger hazard row; RESUME.md A1-F1 caveat).

## Proposed feature set (the decision we want feedback on)

- `at_max_benefit`: indicator for `rawben == benmax`, the honest split the
  reconstruction currently smears into the [0.987, 1) band (findings 28
  defines the artifact; the feature gives the miner the true boundary).
- Outlier indicators for Ben's five: `rawsltexp`, `rawcsded`, `rawmeded`,
  `rawearn`, `rawunearn`, each as "above the within-stratum 99th percentile."
  Two design constraints for deployability and validity:
  - Percentile cutoffs computed on the TRAINING years only and frozen as
    absolute dollar values (a cutoff computed on all years leaks the test
    year into the vocabulary).
  - The published cutoff values ship with the data dictionary, so a state
    can evaluate the rules without our quantile code.
- Explicitly staying out: `second_element_i` (hazard row: state reporting is
  inconsistent); no change to the target, strata, engines, admission, or
  ordering (all settled rows; findings 4, 11, 19, 20).

Question for both reviewers: is anything else worth adding while we are
paying for the mine? A feature added later costs another full night; that is
the one-shot economics of this proposal.

## Evaluation design (sketch; the design note will pin it)

Attribution needs arms, because adding both feature groups at once cannot say
which one earned any change:

| arm | vocabulary |
|---|---|
| baseline | current 19 features (re-mined, same seed discipline as findings 31) |
| +atmax | baseline + at_max_benefit |
| +atmax+outliers | the full proposed set |

Any-error national frame, train FY2022-23, test FY2024 (true future year),
one mine per arm (findings 31 timed the identical mine-plus-score at roughly
an hour per arm, so three arms fit one overnight). Metrics per arm: delivered
precision at the 5% and 10% budgets on the findings 25/26 harness, the
findings 28 artifact-exposure share of delivered cases (we want it to fall),
error and dollar coverage, and any-error beside frame-relative throughout.

One yardstick findings 31 gives us for free: budget-depth lists vary with the
mining seed alone (errors-caught overlap 0.531 at the 5% budget), so
precision deltas smaller than seed noise cannot be attributed to the
features. The design note will carry explicit pre-registered bars; the
working candidates are (a) the new vocabulary must not lose delivered
precision beyond seed noise, (b) the artifact-exposure share must fall
materially (findings 28 baseline: median 6.3% of delivered cases at the 5%
budget, worst state 16.3%), and (c) outlier features earn their place only if
they appear in delivered lists with real support.

If the attribution run clears its bars, the follow-on is the full five-frame
regen with the winning feature set, which is also where the queued
deliverable changes ride: rule_id, the characterization columns (findings
29), and the family_id substitutes column. Anything that reaches
`state_delivery_lists/` is a MINOR version bump and Eric's decision at ship
time; nothing in this proposal ships by itself.

## Feedback requested

- **Ben**: the outlier feature definitions. Which variables, the 99th
  percentile against alternatives, within-stratum vs overall, and whether
  frozen train-year dollar cutoffs published in the dictionary work for how
  states would evaluate the rules. Also whether issue #8's ineligible-case
  question should shape any feature here or stays separate (our read:
  separate; the public file has no feature rows for those cases to mine).
- **Eric**: the arm structure, the sequencing against A1-F1 (de-OOM first,
  attribution run second, full regen third), and which deliverable changes
  ride the same version bump.
- **Both**: anything missing from the feature list while the mine is paid
  for.
