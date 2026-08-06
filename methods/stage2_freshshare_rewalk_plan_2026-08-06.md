# Stage 2 pre-registration: a fresh-share floor in the delivery walk, judged on the section 30 bar

**Written 2026-08-06, before any re-walk number existed.** This executes the
no-arm gate of `methods/profile_distance_diagnostic_plan_2026-08-06.md`. The
diagnostic's one SIGNAL was instrument i1, flag overlap / fresh share: 5%-budget
tercile gap 0.0876 against a permutation 95th percentile of 0.0656, positive at
the 10% budget (0.0817), 0.0802 on f < 0.99 capacity, split-half certificate
0.951 (`methods/profile_distance_diagnostic/summary.txt`,
`analysis2_tercile_gaps.csv`). The five other instruments cleared nothing. The
gate committed to a retrospective re-walk on the section 32 harness, same rules,
same capacity, LCB order with the winning instrument as tie-break or diversity
term, scored on FY2024, judged on the section 30 bar. "Tie-break or diversity
term" is two mechanisms; this document pins one, and everything else, before any
re-walk number exists.

## The pinned mechanism: a fresh-share floor, f >= 0.50

The walk becomes two passes, applied identically to the core fill and the
buffer fill (the floor is a property of the whole delivered list, not of the
core alone):

- **Priority pass**: walk in LCB order keeping a rule only if its sequential
  fresh share f = n_new / n_flagged, computed at its turn on the build
  caseload (the bench lists' `n_new_at_rank / n_flagged_state`), is at least
  **0.50**.
- **Completion pass**: if capacity is not filled when the priority pass
  exhausts the pool window, walk the skipped rules again in LCB order under
  the shipped test (n_new >= 1) until capacity fills.

Capacity consumption is therefore identical to the baseline BY CONSTRUCTION
(asserted in the harness; a deviation is a bug that halts the run, never a
result). The design principle this encodes, standing for all future plans
(Eric, 2026-08-06): an outcome preventable by a design change or an
incremental engineering parameter, without overturning the analytical
approach under test, is a design requirement, not a failure mode. Running
out of qualifying rules is an engineering artifact of a one-pass walk; in
practice a review program never runs out of cases to review, and pretending
otherwise would have put the analytical question at the mercy of a
construction detail. Nothing else about the walk changes; the 99% Wilson LCB
ordering statistic is untouched in both passes.

Why this and not the alternatives:

- It is the minimal extension of the shipped walk. The walk already applies an
  outcome-free skip test at every rank (n_new >= 1, section 27); the floor
  tightens that test from f > 0 to f >= 0.50. The 99% Wilson LCB ordering
  statistic is untouched.
- The penalized score (LCB minus a penalty on 1 - f) is a NEW ordering
  statistic with a continuous weight to set. Every ordering-statistic
  intervention tried against the plain LCB has lost or failed to transfer
  (shrinkage, section 18; alternative z, section 20; out-of-fold, section 30;
  marginal reordering, section 32), and a free continuous parameter is exactly
  the tunable-degree-of-freedom shape that graveyard punishes. Rejected.
- The near-tie break (higher f wins within epsilon of LCB) has no epsilon
  derivable from the diagnostic, which measured tercile contrasts of a
  distance, not LCB spacings; and it acts only inside epsilon bands, so its
  dose cannot reach the ~43% of capacity where the loss lives. Rejected.
- Identification note: the diagnostic's winner is pairwise fresh overlap to
  predecessors; sequential fresh share is the same flag-overlap quantity in
  its walk-native form (overlap with the union of predecessors rather than the
  closest one), computable from the builder's existing columns, from flags
  alone.

**Threshold derivation, fixed here.** 0.50 is the instance-level lower-tercile
boundary of build-time fresh share among the 1,581 core rule-instances on the
49 5%-budget bench lists (computed for this plan from
`methods/anyerror_blended_holdout_2024/bench_list_*_budget05.csv` as
n_new_at_rank / n_flagged_state; the capacity-weighted boundary is 0.60). The
diagnostic's verdict is a tercile contrast, bottom tercile marginal precision
0.245 against top 0.332 at the 5% budget, so the floor removes approximately
the bottom tercile and nothing else. Applied retroactively it would have
skipped 27.3% of core instances carrying 20.7% of 5%-budget capacity (37.3% /
31.5% at 10%) in the priority pass, refilled from deeper ranks (section 31:
every admitted pool covers all 4,803 FY2024 errors, so depth is available)
with the completion pass closing any remainder. Whether the refill's
precision transfers is precisely what this study measures.

**Deployment in one sentence:** f uses flags only and is computed during the
frozen-list build exactly where `n_new_at_rank` is computed today, so the
deliverable remains a frozen list built by the same outcome-free walk
(sections 15-16) and states receive no new machinery.

## One component varies

Held fixed: the cached FY2022-23 mined pools and their admission (pooled BH at
FDR 10% AND n >= 30), the 99%-LCB ordering statistic and the rank order it
produces, the capacity rule (fill to budget, buffer to 3x, core-only scoring),
the 5% and 10% budgets, FY2024 scoring, and the 49-state section 32 harness.
The one change: the per-rank skip test tightens from n_new >= 1 to f >= 0.50.

## Bars, pre-stated

- **Primary (the section 30 bar).** Per state, one paired difference:
  delivered (any-error) precision of the re-walked core list on FY2024 at the
  5% budget, minus the same number for the unmodified bench list. There are no
  partitions here, so nothing is averaged within a state: "within-state" IS
  that single paired difference. Bar: median across the 49 states >= +0.010.
- **Secondary (directional only).** The same median at the 10% budget > 0.
- **Companion guard, required for a win.** Within-state median change in
  FY2024 dollar recall at the 5% budget >= -0.005 (half the bar). At fixed
  capacity, precision and errors caught move together by arithmetic; only the
  dollar mix can decouple, and a drop past half a bar means the mechanism
  buys precision by shrinking dollar coverage, which is not a win. This guard
  is analytical: a dollar-mix shift is an inherent consequence of changing
  which cases fill the budget and cannot be engineered away without
  overturning the mechanism, so it is a legitimate judged outcome. Capacity
  equality, by contrast, is a construction property: it holds by design (the
  completion pass) and is ASSERTED, not judged.
- **Companions reported, no bar:** errors caught, dollar recall, fill rate per
  state, walk depth reached, both budgets.

## Multiplicity, honestly

i1 won a six-instrument race, so its selection carries multiplicity. Two
things guard stage 2. First, the stage-2 statistic (list-level delivered
precision of counterfactual re-walked lists at fixed capacity) was never
computed by the diagnostic, which selected instruments on per-rule
marginal-precision tercile gaps within the EXISTING lists; the two share the
FY2024 outcome year but not the statistic, so the instrument race could not
tune itself to this readout. Second, the guard is partial, not absolute, and
the one-shot rule closes the gap: **if the primary mechanism fails the bar,
the other mechanisms (near-tie break, penalized score, the other five
instruments) are NOT then tried on this outcome.** A failed bar closes the
line at public-data scale, recorded in writing like sections 18, 20, 30, 32.

## Sensitivity readouts, descriptive only

One pre-stated grid: f_min in {0.25, 0.40, 0.50, 0.60, 0.75}. For each, the
median paired precision difference, dollar-recall difference, fill rate, and
median walk depth, both budgets, as one table. It carries no verdict and no
threshold is re-chosen on it; if the curve peaks away from 0.50, that is a
note for a possible second-era pre-registration, not a result of this one.

## Support and arithmetic

No split, no mining; every input is cached, minutes of compute. Harness
verified: the walk machinery reproduces all 98 bench lists and their per-rank
accounting against
`methods/marginal_precision_diagnostic/per_rule_marginal.csv` (98/98,
diagnostic coverage report), and the FY2024 scoring reproduced the committed
scorecard (section 32). Anchor for this run, checked before any counterfactual
number is read: with f_min = 0 the re-walk must rebuild all 98 lists
rule-for-rule and match per_rule_marginal.csv's holdout n_new / k_new at every
rank. Ranked-window evaluation keeps the section 27 slack-zero certificate;
any window that under-fills is redone unpruned.

Detection arithmetic, in advance: a median state's 5% budget flags ~44 FY2024
cases, per-state binomial SE ~0.068; the 49-state median of paired differences
carries an SE of roughly 0.008 (the diagnostic plan's detection arithmetic,
which credits the pairing). The recoverable ceiling is section 32's 3-4pp, all
of it on the ~43% of capacity with f < 0.99; the plausible transferable effect
is 0 to 2pp. So only an effect near the top of the plausible range clears the
bar, and a true +0.005 effect will likely fail. Accepted: a refinement too
small to show +0.010 across 49 states is not worth a pipeline change.

## Consequences, pre-stated

- **Bar and guards cleared:** the fresh-share floor becomes a validated
  selection-refinement CANDIDATE for `INCL_build_blended_delivery_list_v2.R`.
  Promotion into shipped lists is a MINOR bump and Eric's call, and per the
  section 20 discipline it additionally requires replication on the second era
  before shipping: the cached train FY2017-18 / test FY2019 harness
  (`methods/state_similarity_v2/era_validation_train1718_test19/`, national
  pool plus 18 states), same mechanism, same 0.50 threshold, bar pre-stated
  then.
- **Bar failed, or the dollar-recall guard failed:** the line closes at public-data scale and
  the ledger records it beside sections 18/20/30/32. The diagnostic's SIGNAL
  still stands as a logged property: marginal quality IS predictable from
  fresh share, the only instrument of six to clear its gates; its actionable
  value at fixed capacity was insufficient, and the question stays open at
  internal-data scale only, like the section 32 row it extends.

## Constraints

Outputs to `methods/freshshare_rewalk/`; read-only on `state_delivery_lists/`
and the bench directory; no CHANGELOG entry, no version bump. Routing rule
applies: fresh senior-statistician review of the run script before launch.
