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

Memory prerequisite, scoped precisely: the chunked reducer
(`reduce_flags_for_rules()`) already exists in the helpers and has scored
full national pools repeatedly without incident (the delivery builder since
2026-07-22, and three full mines in the findings 31 study), so the
attribution arms and the state-side arm need NO code fixes and are not
gated. What remains unfixed is the two finder scripts (`INCL_find_*_v2.R`,
`EXCL_find_*_v2.R`), which still call the old evaluator 3x per frame x 5
frames x 3 strata; that gates only the follow-on five-frame regen if it runs
through those scripts, and the fix is porting their calls to the existing
helper (ledger hazard row; RESUME.md A1-F1 caveat).

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
which one earned any change. Four arms, the full factorial (decided by Eric
2026-08-05: it judges the outlier features independently of at_max and
detects any interaction between them, since both touch benefit-boundary and
shelter cases):

| arm | vocabulary |
|---|---|
| baseline | current 19 features (re-mined, same seed discipline as findings 31) |
| +atmax | baseline + at_max_benefit |
| +outliers | baseline + the five outlier indicators |
| +atmax+outliers | the full proposed set |

Any-error national frame, train FY2022-23, test FY2024 (true future year),
one mine per arm (findings 31 timed the identical mine-plus-score at roughly
an hour per arm, so four arms fit one overnight). Metrics per arm: delivered
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
deliverable changes ride: rule_id and the characterization columns (findings
29) ship in the same MINOR bump as the vocabulary; the family_id substitutes
column waits for a later bump (decided by Eric 2026-08-05: it is not yet
built, and holding the bump for it buys nothing). One regen, not two: the
A1-F1 code upgrades and the feature change land together rather than
regenerating twice. Anything that reaches `state_delivery_lists/` is a MINOR
version bump and Eric's decision at ship time; nothing in this proposal
ships by itself.

## State-side arm: typed mining within states, unstratified (added 2026-08-06, Eric)

Question: does adding each state's own typed-frame rules, mined WITHOUT
household strata, to the state side of the blend improve that state's
delivered list? Per state: the any-error frame mined per stratum (unchanged)
plus the four typed frames mined pooled across household sizes.

Why the settled record does not forbid this, stated by scope: findings 17
retired typed frames from the NATIONAL delivery vocabulary and says nothing
about state pools; findings 11 validated coarse strata for NATIONAL mining
and the strata choice was never separately tested at state scale, where
strata carve a ~1,500-row pool into pieces whose typed targets have nearly
no events. Dropping strata there is support preservation, not a re-opened
claim.

Support arithmetic, indicative (the design note computes it per state): a
state's public FY2022-23 pool carries 96 to 280 errors; by type that is
roughly 10 to 130 events per typed frame on the full ~1,500 rows. Admission
(BH vs the frame base rate plus n >= 30) will admit nothing in the thinnest
cells; that outcome is measured, not failed.

Named risk, from findings 17's mechanism: small-support state-typed rules
with inflated bounds crowding the top of the blend. The readout therefore
counts how many state-typed rules deploy and what they displace, alongside
the within-state precision and dollar deltas at both budgets on FY2024.

This arm varies the state-pool component of the blend only, so it runs
separately from the four national-vocabulary arms with its own
pre-registered bar, riding the same overnight window. Mining cost is small
(49 states x 4 typed frames at state scale); evaluation reuses the delivery
builder machinery.

## The fresh-share floor rides this release (added 2026-08-07, Eric)

Findings 33-34: the fresh-share floor is two-era validated (era 1 blended
+0.0118 vs a +0.010 bar; era 2 +0.0070 vs a +0.005 bar; two-era pooled
+0.0100; dollar guard 0.0000 everywhere at f = 0.50) and goes into
`INCL_build_blended_delivery_list_v2.R` as part of this release. **The
release target is v2.5.0**, bundling: the re-mine vocabulary (whatever the
attribution arms admit), rule_id, the characterization columns, and the
fresh-share floor. One regeneration, one migration for states.

**Threshold selection, pre-stated before the fine grid is read.** The
existing grid runs {0.25, 0.40, 0.50, 0.60, 0.75}; the fine step adds
{0.55, 0.65, 0.70} on all three harnesses (era-1 blended, era-1
national-only bridge, era-2), re-walk only, no mining, roughly 1.5-2 hours.
The mechanism is validated; this is dose-finding on an engineering
parameter, per the engineering-artifacts rule. Selection rule: choose the f
in {0.40 ... 0.75 at 0.05 steps} maximizing the MINIMUM of the two eras'
5%-budget medians (era-1 blended and era-2), subject to the dollar guard
(>= -0.005) on [SCOPE - Eric's choice, see below]; ties break toward the
lower f. The shipped effect estimate is the two-era pooled median at the
chosen f, never the winning grid cell. Open knob for Eric before the grid
runs: whether the dollar guard binds at both budgets on both eras (stricter;
currently disqualifies 0.60, whose era-1 blended 10% dollar change was
-0.0184) or at the 5% budget only (currently leaves 0.60 the leader).

Builder implementation is protected-file work: PDS writes, fresh review
against findings 33-34 and the chosen threshold, regression test, then the
single v2.5.0 regeneration carries everything.

## Feedback requested

- **Ben**: the outlier feature definitions. Which variables, the 99th
  percentile against alternatives, within-stratum vs overall, and whether
  frozen train-year dollar cutoffs published in the dictionary work for how
  states would evaluate the rules. Also whether issue #8's ineligible-case
  question should shape any feature here or stays separate (our read:
  separate; the public file has no feature rows for those cases to mine).
- **Eric**: decided 2026-08-05, sequencing corrected 2026-08-06. Four
  national arms (full factorial) plus the state-side typed arm; the
  attribution run is not gated on any code fix and goes first, the finder
  de-OOM happens before the follow-on five-frame regen; the bump carries
  the vocabulary, rule_id, and the characterization columns, with family_id
  deferred.
- **Both**: anything missing from the feature list while the mine is paid
  for; and reactions to the state-side typed arm above, particularly whether
  the deploy-and-displace readout covers what a state would want to know
  before its blend gains typed rules.
