# Pre-registration: bridge arm and second-era replication of the fresh-share floor

**Written 2026-08-06, before any bridge or era-2 number existed.** Follows
`methods/stage2_freshshare_rewalk_plan_2026-08-06.md` and its result (findings
section 33): on the blended 49-state harness, the f >= 0.50 floor cleared the
pre-registered bar (median paired precision +0.0118 at the 5% budget vs bar
+0.010; dollar guard +0.0000; secondary +0.0061; 29/16/4 states
better/worse/tied; all anchors and 588 capacity assertions exact,
`methods/freshshare_rewalk/summary.txt`). That plan's consequence clause
requires second-era replication before any shipping decision. This document
pre-states the whole of that replication: a bridge arm, one confirmatory
threshold, one challenger under a compound rule, bars, guards, eligibility,
anchors, and consequences.

## The design chain: one component varies at each link

- **Stage 2 (done)**: blended bench lists, era 1 (build FY2022-23, score
  FY2024), f = 0.50 vs f = 0.
- **Arm A, bridge**: NATIONAL-ONLY lists, same era 1. Against stage 2, pool
  composition varies and nothing else.
- **Arm B/C, era 2**: national-only lists, era 2 (build FY2017-18, score
  FY2019). Against the bridge, the era varies and nothing else.

Stage 2 ran on blended lists; a national-only era 2 compared directly against
it would vary era and pool composition at once, the two-at-once error of the
2026-08-04 study. The bridge isolates pool composition inside era 1 so era 2
carries a single difference.

## Arm A: the bridge (era 1, national-only, f = 0.50)

Re-run the stage-2 re-walk with the state pools removed from the blend: the
cached FY2022-23 national admitted pool
(`methods/delivery_pools_2022_2023_anyerror/filtered_national_any_error_fdr10.rds`,
the source of the bench lists' national slots), 99%-LCB order, budgets 5%/10%
of the state's FY2022-23 caseload, buffer to 3x, core-only scoring on FY2024,
all 49 states, f = 0.50 vs f = 0 baseline.

**The expectation, recorded in advance as a hypothesis:** the state-rule slots were
DILUTING the floor's benefit, so the bridge effect at the 5% budget should be
at least the blended +0.0118. The result is reported against this number and
the verdict on the hypothesis is logged whichever way it goes.

**The proceed/re-scope floor, set separately from the hypothesis:** the bridge
effect has "vanished" if the 5%-budget median paired difference is below
**+0.005**. Rationale: the bridge median carries the same SE as stage 2
(roughly 0.0106; same 49 states, same year, same flag counts), so demanding
the full +0.0118 would misread noise as vanishing (a true +0.0118 effect
falls below +0.0118 about half the time); +0.005 is half the blended point
estimate, 0.6 SE below it (a true blended-size effect fails it ~26% of the
time), and equals the era-2 confirmatory bar, so a proceeding era 2 stays
interpretable.

**Branches, pre-stated:** bridge median >= +0.005 at the 5% budget, era 2
proceeds under this plan. Bridge median < +0.005, era 2 is NOT run under this
plan; the finding logged is that the floor's benefit is concentrated in the
interaction with state-rule slots, the dilution hypothesis is refuted in
writing, and any re-scoped era-2 design (the era cache holds state mines for
18 states only) requires a fresh pre-registration.

The bridge carries no shipping verdict of its own: the floor already cleared
era 1 on the blended harness, and FY2024 is being read a third time here. Its
only job is to make era 2's single-difference comparison valid.

## Arm B: era-2 confirmatory, f = 0.50 (the shipping gate)

**Pool.** The cached national FY2017-18 mine:
`methods/state_similarity_v2/era_validation_train1718_test19/raw_vocab/raw_national.rds`,
verified this session as 145,313 rules with FY2017-18 train n/k (columns hh,
rule, engines, mined_frames, n, k). No state mines are used or needed.
Admission is recomputed from the cached n/k with the SHIPPED recipe (pooled BH
at FDR 10% against FY2017-18 stratum base rates AND n >= 30), not the era
scripts' within-stratum variant that section 31's review flagged. Ordering:
99% Wilson LCB from the cached n/k. Budgets 5%/10% of the state's FY2017-18
caseload, buffer to 3x, frozen, core-only scoring on FY2019. Comparison:
f = 0.50 vs f = 0, identical inputs, the per-rank skip test the only
difference.

**Eligible states, fixed by threshold, never by results.** Computed from
`reg_model_data.rds` this session (FY2017-18 build, FY2019 test): national
build 79,907 rows / 7,115 errors; test 39,221 rows / 3,931 errors; 49 states.
Threshold, pre-stated: a state is eligible if its FY2019 caseload has at
least 400 rows (so the 5% budget projects to >= 20 flags, per-state binomial
SE at most ~0.11, the scale of era 1's worst states) AND at least 20 FY2019
error events (a non-degenerate dollar-recall denominator). Exactly two states
fail: Wyoming (273 rows, 13 errors) and South Dakota (654 rows, 15 errors).
**Eligible set: 47 states**, fixed here. Median FY2019 caseload projects 42
flags at the 5% budget (10th percentile 30, maximum 53), before the ~0.85
median fill observed at era 1.

**The bar, with the power arithmetic done on our own numbers.** Per eligible
state, one paired difference (re-walk minus baseline, core-list any-error
precision on FY2019 at the 5% budget). Bar: **median across the 47 states
>= +0.005**, plus the dollar guard below. Why +0.005 and not +0.010: the SD
of the stage-2 per-state paired differences is 0.0589
(`methods/freshshare_rewalk/per_state_paired.csv`, f = 0.50, 5% budget), so
the median's SE is about 1.2533 x 0.0589 / sqrt(47) = 0.0108 (era-2 flag
counts are like era 1's, median 42 vs ~44, so the era-1 SD is the best
available estimate). Against a true effect equal to the era-1 point estimate
+0.0118, a +0.010 bar has 57% power, and the era-1 estimate is itself biased
upward conditional on having cleared its gate, so realistic power is below a
coin flip; a confirmatory gate that fails a true effect more often than not
is a badly designed gate. At +0.005 the power against +0.0118 is 74%. The
type-I cost is controlled jointly, which is the correct unit for a two-stage
design: under a true null the era-1 gate is cleared with probability ~0.17
(bar 0.010, SE 0.0106) and this bar with probability ~0.32, jointly ~5.5%,
with each stage pre-registered. The shipping claim, if both hold, is
therefore "a +0.010-scale effect supported by two eras jointly", and the
two-era pooled median is reported beside the era-2 number.

**Pre-stated expectation (the statistician's, distinct from the bar):**
directional replication with attenuation; era-2 median positive but likely
below +0.0118.

**Guards.** Dollar recall is the only judged guard: within-state median
change in FY2019 dollar recall at the 5% budget >= -0.005, same absolute
value as stage 2 since dollar recall is on the same scale across eras. The
justification is unchanged and analytical: at fixed capacity, precision and
errors caught move together by arithmetic, only the dollar mix can decouple,
and a drop past this size means the mechanism buys precision by shrinking
dollar coverage. Capacity equality is NOT a judged guard: the two-pass walk
asserts it by construction (588/588 exact at stage 2), and any assertion
failure aborts the run as a bug, not a result. Secondary, directional only:
the 10%-budget median > 0. Companions reported, no bar: errors caught,
dollar recall both budgets, fill rate, walk depth.

**Independence of FY2019, stated honestly.** FY2019 was used by the section
19-20 era validation to confirm the shipped admission and ordering defaults,
so it is not virgin for the pipeline. It is virgin for the fresh-share line:
no fresh-share quantity has ever been computed on it. The judged comparison
is paired, baseline and floor sharing the same shipped defaults, so the
defaults' prior exposure to FY2019 cancels in the difference.

## Arm C: era-2 challenger, f = 0.60 (compound rule)

The era-1 sensitivity grid showed 0.60 at +0.0261 (5% budget) but with a
warning signature at 10%: precision +0.0000 with dollar recall -0.0158
(`methods/freshshare_rewalk/sensitivity_grid.csv`). The challenger is
evaluated ONLY if the confirmatory arm clears its bar, and it changes the
shipping threshold only if ALL four conditions hold:

1. clears the era-2 confirmatory bar (median >= +0.005 at the 5% budget, 47
   states);
2. strictly beats 0.50's era-2 5%-budget median;
3. holds the dollar guard at the 5% budget (>= -0.005);
4. does not reproduce its era-1 warning signature at era 2, "material"
   defined from the era-1 grid: signature = 10%-budget median precision diff
   <= 0 AND 10%-budget median dollar-recall change <= -0.010 (twice the
   judged guard and about two thirds of the era-1 observed -0.0158; a smaller
   loss inside the guard's noise band is not the signature).

Multiplicity, stated plainly: two thresholds are being judged on one new era.
The price is paid by asymmetry: 0.50 is the default in every world, the
challenger must clear four conditions including everything 0.50 must clear
plus superiority plus the signature test, and if the confirmatory arm fails,
the challenger cannot ship regardless of its own numbers, because that would
be choosing the threshold on the era-2 outcome.

## One-shot rule

If the confirmatory arm fails, no other threshold, mechanism, or instrument
is then tried on the FY2019 outcome. The line closes at public-data scale
and is recorded in writing, the section 20 pattern: the era-1 result (section
33) stands as recorded, and the floor does not ship.

## Sensitivity readouts, descriptive only

The full grid f_min in {0.25, 0.40, 0.50, 0.60, 0.75} at both the bridge and
era 2, both budgets: median paired precision difference, dollar-recall
difference, fill rate, walk depth, one table each. No verdict attaches and no
threshold is re-chosen on it; a curve peaking elsewhere is a note for a
future pre-registration, not a result of this one.

## Anchors and construction properties

Checked before any counterfactual number is read; any failure aborts.

- **Bridge baseline identity.** No committed national-only era-1 lists exist,
  so the anchor is algorithmic: the F_MIN = 0 two-pass walk on the national
  pool must reproduce, rule for rule, the output of the shipped fill logic
  (`INCL_build_blended_delivery_list_v2.R`) run national-only on identical
  inputs, all 49 states x 2 budgets; and every national rule's
  `precision_train_lcb` must match its value on the committed bench lists.
  The blended machinery itself is already anchored (98/98 rebuilds, stage 2).
- **Era-2 cache determinism** (the section 31 pattern): recompute n/k on
  FY2017-18 from `reg_model_data.rds` for all 145,313 cached rules; must
  match `raw_national.rds` exactly.
- **Era-2 baseline identity**: same algorithmic anchor as the bridge, on the
  era-2 admitted pool, 47 states x 2 budgets.
- **Support reconciliation**: per-state FY2017-18 and FY2019 row and
  error-event counts recomputed by the run must match this plan's support
  table (totals above; full table written as the run's first output).
- Ranked-window evaluation keeps the section 27 slack-zero certificate;
  under-filled windows are redone unpruned (era-1 stage 2 needed 54 redos).

## Consequences, pre-stated

- **Bridge held and confirmatory cleared (guards held):** the fresh-share
  floor is validated on two eras and goes to the project lead as a MINOR-bump promotion
  decision for `INCL_build_blended_delivery_list_v2.R`. The shipping
  threshold is 0.50 unless the challenger's four-condition compound rule was
  met, in which case 0.60 goes forward as the proposed threshold. Either way
  the decision, the CHANGELOG entry, and the version bump are the project
  lead's, not this study's.
- **Confirmatory failed (or its guard failed):** the section 20 pattern. The
  era-1 result stands as recorded in section 33; the floor does not ship;
  the ledger row closes the line at public-data scale in writing, open at
  internal-data scale only.
- **Bridge failed:** the re-scope branch of Arm A; era 2 does not run under
  this plan; the pool-composition finding is logged either way.

## Constraints

Outputs to `methods/freshshare_rewalk_era2/` (bridge outputs in a `bridge/`
subfolder); read-only on `state_delivery_lists/`, the bench directory, the
delivery pool caches, and the era cache; no CHANGELOG entry, no version bump.
Routing rule applies: fresh senior-statistician review of the run script
before launch.
