# Pre-registration: seed stability of the shipped pipeline's error coverage

**Written 2026-08-05, before any result existed.** No overlap number between
mining seeds has ever been computed on this pipeline (the 2026-08-05 partition
study, findings section 30, measured half-data partitions, a different and
larger perturbation). Nothing below was informed by a result. Approved on
2026-08-05 (after two amendments he requested: the five-seed
union-accumulation extension and the explicit national-only statement); the
run launches only after the routing-rule review of the implementation.

## The question

Hold the shipped full-data recipe fixed and change only the mining RNG seed.
At depth K of the ranking, do the top-K rules from different seeds cover the
same FY2024 error cases through different rule text (an ORDERING property), or
different error cases (a VOCABULARY property)? Depth-resolved at
K = 100, 200, 1,000, 20,000, and the full admitted pool.

Why it matters, either way:

- **Ordering fork.** If coverage converges by K = 20,000, the rule churn is
  cosmetic at depth and the deep pool is a stable object. That opens
  preference-based ordering: a state could promote rules by what it can
  actually catch and fix (the section 29 characterization fields: element
  groups, discovery mode, cause, timing) without losing reach, because the
  errors are reachable through many orderings.
- **Vocabulary fork.** If even the full admitted pools cover substantially
  different error cases across seeds, the miner is sampling, not exhausting,
  the reachable coverage, and there is real data-mining exploration to do
  (bigger or more diverse ensembles) before ordering questions matter.

## What varies, component by component (exactly one varies)

| component | setting | status |
|---|---|---|
| mining RNG seed (xgboost seed + ranger seed) | **VARIES**: seed A = the cached full-data mine; seeds B, C = fresh explicit seeds | the component under test |
| training data | FY2022-23, all 77,806 rows, NO split | fixed |
| frame | any-error | fixed (ledger: typed delivery retired, section 17) |
| strata | household size 1 / 2-3 / 4+ | fixed (section 11) |
| engine parameters | xgboost 1000 rounds, eta 0.02, subsample 0.20, depth 4; ranger 1000 trees, mtry 2 | fixed (sections 4, 13) |
| admission | BH at FDR 10% vs stratum base rate AND n >= 30, self-scored, as shipped | fixed (section 19) |
| ordering | one-sided 99% Wilson LCB of train precision, self-scored, as shipped | fixed (section 20) |
| evaluation | FY2024 error-case coverage; refill walk for the budget readout | fixed |

## Support, computed not estimated

There is no split. Every seed mines on the full national FY2022-23 frame:
**77,806 rows carrying 8,485 errors** (the same figures the 2026-08-05
pre-registration verified). Per-rule support is therefore identical in
distribution to the shipped pipeline's; the n >= 30 floor operates at its
design scale. The test year is **40,457 rows carrying 4,803 errors across 49
states**. The 2026-08-04 failure mode (halving support) cannot occur here by
construction.

## Design

**National pool only.** No state pools are mined in any arm. Three reasons:
the national pool is where the miner runs at its validated scale; state pools
(~1,500 public rows, 96 to 280 errors) sit in the ledger's hazard regime,
where seed variation is dominated by small-sample effects we already
understand, and mixing them in would make low overlap uninterpretable; and
the section 30 budget baseline this study compares against is national-only.
Caveat carried with the result: this measures the seed stability of the
shipped blend's national component, not of the exact blended list a state
receives; a state whose own rules enter its list in numbers (Arizona deploys
20 of its own, section 16) has a seed-noise component this study does not
measure. State-pool seed stability is a cheap follow-up if the ordering fork
wins.

Three seeds: A (the cached full-data mine, ~144.5k raw candidates), B and C
(fresh mines, explicit seeds recorded in the script). Three pairwise
comparisons. Each seed's pool goes through the shipped admission and ordering
untouched. For each seed s and depth K in {100, 200, 1,000, 20,000, full}:

- `E_s(K)` = the set of FY2024 error cases flagged by the union of the top-K
  rules on the pooled national LCB scale (strata interleaved, as the delivery
  builder sorts). Case-level, each error case counted once.
- Pairwise Jaccard of `E_s(K)` and `E_t(K)`; each seed's reach
  `|E_s(K)| / 4,803`; the three-seed intersection share.
- Rule-signature Jaccard at each K (descriptive contrast; section 30 found
  0.032 at the top 100 across partitions).
- Chance baseline at matched reach, computed as in section 30.

**Tie handling (bias guard).** Rules tied on the LCB at a depth boundary must
be broken by a seed-independent key (LCB, then train support, then canonical
rule text), never by pool insertion order, or a fixed K injects artificial
seed dependence at the cut.

**Budget readout (the deployment-anchored depth).** For the same 10 states as
section 30's run 2, build each seed's frozen list at the 5% and 10% budgets
and record pairwise Jaccard on errors caught. This is the direct contrast
with the partition numbers (0.325 / 0.435): partitions varied data AND seed;
this varies seed alone, so the gap between the two studies estimates how much
of the section 30 churn was data sampling.

**Determinism anchor.** Before any new number is trusted, re-score seed A
through the study's machinery and confirm it reproduces the cached era
artifacts it feeds (admitted count, top-of-ranking identity). A mismatch
stops the study.

**Dollar-weighted coverage** is recorded as a secondary descriptive alongside
case counts (both endpoints of every comparison, per the completeness
checklist).

## Pre-registered expectation and bars

**Expectation, written down now:** full-pool coverage will be nearly
identical across seeds, and the divergence will live at small K; that is, we
expect the ordering fork. If the data contradict this it is the more
important result.

**Primary bar, pairwise Jaccard on FY2024 error cases covered at K = 20,000**
(median of the three pairs), read with the full-pool Jaccard as ceiling:

- **>= 0.80**: coverage converged; the churn is ordering. The
  preference-ordering line of work opens.
- **< 0.60**: vocabularies reach different errors; the exploration fork wins.
- **Between 0.60 and 0.80**: extend to five seeds (D and E, two more mines),
  but not to re-read the same bar; a pairwise Jaccard is exact given the
  seeds, and each pool aggregates 2,000 trees, so if three pairs cluster
  inside the gap the true value is in the gap and more pairs will not move
  it. The five-seed read is the **union-accumulation curve**: distinct FY2024
  error cases covered by the union of the first 1, 2, ..., 5 full pools.
  Decision rule on the marginal gain of the fifth seed, as a share of the
  4,803 test errors:
  - **< 2%** new errors from seed five: reachable coverage is saturated;
    resolve toward the ordering fork, recorded with the intermediate overlap
    as its qualifier.
  - **>= 5%**: each mine samples a genuinely larger reachable set; resolve to
    the vocabulary fork, and the accumulation curve's remaining slope is the
    measure of how much coverage is still on the table.
  - Between 2% and 5%: record as partial with the full curve; no further
    seeds (the information per overnight mine is diminishing), and the next
    move is a design question, not more replication.
  If instead the three pairs are spread wide (range above 0.10), that
  dispersion is itself the finding, a seed lottery in reach, and the
  five-seed extension serves to estimate the spread.
  Section 30's unresolved bar taught us to commit the extension path in
  advance rather than leave the middle undecided.

**Descriptive, no bar:** the depth K* at which median pairwise Jaccard first
crosses 0.70 (the "safe reordering depth"); the budget-readout contrast with
section 30; the signature-vs-case-level gap.

## What the record already says (ledger rows quoted)

- "Seed-to-seed variation of the shipped full-data pipeline: **open** (never
  measured; partition variation is a different quantity)" (section 30 scope
  note). This study is that row.
- "Coverage-aware ordering / union of partitions: **open**; any mechanism
  collides with the deliberate removal of greedy nets." The ordering fork
  feeds this; nothing here builds a net.
- "Admission = BH FDR 10% AND n >= 30: **settled**" and "ordering z = 2.326:
  **settled** on two eras" (sections 19, 20). Both held fixed; nothing here
  re-opens them.
- **Hazard**: heavy scoring must go through `reduce_flags_for_rules()` (the
  29 GB box OOMs otherwise); mines checkpoint per seed and resume.
- Section 27: evaluation, not mining, is the expensive step; the slack-zero
  window certificate applies to the budget readout fills.
- Section 29's reliability numbers are the payoff mechanism on the ordering
  fork, not an input here.

## Cost and schedule

Seed A is cached (mine and, from the July rebuild era, its admitted set).
Seeds B and C each need one full national any-error mine (3 strata) plus
train scoring for admission and ordering, and FY2024 scoring to depth. The
mine-plus-score cost is estimated at a few hours per seed on this box and is
confirmed by the PDS with a timed dry run before launch; the two new seeds
are expected to fit one overnight run, sequential, checkpointed per seed.

## Explicitly out of scope

Whether preference-based reordering preserves precision (the next study, if
the ordering fork wins); shipping a union-of-seeds vocabulary; K-fold
ordering (dropped by the section 30 decision rule); any change to
`state_delivery_lists/`; any claim about state-internal data.

## Artifacts

`methods/seed_stability_v2.R` (runner `runners/run_seed_stability.R`), outputs
to `methods/seed_stability_v2/`. Written under the principal-data-scientist
skill and reviewed by a fresh senior-statistician before launch, per
`methods/known_constraints.md#routing`.
