# Findings ledger: every claim, its status, and the scope it was tested at

One row per claim we have tested, retired, or posed. This is the planning index:
read it whole before designing any experiment or pipeline change, then open the
cited sections for the evidence. The findings docs own every number; a number
quoted here carries its section citation and must match it. Statuses:

- **settled**: validated on held-out data; do not re-open without new evidence.
- **retired**: tested and refuted in writing; do not revisit without new evidence.
- **option**: real effect, not adopted as default; labeled alternative.
- **hazard**: an operational failure mode we have hit; the guard is stated.
- **open**: posed, not yet tested.
- **question open**: a study of it was invalid; the result says nothing about
  the question (never read these rows as refuted).

`§N` cites `modeling_findings.md` / `modeling_findings_detailed.md` section N.
Maintained by the `log-finding` skill: every new finding adds or updates its
row(s) here.

Provisional citation: §30 (national-only cross-fit) is drafted and awaiting
review (2026-08-05, draft outside the repo); rows citing it are provisional
until the section lands in the findings docs.

## Ordering and selection statistics

| Claim | Status | Scope tested | Source |
|---|---|---|---|
| Rank and filter on the one-sided Wilson LCB of train precision; never shortlist on raw train or holdout precision (raw 0.20 delivers ~0.10) | settled | two eras | §1, §20, §22 |
| z = 2.326 (99%) ordering stringency; no fixed z beat it on both eras (the 2024 hint for 2.576 failed replication on 2017-19) | settled | two eras, pre-registered | §20 |
| Shrinkage (empirical-Bayes posterior-mean) ranking beats the LCB | retired | two eras (0.259 vs 0.324 on 2024; 0.201 vs 0.219 on 2019) | §18 |
| Dollar-yield ranking: dollars per flag carries across years better than precision, but the budget gain missed its pre-set bar on the second era | option | two eras, pre-registered | §21 |
| Out-of-fold (cross-fitted) ordering of the national pool improves the deliverable | retired at public national scale; untested on a state's 40k-100k internal rows | 49 states, train FY2022-23, test FY2024, pre-registered (-0.0044 at 5%, +0.0000 at 10% vs a +0.010 bar) | §30 |
| Cross-fitting/K-fold designs: a 50/50 split is NOT required; K-fold mines on (K-1)/K of the data. Always compute support after any split (rows AND events per unit) before running | settled methodology | halving state pools to 48-140 errors invalidated the 2026-08-04 study | RESUME.md invalid designs; §30 |
| Structure-anchored dollar statistic (credit dollar size only as far as flagged cases' benefit levels justify) | open | none | §21 motivates |
| Rank-position-aware ordering reliability criterion (principled replacement for the fixed-z bound) | open | none | §20-22 motivate |
| Coverage-aware ordering / union of partitions vs a workload-matched single list | open; any mechanism collides with the deliberate removal of greedy nets | none | §30 partition dependence; handoff 2026-08-05 |
| Seed-to-seed variation of the shipped full-data pipeline | open; study pre-registered and approved 2026-08-05, in flight | none yet | §30 scope note; methods/preregistration_seed_stability_2026-08-05.md |

## Admission (which rules enter the pool)

| Claim | Status | Scope tested | Source |
|---|---|---|---|
| Admission = Benjamini-Hochberg at FDR 10% vs the stratum base rate AND n >= 30 flagged training cases; the two guards do different jobs and neither replaces the other | settled default (v2.3.0) | two eras at the 5% budget | §19 |
| Floorless admission (BH without the n >= 30 floor) | retired (0.335 fell to 0.284) | one era, 18 states | §19 |
| Tightening FDR 10% to 5% changes delivered lists (it removes mid-ranking rules; budgets deploy the top ~16-27) | retired: median within-state difference 0.000 | one era, 18 states, exploratory | §25 |
| Raising the support floor above 30 helps a bigger search | retired: precision falls monotonically (0.3345 to 0.2826 at floor 778); lowering to ~15 also loses | one era, 18 states, exploratory | §26 |

## States and deployment

| Claim | Status | Scope tested | Source |
|---|---|---|---|
| The blended frozen list (state + national rules on one 99%-LCB scale, core to budget, buffer to 3x, outcome-free walk) is the default deliverable | settled | 18 states future-year; 49-state scorecard | §15, §16 |
| The plain national ranking is the best deployable default; own-state mining is boom-or-bust and can land below the base rate | settled | 12 states, true future year | §14 |
| Per-state re-filtering or tuning beats the national ordering for the median state | retired on a true future year | 12 states | §9, §14 |
| Single-state mining at public scale (~1,500 rows) collapses without the n >= 30 floor; with it, rules deflate gently (~1/3) | hazard | Virginia 2026-07-06; §9 | §9; CLAUDE.md |
| Similar-state transfer pools beat national at the 10% budget | retired (did not survive the future-year test) | 12 states | §12, §14 |
| Blend blind spot: a national rule's bound says nothing about transfer to a given state; low-visibility states (below ~60% of errors visible in public data) should mine internally | settled caution | per-state visibility 43-91% | §10, §16 |
| Hybrid internal + national blend at a real state | open (documented, untested; blocked on an engagement) | none | §10, RESUME.md |
| Evaluate deployment at review budgets (5% / 10% of caseload), not filter floors | settled method | §12 onward | §12, §14 |
| Frozen lists under-fill their budget (median fill 0.855 at 5%); buffer rules close the gap; the scorecard scores core only | settled caution | 49 states, FY2024 | RESUME.md 2026-08-02 correction |

## Vocabulary, frames, strata, features

| Claim | Status | Scope tested | Source |
|---|---|---|---|
| The delivery vocabulary is the any-error frame; adding typed frames to the delivery pool lowers budget precision (three rescue attempts failed) | retired (typed delivery) | one era + 49-state same-design re-test (precision a wash, any-error ahead on dollars) | §17; RESUME.md 2026-08-02 |
| At fixed filter floors, pooling typed + any-error can only add reach (mechanical union growth); the gain does not survive a tight budget | settled | held-out year + replication | §3, §17 |
| Frame-relative precision understates deployed (any-error) precision ~2x; always compute and quote both | settled | all frames | §6 |
| Coarse household-size strata (1 / 2-3 / 4+) never lose; 5-way adds nothing at ~1.6x compute | settled | two studies + year-swap | §11 |
| elderly/disabled is a feature, not a stratum (the ensembles carve the caseload themselves) | settled | §8 | §8 |
| second_element_i must never be a mining feature (state reporting is inconsistent) | hazard | frame rebuild 2026-07-07 | CLAUDE.md |
| other_error is the largest error category and is learnable; states treat it as low-priority, so it is completeness, not a headline | settled | 2023 holdout | §7 |
| Engine pair xgboost + ranger, depth 4, mtry 2, eta 0.02; the engine is worth ~1 point; the leverage is strict filtering and any-error scoring | settled | grid + year-swap | §2, §4, §13 |
| subsample 0.20 is "as good as anything, not proven best" (the low-beats-high edge failed the year-swap) | settled | year-swap | §4, §13 |
| Mine big, filter stringently: a big pool at the 99% bound matches a small pool's operating point with a much longer usable list | settled | §5 + year-swap | §5, §13 |

## Data and munging

| Claim | Status | Scope tested | Source |
|---|---|---|---|
| reg_model_data.rds is saved by the munging script only; a hand-built frame silently dropped ~31% of errors for weeks | hazard | 2026-07-07 rebuild | §10; CLAUDE.md |
| FY2020/FY2021 are excluded by decision; the reconciliation filter is a validity guard and additive-only on the six kept years | settled | relax-and-measure study | §24 |
| Multi-element error cases are kept; deduction-field NAs are zero-filled, not dropped | settled | frame rebuild | §10 |
| Public data shows a state 43-91% of its own errors (ineligible cases are invisible) | settled | FY22-24, per state | §10 |
| rawben_rel_max just below 1 is a reconstruction artifact; 88 delivered rules take 76.7% of their flags from artifact rows (median 6.3% of a 5% list's cases) | settled diagnostic; the at_max_benefit feature fix is open and needs a full re-mine | FY2022-24 frame + all delivered lists | §28 |
| Nature codes 56, 57, 33, 58 exist only from FY2024; cross-year nature comparisons must account for the recode | settled | FY2022-24 | §29 |
| AGENCY 26 is not a fault code ("change not required to be reported or acted on"); 1.4% of error-case variances | settled | FY2024 tech doc + data | §29 |
| Rule characterization: group shares are reliable (0.72-0.94) and state-stable for WHAT fields; the modal element is uninformative and fragile and is not reported; profile distinctiveness falls with rule support | settled | 543 rules, FY2022-24, split-half | §29, §6 |

## Exclusion pipeline

| Claim | Status | Scope tested | Source |
|---|---|---|---|
| Exclusion rules: 95% clean-rate LCB, support floor 25, excluded pockets at most 1/5 of stratum base rate | settled settings | ONE holdout year only (2023); no multi-era or deployment test | §23 |

## Evaluation machinery

| Claim | Status | Scope tested | Source |
|---|---|---|---|
| Ranked-window evaluation with the slack-zero certificate reproduces the full-pool fill exactly at 2-10x lower cost; never cap the pool at a fixed rank as policy (median core alone reaches rank 969 at the 10% budget) | settled | 98 shipped fills + 70 research comparisons | §27 |
| A median state's 5% budget flags ~44 cases; precision on 44 cases carries a binomial SE of ~0.068. Read state-level precision moves against that | settled arithmetic | §30 | §30 |

## Open work not covered above

| Item | Status | Source |
|---|---|---|
| family_id labeled-substitutes column (design decided, not built) | open | RESUME.md roadmap |
| Characterization columns on delivery lists (built, staged for the next list build; MINOR bump is Eric's call) | open | RESUME.md roadmap; §29 |
| A1-F1 pipeline upgrade (rule_id, admit_bh helper, finder upgrades; de-OOM before any heavy regen) | open | RESUME.md next-session plan |
| Case-overlap pre-registered bar landed between its thresholds (0.325/0.435 vs bars 0.3/0.5); disposition undecided | open | §30 |
| Per-stratum outlier features (value above the within-stratum 99th percentile on shelter, deductions, income fields) as rule inputs; the issue reports 26% error among extreme-shelter HH-3 cases vs the ~11% base | open (Ben, issue #7, 2026-08-05); a vocabulary change needing a full re-mine, same class as the at_max_benefit feature, and a candidate to ride the same regen mine | GitHub #7 |
| Representing ineligible households in the modeling frame; the public file omits them entirely and only per-state/year counts exist (additional_data/snap_qc_exclusion_all_years.csv), no case records; the issue conjectures many of the largest errors are FSBEN = $0 cases | open discussion (Ben, issue #8, 2026-08-05); bears directly on the §10 visibility limit; no method proposed yet | GitHub #8; §10 |

## Process rules (always in force)

- Nothing enters the recommended workflow without held-out-year validation,
  ideally two eras; failures are retired in writing.
- Before any run that costs a night, the four-item design note goes to Eric:
  the question in one sentence; what varies with exactly one component varying;
  support after the split, computed; what this ledger and the findings already
  say, cited.
- An invalid design is never filed as refuted; record the scope, keep the
  question open.
- Study outputs stay in `methods/`; no writes to `state_delivery_lists/`, no
  CHANGELOG entry, no version bump without Eric's decision.
