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

## Ordering and selection statistics

| Claim | Status | Scope tested | Source |
|---|---|---|---|
| Rank and filter on the one-sided Wilson LCB of train precision; never shortlist on raw train or holdout precision (raw 0.20 delivers ~0.10) | settled | two eras | §1, §20, §22 |
| z = 2.326 (99%) ordering stringency; no fixed z beat it on both eras (the 2024 hint for 2.576 failed replication on 2017-19) | settled | two eras, pre-registered | §20 |
| Shrinkage (empirical-Bayes posterior-mean) ranking beats the LCB | retired | two eras (0.259 vs 0.324 on 2024; 0.201 vs 0.219 on 2019) | §18 |
| Dollar-yield ranking: dollars per flag carries across years better than precision, but the budget gain missed its pre-set bar on the second era | option | two eras, pre-registered | §21 |
| Narrow two-sided dollar intervals (rel width < 5%) are the fragile tail: at matched train n 30-300, median held-out decay -0.04 to -0.07 vs -0.02 to -0.03 for other rules, and 8-36% flag < 10 held-out cases (reach collapse, invisible to precision-based selection); the 99% LCB median margin stays non-negative in every width bucket, so the LCB is not miscalibrated for them | settled (diagnostic) | two eras (22-23 to 24; 17-18 to 19), reviewed design, post-hoc buckets fixed pre-run | §40 |
| Delivery-time width floor (5% relative, dollar variables only, ratio-boundary intervals exempt) is removal-invariant and SHIPPED as workbook delivery hygiene: 49-state re-walk, median paired change +0.0000 at both budgets for precision and dollar recall, zero states harmed, zero fill gaps, unfiltered arm reproduced the committed benchmark 98/98 cells | settled (shipped in rule_selection.py, 2026-08-21) | one era (walked FY2022-23 pools, scored FY2024), 49 states, pre-registered bar | §40 addendum |
| Out-of-fold (cross-fitted) ordering of the national pool improves the deliverable | retired at public national scale; untested on a state's 40k-100k internal rows | 49 states, train FY2022-23, test FY2024, pre-registered (-0.0044 at 5%, +0.0000 at 10% vs a +0.010 bar) | §30 |
| Cross-fitting/K-fold designs: a 50/50 split is NOT required; K-fold mines on (K-1)/K of the data. Always compute support after any split (rows AND events per unit) before running | settled methodology | halving state pools to 48-140 errors invalidated the 2026-08-04 study | RESUME.md invalid designs; §30 |
| Structure-anchored dollar statistic (credit dollar size only as far as flagged cases' benefit levels justify) | open | none | §21 motivates |
| Rank-position-aware ordering reliability criterion (principled replacement for the fixed-z bound) | open | none | §20-22 motivate |
| Coverage-aware ordering / union of partitions vs a workload-matched single list | open; any mechanism collides with the deliberate removal of greedy nets | none | §30 partition dependence; handoff 2026-08-05 |
| Marginal-quality-aware selection (reordering the walk on marginal precision) | retired at public-data scale: deployable reordering gains +0.000 (5%) / +0.011 (10%) vs an unachievable oracle +0.175; the median marginal slice is 1-2 cases, so no marginal statistic is estimable and the n >= 30 discipline cannot apply to slices. The residual adverse selection it targeted costs 3-4pp capacity-weighted and is priced into all quoted numbers. Open at internal-data scale only | one era (FY2022-23 build, FY2024 score), 49 states | §32 |
| Characterization-profile distance (and spectral, NB-divergence, signature, consensus distances) as predictors of marginal quality | resolved NO SIGNAL: all five failed the pre-stated f < 0.99 bar; their apparent signal was the fresh-share structure, not incremental information. Profiles remain the state-facing preference vocabulary (§29), with no recall or precision claim | one era, 49 states, six-instrument pre-registered race | §33 |
| Fresh-share floor (two-pass walk on core and buffer): TWO-ERA VALIDATED (era 1 blended +0.0118 vs +0.010 bar; era 2 national-only +0.0070 vs +0.005 bar, 47 states; two-era pooled +0.0100; dollar guard 0.0000 everywhere at 0.50; bridge showed the effect indifferent to pool composition, +0.0118 national-only) | SHIPPED in v2.4.0 at threshold 0.50 (the two-era confirmatory point), behind SORT_WALK_USE_FRESH_SHARE / SORT_WALK_MIN_FRESH_SHARE (walk2 verbatim to floor-0 consumed targets, identity 12/12). The fine-grid median rule's 0.60 was adjudicated out by the mandatory companions: means never materially favored it, and it doubled the harmed tail (worse than -0.05) on all six harness-budget readouts | two eras, pre-registered at every stage, one shot each; adjudication 2026-08-07 | §33, §34 + addendum |
| Depth-indexed fresh-share threshold (e.g. 0.60 to the 5%-budget depth, 0.50 beyond) | frozen pre-registered candidate: coherent in theory (opportunity cost of a skipped slot grows with depth) and the tranche decomposition supports decline on era 1 but NOT era 2, so calibrating today would hard-code era 1's profile (the §20 failed-hint shape). Awaits a genuinely unread test bed: the FY2025 public file or a state's internal data | tranche decomposition on all three harnesses | §34 addendum |
| The dilution hypothesis (state-rule slots dilute the floor's benefit) | refuted as stated (bridge +0.011781 vs the >= +0.0118 rule); substantively: no dilution and no amplification, the effect is a property of the walk. Carries the dedup caveat | era 1, 49 states, national-only bridge | §34 |
| Seed-to-seed variation of the shipped pipeline: deep coverage is seed-stable (pairwise error-case Jaccard 0.959-0.965 at depth 20,000, every pool covers all 4,803 FY2024 errors, one mine saturates reach) but budget-depth lists are not (errors-caught overlap 0.531 at 5%, 0.666 at 10%); the instability is ordering, not vocabulary | settled 2026-08-05, pre-registered bar cleared by every pair | one era (FY2022-23 to FY2024), national pools only, no dedup machinery | §31 |
| Preference-based reordering of the deep pool (state promotes rules via §29 characterization fields): does it preserve precision? | open; unlocked by §31 (coverage is seed-stable at depth, the necessary condition) | none | §31; §29 |

## Admission (which rules enter the pool)

| Claim | Status | Scope tested | Source |
|---|---|---|---|
| Admission = Benjamini-Hochberg at FDR 10% vs the stratum base rate AND n >= 30 flagged training cases; the two guards do different jobs and neither replaces the other | settled default (v2.3.0) | two eras at the 5% budget | §19 |
| The BH call is ONE joint pass across the frame's candidate rules with per-stratum base rates inside the p-values; running BH per stratum is a different multiplicity correction that admits a different set (smoke: any-error pool 393 vs 436 rules) | hazard, caught in review before the 2026-08-09 state re-mine launched (that run's results were later deleted; the catch stands on its own) | smoke-measured, 2026-08-09 | methods/state_remine_review_2026-08-09.md |
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
| The v2.5.0 package (corrected frame, per-size 19-var vocabulary, state + national blend, fresh-share walk, artifact gates) beats the shipped v2.4.0 lists a year ahead: paired median +0.0232 at 5% (0.3182 vs 0.2861), +0.0300 at 10% (0.2976 vs 0.2671), dollars up at both budgets. Package-level comparison; the walk alone is ~+0.0118 of it (§34) | settled (the shipped lists since 2026-08-14) | one era (mined FY2022-23, walked FY2024), 49 states, single seed | §39 |
| The correctness package (bbce swap + indicator canonicalization + Illinois fix) is performance-neutral: paired run-to-run median +0.0000 at both budgets | settled (its value is correctness, not performance) | two full cycles, same recipe, one era | §39 |
| Typed-frame mining at state scale, pooled across household sizes (support preservation; findings 17's typed retirement is national-scope) | design argument from the remine proposal; untested for delivered performance. The 2026-08-10 state pools built on it were DELETED 2026-08-11 (their vocabulary runoff used the wrong percentile construction); state pools will be rebuilt after the new §37 exploratory study | design argument only | remine proposal |

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
| Feature vectors must be verified against frame column names before mining: `prep_features()` drops unknown names silently (the finder's three `raw*_by_hh_size` names never existed in the frame, so every v2 mine has used 16 features, not the listed 19) | hazard | discovered 2026-08-08 (Gate-1 session); guarded by `assert_features_present()` since 2026-08-13 | §35 |
| Case-level `cat_elig` must not be a mining feature: the FY2024 public file recoded it (code 1: 32,502 to 11,033 across FY2023-24; code 2: 1,972 to 23,194; code 3 FY2024-only), so rules on it read the data era, not the case | hazard | FY2022-24 frame | §39 |
| `bbce_state_i` (state-year share of cat_elig >= 1 reaching 0.5) is the regime-level replacement: 41 BBCE / 8 non-BBCE states, zero flips FY2022-24, 98/98 state-years agree with the USDA options file; 127 of 2,824 delivered rules use it | settled (v2.5.0 vocabulary) | FY2022-24; cross-check vs state-options file | §39 |
| The v2.5.0 delivery vocabulary is the per-size 19-feature set: 15 base features (16 minus cat_elig) + bbce_state_i + earned/unearned/gross_by_hh_size | settled by the v2.5.0 promotion (package-level benchmark, §39); the exploratory inputs behind the choice are §§35-37 | one era, single seed, package comparison | §39 |

The 2026-08-08/09 vocabulary comparisons (26-feature package, shelter
percentile, per-size vs frozen-percentile representation; "frozen" =
percentile cutoffs fit on the train years only and applied unchanged to
the test year, per §35 - not the pooled-years as-built `_p` columns) are
EXPLORATORY and deliberately carry no ledger rows: one era, ten evaluation
states.
They informed the v2.5.0 vocabulary decision (made 2026-08-13; see the
Vocabulary section and §39) and are recorded with full numbers and limits
in §§35-36; do not cite them as established findings. The 2026-08-10 state-scale runoff and its pools were
DELETED 2026-08-11: the percentile arm used the frozen construction, not
the pooled-years design, so it tested the wrong thing.

## Data and munging

| Claim | Status | Scope tested | Source |
|---|---|---|---|
| reg_model_data.rds is saved by the munging script only (one code path writes the frame); results predating the 2026-07-07 rebuild were mined on the single-element frame, ~31% of errors excluded | hazard | 2026-07-07 rebuild | §10; CLAUDE.md |
| reg_model_data.rds is the source of truth; the CSV export is lossy at 15 significant digits and pandas' default parser lands 1 ULP low, which flipped rule flags on threshold-straddling cases (four §29 artifact flag counts were off by one; corrected 2026-08-06). CSV consumers must parse round-trip or read the rds | hazard | full-universe cross-evaluator check, 543 rules, 2026-08-06 | methods/known_constraints.md#munging; ruling of 2026-08-06 |
| FY2020/FY2021 are excluded by decision; the reconciliation filter is a validity guard and additive-only on the six kept years | settled | relax-and-measure study | §24 |
| Multi-element error cases are kept; deduction-field NAs are zero-filled, not dropped | settled | frame rebuild | §10 |
| Public data shows a state 43-91% of its own errors (ineligible cases are invisible) | settled | FY22-24, per state | §10 |
| rawben_rel_max just below 1 is a reconstruction artifact; 88 delivered rules take 76.7% of their flags from artifact rows (median 6.3% of a 5% list's cases) | settled diagnostic; the 2026-08-08 rebuild's $0-tolerance recreation removed most of the band (in-band errors 537 to 227; strict-band share of flags 2-3% in both §35 arms at benchmark-level numbers, so performance no longer runs through the band itself). NOT established: freedom from artifact CORRELATES (near-boundary rel_max shapes still top the rankings), and pre-vs-post-fix level comparisons are unpaired (different frame and panel). Superseded in part by the source repair, §38 | FY2022-24 frame + all delivered lists; rebuilt-frame arms §35 | §28, §35 |
| The reconstruction defect was repaired at the source (2026-08-12/13): mismatch rows (recorded benefit at/above max, reconstructed below) 4,011 to 571 on FY2022-24; clean-case within-$1 agreement 95.6% to 97.9%; Illinois offset family 477 to 11. Error flags and row counts identical across the fix | settled | current frame vs archived pre-fix frame, FY2022-24 | §38 |
| The residual 571 mismatch rows run error rate 0.750 vs the frame's 0.114 - still a magnet. Delivery builds must keep the artifact gates on (tag at >= 0.25 mm-share of flags or errors, drop before the fill, head gates, removal-invariance re-walk) until the residual is gone. v2.5.0 record: 0.03% of national rules flag-tagged, zero tagged in any top 10, re-walk median change 0.000 | hazard, guarded | v2.5.0 build + benchmark, 49 states | §38 |
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
| v2.5.0 re-mine vocabulary: the per-size candidate (16 + gross/earned/unearned per size) vs alternatives | DECIDED 2026-08-13: the per-size 19-feature set with bbce_state_i shipped in v2.5.0 (see the Vocabulary section row and §39). The exploratory inputs were §§35-37. Still deferred (2026-08-11, not important enough now): the 49-state walk readout of the §36 frozen-percentile contrast from cache | §35-§37, §39; features.R |
| family_id labeled-substitutes column (design decided, not built) | open | RESUME.md roadmap |
| Characterization columns on delivery lists | SHIPPED in v2.5.0 (7 curated columns on every list, full sheet alongside as `state_delivery_lists/rule_characterization.csv`) | §29, §39; state_delivery_lists/README.md |
| A1-F1 pipeline upgrade (rule_id, admit_bh helper, finder upgrades; de-OOM before any heavy regen) | open | RESUME.md next-session plan |
| Case-overlap pre-registered bar landed between its thresholds (0.325/0.435 vs bars 0.3/0.5) | resolved by §31's seed-only decomposition: the reachable error set is stable, budget-depth lists are not, under any single draw | §30, §31 |
| Per-stratum outlier features (value above the within-stratum 99th percentile on shelter, deductions, income fields) as rule inputs; the issue reports 26% error among extreme-shelter HH-3 cases vs the ~11% base | the continuous-percentile route is now measured: four of the five variables are expressible by the package percentiles, which failed do-no-harm (§35), and the fifth (shelter) failed its positive bar despite passing its standalone pre-screen (§36). The issue-7 BINARY outlier-indicator construction with its own pre-screen is a different construction and remains open (issue #7); the frozen train-only percentile plumbing (§35) is its validated cutoff template | GitHub #7; §35, §36 |
| Representing ineligible households in the modeling frame; the public file omits them entirely and only per-state/year counts exist (additional_data/snap_qc_exclusion_all_years.csv), no case records; the issue conjectures many of the largest errors are FSBEN = $0 cases | open discussion (issue #8, 2026-08-05); bears directly on the §10 visibility limit; no method proposed yet | GitHub #8; §10 |

## Process rules (always in force)

- Nothing enters the recommended workflow without held-out-year validation,
  ideally two eras; failures are retired in writing.
- Before any run that costs a night, the four-item design note goes to the project lead:
  the question in one sentence; what varies with exactly one component varying;
  support after the split, computed; what this ledger and the findings already
  say, cited.
- An invalid design is never filed as refuted; record the scope, keep the
  question open.
- Study outputs stay in `methods/`; no writes to `state_delivery_lists/`, no
  CHANGELOG entry, no version bump without a project-lead decision.
- Shipping readouts carry mandatory companions (2026-08-07): the
  within-state mean and the harmed-tail count (paired change worse than
  -0.05) beside the decision median; a median win contradicted by both
  companions does not ship.
- Attribution readouts additionally carry per-state same-sign paired-cell
  counts and per-arm seed spread (2026-08-09, from §35's results review:
  the sign counts catch redistribution that the median/mean/harmed-tail
  trio missed, and a vocabulary that widens seed instability is a cost
  even at equal precision).
- Formality is proportional to the question (2026-08-11, from §35).
  Pre-registration, bars, and verdict language are for research claims
  headed for the recommended workflow. Feature-set membership and similar
  pipeline choices that end in a judgment call are technical explorations:
  measure, report the companions, and decide - do not bind them to
  adoption bars that later have to be reasoned around. Before designing
  any study, ask whether the answer would change a decision; if not, it
  is not worth a study.
- The ledger carries only established claims someone can count on
  (2026-08-11, from §36). Exploratory results - one era, small
  evaluation panels, single-seed, or otherwise half-baked - stay in the
  findings docs marked EXPLORATORY and get no ledger row and no GUIDANCE
  point. If an exploratory result matters to a pending decision, it is
  referenced from an Open-work row, not promoted to a claim.
