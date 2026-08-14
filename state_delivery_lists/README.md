# Blended delivery lists by state (2022-24 public QC data)

One ranked rule list per state and review budget (5% and 10% of caseload).
This is the default deployment deliverable described in the README and
`methods/modeling_findings.md` (sections 14-16). The current lists are the
v2.5.0 build (2026-08-13, `methods/v250_build_staged_lists_v2.R`), mined
fresh on the reconstruction-fixed 2022-24 frame; the one-year-ahead
benchmark for this exact recipe is `methods/v250_benchmark_2024/`
(findings section 39).

## How each list is built

- Rules are mined per household-size stratum (1 / 2-3 / 4+) on the
  any-error target, from a 19-feature vocabulary: the base case features,
  per-household-size income amounts, and a state-level broad-based
  categorical eligibility (BBCE) regime flag (`bbce_state_i`).
- A candidate rule is admitted to a pool if a Benjamini-Hochberg test
  (false-discovery rate 10%) rejects "precision at or below the stratum
  base rate" and it flags at least 30 training cases.
- The state's own mined rule pool is merged into the national pool on one
  scale: every rule is ranked by the one-sided 99% Wilson lower confidence
  bound of its training precision (`precision_train_lcb`).
- Mining uses ALL public years 2022-24 (there is no held-out year in the
  delivery build; the recipe itself was validated on a train 2022-23 /
  test 2024 benchmark, findings section 39).
- Rules that key on a known benefit-reconstruction measurement artifact
  are dropped before the fill: any rule with at least 25% of its training
  flags or caught errors on reconstruction-failure cases is removed, and
  every remaining rule carries audit columns (`mm_share_flags`,
  `mm_share_errors`, `mm_inflation`) so the residue is visible. Re-walking
  every list with the remaining tagged rules also removed changes the
  median state's benchmark precision by 0.000 (findings section 38).
- The ranked pool is filled against the state's own 2022-24 caseload until
  the review budget (5% or 10% of caseload) is reached (those rules are
  `role = "core"`) and then to 3x that depth as substitutes
  (`role = "buffer"`).
- **The fill walk is overlap-aware and carries a fresh-share floor**
  (since v2.4.0). Reading down the pool in bound order, a rule enters the
  list only if it flags at least one case that no higher-ranked rule
  already flagged (`n_new_at_rank` > 0 at build time), AND at least half
  of its flagged cases are new work (fresh share
  f = n_new / n_flagged >= 0.50); rules below the floor are passed over
  and their slots refill from deeper ranks at unchanged consumed workload
  (two-era validation: `modeling_findings.md` sections 33-34). The order
  is still strictly the confidence bound; the walk never promotes a
  lower-bound rule for having new cases. `rank` is the position in the
  delivered walk order, not the rule's position in the underlying pool
  (which is much deeper; see section 27).
- Deployment is outcome-free: walk the list in `rank` order, activating
  rules while review capacity fits. No outcome data or modeling is needed
  to run it.

## File names: the statistic-goal pairing

The machinery that mines rules and computes their evidence is general
purpose; the RANKING STATISTIC and the GOAL METRIC it is judged by are a
module the user chooses (see the README's "Statistics and goal metrics"
section). The naming rule: the DEFAULT, validated pairing keeps the plain
filename; any other pairing carries its label:

```
blended_delivery_<State>_2022_2024_budget05.csv                      <- the default
blended_delivery_<State>_2022_2024_<statistic>_<goalmetric>_budget05.csv   <- anything else
```

An unlabeled file always means "the recommended list." The default pairing
is `lcb99_workloadfill`: rules ranked by the one-sided 99% Wilson lower
confidence bound of any-error precision, filled to a review workload of 5%
or 10% of the caseload. A different goal (for example, prioritizing error
dollars) is a different pairing and ships with its own label only after it
passes the same validation.

## National-only lists (`national_delivery_*`)

For the states where the one-year-ahead test shows the national pool
alone matching or beating the blend, the folder also carries

```
national_delivery_<State>_2022_2024_budget05.csv
```

built from the national pool only (every row has `pool = national`),
with the same columns, walk, artifact gates, and characterization as the
blended lists. Selection (2026-08-14): a state x budget cell gets a
national-only file when the three-arm evaluation (all three list types
mined on FY2022-23, frozen, walked on the state's FY2024 cases;
`methods/threearm_2024/threearm_results_2024.csv`, selection table
`methods/national_only_lists/selection_2024.csv`) shows national
precision at or above blended precision, ties included - where the
blend deployed no state rules the two lists coincide and the national
file is an explicit copy. That is 43 of 49 states at the 5% budget and
36 at 10%, so some states carry a national-only file at one budget
only. The selection rests on a single test year and one mining draw; a
state's own internal validation on newer data remains the deciding test
between its blended and national-only list.

## Columns

A rule's mining frame is provenance only; every rule is scored, filtered,
and ranked on the any-error target.

**Scope matters on every column.** Three different populations appear on
one row: the pool's own training rows (the whole country for national
rules, that state only for state rules), this state's caseload, and the
full 49-state frame. Each column below names its scope.

| column | scope | meaning |
|---|---|---|
| `rank` | this list | position in the delivered walk order (1 = activate first); only rules that added new cases at build time are listed, so ranks count kept rules, not the underlying pool |
| `role` | this list | `core` (fills the budget on the 2022-24 caseload) or `buffer` (substitutes, to 3x depth) |
| `rule` | - | the flag condition, in public QC variable vocabulary (see [the data dictionary](../DATA_DICTIONARY.md)); indicator variables render as `>= 1` (present) or `<= 0` (absent) |
| `hh` | - | household-size stratum the rule applies to (`1`, `2-3`, `4+`, from `cert_HH_size_FS_n`) |
| `pool` | - | which pool the rule came from: `national` or `state` |
| `engines` | - | which tree engine(s) produced the rule: `xgboost`, `ranger`, or `ranger+xgboost` |
| `mined_frames` | - | every mining frame that produced the rule (`any_error` for every rule in this build) |
| `n_flagged_train` | pool's own training rows, FY2022-24 | cases the rule flagged where it was mined: the whole country for national rules, THIS STATE ONLY for state rules |
| `precision_train` | pool's own training rows | share of flagged training cases with any over-threshold payment error |
| `precision_train_lcb` | pool's own training rows | one-sided 99% Wilson lower confidence bound of that precision; the ranking statistic |
| `dollars_per_flag_train` | pool's own training rows | error dollars per flagged case (error dollars = the file's recorded raw-vs-corrected benefit difference) |
| `mm_share_flags` | pool's own training rows | share of the rule's flags on reconstruction-failure cases ("mismatch rows": recorded benefit at or above the maximum, reconstructed below). Audit column; rules at or above 0.25 on either mm share were dropped before the fill |
| `mm_share_errors` | pool's own training rows | share of the rule's caught errors on mismatch rows; how much of the measured precision leans on them |
| `mm_inflation` | pool's own training rows | mismatch errors / flags: the additive amount by which `precision_train` is inflated by mismatch rows |
| `n_flagged_state` | this state's caseload, FY2022-24 | cases the rule flags in this state (the fill denominator; for state rules equals `n_flagged_train`) |
| `n_new_at_rank` | this state's caseload | of those, cases not already flagged by higher-ranked rules (walked in rank order) |
| `n_error_cases_national` | NATIONAL, all 49 states FY2022-24 | error cases nationwide matching the rule's conditions; the support behind the characterization shares below. NOT this state's count (a Michigan rule can read 596 here while flagging a few dozen cases in Michigan) |
| `element_groups_to_75` | national (those error cases) | error-element groups covering 75% of the rule's error-case variances, with shares |
| `nature_groups_to_75` | national | how-the-error-happened groups covering 75%, with shares |
| `found_in_case_record` | national | share of variances the QC reviewer discovered from the case record itself (vs contact or a fresh match). QC's discovery, not proof of desk catchability |
| `share_overissuance` | national | of the rule's directional error cases, the share overissued |
| `timing_at_certification` | national | share of variances that arose at the agency's action |
| `cause_agency` | national | share coded agency-caused (client-caused is roughly the complement, less ~3pp of third-party / no-fault / other) |

The characterization scopes are deliberate (findings section 29):
descriptive shares need support to be readable, and state-level counts
would make them noise. Caution from the same record: the discovery, cause,
and timing fields carry real state-to-state variation beyond sampling, so
a national share will be somewhat off for any one state; the element and
nature fields travel well. The FULL characterization sheet, every share
with its Wilson interval (~150 columns), is `rule_characterization.csv`
in this folder, joinable on (`hh`, `rule`); its support column is named
`n_error_cases`.

## Caveats

- Precision columns are training-data numbers on 2022-24. On the
  time-shifted benchmark (this recipe mined on 2022-23, walked on each
  state's 2024), the median state's delivered any-error precision is
  0.3182 at the 5% budget (2.75x the median base rate) and 0.2976 at 10%
  (2.49x); against the shipped v2.4.0 lists on the same paired test, the
  median state moves +0.0232 at 5% (mean +0.0124; 8 of 49 states worse
  than -0.05, 14 better than +0.05) and +0.0300 at 10% (mean +0.0227;
  3 worse, 15 better), with dollar recall also up at both budgets
  (findings section 39). Expect deflation from the training numbers, not
  a match.
- All quoted list-level precision is computed on the union of flagged
  cases: a case counts once no matter how many rules flag it, and an
  error caught by several rules counts once. Overlap between rules cannot
  inflate these numbers.
- The public files show a state only 43% (New Jersey) to 91% of its own
  error cases (ineligible determinations are excluded; see
  `methods/state_error_accounting/`). A state's own internal validation
  on FY25/26 files is the deciding test before relying on any list.
