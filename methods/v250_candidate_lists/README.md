# v2.5.0 CANDIDATE delivery lists - STAGED, not shipped

One ranked rule list per state and review budget (5% / 10% of caseload),
built 2026-08-13 by `methods/v250_build_staged_lists_v2.R` on the
2026-08-12 rebuilt frame (the reconstruction fix). These are staged for
review; the shipped deliverable remains `state_delivery_lists/`
(v2.4.0) until promotion is decided. Recipe: per-size 19-feature
vocabulary, any-error mining, state + national pools blended on one
99%-LCB scale, fresh-share walk (f = 0.50), measurement-contaminated
rules dropped before the fill (see the mm_ columns). Illinois's state
pool is HELD (its lists are national-rules-only) pending a further
Illinois-specific reconstruction fix.

The one-year-ahead performance benchmark for this recipe is
`methods/v250_benchmark_2024/` (same recipe mined on FY2022-23, walked
on FY2024, paired against the shipped v2.4.0 scorecard).

## Column dictionary

**SCOPE matters on every column.** Three different populations appear on
one row: the pool's own training scale (state rows for state rules,
national rows for national rules), this state's caseload, and the full
49-state frame. Each column below names its scope.

| column | scope | meaning |
|---|---|---|
| `rank` | this list | position in the delivered ordering (core first, then buffer) |
| `role` | this list | `core` fills the review budget; `buffer` extends to 3x depth as substitutes |
| `rule` | - | the flagging conditions (canonical text; AND of threshold clauses) |
| `hh` | - | household-size stratum the rule was mined and applies in: 1 / 2-3 / 4+ |
| `pool` | - | `national` or `state` - which pool the rule came from |
| `engines` | - | which mining engine(s) generated it (xgboost, ranger) |
| `mined_frames` | - | mining frame (any_error for every rule in this build) |
| `n_flagged_train` | POOL'S OWN training rows, FY2022-24 | cases the rule flags where it was mined: the whole country for national rules, THIS STATE ONLY for state rules |
| `precision_train` | pool's own training rows | share of those flags that are error cases |
| `precision_train_lcb` | pool's own training rows | one-sided 99% Wilson lower bound of that precision - the ranking statistic |
| `dollars_per_flag_train` | pool's own training rows | error dollars per flagged case (error dollars = the file's recorded raw-vs-corrected benefit difference) |
| `mm_share_flags` | pool's own training rows | share of the rule's flags on reconstruction-failure cases ("mismatch rows": recorded benefit at/above max, reconstructed below). Audit column; rules >= 0.25 on either mm share were dropped before the fill |
| `mm_share_errors` | pool's own training rows | share of the rule's caught ERRORS on mismatch rows - how much of the measured precision leans on them |
| `mm_inflation` | pool's own training rows | mismatch errors / flags: the additive amount by which precision_train is inflated by mismatch rows |
| `n_flagged_state` | THIS STATE's caseload, FY2022-24 | cases the rule flags in this state (the fill denominator; for state rules equals n_flagged_train) |
| `n_new_at_rank` | this state's caseload | NEW cases this rule added at its turn in the delivered order (0-new rules are excluded at build) |
| `n_error_cases_national` | NATIONAL, all 49 states FY2022-24 | error cases nationwide matching the rule's conditions - the support behind the characterization shares below. NOT this state's count (a Michigan rule can read 596 here while flagging a few dozen cases in Michigan). Named `n_error_cases` in the full sheet |
| `element_groups_to_75` | national (those n_error_cases) | error-element groups covering 75% of the rule's error-case variances, with shares |
| `nature_groups_to_75` | national | how-the-error-happened groups covering 75%, with shares |
| `found_in_case_record` | national | share of variances the QC reviewer discovered from the case record itself (vs contact or a fresh match). QC's discovery, not proof of desk catchability |
| `share_overissuance` | national | of the rule's directional error cases, the share overissued |
| `timing_at_certification` | national | share of variances that arose at the agency's action |
| `cause_agency` | national | share coded agency-caused (client-caused is roughly the complement, less ~3pp of third-party/no-fault/other) |

The characterization scopes are deliberate (section 29): descriptive
shares need support to be readable, and state-level counts would make
them noise. Caution from the same record: the discovery / cause / timing
fields carry real state-to-state variation beyond sampling, so a
national share will be somewhat off for any one state; the element and
nature fields travel well. The FULL characterization sheet - every share
with its Wilson interval, ~150 columns - is
`rule_characterization_v250.csv` in this folder, joinable on (hh, rule).
`build_summary.csv` records per-list pool sizes, state-rule counts,
artifact-rule drops, and fill gaps.
