# Any-error blended delivery: 2024 holdout scorecard

What prices the delivered lists in `state_delivery_lists/`. Built 2026-08-02 by
`methods/build_anyerror_blended_delivery_v2.R` (runner:
`runners/run_anyerror_blended_delivery.R`), 49 states, both review budgets.

Two builds, one run:

- **BENCH** mines 2022-2023, fills each state's list against its 2022-23
  caseload, freezes it, and scores it on that state's 2024 cases. Nothing from
  2024 touches mining, admission, ranking, or fill.
- **FINAL** mines 2022-2024 and fills against the 2022-24 caseload. That is the
  list a state is handed; it has no holdout, and BENCH is what prices it.

Recipe unchanged from the shipped one: xgboost + ranger at depth 4 per
household-size stratum, admission by Benjamini-Hochberg at FDR 10% vs the
stratum base rate AND n >= 30, ordering by the one-sided 99% Wilson lower bound
of training precision, fill to the budget as core plus substitutes to 3x depth
as buffer.

## Files

| file | contents |
|---|---|
| `holdout_metrics.json` | the scorecard: run settings plus one record per (state, budget) |
| `holdout_metrics.jsonl` | the same records, one per line, appended as each state finished |
| `holdout_metrics.csv` | the same records as a flat table |
| `bench_list_<State>_budget{05,10}.csv` | the frozen 2022-23 list that was scored, core then buffer in rank order |
| `anyerror_vs_typed_2024.csv` | per-state head-to-head against the typed five-frame run |

Each record carries the components (flagged and total error dollars, weighted
and unweighted, case counts, weights, issuance) so any other rate can be
rebuilt without re-running.

## Two scoring bases per record

Every record reports the same list two ways, because they answer different
questions and only one of them is workload matched.

- **Frozen core** (fields with no suffix): the core list is filled against the
  FY2022-23 caseload, frozen, and scored on FY2024 with no refilling. This
  answers "what does last year's list do next year". It is why the median state
  carries only 0.86 of its budgeted workload at the 5% budget.
- **Refill** (fields suffixed `_refill`, added 2026-08-03 by
  `methods/add_refill_metrics_v2.R`): core AND buffer are walked against the
  FY2024 caseload in rank order, taking a rule when it adds unflagged cases and
  the running total still fits that year's cap. Workload then equals the budget
  by construction, so precision is comparable across states and against the
  admission auditions (modeling_findings.md sections 25 and 26). The refill
  reads the FY2024 caseload but never FY2024 outcomes, so it is outcome free;
  what it assumes is that the state re-walks the delivered list against its
  current pile, which is what the buffer rules are for.

The refill fields are `n_rules_deployed_refill`, `n_rules_available_refill`,
`n_cases_flagged_refill`, `flagged_share_of_caseload_refill`,
`n_errors_flagged_refill`, `precision_refill`,
`per_reduction_pts_unweighted_refill`, `per_reduction_pts_weighted_refill`, and
the two flagged-dollar components.

49-state medians, the two bases side by side:

| | 5% frozen | 5% refill | 10% frozen | 10% refill |
|---|---|---|---|---|
| workload (share of caseload) | 0.043 | 0.049 | 0.093 | 0.099 |
| precision | 0.314 | 0.286 | 0.275 | 0.267 |
| lift over base rate | 2.58x | 2.59x | 2.45x | 2.38x |
| PER reduction, unweighted | 12.0% | 14.3% | 24.5% | 25.2% |
| rules deployed | 33 | 19 | 62 | 28 |

Refilling buys dollars with the review capacity the frozen list leaves unused:
2.3 more points of error dollars at the 5% budget for 0.6 points more workload,
at 0.028 lower precision. Lift is unchanged at the 5% budget (2.58x against
2.59x). No state exhausted its buffer in either budget (rules deployed 19 of 94
available at 5%, 28 of 192 at 10%), so the 3x buffer depth is not the binding
constraint on reach.

## 49-state medians

| | 5% budget | 10% budget |
|---|---|---|
| holdout precision | 0.314 | 0.275 |
| lift over base rate | 2.58x | 2.45x |
| PER reduction, unweighted | 12.0% | 24.5% |
| PER reduction, weighted | 11.2% | 24.4% |
| core rules | 33 | 62 |
| share of budget used by core | 0.86 | 0.93 |

One state at 5% and none at 10% fail to beat their own base rate.

## Reading the fill ratio

`flagged_share_of_caseload / budget` is the share of the review budget the
frozen CORE list used on the holdout year. It is below 1 for 27 of 49 states at
the 5% budget: a list frozen on 2022-23 does not flag exactly 5% of a different
year's caseload, and the buffer rules exist for the state to make up the
difference. The scorecard scores core only, so these numbers are the
conservative reading.

This is a different measurement from the `workload` column in
`methods/state_similarity_v2/transfer_benchmark_train2223_test24/fdr_admission_audition.csv`.
That study re-walks core AND buffer against the 2024 caseload and stops at the
2024 cap, so its workload equals the budget by construction (0.048-0.100 in all
36 cells). The two are not comparable, and neither is wrong: this file answers
"what does a frozen core list do next year", the audition answers "what does a
state that fills its budget from the delivered list get".
