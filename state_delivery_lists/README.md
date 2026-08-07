# Blended delivery lists by state (2022-24 public QC data)

One ranked rule list per state and review budget, built by
`INCL_build_blended_delivery_list_v2.R` (batch: `runners/` pattern; see
`custom_one_off/delivery_batch_run.log` provenance in the private workspace).
This is the default deployment deliverable described in the README and
`methods/modeling_findings.md` (sections 14-16).

## How each list is built

- A candidate rule is admitted to a pool if a Benjamini-Hochberg test
  (false-discovery rate 10%) rejects "precision at or below the stratum
  base rate" and it flags at least 30 training cases.
- The state's own mined rule pool is merged into the national pool on one
  scale: every rule is ranked by the one-sided 99% Wilson lower confidence
  bound of its training precision (`precision_train_lcb`).
- Mining uses ALL public years 2022-24 (there is no held-out year in the
  delivery build; the recipe itself was validated on a train 2022-23 /
  test 2024 benchmark, `methods/state_similarity_v2/transfer_benchmark_train2223_test24/`).
- The ranked pool is filled against the state's own 2022-24 caseload until
  the review budget (5% or 10% of caseload) is reached — those rules are
  `role = "core"` — and then to 3x that depth as substitutes
  (`role = "buffer"`).
- **The fill walk is overlap-aware, and since v2.4.0 it carries a
  fresh-share floor.** Reading down the pool in bound order, a rule enters
  the list only if it flags at least one case that no higher-ranked rule
  already flagged (`n_new_at_rank` > 0 at build time), AND at least half of
  its flagged cases are new work (fresh share f = n_new / n_flagged >= 0.50);
  rules below the floor are passed over and their slots refill from deeper
  ranks at unchanged consumed workload (two-era validation:
  `modeling_findings.md` sections 33-34). The order is still strictly the
  confidence bound — the walk never promotes a lower-bound rule for having
  new cases.
  `rank` is therefore the position in the delivered walk order, not the
  rule's position in the underlying pool (which is much deeper; see
  `methods/modeling_findings.md` section 27).
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
or 10% of the caseload — the pairing validated on the train-2022-23 /
test-2024 benchmark. A different goal (for example, prioritizing error
dollars) is a different pairing and ships with its own label only after it
passes the same validation.

## Columns

A rule's mining frame is provenance only — every rule is scored, filtered,
and ranked on the any-error target.

| column | meaning |
|---|---|
| `rule` | the flag condition, in public QC variable vocabulary (see [the data dictionary](../DATA_DICTIONARY.md)) |
| `hh` | household-size stratum the rule applies to (`1`, `2-3`, `4+`, from `cert_HH_size_FS_n`) |
| `pool` | which pool the rule came from: `national` or `state` |
| `engines` | which tree engine(s) produced the rule: `xgboost`, `ranger`, or `ranger+xgboost` |
| `mined_frames` | every mining frame that independently produced the rule (e.g. `other_error+any_error`) |
| `n_flagged_train` | cases the rule flagged in its training data (its own pool) |
| `precision_train` | share of flagged training cases with any over-threshold payment error |
| `precision_train_lcb` | one-sided 99% Wilson lower confidence bound of that precision — the ranking statistic |
| `n_flagged_state` | cases the rule flags on the state's own 2022-24 caseload |
| `n_new_at_rank` | of those, cases not already flagged by higher-ranked rules (walked in rank order) |
| `rank` | position in the delivered walk order (1 = activate first); only rules that added new cases at build time are listed, so ranks count kept rules, not the underlying pool |
| `role` | `core` (fills the budget on the 2022-24 caseload) or `buffer` (substitutes, to 3x depth) |

## Caveats

- Precision columns are training-data numbers on 2022-24. On the time-shifted
  benchmark (mined 2022-23, tested on 2024), the v2.4.0 fresh-share walk
  improves the median state's delivered precision by +0.012 at the 5% budget
  (mean +0.015) and +0.006 at 10% (mean +0.009), at review workload unchanged
  by construction and dollar recall essentially unchanged at both budgets;
  3 of 49 states move worse than -0.05 at the 5% budget
  (`modeling_findings.md` sections 33-34 and the section 34 addendum; set
  `SORT_WALK_USE_FRESH_SHARE <- FALSE` in the builder to restore the legacy
  walk). Expect deflation from the training numbers, not a match.
- All quoted list-level precision is computed on the union of flagged cases:
  a case counts once no matter how many rules flag it, and an error caught
  by several rules counts once. Overlap between rules cannot inflate these
  numbers.
- The public files show only 43-81% of each state's error cases (ineligible
  determinations are excluded; see `methods/state_error_accounting/`). A
  state's own internal validation on FY25/26 files is the deciding test
  before relying on any list.
