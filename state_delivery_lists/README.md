# Blended delivery lists by state (2022-24 public QC data)

One ranked rule list per state and review budget, built by
`INCL_build_blended_delivery_list_v2.R` (batch: `runners/` pattern; see
`custom_one_off/delivery_batch_run.log` provenance in the private workspace).
This is the default deployment deliverable described in the README and
`methods/modeling_findings.md` (sections 14-16).

## How each list is built

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
- Deployment is outcome-free: walk the list in `rank` order, activating
  rules while review capacity fits. No outcome data or modeling is needed
  to run it.

## Columns

| column | meaning |
|---|---|
| `rule` | the flag condition, in public QC variable vocabulary (see `Definitions for variables used.txt`) |
| `hh` | household-size stratum the rule applies to (`1`, `2-3`, `4+`, from `cert_HH_size_FS_n`) |
| `source` | which pool the rule came from: `national` or `state` |
| `n_flagged_train` | cases the rule flagged in its training data |
| `precision_train` | share of flagged training cases with any over-threshold payment error |
| `precision_train_lcb` | one-sided 99% Wilson lower confidence bound of that precision — the ranking statistic |
| `rank` | position in the walk order (1 = activate first) |
| `role` | `core` (fills the budget on the 2022-24 caseload) or `buffer` (substitutes, to 3x depth) |

## Caveats

- Precision columns are training-data numbers on 2022-24. On the time-shifted
  benchmark (mined 2022-23, tested on 2024), walked national/blended lists
  delivered median precision ~0.27-0.32 at these budgets against state base
  rates of 8-17%; expect deflation from the training numbers, not a match.
- The public files show only 43-81% of each state's error cases (ineligible
  determinations are excluded; see `methods/state_error_accounting/`). A
  state's own internal validation on FY25/26 files is the deciding test
  before relying on any list.
