# Reading notes for the exploratory percentile-runoff outputs

EXPLORATORY (2026-08-11): no bars, no winner, no ledger rows. The
design and its pre-stated reading limits:
`methods/design_note_state_pctl_runoff_2026-08-11.md` (with addendum).

- `pctl_runoff_readout.csv` - one row per state x arm x budget.
  `n_rules_deployed` counts rules that contributed at least one NEW case
  in the FY2024 walk (rules on the frozen list that added nothing are not
  counted); `n_rules_deployed_p` is the subset referencing at least one
  `_p` feature. `fill_gap_core` / `fill_gap_total` are the tolerated
  fresh-share refill shortfalls (design-note addendum); gaps of a few
  cases carry no reading. The any-error frame is the mining frame here,
  so frame-relative and any-error precision coincide.
- `pctl_runoff_paired_table.csv` - per-state paired deltas
  (benp - persize), both budgets.
- `pctl_runoff_p_condition_inventory.csv` - every `_p` condition in every
  deployed benp rule: variable, direction (op), threshold, rank, new
  cases at that rank. The op column answers whether the construction is
  used as a high-tail outlier detector or as a low-side / zero-absence
  encoding (zeros are pinned to percentile 0).
- `pctl_runoff_p_rule_catch.csv` - do `_p`-using rules earn their slots:
  flags, precision, and caught-dollar share for `_p`-rule-flagged vs
  other flagged cases, overlap stated.
- `pctl_runoff_p_variable_profile.csv` - per variable: tail shares
  (> p90 / > p99) of that variable among cases flagged by rules USING it,
  vs the caseload and vs error cases; the pinned-zero mass; and the tail
  shares among benp-only vs shared error catches.
- `pctl_runoff_incremental_catch.csv` - errors caught only by benp, only
  by persize, and by both, per state x budget - the sharpest form of the
  "what do percentile rules catch that per-size misses" question.

INTERPRETATION HAZARD (carry into any write-up verbatim): `_p` conditions
appear conjoined with non-percentile conditions, so "flagged by a rule
using `_p`" is rule-level attribution, not causal attribution - a case may
satisfy the rule chiefly through its other conditions. These files
characterize; they do not decompose.
