# Do delivered rules key on a benefit-reconstruction artifact near the maximum benefit?

Diagnostic for issue #1. Measured 2026-08-04 by `methods/at_max_benefit_diagnostic.py`
(entry point `runners/run_at_max_benefit_diagnostic.py`). Nothing here changes a
feature definition or a rule; it reads `state_delivery_lists/*.csv` and the
modelling frame and reports.

## The question

`rawben_rel_max` is the reconstructed benefit divided by the maximum benefit for
the household size. In state data a household at the maximum would carry exactly
1. In our reconstruction some of them fall a little short. A rule whose clauses
confine the ratio to just below 1 would then flag those households here and flag
nothing in a state's own file. Raised on the Alabama 5% list (issue #1).

## Terms, used the same way throughout

| term | definition |
|---|---|
| truly at max | `rawben == benmax` |
| exact-1 | `rawben_rel_max == 1` |
| artifact row | truly at max **and** `rawben_rel_max < 1` — the mis-recreated case |
| suspect band | `[0.987, 1)` |
| excludes exact-1 | the rule's clauses bound `rawben_rel_max` strictly below 1, so it can never flag a household whose ratio is exactly 1 |
| band-confined | excludes exact-1 **and** the implied lower bound is at least 0.987, so among at-max households the rule can only match artifact rows |
| unc-capped | `unc_rawben_rel_max` bounded above by something below 1 |
| artifact-dependent | **measured, not read off the text**: at least half of what the rule flags in the frame is an artifact row |

Rules are conjunctions of numeric comparisons, so the clauses on one feature
imply an interval; the labels above come from that interval.

## The frame (FY2022-24, 118,263 rows — the frame the mining runs report)

| quantity | value |
|---|---|
| rows with `rawben_rel_max` exactly 1 | 37.02% |
| rows truly at max | 37.37% |
| artifact rows | 1,724 (1.46% of rows) |
| of truly-at-max households, share on exact-1 | 96.06% |
| of truly-at-max households, share in the suspect band | 2.39% |
| of truly-at-max households, share with `unc_rawben_rel_max > 1` | 95.72% |

The last line is why `unc_rawben_rel_max <= 0.997` acts as an at-max *exclusion*
rather than an at-max selector, which is the mechanism in the issue's first example.

## How much of the delivered vocabulary is exposed

Across the 98 lists there are 19,316 rule instances (2,028 distinct
stratum-and-rule pairs once the repeats across states and budgets are removed).

| class | rule instances | share of 19,316 |
|---|---|---|
| mention `rel_max` at all | 15,398 | 79.7% |
| exclude exact-1 | 7,748 | 40.1% |
| unc-capped | 3,155 | 16.3% |
| band-confined | 625 | 3.2% |
| **artifact-dependent (measured)** | **1,134** | **5.9%** |

Text-based exposure is a bad guide on its own. Of the 2,028 distinct exposed
rules, 1,940 draw only 1.35% of their flags from artifact rows — they bound the
ratio below 1 somewhere far from the band and are not touching this at all. The
problem is concentrated in **88 distinct rules** that draw **76.7%** of their
flags (17,765 of 23,160) from artifact rows. Every one of the 625 band-confined
instances is in that group.

The 88 are not low-value rules that could be dropped without cost. On the frame
they run at precision 0.3612 (8,366 errors on 23,160 flags) against 0.2339 for
the other 1,940 exposed rules. They earned their rank on flags that would not
exist in a state's own data.

## What reaches a delivered list

`n_new_at_rank` is the marginal new cases a rule contributed at its rank in the
walk, so summing it over a list does partition the list's cases.

| budget | median share of delivered cases from artifact-dependent rules | 90th pct | max |
|---|---|---|---|
| 5% | 6.3% | 11.0% | 16.3% (Massachusetts) |
| 10% | 4.1% | 8.2% | 9.0% (Massachusetts) |

Most exposed at 5%: Massachusetts 16.3%, Vermont 14.7%, Pennsylvania 14.3%,
Rhode Island 13.0%, Michigan 11.4%. Of the 1,134 artifact-dependent instances,
724 sit in the core and 410 in the buffer.

**Read that as an upper bound on the damage, not an estimate of it.** It is the
share of delivered cases contributed by rules that mostly flag artifact rows. It
is not a claim that those cases vanish in state data: whether a state's own
reconstruction lands at exactly 1 is a question about the state's file, and this
repo cannot answer it.

## The three examples from issue #1

All three behave as he described. Frame counts are FY2022-24 within the rule's
own household-size stratum.

**1.** `elderly_disabled_i > 0.500 & rawben_rel_max > 0.993 & total_deductions_by_hh_size > 348.000 & unc_rawben_rel_max <= 0.997` (stratum 2-3, in 28 list rows across 14 states)

Flags 54 cases, 35 of them errors. **45 of the 54 (83%) are artifact rows.** It
does not bound `rawben_rel_max` above at all — the at-max exclusion comes
entirely from the `unc_rawben_rel_max <= 0.997` clause, exactly as the issue read it.

**2.** `rawben_rel_max >= 0.987 & rawben_rel_max < 0.997 & shelter_expenses_by_hh_size >= 850.000 & utilities < 576.000` (stratum 1, in 67 list rows)

Flags 353 cases, 165 errors, precision 0.467. **277 of the 353 (78%) are
artifact rows.** Band-confined.

**3.** `rawben_rel_max >= 0.987 & rawben_rel_max < 0.991 & total_deductions_by_hh_size >= 276.000 & utilities < 578.000` (stratum 1, in 63 list rows)

Flags 382 cases, 177 errors, precision 0.463. **341 of the 382 (89%) are
artifact rows.** Band-confined.

## What this does and does not settle

Settled: the exposure is real, it is narrow (5.9% of rule instances), the rules
carrying it are above-average performers on our frame, and the delivered
footprint is a median 6.3% of cases at the 5% budget.

Not settled, and out of scope tonight: whether an `at_max_benefit` feature fixes
it. Adding a feature changes the mining vocabulary and needs a full re-mine.
Whether smoothing `unc_rawben_rel_max` to exactly 1 is the right repair is also
open — it is a change to the reconstruction, not to the pipeline.

## Files

| file | contents |
|---|---|
| `rule_classification.csv` | one row per delivered rule instance (19,316) with its implied interval, text labels and measured artifact share |
| `affected_rule_eval.csv` | one row per distinct exposed stratum-and-rule pair (2,028) with frame flags, errors, artifact and at-max counts |
| `delivered_footprint.csv` | per state and budget: cases delivered, cases from artifact-dependent rules, share |
| `ben_examples.csv` | the three rules from the issue |
| `summary.json` | headline counts (the console output also lands in `run.log`, which is gitignored) |

A case can trip several rules, so per-rule flag counts sum to more than the
number of distinct cases. The footprint table is the exception: it uses
`n_new_at_rank`, which is marginal by construction.
