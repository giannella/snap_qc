# Per-rule characterization: what each delivered rule finds

Built 2026-08-04 by `methods/rule_error_profiles.py` (entry point
`runners/run_rule_error_profiles.py`). Mines nothing and changes no delivered list.

## What this is for

A state deciding whether a rule is worth using as a flagging criterion needs to
know what that rule tends to surface, so it can weigh that against what it can
actually catch and fix. This produces descriptive fields for each rule and stops
there. It does not sort rules into suitable and unsuitable, and it reports no
conjunctions such as "agency-caused AND catchable at the desk"; combining the
fields is the state's judgement, not ours.

`rule_characterization.csv` is the sheet: one row per (stratum, rule), joinable
onto any state's delivery list. Every share carries a Wilson interval, because a
share computed on 20 variances and one computed on 400 are different objects.

## Where the codes come from

All definitions are read from **`additional_data/FY-2024-Tech-Doc.pdf`** and the
element/nature nesting in **`additional_data/FNS380-1WithInstructions.pdf`**.
Nothing is inferred from the data, and the `.sav` files carry no value labels for
these variables.

The FY2023 tech doc is superseded and should not be used: it defines neither
`AGENCY` 22-26 nor `NATURE` 33, 56, 57, 58, all of which appear in FY2024 data.
Two consequences that changed the results:

- **`AGENCY` 26 is not a fault code.** It is "change was not required to be
  reported by the client or acted upon by the State." On error-case variances it
  is 1.4%.
- **`NATURE` 56 and 57** ("incorrect deduction amount included, budgeted too
  much / too little") are new in FY2024. See the era-drift section.

Element groups follow the munging script's own reconstruction groups
(`1_data_munging...`, lines 603-627) so the characterization and the modelling
frame agree on what an earned-income error is. Earned income deductions (321) sit
with earned income.

Nature groups are element-independent. FNS-380-1 nests natures under elements only
to say which are *permitted* where; a nature code carries the same meaning
wherever it appears, verified across every element page. The grouping therefore
describes **how the error happened** rather than what it was about.

| nature group | codes |
|---|---|
| wrong amount, known item | 38, 44, 56, 57, 54 |
| wrong include/exclude decision | 52, 53, 37, 58, 32, 33, 24, 30, 51 |
| unreported source of income | 35 |
| household composition | 6, 7, 12-16, 200, 201 |
| change in circumstances | 39, 40, 41, 64, 65 |
| method or computation | 36, 42, 43, 45, 46, 75, 79, 80, 98, 123 |
| reporting system or process | 77, 97, 120, 124, 301-314 |
| limits and thresholds | 20, 28, 29 |
| child support handling | 111, 112, 127 |
| other | 99 |

Every variance maps; there is no residual bucket.

## The flagged set, and coverage

The cases characterized are the ones the lists actually pull: the refill walk from
`methods/add_refill_metrics_v2.R` against the FY2024 caseload. **All 98
state-and-budget combinations reproduce
`methods/anyerror_blended_holdout_2024/holdout_metrics.json`** on rules deployed,
rules available, cases flagged, errors and precision; the script asserts it and
stops on drift.

Coverage on FY2024 **error-case** variances is near total: NATURE 1.000, AGENCY
1.000, DISCOV 0.992, TIMEPER 0.990, with no state below 0.938 on timing. An
earlier version of this note reported 70% timing coverage; that figure was
computed over all variance records including sub-threshold variances on non-error
cases, and does not describe the population characterized here.

National mix on FY2024 error-case variances, so a rule's share reads as lift:
cause is agency 54.7%, client 41.9%, other 1.6%, no-fault 1.4%, third party 0.3%;
of variances reporting a timing, 61.3% arose at certification.

543 distinct rules, median 203 variances and 126 error cases each pooled over
FY2022-24. All 543 clear 20 variances.

## Why there is no "mode element" column

Issue #2 asked for the modal element and its share. That was built first. It was
not unstable across years: the modal element agreed between FY2022-23 and FY2024
for 463 of 529 rules, 87.5% against 41.6% chance. It was uninformative, at a median
34.9% of a rule's errors with 320 of 529 rules sharing wages and salaries. And a
mode is a fragile summary of a flat distribution: even on the 7-group scale the top
group flips between two random halves of the 49 states in 11.2% of 19,061
comparisons, because it leads the runner-up by a median 0.179 and by under 0.10 for
22.7% of rules. The shares themselves are stable at the sampling floor over the same
splits; collapsing them to a winner is what is not. `element_groups_to_75` gives the
ordered list for anyone who wants a headline.

## Does each field carry signal?

Three terms, defined once.

- **Reliability**: the share of the spread across rules that is real difference
  between rules rather than sampling error. 0 means the apparent differences are
  entirely noise, 1 means entirely real. It answers "does this number describe
  *this* rule?"
- **Split-half over states**: split the 49 states at random into two halves,
  compute the same rule's share on each, record the absolute gap, repeat over 40
  splits. It answers "would a different set of states have told the same story?"
- **Sampling floor**: the gap you would expect from that split if the rule's true
  share were identical in both halves and only sampling differed. It is not zero.

The **ratio** of observed gap to floor is what carries the meaning. Near 1, the
field differs across halves of the country by exactly what noise predicts, so
there is nothing to explain. Above 1, there is real between-state variation on top
of noise.

| field | median share | reliability | split-half obs | floor | ratio |
|---|---|---|---|---|---|
| earned income | 0.330 | 0.94 | 0.034 | 0.032 | 1.06 |
| unearned income | 0.188 | 0.94 | 0.030 | 0.031 | 0.96 |
| shelter deduction | 0.182 | 0.92 | 0.030 | 0.038 | 0.79 |
| utility allowance | 0.050 | 0.90 | 0.018 | 0.019 | 0.92 |
| medical deduction | 0.013 | 0.98 | 0.008 | 0.012 | 0.73 |
| dep care or child support deduction | 0.043 | 0.92 | 0.013 | 0.014 | 0.97 |
| wrong amount, known item | 0.298 | 0.75 | 0.041 | 0.039 | 1.04 |
| wrong include/exclude decision | 0.287 | 0.93 | 0.039 | 0.046 | 0.86 |
| unreported source of income | 0.104 | 0.70 | 0.026 | 0.025 | 1.05 |
| household composition | 0.059 | 0.79 | 0.012 | 0.016 | 0.79 |
| change in circumstances | 0.078 | 0.63 | 0.025 | 0.023 | 1.11 |
| arose at certification | 0.604 | 0.85 | 0.057 | 0.046 | 1.23 |
| arose after certification | 0.298 | 0.88 | 0.058 | 0.044 | 1.33 |
| coded agency-caused | 0.566 | 0.72 | 0.062 | 0.047 | 1.32 |
| coded client-caused | 0.395 | 0.75 | 0.061 | 0.047 | 1.30 |
| surfaced from the case record | 0.409 | 0.75 | 0.061 | 0.045 | 1.35 |
| overissuance, of directional error cases | 0.746 | 0.95 | | |
| error_status is other_error | 0.269 | 0.90 | | | |

Two readings, and they differ by field type:

- **What the error is about** (element and nature groups) travels. Reliability
  0.63 to 0.98, and split-half ratios of 0.73 to 1.11, meaning the difference
  between two halves of the country is what sampling alone would give.
- **Who was coded as the cause, when it arose, and how it surfaced** carry real
  state-to-state variation on top of sampling: ratios 1.23 to 1.35. These fields
  are informative but a national number will be somewhat off for any one state,
  which is a fact about state coding and process rather than about the rule.

## Direction: over versus under

`status` (2 overissuance, 3 underissuance) is populated for every error case, so the
direction field uses it. The median rule is **0.746 overissuance** of its directional
error cases, 10th to 90th percentile 0.606 to 0.999, reliability 0.95.

It deliberately does not use `error_status`. That field's `other_error` category is a
residual error *type* carrying both directions (3,830 overissuance and 2,209
underissuance cases in FY2022-24), so including it in the denominator produces a field
that correlates -0.90 with how little other_error a rule catches and only 0.21 with
direction. The share of a rule's error cases whose `error_status` is `other_error` is
reported separately, median 0.269.

## Era drift, and one codebook change

| field | median abs difference | sampling floor | ratio |
|---|---|---|---|
| wrong amount, known item | 0.138 | 0.038 | **3.60** |
| wrong include/exclude decision | 0.070 | 0.042 | **1.68** |
| arose after certification | 0.053 | 0.041 | 1.28 |
| unearned income | 0.036 | 0.031 | 1.16 |
| arose at certification | 0.051 | 0.044 | 1.15 |
| surfaced from the case record | 0.047 | 0.043 | 1.10 |
| coded client-caused | 0.045 | 0.045 | 1.00 |
| all remaining fields | | | 0.75 to 0.98 |

Everything sits at or near its sampling floor except the two largest nature
groups, and that is a coding change rather than instability. **Nature codes 56,
57, 33 and 58 are 0.00% of variances in FY2022 and FY2023 and appear only in
FY2024.** Deduction errors previously coded 52 or 53 are now split into 56 and 57,
which moves "wrong amount, known item" from 14.3% to 23.3% and pulls "wrong
include/exclude decision" from 21.6% to 17.3%.

The same two fields score 1.04 and 0.86 on the split-half-over-states test, which
is why that was the primary test: the geographic axis gives many draws and is not
contaminated by a mid-window codebook revision, while the temporal axis gives one
draw and was.

## Does knowing the rule tell you anything at all?

Normalized mutual information runs from 0, when knowing the rule tells you nothing
about the error's group, to 1, when it tells you everything. The null is what NMI
would be if the two were unrelated, obtained by shuffling group labels across
variances 60 times.

| pairing | groups | NMI observed | NMI under the null | distance above null |
|---|---|---|---|---|
| rule and element group | 7 | 0.0435 | 0.0020 | 985 sd |
| rule and nature group | 10 | 0.0159 | 0.0096 | 233 sd |

The two right-hand columns answer different questions. The distance says the
association is not chance, and it is large only because the null is tight: 259,958
(rule, variance) pairs sit behind it, so a small association is easy to detect.
**It is not an effect size.** The effect size is NMI, and 0.0435 on a 0-to-1 scale
says a rule shifts the mix of error types well short of determining it.

The magnitude a state actually uses is the spread across rules: earned-income
share runs 0.03 to 0.43 between the 10th and 90th percentiles, and 0.00 to 0.70
end to end, against a national mix of 0.33.

A case trips several rules, so the units here are (rule, variance) pairs rather
than a partition. The permutation null has the same structure, so the comparison
holds.

## Files

| file | contents |
|---|---|
| `rule_characterization.csv` | the sheet: one row per rule, FY2022-24 pooled, every share with its Wilson interval |
| `rule_profiles.csv` | the same fields for each era separately, which is what the drift table is computed from |
| `deployed_rules.csv` | 2,390 deployed rule instances by state and budget |
| `stage1_checks.csv` | the 98 reproduction checks against the shipped scorecard |
| `characterization.md` | the generated evidence tables above |

Per-rule counts double-count cases that trip several rules, so they are not a
partition of the flagged total.
