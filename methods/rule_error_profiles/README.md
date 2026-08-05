# Per-rule error profiles: what each delivered rule actually catches

Issue #2 (mode `ELEMENT1` per rule), Ben's over/under follow-up, and the
review-mode extension. Built 2026-08-04 by `methods/rule_error_profiles.py`
(entry point `runners/run_rule_error_profiles.py`), nationally across the 49
states in the any-error scorecard. Mines nothing and changes no delivered list.

**Headline: two of the three promotion criteria fail.** The profile is
era-stable, which was the test we expected to be hardest. It is not
concentrated (the mode element describes about a third of a rule's errors) and
it barely discriminates (60% of rules carry the same mode element, and 93% of
the ones that can be characterised land in the same review mode). Shipping a
mode-element column would put "wages and salaries" next to most rules in the
list, describing a minority of what each one catches.

## How the flagged set is defined

The cases profiled are the ones a delivered list actually pulls: the refill walk
from `methods/add_refill_metrics_v2.R`, core then buffer in rank order, taking a
rule when it adds unflagged cases and the running total still fits
`floor(budget * n_rows)`, run against the FY2024 caseload.

**All 98 state-and-budget combinations reproduce
`methods/anyerror_blended_holdout_2024/holdout_metrics.json`** on rules
deployed, rules available, cases flagged, errors caught and precision. The
script asserts this and stops if it drifts. Michigan at the 10% budget comes out
at 19 rules deployed of 194, 86 cases flagged, 24 errors, precision 0.2791, as
it must.

2,390 deployed rule instances across the 98 lists, 543 distinct
stratum-and-rule pairs, 12,869 flagged case rows.

## The join and its levels

`reg_model_data.csv` joins to `qc_data/qc_pub_fy{2022,2023,2024}.sav` on
(FIPS, `YRMONTH`, `HHLDNO`) within fiscal year. The frame's `state` column holds
the state **name**, not FIPS; `additional_data/state_data.csv` is the lookup.
60,055 variance records match a frame row, covering **13,283 of the 13,288 error
cases** in the frame.

`AGENCY`, `DISCOV`, `VERIF`, `TIMEPER` and `ELEMENT` are recorded **per
variance**, and a case carries up to nine, so every figure here is
variance-level unless it says cases. A case also trips several rules, so
per-rule counts sum to more than the flagged total and are not a partition.

Code definitions come from the FY2023 SNAP QC Technical Documentation
(Mathematica), Chapter V detailed codebook. The `.sav` files carry no value
labels for these variables, so nothing here is inferred from the data.

## Two bases, deliberately

- **national**: the rule evaluated against every national row of the era within
  its household-size stratum. Both eras are computed the same way, which is what
  makes them comparable. Used for all three promotion criteria.
- **deployed**: the FY2024 cases the delivered lists actually pull through that
  rule. Used for the decision-support numbers. Cells are much thinner here:
  median 4 error cases and 5 variances per deployed rule, and 80 of 543 rules
  catch no error at all in their deployed cases.

## Criterion 1: concentration. Fails.

Does the mode element dominate a rule's errors?

| basis | median top-element share | quartiles | rules above 0.50 |
|---|---|---|---|
| national, FY2024 | 0.349 | 0.308 / 0.401 | 9.6% |
| national, FY2022-23 | 0.327 | 0.295 / 0.379 | 11.7% |
| deployed, FY2024 (n >= 5) | 0.412 | | |

The bar was that a top element near 20% makes the column noise. At 35% it is
better than that, but for nine rules in ten the modal element is a minority of
what the rule catches. A state reading "this rule finds wages and salaries
errors" would be right about a third of the time.

## Criterion 2: era-stability. Passes.

Does a profile computed on FY2022-23 still describe FY2024? This was the real
test, and it is the one the profile clears.

- mode element agrees across eras for **463 of 529 rules (87.5%)**
- chance agreement, if the mode were drawn from the FY2024 marginal, is 41.6%
- so agreement above chance is 0.786 on a kappa scale

The continuous shares are weaker but positive across eras: catchable-at-action
0.580, desk-closable 0.500, agency-caused 0.411, pre-authorization 0.283.

## Criterion 3: discrimination. Fails for the review mode.

Does the profile vary across rules enough to sort them?

Mode element across the 529 characterisable rules on FY2024:

| element | rules | median share |
|---|---|---|
| 311 wages and salaries | 320 | 0.336 |
| 363 shelter deduction | 98 | 0.405 |
| 365 medical expense deductions | 52 | 0.473 |
| 331 RSDI benefits | 40 | 0.357 |
| 323 dependent care deduction | 9 | 0.333 |
| seven further elements | 10 | |

Eleven distinct elements, but 60% of rules share one. The continuous axes spread
more, though not by much:

| axis (FY2024, national) | median | 10th to 90th | sd |
|---|---|---|---|
| agency-caused share | 0.504 | 0.388 to 0.650 | 0.109 |
| catchable at the agency's action | 0.687 | 0.471 to 0.818 | 0.142 |
| desk-closable (found AND verified in the record) | 0.246 | 0.145 to 0.364 | 0.101 |
| pre-authorization share | 0.134 | 0.052 to 0.217 | 0.073 |

The review-mode indicator is defined as: a variance is pre-authorization work
when it was catchable at or before the agency's action **and** closable from the
case record **and** agency-caused. A rule is `pre_authorization` when at least
half its variances qualify, `post_authorization` below a quarter, `mixed`
between, `insufficient` under 5 variances.

| review mode | national basis | deployed basis |
|---|---|---|
| pre_authorization | 2 | 10 |
| mixed | 33 | 46 |
| post_authorization | 494 | 243 |
| insufficient | | 244 |

That is not a sorting variable. Almost everything is post-authorization work,
which is a fact about SNAP QC variances rather than about our rules: the
desk-closable share is only 0.246 nationally, so most errors need an outside
contact to verify no matter which rule surfaced them.

## Ben's over/under question

`E_FINDG` on the deployed basis, variance-level: **2,727 overissuance (73.8%),
966 underissuance (26.1%), 2 ineligible**.

One caveat matters here. `E_FINDG` is populated for 13,532 of the 20,769
variance records that carry an `AGENCY` code in FY2024, and three states
populate none of it. **Michigan populates it for 2 variances**, so this question
cannot be answered for Michigan from `E_FINDG` at all. The frame's own
`error_status` field carries over/under at the case level for every state and is
the better source if this becomes a delivered column.

## Limits that bound every cause figure above

**Undocumented cause codes.** `AGENCY` codes 22 to 26 appear in FY2023 and
FY2024 data and are defined in no technical documentation through FY2023. They
are bucketed `UNDOCUMENTED` rather than guessed at. Code 26 alone carries 5,669
of the 20,769 populated FY2024 `AGENCY` values, 27.3%. Every agency-versus-client
figure here is computed over the documented remainder.

**Thin cells on the deployed basis.** Median 5 variances per deployed rule, and
244 of 543 rules fall under the 5-variance bar. Pooling 49 states is what makes
the national basis usable; single-state characterisation does not work, which
the Michigan one-off already showed.

**Not a partition.** Per-rule counts double-count cases that trip several rules.

## Files

| file | contents |
|---|---|
| `stage1_checks.csv` | the 98 reproduction checks against the shipped scorecard |
| `deployed_rules.csv` | 2,390 deployed rule instances, per state and budget |
| `rule_profiles.csv` | 1,629 profile rows: 543 rules x 2 eras on the national basis, plus the deployed basis |
| `promotion_criteria.md` | the three criteria, generated |
