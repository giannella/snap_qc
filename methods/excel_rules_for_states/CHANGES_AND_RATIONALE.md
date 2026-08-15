# SNAP QC dashboard: what changed in the tuning, and why

Written 2026-07-29, for the original author of `build_workbook.py` and for Eric.

The workbook you built does something we needed and did not have: it puts the
delivery rules in front of a reviewer, lets them turn rules on and off, and shows
what happens to the review pile as they do. All of that is unchanged. Every sheet,
the checkbox mechanics, the hidden hit matrices, the Error Cases panel, the
openpyxl-then-postprocess split that keeps Excel from offering to "repair" the
file: all kept as written. The `--v2` build is the same workbook.

What changed is narrower than it looks from the diff. Two things:

1. **Where the case data comes from.** The rules were mined on a frame whose
   variables have been restored to their pre-QC-process values. The workbook was
   rebuilding those variables from the raw `.sav` files, which does not reproduce
   them, so the rules were being scored against a different scale.
2. **How the thresholds get tuned.** The original searched every threshold over a
   wide range and kept whatever scored best on the same rows it searched. That is
   the setup that reliably produces numbers which do not survive contact with next
   year's data. It is now a tiered procedure with explicit guards, and each guard
   exists because of a specific way the old setup could mislead.

Everything below is the reasoning, with the measurements behind it. Files: the new
tuning lives in `tuning.py` (statistics only, no Excel) and
`build_workbook_v2.py` (the same builder, calling it). `build_workbook.py` is
untouched apart from a stderr warning about which frame it is on.

---

## Part 1. Why we changed the data source

### What we found

The workbook rebuilt its features in Python from `qc_data/*.sav`: rent, utilities,
earned income, deductions, then benefit ratios on top of those. We compared that
reconstruction against `reg_model_data.rds`, the frame the R pipeline mines rules
on, joining row by row on `yrmonth + hhldno + stratum`.

The **rows** matched perfectly. Both had 2,356 Washington cases for FY2022-24,
753/788/815 by year, with the error flag and error amount agreeing on every single
row and the household-size stratum agreeing on 2,355 of 2,356. So the row filters
had been replicated correctly.

The **feature values** did not match:

| feature | rows differing (of 2,356) |
|---|---|
| `unc_rawben_rel_max` | 24.8% |
| `rawben_rel_max` | 24.4% |
| `total_deductions_by_hh_size` | 24.1% |
| `shelter_expenses_by_hh_size` | 11.1% |
| `utilities`, `medical_deductions` | 1.2% |

Everything not in that list agreed on all 2,356 rows: `children_i`, `cat_elig`,
`expedited_i`, `elderly_disabled_i`, `married`, `percent_abawd`,
`months_since_cert_n`, `count_divisible_by_100`.

### The cause, and how we pinned it

`1_data_munging_and_raw_variable_reconstruction_for_using_public_qc_data.R` runs
with `correct_variables <- TRUE`. For each case it reads the QC error elements and
restores the field to its pre-QC-process value, iterating until the benefit it
recomputes from the restored fields matches its target, then snaps utilities to the
nearest valid state/year utility allowance. The Python code read `RENT`, `UTIL`,
`FSMEDDED`, `FSEARN` and `FSUNEARN` straight from the file, skipping all of that.

The attribution is exact, with no exceptions in either direction:

| field | rows where R differs from the raw value | all carrying the matching error element | rows changed *without* that element |
|---|---|---|---|
| rent | 226 | 226 (`ELEMENT1 = 363`) | 0 |
| utilities | 28 | 28 (`364`) | 0 |
| medical deductions | 28 | 28 (`365`) | 0 |
| earned income | 177 | 177 (income elements) | 0 |

Restored rent and utilities then cascade through the shelter deduction, net income
and benefit, which is why the derived ratios diverge more than their inputs.

### What it cost

Same 2,356 rows, same 114 delivery rules, scored on held-out FY2024:

| | features rebuilt in Python | features from the munged frame |
|---|---|---|
| rules deployed at a 10% review budget | 64 | 74 |
| **rules that never fire at all** | **21** | **0** |
| held-out precision | 0.255 | 0.318 |
| held-out recall | 0.188 | 0.203 |
| held-out dollar recall | 0.253 | 0.274 |

Twenty-one of 114 rules were dead. A rule mined at a threshold on the restored
scale, applied to the unrestored scale, can simply never match anything.

### How it works now

`export_state_frame.R` pulls the state's rows out of `reg_model_data.rds` and
applies `prep_features()` from `rule_mining_helpers.R`. That second part matters:
`prep_features()` is the same function the delivery-list builder calls, and it is
what turns logicals and two-level factors into the 0/1 columns the rule text refers
to (for Washington it reports `homeless coerced to 0/1 (1 = "TRUE")`). Calling the
miner's own function means the coercion cannot drift away from the rule text. The
export is cached under `.frames/` and rebuilt with `SNAP_REFRESH_FRAME=1`.

Cost of this approach: v2 needs `Rscript` on the path and `reg_model_data.rds`
present. It no longer opens the `.sav` files at all.

### Why *this* frame and not more data

Reasonable next question: is the munging script throwing away rows we could use? We
tested it by re-running the script with its row exclusions relaxed. The answer was
no, and it is worth knowing why, because it is the same class of reasoning as the
tuning guards.

The script drops rows in four places. Relaxing them took the frame from 237,391 rows
to 305,954, and from 24,334 errors to 42,102. Tempting, and wrong:

- **19,095 of the added rows come from switching off one filter**, the one keeping
  only rows where the file's two independent statements of the benefit error agree
  within $5. In FY2022-24 those rows are 66.6% "errors" against 11.2% elsewhere.
  That looks like a rich seam and is not one: they are by definition the rows where
  the two statements disagree, and the pipeline decides "error" by reading one of
  those two statements. The 66.6% restates the disagreement. On 59% of them the
  reported error amount is zero while the benefit figures differ by a median of $93.
  Independently, the pre-QC restoration fails on exactly these rows: the recomputed
  benefit misses its target by a median $51 against $0 on the rows that pass the
  filter, with 29.3% landing within $5 against 95.5%. So: a label we cannot trust,
  on features that were not successfully restored.
- **FY2020 and FY2021 are excluded by decision**, not by that measurement. The data
  for those years is poor and misleading and the practices states used were
  qualitatively different, so including them mixes eras rather than adding data.
- The fourth exclusion turns out to drop 9,456 rows that are *all* FY2021, 100% of
  that year and 0% of every other year, because FY2021 carries the pandemic 15%
  allotment increase that the lookup table does not. Moot, given the year decision.

The by-product is the useful part: on the six years we use, that filter is
additive-only. The rows it keeps reproduce the production frame's rows and errors
exactly, year by year, zero mismatches. We know precisely what it removes and that
it perturbs nothing else.

Full record: `methods/modeling_findings.md` §24.

---

## Part 2. Why the tuning changed

### What the original did

For each rule, for each numeric condition, build a grid over the 2nd-to-98th
percentile of the state's own data; evaluate every combination; keep the one
maximising error dollars subject to raw precision >= 25%, at least 10 cases
flagged, and at least 2% of error dollars caught. Report those as the tuned
thresholds.

Each piece of that is a defensible instinct. Together they have a problem: the
combination is chosen on the same rows it is then scored on. With hundreds of
combinations per rule and a few dozen cases behind each, the winner is partly the
luckiest, and the reported precision is the luck included. This project has measured
that effect directly: a rule list built to hit 0.20 precision delivered about 0.10
out of sample, and in single-state work at a support floor of 5 cases the median
held-out precision of the selected rules was **0.000**, with 59% of them catching
nothing at all. With a floor of 30 the same procedure deflated gently instead, from
0.33 on training data to 0.21 held out.

That is the failure mode the v2 structure is built around. It is not a criticism of
the code, which does what it says; it is that "best on the data you searched" is not
an estimate of anything.

### The structure now: three tiers, default lowest

| tier | what may change | what has to be true |
|---|---|---|
| **0** | nothing. The delivered rules at delivered thresholds, re-filled against the state's caseload in rank order | nothing. Uses no outcomes at all |
| **1** | which rules are used, and in what order. Rule text frozen | each rule beats its own stratum's base rate under Benjamini-Hochberg at FDR 10%, with at least 30 flagged tuning-year cases |
| **2** | numeric thresholds, inside ±25% of delivered values | Tier 1 admission, plus a precision bound that tightens with the size of the search |

Tier 0 is not a placeholder. Freezing the national list and sizing it against a
state's own caseload costs under a point of precision versus an idealised
after-the-fact list (0.294 vs 0.301 at a 5% review budget across 18 states), and it
needs no outcome data, so there is nothing to overfit. For Washington on public
data, Tier 0 is what deploys.

### What each constraint does

Taking them one at a time, because each buys something specific.

**Structure is frozen; only thresholds move.** Variables, operators, the number of
conditions, and the household-size stratum are fixed. This is the single biggest
constraint, and it is the difference between perturbing a validated rule and
searching for a new one. It also bounds the search: a 3-condition rule has 125
candidate combinations, not thousands.

**±25% bracket instead of the 2nd-98th percentile.** `(0.75, 0.90, 1.00, 1.10,
1.25)` for rules with three or fewer conditions, `(0.90, 1.00, 1.10)` beyond that,
because combinations grow exponentially in the condition count. The justification is
concrete rather than aesthetic: the thresholds were rounded to three significant
figures from wherever a tree happened to split, so a nearby cut is the same rule
relocated inside its own estimation noise. A percentile-wide grid is a different
hypothesis wearing the same rule's clothes.

**The delivered value is always in the grid, and wins ties.** So "leave it alone" is
always available, the search can never do worse than what shipped, and a tie does
not silently move a threshold.

**Candidates that partition the data identically are collapsed.** Two cuts with no
observed value between them flag the same cases. They are one test, not two.
Counting them separately would inflate the multiplicity without adding a candidate.
Binary indicators collapse to a single cut, and cuts that can never fire in this
state are dropped. This is a correctness measure, not a speed one: the number of
genuinely distinct tests is what the winner's curse scales with.

**Admission is Benjamini-Hochberg, not a fixed precision floor.** The question "is
this rule better than reviewing at random in this stratum?" is a hypothesis test,
and there are 114 of them. A fixed floor like 25% ignores how many candidates were
tried and how strong they are; BH sets the bar from the tests actually conducted.
Practical difference: with ten p-values at 0.09 and one at 0.005, an unadjusted 10%
cut admits two and BH admits one.

**The support floor of 30 flagged cases is separate from, and not replaceable by,
the confidence bound.** Discussed at length below, because it is the number you
asked about.

**Ordering is the 99% Wilson lower bound on training precision, never raw
precision.** The bound pulls small-sample estimates down toward what holds up, so a
rule with 6 of 12 does not outrank one with 40 of 100.

**The within-rule search is paid for explicitly.** For a rule with *m* distinct
threshold combinations, the qualification bound uses `alpha / m` rather than
`alpha`. Searching wider means clearing a higher bar. This is why, in practice,
Tier 2 rarely moves a threshold at all: in the Washington runs it moved **zero** of
them at every support floor we tried.

**The split is by fiscal year, most recent year held out, and there is no random
option.** A random split leaks: the same caseload composition sits on both sides.
`time_split()` verifies the split is disjoint and forward-looking and refuses a
single-year frame. (There is one deliberate exception, `holdout_year_set`, for
running the 2022+2024 → 2023 year swap; anything produced that way is labelled
INTERPOLATED wherever it is reported, because a held-out year that sits *between*
the training years flatters every arm.)

**The held-out year decides one comparison per tier, never one per rule.** This is
the guard that is easiest to lose. If you let the held-out year pick which rules to
keep, you have simply moved the overfitting one step later. So each tier's whole
list is built on the tuning years, and the held-out year answers a single
pre-declared question per tier: does this tier's held-out precision, taken at its
90% lower bound, beat Tier 0's held-out precision outright, on at least 30 flagged
held-out cases? At most two such comparisons happen in a run, and both are fixed
before it starts.

**The untuned arm is always computed and reported beside the tuned one**, and the
number of distinct combinations evaluated is printed and written into the workbook.
The validity argument depends on that count, so it should be visible rather than
implicit.

**Refusals are explicit.** Below 30 admitted rules, Tier 2 does not run: the
rule-of-thumb from earlier per-state work is that local tuning pays only when
roughly 30 or more rules clear the bar. Below 30 errors in the held-out year, no
tier is judged at all and the delivered list ships untouched.

### Where it all shows up

A new **Tuning Audit** sheet carries the split, the tier that deployed and why, how
many things were compared, each arm's held-out performance side by side, and every
per-rule admission test (flagged, errors, p-value, BH cutoff, bound, admitted,
threshold moved, deployed). The Summary tab now says in its header which tier it is
showing, and states plainly that its own figures are computed over all years
including the ones used to select, so the held-out figures on the audit tab are the
ones to quote. The interactive Grid Search sheet was kept, but bounded to the same
±25% bracket and re-gated on the confidence bound rather than raw precision, so it
cannot be used to search wider than the engine allows.

### What Washington actually produced

Tier 0. Not one of the 114 delivered rules flags 30 tuning-year cases: the widest
flags 22. So nothing is admitted, no thresholds move, and the state receives the
delivered list unchanged. On held-out FY2024 that list flags 44 cases at 0.318
precision against an 8.5% base rate, catching 20.3% of errors and 27.4% of error
dollars at 5.4% workload.

We re-ran the whole thing with the years swapped (tune 2022+2024, hold out 2023) as
a robustness check. The verdict replicated exactly: Tier 0 both ways, nothing
admitted either way, widest rule 22 and 20 against a floor of 30. Worth reading the
verdict rows there and not the precision rows: the two splits deployed lists of
different length (74 vs 94 rules), so their precisions are not comparable, and both
rest on 14 and 17 caught errors.

The reason nothing clears the floor is structural, not a Washington quirk. A
budget-filled delivery list is *made of* narrow rules: it is assembled by walking a
ranked pool and adding rules until the review capacity fills, so the rules that get
in are high-precision and small-footprint by construction. On the public QC file,
which samples roughly 800 Washington cases a year, a rule that flags 1% of the
caseload flags about 8 cases a year. The floor becomes reachable on a state's larger
internal frame, not by lowering it.

---

## Part 3. What relaxing the support floor to 15 or 20 would do

You asked us to speculate. We measured it instead, then speculated about the part
the measurement cannot reach.

### The measurement

`support_floor_sweep.py` re-runs the whole procedure at several floors on
Washington's frame, holding everything else fixed:

| min_support | rules clearing the floor | admitted by BH | rules in the tuned list | held-out flagged | held-out precision | held-out recall | thresholds moved |
|---|---|---|---|---|---|---|---|
| 5 | 25 | 4 | 3 | 7 | 0.143 | 0.014 | 0 |
| 10 | 14 | 8 | 7 | 25 | 0.360 | 0.130 | 0 |
| 15 | 5 | 2 | 2 | 6 | 0.167 | 0.014 | 0 |
| 20 | 3 | 2 | 2 | 6 | 0.167 | 0.014 | 0 |
| **30** | **0** | **0** | (Tier 0: 74) | **44** | **0.318** | **0.203** | **0** |

Four things in that table.

**Lowering the floor makes the deployed list shorter, not better.** This is the
part that is easy to miss. Tiers 1 and 2 deploy *only admitted rules*. At a floor of
20, two rules are admitted, so the state's list goes from 74 rules to 2, and recall
falls from 0.203 to 0.014. You trade a fourteen-fold loss of reach for a precision
estimate that is worse (0.167 against 0.318) and rests on six flagged cases.

**15 and 20 are the same number here.** Both admitted the same two rules, both with
exactly n = 20. Rule footprints at state scale are lumpy; there is nothing in
between. Do not expect a smooth dial.

**The results are non-monotone, and that is the finding.** Floor 10 gives 0.360,
floors 15 and 20 give 0.167, floor 5 gives 0.143. Precision does not degrade
gracefully as the floor drops; it jumps around, because with 2 to 8 rules and 6 to
25 held-out flagged cases these numbers are not measuring a stable property. That
non-monotonicity is the signature of the regime the floor exists to keep us out of.
The 0.360 at floor 10 is the most dangerous cell in the table: it is the one that
would look like evidence for relaxing the floor, and it is 9 caught errors.

**The held-out gate rejected every one of them.** `tier_deployed` is 0 at every
floor, because no tuned arm's held-out lower bound beat Tier 0's 0.318. So on this
state, relaxing the floor would not have changed the delivered workbook at all. It
would have added churn upstream of a gate that says no.

### Why the floor and the bound are not redundant

The confidence bound already penalises small samples, so why also have a hard floor?
Because they answer different questions. Here is the bound at a few sample sizes,
same raw precision:

| | raw 30% | raw 50% |
|---|---|---|
| n = 15 | 0.093 | 0.267 |
| n = 20 | 0.127 | 0.269 |
| n = 30 | 0.149 | 0.305 |
| n = 50 | 0.175 | 0.344 |

The bound does its job: at 50% raw precision, n = 20 is worth 0.269 and n = 30 is
worth 0.305, so a small rule sorts below a larger one with the same hit rate. What
the bound cannot do is tell you that an estimate is too poorly measured to *rank at
all*. A rule with 10 of 20 and a rule with 6 of 20 differ by one or two cases going
either way, and the bound will happily order them. The floor is an
estimation-quality guard; the bound is an ordering statistic. Removing one does not
let the other cover for it. In the delivery builder, dropping the floor and keeping
only the false-discovery test cost 4 to 5 points of precision.

### The part that is genuinely speculation

The measurement above is one state, one list, one pair of year splits, and it is a
*filtering* problem: choosing among 114 fixed rules. The floor was originally
established on a harder problem, mining rules from scratch on a single state's data,
where the candidate pool is thousands of rules rather than 114. There, a floor of 5
produced a median held-out precision of 0.000 and 59% of selected rules caught
nothing; a floor of 30 held, deflating from 0.33 to 0.21.

So what would we expect at 15 or 20 on a state's real internal frame, where rules
have genuine support?

- **More rules admitted, and the marginal ones deflating hardest.** From the bound
  table, a marginal rule at n = 20 carries roughly 3 to 4 points less bound than the
  same hit rate at n = 30. That is the average cost, and it is small.
- **The variance is the real cost, not the average.** The marginal rules are exactly
  the ones whose held-out precision can land at or near zero. A list is judged by
  reviewers on whether it wastes their time, and a handful of rules that fire on
  clean cases does disproportionate damage to that judgement.
- **A second-order effect worth knowing:** lowering the floor increases the number
  of BH tests (14 tests at floor 10 here, 3 at floor 20), which makes BH's per-test
  bar *stricter*. So part of what you gain by lowering the floor is taken back by
  the multiplicity correction. The two knobs are coupled.
- **Tier 2 probably still will not move thresholds.** It moved none at any floor
  here, because the per-rule Bonferroni bound is what gates that, and it does not
  depend on the floor.

Our recommendation: leave the floor at 30 as the delivered default. If a state wants
more reach, the lever is a larger review budget or the buffer rules (the delivery
file carries rules to three times the core depth for exactly this reason), not a
lower floor. Those give more reach without weakening any estimate.

If you do want to try 15 or 20, the honest way to do it is on a state's internal
frame rather than the public file, with the held-out gate left in force, and with
the expectation that its main effect is to make the tuned arm *fail* the gate more
often rather than less. Every floor is a one-line change in `states.py`
(`TUNING['min_support']`), the sweep script re-runs the comparison, and the audit
sheet will show exactly which rules the change let in and on how many cases.

---

## Reproducing any of the above

```bash
python snap_dashboard/tuning.py --selfcheck        # 35 checks on the statistics
python snap_dashboard/make_state.py WA --v2        # build the workbook
python snap_dashboard/compare_splits.py WA         # forward split vs year swap
python snap_dashboard/support_floor_sweep.py WA    # the floor table above
```

Numbers in Part 1's data-source section come from
`methods/modeling_findings.md` §24 and the logs in
`methods/munging_exclusion_check/`. Numbers in Parts 2 and 3 come from
`.build/support_floor_sweep.log`, `.build/splits.log` and the Tuning Audit sheet of
the built workbook. The winner's-curse and support-floor evidence predating this
work is in `methods/modeling_findings.md` §1, §9 and §19, and the design reasoning
is in `methods/tuning_principles.md`.
