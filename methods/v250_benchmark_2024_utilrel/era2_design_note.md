# Design note: era-2 replication of the utilities SUA-tier variant (2026-08-22; pre-run review APPROVE WITH CHANGES, applied)

**Question (one sentence).** Does the SUA-tier encoding's one-year-ahead
result (era 1: 2022-23 -> 2024, result_2026-08-22.md) replicate on an
independent era (mined FY2017-18, scored held-out FY2019), where no
outcome ever informed the anchor or the band?

**What varies, with exactly one component varying.** TWO fresh mines on
the CURRENT frame. The cached FY2017-18 national pool
(methods/state_similarity_v2/era_validation_train1718_test19/raw_vocab/
raw_national.rds) is NOT a valid baseline arm for two independent
reasons (review 2026-08-22): it was mined with the retired 16-variable
vocabulary (cat_elig, count_divisible_by_100; no bbce_state_i, no
per-size income features), so it would differ from the variant in
vocabulary as well as encoding (the two-at-once error, ledger section
30); and it was mined on the pre-2026-08-08-repair frame, whose FY2017-19
row membership differs from the current frame's (119,128 vs 116,060
rows), so its train n/k cannot be re-derived here. It also holds 18
state mines, not 49. Section 40's era-2 replication could reuse it
because that was a single-arm rule-level diagnostic on the archived
frame; a paired list-level contrast cannot.
- arm A (baseline): the benchmark recipe verbatim with TRAIN_YEARS =
  2017-2018, TEST_YEAR = 2019, raw `utilities`;
- arm B (variant): identical, with `utilities_sua` (the SAME mode - 200
  construction as era 1, verbatim).
Seed 117, joint BH + n >= 30, 99% LCB, artifact gates, shipped
blend/fill/walk, FY2019 cap-walk scoring. Both drivers are copies of
methods/v250_benchmark_2024_utilrel_v2.R; the arm A vs arm B diff must
show exactly the BASE_FEATURES entry, the absence of the tier mutate
block, and OUT_DIR, nothing else (diff recorded in the result file).

**bbce_state_i on this era (measured 2026-08-22).** It is the frame's own
column, computed per state-year over all years by the munging script
(lines ~936-939); no era-specific construction is needed and both arms
share it. On FY2017-19 it is non-degenerate (bimodal shares; 9-10
non-BBCE states per year; cat_elig codes 0/1/2 only, no recode break
inside the era). Facts recorded, not acted on: Indiana flips inside the
training window (share 0.199 FY2017 -> 0.797 FY2018 -> 1.0 FY2019);
Louisiana, Virginia and Mississippi carry the opposite value from FY2022.
VERIFIED against the USDA 2018 state-options edition (options as of
Oct 1 2017; era2_bbce_check.R / .csv, 2026-08-22): the frame's FY2018
flag agrees in 48 of 49 states. The one disagreement is Indiana (flag 1,
USDA "No"), the state measured above as flipping mid-window (share
0.199 -> 0.797 -> 1.0): consistent with an adoption during FY2018 that
the October-2017 snapshot predates, not a flag error. The flag cancels
in the paired contrast in any case. This is the first era-2 run on
the v2.5.0 vocabulary (the section-40 replication and the 2026-08-06
plan used the cached cat_elig pool).

**Support after the split (computed 2026-08-22 on the current frame,
over_threshold != 0).**

| | rows | errors | rate |
|---|---|---|---|
| train FY2017-18 | 77,905 | 7,048 | 0.0905 |
| hh 1 / 2-3 / 4+ | 41,671 / 23,462 / 12,772 | 2,544 / 2,554 / 1,950 | 0.061 / 0.109 / 0.153 |
| test FY2019 | 38,155 | 3,872 | 0.1015 |
| hh 1 / 2-3 / 4+ | 21,393 / 10,914 / 5,848 | 1,525 / 1,334 / 1,013 | 0.071 / 0.122 / 0.173 |

Driver EXPECT_* constants: 77905 / 7048 / 38155 / 3872. Smallest state
train pools: South Dakota 31 errors, North Dakota 34, Wyoming 49; 37 of
147 state x stratum cells carry under 30 train errors (as on era 1; the
national pool carries them). Mismatch-row assert: 423 on FY2017-19
(< 1000). Era-2 pre-check (era2_precheck.csv): 147 state-year cells, all
utilities integral, zero tied top counts, zero modes <= 200, mode range
265-822; the fixed $200 band is proportionally looser here (73 of 147
cells with mode <= 400 vs 39 on era 1), a known property of the v1
parameter. FY2019 eligibility: South Dakota (594 rows, 15 errors) and
Wyoming (273, 13) fall under the 2026-08-06 plan's 400-row / 20-error
threshold; all 49 stay in the PRIMARY readout so the bars match era 1,
and the 47-state companions are reported as a secondary, with SD/WY
harms read as ~14-30-flag cells.

**Readouts and bars (pre-set, identical to era 1 except the harmed-tail
rule).**
1. Paired non-inferiority, arm B vs arm A, per state x budget: median
   >= -0.005, mean >= -0.01 for precision and dollar recall at both
   budgets. Harmed-tail rule, PRE-SET: the count of states with paired
   change < -0.05 must not exceed the independent-sampling NULL BOUND
   computed from FY2019 caseloads (two independent arms of equal true
   precision, flagged n = floor(budget x state rows): expected harmed
   15 of 49 at 5%, 12 at 10%; an UPPER bound on pure noise because the
   two lists overlap heavily). The era-1 figure of 10 harmed at 5% / 4
   at 10% (section 39, which spanned the vocabulary swap, canonicalization,
   Illinois fix and a frame change, i.e. package noise, era 1) is reported
   beside it as the only empirical re-mine reference. A tail above the
   bound is a harm signal; a tail between the package-noise figure and
   the bound is reported as indistinguishable from noise.
2. Residual fragility: family reach-collapse < 1.5x the non-family
   reference in the train-n band [30, 300], with the exact binomial CI
   and the family's in-band rule count; FY2019 slice.
3. Family representation (share of the admitted national pool) and
   per-state tier-1 mass on the FY2019 positive-utilities slice, as
   monitors.

**Pre-stated expectation (distinct from the bars).** Directional
replication with attenuation: readout 2 ratio below 1.5x; readout 1
medians within +/-0.01 of zero at both budgets.

**Shipping rule and the one-shot rule.** If readouts 1 and 2 clear on
era 2 as on era 1, the two-era discipline (findings 20) is satisfied and
the staged utilities_sua lists (methods/v250_candidate_lists_utilsua/)
become promotable to state_delivery_lists/ as a versioned release, a
project-lead decision. If era 2 fails, the era-1 result stands as a
one-era observation and the lists stay staged. ONE-SHOT: after this run,
FY2019 has been read for the utilities-encoding design area; the
continuous-ratio fallback named in the era-1 note could not then be
judged on 2017-18 -> 2019 as a clean confirmatory era and would need a
fresh pre-registration on an unread test bed.

**Driver constraints (copies of the era-1 driver).** (a) V240_SCORECARD
is FY2024-only: disable the inherited NOT_THE_READOUT join in both era-2
drivers; (b) the JSON `built` string must not say FY2022-23; (c)
readouts_utilsua.R is parameterized to arm A's era-2 CSV, the era-2
variant pool, and the FY2019 slices; (d) SMOKE=1 on both drivers before
launch; (e) distinct OUT_DIRs under methods/v250_benchmark_2024_utilrel/
(era2_baseline/, era2_variant/), never the era-1 caches.

**Runtime and scheduling.** Two fresh mines, ~4 h each (FY2017-18 train
77,905 rows vs era 1's 76,031): ~8 h, launch 22:00-23:00 per the
overnight policy; checkpointed per unit.
