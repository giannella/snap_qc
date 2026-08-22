# Design note: era-2 replication of the utilities SUA-tier variant (drafted 2026-08-22)

**Question (one sentence).** Does the SUA-tier encoding's one-year-ahead
result (era 1: 2022-23 -> 2024, result_2026-08-22.md) replicate on an
independent era (mined FY2017-18, scored held-out FY2019), where no
outcome ever informed the anchor or the band?

**What varies, with exactly one component varying.** TWO fresh mines on
the CURRENT frame (archive_data/ holds the pre-repair frame the old
era-2 cache was mined on; a frame-consistent comparison needs both arms
on one frame, so the raw-utilities baseline is re-mined too rather than
reusing that cache):
- arm A (baseline): the benchmark recipe verbatim with TRAIN_YEARS =
  2017-2018, TEST_YEAR = 2019, raw `utilities`;
- arm B (variant): identical, with `utilities_sua` (the SAME mode - 200
  construction as era 1, verbatim; pre-declared in design_note.md).
Seed 117, joint BH + n >= 30, 99% LCB, artifact gates, shipped
blend/fill/walk, FY2019 cap-walk scoring. Era-specific decision to
record: the FY2017-19 public files predate the FY2024 cat_elig recode,
so bbce_state_i's regime construction is computed on those years as
coded (share of cat_elig >= 1 reaching 0.5 per state-year); both arms
share it, so it cancels in the paired contrast.

**Support after the split (computed before the run).** Era-2 pre-check
(era2_precheck.R, 2026-08-22): 147 FY2017-19 state-year cells, all
utilities values integral, ZERO cells with a tied top count (the mode_pos
tie rule never fires), zero cells with mode <= 200 (tier 1 never
definitionally empty), mode range 265-822. The fixed $200 band is
proportionally looser here than on era 1 (73 of 147 cells have mode
<= 400 vs 39 of 147), recorded as a known property of the v1 parameter,
not a study subject (project lead, 2026-08-21). Train FY2017-18 and test
FY2019 row/error counts are asserted in the driver from the current
frame at launch (the 2026-08-06 plan's 79,907 / 7,115 and 39,221 / 3,931
were pre-repair-frame figures and will differ).

**Readouts and bars (the era-1 set, unchanged).**
1. Paired non-inferiority, arm B vs arm A, per state x budget: median
   >= -0.005, mean >= -0.01 for precision and dollar recall at both
   budgets. The zero-harmed clause is NOT carried (result_2026-08-22.md:
   between two independent mines it is unattainable by the null; the
   same-recipe re-mine reference is 10 harmed at 5%, 4 at 10%). Instead
   the harmed-tail count is reported beside that reference: a tail
   materially above the null re-mine's is the signal to read.
2. Residual fragility: family reach-collapse < 1.5x the non-family
   reference in the train-n band, with the exact binomial CI.
3. Family representation and per-state tier-1 mass, as monitors.

**Shipping rule.** If readouts 1 and 2 clear on era 2 as on era 1, the
two-era discipline (findings 20) is satisfied and the staged
utilities_sua lists (methods/v250_candidate_lists_utilsua/) become
promotable to state_delivery_lists/ as a versioned release, a project-lead
decision. If era 2 fails, the era-1 result stands as a one-era
observation and the lists stay staged.

**Runtime and scheduling.** Two fresh mines (~4 h each on this host;
the FY2017-18 train set is of similar size to FY2022-23): ~8 h, launch
22:00-23:00 per the overnight policy; checkpointed per unit. Pre-run:
fresh senior-statistician review of this note and the two drivers.
