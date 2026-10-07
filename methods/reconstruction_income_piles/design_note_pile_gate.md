# Pile gate: design note (2026-10-07)

1. **The question.** Does dropping, before the fill walk, every rule with at
   least 25% of its training flags or errors on reconstruction income-pile
   rows (issue #29) change the v2.7 lists' performance, read on all test
   cases and on test cases that are not pile cases?

2. **What varies and what is held fixed.** Varies: the pile gate (off = the
   shipped v2.7 lists; on = tagged rules dropped from each pool before
   dedup and the walk, like the artifact tag). Fixed: frame (final v2.7,
   1dbe8733ad0f30bc); pools (the v2.7 seed-117 pools, cached, no mining);
   vocabulary; admission (BH FDR 10% + n >= 30); artifact gate; 99% LCB
   order; fresh-share walk f = 0.50; 3x buffer; 5% / 10% budgets; scoring.
   Delivery build: the same gate in both builders, walked against each
   state's FY2022-24 caseload from the cached pools.

3. **Support after the split.** Pile rows (income_pile_rows(), at least 10
   down-corrected cases sharing one as-recorded amount per fiscal year and
   household size): FY2022-24 1,394 of 115,559 rows (errors 470 of 13,161);
   by test year FY2022 427 rows (135 errors), FY2023 468 (173), FY2024 499
   (162). Training rows of the primary window, where its rules are tagged:
   FY2017 499 pile rows (124 errors), FY2018 371 (92), FY2019 362 (104),
   1,232 rows and 320 errors in all. Removal-test windows: train FY2022-23
   (76,031 rows, 8,397 errors) -> test FY2024 (39,528 / 4,764); train
   FY2017-19 (116,060 / 10,920) -> test FY2022-24 (115,559 / 13,161), each
   year walked at its own budget. Per state the non-pile caseload loses a
   median of about 1% of its cases. Scale of a single state's reading: a
   small state's 5% FY2024 budget is about 33 cases (Washington), so one
   error moves its precision by 0.030 and the -0.05 harmed line is two
   errors (ledger: about 44 cases per state at 5%, SE about 0.068).
   On the shipped lists 345 of 3,127 national rules and 20 of 399 state-pool
   rules are tagged (methods/reconstruction_income_piles/rule_pile_share.csv).

4. **What we already know.** The artifact gate (findings §38, ledger
   "mismatch rows" rows) uses the same 0.25 tag threshold and the same
   drop-before-fill placement; the width floor (§40 addendum) shipped on a
   removal-invariance reading. The near-1 benefit-ratio artifact (§35, §38)
   is the same family: a reconstruction defect that held-out tests on the
   public frame cannot see, because test years carry it too. The
   three-years-ahead window is primary for changes to how cutoffs carry
   across years (process rule, 2026-10-06); this change is about the frame's
   measurements, so both windows are read.

**Decision rule (project lead, 2026-10-07).** The gate ships in the v2.7.0
lists and workbooks. Merge to main if the rebuilt workbooks are similar to
the current ones: at least 45 of the 47 released workbooks (all states but
DC and Georgia) whose headline precision (all shipped rules combined, on the
state's FY2022-24 demo cases) drops by no more than 3 percentage points; the
49-state count is reported beside it. Review (fresh senior-statistician,
2026-10-07): pipeline diff and runner approved; the removal test's support
counts and dollar-recall companions added at its request. The removal test is recorded alongside; the
all-cases reading is expected to favor the ungated lists (pile cases are
error-rich in every year), the non-pile reading stands in for a state's own
case file.
