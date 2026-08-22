# Design note: held-out decay by dollar-interval width (2026-08-21)

**Question (one sentence).** Do narrow two-sided dollar intervals in the mined
pool carry excess held-out precision decay beyond what the 99% train-LCB
already prices in (the threshold-grid multiplicity the LCB never sees)?

**What varies, with exactly one component varying.** Nothing is re-mined and
no setting changes: one fixed 2022-2023-era any-error national pool
(`methods/v250_benchmark_2024/cache/bench_national_117.rds`, 54,641 rules
with train n / k / 99%-LCB) is evaluated once, as-is, on the held-out FY2024
frame. The single axis is a post-hoc bucketing of rules by RELATIVE interval
width on dollar variables (narrowest interval per rule; one-sided dollar
rules and non-dollar rules form the reference class). The readout per bucket:
train LCB and train precision vs held-out precision, and the decay
(held-out minus LCB). If width carries no excess decay, the buckets show the
same decay as the reference class and the fishing heuristic is refuted.

**Support after the split (computed before the run; corrected per the
2026-08-21 pre-run review).** The held-out FY2024 frame carries 39,528 rows
with 4,763 errors (12.05%). Measured bucket rule counts on the study pool:
<=2%: 193 / 2-5%: 443 / 5-15%: 1,005 / 15-50%: 2,091 / >50%: 2,525 /
one-sided-or-non-dollar reference: 48,384. The script prints per-bucket rule
counts, held-out flagged counts and held-out errors caught BEFORE the
summary; buckets under 30 rules or under 100 held-out flagged cases are
merged upward by design (engineering artifact, not a judged failure). The
review also measured the train-n confound (median train n 134 in the <=2%
bucket vs 1,342 in the reference class), so the primary read is d_raw
(held-out precision minus train precision) within the shared train-n band
[30, 300], with d_lcb beside it answering whether the LCB prices the decay
in, and the share of rules with held-out n < 10 reported as reach-collapse.

**What the ledger and findings already say.** §6: raw train precision has a
strong winner's curse (~0.20 nominal -> ~0.10 held-out) and the Wilson LCB
fixes calibration; this study asks whether interval width is a RESIDUAL axis
the LCB misses. Findings 20: LCB ordering vindicated on two eras (not
re-opened; ordering is not at issue). Dedup/no-joint-lasso row: any filter
that results is a delivery-time width floor with buffer refill, keeping
overlapping substitutes; no joint re-prune. Budget-level readouts (§12, §14)
are not the unit here (rule-level diagnostic, not a list deliverable); the
delivered-list impact of any candidate floor is quoted from the census CSVs
(e.g., a $10 floor touches 156 of 3,214 delivered rules). Frame-relative vs
any-error (§6): the pool is mined on the any-error frame, so the two
coincide for this readout by construction.

**Decision the answer changes.** Whether rule_selection.py gains a
variable-aware width floor (dollar variables only; ratio-boundary intervals
like unc_rawben_rel_max in (0.999, 1] exempt), and where the floor sits.
Per the pre-run review (2026-08-21): NO floor ships from this run alone —
calibrating one additionally requires (a) replication on the second cached
era and (b) a budget-level with/without-floor re-walk across the 49 states
carrying the mandatory companions (within-state median + mean + harmed-tail
count). This run is the diagnostic that decides whether that work is
warranted.
