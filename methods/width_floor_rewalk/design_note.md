# Design note: 49-state with/without narrow-interval-floor re-walk (2026-08-21)

**Question (one sentence).** Does a delivery-time width floor that drops rules
whose narrowest two-sided dollar interval has relative width below f (the
fragile tail identified in section 40) change budget-level list performance
across the 49 states, or is it removal-invariant, the hygiene claim that
would let it ship?

**What varies, with exactly one component varying.** Three arms over the SAME
cached FY2022-23 benchmark pools (methods/v250_benchmark_2024/cache, seed
117), the SAME artifact gates, the SAME shipped fill (pass-zero + fresh-share
walk, f_min 0.50, buffer 3x), the SAME held-out FY2024 refill scoring
(score_2024): the floor level f in {none, 0.02, 0.05}, applied to the blended
pool BEFORE the fill. Ratio-boundary intervals are exempt by construction
(the width test covers the seven dollar-denominated variables only; the
rel_width definition is the section-40 one, reused verbatim). The unfiltered
arm must reproduce the committed benchmark CSV per state x budget to 1e-6
(the threearm consistency-guard discipline); any mismatch stops the run.

**Support after the split (computed before the run).** 49 states x 2 budgets
(5% / 10% of caseload) x 3 arms. Held-out FY2024: 39,528 rows, 4,764 errors
(12.05%); per-state test rows and base rates land in the output CSV. Floor
exposure, measured in section 40's census: the benchmark pool carries 7,037
two-sided dollar intervals, 278 at rel width <= 2% and 725 at <= 5%; the
script prints the per-state count of blend rules each floor removes before
any scoring. Buffer refill replaces dropped rules by construction (capacity
targets are unchanged), so an arm can differ from baseline only through rule
substitution, which is the effect under test.

**What the ledger and findings already say.** Section 40 (settled,
two eras): narrow dollar intervals carry excess held-out decay at matched
train n and a reach collapse (8-36% flag < 10 held-out cases), while the 99%
LCB median margin stays non-negative; the same section's open row: the floor
does NOT ship without this re-walk. Section 38 is the template: the artifact
gate shipped on a removal-invariance readout (median precision change 0.000
at both budgets, zero states worse than -0.05). Section 34 fixed the walk
this study holds constant. The 2026-08-07 companions rule governs the
readout: the within-state MEDIAN decides, the MEAN and the HARMED-TAIL count
(paired change worse than -0.05) must accompany it, and a median win
contradicted by both companions does not ship.

**Pre-registered shipping bar (restated as one-sided non-inferiority per
the 2026-08-21 pre-run review; the earlier absolute-band phrasing would
have blocked shipping on a genuine improvement).** A floor arm ships as
delivery hygiene iff, at BOTH budgets and for BOTH precision and dollar
recall: median paired change >= -0.005, mean >= -0.01, and zero states
harmed (paired change worse than -0.05). The shipping rationale is hygiene
plus non-harm, so an improvement needs no proof and is never CLAIMED from
this run (no pre-set improvement bar; 2 floors x 2 budgets x 2 metrics of
multiplicity): any positive median is logged as a one-era observation only.
Preference order is pre-stated: 0.05 if it clears the bar, else 0.02, else
the floor stays unshipped and the section-40 rows stand as diagnostic only.
Fill gaps (walk cannot reach its capacity target after the drop) are
reported per arm x budget in the summary; a gap is an engineering artifact
to report, not a judged failure of the floor.

**Runtime and scheduling.** Evaluation only, no mining (the threearm
precedent ran 49 states x 3 arms in ~87 minutes mid-day); per the standing
policy, evaluation jobs run anytime.
