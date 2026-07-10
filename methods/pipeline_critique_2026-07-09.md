# Conceptual critique of the v2 rule-mining pipeline

**Date:** 2026-07-09
**Scope:** conceptual/statistical design only, judged against the project's own goals
(interpretable, individually defensible rules; per-rule guarantees that survive a year
of drift; deliberate redundancy; graceful degradation for small states). Grounded in:
CLAUDE.md, modeling_findings.md, design_drop_pre_architecture.md,
effects_of_munging_options.md, rule_mining_helpers.R, the INCL/EXCL v2 drivers,
state_threshold_gridsearch_v2.R, neighbor_transfer_benchmark_v2.R,
budgeted_transfer_menu_v2.R, and the three similarity scripts.

---

## 1. What is sound

These I would defend before a methods committee without hedging:

1. **The winner's-curse diagnosis before the fix.** The 2026-07-04 analysis
   (modeling_findings §1; design doc "Empirical findings") did the thing most teams
   skip: it established that *unselected* high-support rules are essentially unbiased
   (median train−holdout gap −0.003, r = 0.83) and that the decay is symmetric
   (holdout-selected rules regress on train too). That isolates selection noise as the
   mechanism and rules out model overfit and drift as primary causes. The fix
   (threshold the Wilson LCB, not the point estimate) is then the right *kind* of fix,
   and it was validated as a better *ranking* (12.8% vs 8.2% recall at matched deployed
   precision), not just a better number.

2. **Refusing the joint lasso.** Given that states veto rules one at a time and want
   substitutes, per-rule marginal evaluation with deliberate redundancy is the correct
   decision-theoretic reading of the deployment context. A sparse joint fit would
   optimize a portfolio the state is going to edit, and its coefficients are
   uninterpretable as per-rule promises. This is a case where "worse ML" is better
   statistics for the actual loss function.

3. **Union counting done honestly.** `precision_sweep` counts an error once no matter
   how many rules catch it; workload is the union's size. No self-flattery through
   double-counting, in either the inclusion or exclusion direction.

4. **The dominance-dedup logic itself.** "Drop A only if a same-shape superset B has an
   equal-or-better *selection statistic*" is exactly the condition under which A can
   never add recall at any floor of that statistic. Because the dropped-rule criterion
   uses the same statistic the sweep thresholds on, the pruning is provably harmless to
   the sweep (modulo the eps tolerance, which is documented). Keeping cross-structure
   overlap while pruning within-structure clutter matches the redundancy goal.

5. **Any-error scoring.** Recognizing that frame-relative precision understates
   deployed precision ~2x and always reporting the any-error view (findings §6) is the
   right correction of estimand — though see V1 below: it is only half-finished.

6. **Empirical humility at state scale.** The Virginia lesson (LCB alone collapses at
   n ≥ 5; hard n ≥ 30 floor makes deflation gentle), the Louisiana era-match finding
   (mixed-era pooling collapsed; same-era neighbors worked), and the refusal to rank
   neighbor rules by target-test performance ("a fresh winner's curse") are all
   correct, hard-won calls. The last one in particular shows the team understands that
   selection bias re-enters at every stage where a choice touches the evaluation data.

7. **The year-swap replication of the typed-vs-combined finding** (train 2022+2023,
   test 2024, ordering and magnitudes reproduced) is the model for how every headline
   claim should eventually be graded. It exists for one finding; it should exist for
   the production configuration as a whole (V2).

8. **Frame provenance discipline.** Saving `reg_model_data.rds` from the munging script
   after the stale-frame incident, archiving superseded runs, and diffing rule content
   across data revisions (`compare_rule_sets_v2.R`) is reproducibility done properly.

---

## 2. Principal vulnerabilities (ordered by potential impact)

### V1. The guarantee a state hears is not the guarantee the pipeline computes

**The construction at risk.** The per-rule promise is "this rule's precision is at
least X." The pipeline's X is the Wilson LCB of **frame-relative train precision**
(train = 2022+2024; frame = one error type vs no_error). The number quoted to states is
**any-error deployed precision**, which is only computed on the 2023 holdout
(`universe` in the INCL driver is filtered to HOLDOUT_YEARS; `eval_rules_on(..., "any")`
has no train counterpart). So the guarantee attaches to one estimand and the sales
pitch to another, connected only by the empirical observation that any-error runs
~2–2.7x frame-relative.

**How it fails silently.** The 2x multiplier is an average over rules and frames. For a
specific rule whose flags happen to co-occur with other error types less than average,
the deployed precision can sit *below* what "LCB ≥ 0.20, and any-error runs 2x" implies
— and no per-rule bound was ever computed on the deployed estimand, so nothing in the
CSV warns the analyst. Separately, the guarantee is a **median-calibrated** claim, not a
per-rule tail claim: findings §1 reports that LCB floors *underpromise* (LCB 0.30 →
0.381 delivered) — but those are union/median numbers. With 11,018 shortlisted rules
post-rebuild, a 1% per-rule error rate at the boundary permits on the order of a
hundred rules whose true precision is below their floor, concentrated exactly where a
state analyst browsing the CSV can pick them.

**Evidence in the repo.** Findings §6 (the 2x multiplier and its range 2–2.7x);
effects_of_munging_options (shortlist now 11k rules); the INCL driver (no train
any-error LCB exists).

**The test that settles it.** Compute the Wilson LCB of **any-error precision on the
train years' universe** for every rule and (a) report it as a column, (b) trial it as
the shortlist statistic. This is a small code change — the condition-index evaluator
already runs on the universe; it just needs the train slice. Then report per-rule
**tail calibration** on holdout: the share of shortlisted rules whose holdout any-error
precision falls below their quoted floor, not just the median. If that share is small
(say < 5–10%) the current promise is defensible with a one-line caveat; if not, the
guarantee language must change.

### V2. 2023 has been the judge for every design decision — and 2023 is an interpolated year

**The construction at risk.** Train 2022+2024, test 2023 adjudicated: the engine pair,
z = 2.326, the strata scheme, dedup eps, floors, the typed+pooled combination, the
state qualification mode (hybrid c), and the "tune if ~30+ rules qualify" rule of
thumb. Two distinct problems ride on this:

1. **Adaptive reuse.** Each decision was a comparison of 2023 curves, most decided by
   ≤ 1pp margins that the findings doc itself calls "within the noise band" (§9's
   criterion comparison; §4's plateau claims). Ten-plus adaptive decisions at ≤ 1pp
   each can accumulate 1–3pp of optimism in the final quoted operating points — the
   same order as the differences the studies claim to detect. This is the "garden of
   forking paths" (many analyst choices, each conditioned on the same test data): no
   single decision is damning; the accumulation is.
2. **Interpolation flattery.** 2023 sits *between* the training years. Deployment is
   extrapolation to FY25+. The findings doc flags this exact bias for state own-mining
   ("temporal interpolation flatters all options and own-mining most," §9) but the
   national pipeline's headline numbers carry the same flattery, unquantified. The
   design doc's own era check shows dollar-scaled features age (only 645/1,403 rules
   still fire ≥ 10 times in 2018–19).

**How it fails silently.** A state deploys at "precision 0.24 @ 55% dollar recall" and
realizes 0.19 @ 45% in FY25. Nothing broke; the quoted point was a max over
configurations evaluated on a flattering year.

**Evidence in the repo.** The one place this was tested — the year-swap replication of
typed-vs-combined (§3) — the finding *held*, which is genuinely reassuring and is
evidence the ordering-level conclusions are robust. But the *levels* (the numbers
quoted to states) have never been produced by a frozen configuration on a year that
didn't help choose the configuration.

**The test that settles it.** The cheapest strong guard: freeze the production config
exactly as documented in CLAUDE.md, run INCL and EXCL once with train 2022+2023 → test
2024 (extrapolation direction), and commit to quoting those numbers — no re-tuning
permitted on the result. Cost: two pipeline runs (~hours). When FY25 data arrives,
repeat once, pre-registered (write the expected numbers down first). If the 2024-out
levels sit within ~1–2pp of the 2023 levels, the adaptive-reuse fear is largely
retired; if not, the gap *is* the correction to apply to state-facing quotes.

### V3. A fixed z is not a pool-adaptive multiplicity control — "mine big, filter stringently" is an empirical calibration, not a theorem

**The construction at risk.** One-sided 99% Wilson per rule, screened over 100k+
candidates. The implicit claim: a stiffer per-rule bound compensates for a bigger pool.
The z-sweep (§5) showed that at *this* pool size, z = 2.326 recovers the frontier that
z = 1.28 achieves on a pool 10x smaller. That is a two-point empirical calibration of z
against pool size — with the calibration judged on 2023 (so it is also inside V2's
blast radius). There is no mechanism by which the bound *knows* the pool size: the same
z would be applied to a 10k-rule pool and a 1M-rule pool, and only one of those can be
right.

**Why the per-rule framing undersells the problem.** The LCB gate is a 1%-level test of
H0: precision ≤ floor, per rule. It is neither familywise control nor false-discovery
control. Concretely, at MIN_TRAIN_FLAGGED = 10, a rule flagging 10 cases, all errors,
gets LCB = 0.649 — it clears a 0.60 floor. Tree ensembles are adaptive error-seekers:
they manufacture thousands of small pockets with true precision ~0.3–0.4, and with
enough of them, some go 10-for-10 and enter the shortlist with a 25pp overclaim.
National-scale support distributions have so far kept this tail small (calibration
held on the rebuilt frame — effects doc, "Calibration is unchanged"), but the repo
already contains the counterexample for when the regime shifts: **Virginia** (LCB-only
selection at n ≥ 5 → median holdout precision 0.000). The fix there was a hard n ≥ 30
floor — i.e., the LCB was rescued by a crude support cliff, which is the tool the
design doc said the LCB was meant to replace. That is the tell that the control is not
self-adjusting.

**How it fails silently.** Any change that grows the pool or thins per-rule support —
more features, deeper trees, the next data rebuild (the 2026-07-07 rebuild already 3x'd
the shortlist), per-state mining — silently shifts the boundary contamination rate
while z stays 2.326 and the reported "99%" keeps its reassuring label.

**What adopting an adaptive method would concretely change.**
- **Empirical Bayes shrinkage** (fit a beta prior to the precision distribution of the
  *full unselected* candidate table per frame/stratum; shortlist on the posterior lower
  quantile): one prior fit per frame, then a drop-in replacement for `wilson_lcb`. The
  bound automatically stiffens when the pool is junk-heavy and relaxes when it is not,
  and it fixes the small-n tail (a 10/10 rule gets shrunk toward the pool mean, not
  credited 0.649). This is the natural fit here because the repo already showed
  unselected precision is nearly unbiased — the prior is estimable from exactly that
  table.
- **BH/FDR framing** (binomial p-values against H0: precision ≤ floor, Benjamini–
  Hochberg at q): changes the deliverable's guarantee to "at most q% of the rules on
  this list have true precision below the floor" — which is arguably the honest
  statement for a *list* product, and it scales with list size by construction.

**The test that settles it.** On the V2 extrapolation run (2022+2023 → 2024), compare
tail calibration (share of shortlisted rules below floor on the out-year) for fixed-z
vs EB-posterior vs BH shortlists, at matched shortlist size. Cost: a day on top of V2's
runs. If fixed-z's tail is no worse, acquit it at national scale and document the
support-regime boundary (the Virginia n ≥ 30 lesson) as a hard precondition.

### V4. Exclusion: the standard is weakest exactly where the money is, the failure mode is silent, and the stringency asymmetry runs backwards

Three related indictments of the EXCL design; the machinery itself (inverted target,
union counting, higher support floor) is fine.

**(a) The relative standard concentrates dollar risk in the high-error strata.**
floor_h = 1 − base_err_h / 5 means the excluded pocket may carry up to 1.5% errors in
the size-1 stratum (base 7.7%) but up to ~4% in the 4+ stratum (base 20.4%). "5x safer
than the pile average" is a defensible *triage* claim, but the absolute leakage the
state eats is 2.7x higher in the stratum where benefits — and therefore dollars per
leaked error — are largest. A state that hears "excluded cases are 5x safer" will not
infer that its dollar exposure is concentrated in large households. If the loss that
matters is unreviewed error *dollars*, the floor should be stated in dollars: e.g., a
per-stratum bound on expected error dollars per excluded case, or a uniform absolute
clean-rate floor with the honest admission that the 4+ stratum may then offer no
exclusions. The relative standard was chosen because the absolute one "is only
reachable in the low-error size-1 stratum" (EXCL driver comment) — that is a reason it
is *convenient*, not a reason it is *right*.

**(b) The stringency asymmetry is backwards for the loss structure.** INCL filters at
99%, EXCL at 95%, justified by "clean pools are large, so the bound is already tight."
But if the bound is already tight, raising z to 2.326 costs almost nothing — the
justification argues the choice is immaterial, not that the weaker setting is
appropriate. The loss asymmetry argues the other way: an inclusion mistake costs one
wasted review and is *self-revealing* (the review finds nothing); an exclusion mistake
is an unreviewed error that **no operational feedback loop will ever surface** —
excluded cases are, by construction, not looked at. The one audit channel that exists
is the QC sample itself (drawn independently of review flags), and nothing in the repo
proposes using it as a monitor.

**(c) The guarantee is void for the invisible error class.** "Clean" means no
over-threshold error *in the public frame*, which misses all ineligible-determination
cases — 100%-of-benefit errors — and visibility runs 43–81% by state (findings §10).
For inclusion, invisibility just means missed recall. For exclusion it means the
clean-rate guarantee is computed on the wrong population: a rule can be certified
"≤ 1/5 of base error density" while excluding a pocket enriched in the very cases that
would have been found ineligible. The §10 guidance ("below ~60% visibility, treat
national rules as a supplement") is written for inclusion; exclusion needs the stronger
form: **do not deploy public-data exclusion rules in low-visibility states at all**, or
re-derive them on internal data that contains ineligible determinations.

**The tests that settle it.** (a) Recompute the EXCL sweep with a dollar-based floor
and compare frontiers — an afternoon. (b) Rerun EXCL at z = 2.326; if outputs barely
move (as the "already tight" argument predicts), adopt it and the asymmetry critique
dissolves. (c) Using state_error_accounting, bound each stratum's worst-case invisible
leakage under current visibility rates and print it in the exclusion deliverable.
Additionally: specify a monitoring rule — each year's new QC sample, intersected with
the exclusion union, re-estimates the deployed clean rate; alarm if the LCB of that
estimate falls below the floor.

### V5. Union numbers carry a second-level winner's curse, per-rule guarantees do not compose upward, and the CSV invites a third curse at the consumer end

**(a) Composition failure (structural, not empirical).** Per-rule precision floors do
not imply the same floor for the union: two rules each at 0.50 precision that share
their true positives but not their false positives union to 0.33. The pipeline never
claims otherwise — `precision_sweep` measures the union directly on holdout — but the
deliverable pairing ("every rule has LCB ≥ 0.20" + "the union achieved 0.24") can read
to a state like the first fact supports the second. It does not; only the holdout
measurement does. Worth one explicit sentence in every state-facing artifact.

**(b) Second-level selection.** The operating points quoted from the sweep are chosen
*by looking at the holdout sweep* — the best floor, the best configuration, the best
frame combination. Each sweep point is a high-n estimate (unions flag thousands of
cases), so the inflation per choice is small, but it is systematic and it compounds
with V2. The union numbers have no confidence intervals anywhere in the outputs; at
minimum, a Wilson interval on each sweep row's precision (the union's k/n supports it
directly) would show how much of the floor-to-floor structure is noise.

**(c) The consumer-end curse.** The `*_rules_all.csv` and shortlist artifacts expose
per-rule holdout precision. A state analyst assembling their menu will, naturally,
prefer rules with high *holdout* columns — re-selecting on the noisy statistic and
re-inflating expectations, the precise mistake the pipeline architecture exists to
prevent (and which the LA deliverable explicitly avoided by ranking on neighbor-train
precision). The deliverable design should rank and headline the *guarantee* statistic
(train LCB), demote holdout columns to an appendix or aggregate them, and say why.

**The test that settles (b).** Nested in V2's frozen 2024-out run: compare the quoted
2023 operating points to the same config's 2024 points. The gap measures the combined
second-level curse + drift. Cost: already paid by V2.

### V6. The transfer benchmark answers "cross-state, same era" but will be read as "next year in your state" — and the similarity-metric menu is a fresh selection layer

**The construction at risk.** Neighbor pools train on 2022–24, targets are scored on
2022–24 (different states). The LOO national control closes the known target-in-pool
leak (good). The residual channel is not case-level leakage but **shared era shocks**:
FY22–24 spans the pandemic-benefit unwinding (emergency allotments ended nationally
Feb–Mar 2023), the Thrifty Food Plan revaluation, and historically elevated national
error rates. Rules encoding unwinding-era income/benefit geometry will transfer
beautifully across states *within* that era and are exactly the rules most likely to
age out by FY25–26. The repo's own evidence cuts both ways: the era-match finding
("mixed-era pooling collapsed," §9) proves rules are era-specific — which validates
the benchmark's design *and* proves its numbers are a within-era ceiling, not a
prospective estimate. The LA quote discipline ("FY25 adds temporal drift on top, so
quote below-0.18 expectations") shows awareness; it should be a structural feature of
the benchmark, not a manual caveat.

**Similarity-metric selection.** Five similarity definitions × 12 targets × 3 floors is
a 180-cell grid from which it will be tempting to report each target's best transfer.
Choosing the metric *per target* on target performance is a winner's curse over
metrics; the defensible choice is one metric (or a fixed blend) selected on
*average* performance across targets, then applied uniformly. Relatedly, top-5-by-
cosine is a hard cutoff on a noisy, likely flat similarity profile; similarity-
*weighted* pooling of all states (case weights ∝ similarity) is the principled version
but complicates support/LCB semantics (effective sample size). The cheap middle
ground: show results are flat in K ∈ {3, 5, 10}. If they are, the cutoff is harmless
and acquitted.

**The test that settles it.** A time-shifted variant: neighbors mined on 2022–23,
target scored on its own 2024. One extra pass over cached machinery per target. The
gap between same-era and time-shifted transfer is the honest deflation factor to quote
alongside any transfer number.

### V7. Ladder collapse keeps the max of correlated LCBs — the kept rung's guarantee is mildly but systematically inflated

**The construction at risk.** `dedup_dominated` (eps ladder walk) and especially
`collapse_ladders` keep, within each same-structure family, the rung with the highest
train LCB. The max of k correlated estimates is biased upward relative to the chosen
rung's own true value — a within-family winner's curse. Because rungs are nested and
highly correlated, the inflation is far smaller than for independent picks, and the
LCB's small-n penalty pushes against tight rungs; but families born of continuous
variables can carry dozens of rungs (tree cutpoint jitter surviving 3-digit rounding),
and for those the kept rung's "LCB ≥ 0.20" is weaker than nominal. Note the two dedup
layers push in *opposite* directions — the eps-dominance walk preferentially keeps
looser (higher-n, more honestly bounded) rungs, while `collapse_ladders` then takes the
family max — so the net effect is genuinely unknown, not just small.

**How it fails silently.** The shortlist's worst-calibrated entries cluster among
heavily-laddered families, i.e., rules on the most informative continuous variables —
the ones states are most likely to adopt.

**The test that settles it.** Entirely from existing artifacts: reconstruct family
signatures in `*_rules_all.csv`, bin shortlisted rules by family size (1, 2–4, 5+), and
compare (holdout precision − train LCB) across bins. If the 5+ bin runs materially more
negative, apply a family-size-aware correction (e.g., select the rung on the EB
posterior from V3, which shrinks harder in bigger families, or pick the rung on a
train split). An afternoon of work; likely verdict is "real but small" — but it should
be measured, not presumed.

### V8. Munging choices with modeling consequences beyond row counts

**(a) Zero-filled deductions create a state-recording artifact channel.** For WA/MS/MN
(and any state recording optional deductions "in blocks"), zero-fill makes
non-recording states' cases look like zero-deduction cases. Any rule conditioning on
deduction variables (`total_deductions_by_hh_size`, `medical_deductions`, …) then fires
on a mixture of true zero-deduction households and recording artifacts, and its
national precision partially reflects *which states don't record*, not household risk.
Deployed inside such a state on internal data — where deductions ARE recorded — the
rule's fire pattern shifts and the national guarantee silently detaches.
`ded_fields_imputed` exists but is only a row flag. **Test (cheap):** for every
shortlisted rule using a deduction variable, report the share of its train flags with
`ded_fields_imputed = 1`; quarantine or annotate rules where imputed rows dominate.

**(b) Mine-on-complete, deploy-on-NA-is-FALSE asymmetry.** Ensembles fit on
`complete.cases` rows, but the flag evaluator treats NA conditions as FALSE. So rules
are learned on a cleaner population than they are scored and deployed on; cases with
missing features can never be flagged (inclusion: invisible recall loss concentrated
in missingness-prone states) and never excluded (conservative — fine). Also
RENT/UTIL-missing rows are still dropped entirely: those cases exist in deployment
piles and are covered by no reported metric. One paragraph of quantification (share of
each state's caseload unreachable by any rule due to NA) would close this.

**(c) Design weights are ignored.** Precision/recall/dollar metrics are unweighted
across cases; QC samples similar counts per state regardless of caseload, so national
metrics are roughly state-equal averages, not caseload averages (and FYWGT is used in
state_error_accounting but not in the mining metrics). For per-state deliverables this
is moot; for national headline numbers it modestly misstates the caseload-level
estimand. Acquit for state work; note it whenever a "national" precision is quoted.

**(d) `second_element_i` excluded as a feature: acquitted.** Reporting inconsistency
makes it leakage-adjacent; the effects doc's check that new rules are not multi-element
specialists (34% vs 32% base) settles the modeling question.

### V9. Budgeted greedy union: mostly acquitted, two footnotes

Adding rules in descending pool-train-LCB order under a workload cap is honest (target
evaluation never informs ordering) and makes the per-rule guarantee the ordering
principle, which fits the product. Two footnotes, not indictments:

1. **LCB order ≠ marginal value order.** LCB ranks rules by standalone precision
   (favoring large-n rules at a given raw precision); it ignores overlap with rules
   already admitted. A lazy-greedy ordering on *marginal* pool-train precision (new
   errors per new flag, computed on pool data only — no leakage) would plausibly buy a
   few points of dollar recall at fixed budget. Worth one comparison run on the cached
   pools; if the gap is < 2pp, keep the simpler LCB order for its explainability.
2. **Knapsack instability.** The skip-if-oversized rule means small budget changes can
   produce structurally different menus (a big rule squeaks in or doesn't, reshuffling
   everything after it). Statistically valid; operationally worth knowing when two
   similar states get dissimilar menus.

### V10. State-level tuning and canonicalization: largely acquitted, one thin spot

- **3-significant-digit rounding:** acquitted. Stats are recomputed after rounding
  (never stale), and collapsing near-identical cutpoints *reduces* effective
  multiplicity before screening. The cost is nil at these sample sizes.
- **State grid search:** the partition-aware variant dedup (only keep cuts that induce
  distinct partitions of observed state values) is a genuinely nice touch — it removes
  vacuous multiplicity rather than penalizing it. Variant selection maximizes train
  dollars among qualifiers and is honestly labeled as optimistic on train, with holdout
  as judge. The residual exposure is V2's: the tuned-vs-national verdicts were read off
  the interpolated 2023 year.
- **The thin spot: the "~30+ qualifying rules" two-regime cutoff** rests on seven
  states' single-year outcomes with a plausible mechanism but a loosely identified
  threshold. Before it hardens into deployment doctrine, re-derive it on the 2024-out
  split (piggybacks on V2). If the regime boundary moves a lot, quote it as a soft
  range, not a rule.

---

## 3. What I would do next

Ranked by information per unit cost. Items 1–2 share compute.

1. **Frozen-config extrapolation run (kills or quantifies V2, feeds V3, V5, V10).**
   Freeze the production INCL + EXCL configuration exactly as documented; run train
   2022+2023 → test 2024; write down expected numbers *first*; compare levels, not just
   orderings. Claim tested: "the quoted operating points survive a non-interpolated,
   non-adaptively-reused year within ~1–2pp." Cost: two pipeline runs plus a comparison
   script that mostly exists (`compare_rule_sets_v2.R` pattern). When FY25 lands,
   repeat once, pre-registered — that becomes the permanent guard: every new year is
   judged exactly once by the frozen config before any tuning touches it.

2. **Per-rule tail-calibration audit + adaptive-bound bake-off (V1, V3, V7).** On the
   run from (1): (a) report the share of shortlisted rules whose out-year any-error
   precision falls below the quoted floor, overall and by support bin and by
   ladder-family size; (b) compare fixed-z vs empirical-Bayes beta-binomial posterior
   quantile vs BH-at-q shortlists at matched size. Claim tested: "the fixed-z per-rule
   guarantee holds in the tail, not just the median, at current pool scale — or the EB
   bound fixes where it doesn't." Cost: ~a day; the EB prior is one `optim` call on the
   existing unselected candidate table.

3. **Align the guarantee estimand with the deployment estimand (V1).** Add the train-
   universe any-error Wilson LCB per rule; make it the shortlist statistic (or at least
   the headline column states see); restructure state-facing CSVs to rank on the
   guarantee statistic and demote per-rule holdout columns. Claim tested: "the number a
   state reads as 'at least X' is a bound on the quantity they will experience." Cost:
   small helper change + rerun; no new methodology.

4. **Exclusion hardening (V4).** Rerun EXCL at z = 2.326 (if outputs barely move,
   adopt); add a dollar-based per-stratum floor variant to the sweep and compare
   frontiers; compute worst-case invisible-error leakage by state from
   state_error_accounting and print it in the deliverable; specify the QC-sample
   monitoring rule (annual re-estimate of deployed clean rate on the intersection of
   the new QC draw with the exclusion union, alarm on LCB breach). Claim tested: "the
   skip decision's guarantee is stated in the units of its loss (dollars), holds where
   errors are richest, and cannot fail silently for more than one QC cycle."
   Cost: 1–2 days, no new mining.

5. **Time-shifted transfer variant (V6).** Neighbors mined on 2022–23, target scored on
   its own 2024; one fixed similarity metric chosen on cross-target average. Claim
   tested: "neighbor transfer helps a thin state *prospectively*, not just
   contemporaneously, and by how much less." Cost: one benchmark rerun over cached
   machinery; K-sensitivity (3/5/10) rides along free.

---

*Summary judgment: the pipeline's core insight — per-rule marginal evaluation with a
shrunken selection statistic, redundancy as a feature, unions measured honestly — is
right for the product, and the team's diagnostic habits are unusually good. The
exposure is concentrated in (i) a guarantee whose estimand, tail behavior, and
pool-size dependence are all softer than the "99% lower bound" label implies, (ii) one
holdout year that has both judged every decision and flattered every level, and (iii)
an exclusion product whose failure mode is silent and whose standard is loosest where
the dollars are. All three are testable for roughly two pipeline runs and a week of
analysis.*
