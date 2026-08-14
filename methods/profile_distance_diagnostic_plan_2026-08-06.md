# Plan: which rule-dissimilarity measure predicts marginal quality at a review budget?

**Written 2026-08-06, before any distance-outcome number existed. Amended
2026-08-06 after a fresh senior-statistician review of the first draft and
the direction given then, before any result existed; the amendment history is at the
bottom.** Descriptive diagnostic on existing artifacts, no mining, roughly an
hour of compute plus one fresh profile pass.

## The goal (amended)

The delivered lists lose 3 to 4 points of capacity-weighted precision to
residual adverse selection: a rule entering the walk contributes only its
not-yet-covered cases, and those residual slices run worse than the rules'
own precision (section 32: marginal 0.314 vs own 0.345 at the 5% budget).
The loss is concentrated: 57.1% of 5%-budget capacity has fresh share
f = n_new / n_flagged of at least 0.99, where marginal equals own precision
by identity; the entire gap lives on the other ~43% of capacity, where it
runs about 7 points. Outcome-based repair is closed (section 32: deployable
reordering gained +0.000 / +0.011 against an oracle +0.175, because marginal
slices, median 1 to 2 cases, support no statistic).

The remaining route to those points is structural: order or select rules
using DISSIMILARITY measures computed from full-support, outcome-free
quantities, so the walk stops buying thin residual slices of near-duplicate
rules. This diagnostic tests, head to head, which dissimilarity measure (if
any) predicts marginal quality well enough to justify a selection change,
and prices what a state's preference-based reordering (section 31's opening)
would give up or gain.

## The instruments (head-to-head; each through both analyses)

1. **Deploy-time flag overlap / fresh share** (primary candidate). Pairwise:
   1 - |F_i and F_j| / min(|F_i|, |F_j|) on the FY2022-23 training caseload.
   Predecessor form: fresh share computed on the state's FY2024 caseload
   (outcome-free: flags exist before outcomes). Its link to the goal is
   partly arithmetic: at f = 1 marginal equals own precision; at f = 0.8 a
   rule with own precision 0.345 has marginal at least 0.18.
2. **Spectral embedding of the co-firing matrix** (variant of 1). Rule-rule
   Jaccard similarity matrix on training flags, graph Laplacian, spectral
   embedding, distance in the embedded space. Tests whether denoised,
   transitive co-firing beats raw pairwise overlap; section 31's coverage
   convergence (very different rule text, same errors reached) is direct
   evidence the affinity matrix carries strong low-rank structure, which is
   the regime where spectral methods pay. Embedding dimension pre-set to the
   eigengap rule, capped at 10; stated here so it is not tuned to the
   outcome. Stability is judged on embedded DISTANCES, never coordinates
   (rotation-invariant), under the split-half certificate below. The
   embedding's cluster structure (rule families) is reported descriptively
   for the preference-ordering study regardless of the signal verdict.
3. **Naive-Bayes feature divergence.** Each rule represented by the feature
   distribution of its flagged training cases (the mining features, binned
   as in `state_nb_similarity_v2.R`, whose machinery this adapts);
   dissimilarity = mean per-feature Hellinger distance under independence.
   Measures "same kind of household" independent of exact case overlap.
4. **Profile TV distance on section 29 element-group shares** (as originally
   drafted). Train-side (FY2022-23) profiles, total variation distance on
   the 7 element-group share vectors; nature/cause/timing distances
   secondary. Support floor: at least 20 FY2022-23 error cases (excludes
   7.8% of deployed rule-instances, 5.0% of capacity, at the 5% budget).
   The only instrument that doubles as the state-facing preference
   vocabulary, so its cost/benefit must be known regardless of the winner.
5. **Structural signature distance** (comparator with a written null
   expectation). Variables plus split directions, thresholds discarded, the
   section 30/31 machinery. Sections 30-31 already show text churns while
   coverage coincides (signature Jaccard 0.150 at K = 100 against case
   Jaccard 0.656), so the expectation is weak-to-null; a null here retires
   structural distance in writing rather than leaving it ambient.

6. **Consensus distance** (robustness hedge across views). The four
   instrument distance matrices rank-normalized and averaged. Multi-view
   consensus is typically more stable than any single view; it is pre-stated
   here as a candidate rather than constructed after results, so it carries
   no post-hoc selection. It runs through both analyses and the same bars.

**Eligibility gate: the split-half stability certificate.** Before any
signal reading, every instrument's distance matrix is recomputed on two
random halves of the training caseload (for instruments 1-3 and 6; for
instrument 4, two random halves of each rule's error cases; instrument 5 is
deterministic in the rule text and passes trivially). An instrument is
eligible only if the Spearman correlation between its two half-sample
distance matrices is at least 0.70 (median over 5 random splits). An
instrument that cannot reproduce itself under resampling cannot transfer
across years; its signal reading is not computed, and the failure is
reported as the result.

**Deferred, not adopted: supervised similarity** (random-forest proximities,
learned kernels, rank-on-structure). Training a similarity against an
outcome reintroduces the selection-on-outcomes exposure that closed section
32, and doing it honestly requires cross-fitting, which section 30 measured
as not carrying at deliverable scale. Revisited only if the unsupervised
instruments show signal.

## The two analyses

**Analysis 1 - pairwise complementarity.** Pairs are same-stratum rules
deployed on the same state-budget bench list (built FY2022-23, scored
FY2024); a distinct rule pair counts once even if many states deploy it.
Outcome: complementarity on the NATIONAL FY2024 caseload (40,457 rows, 4,803
errors), as 1 minus the overlap coefficient of the two rules' error-catch
sets. Per-state catch sets cannot carry this outcome: the median deployed
(rule, state) pair catches 1 FY2024 error, 40.3% catch zero and 84.2% catch
two or fewer at the 5% budget; nationally the median deployed rule catches
26 (5%) / 45 (10%). Per-state readouts are secondary and descriptive.
Statistic per instrument: Spearman over distinct eligible pairs, against a
permutation null (instrument values shuffled among rules within stratum, 200
draws). The support-controlled partial correlation (both rules' flag counts)
is the quoted effect.

**Analysis 2 - distance to predecessors (co-primary; the goal in its native
form).** For each holdout-deployed rule-instance with at least one
same-stratum predecessor on its list: d_pred = MINIMUM dissimilarity to its
same-stratum predecessors (one close predecessor is what strips a slice);
support-weighted mean as sensitivity. Outcome: realized FY2024 marginal
precision from `methods/marginal_precision_diagnostic/per_rule_marginal.csv`
(k_new, n_new) - never per-rule ratios (median slice 1 to 2 cases); the
quoted number is pooled capacity-weighted marginal precision
sum(k_new)/sum(n_new) by within-(state, budget) tercile of d_pred, top minus
bottom tercile, per budget. Permutation null: shuffle instrument values
among rules within (list, stratum), recompute, 200 draws (preserves rank and
slice structure). Reported on all capacity AND restricted to f < 0.99
capacity, where the problem lives.

## Pre-stated reading rules (per instrument, separately)

- **SIGNAL** requires all of: (i) the Analysis 2 top-minus-bottom tercile
  gap at the 5% budget exceeds its permutation 95th percentile; (ii) the gap
  is positive at the 10% budget; (iii) for instruments 2-5, the gap
  restricted to f < 0.99 capacity also exceeds its permutation 95th
  percentile (instrument 1 is exempt: acting on f is its mechanism, not a
  confound). Analysis 1 is supporting evidence, not a gate.
- **NO SIGNAL** otherwise. If no instrument clears: characterization stays
  descriptive decision support; the preference-ordering study proceeds on
  state preference alone, with the measured price of preference reordering
  reported and no recall or precision claim; the ledger row closes at
  public-data scale, open at internal-data scale only.
- **The no-arm gate.** SIGNAL does not create a study arm. The consequence
  is a stage-2 retrospective re-walk on the section 32 harness (same rules,
  same capacity, LCB order with the winning instrument as tie-break or
  diversity term, scored on FY2024), minutes of compute, judged on the
  section 30 bar: +0.010 within-state median at the 5% budget across 49
  states. Only past that bar does any precision claim attach. Detection
  arithmetic, stated in advance: the plausible transferable effect is 0 to
  2pp against an SE of roughly 0.008 on the 49-state median (per-state
  binomial SE 0.068 on ~44 flags; ordering-perturbation sd 0.0443, section
  30); the recoverable ceiling is the 3-4pp of section 32, of which only
  the f < 0.99 capacity is addressable.
- If instruments disagree, the quoted winner is the one clearing (i)-(iii)
  with the largest f < 0.99 gap (instrument 1 judged on its all-capacity
  gap); ties break toward the cheaper instrument. A winner chosen among five
  candidates carries selection multiplicity; the stage-2 bar, on an outcome
  none of the instruments touched, is the guard.

## Confound controls and caveats (carried from the review)

- Cross-stratum pairs excluded everywhere (disjoint by construction).
- Support/breadth controls as stated; raw-clears-but-partial-fails is NO.
- Pairwise TV noise floor at the deployed median support (~88 train
  variances) is ~0.135 against a distinctive spread of ~0.26, so Analysis 1
  is also reported for pairs where both rules have at least 50 train error
  cases (attenuation check).
- Nature-group distances are secondary and carry the FY2024 codebook break
  (codes 56/57/33/58 new in FY2024, section 29); element groups, the
  primary, are unaffected.
- Analysis 2's outcome is the same FY2024 data for every instrument; no
  instrument sees any outcome in its construction.

## Constraints

Outputs to `methods/profile_distance_diagnostic/`; read-only on
`state_delivery_lists/` and the bench directory; no CHANGELOG, no version
bump. Routing rule applies: written under the principal-data-scientist
framing, fresh senior-statistician review before the run. Assertion anchor:
per-rule FY2024 flag and catch accounting must reconcile with
`methods/marginal_precision_diagnostic/per_rule_marginal.csv`; profile
machinery reconciles against `methods/rule_error_profiles/` conventions.

## Amendment history

- 2026-08-06 first draft: single instrument (profile TV distance), pairwise
  analysis only, per-state outcome.
- 2026-08-06 amended before any result existed, per the fresh
  senior-statistician review (required: national-caseload outcome, since
  per-state catch sets are 0-2 errors for 84% of deployed rules;
  distance-to-predecessors co-primary; head-to-head of instruments; the
  no-arm gate with detection arithmetic) and per direction (goal
  restated on the 43%-of-capacity problem; instrument menu widened to
  spectral co-firing, naive-Bayes feature divergence, and an explicit
  deferral of supervised similarity).
- 2026-08-06 second amendment, still before any result existed, per the
  robustness direction: the split-half stability certificate as an
  eligibility gate for every instrument; spectral stability judged on
  distances (rotation-invariant) with rule families reported descriptively;
  the consensus distance added as a pre-stated sixth instrument.
