# Design: dropping {pre} from the rule-mining pipeline

Status: draft for discussion (written 2026-07-04, while the RULE_SOURCE="all" run
was in progress). Goal: replace pre() with direct tree-ensemble rule extraction,
robust structure-aware deduplication, and the train-precision threshold sweep as
the single evaluation concept. No lasso, no nets.

## Objective (the one metric that matters)

**Total achievable hold-out recall from the union of all rules whose TRAIN
precision clears a floor**, reported across a grid of floors. Errors caught by
several rules count once (union); individual rules are never penalized for
overlapping. Everything below serves that metric.

## Architecture: one shared helpers file + thin driver scripts

All logic lives in `rule_mining_helpers.R`; every script (INCL, EXCL, parameter
tuning, strata comparison) becomes a short driver: config block, data prep,
calls into the helpers, outputs. This is what makes the system readable — the
pipeline is visible in ~60 lines per script, and a fix lands everywhere at once.

Pipeline stages (each a helper function):

```
generate  ->  canonicalize  ->  dedup  ->  evaluate  ->  sweep / shortlist
```

### 1. generate_rules(df, target, engine, params) -> tibble(rule)

Engines, each ~40 lines walking a node table into "var <= thr & var2 > thr2"
strings:

- **ranger** (preferred forest engine — fast, maintained): fit with
  `max.depth` 3-4, configurable `mtry` (mtry = 1 for the diversity variant);
  walk `ranger::treeInfo(fit, t)` root-to-node per tree. Gotcha: factor splits
  are encoded as level bitmasks and need decoding (most current features are
  numeric/logical, so a small shim).
- **xgboost** (boosted engine, replaces pre's gradient-boosted rpart): depth
  3-4, low eta, subsample ~0.25 mirrors current settings; walk
  `xgb.model.dt.tree()`. The repo's xrf script proves this path works on this
  data. Alternative considered: hand-rolled gradient-boosted rpart (no new
  dependency, ~30 lines) — keep as fallback if xgboost's rule vocabulary
  (it bins on floats) annoys us.

Every internal node contributes a rule (as pre does), not just leaves.

### 2. canonicalize_rule(rule) -> canonical text

Builds on the existing `.parse_cond`/`simplify_rule` code in the INCL script:

- parse into (variable, op, threshold) conditions;
- collapse repeated same-variable same-direction bounds to the binding one;
- sort conditions by variable name so identical rules print identically;
- **optional threshold rounding** (e.g., 3 significant digits, config
  `THRESHOLD_SIGNIF`): trees emit midpoints like 0.34785; rounding makes rules
  presentable to states and collapses near-identical cutpoints. Precision is
  re-measured after rounding, so no reported number is ever stale.

### 3. Deduplication — three layers, in order

a. **Exact text** after canonicalization (free).

b. **Exact coverage**: two rules flagging the *same training cases* are one
   rule (this is pre's removeduplicates, done our way). We already compute
   sparse flag index vectors; identical vectors -> keep the rule with fewer
   conditions. Complement removal is unnecessary: an EXCLUDE-direction
   complement fails the INCLUDE screen on its own.

c. **Same-structure dominance** (the new piece, per the design ask): group rules
   by *signature* = the set of (variable, direction) pairs plus any categorical
   conditions. Within a signature family, members differ only in thresholds,
   so coverage is nested (or partially ordered for multi-bound rules).
   **Drop rule A if some same-family rule B is looser-or-equal on every bound
   (a coverage superset) and has train precision >= A's.** Rationale: at any
   precision floor where A qualifies, B also qualifies and B's coverage
   contains A's — A can never add recall to any union. What survives is the
   family's precision ladder: tighter/higher-precision members (matter at high
   floors) and looser/lower-precision members (add recall at low floors).
   This prunes pointless subset/superset variants while preserving the
   redundancy we want ACROSS different variable combinations — states still
   get substitutes, just not ten thresholds of the same rule.
   Implementation: the INCL script's `.rule_struct`/`.is_superset` already do
   the structure matching; add the precision comparison.

### 4. evaluate: sparse flags via a condition matrix

Rules repeat the same conditions constantly (thousands of rules, few hundred
unique conditions). So: evaluate each *unique condition* once per dataset into
an indicator, then AND the relevant columns per rule. This replaces per-rule
`eval(parse(...))` — orders of magnitude fewer parses — and keeps memory flat
(store flag *index* vectors, as in the current sparse rewrite). Filters:
`MIN_TRAIN_FLAGGED` (currently 10) and the direction screen (INCLUDE: precision
above stratum base rate; EXCLUDE: clean-rate above base clean-rate).

### 5. sweep + shortlist (already built)

The descending-threshold single-pass union sweep and the per-rule
train/holdout table from `compare_optimal_vs_plus_RF_mtry1_rules.R` move into
the helpers unchanged. Outputs per stratum and pooled: sweep curve CSV + plot,
per-rule evaluation CSV, high-precision shortlist CSV.

## Exclusion scripts: same machinery, inverted target

Exclusion = flag SAFE cases. Reuse everything with `target = is_clean`:
"precision" becomes share-of-flagged-that-are-clean (or dollar retention under
OBJECTIVE = "dollars"), union recall becomes workload cut. Report holdout
workload cut and error dollars lost at each train clean-rate floor
(RETAIN_FLOOR maps onto the sweep grid). One code path, two framings.

## Script conversion plan

| Script | Change |
|---|---|
| `INCL_find_inclusion_rules_multi_model_by_hh_size.R` | rewrite as thin driver (per error type x stratum: generate ranger + xgboost, dedup, evaluate, sweep, shortlist). c50/xrf variants likely retire — xgboost engine subsumes xrf; C50 can become an engine later if wanted. |
| `EXCL_find_exclusion_rules_by_hh_size.R` | same driver with inverted target |
| `optimize_rulefit_params.R` (methods/parameter_tuning/) | sweeps engine params (ranger: num.trees, max.depth, mtry, min.node.size; xgboost: eta, nrounds, depth) and compares sweep curves instead of net PR curves |
| `compare_hh_size_strata_schemes_model_performance.R`, `compare_HHsplit_vs_separate_ESAP_model.R` | swap `mine_rules()` internals for `generate_rules()`; the sweep evaluation is already in place |
| `compare_optimal_vs_plus_RF_mtry1_rules.R` | becomes an engine comparison (xgboost-only vs xgboost+ranger-mtry1) on the new stack |
| `INCL/EXCL_optimize_*` grid-search scripts | mostly untouched — they consume rule CSVs; minor column-name updates |

Old pre()-based scripts move to `archive/` rather than deleted, so results
remain reproducible.

## What we deliberately keep from pre's ideas

- coverage-based dedup (layer b) — the genuinely useful part;
- rule generation from *every* tree node, shallow depth, subsampling;
- winsorizing/normalization/lasso/nets: dropped (linear terms unused; lasso
  optimizes prediction fit, not per-rule precision; nets replaced by the sweep).

## Expected wins

- Memory: a few GB even with 100k+ raw rules (no rules-by-rows lasso matrix).
- Speed: ranger + condition-matrix evaluation; no cv.glmnet.
- Readability: drivers are short; the pipeline is five named stages.
- Rules stay state-presentable (canonical ordering + threshold rounding).

## Empirical findings from the 2026-07-04 run (earned income, 71,208 rules)

Diagnosis of the train (>=0.20) -> holdout (~0.10 median) precision decay:

- **Winner's curse confirmed as the dominant mechanism.** Among high-support
  rules with NO selection applied (n = 53,508), train precision is essentially
  unbiased for holdout precision (median holdout - train = -0.003) and strongly
  correlated (r = 0.83). The decay appears only in the selected tail, and it is
  symmetric: rules selected on HOLDOUT >= 0.20 have median TRAIN precision
  0.116 — classic bidirectional regression to the mean.
- **Mining optimism ~ zero**; tree-fitted thresholds do not inflate train
  precision at high support.
- **Era drift is secondary**: the shortlist evaluated on 2018-19 gives ~3.9x
  lift over that era's base rate vs ~3.5x on 2023 — similar relative
  performance — though only 645/1,403 rules still flag >=10 cases there
  (dollar-scaled features age), and per-rule correlation across eras is weak.
- Frame base rates are low by construction (earned frame 2.6%; each typed
  frame excludes the other error types AND `other_error`, which is the largest
  error category — 1,377 of 2,994 total 2023 errors — and is currently
  modeled nowhere).

Design consequences:

- Threshold on a **lower confidence bound** of train precision (e.g., Wilson or
  Jeffreys LCB) instead of the raw point estimate: it smoothly penalizes
  small-support rules and directly counteracts the winner's curse, replacing a
  crude MIN_TRAIN_FLAGGED cliff. Report raw train + holdout precision alongside.
  VALIDATED (2026-07-04, THRESHOLD_STAT="wilson_lcb" in script 2): at matched
  ACHIEVED any-error holdout precision (~0.20), LCB selection catches 12.8% of
  all holdout errors vs 8.2% for raw-precision selection — strictly better rule
  ranking. Nominal LCB thresholds are roughly calibrated to deployed (any-error)
  precision: LCB 0.15 delivered 0.200, LCB 0.20 delivered 0.217. The LCB
  shortlist is small and honest: 68 rules, median any-error holdout precision
  0.25, 46/68 holding >= 0.20 (vs the raw shortlist's 1,403 rules with median
  0.10 frame precision).
- If a hard support floor is kept anyway, 30-50 is better than 10.
- DECIDED (2026-07-04): include `other_error` for completeness, knowing
  the category is heterogeneous/tricky. Two complementary mechanisms:
  (a) a fourth mining frame (`other_error` + `no_error`);
  (b) an "any-error" scoring pass — evaluate every final rule union against ALL
  over-threshold errors regardless of type, since frame-relative precision
  understates deployment precision (a flagged case with a different error type
  counts as a miss in-frame but is an operational win). Report both.
- Note: all current PR curves are frame-relative — recall is within the error
  type's own frame (e.g., 33% recall at the 0.20 floor for earned = ~9% of ALL
  2023 errors).

## Open questions (decide when we revisit)

1. Second engine: xgboost (recommended) vs hand-rolled boosted rpart vs
   ranger-only to start?
2. Dominance dedup uses TRAIN precision for the >= comparison — confirm.
3. Threshold rounding: on by default (3 signif digits) or off?
4. New scripts alongside old (`*_v2.R`) or replace in place with old copies to
   `archive/`?
5. Keep the ESAP/strata comparison scripts on pre() until the new stack is
   validated against one known result, or convert immediately?
