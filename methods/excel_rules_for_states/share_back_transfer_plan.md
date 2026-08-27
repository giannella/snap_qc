# Share-back transfer analysis: design note

*Drafted 2026-08-27 under the principal-data-scientist framing; the veto
protocol follows the senior-statistician recipe agreed the same day.
Pre-registration status: DRAFT until reviewed; the primary hypothesis is
frozen before the first share-back is analyzed.*

## Question (one sentence)

When a state returns Step 6 aggregates from its internal review data, which
rule characteristics predict whether a rule's national performance carries
over — specifically, on QA-type data, does the share of a rule's national
errors discoverable in the case record predict its internal-to-national
precision ratio?

## Data

One row per delivered rule per share-back:

- **Internal (from the Step 6 sheet)**: cases flagged `n_i`, errors caught
  `k_i`, error dollars, ineligible-household catches (STATUS = 4), plus the
  book denominators (total cases, error cases, error dollars, ineligible
  count) and metadata (state, fiscal years pasted, data type QC / QA /
  pre-auth).
- **National priors (delivery list / pool)**: `n_flagged_train`,
  `precision_train`, `precision_train_lcb`, `dollars_per_flag_train`, pool
  origin, stratum.
- **Predictors (rule_characterization sheet, ALL of it)**:
  `found_in_case_record`; cause shares (agency, client, third-party,
  no-fault); timing shares (before / at / after certification); the seven
  element-group shares; the ten nature-group shares; `share_overissuance`;
  each with its count and Wilson interval.

## Primary model (the regression)

Binomial GLM with the national precision as an offset, so the coefficients
measure TRANSFER, not level:

    k_i ~ Binomial(n_i, p_i)
    logit(p_i) = offset(logit(precision_train_i)) + alpha + beta * found_in_case_record_i

- **Frozen primary hypothesis (QA-type share-backs only)**: beta > 0 —
  rules whose national errors are discoverable in the case record transfer
  better to a review channel that only sees the case record. One-sided.
- **Negative control**: on QC-type share-backs the discoverability channel
  is absent, so beta ~ 0 is expected; a large beta there flags a
  confounder, not a discovery.
- alpha absorbs the book-level calibration shift (data-quality gate having
  passed first); quasi-binomial dispersion for rule-level heterogeneity.
- Support: rules enter at internal `n_i >= 10`; sensitivity re-fit at
  `n_i >= 30`. The binomial likelihood weights by support automatically.

## Secondary (labelled exploratory)

1. **Full-characterization model**: the cause, timing, element-group, and
   nature-group shares are compositional; enter each block with a
   drop-one baseline, fit with an L1/elastic-net penalty, and report as
   exploratory — no coefficient from this model ships anything on its own.
2. **One pre-specified interaction**: cause_agency x found_in_case_record
   (agency-caused, record-visible errors are QA's home turf).
3. **Dollar analog**: log dollars-per-flag ratio (internal / national),
   support-weighted, for the same predictor set.
4. **Family readout for humans**: support-weighted transfer ratio with an
   interval per element group and per nature group — the table that goes
   back to the state.

## What this analysis never does

- Never re-ranks kept rules by internal precision (findings 1, 14-16, 20).
- Never promotes a rule on its internal point estimate; promotion into a
  deployed list waits for the next-period certification.
- Never treats one state's coefficients as settled: two share-backs are a
  replication; one is a hypothesis.

## The refined-list protocol this feeds (ingest_share_back.py)

1. **Calibration gate first**: book-level base error rate, error dollars per
   error, per-rule flag-rate ratios vs national. A uniform shortfall is a
   mapping/data problem; the script BLOCKS (exit code 2, no verdicts
   issued) rather than warns, because BH's adaptivity would convert a data
   problem into a mass list rewrite.
2. **Veto test**: for rules with `n_i >= 30`, one-sided binomial p-value
   against H0: precision >= floor (default 0.20, configurable), and drop
   only when Benjamini-Hochberg at FDR 10% rejects AND the one-sided upper
   Wilson bound sits below the floor. BH runs within each DECISION FAMILY
   separately (shipped-rule vetoes; promotion-candidate vetoes): the two
   decisions carry their own FDR budgets, and a merged family would let a
   large measurement tier dilute the power of shipped-rule vetoes. Rules with material ineligible
   catches (>= 3 or >= 10% of the rule's errors) are PROTECTED: flagged for
   human review, never auto-dropped, because the public frame was blind to
   exactly those errors.
3. **Refill**: Include? promotions proposed from the state's own unshipped
   rules in frozen national-LCB rank order, one-for-one with drops; the
   workbook recomputes the union and workload when the state applies the
   configuration (aggregates cannot see overlap, so capacity is confirmed
   on their side). **Promotion faces the same veto as deployment**: a
   measurement rule whose own internal read fails the BH + upper-bound
   test is barred from promotion (frozen-order refill stays outcome-free;
   the veto only removes candidates the same conservative test rejects).
4. **Certification**: the refined list's quoted numbers are the national
   priors plus veto rationale; performance is judged on the NEXT
   share-back, never on the data that selected.

## The measurement tier (why the 300-rule workbook)

The workbook normally carries only the capacity-filled effective list
(tens of rules for a typical state). Step 6 reports every rule in the workbook at zero
marginal cost to the state, so the build adds a **measurement tier**,
shipped with Include? = FALSE (inert for deployment, priced at zero
review capacity):

- Tier A: the remainder of the state's own delivery list (unpromoted
  buffer) — locally relevant, already vetted by the national pipeline.
- Tier B: national-pool rules chosen for PREDICTOR COVERAGE, not rank:
  stratified across found_in_case_record terciles x cause_agency terciles,
  with every nature group represented, subject to pool support n >= 30,
  admitted-pool membership, the delivery-transform gates, and an expected
  internal flag count high enough to carry information
  (national flag rate x state caseload >= ~15).

Rank-tail sampling alone would concentrate in a few families and leave the
regression unidentified on the predictors it exists to estimate; the
stratification buys spread on exactly those axes.

## Power sketch (honest)

If a state pastes a QC-sized period (a few thousand cases), a median measurement
rule flags a few dozen cases: per-rule intervals stay wide, and inference
runs at the family level (10-30 rules per family x 20-60 flags each is
enough for family contrasts of ~10-15pp). If they paste QA volume
(tens of thousands of cases), per-rule estimates sharpen and the
regression identifies individual-rule transfer. Either way the design is
informative; the paste size determines the resolution, and the plan
commits to reporting at the level the support actually earns.
