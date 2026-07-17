# Pre-registration: independent-era validation (train 2017-18, test 2019)

Written 2026-07-17, BEFORE the run. Companion to
`methods/yearswap_preregistration_2026-07-09.md`; this is the
pre-registered-validation stage of the refinement loop for every candidate
that passed 2024 exploration, plus the ordering candidates that have touched
no test year at all.

## Why this design

Every exploratory result of 2026-07 was scored on the same benchmark
(train 2022-23, test 2024). The 2017-19 public files are a fully separate
era: different economy, different policy environment, different visibility.
A METHODOLOGY (an admission gate, an ordering statistic, a dollar ranking)
that delivers on both eras is calibrated to the statistics of the selection
problem, not to one dataset. Rules themselves are NOT expected to transfer
across eras (the era-match lesson, findings #12); recipes are.

Setup mirrors the 2024 benchmark exactly: pools mined on 2017-18 (national +
each of the 18 benchmark states), blended on one scale, frozen lists filled
to 5%/10% of the state's 2017-18 caseload, walked on the state's 2019,
scored on any-error precision and error-dollar recall. Support floor n >= 30
everywhere except where an arm states otherwise.

## Arms

**Admission** (on raw unfiltered vocabularies):
- `prod`: the production keep filter (n >= 30, raw >= 0.05, raw > base).
- `fdr10f`: Benjamini-Hochberg vs stratum base rate at alpha = .10, plus
  the n >= 30 floor. (Floorless FDR is not re-tested; refuted on 2024.)

**Ordering** (on the admitted pool, precision goal):
- Fixed-z ladder: Wilson lower bound at z in {1.645, 2.326, 2.576, 3.09}.
- `zN`: competition-scaled z, PRE-SPECIFIED as
  z(N) = qnorm(1 - 0.01 * 48429 / N), where N is the admitted pool size
  (anchored so N = 48,429 - the 2022-24 national pool - gives the current
  z = 2.326; larger pools get stiffer bounds, smaller pools laxer).
- `famEB`: family-aware shrinkage. Near-duplicate families at Jaccard 0.95
  on pool-train coverage; ONE representative per family (max support);
  beta-binomial prior fit per stratum on representatives only; rank by
  posterior mean. (The 2024 EB failure is attributed to a prior fit on
  redundant renderings; this is the repaired version's one shot.)
- `xfit` (national pool only): cross-fitted ordering - mine on a random
  half of 2017-18 cases, order by the untouched half's Wilson lower bound
  (z = 2.326). Selection-free estimates by construction.

**Dollar goal** (ordering judged on dollar recall):
- `dpf` (dollars per flagged case) and `dpflb` (log-scale lower bound)
  vs the `lcb99` baseline.

## Pre-registered expectations

- E1 (admission): `fdr10f` matches `prod` within 1pp of median precision at
  the 5% budget and is >= `prod` at 10%, as on 2024.
- E2 (stringency direction): z = 2.576 >= z = 2.326 on median precision at
  the 5% budget (the 2024 sweep's direction replicates).
- E3 (competition scaling): `zN` lands within 1pp of the best fixed z at
  both budgets (the formula reproduces what hand-tuning finds).
- E4 (dollar goal): `dpf` beats `lcb99` on median dollar recall at the 10%
  budget by >= 2pp (5.5pp on 2024 era... recorded: +3.5pp on 2024).
- E5 (repaired shrinkage): stated as an OPEN test, no directional claim -
  the 2024 evidence is against shrinkage ordering; if `famEB` beats the
  fixed-z ladder at both budgets here AND on a 2024 confirmation, the
  Bayes-ordering path reopens; otherwise quantile ordering stands.
- E6 (validity check): `xfit` >= the fixed-z ladder at national scale;
  if selection-free ordering does NOT at least match, our understanding of
  the winner's curse in this pipeline is wrong somewhere, and adoption of
  everything above pauses for diagnosis.

## Decision rules

- An arm already explored on 2024 (fdr10f, dpf, dpflb, the z ladder) is
  ADOPTABLE if its expectation holds here: 2019 + 2024 = two eras.
- An arm new here (zN, famEB, xfit) that wins on 2019 must ALSO be confirmed
  on the untouched-for-it 2024 benchmark before adoption.
- Failures are retired in writing in modeling_findings.md, per the loop.
- Anything adopted ships together as ONE minor release after the study
  concludes (continuity policy; no piecemeal methodology churn).

## Compute plan

reg_model_data.rds already contains 2017-19 (119,128 rows). Jobs, in order:
1. Raw any-error vocabularies on 2017-18 (national + 18 states), with n/k -
   the fdr_raw_vocabulary_mine_v2.R recipe pointed at the era (overnight).
2. Admission x ordering x dollar sweeps from those caches (fast, harness
   lineage) scored on 2019.
3. Confirmation runs on the 2022-24 era for any new-arm winners (cached).
