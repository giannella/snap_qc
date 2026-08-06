# Plan: does characterization-profile distance predict rule complementarity?

**Written 2026-08-06, before any result existed.** Descriptive diagnostic on
existing artifacts, no mining, expected under an hour of compute. It decides
whether profile dissimilarity earns a place in the preference-ordering study
(ledger: open row, unlocked by section 31).

## The question

Section 32 closed marginal-quality-aware ordering at public scale because
marginal slices (median 1-2 cases) support no statistic. A rule's section 29
characterization profile is computed on its FULL error set (median 126 error
cases), so profile DISTANCE between two rules is estimable where marginal
precision is not. It is a useful ordering or diversification signal only if
train-side profile distance predicts case-level complementarity on a future
year: do profile-dissimilar rules catch different errors next year?

## Design

- **Universe**: rules deployed on the 98 bench lists (built FY2022-23, scored
  FY2024), the same universe section 32 verified. Pairs are formed WITHIN a
  state-budget list and WITHIN a household-size stratum; cross-stratum pairs
  are excluded because their flag sets are disjoint by construction and would
  fake a diversity signal.
- **Profile, train side only**: each deployed rule's error-case profile
  computed on FY2022-23 (the build years), by the section 29 machinery:
  element-group shares (primary), nature-group shares, cause / timing /
  discovery shares (secondary). Rules with fewer than 20 FY2022-23 error
  cases are excluded from pairs (their profiles are intervals, not
  estimates); the excluded share is reported, since section 29 says
  distinctive profiles concentrate in exactly the narrow rules.
- **Distance**: total variation distance on element-group shares (primary);
  secondary distances on the other field groups.
- **Complementarity, holdout side**: for each pair, on the state's FY2024
  caseload: 1 minus the overlap coefficient |A and B| / min(|A|, |B|) of the
  two rules' error-catch sets (overlap coefficient rather than Jaccard
  because catch sets differ in size). Flag-set complementarity reported
  alongside.
- **Confound controls**: (i) support: partial Spearman controlling both
  rules' flag counts, since narrow rules have both distinctive profiles and
  noisy catch sets; (ii) the same computation with TRAIN-side complementarity
  as the outcome, to show how much of any association is mechanical overlap
  persistence rather than profile signal.
- **Statistic**: within each state-budget list, Spearman correlation between
  train profile distance and FY2024 error-catch complementarity across
  same-stratum pairs; the readout is the median across lists, against a
  permutation null (profiles shuffled among rules within list and stratum,
  200 draws).

## Pre-stated reading rules

- **Signal exists** if the median within-list Spearman exceeds the 95th
  percentile of its permutation null AND the positive direction holds in at
  least 60% of lists with 10 or more eligible pairs. Consequence: a
  profile-diversity component enters the preference-ordering study's
  pre-registration as a candidate arm.
- **No signal** (either condition fails): characterization stays what section
  29 built it as, descriptive decision support; diversity by profile makes no
  recall claim, and the preference-ordering study proceeds on state
  preference alone.
- The partial correlation (support-controlled) is the quoted effect; if the
  raw correlation clears the bar but the partial does not, the verdict is NO
  SIGNAL (the association was breadth, not profile).

## Constraints

Outputs to `methods/profile_distance_diagnostic/`; no writes to
`state_delivery_lists/`, no CHANGELOG, no version bump. Routing rule applies:
written under the principal-data-scientist framing, fresh senior-statistician
review before the run. Assertion anchor: the per-rule FY2024 catch sets must
reconcile with `methods/marginal_precision_diagnostic/per_rule_marginal.csv`
totals (same walk, same caseloads).
