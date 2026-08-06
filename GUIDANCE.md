# Guidance from validation studies

A brief overview of what we've learned about what works and what doesn't from modeling QC data. 
Note: Because of the amount of underlying detail, if you want to make use of all this in your modeling, it may be easier to point an AI assistant at this document, the ledger, modeling_findings.md, and detailed_modeling_findings.md. This overview recaps the key lessons in 
[`methods/findings_ledger.md`](methods/findings_ledger.md), the one-row-per-claim
record we use to improve our own models: every claim there carries a status
(settled, retired, option, hazard, open) and the scope it was tested at. Each
point below closes with that status and scope, and cites its findings section:
`§N` is the same section in
[`methods/modeling_findings.md`](methods/modeling_findings.md) (the
plain-language account) and
[`methods/modeling_findings_detailed.md`](methods/modeling_findings_detailed.md)
(every arm, every number, every caveat). The findings documents own the numbers;
nothing here originates one.

A statistic used throughout: several points rank rules by the **99% Wilson lower
confidence bound** of a rule's training precision, a cautious "at least this
precise" figure rather than the raw training number. We rank on the lower bound
because the best-looking rules are partly lucky (the *winner's curse*). For example, rules
selected at raw training precision 0.20 delivered about 0.10 on data they were
not built from (§1). "Lower bound," "the bound," and "confidence-bound scale"
below all mean this statistic.

## What to rank on

- **Rank and filter on the lower bound, never on raw training precision or on
  holdout performance.** The bound is what makes training estimates track
  deployment (§1). No fixed stringency beat the 99% level on both test eras
  (§20); ranking by a smoothed posterior mean was tested and did worse on both
  eras (§18); and ranking by a bound computed on data the rules were not mined
  from, the theoretically clean repair, did not improve the delivered list
  either (within-state change -0.0044 at the 5% budget against a
  pre-registered +0.010 bar, §30). *(settled, two eras; the out-of-fold result
  is one era, 49 states, and untested on the 40k-100k internal case files a
  state holds)*
- **Count any error found as a win, even outside the mined error type.** A case
  flagged by a rule mined for one error type often has a different real error,
  which a reviewer can still act on. Frame-relative precision understates
  deployed precision about 2x (0.178 vs 0.080 on the earned frame, §6); quote
  the any-error number. *(settled)*

## Which rules to keep, and the knobs not to tune

- **Admit rules with a false-discovery-rate test AND a minimum-case floor;
  neither replaces the other.** The test (Benjamini-Hochberg at 10% against the
  stratum base rate) limits how many kept rules are flukes; the floor (at least
  30 flagged training cases) keeps poorly measured rules out of the top of the
  ranking. Dropping the floor cost 5 points of delivered precision (0.335 to
  0.284, §19). *(settled, two eras at the 5% budget)*
- **Do not tune those two knobs; both temptations are tested.** Tightening the
  rate from 10% to 5% changed essentially nothing delivered: the rules it
  removes sit mid-ranking while a budget deploys roughly the top 16 to 27
  (median within-state difference 0.000, §25). Raising the floor above 30 made
  precision monotonically worse (0.3345 at floor 30 down to 0.2826 at 778), and
  lowering it to about 15 also lost (0.2558), so 30 is near optimal from both
  directions even on a pool mined from 77,806 cases, roughly internal-data
  scale (§26). Nor is the fill walk's admission of small marginal slices a
  knob to fix: reordering the walk on marginal precision recovered nothing
  out of sample (+0.000 at the 5% budget), because the median marginal slice
  is 1 to 2 cases and no statistic that small is estimable (§32). *(one era,
  18 states, exploratory; the floor result answers "a bigger search needs a
  bigger floor" with no)*
- **Never cap the ranked pool at a fixed rank as policy.** Building a list
  reads far deeper than its final length: the median state's core alone
  reaches rank 969 at the 10% budget, so a cap at 1,000 truncates real
  delivered rules for about half the states. Pruning is safe only with the
  fill's own completeness check (§27). *(settled; verified on all 98 shipped
  fills and 70 research comparisons)*

## What vocabulary to mine

- **The delivered list is built from the pooled all-errors frame; typed frames
  help only at fixed filter floors.** At floors, mining per error type and
  pooling the union adds reach and can only add catches (§3, replicated on a
  second year). But under a review budget the enlarged pool lowered delivered
  precision (0.306 vs 0.324 at the 5% budget) and three rescue attempts failed,
  so typed frames were retired from the delivery vocabulary; ten shipped typed
  lists were withdrawn (§17). Mine typed frames for exploration; deliver from
  any-error. *(typed delivery retired; re-tested across all 49 states with
  precision a wash and any-error ahead on dollars)*
- **Stratify by household size, coarsely: 1 / 2-3 / 4+.** The coarse split
  never lost across two studies and a year-swap (on the swap year it won
  precision outright, 0.302 vs 0.262), and finer 5-way splits added nothing at
  about 1.6x the compute (§11). Elderly/disabled is a feature, not a stratum:
  the ensembles carve that population out on their own (§8). *(settled)*
- **The engine matters less than the filtering.** The xgboost + ranger pair is
  the validated generator, but the engine choice is worth about a point; the
  leverage is strict filtering and any-error scoring (§2, §4). The pairing
  replicated on a fresh year with a thinner margin than predicted (2.1pp
  against 3, §13). Mine big ensembles and filter stringently: the big pool's
  advantage is a longer usable list of substitutes, not better numbers (§5).
  *(settled, year-swapped)*

## How to deploy

- **Evaluate at review budgets (5% / 10% of caseload), not just filter floors.**
  States plan around review capacity; several configurations that looked like
  failures at fixed floors were fine under a budget (§12). Tested the way a
  state faces it (rules built on past years, scored a year ahead), the national
  list delivered median precision 0.300 at a 5% budget and 0.273 at 10%, with
  every state above its base error rate, 1.5-3.4x lift over random review
  (§14). A reading aid for any budget-level number: a median state's 5% budget
  flags about 44 cases, and precision on 44 cases carries a binomial standard
  error of about 0.068 on its own (§30). *(settled method; 12-state future-year
  benchmark)*
- **Deploy the blended frozen list as the default; re-tune to your state only
  as a validated fallback.** The blend puts your state's own mined rules and
  the national rules on one confidence-bound scale, sizes the core to your
  budget against your own recent caseload, and adds buffer rules to 3x depth so
  reviewers never run dry; you walk it in rank order with no outcome data.
  The fill walk is overlap-aware: a rule enters the list only if it flags
  cases no higher-ranked rule already flagged, so redundant rules never
  occupy list slots (at the 5% budget the walk reads a median 1,544 ranked
  rules to deliver 137, §27), and every quoted precision counts each case
  and each error once, so overlap cannot inflate it. The rank on your list
  is the delivered walk order among kept rules.
  Freezing in advance cost under a point of precision against an
  after-the-fact list (0.294 vs 0.301 at 5%; 0.270 vs 0.275 at 10%, §15), and
  the blend beat a national-only list at the 5% budget (0.324 vs 0.294) while
  roughly tying at 10% (§16). Known blind spot: a national rule's bound says
  nothing about transfer to your state, so where the national mix fits worst
  the scale over-trusts national rules; keep your own-pool list as the
  fallback and let your internal validation on newer data decide (§16, §14).
  Explicit re-filtering or re-tuning of national rules on your own data did
  not beat the shared scale for the median state, and at thin sample sizes it
  collapses; hold the 30-case floor and a holdout year (§9, §14). *(settled,
  18-state and 12-state future-year tests)*
- **Read your list as one of many near-equivalent orderings, not as canonical.**
  Re-running the miner with a different random seed changes most of the rule
  text and changes about half of what a 5% budget list catches (median overlap
  0.531 between seeds), yet by depth 20,000 of the ranking different seeds
  cover 96% the same errors and every seed's full pool reaches every test-year
  error (§31). Run-to-run churn in rule text is not a performance difference,
  and a state that prefers rules it can act on has room to impose that
  preference at depth; whether preference-based reordering preserves precision
  is not yet tested. *(settled, one era, national pools)*
- **Check your list's exposure to the near-max-benefit reconstruction
  artifact.** Some delivered rules earn their rank on households our public
  reconstruction places just below the benefit maximum, a band your own file
  may not reproduce: a median 6.3% of a 5% list's delivered cases (16.3% at
  the most exposed state) come from such rules. Exposure must be measured, not
  read off the rule text, which overstates it about fivefold (§28). A feature
  fix is designed but not yet validated. *(settled diagnostic; fix open)*

## Know your data

- **Check your state's visibility before relying on public-data rules.** The
  public QC file excludes ineligible cases entirely, each a 100%-of-benefit
  error, so states see 43% (New Jersey) to 91% of their own error population
  in it, about 71% nationally. Below roughly 60%, treat national rules as a
  supplement and run the mining on your internal data, which includes those
  determinations; the scripts support this directly (§10). *(settled,
  FY2022-24, per state)*
- **The exclusion pipeline is validated less deeply than the inclusion list.**
  Its settings (clean-rate bound, floor of 25, relative safety bar) rest on a
  single holdout year, with no multi-era or multi-state deployment test yet
  (§23). Weigh that scope before building a review process on it. *(settled
  settings, one holdout year)*
- **Re-test your selection choices on a year that never judged them.** Every
  choice here was first judged on one holdout year, so the decisive comparisons
  were re-run with the year roles swapped and expected outcomes written down in
  advance: three of four replicated and one was retired (§13), and a later
  stringency hint likewise failed its second-era test and was not adopted
  (§20). Pre-committing predictions is what separates a real effect from a
  lucky one. *(standing practice; two worked instances)*
