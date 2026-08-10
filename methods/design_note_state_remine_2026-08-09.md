# Design note: state re-mine — representation runoff, then the state pools
# (2026-08-09, chained after the national factorial tonight)

Eric's instruction (2026-08-09 evening): with hours to spare after the
factorial, launch the state re-mining. First a runoff between the
_by_hh_size and _p vocabularies ON THE TYPED (specific-error) MINING at
state scale; then, with the winner, mine each state's full pool: the four
typed error frames plus the any-error frame by household-size strata.

## 1. The questions (one sentence each)

Stage 1: at state scale, does typed-frame mining deliver better FY2024
within-state budget-list performance under the per-size vocabulary or the
percentile vocabulary? Stage 2 (production mining, not a comparison):
build every state's admitted rule pool with the winning vocabulary, as
the state side for the v2.5.0 blend.

## 2. What varies

Stage 1 varies the vocabulary only, two arms mirroring tonight's national
representation contest exactly:

- **ps**: the 16 + gross/earned/unearned_by_hh_size (all five per-size
  fields, zero percentiles)
- **pct**: the 16 minus its two per-size features, plus the nine
  percentiles (incl. shelter_expenses_p, total_deductions_p)

Held fixed: rebuilt frame; frozen train-year percentile fit (per
state x size cell — at state scale these are the state's own cells);
shipped engines and params; typed frames mined SEPARATELY and pooled
across household sizes (the proposal's support-preservation design,
findings 17 scoped to national only); admission BH FDR 10% vs the typed
frame's own base rate AND n >= 30; 99%-LCB ordering; the findings-31
walk within state (fill FY2022-23 to 5%/10% core + 3x buffer, freeze,
walk FY2024); seed 117 (production mines once; the state-level seed
noise caveat is recorded, and the paired-across-49-states design
averages over it).

Stage 2 mines with the winner only: 4 typed frames (pooled sizes,
resumed from stage 1's winner-side cache — identical config) plus
any_error x the three HH strata, per state. Output: admitted pools per
state, cached and summarized. NO delivery lists are built tonight
(promotion and blending are Eric's decisions; ledger: state-scale
tuning is winner's-curse territory, the n >= 30 floor stands).

**Pre-stated decision rule (stage 1 -> stage 2), fixed before any
result exists**: among states where BOTH arms admit at least one typed
rule, the winner is the arm with the higher median within-state paired
FY2024 any-error precision delta at the 5% budget; zero or negative
median -> per-size wins (table-free tie-break). Companions reported as
always (mean, harmed tail < -0.05, 10% budget, dollar recall,
states-admitting counts, sign-consistency). If fewer than 15 states
admit under both arms, the runoff is unreadable at state scale and the
winner defaults to the NATIONAL representation-contest winner from
tonight's factorial (pct_vs_persize contrast, same definitions), with
that fallback logged. The runoff readout is recorded either way; the
stage-2 pools are production artifacts, not evidence for the
representation question.

## 3. Support (proposal arithmetic; per-state table printed at runtime)

A state's public FY2022-23 pool is ~1,500 rows carrying 96-280 errors;
by type roughly 10-130 events per typed frame on the full pooled rows
(remine proposal, "Support arithmetic"). mine_rule_vocabulary skips
frames under 100 rows / 10 events, and admission (BH + n >= 30) will
admit nothing in thin cells — measured outcomes, not failures, per the
proposal and the engineering-artifacts rule. any_error x stratum at
state scale: ~500-800 rows and ~50-100 events per stratum. The script
prints rows AND events per state x frame x stratum before mining.

## 4. What the record says (cited)

- Findings 17 retired typed frames from the NATIONAL delivery
  vocabulary and says nothing about state pools; findings 11 validated
  coarse strata for national mining; dropping strata for state typed
  frames is support preservation (both scoping arguments verbatim from
  the remine proposal, which Eric approved as the coordinating doc).
- Virginia hazard (CLAUDE.md 2026-07-06): single-state mining collapses
  without the n >= 30 floor — the floor is on everywhere here.
- Findings 14-16: the state's own pool is a fallback/blend component;
  nothing tonight changes the recommended deliverable.
- Findings 31 walk machinery + slack rule (findings 27) as in the
  factorial; OOM hazard moot at state scale but scoring still uses the
  shared helpers.
- Rule-pool incomparability (memory): one LCB scale across pools of
  different search sizes is a known hazard for the BLEND; tonight's
  runoff compares like-for-like arms and defers blending entirely.

## Mechanics

`methods/state_remine_v2.R` + `runners/run_state_remine.R`, chained to
launch AFTER the 6-arm factorial completes (shared machine; the
factorial must exit first). Caches per state x arm to
`methods/state_remine_v2/cache/`; outputs (runoff readout CSVs +
stage-2 pool summaries + per-state admitted pools as .rds) ->
`methods/state_remine_v2/`. SMOKE=1 runs 3 states with tiny ensembles.
Estimated: stage 1 ~2.5-4.5 h (49 states x 2 arms), stage 2 ~1-2 h
(any_error strata only; typed resumes from the winner's cache). No
writes to state_delivery_lists/, no CHANGELOG entry, no version bump.
