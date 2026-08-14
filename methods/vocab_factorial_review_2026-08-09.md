# Senior-statistician review record: vocabulary factorial 2x2 (2026-08-09)

Fresh-context review per the routing rule, of
`methods/design_note_vocab_factorial_2026-08-09.md` and
`methods/vocab_factorial_v2.R`. The launch gate was delegated to this
review; launch scheduled 18:00 (Task Scheduler
`snapqc_vocab_factorial_20260809`).

**Verdict: APPROVE WITH FLAGS - launch at 18:00.**

Rubric: each pre-registered contrast varies one component; the 2x2 arms
match the note; cache reuse of base/cand verified MECHANICALLY
(mine/score/admit functions byte-identical to the 2026-08-08 script;
configs identical; all 12 cache files present; smoke isolated to its own
cache); frozen-percentile construction leakage-clean with the 8th source
added; the pre-screen replicated exactly from scratch (train precision
0.2135, LCB 0.1840, holdout 0.2302); no retired or hazard ledger row
re-opened; positive bar for shelter_expenses_p encoded with the
sign-consistency and seed-spread companions.

## Flags, all fixed pre-launch except the standing pair

1. **Design-note factual error, corrected**: `rawsltexp` is NOT
   pre-adjustment-stale; it is byte-identical to `shelter_expenses` in
   all 115,559 rows of 2022-2024 (recomputed after every adjustment).
   The source choice stands on naming consistency only; the note and
   script comments now say so.
2. **Artifact-share companion restored** (`artifact_share_flagged` per
   cell): the package's adoption rests on artifact-independence grounds,
   so the findings-28 companion belongs in this readout. Fixed.
3. **Shelter-only deployed usage added** (`n_rules_deployed_slt` per
   cell + summary share): the shelter bar's usage leg is now measurable
   on both slt contrasts. Fixed; the dead `dep` variable now feeds the
   summary stamp.
4. **NA guard on the usage summary's best-rank** (na.rm). Fixed.
5. **Standing operating rules** (carried from 2026-08-08): slack > 0
   cells are invalid by construction (re-walk from cache with a larger
   window; never read); cache keys omit config, so any edit to ARMS,
   engines, or PCT_MAP requires clearing the shared cache first. New
   hardening added at review's suggestion: a RESUME ANCHOR asserts the
   resumed base/cand pools reproduce the 2026-08-08 run_info (admitted
   counts and top rules) or the run STOPS.

Re-smoked clean after all fixes (16 cells, slack 0, all summary lines
live).

## Delta review: representation-contest arms (same day, evening)

**Verdict: APPROVE WITH FLAGS - relaunch tonight** (delta = commit
347d9e2 over approved 149d672; confined to the arm/contrast config; no
mine/score/admit/walk code changed; the four approved arms byte-identical
to what launched at 18:00).

1. **pct_vs_persize is a PACKAGE contrast, not representation per se**
   (pct_pure carries four component-level percentiles with no per-size
   counterpart; rawrent_p best rank 3 in smoke shows the confound is
   live). Pre-stated reading rule added to the design note before any
   results existed: attribution to representation requires the usage
   table + deployed-rule text; counterpart-less percentiles dominating
   -> log "percentile package wins."
2. **Operational**: the 18:00 run's outputs archived aside before the
   relaunch overwrites them; afterward the four shared arms' rows are
   diffed (identical expected; any difference = drift, do not read).
3. **Operational**: the 6-arm run must not start until the 18:00
   process has exited (shared cache, non-atomic saveRDS).
4. **Cache-editing rule amended** (reviewer's sanction): additive ARMS/
   PCT_MAP edits are allowed iff the resumed arms are diff-verified
   unchanged and anchored; total_deductions_p verified leakage-clean
   (case-characteristic source, per-column-independent builder).
