# Senior-statistician review record: state re-mine (2026-08-09)

Fresh-context review per the routing rule, of
`methods/design_note_state_remine_2026-08-09.md` and
`methods/state_remine_v2.R`. Eric delegated the launch gate; the run
chains after the national factorial exits.

**Verdict: REVISE -> fixed -> chains tonight.**

Verified as pre-stated: the runoff varies vocabulary only (ps/pct
byte-equivalent to the factorial's arms; frame prep verbatim); the
decision rule's three branches match the note; scope is clean (findings
17 retirement is national-scale, the proposal pre-argues state-scale
typed mining without strata; n >= 30 everywhere; Virginia hazard
honored); the typed-admission/any-error-evaluation pairing is coherent
because both arms pass through identical machinery and the shared scale
cancels in the paired contrast.

## Blocking item, fixed before launch

**Stage-2 any-error admission ran BH per stratum; the settled §19
admission is ONE BH across the frame's candidates with per-stratum base
rates inside the p-values** (the shipped builder's and the factorial's
path). Per-stratum BH is a different multiplicity correction admitting
a different set, and stage-2 pools are production artifacts. Fixed to
the joint call (pbinom vectorizes over the base-rate argument);
re-smoked - the corrected admission measurably changed pool contents
(smoke Mississippi any-error 393 -> 436).

## Flags, fixed before launch

- Resume no longer drops finished states from the stage-2 summary (the
  summary row is rebuilt from the resumed pool).
- Pool filenames carry the winner vocabulary
  (`pool_<state>_<vocab>.rds`), so a fallback-flip on resume cannot mix
  vocabularies silently.
- Fallback branch 2 now mirrors the factorial's own aggregation
  (per-state seed means, then the median across states) and guards
  against an NA median.
- Stage-2 per-stratum support prints (verbose mine), per the note.
- The pool artifact carries an `lcb_scale` column and
  `pools/README.md` documents the two hazards for blend consumers:
  cross-scale sorting (rule-pool incomparability) and cross-frame
  duplicate rule texts (kept by design; dedup with provenance priority).

## Noted for the write-up (not code)

- The 10%-budget paired summary and sign-consistency companions are
  derived from `state_runoff_readout.csv` in the write-up (the CSV
  carries every cell); the factorial's seed-noise reference is quoted
  when interpreting the median.
- The WINNER pick is median-only automation - acceptable because it
  selects an arm for production mining, not a shipping readout;
  overriding later costs only the ~1-2h any-error re-mine (both arms'
  typed caches survive).
