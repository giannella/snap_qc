# Senior-statistician review record: vocabulary attribution study (2026-08-08)

Fresh-context review per the routing rule (`methods/known_constraints.md#routing`),
of `methods/design_note_vocab_attribution_2026-08-08.md` and
`methods/vocab_attribution_v2.R`. Eric delegated tonight's launch authority to
this review (2026-08-08, away overnight).

**Verdict: APPROVE WITH FLAGS — launched 2026-08-08 21:21.**

Rubric: one-component contrast verified against the findings-31 machinery
(walk_mask and admit_and_rank verbatim; shared constants across arms); support
computed and asserted; no retired or hazard ledger row re-opened; budgets
5%/10% with the mandatory companions; frozen-percentile construction checked
for leakage (train-only fit confirmed; findInterval matches cume_dist
semantics on the frozen fit); paired-delta and seed-noise logic verified.

## Flags (none invalidates the contrast)

1. **Deployed-list usage was not measurable from the planned outputs.**
   Fixed before launch (readout-only change, cache empty at the time):
   walk_mask now returns the deployed rule identities and the readout carries
   `n_rules_deployed` / `n_rules_deployed_newfeat`; the summary prints the
   deployed-usage share for the candidate arm. Re-smoked clean before launch
   (deltas reproduced exactly; slack 0 in all smoke cells).
2. **Slack is warned, not asserted.** Operating rule for reading the results:
   any cell with slack > 0 is invalid by construction — re-run those cells
   from cache with a larger window; never read them as results. The summary
   prints a slack-cell count so this cannot be missed.
3. **The candidate arm bundles two feature families** (3 name-fixed per-size
   income features + 7 frozen percentiles). A win or loss attributes to the
   PACKAGE; the usage table is descriptive, not causal. Family-level
   attribution needs its own arm (or the paired ablation re-walks from
   cache).
4. **Checkpoint keys omit the vocabulary/engine config** (`mine_<arm>_<seed>`).
   Do not edit ARMS or engine params and resume against the same cache;
   clear the cache or add a config hash first.

Notes: the ten-state × three-seed panel supports a recommendation only —
ledger promotion needs Eric and eventually a second era; the legacy-walk
deviation cannot touch the contrast (identical across arms); the fresh-share
49-state scorecard is the follow-up for the winning vocabulary.
