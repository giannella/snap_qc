# Where the work stands — resume notes

A working memory for picking the project back up after a restart or a new
session. The durable record of *findings* is `methods/modeling_findings.md`;
this file is the shorter "what's live, what's next, how we work" companion.
Last updated 2026-07-17.

## The validated pipeline (what ships)

The recommended deliverable is the **blended frozen delivery list** per state,
built by `INCL_build_blended_delivery_list_v2.R` (batch:
`runners/run_blended_delivery_batch.R`), output to `state_delivery_lists/`.
The machinery is a general-purpose evidence core plus a user-chosen
statistic-goal module (README "Statistics and goal metrics"):

- **Mine** candidate rules with xgboost + ranger, depth 4, per household-size
  stratum (1 / 2-3 / 4+), on the any-error target.
- **Admit** a rule with **Benjamini-Hochberg at FDR 10% vs the stratum base
  rate, AND at least 30 flagged training cases** (v2.3.0; findings 19). The
  two guards are different: FDR controls the fluke share, the 30-case floor
  keeps poorly-measured rules out of the top of the ranking. Neither replaces
  the other. `ADMISSION <- "legacy"` restores the pre-2.3.0 raw-precision
  filter.
- **Order / rank** by the **one-sided 99% Wilson lower confidence bound** of
  training precision — validated on two independent eras (findings 20); this
  did NOT change in v2.3.0.
- **Fill** to the review budget (5% / 10% of caseload) as "core", plus
  substitute rules to 3x depth as "buffer"; walk in rank order, outcome-free.

Performance (train 2022-23, tested a year ahead on 2024): median deployed
precision ~0.32 at 5% / ~0.26 at 10%, every state above its base error rate.

## Retired in writing (do not revisit without new evidence)

All in `methods/modeling_findings.md`, sections 17-22:

- Typed (five-frame) delivery vocabulary — worse at budget fill; three rescue
  attempts failed (17).
- Empirical-Bayes / shrinkage ranking — refuted on two eras (18).
- Floorless FDR admission — the support floor is not optional (19).
- Raising the ordering stringency above z = 2.326 — the 2024 hint did not
  replicate on 2019 (20).
- Dollar-yield ranking as a default — real direction, era-unstable magnitude;
  a labeled *option*, not adopted (21). A structure-anchored dollar statistic
  (credit size only as far as flagged cases' benefit levels justify) is the
  untested follow-up.

## Open roadmap

- Structure-anchored dollar statistic (the one dollar-goal idea not yet tried).
- A rank-position-aware ordering reliability criterion (the principled
  replacement for the fixed-z bound that findings 20-22 point toward but that
  nothing has yet beaten).
- The `family_id` labeled-substitutes deliverable column (design decided,
  not built).
- Low-visibility states (< ~60% of error cases in public data) should mine
  internally; the hybrid internal+national blend is documented but untested at
  a real state.

## How the studies run (and resume)

Every long study is a `runners/run_*.R` script sourcing a `methods/*_v2.R`
driver, launched detached (PowerShell `Start-Process`, high priority) with a
Monitor watching the log. Mines checkpoint per frame/pool to gitignored `.rds`
caches, so a killed run **resumes from the last checkpoint on re-launch** —
nothing re-mines from scratch. Raw vocabularies, dollar caches, and pool
caches are all gitignored (large); only scripts and result CSVs are tracked.
Benchmark artifacts live under
`methods/state_similarity_v2/transfer_benchmark_train2223_test24/` (2024) and
`.../era_validation_train1718_test19/` (2019).

Data: `reg_model_data.rds` (workspace root, gitignored) is the input to
essentially everything. The raw QC `.sav` files (`C:/Users/ericg/qc/`) are
only needed to rebuild it.

## How we work together (see also memory files and CLAUDE.md)

- **Ask before user-facing changes.** Anything affecting how state agencies
  use or learn the code — READMEs, CHANGELOG, VERSIONING, delivery filenames
  or schemas, figures embedded in docs, releases/tags — is presented and
  decided together, not pushed autonomously. Internal `methods/` research
  artifacts are fine to commit directly.
- **Versions track the user contract, not research activity** (VERSIONING.md
  "Release cadence and restraint"). Auditions and retired claims accumulate
  quietly; a release happens only when the recommended workflow or its
  published artifacts change. Major (v3+) is rare; decimals are the norm.
- **Nothing enters the recommended workflow without held-out-year validation**
  — ideally two eras. Failures are retired in writing. This discipline is the
  product's credibility and is drawn in `presentation_figures/refinement_loop.png`.
- **Write-ups persuade with our own numbers, no boasting** (CLAUDE.md
  "Presentations and write-ups"). Plain language; every claim carries its
  measurement.
- Prefer redundant high-precision rules; use {ranger} for forests. (Memory.)

## Decks (local, mostly uncommitted)

`slides/lessons_getting_more_signal_from_snap_qc_data.pptx` is the presented deck;
earlier revisions and backups are in gitignored `do_not_commit_decks/`.
Deck edits are held locally and committed only when asked. Slide rendering
used Windows PowerPoint automation on the host; in the container use
LibreOffice headless as a rough preview.
