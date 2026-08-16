# excel_rules_for_states

Builds, per state, the Excel workbook a state agency uses to run the delivery
rule lists on its own data — with no Python, no R and no macros. One command:

```bash
python methods/excel_rules_for_states/make_state.py WA          # one state
python methods/excel_rules_for_states/make_state.py all         # every listed state
python methods/excel_rules_for_states/make_state.py WA --refresh  # re-export the frame after a munging rebuild
```

Every state with a blended delivery list in the repo's tracked
`state_delivery_lists/` folder resolves automatically from the naming
convention (`blended_delivery_<State>_2022_2024_budget10.csv`, the 10%
review-budget list, `core` rules). Nothing needs adding to `states.py` for a
new state; `OVERRIDES` there is for deviations, and `EXCLUDE` holds states
withheld from batch builds (currently Illinois: the IL_OFFSET standard
deduction is not implemented in the workbook formulas). All lists share one
19-variable vocabulary with at most 4 conditions per rule (checked
2026-08-15; `bbce_state_i` replaced `cat_elig` in the 2026-08-13 list
rebuild).

## The deliverable

**One file per state**: `state_workbooks/<ABBR>/snap_qc_dashboard_<ABBR>.xlsx`.
Its Data tab holds the raw FNS QC-schedule fields as VALUES and every model
feature as an in-workbook FORMULA, so a state pastes the fields it already
reports to FNS and every figure recomputes. The intermediate stages (the
plain frame-values build and the LIVE table build) land in
`.build/out_<ABBR>/`; they exist for verification, not delivery —
`crosscheck_rules.py` reads the plain build's static values.

### The tabs

The rules tabs each present a POTENTIAL rule list for the state to evaluate; nothing is deployed anywhere by this workbook, and nothing in it tunes or modifies the rules.

| Tab | What it shows |
|---|---|
| **Blended Rules** | The BLENDED delivery list — the state's own mined rules merged into the national pool, filled to the 10% review budget — at delivered thresholds, as-is. Its own Include? selection. |
| **National Rules** | The NATIONAL-only delivery list (built purely from the all-state pool), where the repo carries one — 39 of 49 states as of 2026-08-16. Compare against Blended Rules to see what merging the state's own rules adds. |
| **View error cases by rule** | Rules currently catching errors, sorted; the grid shows the true-error cases the selected rule flags. Sized to the build's row count. |
| Data | Raw FNS fields (amber, values — what a state pastes) + model features (blue, formulas). |
| Dashboard, Grid Search, RuleFlags, FederalTables | Hidden engines: the per-rule threshold tuner, the bracket-bounded threshold search, the case × rule hit matrices, and the federal parameter tables. Unhide the Dashboard to tune thresholds interactively. |

### The Data-tab contract

The amber block is what a state supplies: 23 columns carrying generic
reported-value concept names in `features.R`'s `state_col_map` style —
`HOUSEHOLD_SIZE`, `EARNED_INCOME`, `UNEARNED_INCOME`, `MEDICAL_DEDUCTION`,
`DEPENDENT_CARE_DEDUCTION`, `CHILD_SUPPORT_DEDUCTION`, `RENT`,
`UTILITY_COSTS`, unit counts (`NUM_CHILDREN`, `NUM_ELDERLY`, `NUM_DISABLED`,
`NUM_ABAWD`), 0/1 indicators (`MARRIED_FLAG`, `EXPEDITED`,
`CATEGORICALLY_ELIGIBLE`, `HOMELESS_FLAG`), `MONTHS_SINCE_CERT`,
`NUM_AMOUNTS_DIVISIBLE_BY_100` (state-precomputed; its mined definition counts
the QC file's 28 component fields, which a totals-based contract cannot
reproduce), and the state's own QC review outcome (`ERROR_FLAG`,
`ERROR_AMOUNT`). The Data Dictionary tab defines every column and carries the
crosswalk to the SNAP QC technical documentation's variables. A handful of
names deviate from the feature vocabulary only because Excel table column
names are case-insensitively unique (an input may not reuse a feature column's
name — the collision makes Excel reject the file as damaged, and
`make_recon.py` asserts against it).

The demo fills every input column with the research frame's RECONSTRUCTED
(pre-QC-review) values — never the QC-corrected `FS*` values from the public
files. This is a hard design principle (2026-08-16): a state's internal case
data is effectively the reconstructed scale (as-reported, no QC corrections),
and the rules were mined on that scale, so demoing against corrected values
would have states selecting rules that flag corrected case data — the
measured v1 failure mode (21 of 114 WA rules never fired; held-out precision
0.255 vs 0.318). `export_state_frame.R` ships the reconstructed input-level
fields and `make_recon.py` builds the whole input block from that export; the
`.sav` files are not read anywhere in the pipeline. The blue feature columns
are formulas over the amber block — including the benefit recomputation chain
(standard deduction, max allotment, shelter cap and minimum allotment looked
up per year x size from the hidden FederalTables sheet, extendable by
appending a row per fiscal year) and the state-year `bbce_state_i` share
formula. Do not paste values over the blue columns.

Every build validates the formulas: it mirrors each formula in pandas over
the raw extract and compares every formula-computed feature against the research frame and
FAILS THE BUILD unless the match is complete (WA, AL, VT, DE: 100.0% on
every feature, every row). That gate is what enforces the reconstructed-
values principle: if corrected values ever creep back into the input
block, no workbook ships. A consequence worth knowing: the delivered
workbook's live figures now equal the research pipeline's and the R
cross-check's exactly (WA blended: 195 flagged, 71 errors, $16,668 at
36.4% precision).

## Cross-checking the workbook against R

```bash
python methods/excel_rules_for_states/crosscheck_rules.py WA   # exit 0 = everything matches
```

Re-implements the state's delivery rules independently in R
(`crosscheck_rules.R`) over the same years of `reg_model_data.rds`, with the
miner's own `prep_features()`, and compares per rule (n flagged, errors,
dollars, precision, recall, dollar recall, workload) plus the all-rules union
overall and per stratum, recomputed from the workbook's RuleFlags hit
matrix — for the Blended Rules tab AND the National Rules tab where present.
On 2026-08-16: WA (92 blended + 101 national), AL (91 + 121), VT (66 + 66)
and DE (35 blended, no national list) all matched with zero mismatches. It
reads the plain build in `.build/out_<ABBR>/`; the delivered workbook holds
the same numbers as formulas, verified by the open-in-Excel stage.

## Why the workbook carries no tuning

The workbook measures the delivered rule lists; it does not modify them
(decision 2026-08-16). Two measured reasons. First, at public-QC sample sizes
the guarded tuning in `tuning.py` (methods/tuning_principles.md) can never
act: on Washington's 92-rule list the widest rule flags 33 tuning-year cases
against an n >= 30 support floor, threshold moves were zero at every floor
swept, and every tuned arm failed its held-out gate — so a tuning tab only
ever displayed a refusal. Second, the tuning runs in Python at build time:
when a state pastes its own data, the rules tabs recompute but a tuning tab
could not, so it would permanently show a result computed on the public
sample. The unguarded search this replaced (the v1 builder, retired to
`custom_one_off/legacy_dashboard/`) delivered 0.048 precision where the
untouched list held 0.364 — CHANGES_AND_RATIONALE.md records the full case.

`tuning.py` stays in the package for pipeline-side use on a state's internal
data, with its own regression test (`python tuning.py --selfcheck`), and
`compare_splits.py` / `support_floor_sweep.py` reproduce the studies behind
the removal decision.

## How it works

| Stage | Script | Does |
|---|---|---|
| 1 | `build_workbook_v2.py` | Exports the state's rows from `reg_model_data.rds` (via `export_state_frame.R` + the miner's `prep_features()`), parses the blended and national delivery lists, scores every rule at its delivered thresholds, writes every sheet. |
| 2 | `make_live.py` | Data becomes an Excel table; the three rules tabs and the Dashboard recompute from it by column NAME, so pasted rows flow through. |
| 3 | `make_recon.py` | Appends the raw FNS block + benefit-chain helpers, turns every feature column into a formula, validates the formulas against the frame, adds FederalTables. |
| 4 | `postprocess_workbook.py` | Native Excel-365 checkboxes; drops the stale calc chain. Applied to every stage's output. |
| 5 | `verify_workbook.py` (macOS) / `verify_workbook_win.ps1` (Windows) | Opens each workbook in desktop Excel (AppleScript / COM), forces a full recalculation, reads probe cells back, and fails on any formula error cell on any sheet. Needs desktop Excel installed. Formulas written by openpyxl must carry the `_xlfn.` prefix for post-2007 functions (NORM.S.INV, FLOOR.MATH) or Excel renders #NAME? — the verifier's error scan is what catches that class of bug. |

The recon stage then becomes the deliverable, copied to
`state_workbooks/<ABBR>/snap_qc_dashboard_<ABBR>.xlsx`.

## Requirements

```bash
pip install -r methods/excel_rules_for_states/requirements.txt
```

Needs the snap_qc repo checked out (found by walking up from this package, or
`SNAP_REPO`), `reg_model_data.rds` built in it (gitignored, written by the
munging script), `Rscript` on the path or `RSCRIPT` set; the `.sav` files are no
longer read by any stage. The per-state frame export is cached under
`.frames/`; pass `--refresh` (or set `SNAP_REFRESH_FRAME=1`) after the
munging script is re-run. The frame was last rebuilt 2026-08-13; workbooks
built before that date are on a stale row universe and should be rebuilt, not
edited. WA went from 2,356 rows to 1,957: the munging now excludes MFIP and
SSI-CAP cases (non-standard benefit calculations the recreation cannot model;
merged 2026-08-08), and Washington runs an SSI Combined Application Project,
so it lost 399 rows — all single-person elderly/disabled — where the national
frame lost 2.4%.

## Gotchas worth knowing

- **Close the workbook in Excel before rebuilding.** Writing underneath an
  open session causes OneDrive conflict copies, and Excel's autosave can
  overwrite the new file. `make_state.py` checks and refuses.
- **Do not repoint an existing workbook at another state by pasting into
  Data.** Rebuild instead.
- View error cases by rule and the RuleFlags hit matrices are sized to the build's case
  count; the rules tabs do follow pasted rows (they read the Data table); the error-case viewer does not.
- The munged frame restores each field to its pre-QC-process value
  (`correct_variables <- TRUE`); scoring rules against raw `.sav` values
  kills about a fifth of the rules (see CHANGES_AND_RATIONALE.md for the v1
  post-mortem). That is why stage 1 reads the frame, and why the delivered
  workbook's live figures on public data differ from the plain build's.
