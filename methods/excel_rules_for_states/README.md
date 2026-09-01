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
review-budget list). Nothing needs adding to `states.py` for a new state;
`OVERRIDES` there is for deviations, and `EXCLUDE` holds states withheld
from batch builds (currently Illinois: the IL_OFFSET standard deduction is
not implemented in the workbook formulas). All lists share one 19-variable
vocabulary with at most 4 conditions per rule (checked 2026-08-15;
`bbce_state_i` replaced `cat_elig` in the 2026-08-13 list rebuild).

The workbook does not carry the delivery CSV verbatim (2026-08-18,
`rule_selection.py`): rules conditioned on `count_divisible_by_100` are
dropped (it is the one input a state cannot derive from the paste-in
contract's totals), `bbce_state_i` conjuncts that are trivially true for the
state are stripped from the rule text (a state's list already applies only
to that state), and buffer rules are promoted in delivery-rank order to
refill the freed capacity, up to the original core list's union workload on
the state frame. The result — the workbook's EFFECTIVE rule list, sorted by
error dollars caught on the state frame — is written to
`.build/effective_rules_<ABBR>.csv`, which is also what `crosscheck_rules.py`
verifies against. The tracked delivery CSVs are untouched.

## The deliverable

**One file per state**: `state_workbooks/<ABBR>/SNAP_flagging_rules_<ABBR>.xlsx`.
Its "Step 2. Import Testing Data" tab (the Data tab; sheet names live in
`workbook_layout.py`) holds the raw FNS QC-schedule fields as VALUES and every model
feature as an in-workbook FORMULA, so a state pastes the fields it already
reports to FNS and every figure recomputes. The intermediate stages (the
plain frame-values build and the LIVE table build) land in
`.build/out_<ABBR>/`; they exist for verification, not delivery —
`crosscheck_rules.py` reads the plain build's static values.

### The tabs

The rules tab presents a POTENTIAL rule list for the state to evaluate; nothing is deployed anywhere by this workbook, and nothing in it tunes or modifies the rules. Rule selection is a plain TRUE/FALSE value in the yellow Include? column (native Excel-365 checkboxes were dropped 2026-08-18 so the same mechanism works on every Excel version).

| Tab | What it shows |
|---|---|
| Step 1. Data Dictionary | Every Data-tab column: the input fields a state maps its data onto (with the QC technical-manual crosswalk) and the constructed model variables. |
| Step 2. Import Testing Data | Raw FNS fields (light yellow, on the left — what a state pastes) + model features (gray, formulas). |
| **Step 3. Select Rules** | The effective rule list (see above), sorted by error dollars caught on the data present, at delivered thresholds. Per-rule Recall / $ Recall are each rule ALONE as a share of ALL errors / error dollars (rules overlap, so they do not sum to the orange union rows). Include? = TRUE/FALSE per rule. |
| Step 4. Export Rules | The rules still set to TRUE, in order, with plain-English text and exact machine logic — live-shrinks as rules are set FALSE. For filtering in Excel, translating to a query, or sending to a vendor. |
| Step 5.1 Screen New Cases (optional) | A second paste table (`ScreenData`): the Step 2 contract minus the outcome columns, for new cases with no review outcome. Hidden per-rule hit columns auto-extend on paste. |
| Step 5.2 Flagged New Cases (optional) | One row per flagged case × rule from Step 5.1 (case ID, household size, benefit amount, rule id, plain-English rule). First 5,000 pairs shown, total in B3; enumeration is a binary-search MATCH on a running pair count + AGGREGATE for the j-th matching rule (COM-verified 2026-08-18). |
| Step 6. Share results back (optional) | Per-rule aggregates on the pasted Step 2 data to send back (copy the sheet as values into a new workbook): flagged, errors, precision, $ recall, and ineligible-household catches (STATUS = 4 — not in the public QC sample, so a separate count), plus metadata inputs (years, QC/QA/pre-auth) and poolable denominators. Every rule alone, independent of Include?. |
| **See cases flagged by a rule** | Rules currently catching errors, sorted; the grid shows the cases the selected rule flags — including pasted rows (matching moved into hidden `_view_*` columns of the CaseData table, 2026-08-18; first 60 matches shown), with the rule's columns highlighted in blue. |
| FederalTables (hidden) | Reference: max shelter, minimum allotment and the QC error threshold by fiscal year; standard deductions and max allotments by year × size; the state's USDA state-options rows (BBCE etc.). Unhide to inspect or to append a row per new fiscal year. |
| Dashboard, Grid Search, RuleFlags | Hidden engines: the per-rule threshold tuner, the bracket-bounded threshold search, the case × rule hit matrices. Unhide the Dashboard to tune thresholds interactively. |

### The Data-tab contract

The light-yellow block is what a state supplies: 23 columns carrying generic
reported-value concept names in `features.R`'s `state_col_map` style —
`HOUSEHOLD_SIZE`, `EARNED_INCOME`, `UNEARNED_INCOME`, `MEDICAL_DEDUCTION`,
`DEPENDENT_CARE_DEDUCTION`, `CHILD_SUPPORT_EXPENSES` (child support
payments; in most cases the same as the deduction amount, and the original
expenses also cover cases where child support is excluded from income
instead of deducted — see its dictionary entry), `RENT`,
`UTILITY_COSTS`, unit counts (`NUM_CHILDREN`, `NUM_ELDERLY`, `NUM_DISABLED`,
`NUM_ABAWD`), 0/1 indicators (`MARRIED_FLAG`, `EXPEDITED`,
`CATEGORICALLY_ELIGIBLE`, `HOMELESS_FLAG`), `MONTHS_SINCE_CERT`, and the QC
review outcome as the benefit pair plus disposition (2026-08-18):
`ORIGINAL_BENEFIT_AMOUNT` (QC manual RAWBEN — the benefit as issued),
`CORRECTED_BENEFIT_AMOUNT` (FSBEN), and `STATUS` (2 = overissuance,
3 = underissuance, 4 = ineligible household). The workbook computes
`total_error_amount` = ROUND(ABS(original − corrected)) and flags
`over_threshold` when it exceeds the review year's federal QC tolerance
(FederalTables `error_threshold`) — the munging script's own definitions, so
the demo matches the frame exactly (the earlier `ERROR_FLAG`/`ERROR_AMOUNT`
paste-in columns and the `NUM_AMOUNTS_DIVISIBLE_BY_100` precompute are
gone). The Step 1 Data Dictionary tab defines every column and carries the
crosswalk to the SNAP QC technical documentation's variables. A handful of
names deviate from the feature vocabulary only because Excel table column
names are case-insensitively unique (an input may not reuse a feature column's
name — the collision makes Excel reject the file as damaged, and
`make_input_workbook.py` asserts against it).

The demo fills every input column with the research frame's RECONSTRUCTED
(pre-QC-review) values — never the QC-corrected `FS*` values from the public
files. This is a hard design principle (2026-08-16): a state's internal case
data is effectively the reconstructed scale (as-reported, no QC corrections),
and the rules were mined on that scale, so demoing against corrected values
would have states selecting rules that flag corrected case data — the
measured v1 failure mode (21 of 114 WA rules never fired; held-out precision
0.255 vs 0.318). `export_state_frame.R` ships the reconstructed input-level
fields and `make_input_workbook.py` builds the whole input block from that export; the
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

Re-implements the workbook's EFFECTIVE rule list
(`.build/effective_rules_<ABBR>.csv`) independently in R
(`crosscheck_rules.R`) over the same years of `reg_model_data.rds`, with the
miner's own `prep_features()`, and compares per rule (n flagged, errors,
dollars, precision, recall, dollar recall, workload) plus the all-rules union
overall and per stratum, recomputed from the workbook's RuleFlags hit
matrix. Because R re-derives each mask from the transformed rule text, a
bbce strip that changed any flag set would surface here. On 2026-08-18: WA
(89 effective rules from 92 core: 3 div-100 dropped, 6 bbce-stripped)
matched with zero mismatches on all 8 fields and all 4 union scopes. It
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
| 1 | `build_workbook_v2.py` | Exports the state's rows from `reg_model_data.rds` (via `export_state_frame.R` + the miner's `prep_features()`), derives the effective rule list from the blended delivery CSV (`rule_selection.py`: div-100 drop, bbce strip, buffer promotion, dollars-caught sort), scores every rule at its delivered thresholds, writes every sheet. |
| 2 | `make_live.py` | Data becomes an Excel table; the rules tab and the Dashboard recompute from it by column NAME, so pasted rows flow through. Shares its rule-to-formula translation with stage 3 via `live_formulas.py`. |
| 3 | `make_input_workbook.py` | Appends the raw FNS block + benefit-chain helpers, turns every feature column into a formula (including the recomputed QC outcome), validates the formulas against the frame, adds Start Here / the Step 1 dictionary / FederalTables / the Step 5 screening tabs / the Step 6 share-back tab. |
| 4 | `postprocess_workbook.py` | Drops the stale calc chain (its checkbox stage is retired: Include? is plain TRUE/FALSE since 2026-08-18). Applied to every stage's output. |
| 5 | `verify_workbook.py` (macOS) / `verify_workbook_win.ps1` (Windows) | Opens each workbook in desktop Excel (AppleScript / COM), forces a full recalculation, reads probe cells back, and fails on any formula error cell on any sheet. Needs desktop Excel installed. Formulas written by openpyxl must carry the `_xlfn.` prefix for post-2007 functions (NORM.S.INV, FLOOR.MATH) or Excel renders #NAME? — the verifier's error scan is what catches that class of bug. |

The recon stage then becomes the deliverable, copied to
`state_workbooks/<ABBR>/SNAP_flagging_rules_<ABBR>.xlsx`.

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

- **Never put an array-evaluated expression inside a per-row table
  formula.** `MODE.SNGL(IF(CaseData[year]=..., ...))` in a calculated
  column collapses under implicit intersection to a single cell, and an
  enclosing `IFERROR` turns the failure into a clean 0 on every row: the
  error scan passes, the pandas validation gate passes (it mirrors the
  intended formula, not Excel's evaluation), and only the union count
  reveals it (2026-08-22, the SUA-tier feature: 413 flagged in the
  delivered file vs 193 in the plain build). Per-year aggregates live in
  an array-entered block on FederalTables (`write_sua_mode_block`, written
  after the table is rebound) and each row does a plain `INDEX/MATCH`.
  The permanent guard: `make_state.py` recomputes the all-rules union
  from the plain build's static RuleFlags matrix and passes it to the
  verifier as `Sheet!Cell=expected` probes on the delivered file; a live
  union that drifts from the static one fails the build.

- **Close the workbook in Excel before rebuilding.** Writing underneath an
  open session causes OneDrive conflict copies, and Excel's autosave can
  overwrite the new file. `make_state.py` checks and refuses.
- **Do not repoint an existing workbook at another state by pasting into
  Data.** Rebuild instead.
- The RuleFlags hit matrices are static (sized to the build's case count) but feed only
  the union rows via the live selection vector; the rules tab, the case viewer, and the
  Step 4-6 tabs all follow pasted rows (they read the CaseData/ScreenData tables).
- The munged frame restores each field to its pre-QC-process value
  (`correct_variables <- TRUE`); scoring rules against raw `.sav` values
  kills about a fifth of the rules (see CHANGES_AND_RATIONALE.md for the v1
  post-mortem). That is why stage 1 reads the frame, and why the delivered
  workbook's live figures on public data differ from the plain build's.
