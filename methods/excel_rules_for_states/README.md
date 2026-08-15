# excel_rules_for_states

Builds the SNAP QC inclusion-rule dashboard workbook for a state, in one command.
No macros, no manual steps — the output opens cleanly in any recent Excel.

```bash
python methods/excel_rules_for_states/make_state.py WA --v2            # tiered, guarded tuning
python methods/excel_rules_for_states/make_state.py WA --v2 --recon    # + live table + raw-FNS-field Data tab
python methods/excel_rules_for_states/make_state.py all --v2 --recon   # every state with a delivery list
python methods/excel_rules_for_states/make_state.py WA --v2 --refresh  # re-export the frame after a munging rebuild
```

Every state with a blended delivery list in the snap_qc repo's tracked
`state_delivery_lists/` folder resolves automatically from the naming
convention (`blended_delivery_<State>_2022_2024_budget10.csv`, the 10%
review-budget list, `core` rules). Nothing needs adding to `states.py` for a
new state; `OVERRIDES` there is for deviations only. All 49 lists share one
19-variable vocabulary with at most 4 conditions per rule (checked 2026-08-15;
`bbce_state_i` replaced `cat_elig` in the 2026-08-13 list rebuild).

## Three workbook variants per state

| variant | Data tab | for |
|---|---|---|
| `snap_qc_dashboard_<ABBR>.xlsx` | feature values from the munged frame | the reference build; its Summary (National) numbers are the ones `crosscheck_rules.py` verifies against R |
| `..._LIVE.xlsx` | same values, bound into an Excel table | pasting more case rows: every Summary figure recomputes |
| `..._RECON.xlsx` | raw FNS QC-schedule fields as VALUES; every model feature an in-workbook FORMULA | states that can run neither Python nor R: they paste the fields they already report to FNS, under the FNS field names |

### The RECON Data-tab contract

The amber block on the right of the Data tab is what a state supplies: 47
columns carrying FNS QC-schedule names (`FSUSIZE`, `FSEARN`, `FSUNEARN`,
`RENT`, `UTIL`, `LASTCERT`, the 21 income-type and 7 deduction-type fields for
`count_divisible_by_100`, ...), two compressed person-level counts
(`NUM_ABAWD` = members with ABWDST1-18 in 2..5, `MARRIED_I` = any REL1-16 = 2),
and the state's own QC review outcome (`ERROR_FLAG`, `AMTERR`; not named
OVER_THRESHOLD because Excel table column names are case-insensitively unique
and the feature column `over_threshold` claims that name — a collision makes
Excel reject the file as damaged, and `make_recon.py` asserts against it). The blue
feature columns are formulas over that block — including the benefit
recomputation chain (standard deduction, max allotment, shelter cap and
minimum allotment looked up per year x size from the hidden FederalTables
sheet, extendable by appending a row per fiscal year) and the state-year
`bbce_state_i` share formula. Do not paste values over the blue columns.

Every `--recon` build validates its formulas: it mirrors each formula in
pandas over the raw extract and prints the match rate against the munged
frame, overall and on rows carrying no QC error element. On WA and VT
(2026-08-15), every feature matched 100% on element-free rows. Rows the pre-QC
restoration touched differ by construction on public data — the delivery rules
were mined on restored values while the raw block holds as-reported values —
which is why the RECON tab's live figures deviate from the reference build's
on public data. For state-supplied as-reported data that gap does not exist.
One caveat for automation: the munging applies an Illinois-specific standard
deduction offset (`IL_OFFSET` in `standard_deductions.csv`) that the workbook
formula does not implement; check the IL validation block before shipping IL.

## Cross-checking the workbook against R

```bash
python methods/excel_rules_for_states/crosscheck_rules.py WA   # exit 0 = everything matches
```

Re-implements the state's delivery rules independently in R
(`crosscheck_rules.R`) over the same years of `reg_model_data.rds`, with the
miner's own `prep_features()`, and compares per rule (n flagged, errors,
dollars, precision, recall, dollar recall, workload) plus the all-rules union
overall and per stratum, recomputed from the workbook's RuleFlags hit matrix.
On 2026-08-15 both WA (92 rules) and VT (66 rules) matched with zero
mismatches. Run it against the plain build (static values); the LIVE/RECON
variants hold the same numbers as formulas, so verify those by opening them in
Excel once.

## Two builders

**Use `--v2` for anything a state will act on.** v1 (`build_workbook.py`)
searches every threshold over the 2nd-to-98th percentile of the state's own
data, keeps whatever maximises error dollars at raw precision >= 25% with
>= 10 cases flagged, and judges it on the same rows it searched. That is the
winner's curse with nothing holding it back.

v2 (`build_workbook_v2.py` + `tuning.py`) implements the tiered procedure and
guards in `snap_qc/methods/tuning_principles.md`:

| Tier | What may change | Gate |
|---|---|---|
| 0 (default) | nothing; the delivered list re-filled against the state's caseload | none needed, uses no outcomes |
| 1 | which rules are used and in what order; rule text frozen | Benjamini-Hochberg at FDR 10% vs the state's own stratum base rate, n >= 30 |
| 2 | numeric thresholds, inside ±25% of their delivered values | Tier 1 admission, plus a Wilson bound whose confidence rises with the number of combinations searched |

The guards: the split is by fiscal year with the most recent held out (never
random), the n >= 30 support floor applies to the variant actually deployed,
Tier 2 is refused below 30 admitted rules, and the held-out year decides one
pre-declared comparison **per tier** rather than one per rule. The untuned arm
is always computed beside the tuned one, and the number of distinct threshold
combinations evaluated is printed and written to the workbook.

`tuning.py` carries its own regression test, which needs no state data:

```bash
python methods/excel_rules_for_states/tuning.py --selfcheck
```

Expect Tier 0 on public data: a budget-filled delivery list is made of narrow
rules. On the 2026-08-13 frame, Washington's 92-rule 10% list has exactly one
rule clearing n >= 30 on the tuning years (widest flags 33); it is admitted by
BH but the one-rule Tier 1 arm flags nothing on the holdout, so Tier 0
deploys. Tier 0 itself: 60 rules fit the 10% budget; on held-out FY2024 they
flagged 31 cases at 0.419 precision against a 9.0% base rate, catching 21.3%
of errors and 27.1% of error dollars at 4.6% workload.

## Checking a verdict against a different year split

```bash
python methods/excel_rules_for_states/compare_splits.py WA     # run the builder first
```

On the 2026-08-13 frame the verdict replicates across splits: Tier 0 both
ways. Forward (tune 2022+2023, hold out 2024): 1 rule clears the floor, 60
deployed, held-out precision 0.419 on 31 flagged. Year swap (tune 2022+2024,
hold out 2023): 0 clear, 74 deployed, held-out precision 0.258 on 62 flagged —
labelled INTERPOLATED because the held-out year sits between the tuning years.
The precision figures are not comparable across splits (different list lengths
and workloads, 13 and 16 caught errors); read the verdict rows.

## What it produces

`state_workbooks/<ABBR>/snap_qc_dashboard_<ABBR>[_LIVE|_RECON].xlsx` (inside this package)
(or `SNAP_OUT_DIR` if set), with these sheets:

| Sheet | For | Notes |
|---|---|---|
| **Summary** | reviewers | deployed vs national thresholds per rule, with the tier and its reason in the header. Ordered by expected error $ per flagged case. Tick **Include?** to mix rules; the combined rows recompute live. |
| **Tuning Audit** | reviewers | The split, the tier decision and why, how many things were compared, each arm's held-out performance, and every per-rule admission test. Deliberately static: it records a past decision. |
| **Summary (National)** | reviewers | The delivery rules at their national thresholds, as-is, in delivery-rank order. Its own independent Include? selection. |
| **Error Cases** | reviewers | Rules currently catching errors, sorted; the grid shows the true-error cases the selected rule flags. Sized to the build's row count — a rebuild, not a paste, after row-count changes. |
| Dashboard | engine (hidden; visible in RECON) | One tuning block per rule; every metric elsewhere reads from here. |
| Grid Search | engine (hidden) | Formula-based threshold search, bounded to the ±25% Tier-2 bracket, gated on a multiplicity-adjusted precision bound. |
| Data | reference / input | plain + LIVE: the state's munged-frame cases. RECON: raw FNS fields (values) + features (formulas). |
| RuleFlags | engine (hidden) | Case × rule hit matrices and the selection vectors behind the Include? checkboxes. |
| FederalTables | engine (hidden, RECON only) | Federal parameters by fiscal year x household size. |

## How it works

| Stage | Script | Does |
|---|---|---|
| 1 | `build_workbook_v2.py` | Exports the state's rows from `reg_model_data.rds` (via `export_state_frame.R` + the miner's `prep_features()`), parses the delivery rules, runs the tiered tuning (`tuning.py`), writes every sheet. |
| 2 | `make_live.py` | Data becomes an Excel table; both Summary tabs and the Dashboard recompute from it by column NAME, so pasted rows flow through. |
| 3 | `make_recon.py` | Appends the raw FNS block + benefit-chain helpers, turns every feature column into a formula, validates the formulas against the frame, adds FederalTables. |
| 4 | `postprocess_workbook.py` | Native Excel-365 checkboxes; drops the stale calc chain. Applied to every variant. |
| 5 | `verify_workbook.py` (macOS) / `verify_workbook_win.ps1` (Windows) | Opens every produced workbook in desktop Excel (AppleScript / COM), forces a full recalculation, reads probe cells back, and fails on any formula error cell on any sheet. Needs desktop Excel installed. Formulas written by openpyxl must carry the `_xlfn.` prefix for post-2007 functions (NORM.S.INV, FLOOR.MATH) or Excel renders #NAME? — the verifier's error scan is what catches that class of bug. |

## Requirements

```bash
pip install -r methods/excel_rules_for_states/requirements.txt
```

Needs the snap_qc repo checked out (found by walking up from this package, or
`SNAP_REPO`), `reg_model_data.rds` built in it (gitignored, written by the
munging script), `Rscript` on the path or `RSCRIPT` set, and for `--recon` the
public `qc_data/*.sav` files. The per-state frame export is cached under
`.frames/`; pass `--refresh` (or set `SNAP_REFRESH_FRAME=1`) after the munging
script is re-run. The frame was last rebuilt 2026-08-13; workbooks built
before that date are on a stale row universe and should be rebuilt, not
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
  Data.** Both Summary tabs' per-rule rows in the plain build are snapshots;
  rebuild instead.
- Error Cases and the RuleFlags hit matrices are sized to the build's case
  count; row-count changes need a rebuild. In the LIVE/RECON variants the
  Summary tabs do follow pasted rows (they read the table), Error Cases does
  not.
- The munged frame restores each field to its pre-QC-process value
  (`correct_variables <- TRUE`); scoring rules against raw `.sav` values kills
  about a fifth of the rules (see CHANGES_AND_RATIONALE.md for the v1
  post-mortem). That is why stage 1 reads the frame, and why the RECON tab's
  live figures on public data differ from the reference build's.
