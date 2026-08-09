# Effects of the data-munging options on the mined rules

Measured effects of the 2026-07-07 rebuild of `reg_model_data` on the v2
inclusion-rule pipeline. Companion to the frame-provenance notes in CLAUDE.md
and the findings in `methods/modeling_findings.md`.

## What changed in the munging script

`1_data_munging_and_raw_variable_reconstruction_for_using_public_qc_data.R`:

1. **Multi-element error cases are kept.** The old frame excluded every
   case whose review found more than one error element (`ELEMENT2` non-missing)
   — about 31% of all above-threshold errors. A `second_element_i` flag marks
   them, but it is NOT used as a mining feature (state reporting of second
   elements is highly inconsistent).
2. **Deduction-field NAs are zero-filled, not dropped.** `FSDEPDED`, `FSMEDDED`,
   `FSCSDED`, `FSSTDDED`, `HOMELESS_DED` NAs mean "deduction not claimed";
   rows are kept with 0 and flagged via `ded_fields_imputed`. Only rows missing
   `RENT`/`UTIL` are still dropped.
3. **The frame is saved by the script itself** (`saveRDS` at the end), so
   `reg_model_data.rds` always matches the munging code. The previous
   single-element frame is archived at
   `archive_data/reg_model_data_singleelement_stale_2026-07-04.rds`; results
   mined from it live in `*/run3_singleelement_frame/` (INCL) and analogous
   `run*` archive subfolders.

## Effects on the mining frame

- 237,391 rows; ~11% of cases have an above-threshold error (was ~8.4% on the
  stale frame). Train-side any-error base rates by household-size stratum:
  7.7% (HH 1), 14.8% (HH 2-3), 20.4% (HH 4+).
- Visible errors for 2022-24 roughly doubled in the states we had been
  studying (WA, LA, VA) — the drop had been absorbing most of their
  multi-element errors. Full per-state accounting: `methods/state_error_accounting/`.

## Effects on the mined rules (old frame vs rebuilt frame, LCB_Z = 2.326, floor 0.20)

Diff produced by `methods/compare_rule_sets_v2.R` →
`inclusion_rules_by_hh_size_v2/rule_diff_old_vs_new.csv`.

### Inventory: ~3x

| | old frame | rebuilt frame |
|---|---|---|
| combined high-precision shortlist | 3,741 rules | 11,018 rules |

### The old rule set survives nearly intact

Of the 3,741 old rules: **31 exact** matches, **2,367 (63%) same-structure**
rules with shifted thresholds, **1,080 (29%)** matched by a new rule flagging a
highly overlapping case set (Jaccard ≥ 0.5 on 2023), and only **263 (7%)
dropped** with no counterpart. States holding the old shortlist mostly hold the
same patterns with recalibrated cutpoints.

New-set composition by frame (from the diff):

| frame | exact | shifted | overlap | brand-new |
|---|---|---|---|---|
| any_error | 16 | 1,332 | 2,589 | 4,404 |
| other_error | 10 | 695 | 606 | 389 |
| unearned_income | 5 | 319 | 435 | 137 |
| earned_income | 0 | 15 | 2 | 44 |
| underissuance | 0 | 6 | 10 | 4 |

### Rules clear higher confidence floors

Max populated 99% lower-bound floor in the sweep, by frame:

| frame | old | rebuilt |
|---|---|---|
| earned_income | 0.20 | 0.30 |
| underissuance | 0.25 | 0.30 |
| unearned_income | 0.40 | 0.45 |
| other_error | 0.65 | 0.70 |

More errors at the same support tightens each rule's Wilson lower bound, so
more rules clear higher floors. The gain is statistical power for the
guarantee, not necessarily sharper rules.

### The new rules are NOT multi-element specialists

The brand-new rules' 2023 catches are **34% multi-element cases vs 32% in the
error universe** — essentially no concentration. Restoring multi-element cases
did not unlock a distinct "multi-element pattern"; it added error mass across
the board (multi-element cases look like ordinary cases on the mining
features), which raised support everywhere and let ~5,000 additional rules
clear the filter.

### Calibration is unchanged

Frame-level median holdout precision at the 0.20 floor stays in the expected
band (any_error 0.243; typed frames 0.24-0.32) — the winner's-curse control
did not degrade with the bigger error mass.
