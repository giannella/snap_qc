# additional_data

Lookup tables used by the SNAP QC modelling code, plus the SNAP State Options
panel and the script that builds it.

## SNAP State Options panel

| file | what it is |
|---|---|
| `snap_state_options_all_years.csv` | the panel: one row per jurisdiction-year, `Year` and `State` followed by the same 19 option columns as `snap_state_options_2023.csv` |
| `snap_state_options_long_all_years.csv` | every option read from every source, one row per jurisdiction-option, including options that have no column in the panel. Carries the source file name |
| `snap_state_options_pipeline.R` | builds both: `source(...)` then `run_pipeline(".", write_dir = ".")` |
| `diag_unmatched_labels.csv` | option labels with no column in the panel, by edition. Mostly options the 2023 schema does not carry, which is expected; see below |

The panel holds 318 rows: all 53 jurisdictions in each of six editions.

### Sources

Every year is read from the original FNS report. `Year` is the edition's
publication year throughout. Each edition reports options in effect as of
October 1 of the year before, so the publication year and the as-of year differ
by one. The file names do not agree on which of the two they use, which is why
`snap_state_options_pipeline.R` lists its sources explicitly in `EDITIONS`
rather than reading years off file names.

| Year | Edition | Options as of | Source file |
|---|---|---|---|
| 2016 | 12th | Oct 1, 2015 | `12-State_Options_Oct1_2015.pdf` |
| 2017 | 13th | Oct 1, 2016 | `state_options_revised_2016.pdf` |
| 2018 | 14th | Oct 1, 2017 | `snap_stateOptionsReport_2018.pdf` |
| 2023 | 15th | Oct 1, 2022 | `snap_stateOptionsReport_2023.pdf` |
| 2024 | 16th | Oct 1, 2023 | `snap-stateOptionsReport_2024.pdf` |
| 2025 | 17th | not stated on the cover | `snap-stateOptionsReport_2025.pdf` |

All six live in `state options reports/`.

**The PDFs are not tracked in this repository.** They are about 31 MB of public
FNS documents and are gitignored to keep them out of git history. To rebuild the
panel, download them into `state options reports/` under the file names above.
They are published at:

https://www.fna.usda.gov/snap/waivers/state-options-report

The two CSV outputs are tracked, so nothing needs to be downloaded to use the
panel. The PDFs are only needed to regenerate it.

### How the reports are laid out

Every edition gives each jurisdiction a page holding a two-column table of
option against selection, and that is what the parser reads. The rest of the
layout varies:

- The 12th to 14th editions open with a section of one page per option, giving
  prose and the list of jurisdictions that chose it, and only then the
  per-jurisdiction pages. The parser ignores the first section.
- The 15th to 17th editions have per-jurisdiction pages only.
- The 17th edition splits each jurisdiction across three sub-tables (policy
  options, demonstration projects, waivers), each with its own header.

The parser works from word coordinates rather than from extracted text, because
`pdf_text()` flattens the two columns into one stream and cannot be unflattened
once an option selection wraps across lines.

### Coverage

All 53 jurisdictions are present in all six years. A blank cell means the
edition did not report that option, not that the jurisdiction had nothing to
report. Nothing is imputed.

| column | blank in |
|---|---|
| `SNAPETPrograms` | 2016, 2017, 2018 |
| `ABAWDTimeLimitWaiver` | 2016, 2017; and Connecticut in 2023, whose page has no such row |
| `ABAWDDiscretionaryExemptions` | 2016, 2017 |
| `SelfEmploymentIncome` | 2025 |
| `IneligibleNoncitizensPrePRWORA` | 2016, 2025 |
| `IneligibleNoncitizensPostPRWORA` | 2016, 2025; and one jurisdiction in 2017, see below |

The 12th edition reports one combined ineligible-noncitizen row rather than the
pre- and post-PRWORA split, so both of those columns are empty for 2016.

One jurisdiction's 2017 `IneligibleNoncitizensPostPRWORA` is blank. On the
densest pages of that edition a new table row starts 13 pt below the last, which
is the same spacing as a wrapped line, so the two ineligible-noncitizen rows
cannot be told apart by geometry. The cell was left blank rather than guessed.

Two 14th edition pages differ from the rest of that edition, and both are
properties of the document rather than of the parser: Guam's page carries an
extra self-employment row, and Missouri's page still uses the 13th edition's
names for two options. Neither costs a panel column.

### Options with no column in the panel

The panel carries the 19 columns of `snap_state_options_2023.csv` and nothing
else. Options that other editions report but that file does not are in
`snap_state_options_long_all_years.csv` only: the 17th edition's nine waivers
and Pledge States, the 16th edition's second work-requirements row, and the
technology and process options the 12th to 14th editions carry (Call Centers,
Mobile Technology, Online Application, Online Case Management, Electronic Case
Files, Document Imaging, Treatment of Vehicles, Joint Processing, Simplified
Homeless Housing Cost, and others).

Where the 16th and 17th editions split ESAP and SMD into two yes/no rows, the
panel folds them back into the 15th edition's single four-level column
(`SMD and ESAP` / `SMD only` / `ESAP only` / `No SMD or ESAP`) so the column
means the same thing every year. The raw pair stays in the long file.

Values are kept as printed in each source. Wording changes between editions and
is not harmonised: BBCE reads `Yes` / `No` in the 12th to 14th editions and
`BBCE` / `No BBCE` from the 15th on.

### Checks

Run after each rebuild:

- 2023 reproduces `snap_state_options_2023.csv`, the hand-checked reference, on
  779 of 779 cells.
- Every cell traces back to its own source PDF: 2016 742/742, 2017 847/847, 2018
  954/954, 2023 1006/1006, 2024 954/954, 2025 795/795.
- The derived ESAP/SMD column agrees with the raw pair, 53/53 in both 2024 and
  2025.
- The 14th edition can be checked against itself, since it reports the same facts
  twice in different layouts. Its option-page list of county-administered
  jurisdictions and the values read from its per-jurisdiction pages give the same
  ten: California, Colorado, Minnesota, New Jersey, New York, North Carolina,
  North Dakota, Ohio, Virginia, Wisconsin.
- Every option label in 2023, 2024 and 2025 maps to a panel column, so
  `diag_unmatched_labels.csv` holds nothing for those years.

`diag_unmatched_labels.csv` is not an error log. Its 2016, 2017 and 2018 entries
are options the 2023 schema does not carry, such as Call Centers and Mobile
Technology, which live in the long file instead. Two entries are worth knowing
about: the 12th edition's single combined `treatment of income and deductions of
ineligible non citizens` row, and `online case management call centers` for 14
jurisdictions, the one row fusion in that edition. Neither affects a panel
column.

### Files kept but not read

Three CSVs are earlier extractions of editions the panel now reads from the PDF.
Each was compared against its own PDF before being set aside. They are kept for
reference and are not inputs.

| file | edition | agreement with its PDF |
|---|---|---|
| `snap_options_2016.csv` | 12th | 93.1% of 1,239 cells exact; about 24 cells carry text from a neighbouring table row |
| `snap_options_2017.csv` | 13th | 89.1% of 1,322 cells exact; about 76 carry page footers or text from following rows. Its `Treatment of Child Support Payments` cell absorbed the two ineligible-noncitizen rows for all 53 jurisdictions, which is why that file has no such categories |
| `snap_options_2018.csv` | 14th | 91.1% of 1,070 cells exact. Covers only 45 of the 53 jurisdictions, stopping partway through the alphabet, and omits the ABAWD and ineligible-noncitizen options |

In all three the remainder is mostly a shorter wording of the same answer, `No`
where the PDF says `No mobile technology`.

`snap_state_options_2023.csv` is the hand-checked 2023 reference the panel is
validated against, and is also not an input.

## Other lookup tables

| file | contents |
|---|---|
| `max_allotments.csv` | maximum monthly allotment by year and household size 1-20 |
| `standard_deductions.csv` | standard deduction by year and household size 1-20 |
| `standard_medical_deductions.csv` | standard medical deduction by state, 2017-2024 |
| `year_data.csv` | per-year parameters: error threshold, minimum allotment, maximum shelter deduction, Medicare Part B premium |
| `state_data.csv` | state name to FIPS code |
| `qc_elements.csv` | QC element code to description |
| `qc_natures.csv` | QC nature-of-error code to description |
| `snap_qc_exclusion_all_years.csv` | per state, month and stratum: sampling interval, sample sizes, SNAP units, ineligible and failing units, and the derived weights and rates |
