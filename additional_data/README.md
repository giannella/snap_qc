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

The panel holds 310 rows: 53 jurisdictions in each edition except 2018, which
has 45.

### Sources

`Year` is the edition's publication year throughout. Each edition reports
options in effect as of October 1 of the year before, so the publication year
and the as-of year differ by one. The file names do not agree on which of the
two they use, which is why `snap_state_options_pipeline.R` lists its sources
explicitly in `EDITIONS` rather than reading years off file names.

| Year | Edition | Options as of | Source file |
|---|---|---|---|
| 2016 | 12th | Oct 1, 2015 | `state options reports/12-State_Options_Oct1_2015.pdf` |
| 2017 | 13th | Oct 1, 2016 | `state options reports/state_options_revised_2016.pdf` |
| 2018 | 14th | Oct 1, 2017 | `snap_options_2018.csv` |
| 2023 | 15th | Oct 1, 2022 | `state options reports/snap_stateOptionsReport_2023.pdf` |
| 2024 | 16th | Oct 1, 2023 | `state options reports/snap-stateOptionsReport_2024.pdf` |
| 2025 | 17th | not stated on the cover | `state options reports/snap-stateOptionsReport_2025.pdf` |

Every year except 2018 is read from the original PDF.

**The PDFs are not tracked in this repository.** They are about 31 MB of public
FNS documents and are gitignored to keep them out of git history. To rebuild the
panel, download them into `state options reports/` under the file names in the
table above. They are published at:

https://www.fna.usda.gov/snap/waivers/state-options-report

The two CSV outputs are tracked, so nothing needs to be downloaded to use the
panel. The PDFs are only needed to regenerate it.

### Missing jurisdictions

2018 covers 45 of the 53 jurisdictions. `snap_options_2018.csv` stops partway
through the alphabet, so these eight are absent from that year and only that
year:

Utah, Vermont, Virgin Islands, Virginia, Washington, West Virginia, Wisconsin,
Wyoming

Filling them means reading the 14th edition PDF, which is laid out as prose plus
one page per option listing the jurisdictions that chose it. That needs a
different parser than the one in `snap_state_options_pipeline.R`, which reads
per-jurisdiction tables.

### Empty cells

A blank cell means the edition did not report that option, not that the
jurisdiction had nothing to report. Nothing is imputed.

- 2025 does not report `SelfEmploymentIncome`, `IneligibleNoncitizensPrePRWORA`
  or `IneligibleNoncitizensPostPRWORA`.
- 2016 reports one combined ineligible-noncitizen row rather than the pre- and
  post-PRWORA split, so both of those columns are empty that year.
- 2016, 2017 and 2018 predate `SNAPETPrograms`, `ABAWDTimeLimitWaiver` and
  `ABAWDDiscretionaryExemptions`.
- Connecticut's 2023 page has no ABAWD Time Limit Waiver row.
- One jurisdiction's 2017 `IneligibleNoncitizensPostPRWORA` is blank. On the
  densest pages of that edition a new table row starts 13 pt below the last,
  which is the same spacing as a wrapped line, and the two ineligible-noncitizen
  rows cannot be told apart by geometry. The cell was left blank rather than
  guessed.

### Options with no column in the panel

The panel carries the 19 columns of `snap_state_options_2023.csv` and nothing
else. Options that later or earlier editions report but that file does not are
in `snap_state_options_long_all_years.csv` only: the 17th edition's nine
waivers and Pledge States, the 16th edition's second work-requirements row, and
the technology options the 12th and 13th editions carry (Call Centers, Mobile
Technology, Online Application, Online Case Management, Electronic Case Files,
Document Imaging, Treatment of Vehicles, Joint Processing, and others).

Where the 16th and 17th editions split ESAP and SMD into two yes/no rows, the
panel folds them back into the 15th edition's single four-level column
(`SMD and ESAP` / `SMD only` / `ESAP only` / `No SMD or ESAP`) so the column
means the same thing every year. The raw pair stays in the long file.

Values are kept as printed in each source. Wording changes between editions and
is not harmonised: BBCE reads `Yes` / `No` in the 12th and 13th editions and
`BBCE` / `No BBCE` from the 15th on.

### Checks

Run after each rebuild:

- 2023 reproduces `snap_state_options_2023.csv`, the hand-checked reference, on
  779 of 779 cells.
- Every cell traces back to its own source: 2016 742/742, 2017 847/847, 2023
  1006/1006, 2018 628/628.
- The derived ESAP/SMD column agrees with the raw pair, 53/53 in both 2024 and
  2025.
- Every option label in 2023, 2024 and 2025 maps to a panel column, so
  `diag_unmatched_labels.csv` holds nothing for those years.

That file is not an error log. Its 2016, 2017 and 2018 entries are options the
2023 schema does not carry, such as Call Centers and Mobile Technology, which
live in the long file instead. Two entries are worth knowing about: the 12th
edition's single combined `treatment of income and deductions of ineligible non
citizens` row, which is why 2016 has no PRWORA columns, and `online case
management call centers` for 14 jurisdictions, the one row fusion in that
edition. Neither affects a panel column.

### Files kept but not read

| file | why |
|---|---|
| `snap_options_2016.csv` | same edition as the 12th edition PDF. Agrees with it on 93.1% of 1,239 cells; about 24 cells carry text from a neighbouring table row |
| `snap_options_2017.csv` | same edition as the 13th edition PDF. Agrees on 89.1% of 1,322 cells; about 76 carry page footers or text from following rows. Its `Treatment of Child Support Payments` cell absorbed the two ineligible-noncitizen rows for all 53 jurisdictions, which is why that file has no such categories |
| `snap_state_options_2023.csv` | the hand-checked 2023 reference the panel is validated against |
| `state options reports/snap_stateOptionsReport_2018.pdf` | 14th edition, prose-and-lists layout, needs a different parser |

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
