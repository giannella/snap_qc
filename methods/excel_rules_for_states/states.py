"""
State registry — every state with a blended delivery list in the snap_qc repo's
state_delivery_lists/ folder resolves automatically; nothing needs adding here
to build a new state.

Entries are generated from the naming convention
    state_delivery_lists/blended_delivery_<State_Name>_2022_2024_budget10.csv
(the 10% review-budget list; its `core` rows are what a state reviewing 10% of
its caseload deploys, and the `buffer` rows extend the pool to 3x depth).
`build_workbook_v2.py` resolves the path against the snap_qc checkout, so the
value here is repo-relative. Set an explicit entry in OVERRIDES only when a
state should deviate from the convention (different list or years).
"""

import os

FY0, FY1 = 2022, 2024
_BUDGET = 'budget10'          # keep in step with TUNING['budget'] below

# abbr -> (full name as used in the delivery-list filenames, FIPS code)
_STATES = {
    'AL': ('Alabama', 1),               'AZ': ('Arizona', 4),
    'AR': ('Arkansas', 5),              'CA': ('California', 6),
    'CO': ('Colorado', 8),              'CT': ('Connecticut', 9),
    'DE': ('Delaware', 10),             'DC': ('District_of_Columbia', 11),
    'FL': ('Florida', 12),              'GA': ('Georgia', 13),
    'ID': ('Idaho', 16),                'IL': ('Illinois', 17),
    'IN': ('Indiana', 18),              'IA': ('Iowa', 19),
    'KS': ('Kansas', 20),               'KY': ('Kentucky', 21),
    'LA': ('Louisiana', 22),            'ME': ('Maine', 23),
    'MD': ('Maryland', 24),             'MA': ('Massachusetts', 25),
    'MI': ('Michigan', 26),             'MN': ('Minnesota', 27),
    'MS': ('Mississippi', 28),          'MO': ('Missouri', 29),
    'MT': ('Montana', 30),              'NE': ('Nebraska', 31),
    'NV': ('Nevada', 32),               'NH': ('New_Hampshire', 33),
    'NJ': ('New_Jersey', 34),           'NM': ('New_Mexico', 35),
    'NY': ('New_York', 36),             'NC': ('North_Carolina', 37),
    'ND': ('North_Dakota', 38),         'OH': ('Ohio', 39),
    'OK': ('Oklahoma', 40),             'OR': ('Oregon', 41),
    'PA': ('Pennsylvania', 42),         'RI': ('Rhode_Island', 44),
    'SC': ('South_Carolina', 45),       'SD': ('South_Dakota', 46),
    'TN': ('Tennessee', 47),            'TX': ('Texas', 48),
    'UT': ('Utah', 49),                 'VT': ('Vermont', 50),
    'VA': ('Virginia', 51),             'WA': ('Washington', 53),
    'WV': ('West_Virginia', 54),        'WI': ('Wisconsin', 55),
    'WY': ('Wyoming', 56),
}

_YEARS = tuple(range(FY0, FY1 + 1))


def _default_entry(abbr):
    name_us, fips = _STATES[abbr]
    return {
        'name': name_us.replace('_', ' '),
        'fips': fips,
        'fy_label': f'{FY0}–{FY1}',
        'years': _YEARS,
        # SNAP_LIST_DIR (2026-08-22) redirects the source folder, e.g. to a
        # STAGED candidate build under methods/ for preview workbooks; the
        # default stays the tracked, shipped folder
        'delivery_csv': (f'{os.environ.get("SNAP_LIST_DIR", "state_delivery_lists")}'
                         f'/blended_delivery_{name_us}_{FY0}_{FY1}_{_BUDGET}.csv'),
        # 2026-08-18: 'national_csv' and 'role_filter' retired — the National
        # Rules tab is gone, and rule_selection.py consumes ALL roles of the
        # blended CSV (transformed core + promoted buffer), so no role filter
        # applies anywhere in the build any more.
    }


# Per-state deviations from the convention (merged over the default entry).
# Keys beyond the default entry's, read by make_input_workbook.py:
#   std_ded_offset_col      column of additional_data/standard_deductions.csv
#                           subtracted from the federal standard deduction for
#                           this state's cases, mirroring the munging script's
#                           get_standard_deduction(); carried in the workbook as
#                           the state_offset column of the FederalTables
#                           standard-deduction table (0 for every other state)
#   federal_tables_visible  ship the FederalTables sheet unhidden
#   start_here_note         (title, body): a state-specific warning under the
#                           Start Here summary block; {n_rules} and
#                           {n_benefit_rules} in the body are filled from the
#                           effective rule list at build time
OVERRIDES = {
    # Illinois: held back from the 2026-08-24 release because the munging
    # subtracts IL_OFFSET from the federal standard deduction for Illinois
    # cases and the workbook's benefit chain did not; added 2026-09-15 once
    # the chain carried the offset (the validation gate fails on the
    # benefit-ratio features without it).
    'IL': {
        'std_ded_offset_col': 'IL_OFFSET',
        'federal_tables_visible': True,
        'start_here_note': (
            'Read this first: the Illinois standard deduction',
            'The rules in this workbook were mined on a research frame built '
            'from the public QC files. For Illinois cases, that frame subtracts '
            'an offset from the federal standard deduction before it recomputes '
            'the benefit: $7 per month for FY2017-2024 and $4 for FY2025-2026 '
            '(the IL_OFFSET column of additional_data/standard_deductions.csv in '
            'the repository). We added the offset after finding that Illinois '
            'cases recomputed poorly without it: 27% of Illinois cases with no '
            'payment error recomputed more than $1 away from the recorded '
            'benefit, against about 2% in other states. With the offset the '
            'Illinois rate is 5.2%, still above other states, so the '
            'benefit-based variables carry more recomputation noise for Illinois '
            'than elsewhere. This workbook applies the same offset through the '
            'state_offset column of the standard-deduction table on the '
            'FederalTables tab, which is left visible in this workbook so that '
            'you can check and edit it. The offset matters for most of the list: '
            '{n_benefit_rules} of the {n_rules} rules test a variable computed '
            'through the standard deduction (rawben_rel_max, unc_rawben_rel_max '
            'or total_deductions_by_hh_size). Before pasting internal data, '
            'confirm that the offset matches the standard deduction your '
            'eligibility system applied in each fiscal year, and change the '
            'state_offset values if it does not: a wrong offset shifts every '
            'benefit-based variable and the rules that test them. Illinois was '
            'not in the first workbook release (2026-08-24) because its formulas '
            'lacked this offset; this workbook was built 2026-09-15.'),
    },
}

# States excluded from batch builds (make_state.py all), with the reason.
# An explicit single-state build still works but prints the reason.
EXCLUDE = {}

STATES = {a: {**_default_entry(a), **OVERRIDES.get(a, {})} for a in _STATES}

# ── v2 tuning contract (build_workbook_v2.py) ─────────────────────────────────
# Overrides for tuning.TuningConfig; anything omitted keeps that dataclass's
# default. The defaults are the validated ones (methods/tuning_principles.md) and
# the floors below them are not meant to be loosened per state: min_support,
# min_admitted_for_tier2 and the holdout gate are what stop a small sample from
# turning the search into a fishing expedition.
#
#   max_tier = 0   ship the delivered list untouched (always safe)
#   max_tier = 1   allow re-filtering and re-ranking, rule text frozen
#   max_tier = 2   allow threshold tuning inside the bracket (the default)
# Keep `budget` matched to the delivery list in use: a list filled to a 10%
# review budget is evaluated and re-filled at 10%, not 5%.
TUNING = {
    'max_tier': 2,
    'budget': 0.10,        # review capacity as a share of the caseload
    'holdout_years': 1,    # most recent fiscal year is held out, never a random split
}


def get(abbr):
    if abbr not in STATES:
        raise SystemExit(f'unknown state {abbr!r}; known: {", ".join(sorted(STATES))}')
    if abbr in EXCLUDE:
        print(f'NOTE: {abbr} is excluded from batch builds: {EXCLUDE[abbr]}')
    return dict(STATES[abbr], abbr=abbr)


def all_abbrs():
    """States for batch builds; EXCLUDE members are skipped with their reason."""
    for a, why in EXCLUDE.items():
        print(f'skipping {a}: {why}')
    return sorted(a for a in STATES if a not in EXCLUDE)
