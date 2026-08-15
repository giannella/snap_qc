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
state should deviate from the convention (different list, years, or role).
"""

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
_QC_FILES = [f'qc_data/qc_pub_fy{y}.sav' for y in _YEARS]


def _default_entry(abbr):
    name_us, fips = _STATES[abbr]
    return {
        'name': name_us.replace('_', ' '),
        'fips': fips,
        'fy_label': f'{FY0}–{FY1}',
        'years': _YEARS,
        'delivery_csv': (f'state_delivery_lists/blended_delivery_'
                         f'{name_us}_{FY0}_{FY1}_{_BUDGET}.csv'),
        'qc_files': _QC_FILES,
        'role_filter': 'core',      # which delivery rules to implement
    }


# Per-state deviations from the convention (merged over the default entry).
OVERRIDES = {}

# States excluded from batch builds (make_state.py all), with the reason.
# An explicit single-state build still works but prints the reason.
EXCLUDE = {
    'IL': ('the munging applies an Illinois-specific standard-deduction offset '
           '(IL_OFFSET in standard_deductions.csv) that the RECON workbook '
           'formulas do not implement; held back 2026-08-15'),
}

STATES = {a: {**_default_entry(a), **OVERRIDES.get(a, {})} for a in _STATES}

# ── v1 tuning floors (build_workbook.py only) ─────────────────────────────────
# These mirror the R optimizer's config and apply to every state. They are the
# UNGUARDED search: no held-out year, no multiplicity adjustment, and a support
# floor well below the n >= 30 the state-scale evidence requires. Kept so the
# original workbooks stay reproducible; prefer build_workbook_v2.py.
PREC_FLOOR = 0.25      # precision floor            (R: PRECISION_TARGET)
MIN_FLAGGED = 10       # ignore combos flagging fewer cases   (R: MIN_FLAGGED)
RECALL_FLOOR = 0.02    # min share of error $ a combo must catch (R: RECALL_FLOOR)

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
