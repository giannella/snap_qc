"""Shared workbook layout: sheet names and the raw input contract.

Excel forbids : \\ / ? * [ ] in sheet names and caps them at 31 characters,
so the step tabs use periods ("Step 1." not "Step 1:"); BLENDED_SHEET is
exactly 31 characters. Every stage of the build chain (build_workbook_v2,
make_live, make_input_workbook, crosscheck_rules, make_state's verify
probes) imports these so a rename happens in one place.

Data tab column layout (fixed from the first build stage):
  [ input contract ]    RAW_COLS below, columns 1..len(RAW_COLS): what a
                        state pastes (amber). Headers exist from the first
                        build; make_input_workbook fills the values.
  [ feature columns ]   the model features (gray), computed by formula in
                        the delivered workbook
  [ hit columns ]       per-case rule tests, added by make_live (hidden)
  [ helper columns ]    the benefit-recomputation chain, added by
                        make_input_workbook (hidden)
"""

DATA_SHEET     = 'Step 1. Paste-in Data'
BLENDED_SHEET  = 'Step 2. Review and Select Rules'
NATIONAL_SHEET = 'National Rules'


def qref(name):
    """The sheet name as it appears inside a formula reference."""
    return f"'{name}'" if any(ch in name for ch in " .-'") else name


# ── the raw input contract, one row per reviewed case ─────────────────────────
# Column names follow features.R's state_col_map style: generic reported-value
# concepts a state maps its own system's fields onto. The Data Dictionary tab
# carries the crosswalk to the QC technical manual's variables. NB the QC
# outcome is named ERROR_FLAG, not OVER_THRESHOLD: Excel table column names
# are case-insensitively unique and the feature column over_threshold already
# claims that name.
RAW_COLS = [
    'CASE_ID', 'REVIEW_FISCAL_YEAR',
    'HOUSEHOLD_SIZE',
    'NUM_CHILDREN', 'NUM_ELDERLY', 'NUM_DISABLED', 'NUM_ABAWD',
    'MARRIED_FLAG', 'EXPEDITED', 'CATEGORICALLY_ELIGIBLE', 'HOMELESS_FLAG',
    'MONTHS_SINCE_CERT',
    'EARNED_INCOME', 'UNEARNED_INCOME',
    'MEDICAL_DEDUCTION', 'DEPENDENT_CARE_DEDUCTION', 'CHILD_SUPPORT_DEDUCTION',
    'HOMELESS_DEDUCTION',
    'RENT', 'UTILITY_COSTS',
    'NUM_AMOUNTS_DIVISIBLE_BY_100',   # state-precomputed; definition in the dictionary
    'ERROR_FLAG', 'ERROR_AMOUNT',
]
RAW_OFF = len(RAW_COLS)               # feature block starts at column RAW_OFF + 1
