"""Shared workbook layout: sheet names and the raw input contract.

Excel forbids : \\ / ? * [ ] in sheet names and caps them at 31 characters,
so the step tabs use periods ("Step 1." not "Step 1:"). Every stage of the
build chain (build_workbook_v2, make_live, make_input_workbook,
crosscheck_rules, make_state's verify probes) imports these so a rename
happens in one place.

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
import os

DICT_SHEET    = 'Step 1. Data Dictionary'
DATA_SHEET    = 'Step 2. Import Testing Data'
BLENDED_SHEET = 'Step 3. Select Rules'
EXPORT_SHEET  = 'Step 4. Export Rules'
SCREEN_SHEET  = 'Step 5.1 Screen New Cases'
FLAGGED_SHEET = 'Step 5.2 Flagged New Cases'
# Vote builds with the national vote rules (Eric's tab names, 2026-10-29):
# Step 5.2 becomes 5.2.a and the vote tab follows it as 5.2.b ("Step 5.2.b
# Expected Error Amounts" would pass the 31-character cap)
VOTE_RULES_BUILD = (os.environ.get('SNAP_VOTE') == '1'
                    and bool(os.environ.get('SNAP_VOTE_RULES_CSV')))
FLAGGED_SHEET_VOTE = 'Step 5.2.a Flagged New Cases'
if VOTE_RULES_BUILD:
    FLAGGED_SHEET = FLAGGED_SHEET_VOTE
VOTE_SHEET    = '5.2.b Expected Error Amounts'
PRIORITY_SHEET = 'Step 5.3 Case Priority List'   # 2026-09-17: pasted cases ranked by rules-flagging x benefit
# "Share aggregate results back" exceeds Excel's 31-char sheet-name cap;
# the full phrase lives in the sheet's title band instead
SHARE_SHEET   = 'Step 6. Share results back'
VIEWER_SHEET  = 'See cases flagged by a rule'


def qref(name):
    """The sheet name as it appears inside a formula reference."""
    return f"'{name}'" if any(ch in name for ch in " .-'") else name


# ── the raw input contract, one row per reviewed case ─────────────────────────
# Column names follow features.R's state_col_map style: generic reported-value
# concepts a state maps its own system's fields onto. The Data Dictionary tab
# carries the crosswalk to the QC technical manual's variables. The QC outcome
# arrives as ORIGINAL_BENEFIT_AMOUNT / CORRECTED_BENEFIT_AMOUNT (QC manual
# RAWBEN / FSBEN), STATUS (the review disposition: 2 = overissuance,
# 3 = underissuance, 4 = ineligible household) and ERROR_AMOUNT (QC manual
# AMTERR). The error amount is ERROR_AMOUNT, as in the munging since Ben's
# 2026-11-09 change (Eric, 2026-11-10); the over-threshold flag compares it
# with the FederalTables error-threshold table. A blank ERROR_AMOUNT falls
# back to |ORIGINAL - CORRECTED|. ERROR_AMOUNT and ALLOTMENT_ADJUSTMENT (QC
# manual ALLADJ, optional) sit last so earlier paste layouts still line up:
# the benefit check skips cases coded 2 (prorated) or 3 (other adjustment).
RAW_COLS = [
    'CASE_ID', 'REVIEW_FISCAL_YEAR',
    'HOUSEHOLD_SIZE',
    'NUM_CHILDREN', 'NUM_ELDERLY', 'NUM_DISABLED', 'NUM_ABAWD',
    'MARRIED_FLAG', 'EXPEDITED', 'CATEGORICALLY_ELIGIBLE', 'HOMELESS_FLAG',
    'MONTHS_SINCE_CERT',
    'EARNED_INCOME', 'UNEARNED_INCOME',
    'MEDICAL_DEDUCTION', 'DEPENDENT_CARE_DEDUCTION',
    'CHILD_SUPPORT_EXPENSES',
    'HOMELESS_DEDUCTION',
    'RENT', 'UTILITY_COSTS',
    'ORIGINAL_BENEFIT_AMOUNT', 'CORRECTED_BENEFIT_AMOUNT', 'STATUS',
    'ERROR_AMOUNT', 'ALLOTMENT_ADJUSTMENT',
]
RAW_OFF = len(RAW_COLS)               # feature block starts at column RAW_OFF + 1
