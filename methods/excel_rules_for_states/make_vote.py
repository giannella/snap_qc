"""
Stage 3b (optional, SNAP_VOTE builds only): case scoring by rule votes in the
delivered workbook (issues #16 / #21; methods/vote_scoring/design_note.md,
findings section 44; shipped by Eric's decision of 2026-10-29).

The rules tab carries a second yellow TRUE/FALSE column, Vote? (written by
build_workbook_v2.py when SNAP_VOTE=1). With SNAP_VOTE_RULES_CSV the 300
national vote rules are appended to the tab with Vote? = TRUE and the state's
list rules ship Vote? = FALSE. This stage adds:

  Step 2 data   hidden per-case vote columns (one term per rule, weighted by
                that rule's Vote? cell) and visible ones: Votes (how many
                Vote? = TRUE rules flag the case), Relative error rate,
                Expected error $, Vote priority rank, Vote rank in size group.
  5.2.b         a new tab after Step 5.2.a (VOTE_SHEET, "5.2.b Expected
                Error Amounts"; "Vote Score" until 2026-10-29): the top cases by expected error $
                across all new cases (the main list; 2026-10-30), the top cases
                in each household-size group beside it, a comparison with the
                Include? rules at the same workload, and the FIXED table
                expected error $ is computed from.
  Step 5.1      the same columns for new cases.

Nothing is estimated in the workbook. The relative error rate by household-
size group and vote count is national and fixed (methods/vote_scoring/
deploy/vote_table.csv: FY2017-19 rules, votes counted on FY2022, the age at
which the workbook's rules will be used). Error size is a national gamma
model (log link) of error dollars on log issued benefit, household size and
the vote, capped at 7 (deploy/size_coefs.csv; deploy_table.R applies the same
cap). The 5.2.b lists show 7 or more votes as "7+" (Eric, 2026-11-09). The level is the state's FNS-published
payment error rate (cell E5): expected error $ is scaled so that, over the
Step 2 cases, it totals that rate times issued benefits. The level does not
change the order of cases within the state.

Usage:  python make_vote.py <recon_workbook.xlsx> --state WA [-o out.xlsx]
"""
import argparse
import csv
import json
import math
import os
import re
import shutil

import openpyxl
from openpyxl.comments import Comment
from openpyxl.styles import Alignment, Font, PatternFill, Border, Side
from openpyxl.utils import get_column_letter as CL
from openpyxl.utils.cell import range_boundaries
from openpyxl.worksheet.datavalidation import DataValidation
from openpyxl.worksheet.table import TableColumn, TableFormula

from live_formulas import pack_chunks, read_delivery_tab, rule_terms
from make_live import MERGED_VOTE, VOTE_BASE
from workbook_layout import BLENDED_SHEET, DATA_SHEET, FLAGGED_SHEET, SCREEN_SHEET, VOTE_SHEET, qref

PKG = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.abspath(os.path.join(PKG, '..', '..'))
HH = ['1', '2-3', '4+']
REVIEW_SHARE = 0.10
VOTE_CAP = 7          # votes above this count as 7 in the error size and show as "7+" (deploy_table.R VOTE_CAP)
DEPLOY = os.environ.get('SNAP_VOTE_DEPLOY_DIR') or os.path.join(REPO, 'methods', 'vote_scoring', 'deploy')   # override: smoke tests
PER_CSV = os.path.join(REPO, 'methods', 'vote_scoring', 'fns_payment_error_rates_fy2025.csv')
PER_LABEL = 'FY2025 payment error rate, FNS (published June 24, 2026)' 
N0 = 10                    # first rule row on the rules tab

YELLOW = PatternFill('solid', fgColor='FFFF99')
GRAY = PatternFill('solid', fgColor='F2F2F2')
GREEN = PatternFill('solid', fgColor='E2EFDA')
BLUE = PatternFill('solid', fgColor='2F5496')
ORANGE = PatternFill('solid', fgColor='F8CBAD')
THIN = Border(*(Side(style='thin', color='BFBFBF'),) * 4)
WRAP = Alignment(horizontal='left', vertical='top', wrap_text=True)
CENTER = Alignment(horizontal='center', vertical='center')


def size_coefs():
    with open(os.path.join(DEPLOY, 'size_coefs.csv'), newline='') as fh:
        return {r['term']: float(r['estimate']) for r in csv.DictReader(fh)}


def vote_table():
    with open(os.path.join(DEPLOY, 'vote_table.csv'), newline='') as fh:
        rows = list(csv.DictReader(fh))
    for r in rows:
        assert r['hh'] in HH
    return rows


def state_per(name):
    with open(PER_CSV, newline='') as fh:
        for r in csv.DictReader(fh):
            if r['state'] == name:
                return float(r['payment_error_rate']) / 100
    raise SystemExit(f'no FNS payment error rate for {name} in {PER_CSV}')


def extend_table(ws, tname, new_cols):
    """Append columns (name, row-2 formula) to an existing table; the table's
    calculatedColumnFormula carries each formula to rows added by a paste."""
    tbl = ws.tables[tname]
    c0, r0, c1, r1 = range_boundaries(tbl.ref)
    assert all(ws.cell(row=r0, column=c).value is None for c in range(c1 + 1, ws.max_column + 2)),         f'{tname}: cells right of the table would be overwritten'
    for k, (name, _) in enumerate(new_cols):
        ws.cell(row=r0, column=c1 + 1 + k, value=name).font = Font(bold=True)
    nid = max(tc.id for tc in tbl.tableColumns)
    for k, (name, f) in enumerate(new_cols, 1):
        tc = TableColumn(id=nid + k, name=name)
        tc.calculatedColumnFormula = TableFormula(attr_text=f[1:])
        tbl.tableColumns.append(tc)
    c1n = c1 + len(new_cols)
    tbl.ref = f'{CL(c0)}{r0}:{CL(c1n)}{r1}'
    if tbl.autoFilter is not None:
        tbl.autoFilter.ref = tbl.ref
    return c1 + 1, r0 + 1, r1


def vote_columns(table, rules, vote_ref, coefs, t_hh, t_from, t_rel, scale, merged=None):
    """(name, formula) for the hidden vote chunks and the visible columns.

    merged: the table's make_live chunk columns when they already carry the
    votes (make_live.MERGED_VOTE: Include? + VOTE_BASE x Vote? per rule); the
    vote count is then read off them instead of testing every rule again."""
    T = lambda col: f'{table}[[#This Row],[{col}]]'
    cols = []
    if merged:
        cols.append(('Votes', f'=INT(({"+".join(T(p) for p in merged)})/{VOTE_BASE})'))
    else:
        parts = []
        for k, ch in enumerate(pack_chunks(rule_terms(rules, vote_ref, table)), 1):
            name = f'_vt{k}' if table == 'CaseData' else f'_svt{k}'
            f = '=' + ('+'.join(ch) if ch else '0')
            assert len(f) < 8000, f'{name}: formula {len(f)} chars'
            cols.append((name, f)); parts.append(name)
        cols.append(('Votes', '=' + '+'.join(T(p) for p in parts)))
    # the fixed table is sorted by size group, then votes: the last row of the case's group whose
    # "votes from" is at or below its vote count is its bin
    cols.append(('Relative error rate', f'=IFERROR(LOOKUP(2,1/(({t_hh}={T("hh_group")})*({t_from}<={T("Votes")})),{t_rel}),0)'))
    hs = (f'INDEX({{0,{coefs["hsz2"]:.6f},{coefs["hsz3"]:.6f},{coefs["hsz4"]:.6f},{coefs["hsz5"]:.6f},{coefs["hsz6+"]:.6f}}},'
          f'MIN(MAX(ROUND(N({T("HOUSEHOLD_SIZE")}),0),1),6))')
    rs = '_rs' if table == 'CaseData' else '_srs'
    cols.append((rs, (
        f'=IF(NOT(ISNUMBER({T("ORIGINAL_BENEFIT_AMOUNT")})),0,{T("Relative error rate")}'
        f'*EXP({coefs["(Intercept)"]:.6f}+{coefs["lben"]:.6f}*LN(MAX({T("ORIGINAL_BENEFIT_AMOUNT")},1))+{hs}'
        f'+{coefs["vote"]:.6f}*MIN({T("Votes")},{VOTE_CAP})))')))
    cols.append(('Expected error $', f'={scale}*{T(rs)}'))
    # tie-break: issued benefit, then row
    sx = '_xs' if table == 'CaseData' else '_sxs'
    cols.append((sx, f'=IF({T("Expected error $")}>0,{T("Expected error $")}+N({T("ORIGINAL_BENEFIT_AMOUNT")})/1E9+ROW()/1E13,0)'))
    cols.append(('Vote priority rank', f'=IF({T(sx)}=0,"",COUNTIF({table}[{sx}],">"&{T(sx)})+1)'))
    cols.append(('Vote rank in size group', f'=IF({T(sx)}=0,"",COUNTIFS({table}[hh_group],{T("hh_group")},{table}[{sx}],">"&{T(sx)})+1)'))
    hk = '_hkey' if table == 'CaseData' else '_shkey'
    cols.append((hk, f'=IF({T("Vote rank in size group")}="","",{T("hh_group")}&"|"&{T("Vote rank in size group")})'))
    return cols


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('workbook')
    ap.add_argument('-o', '--out')
    ap.add_argument('--state', required=True, help='state name as in the FNS table, e.g. Washington')
    a = ap.parse_args()
    out = a.out or a.workbook.replace('.xlsx', '_VOTE.xlsx')
    if os.path.abspath(out) != os.path.abspath(a.workbook):
        shutil.copy(a.workbook, out)
    wb = openpyxl.load_workbook(out)
    rws = wb[BLENDED_SHEET]
    vcol = next(c for c in range(1, rws.max_column + 1) if rws.cell(row=3, column=c).value == 'Vote?')
    BQ = qref(BLENDED_SHEET)
    rules = read_delivery_tab(rws, first_row=N0)
    vote_ref = lambda i: f'({BQ}!${CL(vcol)}${N0 + i}=TRUE)'
    vmax = {h: sum(1 for c, hh in rules if c is not None and hh == h) for h in HH}
    coefs = size_coefs()
    table = vote_table()
    per = state_per(a.state)
    print(f'rules: {len(rules)} ({", ".join(f"{h}: {vmax[h]}" for h in HH)}); Vote? in column {CL(vcol)}')

    # ── the vote tab (5.2.b), after Step 5.2.a and in its color ──
    # rows 1-6: title, explanation, settings | rows 7-8: list titles and headers | rows 9..: the main
    # list (all new cases, ranked together) and, to its right, one list per household-size group |
    # below: the comparison block, then the error-rate-by-vote table that expected error $ is computed from.
    # The main list ranks across groups (Eric, 2026-10-30): on WA's later years the per-group lists, which
    # put about 60% of the review into one-person households, caught 6-10 points fewer errors
    # (custom_one_off/washington/exploration_2026-10-30); the group lists stay for states that want them.
    vs = wb.create_sheet(VOTE_SHEET, index=wb.sheetnames.index(FLAGGED_SHEET) + 1)
    vs.sheet_view.showGridLines = False
    vs.sheet_properties.tabColor = wb[FLAGGED_SHEET].sheet_properties.tabColor
    VQ = qref(VOTE_SHEET)
    ndata = range_boundaries(wb[DATA_SHEET].tables['CaseData'].ref)[3] - 1
    CAP = max(50, math.ceil(0.2 * ndata))                 # rows per list
    # the lists rank the NEW cases pasted on Step 5.1 (Eric, 2026-10-29; they ranked the Step 2 cases
    # before, with fiscal year and the QC outcome); the comparison block below stays on Step 2
    MCOLS = [('Rank', 8), ('CASE_ID', 14), ('Household size', 10), ('Votes', 8), ('Expected error $', 13),
             ('Issued benefit', 12)]                      # the main list, columns A-F
    MW = len(MCOLS)
    G0 = MW + 2                                           # first group-list column, after a narrow gap
    BW = 5                                                # columns per group list; group lists sit edge to edge
    LCOLS = [('Rank', 8), ('CASE_ID', 14), ('Votes', 8), ('Expected error $', 13), ('Issued benefit', 12)]
    LT, LH, L0 = 7, 8, 9                                  # list title row, header row, first list row
    RC = L0 + CAP + 2                                     # comparison block
    H0 = RC + 8                                           # fixed table header
    last_col = CL(G0 - 1 + 3 * BW)
    for k, (_, w) in enumerate(MCOLS):
        vs.column_dimensions[CL(1 + k)].width = w
    vs.column_dimensions[CL(MW + 1)].width = 3
    for b_ in range(3):
        for k, (_, w) in enumerate(LCOLS):
            vs.column_dimensions[CL(G0 + b_ * BW + k)].width = w
        # each group list's first column (H, M, R) is 235 pixels wide, so its
        # household-size description reads on arrival (Eric, 2026-11-10).
        # Excel's pixels scale with the display: 33.57 showed as 369-370 px on
        # Eric's screen (11 px per character unit), so 21.37 -> 235 px there
        vs.column_dimensions[CL(G0 + b_ * BW)].width = 21.37
    vs.merge_cells(f'A1:{last_col}1')
    c = vs.cell(row=1, column=1, value='Expected error amounts: the new cases with the highest expected error $')
    c.font = Font(bold=True, size=16, color='FFFFFF'); c.fill = BLUE; c.alignment = CENTER
    vs.row_dimensions[1].height = 30
    vs.merge_cells(f'A2:{last_col}2')
    vs.cell(row=2, column=1, value=(
        'The main list (left) ranks all the new cases pasted on the "' + SCREEN_SHEET + '" tab together by expected '
        'error $; review from the top. The three lists on the right rank the same cases within one household-size '
        "group each, for a state that reviews a set share of each group. A vote is one rule with Vote? = TRUE on the \"" + BLENDED_SHEET + '" tab that flags the case. '
        "Expected error $ = the national error rate for the case's household size and number of votes (fixed "
        f'table at row {H0}) x the typical size of an error for its issued benefit, household size and votes, '
        'scaled to the state payment error rate in E5. ' + f'{VOTE_CAP} or more votes show as {VOTE_CAP}+ in the lists. Each case\'s figures are also on the "' + SCREEN_SHEET + '" tab '
        '("Votes" to "Vote rank in size group").')).alignment = WRAP
    vs.row_dimensions[2].height = 90
    vs.merge_cells('A4:D4'); vs.merge_cells('A5:D5'); vs.merge_cells('A6:D6')
    vs.cell(row=4, column=1, value='Share of new cases to list').font = Font(bold=True)
    c = vs.cell(row=4, column=5, value=REVIEW_SHARE); c.fill = YELLOW; c.border = THIN; c.alignment = CENTER
    c.number_format = '0%'
    vs.cell(row=5, column=1, value=PER_LABEL).font = Font(bold=True)
    c = vs.cell(row=5, column=5, value=round(per, 4)); c.fill = YELLOW; c.border = THIN; c.alignment = CENTER
    c.number_format = '0.00%'
    vs.cell(row=6, column=1, value='Scale applied to every case (set by E5)').font = Font(bold=True)
    SHARE, PER, SCALE = f'{VQ}!$E$4', f'{VQ}!$E$5', f'{VQ}!$E$6'
    vs.cell(row=6, column=5, value='=IFERROR(E5*SUM(CaseData[ORIGINAL_BENEFIT_AMOUNT])/SUM(CaseData[_rs]),0)').number_format = '0.000'
    vs.cell(row=6, column=5).border = THIN; vs.cell(row=6, column=5).alignment = CENTER
    # probes for the verify stage: J4 total votes, J5 cases with any vote, J6 expected $ over issued $ (= E5)
    vs.cell(row=4, column=7, value='Total votes').font = Font(size=9, color='808080')
    vs.cell(row=4, column=10, value='=SUM(CaseData[Votes])').font = Font(size=9, color='808080')
    vs.cell(row=5, column=7, value='Cases with a vote').font = Font(size=9, color='808080')
    vs.cell(row=5, column=10, value='=COUNTIF(CaseData[Votes],">0")').font = Font(size=9, color='808080')
    vs.cell(row=6, column=7, value='Expected $ / issued $').font = Font(size=9, color='808080')
    vs.cell(row=6, column=10, value='=ROUND(SUM(CaseData[Expected error $])/SUM(CaseData[ORIGINAL_BENEFIT_AMOUNT]),6)').font = Font(size=9, color='808080')

    # the fixed table; the data columns point at it
    hdr = ['Household size', 'Votes from', 'Votes to', 'Relative error rate (national case = 1.00)',
           'Times the rate at no votes', 'Cases here', 'Errors here', 'Error rate here']
    vs.merge_cells(f'A{H0 - 1}:{last_col}{H0 - 1}')
    c = vs.cell(row=H0 - 1, column=1, value=(
        'How expected error $ is computed. Relative error rate: fixed national figures for rules about three years '
        'old (rules mined on FY2017-19, counted on FY2022 cases). Expected error $ = scale (E6) x relative error '
        'rate x typical error size for the case\'s issued benefit, household size and votes. The scale makes the '
        'expected error $ of the Step 2 cases total E5 x their issued benefits. The columns on the right show '
        'this state\'s Step 2 cases for comparison; they are not used.'))
    c.font = Font(bold=True); c.fill = GRAY; c.alignment = WRAP
    vs.row_dimensions[H0 - 1].height = 48
    for ci, h in enumerate(hdr, 1):
        c = vs.cell(row=H0, column=ci, value=h); c.font = Font(bold=True, size=10); c.fill = GRAY
        c.border = THIN; c.alignment = Alignment(horizontal='center', wrap_text=True)
    vs.row_dimensions[H0].height = 44
    HC = G0 + 3 * BW + 1                                   # hidden helpers right of the lists
    ROWM = HC + 2                                          # hidden: each main-list row's Step 5.1 row number
    ROWC = [HC + 3 + b_ for b_ in range(3)]                # hidden: the same for each group list
    R0, R1 = H0 + 1, H0 + len(table)
    rng = lambda col: f'{VQ}!${col}${R0}:${col}${R1}'
    base = {}
    for i, t in enumerate(table):
        r = R0 + i
        base.setdefault(t['hh'], float(t['rel_rate']))
        vs.cell(row=r, column=1, value=t['hh'])
        vs.cell(row=r, column=2, value=int(float(t['votes_from'])))
        vs.cell(row=r, column=3, value=(int(float(t['votes_to'])) if t['votes_to'] not in ('', 'NA') else 'or more'))
        vs.cell(row=r, column=4, value=round(float(t['rel_rate']), 4)).number_format = '0.00'
        vs.cell(row=r, column=5, value=round(float(t['rel_rate']) / base[t['hh']], 2)).number_format = '0.0"x"'
        hi = f'IF(ISNUMBER(C{r}),C{r},1E9)'
        vs.cell(row=r, column=6, value=f'=COUNTIFS(CaseData[hh_group],A{r},CaseData[Votes],">="&B{r},CaseData[Votes],"<="&{hi})')
        vs.cell(row=r, column=7, value=f'=COUNTIFS(CaseData[hh_group],A{r},CaseData[Votes],">="&B{r},CaseData[Votes],"<="&{hi},CaseData[over_threshold],1)')
        vs.cell(row=r, column=8, value=f'=IF(F{r}=0,"",G{r}/F{r})').number_format = '0.0%'
        for ci in range(1, 9):
            cc = vs.cell(row=r, column=ci); cc.border = THIN; cc.font = Font(size=10); cc.alignment = CENTER
    for k in range(6):
        vs.column_dimensions[CL(HC + k)].hidden = True
    t_hh, t_from, t_rel = rng('A'), rng('B'), rng('D')

    # ── per-case columns on Step 2 and Step 5.1 ──
    def add_cols(ws, tname, cols):
        cfirst, rfirst, rlast = extend_table(ws, tname, cols)
        for k, (name, f) in enumerate(cols):
            ci = cfirst + k
            for r in range(rfirst, rlast + 1):
                ws.cell(row=r, column=ci, value=f)
            if name.startswith('_'):
                ws.column_dimensions[CL(ci)].hidden = True
            else:
                ws.cell(row=1, column=ci).fill = PatternFill('solid', fgColor='C6E0B4')
                ws.column_dimensions[CL(ci)].width = 14
                fmt = {'Relative error rate': '0.00', 'Expected error $': '$#,##0.00'}.get(name)
                if fmt:
                    for r in range(rfirst, rlast + 1):
                        ws.cell(row=r, column=ci).number_format = fmt
        return len(cols)
    merged = None
    if MERGED_VOTE:
        dws = wb[DATA_SHEET]
        hdr = [c.value for c in dws[1]]
        merged = [h for h in hdr if isinstance(h, str) and re.fullmatch(r'_nat\d+', h)]
        f1 = '\n'.join(str(dws.cell(row=2, column=hdr.index(h) + 1).value) for h in merged)
        # every rule's Vote? cell must sit in the chunks, at the row this tab reads it from
        assert merged and all(f'{VOTE_BASE}*{vote_ref(i)}' in f1 for i, (cd, _) in enumerate(rules) if cd is not None), \
            'make_live chunks do not carry the Vote? weights this tab expects'
    n2 = add_cols(wb[DATA_SHEET], 'CaseData', vote_columns('CaseData', rules, vote_ref, coefs, t_hh, t_from, t_rel, SCALE,
                                                           merged))
    print(f'Step 2: {n2} columns appended')
    if SCREEN_SHEET in wb.sheetnames:
        n5 = add_cols(wb[SCREEN_SHEET], 'ScreenData', vote_columns('ScreenData', rules, vote_ref, coefs, t_hh, t_from, t_rel, SCALE))
        print(f'Step 5.1: {n5} columns appended')

    # ── the main list: all new cases, ranked together ──
    LABEL = {'1': 'Households of 1', '2-3': 'Households of 2-3', '4+': 'Households of 4 or more'}
    hdr_fill = PatternFill('solid', fgColor='D9E1F2')
    main_fill = PatternFill('solid', fgColor='C6E0B4')
    n_new = '+'.join(f'COUNTIF(ScreenData[hh_group],"{h}")' for h in HH)
    c = vs.cell(row=LT, column=1, value='All new cases'); c.font = Font(bold=True, size=11)
    vs.cell(row=LT, column=2, value=f'=ROUND({SHARE}*({n_new}),0)').font = Font(bold=True, size=11)
    vs.merge_cells(f'C{LT}:{CL(MW)}{LT}')
    vs.cell(row=LT, column=3, value=f'="listed of "&TEXT({n_new},"#,##0")&" new cases, all household sizes"')
    for k in range(MW):
        vs.cell(row=LT, column=1 + k).fill = main_fill
    for k, (name, _) in enumerate(MCOLS):
        c = vs.cell(row=LH, column=1 + k, value=name)
        c.font = Font(bold=True, size=10); c.fill = GRAY; c.border = THIN
        c.alignment = Alignment(horizontal='center', wrap_text=True)
    def list_cell(R, col):   # one list cell, read off Step 5.1; votes of VOTE_CAP or more show as "7+"
        v = f'INDEX(ScreenData[{col}],{R})'
        if col == 'Votes':
            return f'=IF({R}="","",IF({v}>={VOTE_CAP},"{VOTE_CAP}+",{v}))'
        return f'=IF({R}="","",{v})'
    RM_ = CL(ROWM)
    for i in range(CAP):
        r = L0 + i
        R = f'${RM_}{r}'
        vs.cell(row=r, column=1, value=f'=IF({i + 1}>$B${LT},"",{i + 1})')
        vs.cell(row=r, column=ROWM, value=f'=IF(A{r}="","",IFERROR(MATCH(A{r},ScreenData[Vote priority rank],0),""))')
        for k, col in ((1, 'CASE_ID'), (2, 'hh_group'), (3, 'Votes'), (4, 'Expected error $'), (5, 'ORIGINAL_BENEFIT_AMOUNT')):
            vs.cell(row=r, column=1 + k, value=list_cell(R, col))
        for k in range(MW):
            cc = vs.cell(row=r, column=1 + k); cc.border = THIN; cc.font = Font(size=10)
            cc.alignment = Alignment(horizontal='center')
        vs.cell(row=r, column=5).number_format = '$#,##0.00'
        vs.cell(row=r, column=6).number_format = '$#,##0'

    # ── the three household-size lists ──
    for b_, h in enumerate(HH):
        c0 = G0 + b_ * BW
        cl = lambda k: CL(c0 + k)
        ncell = f'${cl(1)}${LT}'
        c = vs.cell(row=LT, column=c0, value=LABEL[h]); c.font = Font(bold=True, size=11); c.fill = hdr_fill
        # number of cases listed for this group
        vs.cell(row=LT, column=c0 + 1, value=f'=ROUND({SHARE}*COUNTIF(ScreenData[hh_group],"{h}"),0)').fill = hdr_fill
        vs.cell(row=LT, column=c0 + 1).font = Font(bold=True, size=11)
        vs.merge_cells(f'{cl(2)}{LT}:{cl(BW - 1)}{LT}')
        # and how many of this group's cases the main list takes
        vs.cell(row=LT, column=c0 + 2, value=(
            f'="listed of "&TEXT(COUNTIF(ScreenData[hh_group],"{h}"),"#,##0")&"; "&TEXT(COUNTIFS(ScreenData[hh_group],"{h}",'
            f'ScreenData[Vote priority rank],"<="&$B${LT}),"#,##0")&" in the main list"')).fill = hdr_fill
        for k in range(BW):
            vs.cell(row=LT, column=c0 + k).fill = hdr_fill
        for k, (name, _) in enumerate(LCOLS):
            c = vs.cell(row=LH, column=c0 + k, value=name)
            c.font = Font(bold=True, size=10); c.fill = GRAY; c.border = THIN
            c.alignment = Alignment(horizontal='center', wrap_text=True)
        RC_ = CL(ROWC[b_])
        for i in range(CAP):
            r = L0 + i
            R = f'${RC_}{r}'
            vs.cell(row=r, column=c0, value=f'=IF({i + 1}>{ncell},"",{i + 1})')
            vs.cell(row=r, column=ROWC[b_], value=f'=IF({cl(0)}{r}="","",IFERROR(MATCH("{h}|"&{cl(0)}{r},ScreenData[_shkey],0),""))')
            for k, col in ((1, 'CASE_ID'), (2, 'Votes'), (3, 'Expected error $'), (4, 'ORIGINAL_BENEFIT_AMOUNT')):
                vs.cell(row=r, column=c0 + k, value=list_cell(R, col))
            for k in range(BW):
                cc = vs.cell(row=r, column=c0 + k); cc.border = THIN; cc.font = Font(size=10)
                cc.alignment = Alignment(horizontal='center')
            # a heavier rule between the three lists
            vs.cell(row=r, column=c0).border = Border(left=Side(style='medium', color='2F5496'),
                                                      right=THIN.right, top=THIN.top, bottom=THIN.bottom)
            vs.cell(row=r, column=c0 + 3).number_format = '$#,##0.00'
            vs.cell(row=r, column=c0 + 4).number_format = '$#,##0'
    vs.cell(row=L0 + CAP, column=1, value=(
        f'Each list stops at {CAP} cases; past that, the cases are ranked on the "{SCREEN_SHEET}" '
        'tab ("Vote priority rank", "Vote rank in size group") but not listed here.')).font = Font(size=9, italic=True, color='808080')

    # ── comparison block: the same workload chosen two ways ──
    vs.merge_cells(f'A{RC}:{last_col}{RC}')
    c = vs.cell(row=RC, column=1, value=(
        'On the testing data (Step 2), at the share in E4: the top cases by expected error $, ranked across all '
        'cases and within each group, against the rules with Include? = TRUE. All are optimistic here: the vote '
        'rules and the list rules were partly mined on these cases.'))
    c.font = Font(bold=True); c.fill = GRAY; c.alignment = WRAP
    vs.row_dimensions[RC].height = 32
    for ci, (h_, w_) in enumerate([('', 0), ('Cases', 0), ('Errors', 0), ('Error $', 0), ('Precision', 0),
                                   ('Error $ per case', 0), ('Share of error $', 0)], 1):
        cc = vs.cell(row=RC + 1, column=ci if ci == 1 else ci + 3, value=h_ or None)
        cc.font = Font(bold=True, size=10); cc.fill = GRAY; cc.border = THIN
    vs.merge_cells(f'A{RC + 1}:D{RC + 1}')
    tot_d = 'SUMIFS(CaseData[total_error_amount],CaseData[over_threshold],1)'
    union = f"'{BLENDED_SHEET}'"
    n2 = lambda h: f'ROUND({SHARE}*COUNTIF(CaseData[hh_group],"{h}"),0)'     # Step 2 cases per group at E4
    nlist = '+'.join(n2(h) for h in HH)
    nall = f'ROUND({SHARE}*(' + '+'.join(f'COUNTIF(CaseData[hh_group],"{h}")' for h in HH) + '),0)'
    top_all = lambda n: (f'=COUNTIFS(CaseData[Vote priority rank],"<="&{n},CaseData[over_threshold],1)',
                         f'=SUMIFS(CaseData[total_error_amount],CaseData[Vote priority rank],"<="&{n},CaseData[over_threshold],1)')
    errs_list = '+'.join(f'COUNTIFS(CaseData[hh_group],"{h}",CaseData[Vote rank in size group],"<="&{n2(h)},CaseData[over_threshold],1)'
                         for h in HH)
    dols_list = '+'.join(f'SUMIFS(CaseData[total_error_amount],CaseData[hh_group],"{h}",CaseData[Vote rank in size group],"<="&{n2(h)},CaseData[over_threshold],1)'
                         for h in HH)
    blocks = [
        (RC + 2, 'Top share of all cases by expected error $ (as the main list ranks new cases)', f'={nall}',
         *top_all(f'E{RC + 2}'), GREEN),
        (RC + 3, 'Top share of each group by expected error $ (as the group lists rank new cases)', f'={nlist}',
         f'={errs_list}', f'={dols_list}', GREEN),
        (RC + 4, 'Rules with Include? = TRUE (a case is flagged if any of them flags it)', f'={union}!$G$5', f'={union}!$H$5', f'={union}!$I$5', ORANGE),
        (RC + 5, 'Top cases by expected error $ across all groups, as many as the Include? rules flag',
         f'={union}!$G$5', *top_all(f'E{RC + 5}'), GREEN),
    ]
    for r, label, fn, fe, fd, fill in blocks:
        vs.merge_cells(f'A{r}:D{r}')
        vs.cell(row=r, column=1, value=label).alignment = WRAP
        vs.cell(row=r, column=5, value=fn)
        vs.cell(row=r, column=6, value=fe)
        vs.cell(row=r, column=7, value=fd).number_format = '$#,##0'
        vs.cell(row=r, column=8, value=f'=IF(E{r}=0,0,F{r}/E{r})').number_format = '0.0%'
        vs.cell(row=r, column=9, value=f'=IF(E{r}=0,0,G{r}/E{r})').number_format = '$#,##0.00'
        vs.cell(row=r, column=10, value=f'=IFERROR(G{r}/{tot_d},0)').number_format = '0.0%'
        for ci in range(1, 11):
            vs.cell(row=r, column=ci).border = THIN
            vs.cell(row=r, column=ci).fill = fill
        vs.row_dimensions[r].height = 30
    vs.freeze_panes = f'A{L0}'
    dv = DataValidation(type='decimal', operator='between', formula1='0', formula2='1')
    vs.add_data_validation(dv); dv.add('E4')
    dv2 = DataValidation(type='decimal', operator='between', formula1='0', formula2='1')
    vs.add_data_validation(dv2); dv2.add('E5')
    json.dump({'comparison_row': RC + 2, 'table_header_row': H0, 'list_rows': CAP, 'per': round(per, 4),
               # first row of each list: CASE_ID (main list, then the group lists), then the main list's
               # household size, expected error $ and issued benefit
               'list_cells': [f'B{L0}'] + [f'{CL(G0 + 1 + b_ * BW)}{L0}' for b_ in range(3)] + [f'C{L0}', f'E{L0}', f'F{L0}']},
              open(out.replace('.xlsx', '_layout.json'), 'w'))
    wb.save(out)
    print('saved', out, f'(lists: {CAP} rows each; comparison at row {RC + 2}; table at row {H0}; PER {per:.2%})')


if __name__ == '__main__':
    main()
