"""
Turn a built workbook into a LIVE one: every figure recomputes from the Data tab.

What changes
  Data          becomes an Excel Table (sheet keeps its name), so pasting more
                rows extends every reference automatically, and columns are
                bound by NAME rather than by position.
  Rules tabs    per-rule metrics and the combined rows on Blended Rules and
                National Rules become COUNTIFS/SUMIFS over that table; the
                hard-coded denominators go live too.
  Dashboard     its fixed Data!$X$2:$X$3000 ranges become table references.

Usage:  python make_live.py <built_workbook.xlsx> [-o out.xlsx]

Run postprocess_workbook.py afterwards to put the native checkboxes back:
openpyxl cannot preserve them through a save.
"""
import argparse
import os
import re
import shutil

import openpyxl
from openpyxl.utils import get_column_letter as CL
from openpyxl.utils import column_index_from_string
from openpyxl.worksheet.table import Table, TableStyleInfo

from workbook_layout import DATA_SHEET, BLENDED_SHEET, NATIONAL_SHEET, qref

TABLE = 'CaseData'            # table object name; the sheet keeps its name
CHUNK_LIMIT = 7500            # max chars per hit-column formula (Excel cap 8192)
COND = re.compile(r'([A-Za-z_][A-Za-z0-9_]*)\s*(>=|<=|>|<|=)\s*(-?[0-9.]+)')


def parse(text):
    """'a > 1 & b <= 2' -> [('a','>','1'), ('b','<=','2')]  (None if unusable)"""
    if (not text or 'no combination' in str(text) or 'not selected' in str(text)
            or 'not deployed' in str(text)):
        return None
    out = COND.findall(str(text))
    return out or None


def countifs(conds, hh, extra='', col=None, fn='COUNTIFS'):
    """A COUNTIFS/SUMIFS over the table for one rule."""
    head = f'{TABLE}[{col}],' if fn == 'SUMIFS' else ''
    parts = [f'{TABLE}[hh_group],"{hh}"']
    for v, op, t in conds:
        parts.append(f'{TABLE}[{v}],"{op if op != "=" else ""}{t}"')
    if extra:
        parts.append(extra)
    return f'{fn}({head}{",".join(parts)})'


def rule_terms(rows, sel_ref):
    """Per-case test terms, one per rule, weighted by that rule's checkbox flag.

    Each numeric condition carries an ISNUMBER guard: when an input column is
    deleted the features built from it go blank (make_input_workbook gates
    them), and a blank must never satisfy a condition — Excel otherwise ranks
    any text above any number, so ``"" >= x`` would flag every case."""
    terms = []
    for i, (conds, hh) in enumerate(rows):
        if conds is None:
            continue
        t = [f'({TABLE}[[#This Row],[hh_group]]="{hh}")']
        for v, op, val in conds:
            ref = f'{TABLE}[[#This Row],[{v}]]'
            t.append(f'ISNUMBER({ref})*({ref}{"=" if op == "=" else op}{val})')
        terms.append(f'{sel_ref(i)}*' + '*'.join(t))
    return terms


def pack_chunks(terms, limit=CHUNK_LIMIT):
    """Split the per-rule terms into as many helper-column formulas as the
    formula length cap requires."""
    chunks, cur, size = [], [], 1
    for t in terms:
        if cur and size + len(t) + 1 > limit:
            chunks.append(cur)
            cur, size = [], 1
        cur.append(t)
        size += len(t) + 1
    if cur:
        chunks.append(cur)
    return chunks or [[]]


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('workbook')
    ap.add_argument('-o', '--out')
    a = ap.parse_args()
    out = a.out or a.workbook.replace('.xlsx', '_LIVE.xlsx')
    shutil.copy(a.workbook, out)

    wb = openpyxl.load_workbook(out)
    dat = wb[DATA_SHEET]
    nat = wb[BLENDED_SHEET] if BLENDED_SHEET in wb.sheetnames else None
    natl = wb[NATIONAL_SHEET] if NATIONAL_SHEET in wb.sheetnames else None
    hdr = [c.value for c in next(dat.iter_rows(min_row=1, max_row=1))]
    NROW, NCOL = dat.max_row, len(hdr)
    print(f'Data: {NROW-1} cases x {NCOL} columns')

    # ── 1. read the rules straight out of the Exact expression column ────────
    N0 = 11

    def read_delivery_tab(ws):
        if ws is None:
            return []
        # the machine-readable rule text lives in the 'Exact expression'
        # column (last); find it by header rather than assuming a position
        expr_col = next((c for c in range(1, ws.max_column + 1)
                         if ws.cell(row=4, column=c).value == 'Exact expression'),
                        13)
        out_, r_ = [], N0
        while ws.cell(row=r_, column=1).value:
            out_.append((parse(ws.cell(row=r_, column=expr_col).value),
                         ws.cell(row=r_, column=2).value))
            r_ += 1
        return out_
    nt_rules, nl_rules = read_delivery_tab(nat), read_delivery_tab(natl)
    print(f'rules: {len(nt_rules)} blended, {len(nl_rules)} national-only')

    # ── 2. hidden per-case hit columns, inside the table so they auto-fill ───
    # The selection vector's column order is NOT the order rules appear on the
    # sheets, so read the mapping out of RuleFlags row 2 rather than assuming it.
    rf = wb['RuleFlags']
    sel_by_row = {BLENDED_SHEET: {}, NATIONAL_SHEET: {}}
    for cc in range(2, rf.max_column + 1):
        v = rf.cell(row=2, column=cc).value
        if not isinstance(v, str):
            continue
        m = re.search(r"(?:'([^']+)'|([A-Za-z_]\w*))!\$[A-Z]+\$(\d+)", v)
        if not m:
            continue
        sheet = m.group(1) or m.group(2)
        if sheet in sel_by_row:
            sel_by_row[sheet][int(m.group(3))] = f'RuleFlags!${CL(cc)}$2'
    print('selection columns found: %d blended, %d national-only'
          % (len(sel_by_row[BLENDED_SHEET]), len(sel_by_row[NATIONAL_SHEET])))
    sel_n = lambda i: sel_by_row[BLENDED_SHEET][N0 + i]
    sel_l = lambda i: sel_by_row[NATIONAL_SHEET][N0 + i]
    blocks = ([('nat', nt_rules, sel_n)] if nat else []) \
        + ([('nl', nl_rules, sel_l)] if natl else [])
    newcols = {}
    c = NCOL
    for tag, rules, sel in blocks:
        terms = rule_terms(rules, sel)
        parts = []
        for k, chunk in enumerate(pack_chunks(terms)):
            c += 1
            name = f'_{tag}{k+1}'
            newcols[name] = c
            dat.cell(row=1, column=c, value=name)
            f = '=' + ('+'.join(chunk) if chunk else '0')
            assert len(f) < 8000, f'{name}: formula {len(f)} chars, too long'
            for rr in range(2, NROW + 1):
                dat.cell(row=rr, column=c, value=f)
            parts.append(name)
        c += 1
        hit = f'_hit_{tag}'
        newcols[hit] = c
        dat.cell(row=1, column=c, value=hit)
        expr = '+'.join(f'{TABLE}[[#This Row],[{p}]]' for p in parts)
        for rr in range(2, NROW + 1):
            dat.cell(row=rr, column=c, value=f'=IF({expr}>0,1,0)')
    LAST = CL(c)
    print(f'added {len(newcols)} computed columns -> Data!A1:{LAST}{NROW}')

    tbl = Table(displayName=TABLE, ref=f'A1:{LAST}{NROW}')
    tbl.tableStyleInfo = TableStyleInfo(name='TableStyleLight1', showRowStripes=False)
    dat.add_table(tbl)
    for name, idx in newcols.items():
        dat.column_dimensions[CL(idx)].hidden = True

    # ── 3. live denominators ─────────────────────────────────────────────────
    def denom(hh, what):
        if what == 'cases':
            return f'COUNTIF({TABLE}[hh_group],"{hh}")' if hh != 'all' \
                else f'COUNTA({TABLE}[hh_group])'
        if what == 'errors':
            return (f'COUNTIFS({TABLE}[hh_group],"{hh}",{TABLE}[over_threshold],1)'
                    if hh != 'all' else f'COUNTIF({TABLE}[over_threshold],1)')
        return (f'SUMIFS({TABLE}[total_error_amount],{TABLE}[hh_group],"{hh}",'
                f'{TABLE}[over_threshold],1)' if hh != 'all'
                else f'SUMIFS({TABLE}[total_error_amount],{TABLE}[over_threshold],1)')

    HITS = {'nat': f'{TABLE}[_hit_nat]', 'nl': f'{TABLE}[_hit_nl]'}
    OV, AMT = f'{TABLE}[over_threshold]', f'{TABLE}[total_error_amount]'

    def combined(ws, row, hh, tag, cols):
        """cols maps logical name -> column index on that sheet."""
        hitp = HITS[tag]
        st = '' if hh == 'all' else f'*({TABLE}[hh_group]="{hh}")'
        F, E, D = (CL(cols['flagged']), CL(cols['errors']), CL(cols['dollars']))
        put = lambda k, f: ws.cell(row=row, column=cols[k], value='=' + f)
        put('flagged', f'SUMPRODUCT(({hitp}){st})')
        put('errors',  f'SUMPRODUCT(({hitp}){st}*({OV}))')
        put('dollars', f'SUMPRODUCT(({hitp}){st}*({OV})*({AMT}))')
        put('prec',    f'IF(${F}{row}=0,0,${E}{row}/${F}{row})')
        put('rec',     f'IFERROR(${E}{row}/{denom(hh,"errors")},0)')
        put('drec',    f'IFERROR(${D}{row}/{denom(hh,"dollars")},0)')
        # summary-row workload: share of ALL cases, so the household-size
        # rows are the percentage-point portions that sum to the 'all' row
        put('work',    f'IFERROR(${F}{row}/{denom("all","cases")},0)')
        put('exp',     f'IFERROR(IF(${F}{row}=0,"",${D}{row}/${F}{row}),"")')

    NT = dict(prec=4, rec=5, drec=6, flagged=7, errors=8, dollars=9, work=10, exp=11)
    for i, hh in enumerate(['all', '1', '2-3', '4+']):
        if nat:
            combined(nat, 6 + i, hh, 'nat', NT)
        if natl:
            combined(natl, 6 + i, hh, 'nl', NT)

    # ── 4. per-rule rows ─────────────────────────────────────────────────────
    def rule_row(ws, row, conds, hh, cols):
        if conds is None:
            return
        F, E, D = (CL(cols['flagged']), CL(cols['errors']), CL(cols['dollars']))
        n = countifs(conds, hh)
        e = countifs(conds, hh, extra=f'{TABLE}[over_threshold],1')
        d = countifs(conds, hh, extra=f'{TABLE}[over_threshold],1',
                     col='total_error_amount', fn='SUMIFS')
        put = lambda k, f: ws.cell(row=row, column=cols[k], value='=' + f)
        put('flagged', n); put('errors', e); put('dollars', d)
        # below 10 flagged cases a percentage misleads; show the counts
        put('prec', f'IF(${F}{row}<10,${E}{row}&" errors of "&${F}{row}'
                    f'&" cases flagged",IFERROR(${E}{row}/${F}{row},0))')
        put('rec',  f'IFERROR(${E}{row}/{denom(hh,"errors")},0)')
        put('drec', f'IFERROR(${D}{row}/{denom(hh,"dollars")},0)')
        put('work', f'IFERROR(${F}{row}/{denom(hh,"cases")},0)')
        put('exp',  f'IFERROR(IF(${F}{row}=0,"",${D}{row}/${F}{row}),"")')

    for i, (conds, hh) in enumerate(nt_rules):
        rule_row(nat, N0 + i, conds, hh, NT)
    for i, (conds, hh) in enumerate(nl_rules):
        rule_row(natl, N0 + i, conds, hh, NT)

    # ── 5. Dashboard: fixed ranges -> table references ───────────────────────
    swapped = 0
    dq = qref(DATA_SHEET)
    rng_pat = re.compile(re.escape(dq) + r'!\$([A-Z]{1,3})\$\d+:\$[A-Z]{1,3}\$\d+')
    for row in wb['Dashboard'].iter_rows():
        for cell in row:
            v = cell.value
            if isinstance(v, str) and f'{dq}!$' in v:
                def sub(m):
                    return f'{TABLE}[{hdr[column_index_from_string(m.group(1)) - 1]}]'
                nv = rng_pat.sub(sub, v)
                if nv != v:
                    cell.value = nv; swapped += 1
    print(f'Dashboard: {swapped} formulas rebound to the table')

    wb.save(out)
    print('saved', out)
    print('\nnow run:  python postprocess_workbook.py "%s" <checkbox_cells.json>' % out)


if __name__ == '__main__':
    main()
