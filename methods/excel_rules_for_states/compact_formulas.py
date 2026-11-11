"""
Rewrite a table's this-row references as plain cell addresses (2026-10-29).

    python compact_formulas.py <workbook.xlsx> [<sheet name>] [<table name>]

Every cell formula on the sheet that reads CaseData[[#This Row],[col]] gets
$X<row> instead (column absolute, row relative). The table's
calculatedColumnFormula declarations keep the structured form, so rows added
by a paste are still filled by Excel as before. Excel flags the cells as
inconsistent with their column formula (a green triangle on every cell), so
the sheet also gets an <ignoredErrors calculatedColumn> entry over its range.

Same values (checked 2026-10-29 on the WA vote workbook: every value on Start
Here, Step 2, Step 3 and Vote Score identical after a full recalculation), and
the workbook opens about three times faster: the 373-rule WA vote workbook
took 83 s with structured references and 28 s with addresses. Run after
postprocess_workbook.py, on the finished workbook.
"""
import html
import os
import re
import sys
import zipfile

from openpyxl.utils import get_column_letter as CL
from openpyxl.utils.cell import range_boundaries

from workbook_layout import DATA_SHEET

CELL = re.compile(r'<c r="([A-Z]+)(\d+)"([^>]*)><f>([^<]*)</f>')
# the tags that follow <ignoredErrors> in a worksheet (ECMA-376 order)
AFTER = re.compile(r'<(smartTags|drawing|legacyDrawing|legacyDrawingHF|drawingHF|picture|oleObjects|controls|'
                   r'webPublishItems|tableParts|extLst)\b')


def _parts(z):
    """{sheet name: worksheet part}, {table name: table part}."""
    wbx = z.read('xl/workbook.xml').decode()
    rels = z.read('xl/_rels/workbook.xml.rels').decode()
    tgt = {}
    for rel in re.findall(r'<Relationship [^>]*/>', rels):
        rid = re.search(r'Id="([^"]+)"', rel).group(1)
        t = re.search(r'Target="([^"]+)"', rel).group(1)
        tgt[rid] = t.lstrip('/') if t.startswith('/') else 'xl/' + t
    sheets = {html.unescape(n): tgt[r] for n, r in re.findall(r'<sheet [^>]*name="([^"]+)"[^>]*r:id="([^"]+)"', wbx)}
    tables = {}
    for n in z.namelist():
        if n.startswith('xl/tables/') and n.endswith('.xml'):
            m = re.search(r'displayName="([^"]+)"', z.read(n).decode())
            if m:
                tables[m.group(1)] = n
    return sheets, tables


def compact(path, sheet=DATA_SHEET, table='CaseData'):
    zin = zipfile.ZipFile(path)
    sheets, tables = _parts(zin)
    part = sheets[sheet]
    tx = zin.read(tables[table]).decode()
    c0, r0, _, _ = range_boundaries(re.search(r'<table [^>]*ref="([^"]+)"', tx).group(1))
    names = [html.unescape(n) for n in re.findall(r'<tableColumn [^>]*name="([^"]*)"', tx)]
    col_of = {n: CL(c0 + k) for k, n in enumerate(names)}
    ref = re.compile(re.escape(table) + r'\[\[#This Row\],\[([^\]\[]+)\]\]')
    x = zin.read(part).decode()
    n_before = len(x)
    n_cells = 0

    def cell(m):
        nonlocal n_cells
        f = m.group(4)
        if f'{table}[[#This Row]' not in f:
            return m.group(0)
        n_cells += 1
        row = m.group(2)
        f = ref.sub(lambda r: f'${col_of[html.unescape(r.group(1))]}{row}', f)
        return f'<c r="{m.group(1)}{row}"{m.group(3)}><f>{f}</f>'

    x = CELL.sub(cell, x)
    dim = re.search(r'<dimension ref="([^"]+)"', x).group(1)
    ign = f'<ignoredError sqref="{dim}" calculatedColumn="1"/>'
    if '<ignoredErrors>' in x:
        x = x.replace('<ignoredErrors>', '<ignoredErrors>' + ign, 1)
    else:
        m = AFTER.search(x, x.index('</sheetData>'))
        i = m.start() if m else x.rindex('</worksheet>')
        x = x[:i] + f'<ignoredErrors>{ign}</ignoredErrors>' + x[i:]
    tmp = path + '.compact.tmp'
    with zipfile.ZipFile(tmp, 'w', zipfile.ZIP_DEFLATED) as zo:
        for it in zin.infolist():
            zo.writestr(it.filename, x.encode() if it.filename == part else zin.read(it.filename),
                        compress_type=zipfile.ZIP_DEFLATED)
    zin.close()
    os.replace(tmp, path)
    left = x.count(f'{table}[[#This Row]')
    print(f'compact_formulas: {sheet}: {n_cells} cell formulas use addresses; sheet XML '
          f'{n_before / 1e6:.0f} MB -> {len(x) / 1e6:.0f} MB; {left} this-row references left')
    return n_cells


if __name__ == '__main__':
    compact(sys.argv[1], *sys.argv[2:4])
