"""Drop low-dollar-band rows from the tracked blended delivery lists, in place.

One-off maintenance filter (2026-08-24): rows whose rule carries a two-sided
dollar interval sitting entirely at or below $5 (rule_selection.low_dollar_band,
the crumbs-not-income artifact signature) are removed from every
state_delivery_lists/blended_delivery_*.csv. Ranks are NOT renumbered - the
surviving rows keep their original `rank`, so rule ids stay stable and the
sequence simply has gaps. The workbook build already drops these rules at its
own gate, so workbook content is unchanged; the buffer just runs slightly
shallower than 3x depth until the delivery builder regenerates the lists with
the upstream fix.

Rewrites raw lines (not a csv round-trip) so the only diff is the dropped rows.
"""
import csv
import glob
import io
import os
import sys

PKG = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, PKG)
from rule_selection import COND_PAT, low_dollar_band  # noqa: E402

LISTS = os.path.join(PKG, '..', '..', 'state_delivery_lists')


def conds(text):
    return [{'var': v, 'op': o, 'thr': float(t)} for v, o, t in COND_PAT.findall(text)]


def main():
    total_dropped = 0
    files_changed = 0
    for path in sorted(glob.glob(os.path.join(LISTS, 'blended_delivery_*.csv'))):
        with open(path, newline='', encoding='utf-8') as fh:
            raw = fh.read()
        lines = raw.splitlines(keepends=True)
        rows = list(csv.DictReader(io.StringIO(raw)))
        # line-per-row alignment: no field in these lists contains a newline
        assert len(lines) == len(rows) + 1, f'{path}: embedded newline?'
        drop = {i for i, r in enumerate(rows) if low_dollar_band(conds(r['rule']))}
        if not drop:
            continue
        kept = [lines[0]] + [ln for i, ln in enumerate(lines[1:]) if i not in drop]
        with open(path, 'w', newline='', encoding='utf-8') as fh:
            fh.writelines(kept)
        # verify the rewrite parses to the expected row count
        with open(path, newline='', encoding='utf-8') as fh:
            assert len(list(csv.DictReader(fh))) == len(rows) - len(drop), path
        total_dropped += len(drop)
        files_changed += 1
        print(f'{os.path.basename(path)}: dropped {len(drop)} of {len(rows)}')
    print(f'\n{files_changed} files changed, {total_dropped} rows dropped')


if __name__ == '__main__':
    main()
