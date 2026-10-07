"""Merge test for the pile gate (project lead, 2026-10-07): compare each
state's workbook headline (all shipped rules combined, on the state's
FY2022-24 demo cases) before and after the gate. Merge to main when at least
45 states lose no more than 3 percentage points of precision.

Old figures: methods/excel_rules_for_states/.build/pre_pilegate_2026-10-07/
headline_old.csv (static union of the v2.7.0 workbooks as published,
recorded before the rebuild). New figures: the same static union
(make_state.static_union) on the rebuilt plain builds. Writes
workbook_headline_comparison.csv beside this script.
    python methods/reconstruction_income_piles/compare_workbook_headline.py
"""
import os
import sys
import pandas as pd

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
PKG = os.path.join(ROOT, 'methods', 'excel_rules_for_states')
sys.path.insert(0, PKG)
os.chdir(PKG)
import make_state  # noqa: E402

old = pd.read_csv('.build/pre_pilegate_2026-10-07/headline_old.csv')
new = []
for st in old.state:
    plain = os.path.join('.build', f'out_{st}', f'SNAP_flagging_rules_{st}.xlsx')
    f, e = make_state.static_union(plain, st)
    new.append({'state': st, 'flagged_new': f, 'errors_new': e, 'precision_new': round(e / max(f, 1), 4)})
c = old.merge(pd.DataFrame(new), on='state')
c['d_precision_pp'] = (100 * (c.precision_new - c.precision_old)).round(2)
c['released'] = ~c.state.isin(['DC', 'GA'])
c.to_csv(os.path.join(ROOT, 'methods', 'reconstruction_income_piles', 'workbook_headline_comparison.csv'), index=False)
for lab, q in (('all 49 states', c), ('47 released workbooks', c[c.released])):
    ok = (q.d_precision_pp >= -3).sum()
    print(f'{lab}: lost no more than 3 points in {ok} of {len(q)} | median change {q.d_precision_pp.median():+.2f} pp, '
          f'mean {q.d_precision_pp.mean():+.2f} pp, worst {q.d_precision_pp.min():+.2f} pp ({q.state[q.d_precision_pp.idxmin()]}) | '
          f'pooled precision {q.errors_old.sum() / q.flagged_old.sum():.4f} -> {q.errors_new.sum() / q.flagged_new.sum():.4f} | '
          f'flagged {q.flagged_old.sum()} -> {q.flagged_new.sum()}')
q = c[c.released]
print('MERGE TEST (released workbooks, >= 45 within 3 points):', 'PASS' if (q.d_precision_pp >= -3).sum() >= 45 else 'FAIL')
print(c.sort_values('d_precision_pp').head(8).to_string(index=False))
