"""Readout for gate_removal_test.R: pile gate on vs off, per list type, budget,
window and scoring (all test cases / non-pile test cases).

Each state x budget cell pools its test years (w1719: FY2022-24) as errors
caught / cases flagged. Reports pooled precision and error-dollar recall for
both arms, and the within-state paired change (gate on minus off): median,
mean, harmed tail (worse than -0.05) and helped tail (better than +0.05).
Writes gate_paired.csv.
    python methods/reconstruction_income_piles/readout_gate.py
"""
import glob
import pandas as pd

D = 'methods/reconstruction_income_piles'
s = pd.concat([pd.read_csv(f) for f in sorted(glob.glob(f'{D}/gate_scores_w*.csv'))
               if not f.endswith('_smoke.csv')])
print('fill gaps (core) > 0:', int((s.fill_gap_core > 0).sum()), '| cells:', len(s))
key = ['window', 'list', 'scoring', 'state', 'budget', 'gate']
g = s.groupby(key).agg(n_flagged=('n_flagged', 'sum'), n_errors=('n_errors', 'sum'),
                       dc=('dollars_caught', 'sum'), dt=('dollars_total', 'sum'),
                       n_te=('n_te', 'sum')).reset_index()
g['precision'] = g.n_errors / g.n_flagged.clip(lower=1)
g['dollar_recall'] = g.dc / g.dt.clip(lower=1)
w = g.pivot_table(index=key[:-1], columns='gate',
                  values=['precision', 'dollar_recall', 'n_flagged', 'n_errors', 'dc', 'dt']).reset_index()
w.columns = ['_'.join(c).strip('_') for c in w.columns]
w['d_precision'] = w.precision_on - w.precision_off
w['d_dollar_recall'] = w.dollar_recall_on - w.dollar_recall_off
w.to_csv(f'{D}/gate_paired.csv', index=False)
label = {'w2224': 'mine FY2022-23, score FY2024', 'w1719': 'mine FY2017-19, score FY2022-24'}
for win in ('w1719', 'w2224'):
    for sc in ('nonpile', 'all'):
        print(f'\n== {win} ({label[win]}), scored on {"non-pile test cases" if sc == "nonpile" else "all test cases"}')
        for lst in ('blended', 'national'):
            for b in (0.05, 0.10):
                q = w[(w.window == win) & (w.scoring == sc) & (w.list == lst) & (w.budget == b)]
                if q.empty:
                    continue
                po = q.n_errors_off.sum() / q.n_flagged_off.sum(); pn = q.n_errors_on.sum() / q.n_flagged_on.sum()
                print(f'  {lst:8s} {b:.0%}: precision off {po:.4f} -> on {pn:.4f} | error $ caught {q.dc_on.sum() / q.dc_off.sum() - 1:+.1%} | '
                      f'within-state median {q.d_precision.median():+.4f}, mean {q.d_precision.mean():+.4f}, '
                      f'harmed {(q.d_precision < -0.05).sum()}, helped {(q.d_precision > 0.05).sum()}, unchanged {(q.d_precision == 0).sum()} of {len(q)}')
                print(f'  {"":8s}      error-dollar recall off {q.dc_off.sum() / q.dt_off.sum():.4f} -> on {q.dc_on.sum() / q.dt_on.sum():.4f} | '
                      f'within-state median {q.d_dollar_recall.median():+.4f}, mean {q.d_dollar_recall.mean():+.4f}, '
                      f'harmed {(q.d_dollar_recall < -0.05).sum()}, helped {(q.d_dollar_recall > 0.05).sum()}')
