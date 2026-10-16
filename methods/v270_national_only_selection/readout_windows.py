"""Readout for score_windows.R: national-only vs blended per state and budget.

Window w2224: pools mined on FY2022-23, scored on FY2024.
Window w1719: pools mined on FY2017-19, scored on FY2022, FY2023 and FY2024
(each year walked at its own budget; the window's precision pools the three
years: errors caught / cases flagged).
Writes paired_by_window.csv (one row per window x state x budget) and prints
the counts, pooled precision, and the within-state companions (mean, median,
harmed tail = national worse than blended by more than 0.05).
    python methods/v270_national_only_selection/readout_windows.py
"""
import glob
import pandas as pd

D = 'methods/v270_national_only_selection'
s = pd.concat([pd.read_csv(f) for f in sorted(glob.glob(f'{D}/scores_w*_[ab].csv'))])
assert s.groupby('window').state.nunique().eq(49).all(), s.groupby('window').state.nunique()
print('cells with a core fill gap:', s[s.fill_gap_core > 0][['window', 'state', 'budget', 'arm']].drop_duplicates().shape[0])

def pooled(g):
    return pd.Series({'n_flagged': g.n_flagged.sum(), 'n_errors': g.n_errors.sum(),
                      'dollars_caught': g.dollars_caught.sum(), 'dollars_total': g.dollars_total.sum(),
                      'n_te': g.n_te.sum(), 'n_err_te': g.n_err_te.sum(),
                      'n_state_rules_core': g.n_state_rules_core.max()})

agg = s.groupby(['window', 'state', 'budget', 'arm']).apply(pooled).reset_index()
agg['precision'] = agg.n_errors / agg.n_flagged.clip(lower=1)
agg['dollar_recall'] = agg.dollars_caught / agg.dollars_total.clip(lower=1)
w = agg.pivot_table(index=['window', 'state', 'budget'], columns='arm',
                    values=['precision', 'dollar_recall', 'n_flagged', 'n_errors', 'dollars_caught',
                            'n_state_rules_core']).reset_index()
w.columns = ['_'.join(c).strip('_') for c in w.columns]
w['d_precision'] = w.precision_national - w.precision_blended
w['national_ge_blended'] = w.precision_national >= w.precision_blended
w['no_state_rule_in_core'] = w.n_state_rules_core_blended == 0
w.to_csv(f'{D}/paired_by_window.csv', index=False)

label = {'w2224': 'mine FY2022-23, score FY2024', 'w1719': 'mine FY2017-19, score FY2022-24'}
for win in ('w2224', 'w1719'):
    print(f'\n== {win}: {label[win]}')
    for b in (0.05, 0.10):
        q = w[(w.window == win) & (w.budget == b)]
        pn = q.n_errors_national.sum() / q.n_flagged_national.sum()
        pb = q.n_errors_blended.sum() / q.n_flagged_blended.sum()
        dn = q.dollars_caught_national.sum(); db = q.dollars_caught_blended.sum()
        print(f'  budget {b:.0%}: national >= blended {q.national_ge_blended.sum()}/49 '
              f'(national higher {(q.d_precision > 0).sum()}, tie {(q.d_precision == 0).sum()} '
              f'[{q.no_state_rule_in_core.sum()} with no state rule in the blended core], blended higher {(q.d_precision < 0).sum()})')
        print(f'      pooled precision national {pn:.4f} vs blended {pb:.4f}; '
              f'errors caught {q.n_errors_national.sum():.0f} vs {q.n_errors_blended.sum():.0f}; '
              f'error dollars caught {dn:,.0f} vs {db:,.0f} ({dn / db - 1:+.1%})')
        print(f'      within-state d_precision mean {q.d_precision.mean():+.4f}, median {q.d_precision.median():+.4f}; '
              f'national worse by > 0.05 in {(q.d_precision < -0.05).sum()}, better by > 0.05 in {(q.d_precision > 0.05).sum()}')

# per test year inside w1719 (is the verdict stable year to year?)
print('\n== w1719 by test year (national >= blended, of 49)')
y = s[s.window == 'w1719'].pivot_table(index=['state', 'budget', 'test_year'], columns='arm',
                                        values='precision').reset_index()
y['ge'] = y.national >= y.blended
print(y.groupby(['budget', 'test_year']).ge.sum().unstack())

# agreement between the two windows
v = w.pivot_table(index=['state', 'budget'], columns='window', values='national_ge_blended').reset_index()
v = v.rename(columns={'w1719': 'ge_w1719', 'w2224': 'ge_w2224'})
v['ge_w1719'] = v.ge_w1719.astype(bool); v['ge_w2224'] = v.ge_w2224.astype(bool)
print('\n== agreement between windows')
for b in (0.05, 0.10):
    q = v[v.budget == b]
    print(f'  budget {b:.0%}: both windows national {(q.ge_w1719 & q.ge_w2224).sum()}, '
          f'both blended {(~q.ge_w1719 & ~q.ge_w2224).sum()}, disagree {(q.ge_w1719 != q.ge_w2224).sum()} '
          f'| national in either window {(q.ge_w1719 | q.ge_w2224).sum()}')
v.to_csv(f'{D}/verdicts_by_window.csv', index=False)
