"""Run the tiered tuning under more than one year split and print them side by
side.

    python compare_splits.py                # default: WA, both splits below
    python compare_splits.py VA

Splits compared:
  A  tune FY2022+2023, hold out FY2024   (forward: the deployment setting)
  B  tune FY2022+2024, hold out FY2023   (year swap, as in
                                          state_threshold_gridsearch_v2.R)

B interpolates: the held-out year sits between the tuning years, so it flatters
every arm and is not a deployment expectation. It is here because agreement
between the two is evidence a verdict is not an artifact of one year, and
disagreement says the verdict rests on which year was held out.

Reads the prepared case frame that build_workbook_v2.py writes
(`<state dir>/<abbr>_cases.csv`), so no feature derivation is repeated here. Run
the builder first.
"""
import os
import re
import sys

import pandas as pd

import states as STATE_REGISTRY
import tuning

PKG = os.path.dirname(os.path.abspath(__file__))
BASE = os.path.dirname(PKG)
NSLOTS = 4

SPLITS = [
    ('A  forward   tune 2022+2023 -> hold 2024', {'holdout_years': 1}),
    ('B  year swap tune 2022+2024 -> hold 2023', {'holdout_year_set': (2023,)}),
]


def load_cases(abbr):
    for c in (os.environ.get('SNAP_OUT_DIR'),
              os.path.join(BASE, 'state spreadsheets', abbr),
              os.path.join(PKG, '.build', f'out_{abbr}')):
        if c and os.path.isfile(os.path.join(c, f'{abbr.lower()}_cases.csv')):
            return os.path.join(c, f'{abbr.lower()}_cases.csv')
    raise SystemExit(f'no prepared case frame for {abbr}; run build_workbook_v2.py first')


def load_rules(cfg):
    # the builder is deliberately not imported: importing it rebuilds everything
    rel = cfg['delivery_csv']
    base = os.path.basename(rel).replace('_core.csv', '.csv')
    repo = os.environ.get('SNAP_REPO')
    cands = [os.path.join(BASE, rel), os.path.join(PKG, rel)]
    if repo:
        cands += [os.path.join(repo, rel), os.path.join(repo, 'state_delivery_lists', base)]
    d = PKG
    for _ in range(6):
        cands.append(os.path.join(d, 'state_delivery_lists', base))
        cands.append(os.path.join(d, 'snap_qc', 'state_delivery_lists', base))
        d = os.path.dirname(d)
    path = next((c for c in cands if os.path.isfile(c)), None)
    if path is None:
        raise SystemExit(f'delivery list not found: {rel}')
    rl = pd.read_csv(path)
    rl = rl[rl['role'] == cfg['role_filter']].sort_values('rank')
    pat = re.compile(r'([A-Za-z_][A-Za-z0-9_]*)\s*(>=|<=|>|<|==)\s*(-?[0-9.]+)')
    rules = []
    for _, rr in rl.iterrows():
        conds = [{'var': v, 'op': o, 'thr': float(t)} for v, o, t in pat.findall(rr['rule'])]
        assert 1 <= len(conds) <= NSLOTS, rr['rule']
        rules.append({'num': int(rr['rank']), 'hh': str(rr['hh']), 'conds': conds})
    print(f'delivery list: {os.path.basename(path)} ({len(rules)} {cfg["role_filter"]} rules)')
    return rules


def main():
    args = [a for a in sys.argv[1:] if not a.startswith('-')]
    abbr = (args[0] if args else 'WA').upper()
    cfg = STATE_REGISTRY.get(abbr)
    base_tuning = dict(getattr(STATE_REGISTRY, 'TUNING', {}))

    cases = load_cases(abbr)
    df = pd.read_csv(cases)
    print(f'cases: {cases} ({len(df)} rows)')
    rules = load_rules(cfg)

    results = {}
    for label, override in SPLITS:
        print('\n' + '=' * 78)
        print(f'SPLIT {label}')
        print('=' * 78)
        tc = tuning.TuningConfig(**{**base_tuning, **override})
        results[label] = tuning.run(df, rules, tc)

    print('\n' + '=' * 78)
    print(f'SIDE BY SIDE — {cfg["name"]}, review budget '
          f'{base_tuning.get("budget", 0.05):.0%}, tier ceiling '
          f'{base_tuning.get("max_tier", 2)}')
    print('=' * 78)
    hdr = ['metric'] + [l.split()[0] for l in results]
    rows = []

    def add(name, fn):
        rows.append([name] + [fn(r) for r in results.values()])

    add('tuning years', lambda r: '+'.join(str(y) for y in r.tune_years))
    add('held-out year', lambda r: '+'.join(str(y) for y in r.hold_years))
    add('interpolates?', lambda r: 'YES' if min(r.hold_years) < max(r.tune_years) else 'no')
    add('tier deployed', lambda r: f'Tier {r.tier}')
    add('cleared support floor', lambda r: int(r.rules_table.support_ok.sum()))
    add('widest rule (tuning n)', lambda r: int(r.rules_table.tune_n.max()))
    add('admitted by BH', lambda r: int(r.rules_table.admitted.sum()))
    add('thresholds moved', lambda r: int(r.rules_table.threshold_moved.sum()))
    add('variants searched', lambda r: r.comparisons['tier2_variants'])
    for field, name, fmt in [
        ('n_rules', 'rules deployed', '{:d}'),
        ('tune_prec', 'tuning precision', '{:.3f}'),
        ('hold_flagged', 'held-out flagged', '{:d}'),
        ('hold_errors', 'held-out errors caught', '{:d}'),
        ('hold_prec', 'held-out precision', '{:.3f}'),
        ('hold_prec_lcb', 'held-out precision bound', '{:.3f}'),
        ('hold_rec', 'held-out recall', '{:.3f}'),
        ('hold_drec', 'held-out dollar recall', '{:.3f}'),
        ('hold_workload', 'held-out workload', '{:.3f}'),
        ('deflation', 'deflation (held-out/tuning)', '{:.3f}'),
    ]:
        def get(r, f=field, fm=fmt):
            a = r.arms[r.arms.tier == r.tier]
            if a.empty:
                return '-'
            v = a.iloc[0][f]
            try:
                return fm.format(int(v) if fm.endswith('d}') else float(v))
            except (TypeError, ValueError):
                return str(v)
        add(name, get)

    w = max(len(str(r[0])) for r in rows) + 2
    print(''.ljust(w) + ''.join(str(h).rjust(16) for h in hdr[1:]))
    for r in rows:
        print(str(r[0]).ljust(w) + ''.join(str(x).rjust(16) for x in r[1:]))
    print('\nA is the deployment expectation. B is a robustness check only: it')
    print('interpolates, so its held-out numbers are optimistic by construction.')


if __name__ == '__main__':
    main()
