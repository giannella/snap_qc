"""Build a measurement-tier CSV whose NEW rules are selected with the frozen
CT selection score (uncommitted; state-specific work).

    python build_scored_tier.py CT --new 200 --top 140 \
        --exclude-csv .build/measurement_rules_CT.csv
    python build_scored_tier.py MI --new 175 --top 100 \
        --base-csv .build/measurement_rules_MI.csv \
        --exclude-csv .build/measurement_rules_MI.csv

Base rows (tier A): the state's delivery list by default, or --base-csv
verbatim (e.g. the state's existing measurement CSV, to ADD a block to an
already-delivered book). New rules come from the national pool with
characterization, pass every delivery gate build_measurement_tier applies
(support n >= 30, mm gate, condition gates, expected state flags >= 15),
exclude anything already on the base or in --exclude-csv, and are scored
with the frozen constants (.build/frozen_score_CT_2026-08-28.json) — no
re-standardization. Selection: the top --top rules by frozen score, then a
spread slice round-robining across score quintiles of the remainder by
descending LCB until --new is reached. New ranks start at 1001 so the
block's vintage is unambiguous in share-backs.
"""
import argparse
import json
import os
import re
import sys

import numpy as np
import pandas as pd

PKG = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.dirname(os.path.dirname(PKG))
sys.path.insert(0, PKG)
import states as STATE_REGISTRY                       # noqa: E402
from build_measurement_tier import (DELIVERY_COLS, MIN_EXPECTED, MIN_POOL_N,  # noqa: E402
                                    POOL_CSV, CHAR_CSV, conds_of, gates_pass)
from rule_selection import cond_mask, smd_dead_zone   # noqa: E402

FROZEN = os.path.join(PKG, '.build', 'frozen_score_CT_2026-08-28.json')
RANK0 = 1001
COND = re.compile(r'([A-Za-z_][A-Za-z0-9_]*)\s*(>=|<=|>|<)\s*(-?[0-9.]+)')


def content_flags(text):
    # identical derivation to the CT share-back analysis (qa_selection.py)
    conds = COND.findall(str(text))
    vars_ = {v for v, _, _ in conds}
    stale = any(v == 'months_since_cert_n' and op in ('>', '>=')
                and float(t) >= 5 for v, op, t in conds)
    return {
        'uses_stale_cert': int(stale),
        'uses_benmax': int(bool({'rawben_rel_max',
                                 'unc_rawben_rel_max'} & vars_)),
        'uses_earned': int('earned_by_hh_size' in vars_),
        'uses_shelter': int(bool({'shelter_expenses_by_hh_size', 'utilities',
                                  'utilities_sua'} & vars_)),
    }


def frozen_score(df, consts):
    s = np.zeros(len(df))
    for col, c in consts['components'].items():
        s += c['sign'] * (df[col].astype(float).values - c['mean']) / c['sd']
    return s / len(consts['components'])


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('state')
    ap.add_argument('--new', type=int, required=True)
    ap.add_argument('--top', type=int, required=True)
    ap.add_argument('--base-csv', default=None)
    ap.add_argument('--exclude-csv', action='append', default=[])
    ap.add_argument('--min-expected', type=int, default=MIN_EXPECTED)
    a = ap.parse_args()
    st = a.state.upper()
    cfg = STATE_REGISTRY.get(st)
    consts = json.load(open(FROZEN))
    print(f"frozen constants: {consts['n_reference_rules']} reference rules, "
          f"top-third cutpoint {consts['top_third_cutpoint']:+.4f}")

    frame = pd.read_csv(os.path.join(PKG, '.frames', f'{st.lower()}_frame.csv'))
    delivery = pd.read_csv(os.path.join(REPO, cfg['delivery_csv']))
    base = (pd.read_csv(a.base_csv) if a.base_csv else delivery.copy())
    assert base['rank'].max() < RANK0, \
        f"base ranks reach {base['rank'].max()}; raise RANK0"

    pool = pd.read_csv(POOL_CSV)
    char = pd.read_csv(CHAR_CSV)
    cand = pool.merge(char, on=['hh', 'rule'], how='inner')
    print(f'pool rules with characterization: {len(cand)}')

    seen = set(zip(base['hh'], base['rule'])) \
        | set(zip(delivery['hh'], delivery['rule']))
    for x in a.exclude_csv:
        e = pd.read_csv(x)
        seen |= set(zip(e['hh'], e['rule']))
    cand = cand[~cand.apply(lambda r: (r['hh'], r['rule']) in seen, axis=1)]
    cand = cand[cand['n'] >= MIN_POOL_N]
    cand = cand[(cand['mm_n'] / cand['n'] < 0.25)
                & (cand['mm_k'] / cand['k'].clip(lower=1) < 0.25)]
    med_zone = smd_dead_zone(cfg['name'], REPO)
    cand = cand[cand['rule'].apply(
        lambda t: gates_pass(conds_of(t), med_zone))].copy()
    print(f'after exclusions, support floor, mm gate, gates: {len(cand)}')

    hh_col = frame['hh_group'].astype(str).values
    flags = []
    for _, r in cand.iterrows():
        m = (hh_col == str(r['hh'])) & cond_mask(frame, conds_of(r['rule']))
        flags.append(int(m.sum()))
    cand['n_flagged_state'] = flags
    for probe in (15, 10, 8, 5):
        print(f'  candidates at expected-flags floor >= {probe}: '
              f'{(cand["n_flagged_state"] >= probe).sum()}')
    cand = cand[cand['n_flagged_state'] >= a.min_expected].copy()
    print(f'after expected-flags floor (>= {a.min_expected}): {len(cand)}')

    for k, v in pd.DataFrame(
            [content_flags(t) for t in cand['rule']]).items():
        cand[k] = v.values
    cand['score'] = frozen_score(cand, consts)
    cand = cand.sort_values(['score', 'lcb'], ascending=False)

    top = cand.head(a.top).copy()
    top['picked_via'] = 'top'
    rest = cand.iloc[a.top:].copy()
    n_spread = max(0, a.new - len(top))
    rest['quint'] = pd.qcut(rest['score'], 5, labels=False, duplicates='drop')
    rest = rest.sort_values('lcb', ascending=False)
    iters = {q: rest[rest['quint'] == q].iterrows()
             for q in sorted(rest['quint'].dropna().unique())}
    spread, exhausted = [], set()
    while len(spread) < n_spread and len(exhausted) < len(iters):
        for q in sorted(iters):
            if len(spread) >= n_spread or q in exhausted:
                continue
            try:
                spread.append(next(iters[q])[1])
            except StopIteration:
                exhausted.add(q)
    spread = pd.DataFrame(spread)
    if len(spread):
        spread['picked_via'] = 'spread'
    picked = pd.concat([top, spread], ignore_index=True)
    n_above = int((picked['score'] >= consts['top_third_cutpoint']).sum())
    print(f"picked {len(picked)}: {len(top)} top + {len(spread)} spread | "
          f"score range {picked['score'].min():+.3f}..{picked['score'].max():+.3f}"
          f" | {n_above} at/above the frozen top-third cutpoint")

    b_rows = []
    for i, (_, r) in enumerate(picked.iterrows()):
        k, n = float(r['k']), float(r['n'])
        b_rows.append({
            'rank': RANK0 + i, 'role': 'measure', 'rule': r['rule'],
            'hh': r['hh'], 'pool': 'national', 'engines': r['engines'],
            'mined_frames': r['mined_frames'], 'n_flagged_train': int(n),
            'precision_train': round(k / n, 4),
            'precision_train_lcb': round(float(r['lcb']), 4),
            'dollars_per_flag_train': round(float(r['doll']) / n, 2),
            'mm_share_flags': round(float(r['mm_n']) / n, 4),
            'mm_share_errors': round(float(r['mm_k']) / max(k, 1), 4),
            'mm_inflation': '',
            'n_flagged_state': int(r['n_flagged_state']),
            'n_new_at_rank': 0,
            'n_error_cases_national': int(r['n_error_cases']),
            'element_groups_to_75': r['element_groups_to_75'],
            'nature_groups_to_75': r['nature_groups_to_75'],
            'found_in_case_record': r['found_in_case_record'],
            'share_overissuance': r['share_overissuance'],
            'timing_at_certification': r['timing_at_certification'],
            'cause_agency': r['cause_agency'],
        })
    out = pd.concat([base[DELIVERY_COLS], pd.DataFrame(b_rows)[DELIVERY_COLS]],
                    ignore_index=True)
    out_path = os.path.join(PKG, '.build', f'measurement_rules_{st}_scored.csv')
    out.to_csv(out_path, index=False)
    rep = picked[['hh', 'rule', 'score', 'picked_via', 'lcb', 'n', 'k',
                  'nat_change_in_circumstances', 'timing_after_certification',
                  'uses_stale_cert', 'uses_earned', 'uses_shelter',
                  'uses_benmax', 'n_flagged_state']]
    rep_path = os.path.join(PKG, '.build', f'scored_tier_report_{st}.csv')
    rep.to_csv(rep_path, index=False)
    print(f'wrote {out_path}: {len(base)} base + {len(b_rows)} new rows')
    print(f'wrote {rep_path}')


if __name__ == '__main__':
    main()
