"""Build a state's measurement-tier CSV for the 300-rule learning workbook.

    python build_measurement_tier.py <ST> --target 300

Design (share_back_transfer_plan.md, 2026-08-27): the tier exists to give
the transfer regression SPREAD on its predictors, not to extend the ranking.

  Tier A - the remainder of the state's own delivery list (every delivery
           row; rule_selection dedups the ones already deployed).
  Tier B - national-pool rules chosen for characterization coverage:
           candidates = pool rules with characterization, not already on the
           state's list, pool support n >= 30, no bbce_state_i conjunct,
           passing every delivery-transform gate, and expected internal
           flags >= MIN_EXPECTED on the state frame. Selection: first
           guarantee every primary nature group NATURE_MIN rules (by pool
           LCB), then round-robin across found_in_case_record terciles x
           cause_agency terciles by descending LCB until the workbook total
           reaches the target.

Output: .build/measurement_rules_<ST>.csv in the delivery-CSV schema
(tier B ranks start at 501). Build the workbook with
SNAP_MEASURE_CSV=<that file> make_state.py <ST>.
"""
import argparse
import os
import sys

import numpy as np
import pandas as pd

PKG = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.dirname(os.path.dirname(PKG))
sys.path.insert(0, PKG)
import states as STATE_REGISTRY                       # noqa: E402
from rule_selection import (COND_PAT, DROP_VARS, STRIP_VAR, cond_mask,   # noqa: E402
                            low_dollar_band, narrow_interval,
                            ratio_span_too_small, smd_dead_zone)

POOL_CSV = os.path.join(PKG, '.build', 'national_pool_export.csv')
CHAR_CSV = os.path.join(REPO, 'methods', 'v250_candidate_lists_utilsua',
                        'rule_characterization_v250.csv')
MIN_POOL_N = 30
MIN_EXPECTED = 15
NATURE_MIN = 5
TIER_B_RANK0 = 501

NATURE_COLS = ['nat_wrong_amount', 'nat_wrong_include/exclude_decision',
               'nat_unreported_source_of_income', 'nat_household_composition',
               'nat_change_in_circumstances', 'nat_method_or_computation',
               'nat_reporting_system_or_process', 'nat_limits_and_thresholds',
               'nat_child_support_handling', 'nat_other']

DELIVERY_COLS = ['rank', 'role', 'rule', 'hh', 'pool', 'engines',
                 'mined_frames', 'n_flagged_train', 'precision_train',
                 'precision_train_lcb', 'dollars_per_flag_train',
                 'mm_share_flags', 'mm_share_errors', 'mm_inflation',
                 'n_flagged_state', 'n_new_at_rank', 'n_error_cases_national',
                 'element_groups_to_75', 'nature_groups_to_75',
                 'found_in_case_record', 'share_overissuance',
                 'timing_at_certification', 'cause_agency']


def conds_of(text):
    return [{'var': v, 'op': o, 'thr': float(t)}
            for v, o, t in COND_PAT.findall(text)]


def gates_pass(conds, med_zone):
    if any(c['var'] in DROP_VARS or c['var'] == STRIP_VAR for c in conds):
        return False
    if narrow_interval(conds) or ratio_span_too_small(conds) \
            or low_dollar_band(conds):
        return False
    if med_zone and any(c['var'] == 'medical_deductions'
                        and med_zone[0] <= c['thr'] <= med_zone[1]
                        for c in conds):
        return False
    return True


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('state')
    ap.add_argument('--target', type=int, default=300)
    a = ap.parse_args()
    st = a.state.upper()
    cfg = STATE_REGISTRY.get(st)

    frame = pd.read_csv(os.path.join(PKG, '.frames', f'{st.lower()}_frame.csv'))
    delivery = pd.read_csv(os.path.join(REPO, cfg['delivery_csv']))
    eff = pd.read_csv(os.path.join(PKG, '.build', f'effective_rules_{st}.csv'))
    if not os.path.isfile(POOL_CSV):
        # provenance: this script is the one writer of the pool export
        import subprocess
        subprocess.run([os.environ.get('RSCRIPT', r'C:\Program Files\R\R-4.5.1'
                                       r'\bin\Rscript.exe'), '-e',
                        "p <- readRDS('state_delivery_lists/"
                        "national_rule_pool_2022_2024_v250.rds'); "
                        f"write.csv(p, '{POOL_CSV.replace(os.sep, '/')}', "
                        "row.names = FALSE)"], cwd=REPO, check=True)
    pool = pd.read_csv(POOL_CSV)
    char = pd.read_csv(CHAR_CSV)
    # tier-A ranks must stay below the tier-B block (review B3)
    assert delivery['rank'].max() < TIER_B_RANK0, \
        f"delivery ranks reach {delivery['rank'].max()}; raise TIER_B_RANK0"

    # the deployed base is the SHIPPED subset of the effective list; an
    # effective csv from a previous measurement build carries ship = False
    # rows that must not count (nor suppress tier sizing)
    if 'ship' in eff.columns:
        eff = eff[eff['ship'].fillna(True).astype(str).str.lower()
                  != 'false'].copy()
    # tier A: the whole delivery list; rule_selection dedups deployed rows
    tier_a = delivery.copy()
    # simulate the dedup + gates to know how many tier-A rows will survive
    med_zone = smd_dead_zone(cfg['name'], REPO)
    deployed = set(zip(eff['hh'], eff['rule']))
    a_kept = sum(1 for _, r in tier_a.iterrows()
                 if (r['hh'], r['rule']) not in deployed
                 and gates_pass(conds_of(r['rule']), med_zone))
    n_b = max(0, a.target - len(eff) - a_kept)
    print(f'{st}: shipped {len(eff)} + tier A ~{a_kept} -> '
          f'tier B target {n_b}')

    # tier B candidates
    cand = pool.merge(char, on=['hh', 'rule'], how='inner')
    print(f'pool rules with characterization: {len(cand)}')
    on_list = set(zip(delivery['hh'], delivery['rule']))
    cand = cand[~cand.apply(lambda r: (r['hh'], r['rule']) in on_list, axis=1)]
    cand = cand[cand['n'] >= MIN_POOL_N]
    # measurement-artifact gate (findings ledger: delivery builds keep the
    # mm gates on; review C2) - a tier-B rule can be promoted into
    # deployment by the ingest protocol, so the same bar applies here
    cand = cand[(cand['mm_n'] / cand['n'] < 0.25)
                & (cand['mm_k'] / cand['k'].clip(lower=1) < 0.25)]
    cand = cand[cand['rule'].apply(
        lambda t: gates_pass(conds_of(t), med_zone))].copy()
    print(f'after list-dedup, support floor, mm gate, gates: {len(cand)}')

    # expected internal flags on the state frame (hh stratum + conditions)
    hh_col = frame['hh_group'].astype(str).values
    flags = []
    for _, r in cand.iterrows():
        m = (hh_col == str(r['hh'])) & cond_mask(frame, conds_of(r['rule']))
        flags.append(int(m.sum()))
    cand['n_flagged_state'] = flags
    cand = cand[cand['n_flagged_state'] >= MIN_EXPECTED].copy()
    print(f'after expected-flags floor (>= {MIN_EXPECTED}): {len(cand)}')

    # characterization axes
    cand['primary_nature'] = cand[NATURE_COLS].astype(float).idxmax(axis=1)
    cand['fcr_ter'] = pd.qcut(cand['found_in_case_record'].astype(float),
                              3, labels=False, duplicates='drop')
    cand['cau_ter'] = pd.qcut(cand['cause_agency'].astype(float),
                              3, labels=False, duplicates='drop')
    cand = cand.sort_values('lcb', ascending=False)

    picked = []
    picked_keys = set()

    def take(row):
        key = (row['hh'], row['rule'])
        if key in picked_keys:
            return False
        picked.append(row)
        picked_keys.add(key)
        return True

    # pass 1: nature coverage
    for nat in NATURE_COLS:
        got = 0
        for _, r in cand[cand['primary_nature'] == nat].iterrows():
            if got >= NATURE_MIN or len(picked) >= n_b:
                break
            got += take(r)

    # pass 2: round-robin over fcr x cause cells by descending LCB
    cells = [(f, c) for f in sorted(cand['fcr_ter'].dropna().unique())
             for c in sorted(cand['cau_ter'].dropna().unique())]
    iters = {cell: cand[(cand['fcr_ter'] == cell[0])
                        & (cand['cau_ter'] == cell[1])].iterrows()
             for cell in cells}
    exhausted = set()
    while len(picked) < n_b and len(exhausted) < len(cells):
        for cell in cells:
            if len(picked) >= n_b or cell in exhausted:
                continue
            for _, r in iters[cell]:
                if take(r):
                    break
            else:
                exhausted.add(cell)

    print(f'tier B picked: {len(picked)}')
    nat_counts = pd.Series([r['primary_nature'] for r in picked]).value_counts()
    print('primary-nature coverage:')
    print(nat_counts.to_string())

    # assemble the delivery-schema csv: tier A rows verbatim + tier B rows
    b_rows = []
    for i, r in enumerate(picked):
        k = float(r['k']); n = float(r['n'])
        b_rows.append({
            'rank': TIER_B_RANK0 + i, 'role': 'measure', 'rule': r['rule'],
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
    parts = [tier_a[DELIVERY_COLS]]
    if b_rows:
        parts.append(pd.DataFrame(b_rows)[DELIVERY_COLS])
    out = pd.concat(parts, ignore_index=True)
    out_path = os.path.join(PKG, '.build', f'measurement_rules_{st}.csv')
    out.to_csv(out_path, index=False)
    print(f'\nwrote {out_path}: {len(tier_a)} tier-A + {len(b_rows)} tier-B '
          f'rows (workbook total ~{len(eff) + a_kept + len(b_rows)})')


if __name__ == '__main__':
    main()
