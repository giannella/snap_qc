"""What would relaxing the support floor to 20 or 15 actually do?

Runs the tiered tuning at several min_support values on the frame the builder
already prepared, holding everything else fixed, and reports what changes: how
many rules clear the floor, how many BH admits, what the tuned list does on the
held-out year, and how the smallest admitted rule's precision is bounded.

    python support_floor_sweep.py WA
"""
import os
import sys

import pandas as pd

import states as STATE_REGISTRY
import tuning
from compare_splits import load_cases, load_rules

FLOORS = [5, 10, 15, 20, 30]


def main():
    args = [a for a in sys.argv[1:] if not a.startswith('-')]
    abbr = (args[0] if args else 'WA').upper()
    cfg = STATE_REGISTRY.get(abbr)
    base = dict(getattr(STATE_REGISTRY, 'TUNING', {}))
    df = pd.read_csv(load_cases(abbr))
    rules = load_rules(cfg)

    rows = []
    for f in FLOORS:
        tc = tuning.TuningConfig(**{**base, 'min_support': f,
                                    'min_admitted_for_tier2': 1})
        res = tuning.run(df, rules, tc, log=lambda *_: None)
        t = res.rules_table
        arm = res.arms[res.arms.tier == res.tier].iloc[0]
        top = res.arms[res.arms.tier == res.arms.tier.max()].iloc[0]
        adm = t[t.admitted]
        rows.append({
            'min_support': f,
            'clear_floor': int(t.support_ok.sum()),
            'admitted': int(len(adm)),
            'smallest_admitted_n': int(adm.tune_n.min()) if len(adm) else 0,
            'median_admitted_n': int(adm.tune_n.median()) if len(adm) else 0,
            'median_admitted_prec': round(float(adm.tune_prec.median()), 3) if len(adm) else 0,
            'median_admitted_lcb99': round(float(adm.lcb99.median()), 3) if len(adm) else 0,
            'thresholds_moved': int(t.threshold_moved.sum()),
            'variants': res.comparisons['tier2_variants'],
            'tier_deployed': res.tier,
            'top_arm_tier': int(top.tier),
            'top_arm_rules': int(top.n_rules),
            'top_arm_tune_prec': round(float(top.tune_prec), 3),
            'top_arm_hold_flag': int(top.hold_flagged),
            'top_arm_hold_prec': round(float(top.hold_prec), 3),
            'top_arm_hold_lcb': round(float(top.hold_prec_lcb), 3),
            'top_arm_hold_rec': round(float(top.hold_rec), 3),
            'tier0_hold_prec': round(float(res.arms[res.arms.tier == 0].iloc[0].hold_prec), 3),
        })
    out = pd.DataFrame(rows)
    pd.set_option('display.width', 250, 'display.max_columns', 50)
    print(f'\n{cfg["name"]}, {len(rules)} rules, budget {base.get("budget", 0.05):.0%}, '
          f'everything but min_support held fixed\n')
    print(out.to_string(index=False))
    print('\ntop_arm_* = the highest tier that was BUILT at that floor, whether or not')
    print('it was deployed. tier_deployed = what the holdout gate actually accepted.')
    print('\nWilson 99% lower bound on precision at a few (k, n), for scale:')
    for n in (15, 20, 30, 50):
        for p in (0.3, 0.5):
            k = round(p * n)
            print(f'  n={n:3d} k={k:3d} raw {k/n:.2f} -> 99% bound '
                  f'{tuning.wilson_lcb(k, n, 2.326):.3f}')


if __name__ == '__main__':
    main()
