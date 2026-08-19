"""Build-time transform of a state's delivery list into the workbook's
effective rule list (decision 2026-08-18). The tracked delivery CSVs in
state_delivery_lists/ are untouched; this reshapes what the WORKBOOK ships:

  1. DROP every rule that conditions on count_divisible_by_100. It is the one
     input column a state cannot derive from the paste-in contract's totals
     (it counts round-$100 amounts across 28 underlying QC fields), so
     carrying such rules forces a custom precompute on every state. 563 of
     14,538 blended rows (3.9%) used it as of 2026-08-18.
  2. STRIP bbce_state_i conjuncts that are trivially true for this state's
     own rows: a state's list already applies only to that state, so "rule
     applies to BBCE states" carries no information there. A rule whose
     bbce_state_i conjunct is satisfied by NO row of the state frame can
     never fire and is dropped; a mixed (state-year flip) case keeps the
     conjunct and warns (zero flips observed FY2022-24, findings ledger).
  3. REFILL the review budget: the drops above free capacity, so buffer
     rules are promoted in delivery-rank order (transformed the same way)
     while the union workload on the state frame stays at or below the
     ORIGINAL core list's union workload. Stop before overshoot; the walk
     never looks at outcomes, matching the delivery builder's fill.
  4. SORT the final list descending by error dollars caught on the state
     frame at delivered thresholds (the Step 3 tab's static order).

Rule ids ("num") stay the delivery-CSV rank, so an id refers to the same
rule across workbook releases regardless of the sort. The result is also
written to .build/effective_rules_<ABBR>.csv in the delivery-CSV schema
(with rule text regenerated after stripping) — crosscheck_rules consumes
that file, not the raw delivery CSV.
"""
import re

import numpy as np
import pandas as pd

COND_PAT = re.compile(r'([A-Za-z_][A-Za-z0-9_]*)\s*(>=|<=|>|<|==)\s*(-?[0-9.]+)')
NSLOTS = 4                        # condition slots in the workbook's engines
DROP_VARS = {'count_divisible_by_100'}
STRIP_VAR = 'bbce_state_i'


def cond_mask(df, conds):
    """Row mask for a condition list; NaN never satisfies a condition."""
    m = np.ones(len(df), bool)
    for c in conds:
        assert c['op'] in ('>=', '>', '<=', '<'), c
        xv = df[c['var']].values.astype(float)
        if   c['op'] == '>=': cm = xv >= c['thr']
        elif c['op'] == '>':  cm = xv > c['thr']
        elif c['op'] == '<=': cm = xv <= c['thr']
        else:                 cm = xv < c['thr']
        m &= np.where(np.isnan(xv), False, cm)
    return m


def rule_mask(df, rule):
    return (df['hh_group'] == rule['hh']).values & cond_mask(df, rule['conds'])


def rule_text(conds):
    return ' & '.join(f"{c['var']} {c['op']} {c['thr']:g}" for c in conds)


def _parse_rows(csv_path, char_keys):
    """Every row of the delivery CSV (all roles), rank order, conds parsed."""
    rdf = pd.read_csv(csv_path).sort_values('rank').reset_index(drop=True)
    out = []
    for _, rr in rdf.iterrows():
        conds = [{'var': v, 'op': op, 'thr': float(t)}
                 for v, op, t in COND_PAT.findall(rr['rule'])]
        assert 1 <= len(conds) <= NSLOTS, rr['rule']
        out.append({'num': int(rr['rank']), 'hh': str(rr['hh']), 'conds': conds,
                    'role': str(rr['role']),
                    'prec_train': float(rr['precision_train']),
                    'prec_lcb': float(rr['precision_train_lcb']),
                    'engine': rr['engines'], 'frame': rr['mined_frames'],
                    'char': {k: rr.get(k) for k in char_keys},
                    '_row': rr})
    return out


def _transform(rules, df, log):
    """Steps 1-2 on one role's rules: div-100 drop + bbce strip/drop."""
    out, n_div, n_strip, n_never = [], 0, 0, 0
    for r in rules:
        if any(c['var'] in DROP_VARS for c in r['conds']):
            n_div += 1
            continue
        bb = [c for c in r['conds'] if c['var'] == STRIP_VAR]
        if bb:
            sat = cond_mask(df, bb)
            other = [c for c in r['conds'] if c['var'] != STRIP_VAR]
            if sat.all():
                if not other:
                    # a bare "applies to this state" rule would flag everything
                    n_never += 1
                    continue
                r = dict(r, conds=other)
                n_strip += 1
            elif not sat.any():
                n_never += 1
                continue
            else:
                log(f'  warn: rule {r["num"]} keeps its {STRIP_VAR} conjunct '
                    f'(mixed within the state frame: {int(sat.sum())}/{len(sat)} rows)')
        out.append(r)
    return out, n_div, n_strip, n_never


def effective_rules(csv_path, df, char_keys, out_csv=None, log=print):
    """The workbook's rule list: transformed core + promoted buffer, sorted by
    error dollars caught on the state frame. See the module docstring."""
    rows = _parse_rows(csv_path, char_keys)
    core   = [r for r in rows if r['role'] == 'core']
    buffer = [r for r in rows if r['role'] != 'core']

    # capacity target: what the delivered core list flags on this frame
    orig_union = np.zeros(len(df), bool)
    for r in core:
        orig_union |= rule_mask(df, r)
    target = int(orig_union.sum())

    kept, n_div_c, n_strip_c, n_never_c = _transform(core, df, log)
    promotable, n_div_b, n_strip_b, n_never_b = _transform(buffer, df, log)
    log(f'core: {len(core)} -> {len(kept)} '
        f'(dropped {n_div_c} count_divisible_by_100, {n_never_c} never-firing; '
        f'stripped {STRIP_VAR} from {n_strip_c})')

    union = np.zeros(len(df), bool)
    for r in kept:
        union |= rule_mask(df, r)
    promoted = 0
    for r in promotable:
        cand = union | rule_mask(df, r)
        if int(cand.sum()) > target:
            break                          # stop before overshoot
        union = cand
        kept.append(dict(r, promoted=True))
        promoted += 1
    log(f'buffer promoted: {promoted} of {len(promotable)} '
        f'(union {int(union.sum())} of target {target} flagged rows, '
        f'{int(union.sum())/max(len(df),1):.1%} of the frame)')

    # dollars caught on the frame at delivered thresholds -> Step 3 sort order
    is_err = (pd.to_numeric(df['over_threshold'], errors='coerce')
              .fillna(0).values == 1) if 'over_threshold' in df else \
             (pd.to_numeric(df['is_error'], errors='coerce').fillna(0).values == 1)
    ed = np.where(is_err,
                  pd.to_numeric(df['total_error_amount'], errors='coerce')
                  .fillna(0).abs().values, 0.0)
    for r in kept:
        r['dollars_frame'] = float(ed[rule_mask(df, r)].sum())
    kept.sort(key=lambda r: -r['dollars_frame'])

    for r in kept:
        assert not any(c['var'] in DROP_VARS for c in r['conds'])

    if out_csv:
        recs = []
        for r in kept:
            rec = r['_row'].copy()
            rec['rule'] = rule_text(r['conds'])
            recs.append(rec)
        pd.DataFrame(recs).to_csv(out_csv, index=False)
        log(f'effective list written: {out_csv} ({len(recs)} rules)')
    for r in kept:
        r.pop('_row', None)
    return kept


if __name__ == '__main__':
    # self-check: run the transform for a couple of states against their
    # cached frames (no workbook build). Usage: python rule_selection.py WA VA
    import os
    import sys
    import states as STATE_REGISTRY
    PKG = os.path.dirname(os.path.abspath(__file__))
    for abbr in (sys.argv[1:] or ['WA']):
        cfg = STATE_REGISTRY.get(abbr)
        frame = os.path.join(PKG, '.frames', f'{abbr.lower()}_frame.csv')
        if not os.path.isfile(frame):
            print(f'{abbr}: no cached frame at {frame}; build it first')
            continue
        df = pd.read_csv(frame)
        df['over_threshold'] = pd.to_numeric(df['is_error'], errors='coerce') \
            .fillna(0).astype(int)
        csv = os.path.join(os.path.dirname(PKG), '..', cfg['delivery_csv'])
        if not os.path.isfile(csv):
            csv = os.path.join(os.path.dirname(os.path.dirname(PKG)),
                               'state_delivery_lists',
                               os.path.basename(cfg['delivery_csv']))
        print(f'== {abbr} ==')
        rules = effective_rules(csv, df, char_keys=[])
        assert all(STRIP_VAR not in rule_text(r['conds'])
                   or not cond_mask(df, [c for c in r['conds']
                                         if c['var'] == STRIP_VAR]).all()
                   for r in rules)
        print(f'{abbr}: {len(rules)} effective rules; '
              f'top dollars {rules[0]["dollars_frame"]:.0f}')
