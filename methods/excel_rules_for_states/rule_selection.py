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
  2a. DROP rules whose narrowest two-sided dollar interval has relative
     width under 5% (2026-08-21): the fragile tail of findings section 40
     (excess held-out decay, 8-36% reach collapse on two eras); the
     49-state re-walk shipped the floor as removal-invariant hygiene
     (median paired change +0.0000, zero states harmed).
  2b. DROP rules whose medical_deductions threshold falls in the state's SMD
     dead zone [min FY2022-24 SMD, max SMD in the table] (2026-08-21): a
     threshold between the training-era standard medical deduction and the
     current one silently changes meaning when the SMD level moves. Zone
     from additional_data/standard_medical_deductions.csv; appending new
     year rows widens it automatically (the table currently ends at 2024).
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

# Narrow-interval width floor (shipped 2026-08-21 after clearing its
# pre-registered bar: methods/width_floor_rewalk/, median paired change
# +0.0000 at both budgets for both metrics, zero of 49 states harmed,
# unfiltered arm reproduced the committed benchmark 98/98 cells). Rules
# whose narrowest two-sided DOLLAR interval has relative width below the
# floor are the fragile tail measured in findings section 40 (excess
# held-out decay at matched support; 8-36% reach collapse on two eras);
# buffer refill replaces them at unchanged capacity. Ratio-boundary
# intervals (e.g. unc_rawben_rel_max) are exempt by construction: the
# width test covers dollar-denominated variables only.
WIDTH_FLOOR = 0.05
DOLLAR_VARS = {'medical_deductions', 'utilities', 'earned_by_hh_size',
               'unearned_by_hh_size', 'gross_by_hh_size',
               'shelter_expenses_by_hh_size', 'total_deductions_by_hh_size'}


def narrow_interval(conds, floor=WIDTH_FLOOR):
    """True when the narrowest two-sided dollar interval in `conds` has
    relative width (hi - lo) / hi below the floor (section-40 definition:
    binding bounds are max(lo) and min(hi))."""
    for v in DOLLAR_VARS:
        lo = [c['thr'] for c in conds if c['var'] == v and c['op'] in ('>', '>=')]
        hi = [c['thr'] for c in conds if c['var'] == v and c['op'] in ('<', '<=')]
        if lo and hi and min(hi) > 0 and min(hi) > max(lo):
            if (min(hi) - max(lo)) / min(hi) < floor:
                return True
    return False


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


def smd_dead_zone(state_name, repo):
    """[lo, hi] for the state's standard-medical-deduction dead zone, or None.

    A medical_deductions threshold between the training-era SMD and the
    current SMD silently changes meaning when the SMD level moves (2026-08-21
    decision; e.g. Vermont: a threshold that separated SMD cases at 116 from
    higher actuals includes every SMD case once the SMD is 156). lo = the
    minimum of the state's FY2022-2024 SMD values (the training era); hi =
    the maximum over ALL year columns present in
    additional_data/standard_medical_deductions.csv, so appending 2025/2026
    rows widens the zone with no code change. NB the table currently ends at
    2024, so the zone is the training-era range until those rows land."""
    import os
    path = os.path.join(repo, 'additional_data',
                        'standard_medical_deductions.csv')
    if not (state_name and os.path.isfile(path)):
        return None
    smd = pd.read_csv(path)
    row = smd[smd['state_name'] == state_name]
    if not len(row):
        return None
    train_cols = [c for c in ('2022', '2023', '2024') if c in smd.columns]
    all_cols = [c for c in smd.columns if c.strip().isdigit()]
    train = pd.to_numeric(row.iloc[0][train_cols], errors='coerce').dropna()
    allv = pd.to_numeric(row.iloc[0][all_cols], errors='coerce').dropna()
    if not len(train) or not len(allv):
        return None
    lo, hi = float(train.min()), float(allv.max())
    # no zone when the state has no SMD program (value 0: a "> 0" threshold
    # means "any medical deduction" and is not SMD-dependent), or when the
    # SMD never moved (zero-width zone: nothing can drift)
    if lo <= 0 or hi <= lo:
        return None
    return lo, hi


def _transform(rules, df, log, med_zone=None):
    """Steps 1-2 on one role's rules: div-100 drop + bbce strip/drop, plus
    the SMD dead-zone drop when a zone is known for this state."""
    out, n_div, n_strip, n_never = [], 0, 0, 0
    n_smd = n_narrow = 0
    for r in rules:
        if any(c['var'] in DROP_VARS for c in r['conds']):
            n_div += 1
            continue
        if narrow_interval(r['conds']):
            n_narrow += 1
            continue
        if med_zone and any(
                c['var'] == 'medical_deductions'
                and med_zone[0] <= c['thr'] <= med_zone[1]
                for c in r['conds']):
            n_smd += 1
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
    return out, n_div, n_strip, n_never, n_smd, n_narrow


def effective_rules(csv_path, df, char_keys, out_csv=None, log=print,
                    state_name=None, repo=None):
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

    med_zone = smd_dead_zone(state_name, repo) if repo else None
    if med_zone:
        log(f'SMD dead zone for {state_name}: '
            f'[{med_zone[0]:g}, {med_zone[1]:g}]')

    kept, n_div_c, n_strip_c, n_never_c, n_smd_c, n_nar_c = _transform(
        core, df, log, med_zone)
    promotable, n_div_b, n_strip_b, n_never_b, n_smd_b, n_nar_b = _transform(
        buffer, df, log, med_zone)
    log(f'core: {len(core)} -> {len(kept)} '
        f'(dropped {n_div_c} count_divisible_by_100, {n_never_c} never-firing, '
        f'{n_smd_c} in the SMD dead zone, {n_nar_c} narrow dollar intervals '
        f'(< {WIDTH_FLOOR:.0%} rel width); stripped {STRIP_VAR} from {n_strip_c})')

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
        repo = os.path.dirname(os.path.dirname(PKG))
        rules = effective_rules(csv, df, char_keys=[],
                                state_name=cfg['name'], repo=repo)
        assert all(STRIP_VAR not in rule_text(r['conds'])
                   or not cond_mask(df, [c for c in r['conds']
                                         if c['var'] == STRIP_VAR]).all()
                   for r in rules)
        print(f'{abbr}: {len(rules)} effective rules; '
              f'top dollars {rules[0]["dollars_frame"]:.0f}')
