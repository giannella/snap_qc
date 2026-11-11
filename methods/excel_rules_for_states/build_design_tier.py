"""Design-based diagnostic tier: choose rules so a state's OWN share-back can
estimate each rule characteristic's transfer effect (component-level learning),
not only the frozen composite score (uncommitted; state-specific work).

    python build_design_tier.py WA --total 350 --force 100 --min-expected 25 --cell-min 4 \
        --base-csv .build/measurement_rules_WA_tierA.csv --exclude-csv .build/measurement_rules_WA.csv

Forced-in: the top --force candidates by the frozen CT score (keeps the frozen
block's identity). Free slots: Fedorov exchange for D-optimality of the
weighted information matrix of the transfer model, weights = expected internal
flags x p(1-p) with p = national LCB, columns = intercept + the six score
components + found_in_case_record + cause_agency + timing_at_certification +
share_overissuance + six element shares + hh dummies + logit(LCB). A minimum
count per content cell (stale x earned x shelter x benmax) x hh is enforced
where supply allows. Reports the design's predictor spread, component
correlations and the expected SE / minimum detectable effect per coefficient
at reference paste sizes, against the current book.
"""
import argparse
import json
import os
import sys

import numpy as np
import pandas as pd

PKG = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.dirname(os.path.dirname(PKG))
sys.path.insert(0, PKG)
import states as STATE_REGISTRY                       # noqa: E402
from build_measurement_tier import (DELIVERY_COLS, MIN_POOL_N, POOL_CSV,     # noqa: E402
                                    CHAR_CSV, conds_of, gates_pass)
from build_scored_tier import content_flags, frozen_score, FROZEN, RANK0    # noqa: E402
from rule_selection import cond_mask, smd_dead_zone   # noqa: E402

COMP = ['nat_change_in_circumstances', 'timing_after_certification',
        'uses_stale_cert', 'uses_earned', 'uses_shelter', 'uses_benmax']
SHARES = ['found_in_case_record', 'cause_agency', 'timing_at_certification',
          'share_overissuance']
ELEM = ['elem_earned_income', 'elem_unearned_income', 'elem_shelter_deduction',
        'elem_utility_allowance', 'elem_medical_deduction',
        'elem_dep_care_or_child_support_deduction']
# timing_at_certification is (almost) the complement of timing_after; it stays
# a reported share but is not a design column (it doubled both SEs)
EXTRA = ['uses_unearned']      # Eric 2026-09-16: a category for unearned-income rules
PRED = COMP + EXTRA + [c for c in SHARES if c != 'timing_at_certification'] + ELEM + ['logit_lcb']
# dollar-driver columns (2026-09-17, from the MI dollar design): what might drive a rule's error $ per flagged case
DOLLAR = ['log_nat_sev', 'ben_index', 'min_gross', 'n_conditions', 'log_n_train']
import re as _re
_COND = _re.compile(r'([A-Za-z_][A-Za-z0-9_]*)\s*(>=|<=|>|<|=)\s*(-?[0-9.]+)')


def add_dollar_columns(df, masks, ben, hh_col):
    """log national $ per error, benefit index of the rule's flagged public cases (mean issued benefit / size-group mean),
    floor on gross income, number of conditions, log national support. `masks` is the rule x case bool matrix."""
    grp = {h: ben[hh_col == h].mean() for h in ['1', '2-3', '4+']}
    df['log_nat_sev'] = np.log(df['doll'].astype(float) / df['k'].astype(float).clip(lower=1))
    df['ben_index'] = [ben[m > 0].mean() / grp[str(h)] if (m > 0).any() else np.nan for m, h in zip(masks, df['hh'])]
    cs = [_COND.findall(t) for t in df['rule']]
    df['min_gross'] = [int(any(v == 'gross_by_hh_size' and op in ('>', '>=') for v, op, _ in c)) for c in cs]
    df['n_conditions'] = [len(c) for c in cs]
    df['log_n_train'] = np.log(df['n'].astype(float))
    return df
ONE_SIDED_80 = 1.645 + 0.842


def design_matrix(df, scale):
    """Columns: intercept, predictors standardized by the candidate-pool SD,
    hh dummies."""
    cols = [np.ones(len(df))]
    names = ['intercept']
    for p in scale['_pred']:
        cols.append((df[p].astype(float).values - scale[p][0]) / scale[p][1])
        names.append(p)
    for h in ['2-3', '4+']:
        cols.append((df['hh'].astype(str).values == h).astype(float))
        names.append('hh' + h)
    return np.column_stack(cols), names


def logdet(M):
    s, ld = np.linalg.slogdet(M)
    return ld if s > 0 else -np.inf


def se_table(X, names, w_ref, phi=2.0):
    M = X.T @ (X * w_ref[:, None])
    V = np.linalg.pinv(M) * phi
    return pd.Series(np.sqrt(np.diag(V)), index=names)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('state')
    ap.add_argument('--total', type=int, default=350)
    ap.add_argument('--force', type=int, default=100)
    ap.add_argument('--min-expected', type=int, default=25)
    ap.add_argument('--cell-min', type=int, default=4)
    ap.add_argument('--base-csv', required=True)
    ap.add_argument('--exclude-csv', action='append', default=[])
    ap.add_argument('--paste-sizes', default='2000,5000')
    ap.add_argument('--passes', type=int, default=30)
    ap.add_argument('--seed', type=int, default=20260916)
    ap.add_argument('--tag', default='')
    ap.add_argument('--drop-vars', default='',
                    help='comma list of rule variables to exclude from the candidates (e.g. percent_abawd)')
    ap.add_argument('--criterion', default='D', choices=['D', 'A'],
                    help="D: log-det of the information matrix (all coefficients equal). "
                         "A: weighted sum of RELATIVE coefficient variances (each variance divided by "
                         "the D design's), started from the D design; weights from --a-weights")
    ap.add_argument('--dollar-drivers', action='store_true',
                    help='add the dollar-driver columns (DOLLAR) to the design model so the block also measures what drives error $ per flagged case')
    ap.add_argument('--max-jaccard', type=float, default=0.0,
                    help='overlap ceiling: no two design rules may share more than this share of their flagged public cases '
                         '(Jaccard); 0 = off. Added 2026-09-17 after the MI block filled with near-copies of a few broad rules')
    ap.add_argument('--a-weights', default='cause_agency=5,uses_shelter=0.2,elem_shelter_deduction=0.2,elem_utility_allowance=0.2',
                    help='name=weight list; unnamed coefficients weigh 1')
    a = ap.parse_args()
    st = a.state.upper()
    cfg = STATE_REGISTRY.get(st)
    consts = json.load(open(FROZEN))
    rng = np.random.default_rng(a.seed)

    frame = pd.read_csv(os.path.join(PKG, '.frames', st.lower() + '_frame.csv'))
    n_frame = len(frame)
    delivery = pd.read_csv(os.path.join(REPO, cfg['delivery_csv']))
    base = pd.read_csv(a.base_csv)
    assert base['rank'].max() < RANK0
    pool = pd.read_csv(POOL_CSV)
    char = pd.read_csv(CHAR_CSV, low_memory=False)
    cand = pool.merge(char, on=['hh', 'rule'], how='inner')
    seen = set(zip(base['hh'], base['rule'])) | set(zip(delivery['hh'], delivery['rule']))
    for x in a.exclude_csv:
        e = pd.read_csv(x)
        seen |= set(zip(e['hh'], e['rule']))
    cand = cand[~cand.apply(lambda r: (r['hh'], r['rule']) in seen, axis=1)]
    cand = cand[cand['n'] >= MIN_POOL_N]
    cand = cand[(cand['mm_n'] / cand['n'] < 0.25)
                & (cand['mm_k'] / cand['k'].clip(lower=1) < 0.25)]
    med_zone = smd_dead_zone(cfg['name'], REPO)
    cand = cand[cand['rule'].apply(lambda t: gates_pass(conds_of(t), med_zone))].copy()
    print('candidates after exclusions, support floor, mm gate, gates: %d' % len(cand))
    hh_col = frame['hh_group'].astype(str).values
    cand['n_flagged_state'] = [
        int(((hh_col == str(r['hh'])) & cond_mask(frame, conds_of(r['rule']))).sum())
        for _, r in cand.iterrows()]
    cand = cand[cand['n_flagged_state'] >= a.min_expected].copy().reset_index(drop=True)
    print('after expected-flags floor (>= %d): %d' % (a.min_expected, len(cand)))
    for k, v in pd.DataFrame([content_flags(t) for t in cand['rule']]).items():
        cand[k] = v.values
    cand['uses_unearned'] = cand['rule'].str.contains(r'\bunearned_by_hh_size\b').astype(int)
    drop_vars = {v.strip() for v in a.drop_vars.split(',') if v.strip()}
    if drop_vars:
        pat = '|'.join(r'\b' + v + r'\b' for v in sorted(drop_vars))
        before = len(cand)
        cand = cand[~cand['rule'].str.contains(pat)].copy().reset_index(drop=True)
        print('dropped %d candidates using %s -> %d' % (before - len(cand), sorted(drop_vars), len(cand)))
    cand['score'] = frozen_score(cand, consts)
    # flagged-case masks on the public frame, for the overlap ceiling
    Mc = np.array([(hh_col == str(r['hh'])) & cond_mask(frame, conds_of(r['rule'])) for _, r in cand.iterrows()], dtype=np.float32)
    nfl = Mc.sum(1)
    pred = list(PRED) + (DOLLAR if a.dollar_drivers else [])
    if a.dollar_drivers:
        cand = add_dollar_columns(cand, Mc, frame['rawben'].values.astype(float), hh_col)
        keep = cand[DOLLAR].notna().all(axis=1).values
        if not keep.all():
            cand = cand[keep].reset_index(drop=True); Mc = Mc[keep]; nfl = nfl[keep]
    def jac_to(idx_list, j):
        if a.max_jaccard <= 0 or not idx_list:
            return 0.0
        inter = Mc[idx_list] @ Mc[j]
        return float((inter / np.maximum(nfl[idx_list] + nfl[j] - inter, 1)).max())
    cand['logit_lcb'] = np.log(cand['lcb'] / (1 - cand['lcb']))
    cand['cell'] = (cand['uses_stale_cert'].astype(str) + cand['uses_earned'].astype(str)
                    + cand['uses_shelter'].astype(str) + cand['uses_benmax'].astype(str)
                    + cand['uses_unearned'].astype(str) + '|' + cand['hh'].astype(str))
    scale = {p: (float(cand[p].mean()), float(cand[p].std()) or 1.0) for p in pred}; scale['_pred'] = pred
    X, names = design_matrix(cand, scale)
    # information weights at the public-frame scale (paste size only rescales)
    p = cand['lcb'].values
    w = cand['n_flagged_state'].values * p * (1 - p)

    # forced-in block: top by frozen score
    order = np.argsort(-cand['score'].values)
    forced = []
    for i in order:
        if len(forced) >= a.force:
            break
        if jac_to(forced, int(i)) < a.max_jaccard or a.max_jaccard <= 0:
            forced.append(int(i))
    m_free = a.total - a.force
    sel = set(forced)
    cells = cand['cell'].values
    supply = pd.Series(cells).value_counts()
    counts = pd.Series(cells[forced]).value_counts().to_dict()
    cell_min = a.cell_min
    while cell_min > 0:
        cmin = {c: min(cell_min, int(supply[c])) for c in supply.index}
        need_total = sum(max(0, cmin[c] - counts.get(c, 0)) for c in cmin)
        if need_total <= m_free:
            break
        cell_min -= 1
    if cell_min != a.cell_min:
        print('cell minimum lowered from %d to %d to fit %d free slots (%d cells with supply)'
              % (a.cell_min, cell_min, m_free, len(cmin)))
    # initial free set: fill cell minimums by descending weight, then random
    free = []
    by_w = np.argsort(-w)
    for c in sorted(cmin, key=lambda c: -cmin[c]):
        need = cmin[c] - counts.get(c, 0)
        if need <= 0:
            continue
        for i in [int(i) for i in by_w if cells[i] == c and int(i) not in sel]:
            if need <= 0:
                break
            if a.max_jaccard > 0 and jac_to(forced + free, i) >= a.max_jaccard:
                continue
            free.append(i)
            sel.add(i)
            counts[c] = counts.get(c, 0) + 1
            need -= 1
    if len(free) > m_free:
        raise SystemExit('cell minimums need %d free slots > %d; lower --cell-min'
                         % (len(free), m_free))
    pool_order = [int(i) for i in by_w if int(i) not in sel]
    rng.shuffle(pool_order)
    for i in pool_order:
        if len(free) >= m_free:
            break
        if a.max_jaccard > 0 and jac_to(forced + free, i) >= a.max_jaccard:
            continue
        free.append(i)
        sel.add(i)
        counts[cells[i]] = counts.get(cells[i], 0) + 1
    I_sel = Mc @ Mc[forced + free].T if a.max_jaccard > 0 else None      # intersections with the chosen set, by column
    def overlap_ok(pos_col):
        """candidates whose overlap with every chosen rule OTHER than the one in column pos_col stays under the ceiling"""
        if I_sel is None:
            return np.ones(len(cand), dtype=bool)
        Ij = I_sel.copy(); Ij[:, pos_col] = 0
        nS = nfl[forced + free]
        Jm = Ij / np.maximum(nfl[:, None] + nS[None, :] - Ij, 1); Jm[:, pos_col] = 0
        return Jm.max(1) < a.max_jaccard

    def info(idx):
        Xi = X[idx]
        return Xi.T @ (Xi * w[idx, None])

    M = info(forced + free)
    print('initial design: %d forced + %d free | log det %.2f' % (len(forced), len(free), logdet(M)))
    all_idx = np.arange(len(cand))
    for it in range(a.passes):
        improved = 0
        Minv = np.linalg.inv(M)
        for pos in range(len(free)):
            i = free[pos]
            if counts[cells[i]] - 1 < cmin[cells[i]]:
                continue
            xi = X[i]
            di = w[i] * xi @ Minv @ xi
            mask = np.ones(len(cand), dtype=bool)
            mask[list(sel)] = False
            mask &= overlap_ok(len(forced) + pos)
            not_sel = all_idx[mask]
            if not len(not_sel):
                continue
            Xj = X[not_sel]
            MXj = Xj @ Minv
            dj = w[not_sel] * np.einsum('ij,ij->i', MXj, Xj)
            dij = np.sqrt(w[i] * w[not_sel]) * (MXj @ xi)
            delta = dj - di - (di * dj - dij ** 2)
            jb = int(np.argmax(delta))
            if delta[jb] > 1e-6:
                jbest = int(not_sel[jb])
                sel.remove(i)
                sel.add(jbest)
                free[pos] = jbest
                counts[cells[i]] -= 1
                counts[cells[jbest]] = counts.get(cells[jbest], 0) + 1
                if I_sel is not None:
                    I_sel[:, len(forced) + pos] = Mc @ Mc[jbest]
                M = info(forced + free)
                Minv = np.linalg.inv(M)
                improved += 1
        print('  pass %d: %d exchanges | log det %.2f' % (it + 1, improved, logdet(M)))
        if improved == 0:
            break
    d_idx = forced + list(free)
    Vd = np.diag(np.linalg.inv(info(d_idx)))            # D design's coefficient variances (reference)
    if a.criterion == 'A':
        wts = {kv.split('=')[0]: float(kv.split('=')[1]) for kv in a.a_weights.split(',') if kv}
        omega = np.array([wts.get(nm, 1.0) for nm in names]) / Vd
        Om = np.diag(omega)
        crit = lambda Minv: float(np.trace(Om @ Minv))
        M = info(forced + free)
        Minv = np.linalg.inv(M)
        print('A-criterion exchange (weights: %s) | start %.3f' % (a.a_weights, crit(Minv)))
        for it in range(a.passes):
            improved = 0
            for pos in range(len(free)):
                i = free[pos]
                if counts[cells[i]] - 1 < cmin[cells[i]]:
                    continue
                xi = X[i]
                Mx = Minv @ xi
                denom = 1.0 - w[i] * (xi @ Mx)
                if denom <= 1e-9:
                    continue
                Mi_inv = Minv + w[i] * np.outer(Mx, Mx) / denom      # M without rule i
                base_crit = crit(Mi_inv)
                mask = np.ones(len(cand), dtype=bool)
                mask[list(sel)] = False
                mask &= overlap_ok(len(forced) + pos)
                not_sel = all_idx[mask]
                if not len(not_sel):
                    continue
                Xj = X[not_sel]
                t = np.einsum('ij,jk,ik->i', Xj, Mi_inv, Xj)
                Q = Mi_inv @ Om @ Mi_inv
                u = np.einsum('ij,jk,ik->i', Xj, Q, Xj)
                new = base_crit - w[not_sel] * u / (1.0 + w[not_sel] * t)
                jb = int(np.argmin(new))
                if new[jb] < crit(Minv) - 1e-9:
                    jbest = int(not_sel[jb])
                    sel.remove(i)
                    sel.add(jbest)
                    free[pos] = jbest
                    counts[cells[i]] -= 1
                    counts[cells[jbest]] = counts.get(cells[jbest], 0) + 1
                    if I_sel is not None:
                        I_sel[:, len(forced) + pos] = Mc @ Mc[jbest]
                    M = info(forced + free)
                    Minv = np.linalg.inv(M)
                    improved += 1
            print('  A pass %d: %d exchanges | criterion %.3f | log det %.2f' % (it + 1, improved, crit(Minv), logdet(M)))
            if improved == 0:
                break
    chosen = forced + free
    D = cand.iloc[chosen].copy()
    D['picked_via'] = ['forced_top_score'] * len(forced) + ['design'] * len(free)

    # ── report ───────────────────────────────────────────────────────────
    sizes = [int(s) for s in a.paste_sizes.split(',')]
    cellsz = D.groupby(['uses_stale_cert', 'uses_earned', 'uses_shelter', 'uses_benmax', 'uses_unearned']).size()
    print('\nDESIGN %s: %d rules | score sd %.3f (forced block %+.2f..%+.2f) | expected public flags '
          'median %d sum %d | uses unc %d'
          % (st, len(D), D['score'].std(), D['score'].iloc[:len(forced)].min(), D['score'].max(),
             int(D['n_flagged_state'].median()), int(D['n_flagged_state'].sum()),
             int(D['rule'].str.contains('unc_rawben_rel_max').sum())))
    if a.max_jaccard > 0:
        Jd = Mc[chosen] @ Mc[chosen].T; Jd = Jd / np.maximum(nfl[chosen][:, None] + nfl[chosen][None, :] - Jd, 1); np.fill_diagonal(Jd, 0)
        print('overlap ceiling %.2f: max pairwise Jaccard in the block %.2f | rules with a twin >= 0.9: %d' % (a.max_jaccard, Jd.max(), int((Jd.max(1) >= 0.9).sum())))
    print('hh:', D['hh'].value_counts().to_dict(),
          '| content cells filled: %d of 32, min per filled cell %d | unearned rules %d'
          % ((cellsz > 0).sum(), int(cellsz.min()), int(D['uses_unearned'].sum())))
    # comparison: the current book's diagnostic rules
    # comparison book: the state's current effective list's diagnostic rules, if
    # it has any (WA/MI/CT carry a score-ranked block; a plain delivery build does not)
    eff = pd.read_csv(os.path.join(PKG, '.build', 'effective_rules_%s.csv' % st))
    sc_path = os.path.join(PKG, '.build', 'measurement_rules_%s_scored.csv' % st)
    if os.path.isfile(sc_path):
        m = pd.read_csv(sc_path)[['rank', 'hh', 'rule']].rename(columns={'rule': 'rule_orig'})
        eff = eff.merge(m, on=['rank', 'hh'], how='left')
        eff['rule'] = eff['rule_orig'].fillna(eff['rule'])
    if 'ship' not in eff.columns:
        eff['ship'] = True
    cur = eff[eff['ship'] != True].merge(char, on=['hh', 'rule'], how='left', suffixes=('', '_c'))
    if len(cur) < 30 or cur['nat_change_in_circumstances'].isna().mean() > 0.5:
        cur = D.copy()          # no comparable current block: compare the design with itself
        print('(no current diagnostic block to compare against; "current" columns repeat the design)')
    else:
        for c in SHARES:
            if c + '_c' in cur:
                cur[c] = cur[c].fillna(cur[c + '_c'])
        for k, v in pd.DataFrame([content_flags(t) for t in cur['rule']]).items():
            cur[k] = v.values
        cur['uses_unearned'] = cur['rule'].str.contains(r'\bunearned_by_hh_size\b').astype(int)
        cur['lcb'] = cur['precision_train_lcb']
        cur['logit_lcb'] = np.log(cur['lcb'] / (1 - cur['lcb']))
        cur['score'] = frozen_score(cur, consts)
        if a.dollar_drivers:
            cur = cur.rename(columns={'n_flagged_train': 'n'}) if 'n' not in cur else cur
            if 'doll' not in cur:
                cur['doll'] = cur['dollars_per_flag_train'].astype(float) * cur['n'].astype(float)
            if 'k' not in cur:
                cur['k'] = cur['precision_train'].astype(float) * cur['n'].astype(float)
            Mcur = np.array([(hh_col == str(r['hh'])) & cond_mask(frame, conds_of(r['rule'])) for _, r in cur.iterrows()], dtype=np.float32)
            cur = add_dollar_columns(cur, Mcur, frame['rawben'].values.astype(float), hh_col)
        # rules without characterization (hand-pinned or state-mined test rows,
        # 2026-10-07) cannot enter the comparison matrix
        cur = cur.dropna(subset=[p for p in pred if p in cur.columns]).copy()
        if len(cur) < 30:
            cur = D.copy()
            print('(current block has too few characterized rules; "current" columns repeat the design)')
    tab = {}
    for label, dd in [('current', cur), ('design', D)]:
        Xd, nm = design_matrix(dd, scale)
        pd_ = dd['lcb'].values
        base_w = dd['n_flagged_state'].values * pd_ * (1 - pd_)
        for s in sizes:
            tab['%s @%d' % (label, s)] = se_table(Xd, nm, base_w * s / n_frame)
    if a.criterion == 'A':
        for s_ in sizes:
            tab['D design @%d' % s_] = pd.Series(np.sqrt(Vd * 2.0 * n_frame / s_), index=names)
    tab = pd.DataFrame(tab).loc[pred + ['hh2-3', 'hh4+']]
    if a.criterion == 'A':
        tab['A vs D @%d' % sizes[0]] = tab['design @%d' % sizes[0]] / tab['D design @%d' % sizes[0]]
    tab['gain @2000'] = tab.iloc[:, 0] / tab.iloc[:, len(sizes)]
    print('\nexpected SE of each coefficient (logit scale, per candidate-pool SD of the predictor; '
          'dispersion 2; paste size in cases). MDE = %.2f x SE for one-sided 80%% power' % ONE_SIDED_80)
    pd.set_option('display.width', 200)
    print(tab.round(3).to_string())
    print('\ncomponent correlations in the design:')
    print(D[COMP].corr().round(2).to_string())
    print('\nspread vs candidate pool (sd ratio): '
          + ', '.join('%s %.2f' % (p, D[p].std() / (cand[p].std() or 1.0)) for p in COMP + SHARES))

    # ── write the measurement csv (tier A + design block at ranks 1001+) ──
    b_rows = []
    for i, (_, r) in enumerate(D.iterrows()):
        k, n = float(r['k']), float(r['n'])
        b_rows.append({
            'rank': RANK0 + i, 'role': 'measure', 'rule': r['rule'], 'hh': r['hh'],
            'pool': 'national', 'engines': r['engines'], 'mined_frames': r['mined_frames'],
            'n_flagged_train': int(n), 'precision_train': round(k / n, 4),
            'precision_train_lcb': round(float(r['lcb']), 4),
            'dollars_per_flag_train': round(float(r['doll']) / n, 2),
            'mm_share_flags': round(float(r['mm_n']) / n, 4),
            'mm_share_errors': round(float(r['mm_k']) / max(k, 1), 4), 'mm_inflation': '',
            'n_flagged_state': int(r['n_flagged_state']), 'n_new_at_rank': 0,
            'n_error_cases_national': int(r['n_error_cases']),
            'element_groups_to_75': r['element_groups_to_75'],
            'nature_groups_to_75': r['nature_groups_to_75'],
            'found_in_case_record': r['found_in_case_record'],
            'share_overissuance': r['share_overissuance'],
            'timing_at_certification': r['timing_at_certification'],
            'cause_agency': r['cause_agency'],
        })
    out = pd.concat([base[DELIVERY_COLS], pd.DataFrame(b_rows)[DELIVERY_COLS]], ignore_index=True)
    out_path = os.path.join(PKG, '.build', 'measurement_rules_%s_design%s.csv' % (st, a.tag))
    out.to_csv(out_path, index=False)
    rep = D[['hh', 'rule', 'score', 'picked_via', 'lcb', 'n', 'k', 'n_flagged_state'] + COMP + EXTRA + SHARES + ELEM + (DOLLAR if a.dollar_drivers else [])]
    rep_path = os.path.join(PKG, '.build', 'design_tier_report_%s%s.csv' % (st, a.tag))
    rep.to_csv(rep_path, index=False)
    print('\nwrote %s: %d base + %d design rows\nwrote %s' % (out_path, len(base), len(b_rows), rep_path))


if __name__ == '__main__':
    main()
