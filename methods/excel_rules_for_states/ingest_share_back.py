"""Ingest a state's Step 6 share-back and emit the refined-list proposal.

    python ingest_share_back.py SHARE_BACK.(xlsx|csv) --state <ST> [--floor 0.20]

Protocol (share_back_transfer_plan.md, 2026-08-27):
  1. CALIBRATION GATE: book-level aggregates vs national expectations; a
     uniform shortfall is a data/mapping problem and the script says so
     loudly instead of judging rules.
  2. VETO: for rules with internal n >= 30, a one-sided exact binomial test
     of H0: precision >= floor, Benjamini-Hochberg at FDR 10%; a rule is
     proposed DROP only when BH rejects AND its one-sided 97.5% Wilson
     upper bound sits below the floor. Rules with material ineligible
     catches (>= 3, or >= 10% of the rule's errors) are PROTECTED (human
     review), never auto-dropped.
  3. REFILL: one Include?-promotion proposed per drop, from the workbook's
     unshipped rules in descending national-LCB order.
  4. Outputs: refined_include_<ST>.csv (verdicts + reasons) and
     share_back_dataset_<ST>.csv (internal + national + full
     characterization, one row per rule) for the transfer regression.

The quoted performance of the refined list is the national priors plus the
veto rationale; certification is the NEXT share-back (never the selecting
data)."""
import argparse
import csv
import math
import os
import re
import sys

import pandas as pd

PKG = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.dirname(os.path.dirname(PKG))
sys.path.insert(0, PKG)
import states as STATE_REGISTRY                     # noqa: E402
CHAR_CSV = os.path.join(REPO, 'methods', 'v250_candidate_lists_utilsua',
                        'rule_characterization_v250.csv')

FDR_ALPHA = 0.10
MIN_N_VETO = 30
INELIG_PROTECT_COUNT = 3
INELIG_PROTECT_SHARE = 0.10


# ── small numerics (no scipy dependency) ─────────────────────────────────────
def binom_cdf(k, n, p):
    """P(X <= k) for X ~ Binomial(n, p), exact via log terms."""
    if p <= 0:
        return 1.0
    if p >= 1:
        return 0.0 if k < n else 1.0
    lf = math.lgamma
    total = 0.0
    for i in range(0, int(k) + 1):
        total += math.exp(lf(n + 1) - lf(i + 1) - lf(n - i + 1)
                          + i * math.log(p) + (n - i) * math.log(1 - p))
    return min(total, 1.0)


def wilson_upper(k, n, z=1.96):
    """One-sided upper bound of k/n (z = 1.96 -> 97.5% one-sided)."""
    if n == 0:
        return 1.0
    ph = k / n
    d = 1 + z * z / n
    c = ph + z * z / (2 * n)
    h = z * math.sqrt(ph * (1 - ph) / n + z * z / (4 * n * n))
    return min(1.0, (c + h) / d)


def bh_reject(pvals, alpha=FDR_ALPHA):
    """Benjamini-Hochberg: returns a set of indices rejected at FDR alpha."""
    order = sorted(range(len(pvals)), key=lambda i: pvals[i])
    m = len(pvals)
    kmax = -1
    for r, i in enumerate(order, 1):
        if pvals[i] <= alpha * r / m:
            kmax = r
    return set(order[:kmax]) if kmax > 0 else set()


# ── Step 6 sheet parsing ─────────────────────────────────────────────────────
def read_share_back(path, sheet=None):
    """Rows of the pasted Step 6 sheet -> (meta dict, denoms dict, rule rows).
    `sheet` picks a worksheet by name when a partner returns several result
    sets in one file (e.g. QC and QA tabs); default is the first sheet.
    A dash/blank in a count column reads as CENSORED (None) - partners
    suppress small cells - and stays distinct from a true zero."""
    if path.lower().endswith('.csv'):
        with open(path, newline='', encoding='utf-8-sig') as fh:
            grid = [row for row in csv.reader(fh)]
    else:
        import openpyxl
        wb = openpyxl.load_workbook(path, data_only=True)
        ws = wb[sheet] if sheet else wb.worksheets[0]
        grid = [[c.value for c in row] for row in ws.iter_rows()]

    def cell(r, c):
        try:
            return grid[r][c]
        except IndexError:
            return None

    def num(v):
        """Tolerant numeric read: dashes, blanks, and text placeholders in a
        pasted export read as 0 (a rule with no usable count is excluded by
        the support floors downstream)."""
        if v is None:
            return 0.0
        try:
            return float(v)
        except (TypeError, ValueError):
            return 0.0

    meta, denoms, hdr_row = {}, {}, None
    for r, row in enumerate(grid):
        a = str(row[0]).strip() if row and row[0] is not None else ''
        if a in ('State', 'Fiscal years pasted into Step 2 (e.g., 2025-2026)',
                 'Data pasted (QC / QA / pre-auth / other)'):
            meta[a.split(' (')[0]] = cell(r, 1)
        elif a in ('Total cases', 'Error cases', 'Error $',
                   'Ineligible-household cases (STATUS = 4)'):
            denoms[a.split(' (')[0]] = num(cell(r, 1))
        elif a == 'Rule' and str(cell(r, 3) or '').strip() == 'Flagged':
            hdr_row = r
            break
    if hdr_row is None:
        sys.exit('could not find the Step 6 header row (Rule / ... / Flagged)')

    def count(v):
        """None = censored (dash/blank suppression of a small cell)."""
        if v is None or (isinstance(v, str) and v.strip() in ('-', '')):
            return None
        return num(v)

    rules = []
    for row in grid[hdr_row + 1:]:
        rid = str(row[0]).strip() if row and row[0] is not None else ''
        parts = rid.split()
        if not rid.startswith('Rule') or len(parts) < 2 \
                or not parts[-1].isdigit():
            continue
        n = count(row[3])
        rules.append({
            'num': int(parts[-1]), 'hh': str(row[1]).strip(),
            'n': n if n is not None else float('nan'),
            'k': (count(row[4]) if count(row[4]) is not None
                  else float('nan')),
            'censored': n is None,
            'drecall': num(row[6]), 'inelig': num(row[7]),
        })
    return meta, denoms, rules


# ── main ─────────────────────────────────────────────────────────────────────
def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('share_back')
    ap.add_argument('--state', required=True)
    ap.add_argument('--floor', type=float, default=0.20)
    ap.add_argument('--sheet', default=None,
                    help='worksheet name when the file has several result sets')
    ap.add_argument('--missing', default='',
                    help='comma list of feature vars the state did not paste '
                         '(e.g. homeless,expedited_i); rules using them are '
                         'no-input, not failures')
    a = ap.parse_args()
    st = a.state.upper()
    missing_vars = {v.strip() for v in a.missing.split(',') if v.strip()}

    meta, denoms, internal = read_share_back(a.share_back, sheet=a.sheet)
    eff = pd.read_csv(os.path.join(PKG, '.build', f'effective_rules_{st}.csv'))
    if 'ship' not in eff.columns:
        eff['ship'] = True
    char = pd.read_csv(CHAR_CSV)

    idf = pd.DataFrame(internal)
    df = idf.merge(eff, left_on=['num', 'hh'], right_on=['rank', 'hh'],
                   how='left', suffixes=('', '_eff'))
    missing = df['rule'].isna().sum()
    if missing:
        print(f'WARNING: {missing} share-back rows match no rule in '
              f'effective_rules_{st}.csv (version mismatch?)')
    # characterization joins through the ORIGINAL delivery/measurement text
    # keyed by rank: the effective list's text is regenerated post-strip, so
    # a text join would miss exactly the bbce-stripped rules (review D3)
    cfg = STATE_REGISTRY.get(st)
    sources = [pd.read_csv(os.path.join(REPO, cfg['delivery_csv']))]
    mpath = os.path.join(PKG, '.build', f'measurement_rules_{st}.csv')
    if os.path.isfile(mpath):
        sources.append(pd.read_csv(mpath))
    orig = (pd.concat(sources, ignore_index=True)[['rank', 'hh', 'rule']]
            .drop_duplicates(['rank', 'hh'])
            .rename(columns={'rule': 'rule_orig'}))
    df = df.merge(orig, left_on=['num', 'hh'], right_on=['rank', 'hh'],
                  how='left', suffixes=('', '_orig'))
    df = df.merge(char, left_on=['hh', 'rule_orig'], right_on=['hh', 'rule'],
                  how='left', suffixes=('', '_char'))

    # 1. calibration gate ----------------------------------------------------
    print(f"share-back: {meta.get('State', '?')} | years "
          f"{meta.get('Fiscal years pasted into Step 2', '?')} | data type "
          f"{meta.get('Data pasted', '?')}")
    tot, errs = denoms.get('Total cases', 0), denoms.get('Error cases', 0)
    base = errs / tot if tot else float('nan')
    print(f'book: {tot:.0f} cases, {errs:.0f} errors (base rate {base:.1%}), '
          f"error $ {denoms.get('Error $', 0):,.0f}, ineligible "
          f"{denoms.get('Ineligible-household cases', 0):.0f}")
    # rules whose conditions read an input the state did not paste can never
    # fire in this workbook: no-input, excluded from every judgment
    var_pat = re.compile(r'[A-Za-z_][A-Za-z0-9_]*')
    df['no_input'] = df['rule'].fillna('').apply(
        lambda t: bool(missing_vars & set(var_pat.findall(t))))
    if df['no_input'].any():
        print(f"no-input rules (conditions use {sorted(missing_vars)}): "
              f"{int(df['no_input'].sum())} - excluded from all judgments")

    scored = df[(df['n'] > 0) & df['precision_train'].notna()
                & ~df['no_input']].copy()
    scored['p_int'] = scored['k'] / scored['n']
    ratio = (scored['p_int'] / scored['precision_train']).median()
    print(f'median per-rule precision ratio internal/national: {ratio:.2f} '
          f'({len(scored)} rules with internal flags)')
    if ratio < 0.5:
        # the design note's step 1 GATES: a uniform 2x shortfall is a
        # data/mapping problem, and BH's adaptivity would convert it into a
        # mass rewrite of the list. No verdicts are issued (review A5).
        print('CALIBRATION FAILURE: the whole book underperforms its national '
              'priors by 2x or more. Check the paste (as-reported values? '
              'column mapping? review-period mix). A uniform shortfall is a '
              'data problem, not rule failure - no verdicts are issued.')
        out2 = os.path.join(PKG, '.build', f'share_back_dataset_{st}.csv')
        df.to_csv(out2, index=False)
        print(f'transfer-analysis dataset still written: {out2}')
        sys.exit(2)

    # 2. veto ----------------------------------------------------------------
    # merge-miss rows are untestable, not shippable (review B4)
    df['matched'] = df['rule'].notna()
    df['ship'] = df['ship'].fillna(False).astype(bool)
    df['verdict'], df['reason'] = 'keep', ''
    df.loc[~df['matched'], 'verdict'] = 'unmatched'
    df.loc[df['matched'] & ~df['ship'], 'verdict'] = 'measure'
    df.loc[df['no_input'], 'verdict'] = 'no-input'
    if 'censored' in df.columns:
        df.loc[df['censored'].fillna(False) & (df['verdict'] == 'keep'),
               'reason'] = 'internal counts censored (small cell) - untested'
    # promotion candidates face the same veto as shipped rules (review A6),
    # but as a SEPARATE BH family: the two decisions (drop a deployed rule;
    # bar a candidate from promotion) carry their own FDR budgets, and one
    # merged family would let 250 measurement rules dilute the power of
    # shipped-rule vetoes
    test = df[(df['n'] >= MIN_N_VETO) & df['matched']
              & ~df['no_input']].copy()
    test['pval'] = [binom_cdf(k, int(n), a.floor)
                    for k, n in zip(test['k'], test['n'])]
    test['upper'] = [wilson_upper(k, int(n))
                     for k, n in zip(test['k'], test['n'])]
    rejected = set()
    for fam in (test['ship'], ~test['ship']):
        sub = test[fam]
        rej = bh_reject(list(sub['pval']))
        pos_of = {j: p for p, j in enumerate(test.index)}
        rejected |= {pos_of[j] for p, j in enumerate(sub.index) if p in rej}
    for pos, (i, row) in enumerate(test.iterrows()):
        if not (pos in rejected and row['upper'] < a.floor):
            continue
        protected = (row['inelig'] >= INELIG_PROTECT_COUNT
                     or (row['k'] > 0 and row['inelig'] / row['k']
                         >= INELIG_PROTECT_SHARE))
        if not row['ship']:
            # a measurement rule failing its own internal read is barred
            # from promotion (review A6); it stays in the dataset
            df.loc[i, 'verdict'] = 'measure-vetoed'
            df.loc[i, 'reason'] = (
                f"internal precision {row['k']:.0f}/{row['n']:.0f}; upper "
                f"bound {row['upper']:.1%} < floor {a.floor:.0%} - not "
                'promotable')
        elif protected:
            df.loc[i, 'verdict'] = 'protected'
            df.loc[i, 'reason'] = (
                f"precision {row['k']:.0f}/{row['n']:.0f} fails the "
                f"{a.floor:.0%} floor but catches "
                f"{row['inelig']:.0f} ineligible households - review, "
                'do not auto-drop')
        else:
            df.loc[i, 'verdict'] = 'drop'
            df.loc[i, 'reason'] = (
                f"internal precision {row['k']:.0f}/{row['n']:.0f} = "
                f"{row['k']/row['n']:.1%}; one-sided upper bound "
                f"{row['upper']:.1%} < floor {a.floor:.0%} "
                f'(BH at FDR {FDR_ALPHA:.0%})')

    n_drop = int((df['verdict'] == 'drop').sum())

    # 3. refill proposals ----------------------------------------------------
    cand = df[(df['verdict'] == 'measure')].copy()
    cand = cand.sort_values('precision_train_lcb', ascending=False)
    promoted = 0
    for i, row in cand.iterrows():
        if promoted >= n_drop:
            break
        df.loc[i, 'verdict'] = 'promote'
        df.loc[i, 'reason'] = ('refill: next by national 99% LCB '
                               f"({row['precision_train_lcb']:.3f})")
        promoted += 1

    # 4. outputs -------------------------------------------------------------
    out_cols = ['num', 'hh', 'rule', 'verdict', 'reason', 'n', 'k', 'inelig',
                'precision_train', 'precision_train_lcb']
    out1 = os.path.join(PKG, '.build', f'refined_include_{st}.csv')
    df[out_cols].rename(columns={'num': 'rule_id', 'n': 'internal_flagged',
                                 'k': 'internal_errors',
                                 'inelig': 'internal_ineligible'}
                        ).to_csv(out1, index=False)
    out2 = os.path.join(PKG, '.build', f'share_back_dataset_{st}.csv')
    df.to_csv(out2, index=False)
    print(f"\nverdicts: keep {int((df['verdict']=='keep').sum())} | "
          f"drop {n_drop} | protected "
          f"{int((df['verdict']=='protected').sum())} | promote {promoted} | "
          f"measure {int((df['verdict']=='measure').sum())} | "
          f"measure-vetoed {int((df['verdict']=='measure-vetoed').sum())} | "
          f"unmatched {int((df['verdict']=='unmatched').sum())}")
    print(f'refined configuration: {out1}')
    print(f'transfer-analysis dataset: {out2}')
    print('\nApply by setting Include? = FALSE on drop rows and TRUE on '
          'promote rows in the state workbook; the orange combined rows '
          'recompute the union workload - confirm it and trim from the '
          'bottom if over budget.')


if __name__ == '__main__':
    main()
