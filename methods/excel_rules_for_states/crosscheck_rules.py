"""
Test that the rules as shown in the workbook's Summary (National) tab match an
independent R implementation of the same rules over the same years of
reg_model_data.rds.

    python crosscheck_rules.py WA [--workbook path.xlsx]

Runs crosscheck_rules.R (which re-parses the delivery CSV, re-applies every
rule in R with the miner's own prep_features(), and scores it) and compares,
per rule: n flagged, errors caught, error dollars, precision, recall, dollar
recall, workload — plus the all-rules union, recomputed from the workbook's
RuleFlags hit matrix, overall and per household-size stratum.

Exit code 0 = everything matches. Run it against the BUILT workbook (the plain
snap_qc_dashboard_<ABBR>.xlsx): its Summary (National) values are static, so
they can be read without Excel. The LIVE/RECON variants hold the same numbers
as formulas; verify those by opening in Excel.
"""
import argparse
import os
import shutil
import subprocess
import sys
import tempfile

import numpy as np
import openpyxl
import pandas as pd

import states as STATE_REGISTRY

PKG = os.path.dirname(os.path.abspath(__file__))
NAT_ROW0, FLAG0 = 11, 5


def find_repo(start):
    cands = [os.environ['SNAP_REPO']] if os.environ.get('SNAP_REPO') else []
    d = start
    for _ in range(6):
        cands += [d, os.path.join(d, 'snap_qc')]
        d = os.path.dirname(d)
    for c in cands:
        if (os.path.isfile(os.path.join(c, 'reg_model_data.rds'))
                and os.path.isfile(os.path.join(c, 'rule_mining_helpers.R'))):
            return os.path.abspath(c)
    raise SystemExit('cannot find the snap_qc checkout; set SNAP_REPO')


def rscript():
    for c in ([os.environ['RSCRIPT']] if os.environ.get('RSCRIPT') else []) + [
            os.path.join('C:' + os.sep, 'Program Files', 'R', 'R-4.5.1',
                         'bin', 'Rscript.exe'), 'Rscript']:
        if os.path.isabs(c) and os.path.isfile(c):
            return c
        if not os.path.isabs(c) and shutil.which(c):
            return shutil.which(c)
    raise SystemExit('Rscript not found; set RSCRIPT')


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('state', nargs='?', default='WA')
    ap.add_argument('--workbook')
    a = ap.parse_args()
    cfg = STATE_REGISTRY.get(a.state.upper())
    repo = find_repo(PKG)
    base = os.path.dirname(PKG)
    wbp = a.workbook or os.path.join(PKG, 'state_workbooks', cfg['abbr'],
                                     f'snap_qc_dashboard_{cfg["abbr"]}.xlsx')
    cases_csv = os.path.join(os.path.dirname(wbp),
                             f'{cfg["abbr"].lower()}_cases.csv')
    delivery = os.path.join(repo, cfg['delivery_csv'])
    if not os.path.isfile(delivery):
        delivery = os.path.join(base, cfg['delivery_csv'])

    tmp = tempfile.mkdtemp(prefix='crosscheck_')
    r_out = os.path.join(tmp, 'r_rules.csv')
    cmd = [rscript(), os.path.join(PKG, 'crosscheck_rules.R'),
           '--state', cfg['name'], '--csv', delivery, '--out', r_out,
           '--years', ','.join(str(y) for y in cfg.get('years', (2022, 2023, 2024))),
           '--repo', repo, '--role', cfg['role_filter']]
    print('$ ' + ' '.join(cmd))
    if subprocess.run(cmd, cwd=repo).returncode != 0:
        raise SystemExit('crosscheck_rules.R failed')
    r_rules = pd.read_csv(r_out)
    r_union = pd.read_csv(r_out.replace('.csv', '_union.csv'))

    wb = openpyxl.load_workbook(wbp)
    nat = wb['Summary (National)']
    NR = len(r_rules)
    fails = 0
    for j in range(NR):
        r = NAT_ROW0 + j
        rk = int(str(nat.cell(row=r, column=1).value).split()[-1])
        rr = r_rules[r_rules['rank'] == rk]
        assert len(rr) == 1, f'rank {rk} missing from the R output'
        rr = rr.iloc[0]
        vals = [('hh', nat.cell(row=r, column=2).value, rr['hh'], None),
                ('precision', nat.cell(row=r, column=4).value, rr['precision'], 5e-5),
                ('recall', nat.cell(row=r, column=5).value, rr['recall'], 5e-5),
                ('dollar_recall', nat.cell(row=r, column=6).value, rr['dollar_recall'], 5e-5),
                ('n_flagged', nat.cell(row=r, column=7).value, rr['n_flagged'], 0),
                ('errors', nat.cell(row=r, column=8).value, rr['errors'], 0),
                ('dollars', nat.cell(row=r, column=9).value, rr['dollars'], 0.51),
                ('workload', nat.cell(row=r, column=10).value, rr['workload'], 5e-5)]
        for name, got, want, tol in vals:
            if tol is None:
                ok = str(got) == str(want)
            else:
                # the builder writes ratios as round(value, 4); compare to that
                ok = abs(float(got) - round(float(want), 4)) <= max(tol, 1e-9)
            if not ok:
                fails += 1
                print(f'MISMATCH rule {rk} {name}: workbook={got!r} R={want!r}')
    print(f'per-rule: {NR} rules x 8 fields checked, {fails} mismatches')

    # union via the RuleFlags original-threshold hit block
    rf = wb['RuleFlags']
    ndata = int(r_union.loc[r_union.scope == 'all', 'tot_cases'].iat[0])
    hit = np.zeros(ndata, dtype=int)
    for j in range(NR):
        col = 3 + NR + j
        for i in range(ndata):
            if rf.cell(row=FLAG0 + i, column=col).value == 1:
                hit[i] = 1
    cases = pd.read_csv(cases_csv)
    assert len(cases) == ndata, 'cases CSV row count differs from the R frame'
    is_err = (cases['over_threshold'] == 1).values
    ed = np.where(is_err, cases['total_error_amount'].fillna(0).values, 0)
    ufails = 0
    for _, u in r_union.iterrows():
        sel = np.ones(ndata, bool) if u['scope'] == 'all' \
            else (cases['hh_group'].astype(str) == u['scope']).values
        got = (int((hit & sel).sum()), int(((hit == 1) & sel & is_err).sum()),
               float(ed[(hit == 1) & sel].sum()))
        want = (int(u['flagged']), int(u['errors']), float(u['dollars']))
        if got != want:
            ufails += 1
            print(f"UNION MISMATCH scope {u['scope']}: workbook={got} R={want}")
        else:
            print(f"union scope {u['scope']:>3}: flagged {got[0]}, errors {got[1]}, "
                  f"dollars {got[2]:.0f}  == R")
    print(f'union: {ufails} mismatches')
    sys.exit(1 if (fails or ufails) else 0)


if __name__ == '__main__':
    main()
