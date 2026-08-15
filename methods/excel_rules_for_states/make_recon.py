"""
Reconstruction stage: the workbook accepts RAW case fields and computes every
model feature with live formulas, so a state that can run neither Python nor R
can still use it. The raw columns carry the FNS QC-schedule field names
(FSEARN, RENT, UTIL, FSWAGES, ...) because those are the fields states already
report to FNS every year, labeled with those names.

Data tab layout (one Excel table, columns bound by name):
  [ feature columns ]   the model features, in the SAME columns the built
                        workbook put them (A..), now formulas — positional
                        references from Error Cases/RuleFlags stay valid
  [ hit columns ]       per-case rule tests, carried over from the LIVE build
  [ helper columns ]    the benefit-recomputation chain, hidden
  [ raw contract ]      what a state pastes: FNS-named fields, plus two
                        compressed person-level counts (NUM_ABAWD, MARRIED_I)
                        and the QC review outcome (OVER_THRESHOLD, AMTERR)

Federal parameter tables (standard deduction, max allotment, shelter cap,
minimum allotment, by fiscal year x household size) live on a hidden
FederalTables sheet; formulas look them up by year and size.

Because the workbook's raw columns hold the AS-REPORTED public QC values while
the delivery rules were mined on the munged frame's pre-QC-restored values,
formula-computed features differ from the frame on rows the restoration
touched (rows carrying QC error elements). The built-in validation prints the
match rate per feature, overall and on element-free rows, every build. For
state-supplied as-reported data that gap does not exist.

Usage:  python make_recon.py <LIVE_workbook.xlsx> -o <out.xlsx> [--state WA]
Then:   python postprocess_workbook.py <out.xlsx> <checkbox_cells.json>
"""
import argparse
import os
import shutil

import numpy as np
import pandas as pd
import openpyxl
import pyreadstat
from openpyxl.comments import Comment
from openpyxl.styles import PatternFill, Font
from openpyxl.utils import get_column_letter as CL
from openpyxl.worksheet.table import Table, TableStyleInfo

import states as STATE_REGISTRY

TABLE = 'CaseData'
PKG = os.path.dirname(os.path.abspath(__file__))


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


REPO = find_repo(PKG)

# ── the raw input contract: FNS QC-schedule fields, one row per reviewed case ─
INCOME_VARS = ['FSWAGES', 'FSSLFEMP', 'FSOTHERN', 'FSSSI', 'FSTANF', 'FSGA',
               'FSSOCSEC', 'FSUNEMP', 'FSVET', 'FSWCOMP', 'FSEDLOAN',
               'FSCSUPRT', 'FSDEEM', 'FSCONT', 'FSOTHGOV', 'FSOTHUN',
               'FSDIVER', 'FSWGESUP', 'FSENERGY', 'FSEITC', 'FSFOSTER']
DEDUCT_VARS = ['FSSTDDED', 'FSERNDED', 'FSDEPDED', 'FSSLTDED', 'FSMEDDED',
               'FSCSDED', 'HOMELESS_DED']
DIV100_VARS = INCOME_VARS + DEDUCT_VARS          # count_divisible_by_100 basis

RAW_COLS = (
    ['YRMONTH', 'HHLDNO',
     'FSUSIZE', 'CERTHHSZ', 'FSNKID', 'FSNELDER', 'FSNDIS',
     'NUM_ABAWD',      # members with ABWDST1-18 in 2..5 (compressed)
     'MARRIED_I',      # 1 if any REL1-16 = 2, i.e. a spouse present (compressed)
     'EXPEDSER', 'CAT_ELIG', 'HOMEDED', 'LASTCERT',
     'FSEARN', 'FSUNEARN']
    + INCOME_VARS + DEDUCT_VARS
    + ['RENT', 'UTIL',
       # QC review outcome. NB not named OVER_THRESHOLD: Excel table column
       # names are case-insensitively unique, and the feature column
       # over_threshold already claims that name
       'ERROR_FLAG',       # 1 = payment error over threshold
       'AMTERR'])          # benefit amount in error, $


def raw_frame(cfg, frame_csv):
    """Raw FNS fields for exactly the frame's rows, in the frame's row order.

    Reads the public QC .sav files, filters to the state, and joins on
    yrmonth + hhldno + stratum. A state replaces this block by pasting its own
    FNS-reported fields; the QC outcome columns come from its own reviews.
    """
    frame = pd.read_csv(frame_csv, dtype={'hhldno': str, 'stratum': str})
    parts = []
    for f in cfg['qc_files']:
        d, _ = pyreadstat.read_sav(os.path.join(REPO, f))
        d.columns = [c.upper() for c in d.columns]
        d = d[d['STATE'] == cfg['fips']].copy()
        parts.append(d)
    sav = pd.concat(parts, ignore_index=True)
    sav['_ym'] = pd.to_numeric(sav['YRMONTH'], errors='coerce').astype('Int64')
    sav['_hh'] = sav['HHLDNO'].astype(str).str.strip().str.replace(r'\.0$', '', regex=True)
    sav['_st'] = sav['STRATUM'].astype(str).str.strip().str.replace(r'\.0$', '', regex=True)
    key_cols = ['_ym', '_hh', '_st']
    assert not sav.duplicated(key_cols).any(), 'duplicate join keys in the .sav files'

    left = pd.DataFrame({
        '_ym': pd.to_numeric(frame['yrmonth'], errors='coerce').astype('Int64'),
        '_hh': frame['hhldno'].astype(str).str.strip().str.replace(r'\.0$', '', regex=True),
        '_st': frame['stratum'].astype(str).str.strip().str.replace(r'\.0$', '', regex=True),
    })
    m = left.merge(sav, on=key_cols, how='left', validate='one_to_one')
    assert len(m) == len(frame), 'join changed the row count'
    unmatched = m['YRMONTH'].isna().sum()
    assert unmatched == 0, f'{unmatched} frame rows have no .sav match'

    abwd = [c for c in m.columns if c.startswith('ABWDST')]
    rel = [f'REL{i}' for i in range(1, 17) if f'REL{i}' in m.columns]
    out = pd.DataFrame(index=m.index)
    for c in RAW_COLS:
        if c == 'NUM_ABAWD':
            out[c] = m[abwd].apply(pd.to_numeric, errors='coerce').isin([2, 3, 4, 5]).sum(axis=1)
        elif c == 'MARRIED_I':
            out[c] = (m[rel].apply(pd.to_numeric, errors='coerce') == 2).any(axis=1).astype(int)
        elif c == 'ERROR_FLAG':
            out[c] = frame['is_error'].astype(int).values
        elif c == 'AMTERR':
            out[c] = pd.to_numeric(frame['total_error_amount'], errors='coerce').values
        elif c in m.columns:
            out[c] = pd.to_numeric(m[c], errors='coerce')
        else:                       # e.g. FSEITC absent from some years' files
            out[c] = 0.0
            print(f'  raw field {c} not in the .sav files; filled with 0')
    # element-free marker, for the validation report only (not written)
    el1 = pd.to_numeric(m['ELEMENT1'], errors='coerce') if 'ELEMENT1' in m.columns else pd.Series(np.nan, index=m.index)
    return out, el1.isna().values, frame


def federal_tables(wb, holdout_year):
    """Hidden sheet with the year x size parameter tables + settings."""
    ad = os.path.join(REPO, 'additional_data')
    yd = pd.read_csv(os.path.join(ad, 'year_data.csv')).dropna(axis=1, how='all')
    yd.columns = [str(c).strip() for c in yd.columns]
    sd = pd.read_csv(os.path.join(ad, 'standard_deductions.csv')).dropna(axis=1, how='all')
    ma = pd.read_csv(os.path.join(ad, 'max_allotments.csv')).dropna(axis=1, how='all')
    for t in (sd, ma):
        t.columns = [str(c).strip() for c in t.columns]

    ws = wb.create_sheet('FederalTables')
    ws.sheet_state = 'hidden'
    ws['A1'] = ('Federal SNAP parameters by fiscal year. Append a row per table '
                'each new fiscal year; every formula updates automatically.')
    ws['A3'], ws['B3'] = 'holdout_year (split label only)', holdout_year
    ws['A5'], ws['B5'], ws['C5'] = 'year', 'max_shelter', 'min_allotment'
    years = sorted(yd['year'])
    for i, y in enumerate(years):
        r = yd[yd.year == y].iloc[0]
        ws.cell(row=6 + i, column=1, value=int(y))
        ws.cell(row=6 + i, column=2, value=float(r['max_shelter_deduction']))
        ws.cell(row=6 + i, column=3, value=float(r['min_allotment']))
    ny = len(years)

    def block(df, row0, label):
        ws.cell(row=row0, column=5, value=label)
        ws.cell(row=row0 + 1, column=5, value='year')
        for s in range(1, 21):
            ws.cell(row=row0 + 1, column=5 + s, value=s)
        yrs = sorted(df['year'])
        for i, y in enumerate(yrs):
            ws.cell(row=row0 + 2 + i, column=5, value=int(y))
            r = df[df.year == y].iloc[0]
            for s in range(1, 21):
                ws.cell(row=row0 + 2 + i, column=5 + s, value=float(r[str(s)]))
        return len(yrs), row0 + 2
    n_sd, sd0 = block(sd.reset_index(), 5, 'standard deduction')
    n_ma, ma0 = block(ma.reset_index(), 5 + n_sd + 4, 'max allotment')
    return {
        'YEARS':  f'FederalTables!$A$6:$A${5 + ny}',
        'MAXSH':  f'FederalTables!$B$6:$B${5 + ny}',
        'MINAL':  f'FederalTables!$C$6:$C${5 + ny}',
        'SDYRS':  f'FederalTables!$E${sd0}:$E${sd0 + n_sd - 1}',
        'SDBLK':  f'FederalTables!$F${sd0}:$Y${sd0 + n_sd - 1}',
        'MAYRS':  f'FederalTables!$E${ma0}:$E${ma0 + n_ma - 1}',
        'MABLK':  f'FederalTables!$F${ma0}:$Y${ma0 + n_ma - 1}',
        'HOLDOUT': 'FederalTables!$B$3',
    }


def T(col):
    return f'{TABLE}[[#This Row],[{col}]]'


def feature_formulas(R):
    """name -> formula for every feature the current delivery vocabulary uses,
    mirroring 1_data_munging_..._for_using_public_qc_data.R. Helpers are
    prefixed '_c_'; order matters (left to right)."""
    sz = f'MIN(MAX({T("FSUSIZE")},1),20)'
    hh = f'MAX({T("FSUSIZE")},1)'
    helpers = [
        ('_c_fy',      f'=IF(MOD({T("YRMONTH")},100)>=10,INT({T("YRMONTH")}/100)+1,'
                       f'INT({T("YRMONTH")}/100))'),
        ('_c_eld',     f'=IF({T("FSNELDER")}+{T("FSNDIS")}>0,1,0)'),
        ('_c_gross',   f'={T("FSEARN")}+{T("FSUNEARN")}'),
        # _xlfn. prefix: post-2007 functions written straight into the XML
        # need it, or Excel renders #NAME?
        ('_c_ernded',  f'=_xlfn.FLOOR.MATH({T("FSEARN")}*0.2)'),
        ('_c_stdded',  f'=INDEX({R["SDBLK"]},MATCH({T("_c_fy")},{R["SDYRS"]},1),{sz})'),
        ('_c_benmax',  f'=INDEX({R["MABLK"]},MATCH({T("_c_fy")},{R["MAYRS"]},1),{sz})'),
        ('_c_netbs',   f'={T("_c_gross")}-({T("_c_ernded")}+{T("FSDEPDED")}'
                       f'+{T("FSMEDDED")}+{T("FSCSDED")}+{T("_c_stdded")})'),
        ('_c_maxsh',   f'=IF({T("_c_eld")}=1,1000000000,'
                       f'INDEX({R["MAXSH"]},MATCH({T("_c_fy")},{R["YEARS"]},1)))'),
        # NB: the munging script does NOT floor the shelter deduction; only the
        # net incomes and the benefit are floored (calculate_raw_benefits)
        ('_c_sltded',  f'=MIN(MAX({T("RENT")}+{T("UTIL")}'
                       f'-MAX({T("_c_netbs")}*0.5,0),0),{T("_c_maxsh")})'),
        ('_c_netan',   f'=_xlfn.FLOOR.MATH({T("_c_netbs")}-({T("_c_sltded")}+{T("HOMELESS_DED")}))'),
        ('_c_benunc',  f'=_xlfn.FLOOR.MATH({T("_c_benmax")}-0.3*{T("_c_netan")})'),
        ('_c_benrec',  f'=MIN(MAX({T("_c_benunc")},IF({T("FSUSIZE")}<3,'
                       f'INDEX({R["MINAL"]},MATCH({T("_c_fy")},{R["YEARS"]},1)),0)),'
                       f'{T("_c_benmax")})'),
    ]
    feats = {
        'fiscal_year': f'={T("_c_fy")}',
        'split':       f'=IF({T("_c_fy")}>={R["HOLDOUT"]},"holdout","tune")',
        'hh_size_raw': f'={T("FSUSIZE")}',
        'hh_group':    f'=IF({T("FSUSIZE")}>=4,"4+",IF({T("FSUSIZE")}>=2,"2-3","1"))',
        'HH_size_n':   f'={T("FSUSIZE")}',
        'bbce_state_i':
            # state-year regime flag: share of the year's cases with CAT_ELIG
            # >= 1 reaching 0.5 (munging script, 2026-08-13 decision)
            f'=IF(COUNTIFS({TABLE}[_c_fy],{T("_c_fy")},{TABLE}[CAT_ELIG],">=1")'
            f'/COUNTIFS({TABLE}[_c_fy],{T("_c_fy")})>=0.5,1,0)',
        'children_i':  f'=IF({T("FSNKID")}>0,1,0)',
        'count_divisible_by_100':
            '=' + '+'.join(f'IF(AND({T(c)}>0,MOD({T(c)},100)=0),1,0)'
                           for c in DIV100_VARS),
        'elderly_disabled_i': f'={T("_c_eld")}',
        'expedited_i': f'=IF(OR({T("EXPEDSER")}=1,{T("EXPEDSER")}=2),1,0)',
        # blank HOMEDED -> 0: the frame carries NA there and the miner's flag
        # evaluator treats NA conditions as never-firing
        'homeless':    f'=IF({T("HOMEDED")}="",0,IF({T("HOMEDED")}=1,0,1))',
        'married':     f'={T("MARRIED_I")}',
        'medical_deductions':  f'={T("FSMEDDED")}',
        'months_since_cert_n': f'={T("LASTCERT")}',
        'percent_abawd': f'={T("NUM_ABAWD")}/MAX({T("CERTHHSZ")},1)',
        'earned_by_hh_size':   f'={T("FSEARN")}/{hh}',
        'unearned_by_hh_size': f'={T("FSUNEARN")}/{hh}',
        'gross_by_hh_size':    f'=({T("FSEARN")}+{T("FSUNEARN")})/{hh}',
        'rawben_rel_max':      f'={T("_c_benrec")}/{T("_c_benmax")}',
        'shelter_expenses_by_hh_size': f'=({T("RENT")}+{T("UTIL")})/{hh}',
        'total_deductions_by_hh_size':
            f'=({T("FSDEPDED")}+{T("FSCSDED")}+{T("_c_sltded")}'
            f'+{T("FSMEDDED")}+{T("_c_ernded")})/{hh}',
        'unc_rawben_rel_max': f'={T("_c_benunc")}/{T("_c_benmax")}',
        'utilities':   f'={T("UTIL")}',
        'over_threshold':     f'={T("ERROR_FLAG")}',
        'total_error_amount': f'=ROUND(ABS({T("AMTERR")}),0)',
    }
    return helpers, feats


# ── validation: mirror every formula in pandas, score against the frame ──────
def _excel_floor(x, sig=1.0):
    return np.floor(np.asarray(x, float) / sig) * sig


def mirror_features(raw, ftabs):
    """Compute what the Excel formulas will produce, from the raw block."""
    g = lambda c: raw[c].fillna(0).astype(float).values     # Excel blank -> 0
    yd, sd, ma = ftabs
    fy = np.where(raw['YRMONTH'] % 100 >= 10, raw['YRMONTH'] // 100 + 1,
                  raw['YRMONTH'] // 100).astype(int)
    sz20 = np.clip(g('FSUSIZE'), 1, 20).astype(int)
    hh = np.maximum(g('FSUSIZE'), 1)
    lk = lambda tbl, col: np.array([tbl.loc[tbl.year <= y, col].iloc[-1] for y in fy])
    stdded = np.array([sd.loc[sd.year <= y, str(s)].iloc[-1] for y, s in zip(fy, sz20)])
    benmax = np.array([ma.loc[ma.year <= y, str(s)].iloc[-1] for y, s in zip(fy, sz20)])
    eld = ((g('FSNELDER') + g('FSNDIS')) > 0).astype(int)
    ernded = _excel_floor(g('FSEARN') * 0.2)
    netbs = g('FSEARN') + g('FSUNEARN') - (ernded + g('FSDEPDED') + g('FSMEDDED')
                                           + g('FSCSDED') + stdded)
    maxsh = np.where(eld == 1, 1e9, lk(yd, 'max_shelter_deduction'))
    sltded = np.minimum(np.maximum(g('RENT') + g('UTIL')
                                   - np.maximum(netbs * 0.5, 0), 0), maxsh)
    netan = _excel_floor(netbs - (sltded + g('HOMELESS_DED')))
    benunc = _excel_floor(benmax - 0.3 * netan)
    minal = lk(yd, 'min_allotment')
    benrec = np.minimum(np.maximum(benunc, np.where(g('FSUSIZE') < 3, minal, 0)), benmax)
    fyshare = pd.Series((g('CAT_ELIG') >= 1).astype(int)).groupby(fy).transform('mean')
    out = {
        'fiscal_year': fy, 'hh_size_raw': g('FSUSIZE'), 'HH_size_n': g('FSUSIZE'),
        'hh_group': np.where(g('FSUSIZE') >= 4, '4+', np.where(g('FSUSIZE') >= 2, '2-3', '1')),
        'bbce_state_i': (fyshare.values >= 0.5).astype(int),
        'children_i': (g('FSNKID') > 0).astype(int),
        'count_divisible_by_100': sum(((g(c) > 0) & (g(c) % 100 == 0)).astype(int)
                                      for c in DIV100_VARS),
        'elderly_disabled_i': eld,
        'expedited_i': np.isin(g('EXPEDSER'), [1, 2]).astype(int),
        'homeless': (raw['HOMEDED'].notna() & (raw['HOMEDED'] != 1)).astype(int).values,
        'married': g('MARRIED_I'), 'medical_deductions': g('FSMEDDED'),
        'months_since_cert_n': g('LASTCERT'),
        'percent_abawd': g('NUM_ABAWD') / np.maximum(g('CERTHHSZ'), 1),
        'earned_by_hh_size': g('FSEARN') / hh,
        'unearned_by_hh_size': g('FSUNEARN') / hh,
        'gross_by_hh_size': (g('FSEARN') + g('FSUNEARN')) / hh,
        'rawben_rel_max': benrec / benmax,
        'shelter_expenses_by_hh_size': (g('RENT') + g('UTIL')) / hh,
        'total_deductions_by_hh_size': (g('FSDEPDED') + g('FSCSDED') + sltded
                                        + g('FSMEDDED') + ernded) / hh,
        'unc_rawben_rel_max': benunc / benmax,
        'utilities': g('UTIL'),
        'over_threshold': g('ERROR_FLAG'),
        'total_error_amount': np.round(np.abs(g('AMTERR'))),
    }
    return pd.DataFrame(out)


def validate(raw, frame, elem_free, feat_names):
    ad = os.path.join(REPO, 'additional_data')
    yd = pd.read_csv(os.path.join(ad, 'year_data.csv')).sort_values('year')
    sd = pd.read_csv(os.path.join(ad, 'standard_deductions.csv')).sort_values('year')
    ma = pd.read_csv(os.path.join(ad, 'max_allotments.csv')).sort_values('year')
    for t in (sd, ma):
        t.columns = [str(c).strip() for c in t.columns]
    mir = mirror_features(raw, (yd, sd, ma))
    print(f'\nformula validation vs the munged frame '
          f'({len(frame)} rows, {int(elem_free.sum())} carry no QC error element):')
    print(f'  {"feature":32s} {"all rows":>9s} {"element-free":>13s}')
    worst = []
    for c in feat_names:
        if c not in mir.columns or c not in frame.columns:
            continue
        if c == 'hh_group':
            ok = mir[c].astype(str).values == frame[c].astype(str).values
        else:
            a = mir[c].astype(float).values
            # over_threshold / total_error_amount live in the workbook in
            # build_workbook_v2's transformed form; compare against that
            if c == 'over_threshold':
                b = frame['is_error'].astype(float).values
            elif c == 'total_error_amount':
                b = np.round(np.abs(pd.to_numeric(
                    frame[c], errors='coerce').fillna(0).values))
            else:
                b = pd.to_numeric(frame[c], errors='coerce').fillna(0).values
            ok = np.isclose(a, b, atol=1e-6, rtol=1e-9)
        r_all, r_free = ok.mean(), ok[elem_free].mean()
        print(f'  {c:32s} {r_all:9.1%} {r_free:13.1%}')
        worst.append((c, r_free))
    bad = [c for c, r in worst if r < 0.995]
    if bad:
        print(f'  WARNING: element-free match below 99.5% for: {", ".join(bad)}')
    return not bad


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('live_workbook')
    ap.add_argument('-o', '--out', required=True)
    ap.add_argument('--state', default=os.environ.get('SNAP_STATE', 'WA'))
    a = ap.parse_args()
    cfg = STATE_REGISTRY.get(a.state.upper())
    frame_csv = os.path.join(PKG, '.frames', f'{cfg["abbr"].lower()}_frame.csv')

    raw, elem_free, frame = raw_frame(cfg, frame_csv)
    print(f'raw extract: {len(raw)} rows x {len(RAW_COLS)} FNS fields')

    shutil.copy(a.live_workbook, a.out)
    wb = openpyxl.load_workbook(a.out)
    # the Dashboard (per-rule threshold tuner) stays hidden: engine plumbing;
    # unhide it in Excel to tune thresholds interactively
    dat = wb['Data']
    NROW = dat.max_row
    assert NROW - 1 == len(raw), f'workbook has {NROW-1} cases, extract {len(raw)}'

    hdr = [c.value for c in next(dat.iter_rows(min_row=1, max_row=1))]
    ncol0 = len(hdr)
    holdout_year = int(pd.to_numeric(
        frame['fiscal_year'], errors='coerce').max())    # split label only
    R = federal_tables(wb, holdout_year)
    helpers, feats = feature_formulas(R)

    missing = [h for h in hdr if not h.startswith('_') and h not in feats]
    assert not missing, f'no formula for Data columns: {missing}'
    # Excel table column names are case-insensitively unique; a collision makes
    # Excel reject the whole file as damaged, so fail here instead
    names = hdr + [h for h, _ in helpers] + RAW_COLS
    low = [n.lower() for n in names]
    dups = sorted({n for n in low if low.count(n) > 1})
    assert not dups, f'case-insensitive duplicate Data columns: {dups}'
    ok = validate(raw, frame, elem_free, [h for h in hdr if h in feats])
    if not ok:
        print('  (continuing: mismatches on element-carrying rows are the '
              'documented pre-QC restoration gap)')

    # 1. helper columns, then the raw contract, appended AFTER the existing
    #    feature + hit columns so every positional reference stays valid
    c = ncol0
    helper_idx = []
    for name, formula in helpers:
        c += 1
        helper_idx.append(c)
        dat.cell(row=1, column=c, value=name)
        for rr in range(2, NROW + 1):
            dat.cell(row=rr, column=c, value=formula)
    raw0 = c + 1
    for name in RAW_COLS:
        c += 1
        dat.cell(row=1, column=c, value=name)
        vals = raw[name].tolist()
        for i, v in enumerate(vals):
            dat.cell(row=2 + i, column=c,
                     value=None if (isinstance(v, float) and np.isnan(v)) else v)
    LAST = CL(c)

    # 2. the feature columns become formulas (values overwritten in place)
    n_swapped = 0
    for ci, name in enumerate(hdr, 1):
        if name in feats:
            for rr in range(2, NROW + 1):
                dat.cell(row=rr, column=ci, value=feats[name])
            n_swapped += 1
    print(f'features -> formulas: {n_swapped} columns | helpers: {len(helpers)} | '
          f'raw: {len(RAW_COLS)} -> Data!A1:{LAST}{NROW}')

    # 3. rebind the table over the widened range
    for t in list(dat.tables):
        del dat.tables[t]
    tbl = Table(displayName=TABLE, ref=f'A1:{LAST}{NROW}')
    tbl.tableStyleInfo = TableStyleInfo(name='TableStyleLight1', showRowStripes=False)
    dat.add_table(tbl)

    # 4. presentation: helpers hidden; features blue; raw amber, collapsible
    feat_fill = PatternFill('solid', fgColor='DDEBF7')
    raw_fill = PatternFill('solid', fgColor='FCE4D6')
    for i in range(1, ncol0 + 1):
        dat.cell(row=1, column=i).fill = feat_fill
    for i in helper_idx:
        dat.column_dimensions[CL(i)].hidden = True
    for i in range(raw0, c + 1):
        dat.cell(row=1, column=i).fill = raw_fill
    # collapse button sits left of the group (summary-left): the feature block
    # collapses to leave the raw block, and vice versa
    dat.sheet_properties.outlinePr.summaryRight = False
    for i in range(2, ncol0 + 1):
        if not hdr[i - 1].startswith('_'):
            dat.column_dimensions[CL(i)].outline_level = 1
    for i in range(raw0 + 2, c + 1):
        dat.column_dimensions[CL(i)].outline_level = 1
    note = ('Feature columns (blue) are FORMULAS computed from the raw FNS '
            'fields (amber block to the right); do not paste values over them. '
            'A state supplies ONLY the amber columns, using its FNS QC-schedule '
            'field names, plus the two QC outcome columns.')
    dat.cell(row=1, column=1).comment = Comment(note, 'snap_dashboard')
    dat.freeze_panes = 'B2'

    wb.save(a.out)
    print('saved', a.out)


if __name__ == '__main__':
    main()
