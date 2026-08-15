"""
Stage 1 (v2) — build a state's SNAP QC dashboard under the TIERED, GUARDED
tuning procedure of methods/tuning_principles.md.

What differs from build_workbook.py (v1): v1 searched every threshold over the
2nd-to-98th percentile of the state's own data and kept whatever maximised error
dollars at raw precision >= 25% with >= 10 cases flagged, judged on the same
rows it searched. That is the winner's curse with no brake on it. v2 replaces
that single unconstrained search with three tiers and the guards that make the
chosen tier's numbers mean something:

  Tier 0  the shipped rules at shipped thresholds, re-filled against the state's
          own caseload. No selection, no outcomes. The default.
  Tier 1  re-filter and re-rank: rule text untouched, admission by
          Benjamini-Hochberg at FDR 10% against the state's own stratum base
          rate with n >= 30, ordering by the state's own 99% Wilson bound.
  Tier 2  thresholds move only inside +/-25% of the shipped value, only to cuts
          that partition the state's data differently, and only for rules that
          already cleared Tier 1.

Guards (all in tuning.py, all reported on the Tuning Audit sheet): time-based
split with the most recent fiscal year held out, hard n >= 30 support floor,
the within-rule search paid for by a Bonferroni-adjusted bound, Tier 2 refused
below 30 admitted rules, and ONE pre-declared holdout comparison per tier
rather than one per rule. The untuned arm is always computed beside the tuned
one, and the count of distinct threshold combinations evaluated is printed.

Sheets produced:
  Data               reconstructed state QC case data, with the tune/holdout split
  Tuning Audit       the split, the tier decision, and every admission test
  Dashboard          one tuning block per rule + PR chart      (hidden engine)
  Grid Search        bracket-bounded threshold search          (hidden engine)
  Summary            deployed vs national thresholds per rule, mixable via Include?
  Summary (National) delivery thresholds as-is, no tuning
  Error Cases        live list of rules catching errors + the cases they catch
  RuleFlags          case x rule hit matrices                  (hidden engine)

Pick the state with the SNAP_STATE environment variable (default WA); add new
states in states.py.  Run through make_state.py so the native checkboxes
get injected and the result is verified in Excel.
"""
import os
import re
import math
import shutil
import subprocess
import openpyxl
from openpyxl.styles import PatternFill, Font, Alignment, Border, Side
from openpyxl.utils import get_column_letter
from openpyxl.chart import ScatterChart, Reference, Series
from openpyxl.worksheet.datavalidation import DataValidation
from openpyxl.formatting.rule import FormulaRule
from openpyxl.workbook.defined_name import DefinedName
import numpy as np
import pandas as pd

# ══════════════════════════════════════════════════════════════════════════════
# CONFIG — state settings live in states.py; nothing here needs editing
# ══════════════════════════════════════════════════════════════════════════════
import states as STATE_REGISTRY
import tuning

PKG          = os.path.dirname(os.path.abspath(__file__))
BASE         = os.path.dirname(PKG)                # project root
BUILD_DIR    = os.path.join(PKG, '.build')         # stage-to-stage handoff files
_cfg         = STATE_REGISTRY.get(os.environ.get('SNAP_STATE', 'WA'))

STATE_NAME   = _cfg['name']
STATE_ABBR   = _cfg['abbr']
STATE_FIPS   = _cfg['fips']
FY_LABEL     = _cfg['fy_label']
ROLE_FILTER  = _cfg['role_filter']
# QC_FILES is v1's input; v2 never touches the .sav files, it reads the munged
# frame instead (see section 1), so the entry is kept only for v1 compatibility.
QC_FILES     = _cfg.get('qc_files')


def _find_repo(start):
    """Locate the snap_qc checkout by walking up and looking for the two files
    v2 actually needs: the munged frame and the miner's helpers."""
    cands = [os.environ['SNAP_REPO']] if os.environ.get('SNAP_REPO') else []
    d = start
    for _ in range(6):
        cands += [d, os.path.join(d, 'snap_qc')]
        d = os.path.dirname(d)
    for c in cands:
        if (os.path.isfile(os.path.join(c, 'reg_model_data.rds'))
                and os.path.isfile(os.path.join(c, 'rule_mining_helpers.R'))):
            return os.path.abspath(c)
    raise SystemExit('cannot find the snap_qc checkout (needs reg_model_data.rds, '
                     'written by the munging script, and rule_mining_helpers.R); '
                     'set SNAP_REPO')


REPO = _find_repo(PKG)


def _find_delivery(rel):
    """The delivery list, wherever the state keeps it. Falls back to the repo's
    tracked state_delivery_lists/ copy, which is the same public artifact."""
    base = os.path.basename(rel).replace('_core.csv', '.csv')
    for c in (os.path.join(BASE, rel), os.path.join(REPO, rel),
              os.path.join(PKG, rel), os.path.join(REPO, 'state_delivery_lists', base)):
        if os.path.isfile(c):
            return os.path.abspath(c)
    raise SystemExit(f'delivery list not found: {rel}')


DELIVERY_CSV = _find_delivery(_cfg['delivery_csv'])
STATE_DIR    = os.environ.get('SNAP_OUT_DIR') or os.path.join(
                   PKG, 'state_workbooks', STATE_ABBR)
os.makedirs(STATE_DIR, exist_ok=True)
CASES_CSV    = os.path.join(STATE_DIR, f'{STATE_ABBR.lower()}_cases.csv')
OUT          = os.path.join(STATE_DIR, f'snap_qc_dashboard_{STATE_ABBR}.xlsx')

MAX_ROW      = 3000            # Data-sheet formula range ceiling
MAXG         = 2500            # grid rows allocated per stratum
STRATA       = ['1', '2-3', '4+']
NSLOTS       = 4               # condition slots in the grid engine

# The tuning contract. Defaults live in tuning.TuningConfig (and are documented
# in methods/tuning_principles.md); states.py may override per deployment.
TCFG         = tuning.TuningConfig(**getattr(STATE_REGISTRY, 'TUNING', {}))
PREC_FLOOR   = TCFG.min_variant_precision   # applied to a BOUND, not raw precision
MIN_FLAGGED  = TCFG.min_support             # the non-negotiable support floor
RECALL_FLOOR = 0.0                          # v1 device; the objective is dollar-max
                                            # among qualifiers, so no floor is needed

# ══════════════════════════════════════════════════════════════════════════════
# 1. LOAD THE STATE DATASET from the munged modelling frame
#
# The delivery rules were mined on reg_model_data.rds, whose variables the
# munging script restores to their pre-QC-process values
# (1_data_munging_..._for_using_public_qc_data.R, correct_variables <- TRUE).
# Scoring them against anything else compares thresholds to a different scale:
# rebuilding these features from the raw .sav files in Python, as v1 does,
# reproduces the row universe exactly but leaves 21 of Washington's 114 core
# rules never firing at all. So this reads the munged frame instead, exported by
# export_state_frame.R with the miner's own prep_features() applied.
#
# The export is cached per state under .frames/. Set SNAP_REFRESH_FRAME=1 to
# rebuild it (needed after the munging script is re-run).
# ══════════════════════════════════════════════════════════════════════════════
FRAME_DIR = os.path.join(PKG, '.frames')
FRAME_CSV = os.path.join(FRAME_DIR, f'{STATE_ABBR.lower()}_frame.csv')
YEARS = [str(y) for y in _cfg.get('years', (2022, 2023, 2024))]


def _rscript():
    cands = ([os.environ['RSCRIPT']] if os.environ.get('RSCRIPT') else []) + [
        os.path.join('C:' + os.sep, 'Program Files', 'R', 'R-4.5.1', 'bin', 'Rscript.exe'),
        'Rscript']
    for c in cands:
        if os.path.isabs(c):
            if os.path.isfile(c):
                return c
        elif shutil.which(c):
            return shutil.which(c)
    raise SystemExit('Rscript not found; set RSCRIPT to its full path. The state '
                     'frame comes from reg_model_data.rds via export_state_frame.R.')


def _export_frame():
    cmd = [_rscript(), os.path.join(PKG, 'export_state_frame.R'),
           '--state', STATE_NAME, '--out', FRAME_CSV,
           '--years', ','.join(YEARS), '--repo', REPO]
    print('$ ' + ' '.join(cmd))
    if subprocess.run(cmd, cwd=REPO).returncode != 0:
        raise SystemExit('export_state_frame.R failed (see the output above)')


if os.environ.get('SNAP_REFRESH_FRAME') == '1' or not os.path.isfile(FRAME_CSV):
    _export_frame()
else:
    print(f'using cached frame {FRAME_CSV} (SNAP_REFRESH_FRAME=1 to rebuild)')

df = pd.read_csv(FRAME_CSV)
print(f'{STATE_ABBR}: {len(df)} rows from the munged frame, FY{"+".join(YEARS)}')

# The pipeline's error convention, carried over by the exporter: an error is
# over_threshold != 0, and the dashboard's sheets read a 0/1 `over_threshold`.
df['over_threshold'] = pd.to_numeric(df['is_error'], errors='coerce').fillna(0).astype(int)
df['total_error_amount'] = pd.to_numeric(df['total_error_amount'],
                                         errors='coerce').fillna(0).abs().round(0)
df['fiscal_year'] = pd.to_numeric(df['fiscal_year'], errors='coerce').astype(int)
df['hh_group'] = df['hh_group'].astype(str)
df['hh_size_raw'] = pd.to_numeric(df['hh_size_raw'], errors='coerce')
bad = df.hh_group.isin(['nan', 'None'])
if bad.any():
    print(f'dropping {int(bad.sum())} rows with no household-size stratum')
    df = df[~bad].reset_index(drop=True)
print(f'error rate: {df.over_threshold.mean():.3f} '
      f'({int(df.over_threshold.sum())} errors)')

# GUARD: the tune/holdout split is by fiscal year, most recent year held out.
# There is no random-split option: a random split leaks, because the same
# caseload composition appears on both sides. Carried into the workbook as a
# column so a reviewer can verify every number by filtering.
_tm, _hm, TUNE_YEARS, HOLD_YEARS = tuning.time_split(df, TCFG)
df['split'] = np.where(_hm, 'holdout', 'tune')
print(f'split: tune {TUNE_YEARS} ({int(_tm.sum())} cases) | '
      f'holdout {HOLD_YEARS} ({int(_hm.sum())} cases)')
df.to_csv(CASES_CSV, index=False)

# The Data-sheet formula ranges must cover every case: large states (CA, TX, FL)
# carry more public QC rows than the 3,000-row default.
MAX_ROW = max(MAX_ROW, len(df) + 101)

# ══════════════════════════════════════════════════════════════════════════════
# 2. PARSE THE DELIVERY RULES
# ══════════════════════════════════════════════════════════════════════════════
rules_df = pd.read_csv(DELIVERY_CSV)
rules_df = rules_df[rules_df['role'] == ROLE_FILTER].sort_values('rank').reset_index(drop=True)
COND_PAT = re.compile(r'([A-Za-z_][A-Za-z0-9_]*)\s*(>=|<=|>|<|==)\s*(-?[0-9.]+)')

RULES = []
for _, rr in rules_df.iterrows():
    conds = [{'var': v, 'op': op, 'thr': float(t)} for v, op, t in COND_PAT.findall(rr['rule'])]
    assert 1 <= len(conds) <= NSLOTS, rr['rule']
    RULES.append({'num': int(rr['rank']), 'hh': str(rr['hh']), 'conds': conds,
                  'prec_train': float(rr['precision_train']),
                  'prec_lcb': float(rr['precision_train_lcb']),
                  'engine': rr['engines'], 'frame': rr['mined_frames']})
print('rules implemented:', len(RULES))

RULE_VARS = sorted({c['var'] for r in RULES for c in r['conds']})
for v in RULE_VARS:
    assert v in df.columns, f'missing variable: {v}'

# snapped_grid steps by variable type (dollar 50, ratio 0.05, months/counts 1)
RATIO_VARS = {'rawben_rel_max', 'unc_rawben_rel_max', 'percent_abawd'}
UNIT_VARS  = {'months_since_cert_n', 'HH_size_n', 'count_divisible_by_100', 'cat_elig',
              'bbce_state_i', 'expedited_i', 'elderly_disabled_i', 'married',
              'children_i', 'homeless'}
def base_step(v):
    if v in RATIO_VARS: return 0.05
    if v in UNIT_VARS:  return 1.0
    return 50.0

# ══════════════════════════════════════════════════════════════════════════════
# 3. WORKBOOK SCAFFOLDING
# ══════════════════════════════════════════════════════════════════════════════
DCOLS = ['fiscal_year', 'split', 'hh_size_raw', 'hh_group'] + RULE_VARS + \
        ['over_threshold', 'total_error_amount']
LASTCOL = get_column_letter(len(DCOLS))

def dc(varname): return get_column_letter(DCOLS.index(varname) + 1)
def dr(varname):
    c = dc(varname)
    return f'Data!${c}$2:${c}${MAX_ROW}'

YELLOW     = PatternFill('solid', fgColor='FFFF99')
BLUE_LIGHT = PatternFill('solid', fgColor='BDD7EE')
BLUE_DARK  = PatternFill('solid', fgColor='2F5496')
GRAY       = PatternFill('solid', fgColor='F2F2F2')
GREEN      = PatternFill('solid', fgColor='E2EFDA')
WHITE      = PatternFill('solid', fgColor='FFFFFF')
FONT = 'Calibri'
def bold_font(size=11, color='000000'): return Font(name=FONT, bold=True, size=size, color=color)
center = Alignment(horizontal='center', vertical='center')
left   = Alignment(horizontal='left',   vertical='center')

def thin():
    s = Side(style='thin', color='BFBFBF')
    return Border(left=s, right=s, top=s, bottom=s)

def set_cell(ws, row, col, value=None, formula=None, fill=None, font=None,
             align=None, border=None, number_format=None):
    cell = ws.cell(row=row, column=col)
    cell.value = formula if formula else value
    if fill:   cell.fill = fill
    if font:   cell.font = font
    if align:  cell.alignment = align
    if border: cell.border = border
    if number_format: cell.number_format = number_format
    return cell

def merge(ws, r1, c1, r2, c2, value=None, fill=None, font=None, align=None):
    ws.merge_cells(start_row=r1, start_column=c1, end_row=r2, end_column=c2)
    cell = ws.cell(row=r1, column=c1)
    if value is not None: cell.value = value
    if fill:  cell.fill = fill
    if font:  cell.font = font
    if align: cell.alignment = align
    return cell

wb = openpyxl.Workbook()

# ── Data sheet ────────────────────────────────────────────────────────────────
ws_data = wb.create_sheet('Data')
dfx = df[DCOLS]
for ci, col in enumerate(DCOLS, 1):
    c = ws_data.cell(row=1, column=ci, value=col)
    c.fill = BLUE_DARK
    c.font = Font(name=FONT, bold=True, color='FFFFFF')
    c.alignment = center
for ri, row in enumerate(dfx.itertuples(index=False), 2):
    for ci, val in enumerate(row, 1):
        ws_data.cell(row=ri, column=ci, value=val)
ws_data.freeze_panes = 'A2'

# ══════════════════════════════════════════════════════════════════════════════
# 4. DASHBOARD — one tuning block per delivery rule
# ══════════════════════════════════════════════════════════════════════════════
ws = wb.active
ws.title = 'Dashboard'
ws.sheet_view.showGridLines = False
for col_letter, width in {'A':34,'B':8,'C':16,'D':14,'E':3,'F':18,'G':13,'H':13,'I':3,
                          'J':9,'K':7,'L':12,'M':12,'N':12,'O':12,'P':12,'Q':10}.items():
    ws.column_dimensions[col_letter].width = width

merge(ws,1,1,1,9,
      value=f'SNAP QC Inclusion Rule Tuner — {STATE_NAME} (FY2022–2024) — '
            f'{len(RULES)} core delivery rules',
      fill=BLUE_DARK, font=Font(name=FONT,bold=True,size=16,color='FFFFFF'), align=center)
ws.row_dimensions[1].height = 32

# stratum summary (referenced by every rule block)
merge(ws,3,1,3,9, value='Dataset Summary by Household Size',
      fill=BLUE_DARK, font=Font(name=FONT,bold=True,size=12,color='FFFFFF'), align=left)
for col, txt in [(1,'HH size'),(2,''),(3,'Cases'),(4,'Errors'),(6,'Error $'),(7,'Base error rate')]:
    if txt:
        set_cell(ws,4,col,txt, font=bold_font(10), fill=GRAY, align=center, border=thin())
S_ROWS = {}   # stratum -> summary row
for s, lbl in enumerate(STRATA):
    r = 5 + s
    S_ROWS[lbl] = r
    set_cell(ws,r,1,lbl, font=bold_font(), align=center, border=thin())
    set_cell(ws,r,3,formula=f'=COUNTIF({dr("hh_group")},"{lbl}")',
             fill=BLUE_LIGHT, align=center, border=thin(), number_format='#,##0')
    set_cell(ws,r,4,formula=f'=COUNTIFS({dr("hh_group")},"{lbl}",{dr("over_threshold")},1)',
             fill=BLUE_LIGHT, align=center, border=thin(), number_format='#,##0')
    set_cell(ws,r,6,formula=f'=SUMIFS({dr("total_error_amount")},{dr("hh_group")},"{lbl}",{dr("over_threshold")},1)',
             fill=BLUE_LIGHT, align=center, border=thin(), number_format='$#,##0')
    set_cell(ws,r,7,formula=f'=D{r}/C{r}', fill=BLUE_LIGHT, align=center,
             border=thin(), number_format='0.0%')

def rule_countifs(rule, thr_cells, add_error=False):
    parts = [f'{dr("hh_group")},"{rule["hh"]}"']
    for c, tc in zip(rule['conds'], thr_cells):
        parts.append(f'{dr(c["var"])},"{c["op"]}"&{tc}')
    if add_error:
        parts.append(f'{dr("over_threshold")},1')
    return f'COUNTIFS({",".join(parts)})'

def rule_sumifs(rule, thr_cells):
    parts = [f'{dr("total_error_amount")},{dr("hh_group")},"{rule["hh"]}",{dr("over_threshold")},1']
    for c, tc in zip(rule['conds'], thr_cells):
        parts.append(f'{dr(c["var"])},"{c["op"]}"&{tc}')
    return f'SUMIFS({",".join(parts)})'

METRIC_NAMES = ['n_flagged','errors caught','precision','recall (count)','dollar recall','lift']
METRIC_FMTS  = ['#,##0','#,##0','0.0%','0.0%','0.0%','0.00']
BLOCK_HEIGHT = 10
BASE_ROW = 9
summary_cells = []   # per rule: dict of metric cell refs

for bi, rule in enumerate(RULES):
    r0 = BASE_ROW + bi * BLOCK_HEIGHT
    merge(ws,r0,1,r0,9,
          value=(f'Rule {rule["num"]}  │  HH size {rule["hh"]}  │  {rule["engine"]}, '
                 f'{rule["frame"]}  │  train precision {rule["prec_train"]:.1%} '
                 f'(LCB {rule["prec_lcb"]:.1%})'),
          fill=BLUE_DARK, font=Font(name=FONT,bold=True,size=11,color='FFFFFF'), align=left)
    r1 = r0 + 1
    for col, txt in [(1,'Variable'),(2,'Op'),(3,'Threshold ← EDIT'),(4,'Original'),
                     (6,'Metric'),(7,'This state'),(8,'Train (natl.)')]:
        set_cell(ws,r1,col,txt, font=bold_font(10), fill=GRAY, align=center, border=thin())

    thr_cells = []
    for ci, cond in enumerate(rule['conds']):
        rc = r0 + 2 + ci
        set_cell(ws,rc,1,cond['var'], align=left, border=thin())
        set_cell(ws,rc,2,cond['op'], align=center, border=thin())
        set_cell(ws,rc,3,cond['thr'], fill=YELLOW, align=center, border=thin(), font=bold_font())
        set_cell(ws,rc,4,cond['thr'], align=center, border=thin(),
                 font=Font(name=FONT,color='808080'))
        thr_cells.append(f'C{rc}')

    srow = S_ROWS[rule['hh']]
    flag_f, err_f = rule_countifs(rule, thr_cells), rule_countifs(rule, thr_cells, True)
    dol_f = rule_sumifs(rule, thr_cells)
    formulas = [
        f'={flag_f}',
        f'={err_f}',
        f'=IFERROR({err_f}/{flag_f},0)',
        f'=IFERROR({err_f}/$D${srow},0)',
        f'=IFERROR({dol_f}/$F${srow},0)',
        f'=IFERROR(({err_f}/{flag_f})/($D${srow}/$C${srow}),0)',
    ]
    nat = [None, None, rule['prec_train'], None, None, None]
    cells = {}
    for mi, (mname, mfmt, mf) in enumerate(zip(METRIC_NAMES, METRIC_FMTS, formulas)):
        rm = r0 + 2 + mi
        set_cell(ws,rm,6,mname, font=bold_font(10), fill=GRAY, align=left, border=thin())
        set_cell(ws,rm,7,formula=mf, fill=BLUE_LIGHT, align=center, border=thin(),
                 number_format=mfmt)
        if nat[mi] is not None:
            set_cell(ws,rm,8,nat[mi], align=center, border=thin(), number_format=mfmt,
                     font=Font(name=FONT,color='808080'))
        cells[mname] = f'G{rm}'
    summary_cells.append(cells)

# rule comparison table (right side, below the chart)
SUMM_ROW = 26
merge(ws,SUMM_ROW,10,SUMM_ROW,17, value='Rule Comparison Summary',
      fill=BLUE_DARK, font=Font(name=FONT,bold=True,size=12,color='FFFFFF'), align=left)
for off, hdr in enumerate(['Rule','HH','Train prec.','Precision','Recall','$ Recall',
                           'n_flagged','Errors']):
    set_cell(ws,SUMM_ROW+1,10+off,hdr, font=bold_font(10), fill=GRAY, align=center, border=thin())
for bi, rule in enumerate(RULES):
    sr = SUMM_ROW + 2 + bi
    cc = summary_cells[bi]
    fill_r = BLUE_LIGHT if bi % 2 == 0 else WHITE
    vals = [(f'Rule {rule["num"]}', None), (rule['hh'], None),
            (rule['prec_train'], '0.0%'), (f'={cc["precision"]}', '0.0%'),
            (f'={cc["recall (count)"]}', '0.0%'), (f'={cc["dollar recall"]}', '0.0%'),
            (f'={cc["n_flagged"]}', '#,##0'), (f'={cc["errors caught"]}', '#,##0')]
    for off, (v, fmt) in enumerate(vals):
        c = set_cell(ws,sr,10+off,formula=v if isinstance(v,str) and v.startswith('=') else None,
                     value=None if isinstance(v,str) and v.startswith('=') else v,
                     fill=fill_r, align=center, border=thin())
        if fmt: c.number_format = fmt

# precision-recall chart (79 points; labels off — identify via the table)
chart = ScatterChart()
chart.title = 'Precision vs Recall (tuned thresholds)'
chart.x_axis.title = 'Recall'; chart.y_axis.title = 'Precision'
chart.x_axis.numFmt = '0%';   chart.y_axis.numFmt = '0%'
chart.width, chart.height = 15, 12
for bi, rule in enumerate(RULES):
    rec_c, prec_c = summary_cells[bi]['recall (count)'], summary_cells[bi]['precision']
    xv = Reference(ws, min_col=7, min_row=int(rec_c[1:]),  max_row=int(rec_c[1:]))
    yv = Reference(ws, min_col=7, min_row=int(prec_c[1:]), max_row=int(prec_c[1:]))
    s = Series(yv, xv, title=f'Rule {rule["num"]}')
    s.marker.symbol, s.marker.size = 'circle', 7
    s.graphicalProperties.line.noFill = True
    chart.series.append(s)
chart.legend = None
ws.add_chart(chart, 'J3')
ws.freeze_panes = 'A9'

# ══════════════════════════════════════════════════════════════════════════════
# 5. GRID SEARCH SHEET (same engine as the Virginia workbook)
# ══════════════════════════════════════════════════════════════════════════════
DATA2D = f'Data!$A$2:${LASTCOL}${MAX_ROW}'
HH     = dr('hh_group')
OVER   = dr('over_threshold')
ERRD   = dr('total_error_amount')

HELP0 = 100
GRID0 = HELP0 + 3*6 + 2
def h0(s):  return HELP0 + s*6
def gs(s):  return GRID0 + s*(MAXG + 3)
def rng(s, col):
    top = gs(s) + 2
    return f'${col}${top}:${col}${top + MAXG - 1}'
def sref(i, col): return f'${col}${7 + i}'

def dyn_pairs(lbl, thr_refs):
    parts = []
    for i in range(NSLOTS):
        parts.append(f'INDEX({DATA2D},0,{sref(i,"AF")})')
        parts.append(f'IF({sref(i,"A")}="","{lbl}",{sref(i,"B")}&{thr_refs[i]})')
    return ','.join(parts)

# ── Search bracket, per condition ─────────────────────────────────────────────
# The interactive sheet searches the SAME bracket the Tier-2 engine does: each
# threshold is scaled by the factor grid (fine for rules with <= 3 conditions,
# coarse beyond that, because the combinations grow exponentially in the
# condition count), never the 2nd-to-98th percentile of the state's own data as
# in v1. Structure is frozen; only thresholds move.
#
# A condition is collapsed to a single point (no search) when scaling it is
# meaningless: a shipped threshold of exactly 0, or a variable with <= 5 distinct
# observed values in the rule's own stratum (binary indicators, small counts).
# That mirrors the "low-cardinality variables (single cut each)" rule in
# state_threshold_gridsearch_v2.R.
NOSEARCH_MAX_LEVELS = 5

def condition_bracket(rule, c):
    fac = TCFG.factors_for(len(rule['conds']))
    thr = float(c['thr'])
    obs = df[df.hh_group == rule['hh']][c['var']].dropna().values.astype(float)
    levels = np.unique(obs[np.isfinite(obs)]).size
    if thr == 0.0 or levels <= NOSEARCH_MAX_LEVELS:
        return (thr, thr, max(base_step(c['var']), 1e-9), 1)
    lo, hi = round(thr * min(fac), 6), round(thr * max(fac), 6)
    step = (hi - lo) / (len(fac) - 1)
    if c['var'] in UNIT_VARS:                 # integer-valued: keep cuts whole
        step = max(1.0, round(step))
        lo, hi = math.floor(lo), math.ceil(hi)
    step = round(step, 6)
    npts = int(round((hi - lo) / step)) + 1 if step > 0 else 1
    return (lo, hi, step, npts)

for rule in RULES:
    rule['bracket'] = [condition_bracket(rule, c) for c in rule['conds']]
    combos = int(np.prod([b[3] for b in rule['bracket']]))
    assert combos <= MAXG, (f'rule {rule["num"]}: {combos} bracket combinations '
                            f'exceeds the {MAXG}-row grid capacity')
print('bracket combinations per rule: '
      f'min {min(int(np.prod([b[3] for b in r["bracket"]])) for r in RULES)}, '
      f'max {max(int(np.prod([b[3] for b in r["bracket"]])) for r in RULES)}')

ws_g = wb.create_sheet('Grid Search', 1)
ws_g.sheet_view.showGridLines = False
for col_letter, width in {'A':26,'B':13,'C':13,'D':13,'E':13,'F':12,'G':12,
                          'H':13,'I':13,'J':13,'K':12,'L':10,'M':8}.items():
    ws_g.column_dimensions[col_letter].width = width
for c in ('Y','Z','AA','AB','AC','AD','AE','AF','AG','AH','AI','AJ','L','M'):
    ws_g.column_dimensions[c].hidden = True

merge(ws_g,1,1,1,13, value=f'Grid Search — Optimize One Delivery Rule ({STATE_NAME})',
      fill=BLUE_DARK, font=Font(name=FONT,bold=True,size=16,color='FFFFFF'), align=center)
ws_g.row_dimensions[1].height = 32

# hidden preset tables: names in AA, terms in AB:AE; dropdown list in Y
NP = len(RULES)
set_cell(ws_g,1,25,'Custom')                          # Y1
for p, rule in enumerate(RULES):
    pname = f'Rule {rule["num"]} (HH {rule["hh"]})'
    set_cell(ws_g,2+p,27,pname)                       # AA
    set_cell(ws_g,2+p,25,pname)                       # Y (dropdown source)
    for i in range(NSLOTS):
        rr = 2 + p*NSLOTS + i
        if i < len(rule['conds']):
            c = rule['conds'][i]
            lo, hi, step, _ = rule['bracket'][i]
            set_cell(ws_g,rr,28,c['var']); set_cell(ws_g,rr,29,c['op'])
            set_cell(ws_g,rr,30,step); set_cell(ws_g,rr,31,c['thr'])
            set_cell(ws_g,rr,33,lo);    set_cell(ws_g,rr,34,hi)   # AG/AH: the bracket

merge(ws_g,4,1,4,13,
      value='Step 1 — Pick a rule preset, or pick "Custom" and fill the custom block on the right',
      fill=BLUE_DARK, font=Font(name=FONT,bold=True,size=12,color='FFFFFF'), align=left)
set_cell(ws_g,5,1,'Rule preset', font=bold_font(), fill=GRAY, align=left)
set_cell(ws_g,5,2,f'Rule {RULES[0]["num"]} (HH {RULES[0]["hh"]})', fill=YELLOW,
         align=center, border=thin(), font=bold_font(), number_format='@')
set_cell(ws_g,5,4,'← delivery rules ranked by train precision LCB, or "Custom"',
         font=Font(name=FONT,size=9,color='808080'), align=left)
merge(ws_g,5,8,5,11, value='Custom rule (used when preset = Custom)',
      fill=GRAY, font=bold_font(10), align=center)
for col, txt in [(1,'Variable'),(2,'Op'),(3,'Step'),(4,'Original threshold'),
                 (8,'Variable'),(9,'Op'),(10,'Step'),(11,'Original')]:
    set_cell(ws_g,6,col,txt, font=bold_font(10), fill=GRAY, align=center, border=thin())

PLOOK = f'(MATCH($B$5,$AA$2:$AA${1+NP},0)-1)*{NSLOTS}'
PEND = 1 + NP*NSLOTS
for i in range(NSLOTS):
    r = 7 + i
    vlook = f'INDEX($AB$2:$AB${PEND},{PLOOK}+{i+1})'
    set_cell(ws_g,r,1,
             formula=f'=IF($B$5="Custom",IF($H{r}="","",$H{r}),IF({vlook}=0,"",{vlook}))',
             fill=BLUE_LIGHT, align=left, border=thin())
    for cc, pcol, ccol in [(2,'AC','I'),(3,'AD','J'),(4,'AE','K')]:
        set_cell(ws_g,r,cc,
                 formula=(f'=IF($A{r}="","",IF($B$5="Custom",${ccol}{r},'
                          f'INDEX(${pcol}$2:${pcol}${PEND},{PLOOK}+{i+1})))'),
                 fill=BLUE_LIGHT, align=center, border=thin())
    set_cell(ws_g,r,32,formula=f'=IF($A{r}="",3,MATCH($A{r},Data!$A$1:${LASTCOL}$1,0))')
    # AI/AJ: this slot's search bracket. Presets carry the bracket computed by
    # condition_bracket(); a Custom rule gets the same +/-25% window on its own
    # original value, so the interactive sheet can never search wider than Tier 2.
    for cc, pcol in ((35, 'AG'), (36, 'AH')):
        fpc = TCFG.factors_fine[0] if cc == 35 else TCFG.factors_fine[-1]
        set_cell(ws_g,r,cc,
                 formula=(f'=IF($A{r}="","",IF($B$5="Custom",ROUND($K{r}*{fpc},6),'
                          f'INDEX(${pcol}$2:${pcol}${PEND},{PLOOK}+{i+1})))'))
    if i < len(RULES[0]['conds']):
        c = RULES[0]['conds'][i]
        set_cell(ws_g,r,8, c['var'], fill=YELLOW, align=left,   border=thin(), font=bold_font())
        set_cell(ws_g,r,9, c['op'],  fill=YELLOW, align=center, border=thin(), font=bold_font())
        set_cell(ws_g,r,10,RULES[0]['bracket'][i][2], fill=YELLOW, align=center,
                 border=thin(), font=bold_font())
        set_cell(ws_g,r,11,c['thr'], fill=YELLOW, align=center, border=thin(), font=bold_font())
    else:
        for cc in (8,9,10,11):
            set_cell(ws_g,r,cc,None, fill=YELLOW, align=center, border=thin(), font=bold_font())

for i, v in enumerate(RULE_VARS, 2):
    set_cell(ws_g,i,26,v)                             # Z: variable dropdown source
dv_preset = DataValidation(type='list', formula1=f'=$Y$1:$Y${1+NP}', allow_blank=False)
ws_g.add_data_validation(dv_preset); dv_preset.add('B5')
dv_var = DataValidation(type='list', formula1=f'=$Z$2:$Z${len(RULE_VARS)+1}', allow_blank=True)
ws_g.add_data_validation(dv_var); dv_var.add('H7:H10')
dv_op = DataValidation(type='list', formula1='">=,<=,>,<,="', allow_blank=True)
ws_g.add_data_validation(dv_op); dv_op.add('I7:I10')

merge(ws_g,11,1,11,13, value='Step 2 — Settings',
      fill=BLUE_DARK, font=Font(name=FONT,bold=True,size=12,color='FFFFFF'), align=left)
set_cell(ws_g,12,1,'Precision floor (on the BOUND)', font=bold_font(), fill=GRAY, align=left)
set_cell(ws_g,12,2,PREC_FLOOR, fill=YELLOW, align=center, border=thin(), font=bold_font(),
         number_format='0.0%')
set_cell(ws_g,12,4,'← applied to the lower confidence bound below, not to raw precision: '
                   'raw precision on the rows you searched is optimistic by construction',
         font=Font(name=FONT,size=9,color='808080'), align=left)
set_cell(ws_g,13,1,'Recall basis (maximized)', font=bold_font(), fill=GRAY, align=left)
set_cell(ws_g,13,2,'dollar', fill=YELLOW, align=center, border=thin(), font=bold_font())
set_cell(ws_g,13,4,'← what to maximize among qualifying combinations; fix this before you look, '
                   'never per rule',
         font=Font(name=FONT,size=9,color='808080'), align=left)
set_cell(ws_g,14,1,'Search alpha (per rule)', font=bold_font(), fill=GRAY, align=left)
set_cell(ws_g,14,2,TCFG.variant_gate_alpha, fill=YELLOW, align=center, border=thin(),
         font=bold_font(), number_format='0.00')
set_cell(ws_g,14,4,'← the bound\'s confidence is set from this divided by the number of '
                   'combinations searched, so a wider search must clear a higher bar',
         font=Font(name=FONT,size=9,color='808080'), align=left)
set_cell(ws_g,12,8,'Min flagged', font=bold_font(), fill=GRAY, align=left)
set_cell(ws_g,12,9,MIN_FLAGGED, fill=YELLOW, align=center, border=thin(), font=bold_font(),
         number_format='0')
set_cell(ws_g,12,10,'← hard support floor; below this a combination is too noisy to trust '
                    '(findings §9: at n >= 5 state-scale tuning collapsed)',
         font=Font(name=FONT,size=9,color='808080'), align=left)
set_cell(ws_g,13,8,'Recall floor', font=bold_font(), fill=GRAY, align=left)
set_cell(ws_g,13,9,RECALL_FLOOR, fill=YELLOW, align=center, border=thin(), font=bold_font(),
         number_format='0.0%')
set_cell(ws_g,13,10,'← optional extra floor on the share of error $ / cases caught',
         font=Font(name=FONT,size=9,color='808080'), align=left)
set_cell(ws_g,14,8,'Search bracket', font=bold_font(), fill=GRAY, align=left)
set_cell(ws_g,14,9,f'{TCFG.factors_fine[0]:g}x - {TCFG.factors_fine[-1]:g}x',
         fill=GRAY, align=center, border=thin(), font=bold_font())
set_cell(ws_g,14,10,'← fixed: every threshold is searched only inside this multiple of its '
                    'delivered value (coarser for rules with 4+ conditions). Structure never moves.',
         font=Font(name=FONT,size=9,color='808080'), align=left)
dv_floor = DataValidation(type='decimal', operator='between', formula1='0', formula2='1')
ws_g.add_data_validation(dv_floor); dv_floor.add('B12')
ws_g.add_data_validation(dv_floor2 := DataValidation(type='decimal', operator='between',
                                                     formula1='0', formula2='1'))
dv_floor2.add('I13')
dv_basis = DataValidation(type='list', formula1='"dollar,count"', allow_blank=False)
ws_g.add_data_validation(dv_basis); dv_basis.add('B13')
ws_g.add_data_validation(dv_alpha := DataValidation(type='decimal', operator='between',
                                                    formula1='0.0001', formula2='0.5'))
dv_alpha.add('B14')
dv_minf = DataValidation(type='whole', operator='greaterThanOrEqual', formula1='0')
ws_g.add_data_validation(dv_minf); dv_minf.add('I12')

merge(ws_g,15,1,15,13, value='Stratum Overview',
      fill=BLUE_DARK, font=Font(name=FONT,bold=True,size=12,color='FFFFFF'), align=left)
for col, txt in [(1,'HH size'),(2,'Cases'),(3,'Errors'),(4,'Error $'),(5,'Combinations')]:
    set_cell(ws_g,16,col,txt, font=bold_font(10), fill=GRAY, align=center, border=thin())
for s, lbl in enumerate(STRATA):
    r = 17 + s
    set_cell(ws_g,r,1,lbl, font=bold_font(), align=center, border=thin())
    set_cell(ws_g,r,2,formula=f'=COUNTIF({HH},"{lbl}")', align=center, border=thin(),
             number_format='#,##0', fill=BLUE_LIGHT)
    set_cell(ws_g,r,3,formula=f'=COUNTIFS({HH},"{lbl}",{OVER},1)', align=center,
             border=thin(), number_format='#,##0', fill=BLUE_LIGHT)
    set_cell(ws_g,r,4,formula=f'=SUMIFS({ERRD},{HH},"{lbl}",{OVER},1)', align=center,
             border=thin(), number_format='$#,##0', fill=BLUE_LIGHT)
    set_cell(ws_g,r,5,formula=f'=IF($A$7="","",$H${h0(s)})', align=center,
             border=thin(), number_format='#,##0', fill=BLUE_LIGHT)

F0 = 31
def fs(s): return F0 + s*18

merge(ws_g,21,1,21,13,
      value='Best per Stratum — max reach among combinations whose precision BOUND clears the '
            'floor and which flag at least the support minimum (vs delivered thresholds)',
      fill=BLUE_DARK, font=Font(name=FONT,bold=True,size=12,color='FFFFFF'), align=left)
for col, txt in [(1,'HH size'),(2,'thr 1'),(3,'thr 2'),(4,'thr 3'),(5,'thr 4'),
                 (6,'precision'),(7,'recall'),(8,'dollar recall'),(9,'n_flagged'),
                 (10,'errors'),(11,'workload %'),(12,'precision bound')]:
    set_cell(ws_g,22,col,txt, font=bold_font(10), fill=GRAY, align=center, border=thin())
BEST_FMTS = {6:'0.0%',7:'0.0%',8:'0.0%',9:'#,##0',10:'#,##0'}
for s, lbl in enumerate(STRATA):
    r = 23 + s
    M = f'$M${fs(s)+2}'
    set_cell(ws_g,r,1,f'{lbl} (best)', font=bold_font(), align=center, border=thin())
    for ci, col in enumerate('ABCD'):
        set_cell(ws_g,r,2+ci,
                 formula=f'=IF({M}="","",IF({sref(ci,"A")}="","",INDEX({rng(s,col)},{M})))',
                 fill=GREEN, align=center, border=thin(), font=bold_font())
    for cnum, gcol in [(6,'H'),(7,'I'),(8,'J'),(9,'E'),(10,'F')]:
        set_cell(ws_g,r,cnum,formula=f'=IF({M}="","",INDEX({rng(s,gcol)},{M}))',
                 fill=BLUE_LIGHT, align=center, border=thin(), number_format=BEST_FMTS[cnum])
    set_cell(ws_g,r,11,formula=f'=IF({M}="","",INDEX({rng(s,"E")},{M})/$B${17+s})',
             fill=BLUE_LIGHT, align=center, border=thin(), number_format='0.0%')
    set_cell(ws_g,r,12,formula=f'=IF({M}="","",INDEX({rng(s,"L")},{M}))',
             fill=BLUE_LIGHT, align=center, border=thin(), number_format='0.000')

ORIG_REFS = [sref(i,'D') for i in range(NSLOTS)]
for s, lbl in enumerate(STRATA):
    r = 26 + s
    gray = Font(name=FONT, color='808080')
    set_cell(ws_g,r,1,f'{lbl} (original)', align=center, border=thin(), font=gray)
    for i in range(NSLOTS):
        set_cell(ws_g,r,2+i,formula=f'=IF({sref(i,"A")}="","",{sref(i,"D")})',
                 align=center, border=thin(), font=gray)
    pairs = dyn_pairs(lbl, ORIG_REFS)
    set_cell(ws_g,r,9, formula=f'=IF($A$7="","",COUNTIFS({HH},"{lbl}",{pairs}))',
             align=center, border=thin(), number_format='#,##0', font=gray)
    set_cell(ws_g,r,10,formula=f'=IF($A$7="","",COUNTIFS({HH},"{lbl}",{pairs},{OVER},1))',
             align=center, border=thin(), number_format='#,##0', font=gray)
    set_cell(ws_g,r,12,formula=f'=IF($A$7="","",SUMIFS({ERRD},{HH},"{lbl}",{OVER},1,{pairs}))')
    set_cell(ws_g,r,6, formula=f'=IF($I{r}="","",IF($I{r}=0,0,$J{r}/$I{r}))',
             align=center, border=thin(), number_format='0.0%', font=gray)
    set_cell(ws_g,r,7, formula=f'=IF($J{r}="","",IF($C${17+s}=0,0,$J{r}/$C${17+s}))',
             align=center, border=thin(), number_format='0.0%', font=gray)
    set_cell(ws_g,r,8, formula=f'=IF($L{r}="","",IF($D${17+s}=0,0,$L{r}/$D${17+s}))',
             align=center, border=thin(), number_format='0.0%', font=gray)
    set_cell(ws_g,r,11,formula=f'=IF($I{r}="","",$I{r}/$B${17+s})',
             align=center, border=thin(), number_format='0.0%', font=gray)

merge(ws_g,30,1,30,13,
      value='Feasible Combinations — top 15 by reach per stratum (precision bound ≥ floor, '
            'support ≥ minimum)',
      fill=BLUE_DARK, font=Font(name=FONT,bold=True,size=12,color='FFFFFF'), align=left)
FEAS_HEADS = [(1,'rank'),(2,'thr 1'),(3,'thr 2'),(4,'thr 3'),(5,'thr 4'),
              (6,'precision'),(7,'recall'),(8,'dollar recall'),(9,'n_flagged'),(10,'errors')]
for s, lbl in enumerate(STRATA):
    f0 = fs(s)
    merge(ws_g,f0,1,f0,13, value=f'HH size {lbl}',
          fill=BLUE_DARK, font=Font(name=FONT,bold=True,size=10,color='FFFFFF'), align=left)
    for col, txt in FEAS_HEADS:
        set_cell(ws_g,f0+1,col,txt, font=bold_font(9), fill=GRAY, align=center, border=thin())
    for k in range(1, 16):
        r = f0 + 1 + k
        set_cell(ws_g,r,1,k, align=center, border=thin())
        set_cell(ws_g,r,13,
                 formula=f'=IFERROR(MATCH(LARGE({rng(s,"K")},{k}),{rng(s,"K")},0),"")')
        for ci, col in enumerate('ABCD'):
            set_cell(ws_g,r,2+ci,
                     formula=f'=IF($M{r}="","",IF({sref(ci,"A")}="","",INDEX({rng(s,col)},$M{r})))',
                     align=center, border=thin())
        for cnum, gcol, fmt in [(6,'H','0.0%'),(7,'I','0.0%'),(8,'J','0.0%'),
                                (9,'E','#,##0'),(10,'F','#,##0')]:
            set_cell(ws_g,r,cnum,
                     formula=f'=IF($M{r}="","",INDEX({rng(s,gcol)},$M{r}))',
                     align=center, border=thin(), number_format=fmt)

NOTES = [
    f'This sheet is an EXPLORATION aid. The authoritative Tier-2 search is the one reported on Tuning Audit: it runs on the tuning years only ({", ".join(str(y) for y in TUNE_YEARS)}), drops threshold cuts that partition the data identically, and is judged on the held-out year ({", ".join(str(y) for y in HOLD_YEARS)}). This sheet searches all years at once.',
    f'Each threshold is searched only inside {TCFG.factors_fine[0]:g}x-{TCFG.factors_fine[-1]:g}x its delivered value ({TCFG.factors_coarse[0]:g}x-{TCFG.factors_coarse[-1]:g}x for rules with more than {TCFG.fine_max_conds} conditions). Variables, operators, condition count and stratum never move: this perturbs an already-validated rule, it does not search for a new one.',
    'The stratum matching the rule\'s "HH" tag is the one the delivery list targets.',
    f'Qualification is the precision BOUND (column L, hidden) at a confidence set from the search alpha divided by the number of combinations searched, plus n_flagged >= the support floor. Raw precision on searched rows is optimistic by construction and is not the gate.',
    f'Best row = most reach (chosen basis) among qualifying combinations; ties by grid order. The delivered thresholds are always in the grid, so "leave it alone" is always available.',
    f'Capacity: up to {MAXG:,} combinations per stratum; the bracket keeps every rule well inside it.',
    f'The full scored grid lives in the blocks from row {GRID0} down (helper rows 100-119 are hidden).',
]
for i, txt in enumerate(NOTES):
    set_cell(ws_g,85+i,1,txt, font=Font(name=FONT,size=9,color='808080'), align=left)

for s, lbl in enumerate(STRATA):
    hh0 = h0(s)
    set_cell(ws_g,hh0,8,formula=f'=F{hh0+1}*F{hh0+2}*F{hh0+3}*F{hh0+4}')
    # the multiplicity-adjusted confidence z for this stratum's search: one-sided
    # alpha / (combinations searched). A wider search must clear a higher bar.
    # post-2007 functions must carry the _xlfn. prefix when written straight
    # into the XML, or Excel renders #NAME?
    set_cell(ws_g,hh0,9,
             formula=f'=IF($H${hh0}<=0,3,_xlfn.NORM.S.INV(1-MIN(0.5,$B$14/MAX($H${hh0},1))))')
    for i in range(NSLOTS):
        hr = hh0 + 1 + i
        V, S = sref(i,'A'), sref(i,'C')
        # bracket bounds, NOT data percentiles: the grid is the delivered
        # threshold's own +/-25% window (AI/AJ), so the interactive search can
        # never roam wider than the Tier-2 engine allows.
        ws_g.cell(row=hr, column=2).value = f'=IF({V}="","",{sref(i,"AI")})'
        ws_g.cell(row=hr, column=3).value = f'=IF({V}="","",{sref(i,"AJ")})'
        ws_g.cell(row=hr, column=4).value = f'=IF({V}="","",B{hr})'
        ws_g.cell(row=hr, column=5).value = f'=IF({V}="","",MAX(C{hr},D{hr}))'
        ws_g.cell(row=hr, column=6).value = (
            f'=IF({V}="",1,IF({S}<=0,1,ROUND((E{hr}-D{hr})/{S},0)+1))')
        ws_g.cell(row=hr, column=7).value = ('=1' if i == 0 else f'=F{hr-1}*G{hr-1}')
for rr in range(HELP0, GRID0):
    ws_g.row_dimensions[rr].hidden = True

for s, lbl in enumerate(STRATA):
    g0 = gs(s); data0 = g0 + 2; hh0 = h0(s)
    merge(ws_g,g0,1,g0,13, value=f'GRID — HH size {lbl}',
          fill=BLUE_DARK, font=Font(name=FONT,bold=True,size=10,color='FFFFFF'), align=left)
    for ci, htxt in enumerate(['thr 1','thr 2','thr 3','thr 4','n_flagged','errors_caught',
                               'err_dollars','precision','recall_count','dollar_recall',
                               'sortkey','prec_lcb'], 1):
        set_cell(ws_g,g0+1,ci,htxt, font=bold_font(9), fill=GRAY, align=center)
    for r in range(data0, data0 + MAXG):
        for i in range(NSLOTS):
            hr = hh0 + 1 + i
            V, S = sref(i,'A'), sref(i,'C')
            ws_g.cell(row=r, column=1+i).value = (
                f'=IF($A$7="","",IF($H${hh0}>{MAXG},"",IF(ROW()-{g0+1}>$H${hh0},"",'
                f'IF({V}="","",ROUND($D${hr}+{S}*MOD(INT((ROW()-{data0})/$G${hr}),$F${hr}),6)))))')
        pairs = dyn_pairs(lbl, [f'$A{r}', f'$B{r}', f'$C{r}', f'$D{r}'])
        ws_g.cell(row=r, column=5).value  = f'=IF($A{r}="","",COUNTIFS({HH},"{lbl}",{pairs}))'
        ws_g.cell(row=r, column=6).value  = f'=IF($A{r}="","",COUNTIFS({HH},"{lbl}",{pairs},{OVER},1))'
        ws_g.cell(row=r, column=7).value  = f'=IF($A{r}="","",SUMIFS({ERRD},{HH},"{lbl}",{OVER},1,{pairs}))'
        ws_g.cell(row=r, column=8).value  = f'=IF($E{r}="","",IF($E{r}=0,0,$F{r}/$E{r}))'
        ws_g.cell(row=r, column=9).value  = f'=IF($E{r}="","",IF($C${17+s}=0,0,$F{r}/$C${17+s}))'
        ws_g.cell(row=r, column=10).value = f'=IF($E{r}="","",IF($D${17+s}=0,0,$G{r}/$D${17+s}))'
        # one-sided Wilson lower bound on this combination's precision, at the
        # multiplicity-adjusted z in $I$hh0: what the combination is worth once
        # you have paid for having searched the whole bracket
        Z = f'$I${hh0}'
        ws_g.cell(row=r, column=12).value = (
            f'=IF($E{r}="","",IF($E{r}=0,0,'
            f'MAX(0,($H{r}+{Z}^2/(2*$E{r})-{Z}*SQRT($H{r}*(1-$H{r})/$E{r}'
            f'+{Z}^2/(4*$E{r}^2)))/(1+{Z}^2/$E{r}))))')
        # qualification is the BOUND plus the support floor, never raw precision
        ws_g.cell(row=r, column=11).value = (
            f'=IF($H{r}="","",IF(AND($L{r}>=$B$12,$E{r}>=$I$12,'
            f'IF($B$13="dollar",$J{r},$I{r})>=$I$13),'
            f'IF($B$13="dollar",$J{r},$I{r})+({MAXG}-(ROW()-{g0+1}))*0.0000000001,""))')
        for cc in (8, 9, 10, 11, 12):
            ws_g.cell(row=r, column=cc).number_format = '0.000'

ws_g.freeze_panes = 'A21'
wb.calculation.fullCalcOnLoad = True

# ══════════════════════════════════════════════════════════════════════════════
# 6. THE TIERED TUNING RUN, then the SUMMARY SHEET
#
# All of the statistics live in tuning.py (methods/tuning_principles.md). This
# section only turns its result into sheets. v1 searched here, unconstrained and
# in-sample; nothing in this file selects a threshold any more.
# ══════════════════════════════════════════════════════════════════════════════
print('\n' + '=' * 78)
RES = tuning.run(df, RULES, TCFG)
print('=' * 78 + '\n')

TAB = RES.rules_table.set_index('rule_idx')

def py_score(dd, conds, thresholds):
    is_err = (dd['over_threshold'] == 1).values
    ed = np.where(is_err, dd['total_error_amount'].fillna(0).values, 0)
    m = np.ones(len(dd), bool)
    for c, t in zip(conds, thresholds):
        xv = dd[c['var']].values.astype(float)
        if   c['op'] == '>=': cm = xv >= t
        elif c['op'] == '>':  cm = xv > t
        elif c['op'] == '<=': cm = xv <= t
        else:                 cm = xv < t
        m &= np.where(np.isnan(xv), False, cm)
    n, tp = int(m.sum()), int((m & is_err).sum())
    return {'prec': tp / n if n else 0.0, 'rec': tp / max(is_err.sum(), 1),
            'drec': ed[m].sum() / max(ed.sum(), 1e-9), 'n': n, 'tp': tp,
            'dollars': ed[m].sum()}

def why_not(ri):
    """Plain-language reason a shipped rule is not in the deployed list. Tier 0
    deploys every rule that fits the budget, so admission is not the reason
    there; at Tier 1 and above the admission tests come first."""
    r = TAB.loc[ri]
    budget_msg = (f'not deployed: adding it at its rank would exceed the '
                  f'{TCFG.budget:.0%} review budget')
    if RES.tier == 0:
        return budget_msg
    if not r.support_ok:
        return (f'not deployed: flags {int(r.tune_n)} tuning-year cases, below the '
                f'{TCFG.min_support}-case support floor')
    if not r.admitted:
        return (f'not deployed: {int(r.tune_k)}/{int(r.tune_n)} errors is not distinguishable '
                f'from the {r.base_rate:.1%} base rate at FDR {TCFG.fdr_alpha:.0%}')
    return budget_msg

summary_rows = []
rule_best_thr = {}   # rule index -> deployed thresholds (None if not deployed)
for ri, rule in enumerate(RULES):
    dd = df[df['hh_group'] == rule['hh']]
    shipped = tuple(float(c['thr']) for c in rule['conds'])
    thr = RES.deployed_thresholds[ri]
    on = bool(RES.deployed[ri])
    rule_best_thr[ri] = thr if on else None
    dep_cond = (' & '.join(f'{c["var"]} {c["op"]} {t:g}' for c, t in zip(rule['conds'], thr))
                + ('' if tuple(thr) == shipped else '   [threshold moved from '
                   + ', '.join(f'{s:g}' for s in shipped) + ']')) if on else why_not(ri)
    nat_cond = ' & '.join(f'{c["var"]} {c["op"]} {c["thr"]:g}' for c in rule['conds'])
    summary_rows.append((rule, 'deployed', py_score(dd, rule['conds'], thr) if on else None,
                         dep_cond))
    summary_rows.append((rule, 'national', py_score(dd, rule['conds'], shipped), nat_cond))

# ── union of all rules (a case is flagged if ANY rule flags it on its stratum) ─
def rule_mask_full(rule, thresholds):
    m = (df['hh_group'] == rule['hh']).values.copy()
    for c, t in zip(rule['conds'], thresholds):
        xv = df[c['var']].values.astype(float)
        if   c['op'] == '>=': cm = xv >= t
        elif c['op'] == '>':  cm = xv > t
        elif c['op'] == '<=': cm = xv <= t
        else:                 cm = xv < t
        m &= np.where(np.isnan(xv), False, cm)
    return m

is_err_all = (df['over_threshold'] == 1).values
ed_all = np.where(is_err_all, df['total_error_amount'].fillna(0).values, 0)
opt_masks  = [rule_mask_full(r, rule_best_thr[ri]) if rule_best_thr[ri] is not None else None
              for ri, r in enumerate(RULES)]
orig_masks = [rule_mask_full(r, [c['thr'] for c in r['conds']]) for r in RULES]

# ── hidden RuleFlags sheet: case x rule 0/1 matrices + live union columns ─────
# row 1 rule nums | row 2 selected (from Summary Include? col) | row 3 has-optimal
# row 4 target stratum | rows FLAG0.. one per Data case (only 1s written)
NR, NDATA = len(RULES), len(df)
RULE_ROW0 = 15                 # Summary row of the first rule's "deployed" line
NAT_ROW0  = 11                 # Summary (National) row of the first rule (delivery-rank order)
FLAG0 = 5

# Individual rules are listed by expected error $ per flagged case, best first;
# rules with no floor-compliant combination sink to the bottom.
def _exp_per_case(j):
    sc = summary_rows[2*j][2]
    return (sc['dollars'] / sc['n']) if (sc and sc['n']) else None
rule_order = sorted(range(NR), key=lambda j: (0 if _exp_per_case(j) is not None else 1,
                                              -(_exp_per_case(j) or 0), RULES[j]['num']))
rule_row = {j: RULE_ROW0 + 2*pos for pos, j in enumerate(rule_order)}   # rule idx -> Summary row
ws_f = wb.create_sheet('RuleFlags')
ws_f.sheet_state = 'hidden'
fcol_opt  = lambda j: get_column_letter(2 + j)            # optimal block B..
fcol_orig = lambda j: get_column_letter(3 + NR + j)       # original block (1 col gap)
UCOL, VCOL = get_column_letter(4 + 2*NR), get_column_letter(5 + 2*NR)
NATU  = get_column_letter(7 + 2*NR)                       # union under the National selection
natsel = lambda j: get_column_letter(9 + 2*NR + j)        # National selection vector (row 2)
set_cell(ws_f,1,1,'rule num'); set_cell(ws_f,2,1,'selected')
set_cell(ws_f,3,1,'deployed'); set_cell(ws_f,4,1,'stratum')
for j, rule in enumerate(RULES):
    set_cell(ws_f,1,2+j, rule['num'])
    set_cell(ws_f,2,2+j, formula=f'=IF(Summary!$M${rule_row[j]}=TRUE,1,0)')
    if rule_best_thr[j] is not None:
        set_cell(ws_f,3,2+j, 1)
    set_cell(ws_f,4,2+j, rule['hh'])
    set_cell(ws_f,1,3+NR+j, rule['num'])
    set_cell(ws_f,2,9+2*NR+j,
             formula=f"=IF('Summary (National)'!$L${NAT_ROW0+j}=TRUE,1,0)")
for j in range(NR):
    if opt_masks[j] is not None:
        for i in np.flatnonzero(opt_masks[j]):
            ws_f.cell(row=FLAG0+int(i), column=2+j).value = 1
    for i in np.flatnonzero(orig_masks[j]):
        ws_f.cell(row=FLAG0+int(i), column=3+NR+j).value = 1
SELRNG = f'$B$2:${fcol_opt(NR-1)}$2'
HASRNG = f'$B$3:${fcol_opt(NR-1)}$3'
HHRNG  = f'$B$4:${fcol_opt(NR-1)}$4'
NATSEL = f'${natsel(0)}$2:${natsel(NR-1)}$2'
for i in range(NDATA):
    r = FLAG0 + i
    ws_f.cell(row=r, column=4+2*NR).value = (
        f'=IF(SUMPRODUCT({SELRNG},$B{r}:${fcol_opt(NR-1)}{r})>0,1,0)')
    ws_f.cell(row=r, column=5+2*NR).value = (
        f'=IF(SUMPRODUCT({SELRNG},${fcol_orig(0)}{r}:${fcol_orig(NR-1)}{r})>0,1,0)')
    ws_f.cell(row=r, column=7+2*NR).value = (
        f'=IF(SUMPRODUCT({NATSEL},${fcol_orig(0)}{r}:${fcol_orig(NR-1)}{r})>0,1,0)')

ws_s = wb.create_sheet('Summary', 1)
ws_s.sheet_view.showGridLines = False
for col_letter, width in {'A':9,'B':9,'C':10,'D':9,'E':11,'F':10,'G':11,
                          'H':10,'I':9,'J':13,'K':11,'L':13,'M':9,'N':115}.items():
    ws_s.column_dimensions[col_letter].width = width
_TIER_WORDS = {0: 'Tier 0, no tuning: the delivered rules at their delivered thresholds',
               1: 'Tier 1, re-filtered and re-ranked on your data; rule text unchanged',
               2: 'Tier 2, thresholds tuned inside ±25% of their delivered values'}
merge(ws_s,1,1,1,12, value=f'Summary — Deployed Rule List for {STATE_NAME} '
                           f'({_TIER_WORDS[RES.tier].split(",")[0]})',
      fill=BLUE_DARK, font=Font(name=FONT,bold=True,size=16,color='FFFFFF'), align=center)
ws_s.row_dimensions[1].height = 32

_JUDGED = (f'Rules were selected on FY{"+".join(str(y) for y in TUNE_YEARS)} and the tier was chosen '
           f'on FY{"+".join(str(y) for y in HOLD_YEARS)}, which no selection step saw.'
           if RES.comparisons['holdout'] > 0 else
           f'No tuned list was built, so nothing was selected on your data and '
           f'FY{"+".join(str(y) for y in HOLD_YEARS)} was not needed to decide.')
merge(ws_s,2,1,2,12,
      value=f'Reading guide: "deployed" = {_TIER_WORDS[RES.tier]}. '
            f'{RES.tier_reason[0].upper() + RES.tier_reason[1:]}. {_JUDGED} '
            f'See the Tuning Audit tab for every test behind that decision.',
      fill=GRAY, font=Font(name=FONT,size=9,color='808080'), align=left)
merge(ws_s,3,1,3,12,
      value='"national" = the delivered thresholds on the same data, for comparison; dashes mean the rule '
            'is not in the deployed list (the Conditions column says why). CAUTION: every figure on this '
            'tab is computed over ALL years in the Data tab, including the years used to select, so it is '
            'optimistic. The held-out figures are on Tuning Audit and are the ones to quote. '
            'Check/uncheck Include? to add/remove a rule — the combined rows recompute live. '
            'Individual rules are ordered by expected error $ per flagged case.',
      fill=GRAY, font=Font(name=FONT,size=9,color='808080'), align=left)

set_cell(ws_s,2,13,False, fill=YELLOW, align=center, border=thin(), font=Font(name=FONT))
set_cell(ws_s,2,14,'← check to show the "national" comparison rows (hidden by default)',
         font=Font(name=FONT,size=9,color='808080'), align=left)

for col, txt in enumerate(['Rule','HH size','Version','Rules count','Precision','Recall',
                           '$ Recall','Flagged','Errors','Error $ caught','Workload %',
                           'Expected error $ by case','Include?','Conditions'], 1):
    set_cell(ws_s,4,col,txt, font=bold_font(10), fill=GRAY, align=center, border=thin())

cases_by = {lbl: int((df['hh_group'] == lbl).sum()) for lbl in STRATA}

# ── overall block: union of all rules, overall + per stratum ──────────────────
row = 5
merge(ws_s,row,1,row,12, value='All rules combined (a case is flagged if ANY '
      'Include?-checked rule flags it) — recomputes as you tick/untick rules',
      fill=BLUE_LIGHT, font=bold_font(10), align=left)
row += 1
U_RNG = f'RuleFlags!${UCOL}${FLAG0}:${UCOL}${FLAG0+NDATA-1}'
V_RNG = f'RuleFlags!${VCOL}${FLAG0}:${VCOL}${FLAG0+NDATA-1}'
D_HH  = f'Data!${dc("hh_group")}$2:${dc("hh_group")}${1+NDATA}'
D_OV  = f'Data!${dc("over_threshold")}$2:${dc("over_threshold")}${1+NDATA}'
D_AM  = f'Data!${dc("total_error_amount")}$2:${dc("total_error_amount")}${1+NDATA}'
scopes = [('Overall', 'all', np.ones(len(df), bool))] + \
         [(f'HH {lbl}', lbl, (df['hh_group'] == lbl).values) for lbl in STRATA]
for scope_name, scope_hh, sel in scopes:
    tot_err = max(int((is_err_all & sel).sum()), 1)
    tot_ed  = round(float(ed_all[sel].sum()), 2) or 1
    tot_n   = max(int(sel.sum()), 1)
    sterm = '' if scope_hh == 'all' else f'*({D_HH}="{scope_hh}")'
    for kind, flags in [('deployed', U_RNG), ('national', V_RNG)]:
        is_opt = (kind == 'deployed')
        fnt = Font(name=FONT) if is_opt else Font(name=FONT, color='808080')
        fill = GREEN if is_opt else WHITE
        if scope_hh == 'all':
            f_cnt = (f'=SUMPRODUCT(RuleFlags!{SELRNG},RuleFlags!{HASRNG})' if is_opt
                     else f'=SUM(RuleFlags!{SELRNG})')
        else:
            hterm = f'*(RuleFlags!{HHRNG}="{scope_hh}")'
            f_cnt = (f'=SUMPRODUCT((RuleFlags!{SELRNG})*(RuleFlags!{HASRNG}){hterm})' if is_opt
                     else f'=SUMPRODUCT((RuleFlags!{SELRNG}){hterm})')
        set_cell(ws_s,row,1,'All rules', font=fnt, align=center, border=thin(), fill=fill)
        set_cell(ws_s,row,2,scope_hh, font=fnt, align=center, border=thin(), fill=fill)
        set_cell(ws_s,row,3,kind, font=fnt, align=center, border=thin(), fill=fill)
        set_cell(ws_s,row,4,formula=f_cnt, font=fnt, align=center, border=thin(), fill=fill,
                 number_format='0')
        for col, f, fmt in [
            (5, f'=IF($H{row}=0,0,$I{row}/$H{row})', '0.0%'),
            (6, f'=$I{row}/{tot_err}',               '0.0%'),
            (7, f'=$J{row}/{tot_ed}',                '0.0%'),
            (8, f'=SUMPRODUCT(({flags}){sterm})',                        '#,##0'),
            (9, f'=SUMPRODUCT(({flags}){sterm}*({D_OV}))',               '#,##0'),
            (10,f'=SUMPRODUCT(({flags}){sterm}*({D_OV})*({D_AM}))',      '$#,##0'),
            (11,f'=$H{row}/{tot_n}',                 '0.0%'),
            (12,f'=IFERROR(IF($H{row}=0,"",$J{row}/$H{row}),"")', '$#,##0'),
        ]:
            set_cell(ws_s,row,col,formula=f, font=fnt, align=center,
                     border=thin(), number_format=fmt, fill=fill)
        row += 1

merge(ws_s,row,1,row,12, value='Individual rules',
      fill=BLUE_LIGHT, font=bold_font(10), align=left)
row += 1
assert row == RULE_ROW0, f'first rule row {row} != RULE_ROW0 {RULE_ROW0}'
for j in rule_order:
    for off, (rule, kind, sc, cond) in enumerate(summary_rows[2*j:2*j+2]):
        is_opt = (kind == 'deployed')
        fnt = Font(name=FONT) if is_opt else Font(name=FONT, color='808080')
        fill = GREEN if is_opt else WHITE
        if is_opt:
            assert row == rule_row[j], f'rule {rule["num"]}: row {row} != {rule_row[j]}'
            set_cell(ws_s,row,13,rule_best_thr[j] is not None,
                     fill=YELLOW, align=center, border=thin(), font=Font(name=FONT))
        set_cell(ws_s,row,1,f'Rule {rule["num"]}', font=fnt, align=center, border=thin(), fill=fill)
        set_cell(ws_s,row,2,rule['hh'], font=fnt, align=center, border=thin(), fill=fill)
        set_cell(ws_s,row,3,kind, font=fnt, align=center, border=thin(), fill=fill)
        set_cell(ws_s,row,4,None, font=fnt, align=center, border=thin(), fill=fill)
        if sc:
            for col, key, fmt in [(5,'prec','0.0%'),(6,'rec','0.0%'),(7,'drec','0.0%'),
                                  (8,'n','#,##0'),(9,'tp','#,##0'),(10,'dollars','$#,##0')]:
                set_cell(ws_s,row,col,round(float(sc[key]),4), font=fnt, align=center,
                         border=thin(), number_format=fmt, fill=fill)
            set_cell(ws_s,row,11,round(sc['n']/cases_by[rule['hh']],4), font=fnt, align=center,
                     border=thin(), number_format='0.0%', fill=fill)
            set_cell(ws_s,row,12,(round(sc['dollars']/sc['n'], 2) if sc['n'] else '—'),
                     font=fnt, align=center, border=thin(), number_format='$#,##0', fill=fill)
        else:
            for col in range(5,13):
                set_cell(ws_s,row,col,'—', font=fnt, align=center, border=thin(), fill=fill)
        set_cell(ws_s,row,14,cond, align=left,
                 font=Font(name=FONT,size=10) if is_opt else Font(name=FONT,size=10,color='808080'))
        row += 1

LAST_ROW = row - 1
for rng_ in (f'A5:L{LAST_ROW}', f'N5:N{LAST_ROW}'):
    ws_s.conditional_formatting.add(
        rng_,
        FormulaRule(formula=['AND($C5="national",$M$2<>TRUE)'],
                    font=Font(name=FONT, color='FFFFFF'), stopIfTrue=True))
ws_s.freeze_panes = 'A5'
CHECKBOX_CELLS = {'Summary': ['M2'] + [f'M{rule_row[j]}' for j in range(NR)]}

# ══════════════════════════════════════════════════════════════════════════════
# 7. SUMMARY (NATIONAL) — delivery thresholds as-is, no grid search
# ══════════════════════════════════════════════════════════════════════════════
ws_n = wb.create_sheet('Summary (National)', 2)
ws_n.sheet_view.showGridLines = False
for col_letter, width in {'A':9,'B':9,'C':11,'D':11,'E':11,'F':11,'G':10,'H':10,
                          'I':14,'J':12,'K':21,'L':9,'M':115}.items():
    ws_n.column_dimensions[col_letter].width = width
merge(ws_n,1,1,1,13, value=f'Summary (National Thresholds) — {STATE_NAME} FY{FY_LABEL}',
      fill=BLUE_DARK, font=Font(name=FONT,bold=True,size=16,color='FFFFFF'), align=center)
ws_n.row_dimensions[1].height = 32
merge(ws_n,2,1,2,13,
      value='This tab reports the delivery rules exactly as mined on national QC data: thresholds are '
            f'used AS-IS, with no state-level search or tuning. Every metric below is computed on all '
            f'{STATE_NAME} QC cases in the Data tab. This is the Tier 0 list, and it is what the tiered '
            f'procedure has to beat on a held-out year before any tuning is deployed (Tuning Audit tab).',
      fill=GRAY, font=Font(name=FONT,size=9,color='808080'), align=left)
merge(ws_n,3,1,3,13,
      value='Recall, $ Recall and Workload % are measured within each rule\'s own household-size '
            'stratum. Expected error $ by case = error dollars caught / cases flagged. Rules are listed '
            'in delivery-list rank order, exactly as delivered. Tick or untick Include? to add or '
            'remove a rule from the combined rows above.',
      fill=GRAY, font=Font(name=FONT,size=9,color='808080'), align=left)
for col, txt in enumerate(['Rule','HH size','Rules count','Precision','Recall','$ Recall',
                           'Flagged','Errors','Error $ caught','Workload %',
                           'Expected error $ by case','Include?',
                           'Conditions (national thresholds, as delivered)'], 1):
    set_cell(ws_n,4,col,txt, font=bold_font(10), fill=GRAY, align=center, border=thin())

merge(ws_n,5,1,5,13,
      value='All rules combined (a case is flagged if ANY Include?-checked rule flags it, '
            'at national thresholds)',
      fill=BLUE_LIGHT, font=bold_font(10), align=left)
NAT_RNG = f'RuleFlags!${NATU}${FLAG0}:${NATU}${FLAG0+NDATA-1}'
for i, (scope_hh, sel) in enumerate([('all', np.ones(len(df), bool))] +
                                    [(lbl, (df['hh_group'] == lbl).values) for lbl in STRATA]):
    r = 6 + i
    tot_err = max(int((is_err_all & sel).sum()), 1)
    tot_ed  = round(float(ed_all[sel].sum()), 2) or 1
    tot_n   = max(int(sel.sum()), 1)
    sterm = '' if scope_hh == 'all' else f'*({D_HH}="{scope_hh}")'
    f_cnt = (f'=SUM(RuleFlags!{NATSEL})' if scope_hh == 'all'
             else f'=SUMPRODUCT((RuleFlags!{NATSEL})*(RuleFlags!{HHRNG}="{scope_hh}"))')
    set_cell(ws_n,r,1,'All rules', align=center, border=thin(), fill=GREEN)
    set_cell(ws_n,r,2,scope_hh, align=center, border=thin(), fill=GREEN)
    set_cell(ws_n,r,3,formula=f_cnt, align=center, border=thin(), fill=GREEN, number_format='0')
    for col, f, fmt in [
        (4, f'=IF($G{r}=0,0,$H{r}/$G{r})',                            '0.0%'),
        (5, f'=$H{r}/{tot_err}',                                      '0.0%'),
        (6, f'=$I{r}/{tot_ed}',                                       '0.0%'),
        (7, f'=SUMPRODUCT(({NAT_RNG}){sterm})',                       '#,##0'),
        (8, f'=SUMPRODUCT(({NAT_RNG}){sterm}*({D_OV}))',              '#,##0'),
        (9, f'=SUMPRODUCT(({NAT_RNG}){sterm}*({D_OV})*({D_AM}))',     '$#,##0'),
        (10,f'=$G{r}/{tot_n}',                                        '0.0%'),
        (11,f'=IFERROR(IF($G{r}=0,"",$I{r}/$G{r}),"")',               '$#,##0'),
    ]:
        set_cell(ws_n,r,col,formula=f, align=center, border=thin(),
                 number_format=fmt, fill=GREEN)
    set_cell(ws_n,r,12,None, align=center, border=thin(), fill=GREEN)
    set_cell(ws_n,r,13,'union of the checked rules at their national thresholds',
             align=left, font=Font(name=FONT,size=10))

merge(ws_n,10,1,10,13, value='Individual rules — national thresholds, no tuning',
      fill=BLUE_LIGHT, font=bold_font(10), align=left)
for j, rule in enumerate(RULES):                      # RULES is already in delivery-rank order
    r = NAT_ROW0 + j
    sc, cond = summary_rows[2*j+1][2], summary_rows[2*j+1][3]
    set_cell(ws_n,r,1,f'Rule {rule["num"]}', align=center, border=thin(), fill=GREEN)
    set_cell(ws_n,r,2,rule['hh'], align=center, border=thin(), fill=GREEN)
    set_cell(ws_n,r,3,None, align=center, border=thin(), fill=GREEN)
    for col, key, fmt in [(4,'prec','0.0%'),(5,'rec','0.0%'),(6,'drec','0.0%'),
                          (7,'n','#,##0'),(8,'tp','#,##0'),(9,'dollars','$#,##0')]:
        set_cell(ws_n,r,col,round(float(sc[key]),4), align=center, border=thin(),
                 number_format=fmt, fill=GREEN)
    set_cell(ws_n,r,10,round(sc['n']/cases_by[rule['hh']],4), align=center, border=thin(),
             number_format='0.0%', fill=GREEN)
    set_cell(ws_n,r,11,(round(sc['dollars']/sc['n'], 2) if sc['n'] else '—'),
             align=center, border=thin(), number_format='$#,##0', fill=GREEN)
    set_cell(ws_n,r,12,True, fill=YELLOW, align=center, border=thin(), font=Font(name=FONT))
    set_cell(ws_n,r,13,cond, align=left, font=Font(name=FONT,size=10))
ws_n.freeze_panes = 'A5'
CHECKBOX_CELLS['Summary (National)'] = [f'L{NAT_ROW0+j}' for j in range(NR)]

# ══════════════════════════════════════════════════════════════════════════════
# 7b. TUNING AUDIT — the split, the tier decision, and every test behind it
#
# This tab exists so the tuning is checkable rather than trusted. It carries the
# numbers a reviewer needs to reject the result: how much data each decision was
# made on, how many distinct combinations were compared, and how each arm did on
# a year no selection step ever saw.
# ══════════════════════════════════════════════════════════════════════════════
ws_a = wb.create_sheet('Tuning Audit', 3)
ws_a.sheet_view.showGridLines = False
for col_letter, width in {'A':30,'B':13,'C':13,'D':13,'E':13,'F':13,'G':13,'H':13,
                          'I':13,'J':13,'K':13,'L':13,'M':13,'N':13,'O':90}.items():
    ws_a.column_dimensions[col_letter].width = width

merge(ws_a,1,1,1,15, value=f'Tuning Audit — {STATE_NAME}: what was searched, and what it was judged on',
      fill=BLUE_DARK, font=Font(name=FONT,bold=True,size=16,color='FFFFFF'), align=center)
ws_a.row_dimensions[1].height = 32

def _abanner(r, txt):
    merge(ws_a,r,1,r,15, value=txt, fill=BLUE_DARK,
          font=Font(name=FONT,bold=True,size=12,color='FFFFFF'), align=left)
    return r + 1

def _akv(r, label, value, note=''):
    set_cell(ws_a,r,1,label, font=bold_font(10), fill=GRAY, align=left, border=thin())
    merge(ws_a,r,2,r,5, value=value, fill=BLUE_LIGHT, font=Font(name=FONT), align=left)
    if note:
        set_cell(ws_a,r,6,note, font=Font(name=FONT,size=9,color='808080'), align=left)
    return r + 1

n_tune = int((df['split'] == 'tune').sum())
n_hold = int((df['split'] == 'holdout').sum())
err_tune = int(df.loc[df.split == 'tune', 'over_threshold'].sum())
err_hold = int(df.loc[df.split == 'holdout', 'over_threshold'].sum())

ar = _abanner(3, 'The decision')
ar = _akv(ar, 'Tier deployed', f'Tier {RES.tier} — {_TIER_WORDS[RES.tier]}')
ar = _akv(ar, 'Why', RES.tier_reason)
ar = _akv(ar, 'Tuning years (selection)',
          f'FY{"+".join(str(y) for y in TUNE_YEARS)}: {n_tune:,} cases, {err_tune:,} errors '
          f'({err_tune / max(n_tune, 1):.1%})',
          'every admission and threshold decision was made here')
ar = _akv(ar, 'Held-out year (judging)',
          f'FY{"+".join(str(y) for y in HOLD_YEARS)}: {n_hold:,} cases, {err_hold:,} errors '
          f'({err_hold / max(n_hold, 1):.1%})',
          'split by fiscal year, never at random: a random split leaks')
ar = _akv(ar, 'Review budget', f'{TCFG.budget:.0%} of the caseload',
          'lists are filled in rank order to this capacity, using flag counts only')
ar = _akv(ar, 'Rules shipped / deployed',
          f'{len(RULES)} shipped, {sum(RES.deployed.values())} deployed')

ar = _abanner(ar + 1, 'How many things were compared (this is what the winner\'s curse scales with)')
ar = _akv(ar, 'Tier 1 admission tests', f'{RES.comparisons["tier1_tests"]}',
          f'rules clearing the {TCFG.min_support}-case support floor; '
          f'Benjamini-Hochberg at FDR {TCFG.fdr_alpha:.0%} sets the bar from this count')
ar = _akv(ar, 'Tier 2 threshold combinations', f'{RES.comparisons["tier2_variants"]}',
          f'distinct after dropping cuts that partition the data identically; '
          f'at most {RES.comparisons["tier2_max_per_rule"]} for any one rule')
ar = _akv(ar, 'Held-out comparisons', f'{RES.comparisons["holdout"]}',
          'one per tier, fixed before the run. No rule was ever kept or dropped '
          'because of how it did on the held-out year')

ar = _abanner(ar + 1, 'Arms: each list filled to budget on the tuning years, then scored on the held-out year')
AHEAD = ['Tier','Rules','Tuning precision','Held-out precision','Held-out bound',
         'Flagged','Errors','Recall','$ Recall','Workload %','Deflation']
for ci, txt in enumerate(AHEAD, 1):
    set_cell(ws_a,ar,ci,txt, font=bold_font(10), fill=GRAY, align=center, border=thin())
ar += 1
for _, a in RES.arms.iterrows():
    chosen = int(a.tier) == RES.tier
    fnt = bold_font() if chosen else Font(name=FONT, color='808080')
    fill = GREEN if chosen else WHITE
    vals = [(1, f'Tier {int(a.tier)}' + (' (deployed)' if chosen else ''), '@'),
            (2, int(a.n_rules), '0'),
            (3, round(float(a.tune_prec), 4), '0.0%'),
            (4, round(float(a.hold_prec), 4), '0.0%'),
            (5, round(float(a.hold_prec_lcb), 4), '0.000'),
            (6, int(a.hold_flagged), '#,##0'),
            (7, int(a.hold_errors), '#,##0'),
            (8, round(float(a.hold_rec), 4), '0.0%'),
            (9, round(float(a.hold_drec), 4), '0.0%'),
            (10, round(float(a.hold_workload), 4), '0.0%'),
            (11, (round(float(a.deflation), 3) if np.isfinite(a.deflation) else '—'), '0.000')]
    for ci, v, fmt in vals:
        set_cell(ws_a,ar,ci,v, font=fnt, fill=fill, align=center, border=thin(),
                 number_format=fmt)
    ar += 1
set_cell(ws_a,ar,1,'"Held-out bound" is the 90% one-sided Wilson lower bound on that arm\'s held-out '
                   'precision. A tier is deployed only when its bound clears Tier 0\'s held-out precision '
                   f'outright, on at least {TCFG.min_holdout_flagged} flagged held-out cases. '
                   '"Deflation" is held-out precision divided by tuning-year precision: quote the '
                   'held-out column, not the tuning column.',
         font=Font(name=FONT,size=9,color='808080'), align=left)
ar += 2

ar = _abanner(ar, 'Per-rule record: every admission test, in delivery-rank order')
RHEAD = ['Rule','HH size','Tuning flagged','Errors','Tuning precision','Stratum base rate',
         'p-value','BH cutoff','Clears support','Admitted','99% bound','Combos searched',
         'Bound z used','Threshold moved','Deployed','Deployed conditions']
for ci, txt in enumerate(RHEAD, 1):
    set_cell(ws_a,ar,ci,txt, font=bold_font(10), fill=GRAY, align=center, border=thin())
hdr_row = ar
ar += 1
for ri, rule in enumerate(RULES):
    t = TAB.loc[ri]
    on = bool(RES.deployed[ri])
    fnt = Font(name=FONT) if on else Font(name=FONT, color='808080')
    fill = GREEN if on else WHITE
    cells = [
        (1, f'Rule {rule["num"]}', '@'), (2, rule['hh'], '@'),
        (3, int(t.tune_n), '#,##0'), (4, int(t.tune_k), '#,##0'),
        (5, round(float(t.tune_prec), 4), '0.0%'),
        (6, round(float(t.base_rate), 4), '0.0%'),
        (7, ('—' if not np.isfinite(t.pvalue) else round(float(t.pvalue), 5)), '0.00000'),
        (8, ('—' if not np.isfinite(t.bh_crit) else round(float(t.bh_crit), 5)), '0.00000'),
        (9, 'yes' if t.support_ok else 'no', '@'),
        (10, 'yes' if t.admitted else 'no', '@'),
        (11, round(float(t.lcb99), 4), '0.000'),
        (12, int(t.n_variants), '#,##0'),
        (13, ('—' if not np.isfinite(t.variant_gate_z) else round(float(t.variant_gate_z), 2)), '0.00'),
        (14, 'yes' if t.threshold_moved else 'no', '@'),
        (15, 'yes' if on else 'no', '@'),
        (16, summary_rows[2 * ri][3], '@'),
    ]
    for ci, v, fmt in cells:
        set_cell(ws_a,ar,ci,v, font=fnt, fill=fill, align=center, border=thin(),
                 number_format=fmt)
    ws_a.cell(row=ar, column=16).alignment = left
    ar += 1
ws_a.freeze_panes = f'A{hdr_row + 1}'
last_rule_row = ar - 1
ar += 1

ar = _abanner(ar, 'Settings, fixed before the run')
for label, val, note in [
    ('Tier ceiling', TCFG.max_tier, 'the highest tier this build was allowed to reach'),
    ('Support floor', TCFG.min_support,
     'minimum tuning-year cases flagged; non-negotiable (findings §9: at n >= 5, '
     'state-scale tuning collapsed to zero held-out precision)'),
    ('Admission', f'Benjamini-Hochberg, FDR {TCFG.fdr_alpha:.0%}',
     'against the rule\'s own stratum base rate on the tuning years'),
    ('Ordering statistic', f'{TCFG.lcb_z_rank:g}-sigma Wilson lower bound (99% one-sided)',
     'never raw precision, and never held-out performance'),
    ('Search bracket',
     f'{TCFG.factors_fine[0]:g}x-{TCFG.factors_fine[-1]:g}x for <= {TCFG.fine_max_conds} conditions, '
     f'{TCFG.factors_coarse[0]:g}x-{TCFG.factors_coarse[-1]:g}x beyond',
     'variables, operators, condition count and stratum are frozen'),
    ('Variant bound', f'alpha {TCFG.variant_gate_alpha:g} / combinations searched, '
     f'precision floor {TCFG.min_variant_precision:.0%}',
     'a wider search has to clear a higher bar'),
    ('Selection objective', TCFG.select_objective,
     'fixed globally before the run, never chosen per rule'),
    ('Tier 2 minimum admitted', TCFG.min_admitted_for_tier2,
     'below this, local tuning is refused outright (findings §9 rule of thumb)'),
    ('Cap on combinations', TCFG.max_variants, 'per rule'),
]:
    ar = _akv(ar, label, val, note)

ar = _abanner(ar + 1, 'What to expect in a future year')
for txt in [
    'Tuning buys reach, not precision. In the seven-state study, the state with enough support '
    '(Connecticut, 35 qualifying rules) went from 24.4% of errors caught at 0.228 precision under the '
    'untouched national list to 43.0% of errors (49.1% of error dollars) at 0.209 precision. Reach '
    'nearly doubled; precision did not rise.',
    'Where support was thin, tuning failed outright: Washington fell to 0.048 precision while the '
    'untouched national list held 0.364. That is why the support floor and the refusal thresholds above '
    'are not adjustable in the delivered build.',
    'Expect roughly a third off the tuning-year precision when this list meets a future year. The '
    'held-out column in the arms table above already shows that deflation on your own data; quote it, '
    'not the Summary tab.',
    'Scored against ANY over-threshold error, not the error type a rule was mined for: a review finds '
    'whatever error is present, and state samples are too thin to score by type.',
] + [f'Run note: {n}' for n in RES.notes]:
    merge(ws_a,ar,1,ar,15, value=txt, fill=WHITE,
          font=Font(name=FONT,size=10), align=left)
    ws_a.row_dimensions[ar].height = 30
    ar += 1

# ══════════════════════════════════════════════════════════════════════════════
# 8. ERROR CASES — live list of rules catching errors + the cases they catch
# ══════════════════════════════════════════════════════════════════════════════
ws_e = wb.create_sheet('Error Cases', 4)
ws_e.sheet_view.showGridLines = False
NCOL   = len(DCOLS)
G0     = 3                                   # first grid column (C)
GLAST  = get_column_letter(G0 + NCOL - 1)
DROWC  = get_column_letter(G0 + NCOL)        # data_row column
LISTN  = max(NR, 64)                         # rule-list slots: one per rule, so a
                                             # longer delivery list is never
                                             # silently truncated on this panel
GRIDN  = 60                                  # result rows
HELP_Z = get_column_letter(G0 + NCOL + 2)    # per-case hit flag
HELP_A = get_column_letter(G0 + NCOL + 3)    # cumulative count
HELP_Y = get_column_letter(G0 + NCOL + 1)    # rule index / dashboard row
SC_B   = get_column_letter(G0 + NCOL + 4)    # rule score
SC_C   = get_column_letter(G0 + NCOL + 5)    # rule errors
SC_D   = get_column_letter(G0 + NCOL + 6)    # compacted sorted rule list
PRESETS = f"'Grid Search'!$AA$2:$AA${1+NR}"
for cl, w in {'A':20,'B':9}.items():
    ws_e.column_dimensions[cl].width = w
for i in range(NCOL):
    ws_e.column_dimensions[get_column_letter(G0+i)].width = 13 if i > 2 else 16
ws_e.column_dimensions[DROWC].width = 9
for cl in (HELP_Y, HELP_Z, HELP_A, SC_B, SC_C, SC_D):
    ws_e.column_dimensions[cl].hidden = True

set_cell(ws_e,1,1,formula=f'=${SC_B}$1&" rules with errors"',
         fill=BLUE_DARK, font=Font(name=FONT,bold=True,size=14,color='FFFFFF'), align=center)
ws_e.merge_cells('A1:B1'); ws_e.row_dimensions[1].height = 32
merge(ws_e,2,1,2,2, value='sorted by errors caught — updates live',
      fill=GRAY, font=Font(name=FONT,size=9,color='808080'), align=left)
set_cell(ws_e,3,1,'Rule', font=bold_font(10), fill=GRAY, align=center, border=thin())
set_cell(ws_e,3,2,'errors', font=bold_font(10), fill=GRAY, align=center, border=thin())
for k in range(1, LISTN+1):
    r = 3 + k
    set_cell(ws_e,r,1,formula=f'=IF({k}>${SC_B}$1,"",INDEX(${SC_D}$2:${SC_D}${1+NR},{k}))',
             align=left, border=thin())
    set_cell(ws_e,r,2,formula=f'=IF({k}>${SC_B}$1,"",INT(LARGE(${SC_B}$2:${SC_B}${1+NR},{k})/1000))',
             align=center, border=thin())

merge(ws_e,1,G0,1,G0+11,
      value='Error Cases — true errors caught by the selected rule (live Dashboard thresholds)',
      fill=BLUE_DARK, font=Font(name=FONT,bold=True,size=16,color='FFFFFF'), align=center)
merge(ws_e,2,G0,2,G0+NCOL-1,
      value='Pick a rule (the dropdown lists only rules currently catching errors, sorted by errors '
            'caught). The table lists every case the rule flags at its CURRENT Dashboard thresholds '
            'that is a true payment error (over_threshold = 1).',
      fill=GRAY, font=Font(name=FONT,size=9,color='808080'), align=left)
set_cell(ws_e,3,G0,'Rule', font=bold_font(10), fill=GRAY, align=center, border=thin())
set_cell(ws_e,3,G0+1,f'Rule {RULES[0]["num"]} (HH {RULES[0]["hh"]})',
         fill=YELLOW, align=center, border=thin(), font=bold_font(), number_format='@')
ws_e.merge_cells(start_row=3, start_column=G0+1, end_row=3, end_column=G0+3)
set_cell(ws_e,3,G0+4,'← dropdown lists only the rules currently catching errors',
         font=Font(name=FONT,size=9,color='808080'), align=left)
YREF = f'${HELP_Y}$1'; Y2REF = f'${HELP_Y}$2'
set_cell(ws_e,1,G0+NCOL+1,
         formula=f'=MATCH(${get_column_letter(G0+1)}$3,{PRESETS},0)-1')
set_cell(ws_e,2,G0+NCOL+1, formula=f'={BASE_ROW+2}+{YREF}*{BLOCK_HEIGHT}')
set_cell(ws_e,4,G0,'HH stratum', font=bold_font(10), fill=GRAY, align=center, border=thin())
set_cell(ws_e,4,G0+1,formula=f'=INDEX(RuleFlags!{HHRNG},1,{YREF}+1)',
         fill=BLUE_LIGHT, align=center, border=thin())
set_cell(ws_e,4,G0+3,'n_flagged', font=bold_font(10), fill=GRAY, align=center, border=thin())
set_cell(ws_e,4,G0+4,formula=f'=INDEX(Dashboard!$G:$G,{Y2REF})',
         fill=BLUE_LIGHT, align=center, border=thin())
set_cell(ws_e,4,G0+6,'errors caught', font=bold_font(10), fill=GRAY, align=center, border=thin())
set_cell(ws_e,4,G0+7,formula=f'=INDEX(Dashboard!$G:$G,{Y2REF}+1)',
         fill=BLUE_LIGHT, align=center, border=thin())
for off, txt in enumerate(['Variable','Op','Dashboard threshold','Data col #']):
    set_cell(ws_e,5,G0+off,txt, font=bold_font(10), fill=GRAY, align=center, border=thin())
CV = get_column_letter(G0); OPV = get_column_letter(G0+1)
THV = get_column_letter(G0+2); IXV = get_column_letter(G0+3)
for i in range(NSLOTS):
    r = 6 + i
    set_cell(ws_e,r,G0,  formula=f'=IFERROR(IF(INDEX(Dashboard!$A:$A,{Y2REF}+{i})=0,"",'
                                 f'INDEX(Dashboard!$A:$A,{Y2REF}+{i})),"")',
             fill=BLUE_LIGHT, align=center, border=thin())
    set_cell(ws_e,r,G0+1,formula=f'=IFERROR(IF(INDEX(Dashboard!$B:$B,{Y2REF}+{i})=0,"",'
                                 f'INDEX(Dashboard!$B:$B,{Y2REF}+{i})),"")',
             fill=BLUE_LIGHT, align=center, border=thin())
    set_cell(ws_e,r,G0+2,formula=f'=IF(${CV}{r}="","",INDEX(Dashboard!$C:$C,{Y2REF}+{i}))',
             fill=BLUE_LIGHT, align=center, border=thin())
    set_cell(ws_e,r,G0+3,formula=f'=IF(${CV}{r}="","",IFERROR(MATCH(${CV}{r},'
                                 f'Data!$A$1:${LASTCOL}$1,0),""))',
             fill=BLUE_LIGHT, align=center, border=thin())
for i, h in enumerate(DCOLS):
    set_cell(ws_e,10,G0+i,h, font=bold_font(10), fill=GRAY, align=center, border=thin())
set_cell(ws_e,10,G0+NCOL,'data_row', font=bold_font(10), fill=GRAY, align=center, border=thin())
CUM = f'${HELP_A}$12:${HELP_A}${11+NDATA}'
for r in range(11, 11+GRIDN):
    for i in range(NCOL):
        ws_e.cell(row=r, column=G0+i).value = (
            f'=IFERROR(INDEX(Data!$A$2:${LASTCOL}${1+NDATA},MATCH(ROW()-10,{CUM},0),'
            f'COLUMN()-{G0-1}),"")')
    ws_e.cell(row=r, column=G0+NCOL).value = f'=IFERROR(MATCH(ROW()-10,{CUM},0)+1,"")'
for i in range(NDATA):
    r, dr_ = 12 + i, 2 + i
    slots = '*'.join(
        f'IF(${CV}${6+k}="",1,COUNTIF(INDEX(Data!$A{dr_}:${LASTCOL}{dr_},1,${IXV}${6+k}),'
        f'${OPV}${6+k}&${THV}${6+k}))' for k in range(NSLOTS))
    ws_e.cell(row=r, column=G0+NCOL+2).value = (
        f'=IF(Data!${dc("hh_group")}{dr_}<>${get_column_letter(G0+1)}$4,0,'
        f'IF(Data!${dc("over_threshold")}{dr_}<>1,0,{slots}))')
    ws_e.cell(row=r, column=G0+NCOL+3).value = (
        f'=${HELP_Z}$12' if i == 0 else f'=${HELP_A}{r-1}+${HELP_Z}{r}')
ws_e.cell(row=1, column=G0+NCOL+4).value = f'=COUNTIF(${SC_C}$2:${SC_C}${1+NR},">0")'
for j in range(NR):
    r = 2 + j
    ws_e.cell(row=r, column=G0+NCOL+4).value = (
        f'=INDEX(Dashboard!$G:$G,{BASE_ROW+3}+{j}*{BLOCK_HEIGHT})*1000+({NR}-{j})')
    ws_e.cell(row=r, column=G0+NCOL+5).value = (
        f'=INDEX(Dashboard!$G:$G,{BASE_ROW+3}+{j}*{BLOCK_HEIGHT})')
    ws_e.cell(row=r, column=G0+NCOL+6).value = (
        f'=IF(ROW()-1>${SC_B}$1,"",INDEX({PRESETS},'
        f'{1+NR}-MOD(LARGE(${SC_B}$2:${SC_B}${1+NR},ROW()-1),1000)))')
wb.defined_names.add(DefinedName('RuleListLive',
    attr_text=f"OFFSET('Error Cases'!${SC_D}$2,0,0,MAX('Error Cases'!${SC_B}$1,1),1)"))
dv_live = DataValidation(type='list', formula1='RuleListLive', allow_blank=False)
ws_e.add_data_validation(dv_live)
dv_live.add(f'{get_column_letter(G0+1)}3')
ws_e.freeze_panes = f'{get_column_letter(G0)}11'

# ── final touches ─────────────────────────────────────────────────────────────
ws.sheet_state   = 'hidden'      # Dashboard  (engine)
ws_g.sheet_state = 'hidden'      # Grid Search(engine)

# ── Save ──────────────────────────────────────────────────────────────────────
os.makedirs(STATE_DIR, exist_ok=True)
wb.save(OUT)
import json
os.makedirs(BUILD_DIR, exist_ok=True)
json.dump(CHECKBOX_CELLS, open(os.path.join(BUILD_DIR, 'checkbox_cells.json'), 'w'))
json.dump({'out': OUT, 'state': STATE_NAME, 'abbr': STATE_ABBR},
          open(os.path.join(BUILD_DIR, 'target.json'), 'w'))
print(f'Saved: {OUT}')
print(f'Rules: {len(RULES)} | Data rows: {len(df)}')
print(f'Checkbox cells recorded for post-processing: '
      f'{sum(len(v) for v in CHECKBOX_CELLS.values())}')
