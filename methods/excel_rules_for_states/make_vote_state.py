"""
Build a state's VOTE workbook (issues #16 / #21; findings section 44, shipped
by Eric's decision of 2026-10-29): the standard workbook plus a Vote? column
on the rules tab and the "5.2.b Expected Error Amounts" tab after Step 5.2.a
(make_vote.py; called "Vote Score" until 2026-10-29).

The rules tab carries the state's shipped list exactly as in the standard
build (Include? = TRUE; Vote? follows Include? since 2026-11-12, so the
included rules vote; before, Vote? = FALSE) and, below it, the 300 national
vote rules (Rule 1001 on; Include? = FALSE, Vote? = TRUE). Expected error $
comes from the fixed national table (methods/vote_scoring/deploy/), scaled to
the state's FNS FY2025 payment error rate. "Start Here" leads with the 5.2.b
ranking's figures on the testing data (make_vote.py, 2026-11-12).

SNAP_VOTE_DEST_DIR (2026-11-12) puts the finished workbooks in another folder
instead of custom_one_off/<state>/, for test builds that must not overwrite a
delivered file.

Size (2026-10-29): each rule is tested once per case for both checkboxes
(make_live.MERGED_VOTE: Include? + 1000 x Vote?), and the Step 2 formulas
are then rewritten with cell addresses (compact_formulas.py). Before, the WA
workbook tested every rule twice with table-style references: 36 MB, 83 s to
open.

    python make_vote_state.py WA DC MI
    SNAP_VOTE_RULES_CSV=<csv> python make_vote_state.py WA    (another vote-rule list)

Writes custom_one_off/<folder>/SNAP_flagging_rules_<ABBR>_vote.xlsx and .xlsb
(gitignored; state custom work stays out of GitHub). The intermediate stages
land in .build/out_<ABBR>_vote/. The standard build's shared .build files
(effective_rules_<ABBR>.csv, target.json, checkbox_cells.json) are restored
afterwards, so the standard deliverable's chain is untouched.
"""
import csv
import json
import os
import shutil
import subprocess
import sys

PKG = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.abspath(os.path.join(PKG, '..', '..'))
sys.path.insert(0, PKG)
import states as STATE_REGISTRY                      # noqa: E402
from make_state import run, verify, PROBES_FINAL, in_use   # noqa: E402
from workbook_layout import BLENDED_SHEET, FLAGGED_SHEET, FLAGGED_SHEET_VOTE, VOTE_SHEET   # noqa: E402
from compact_formulas import compact                   # noqa: E402

BUILD_DIR = os.path.join(PKG, '.build')
FOLDER = {'WA': 'washington', 'DC': 'district_of_columbia', 'MI': 'michigan'}
VOTE_RULES = os.environ.get('SNAP_VOTE_RULES_CSV') or os.path.join(
    REPO, 'methods', 'vote_scoring', 'deploy', 'vote_rules_fy2022_2023.csv')


def vote_expectations(plain_xlsx, effective_csv):
    """(total votes, cases with any vote) from the plain build's static
    RuleFlags hit matrix, counting the rules that vote as shipped: the
    national vote rules (vote = True) and, since 2026-11-12, every rule that
    ships Include? = TRUE (ship = True), whose Vote? cell follows Include?."""
    import openpyxl
    with open(effective_csv, newline='') as fh:
        votes = [r['vote'] == 'True' or r.get('ship', 'True') == 'True' for r in csv.DictReader(fh)]
    wb = openpyxl.load_workbook(plain_xlsx, read_only=True)
    rf = wb['RuleFlags']
    nr = sum(1 for c in next(rf.iter_rows(min_row=1, max_row=1))[1:] if isinstance(c.value, int))
    assert nr == len(votes), (nr, len(votes))
    from workbook_layout import DATA_SHEET
    ndata = wb[DATA_SHEET].max_row - 1
    total = anyv = 0
    for i, row in enumerate(rf.iter_rows(min_row=5, max_col=1 + nr, values_only=True)):
        if i >= ndata:
            break
        k = sum(1 for v, on in zip(row[1:], votes) if on and v == 1)
        total += k
        anyv += k > 0
    return total, anyv


def build_vote(state):
    cfg = STATE_REGISTRY.get(state)
    assert os.path.isfile(VOTE_RULES), VOTE_RULES
    out_dir = os.path.join(BUILD_DIR, f'out_{state}_vote')
    saved = {}
    for f in (f'effective_rules_{state}.csv', 'target.json', 'checkbox_cells.json'):
        p = os.path.join(BUILD_DIR, f)
        if os.path.isfile(p):
            saved[p] = open(p, 'rb').read()
    env = {'SNAP_STATE': state, 'SNAP_OUT_DIR': out_dir, 'SNAP_VOTE': '1', 'SNAP_VOTE_RULES_CSV': VOTE_RULES,
           'SNAP_MEASURE_CSV': ''}
    try:
        run('build_workbook_v2.py', env=env)
        out = json.load(open(os.path.join(BUILD_DIR, 'target.json')))['out']
        eff = os.path.join(out_dir, f'effective_rules_{state}_vote.csv')
        shutil.copy(os.path.join(BUILD_DIR, f'effective_rules_{state}.csv'), eff)
        live = out.replace('.xlsx', '_LIVE.xlsx')
        recon = out.replace('.xlsx', '_RECON.xlsx')
        vote = out.replace('.xlsx', '_VOTE.xlsx')
        run('make_live.py', out, '-o', live, env=env)
        run('make_input_workbook.py', live, '-o', recon, '--state', state, env=env)
        run('make_vote.py', recon, '-o', vote, '--state', cfg['name'], env=env)
        ck = os.path.join(BUILD_DIR, 'checkbox_cells.json')
        for wbk in (out, live, recon, vote):
            run('postprocess_workbook.py', wbk, ck)
        compact(vote)        # Step 2 formulas as cell addresses: opens ~3x faster
        total, anyv = vote_expectations(out, eff)
        from make_state import static_union
        g5, h5 = static_union(out, state)    # the shipped (Include? = TRUE) union, from the vote build's effective list
        print(f'expected: {total} votes on Step 2, {anyv} cases with a vote')
        dest_dir = os.environ.get('SNAP_VOTE_DEST_DIR') or os.path.join(
            REPO, 'custom_one_off', FOLDER.get(state, cfg['name'].lower().replace(' ', '_')))
        os.makedirs(dest_dir, exist_ok=True)
        dest = os.path.join(dest_dir, f'SNAP_flagging_rules_{state}_vote.xlsx')
        if in_use(dest):
            raise SystemExit(f'{dest} is open in Excel - close it and re-run.')
        shutil.copy(vote, dest)
        lay = json.load(open(vote.replace('.xlsx', '_layout.json')))
        # this process has no SNAP_VOTE, so PROBES_FINAL carries the standard Step 5.2 name
        probes = [p.replace(f'{FLAGGED_SHEET}!', f'{FLAGGED_SHEET_VOTE}!') for p in PROBES_FINAL
                  if p not in (f"{BLENDED_SHEET}!G5", f"{BLENDED_SHEET}!H5")]
        probes += [f"{BLENDED_SHEET}!G5={g5}", f"{BLENDED_SHEET}!H5={h5}"]
        rc = lay['comparison_row']
        V = VOTE_SHEET
        probes += [f'{V}!J4={total}', f'{V}!J5={anyv}', f"{V}!J6={lay['per']}", f'{V}!E6',
                   *[f'{V}!{c}' for c in lay['list_cells']],
                   f'{V}!B7', f'{V}!C7'] + [f'{V}!{c}{r}' for r in (rc, rc + 1, rc + 2, rc + 3) for c in 'EFI']
        probes += ['Start Here!B4', 'Start Here!B5', 'Start Here!B6', 'Start Here!B8']   # the 5.2.b headline figures
        verify(dest, probes)
        # Excel Binary copy, checked with the same probes
        xlsb = dest.replace('.xlsx', '.xlsb')
        if in_use(xlsb):
            raise SystemExit(f'{xlsb} is open in Excel - close it and re-run.')
        p = subprocess.run(['powershell', '-NoProfile', '-ExecutionPolicy', 'Bypass', '-File',
                            os.path.join(PKG, 'convert_xlsb_win.ps1'), dest, xlsb], cwd=PKG)
        if p.returncode != 0:
            raise SystemExit(f'xlsb export failed: {dest}')
        verify(xlsb, probes)
        print(f'\nDone: {dest} ({os.path.getsize(dest) / 1e6:.1f} MB) and {xlsb} ({os.path.getsize(xlsb) / 1e6:.1f} MB)')
    finally:
        for p, b in saved.items():
            open(p, 'wb').write(b)


if __name__ == '__main__':
    for st in [a.upper() for a in sys.argv[1:]] or ['WA']:
        build_vote(st)
