"""
One-off for v2.8.1 (2026-11-12). Eric's wording for the comparison block's note on 5.2.b, its last row's label,
and the line under the "Start Here" figures (make_vote.py: COMPARISON_NOTE, SAME_N_LABEL, START_NOTE) reached
make_vote.py while the v2.8.1 batch (methods/vote_scoring/run_f1109_v281_all.cmd) was running, at 10:00 with
Montana starting. This script puts it into the workbooks built before then: in each state's delivered
custom_one_off/<state>/SNAP_flagging_rules_<ABBR>_vote.xlsx it replaces the old note and label (stopping
unless both are the old text) and writes the Start Here line into the empty row under the figures. Nothing else
changes. A workbook that already carries the new wording is skipped. Each patched workbook is then verified with
the probes make_vote_state.py uses, converted to .xlsb by desktop Excel, and the .xlsb verified the same way.
Run it after the batch has finished: it reuses the batch's .build/out_<ABBR>_vote/ files and, like
make_vote_state.py, briefly swaps the shared .build/effective_rules_<ABBR>.csv.
    python patch_v281_text.py all        (or: python patch_v281_text.py AL AZ ...)
"""
import json
import os
import shutil
import subprocess
import sys

import openpyxl
from openpyxl.styles import Font

PKG = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.abspath(os.path.join(PKG, '..', '..'))
sys.path.insert(0, PKG)
import states as STATE_REGISTRY                                          # noqa: E402
from make_state import PROBES_FINAL, in_use, static_union, verify       # noqa: E402
from make_vote import COMPARISON_NOTE, SAME_N_LABEL, START_NOTE, START_NOTE_ROW, WRAP   # noqa: E402
from make_vote_state import FOLDER, vote_expectations                    # noqa: E402
from workbook_layout import BLENDED_SHEET, FLAGGED_SHEET, FLAGGED_SHEET_VOTE, VOTE_SHEET   # noqa: E402

BUILD_DIR = os.path.join(PKG, '.build')
OLD_NOTE = ('On the testing data (Step 2), at the share in E4: the top cases by expected error $, ranked across all '
            'cases and within each group, against the rules with Include? = TRUE. All are optimistic here: the vote '
            'rules and the list rules were partly mined on these cases.')
OLD_LABEL = 'Top cases by expected error $ across all groups, as many as the Include? rules flag'


def patch(state):
    cfg = STATE_REGISTRY.get(state)
    dest_dir = os.path.join(REPO, 'custom_one_off', FOLDER.get(state, cfg['name'].lower().replace(' ', '_')))
    dest = os.path.join(dest_dir, f'SNAP_flagging_rules_{state}_vote.xlsx')
    xlsb = dest.replace('.xlsx', '.xlsb')
    out_dir = os.path.join(BUILD_DIR, f'out_{state}_vote')
    out = os.path.join(out_dir, f'SNAP_flagging_rules_{state}.xlsx')
    eff = os.path.join(out_dir, f'effective_rules_{state}_vote.csv')
    lay = json.load(open(os.path.join(out_dir, f'SNAP_flagging_rules_{state}_VOTE_layout.json')))
    rc = lay['comparison_row']                  # the first comparison row; the note sits two rows above it
    for p in (dest, xlsb):
        if in_use(p):
            raise SystemExit(f'{p} is open in Excel - close it and re-run.')

    wb = openpyxl.load_workbook(dest)
    vs, sh = wb[VOTE_SHEET], wb['Start Here']
    note, label = vs.cell(row=rc - 2, column=1), vs.cell(row=rc + 3, column=1)
    if note.value == COMPARISON_NOTE and label.value == SAME_N_LABEL and sh.cell(row=START_NOTE_ROW, column=1).value == START_NOTE:
        print(f'{state}: already carries the wording, skipped')
        return 'skipped'
    assert note.value == OLD_NOTE, f'{state}: note at A{rc - 2} is {note.value!r}'
    assert label.value == OLD_LABEL, f'{state}: label at A{rc + 3} is {label.value!r}'
    assert sh.cell(row=START_NOTE_ROW, column=1).value is None and sh.cell(row=START_NOTE_ROW, column=2).value is None
    note.value = COMPARISON_NOTE
    vs.row_dimensions[rc - 2].height = 48
    label.value = SAME_N_LABEL
    sh.merge_cells(f'A{START_NOTE_ROW}:B{START_NOTE_ROW}')
    c = sh.cell(row=START_NOTE_ROW, column=1, value=START_NOTE)
    c.font = Font(italic=True, size=11); c.alignment = WRAP
    sh.row_dimensions[START_NOTE_ROW].height = 60
    wb.save(dest)
    print(f'{state}: wording written to {dest}')

    # verify exactly as make_vote_state.build_vote does, with the vote build's effective list in place
    shared = os.path.join(BUILD_DIR, f'effective_rules_{state}.csv')
    saved = open(shared, 'rb').read() if os.path.isfile(shared) else None
    try:
        shutil.copy(eff, shared)
        total, anyv = vote_expectations(out, eff)
        g5, h5 = static_union(out, state)
    finally:
        if saved is not None:
            open(shared, 'wb').write(saved)
    probes = [p.replace(f'{FLAGGED_SHEET}!', f'{FLAGGED_SHEET_VOTE}!') for p in PROBES_FINAL
              if p not in (f"{BLENDED_SHEET}!G5", f"{BLENDED_SHEET}!H5")]
    probes += [f"{BLENDED_SHEET}!G5={g5}", f"{BLENDED_SHEET}!H5={h5}"]
    V = VOTE_SHEET
    probes += [f'{V}!J4={total}', f'{V}!J5={anyv}', f"{V}!J6={lay['per']}", f'{V}!E6',
               *[f'{V}!{c}' for c in lay['list_cells']],
               f'{V}!B7', f'{V}!C7'] + [f'{V}!{c}{r}' for r in (rc, rc + 1, rc + 2, rc + 3) for c in 'EFI']
    probes += ['Start Here!B4', 'Start Here!B5', 'Start Here!B6', 'Start Here!B8']
    verify(dest, probes)
    p = subprocess.run(['powershell', '-NoProfile', '-ExecutionPolicy', 'Bypass', '-File',
                        os.path.join(PKG, 'convert_xlsb_win.ps1'), dest, xlsb], cwd=PKG)
    if p.returncode != 0:
        raise SystemExit(f'xlsb export failed: {dest}')
    verify(xlsb, probes)
    print(f'{state}: done ({os.path.getsize(dest) / 1e6:.1f} MB and {os.path.getsize(xlsb) / 1e6:.1f} MB)')
    return 'patched'


if __name__ == '__main__':
    args = [a.upper() for a in sys.argv[1:]]
    if args == ['ALL']:
        args = [line.split()[2] for line in open(os.path.join(REPO, 'methods', 'vote_scoring', 'run_f1109_v281_all.cmd'))
                if line.startswith('call :one ')]
    res = {}
    for st in args:
        res[st] = patch(st)
    print('patched:', ' '.join(s for s, r in res.items() if r == 'patched'))
    print('skipped:', ' '.join(s for s, r in res.items() if r == 'skipped'))
