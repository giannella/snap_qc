"""
Build a state's SNAP QC dashboard end to end.

    python make_state.py                    # default state (WA), v1 builder
    python make_state.py VA --v2            # tiered, guarded tuning
    python make_state.py WA --v2 --recon    # + live table + raw-FNS-field Data tab
    python make_state.py all --v2 --recon   # every state with a delivery list
    python make_state.py WA --v2 --refresh  # re-export the frame from the rds first

Stages:
  1. build_workbook.py / build_workbook_v2.py   every sheet, formula and value
  2. make_live.py       (--live/--recon)  Data becomes an Excel table; Summary
                        tabs recompute from it, so pasted rows flow through
  3. make_recon.py      (--recon)  the Data tab's inputs become the raw
                        FNS QC-schedule fields; every model feature is an
                        in-workbook formula, for states that can run neither
                        Python nor R
  4. postprocess_workbook.py   native checkboxes + calc-chain removal, applied
                        to every workbook produced
  5. verify_workbook.py open-in-Excel probe (macOS only)

Close the workbook in Excel first: writing underneath an open session produces
OneDrive conflict copies and can clobber the new file.
"""
import json
import os
import subprocess
import sys

PKG = os.path.dirname(os.path.abspath(__file__))
BUILD_DIR = os.path.join(PKG, '.build')
PY = sys.executable
PROBES = ['Summary!D6', 'Summary!H6', 'Summary!I6',
          'Summary (National)!C6', 'Summary (National)!G6',
          'Error Cases!A1', 'Error Cases!A4']


def run(script, *args, env=None):
    print(f'\n$ {os.path.basename(script)} {" ".join(args)}')
    p = subprocess.run([PY, os.path.join(PKG, script), *args], cwd=PKG,
                       env={**os.environ, **(env or {})})
    if p.returncode != 0:
        raise SystemExit(f'stage failed: {script}')


def in_use(path):
    if sys.platform == 'win32':
        # no lsof on Windows: an exclusive-open probe catches an Excel lock
        try:
            with open(path, 'r+b'):
                return False
        except FileNotFoundError:
            return False
        except OSError:
            return True
    return subprocess.run(['lsof', path], capture_output=True).returncode == 0


def build_one(state, builder, want_live, want_recon, refresh, verify):
    env = {'SNAP_STATE': state}
    if refresh:
        env['SNAP_REFRESH_FRAME'] = '1'
    run(builder, env=env)

    out = json.load(open(os.path.join(BUILD_DIR, 'target.json')))['out']
    if in_use(out):
        raise SystemExit(f'{out} is open in Excel — close it and re-run.')
    produced = [out]

    if want_live or want_recon:
        live = out.replace('.xlsx', '_LIVE.xlsx')
        run('make_live.py', out, '-o', live)
        produced.append(live)
    if want_recon:
        recon = out.replace('.xlsx', '_RECON.xlsx')
        run('make_recon.py', live, '-o', recon, '--state', state)
        produced.append(recon)

    ck = os.path.join(BUILD_DIR, 'checkbox_cells.json')
    for wbk in produced:
        run('postprocess_workbook.py', wbk, ck)

    if verify:
        if sys.platform == 'darwin':
            run('verify_workbook.py', produced[-1], *PROBES)
        elif sys.platform == 'win32':
            for wbk in produced:
                print(f'\n$ verify_workbook_win.ps1 {os.path.basename(wbk)}')
                p = subprocess.run(
                    ['powershell', '-NoProfile', '-ExecutionPolicy', 'Bypass',
                     '-File', os.path.join(PKG, 'verify_workbook_win.ps1'),
                     wbk] + PROBES, cwd=PKG)
                if p.returncode != 0:
                    raise SystemExit(f'verify failed: {wbk}')
        else:
            print('verify stage skipped (no Excel automation on this platform); '
                  'open the workbook once in Excel before shipping it')
    print(f'\nDone: {" | ".join(produced)}')


def main():
    args = [a for a in sys.argv[1:] if not a.startswith('-')]
    target = (args[0] if args else 'WA').upper()
    builder = 'build_workbook_v2.py' if '--v2' in sys.argv else 'build_workbook.py'
    want_recon = '--recon' in sys.argv
    want_live = '--live' in sys.argv or want_recon
    refresh = '--refresh' in sys.argv
    verify = '--no-verify' not in sys.argv

    if want_recon and builder != 'build_workbook_v2.py':
        raise SystemExit('--recon requires --v2 (the v1 Data tab is not '
                         'the munged-frame layout make_recon expects)')

    if target == 'ALL':
        import states as STATE_REGISTRY
        todo = STATE_REGISTRY.all_abbrs()
        print(f'building {len(todo)} states: {" ".join(todo)}')
        failed = []
        for st in todo:
            try:
                build_one(st, builder, want_live, want_recon, refresh, verify)
            except SystemExit as e:
                print(f'FAILED {st}: {e}')
                failed.append(st)
        if failed:
            raise SystemExit(f'{len(failed)} state(s) failed: {" ".join(failed)}')
    else:
        build_one(target, builder, want_live, want_recon, refresh, verify)


if __name__ == '__main__':
    main()
