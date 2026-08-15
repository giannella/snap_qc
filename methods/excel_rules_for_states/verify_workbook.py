"""
Stage 3 — open the finished workbook in Excel and prove it is healthy.

Excel is the only authority on whether a hand-assembled xlsx is valid: it
silently "repairs" files that pass every XML check.  This driver opens the
workbook, reads back a few cells, and closes it WITHOUT saving.

Usage:  python verify_workbook.py <workbook.xlsx> [Sheet!A1 Sheet!B2 ...]

Exit code 0 = opened cleanly.  macOS only (drives Excel through AppleScript).
"""
import os
import re
import subprocess
import sys
import time

TIMEOUT = 600


def _osa(script, timeout=TIMEOUT):
    try:
        p = subprocess.run(['osascript', '-e', script],
                           capture_output=True, text=True, timeout=timeout)
        return (p.stdout.strip() or 'ERR: ' + p.stderr.strip())
    except subprocess.TimeoutExpired:
        return 'TIMEOUT (Excel is showing a blocking dialog — the file is probably damaged)'


def quit_excel():
    subprocess.run(['pkill', '-x', 'Microsoft Excel'], capture_output=True)
    time.sleep(4)


def verify(path, probes=(), settle=15):
    path = os.path.abspath(path)
    reads = ''
    for probe in probes:
        sheet, ref = probe.split('!')
        reads += (f'\n    set out to out & " | {probe}=[" & (string value of cell "{ref}" '
                  f'of worksheet "{sheet}" of wbk) & "]"')
    quit_excel()
    res = _osa(f'''tell application "Microsoft Excel"
  set display alerts to false
  try
    open workbook workbook file name "{path}"
    delay {settle}
    set wbk to active workbook
    set out to "OPENED"{reads}
    close wbk saving no
  on error e
    set out to "FAILED: " & e
  end try
  out
end tell''')
    quit_excel()
    return res


def main():
    if len(sys.argv) < 2:
        raise SystemExit(__doc__)
    res = verify(sys.argv[1], sys.argv[2:])
    print(res)
    ok = res.startswith('OPENED')
    print('VERIFY: PASS' if ok else 'VERIFY: FAIL')
    sys.exit(0 if ok else 1)


if __name__ == '__main__':
    main()
