# Deck style pass (2026-07): remove em-dashes from slide text.
#
# The decks are binary .pptx, so this script IS the reviewable record of the text
# change. It only touches em-dashes ("—"); it does not alter numbers, wording,
# or the " - " hyphen-dash connectors (those need per-case judgment and are a
# separate pass). Almost every deck em-dash is a "term - definition" separator, so
# the replacement is a colon; a couple are "title - subtitle", also a colon.
#
# Idempotent: re-running finds nothing to change. Edits runs in place to preserve
# formatting; reports every change.
#
#   python methods/deck_style_pass.py
import glob
from pptx import Presentation

DECKS = sorted(glob.glob("*.pptx"))
DASH = "—"

def fix_run(text):
    # " - "-style em-dash use -> ": "; bare em-dash -> ":"; collapse doubled spaces.
    t = text.replace(" " + DASH + " ", ": ").replace(DASH, ": ")
    while "  " in t:
        t = t.replace("  ", " ")
    return t

total = 0
for deck in DECKS:
    p = Presentation(deck)
    changed = 0
    for si, slide in enumerate(p.slides):
        for shape in slide.shapes:
            if not shape.has_text_frame:
                continue
            for para in shape.text_frame.paragraphs:
                for run in para.runs:
                    if DASH in run.text:
                        before = run.text
                        run.text = fix_run(run.text)
                        changed += 1
                        print(f"  [{deck} s{si}] {before!r} -> {run.text!r}")
    if changed:
        p.save(deck)
    print(f"{deck}: {changed} run(s) de-em-dashed")
    total += changed
print(f"TOTAL runs changed: {total}")
