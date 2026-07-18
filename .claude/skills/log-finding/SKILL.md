---
name: log-finding
description: Record an empirical finding from a run or study into methods/modeling_findings.md using the repo's evidence conventions (verified numbers, artifact pointers, honest caveats)
---

You are recording an empirical finding for the SNAP QC rule-mining project.
The finding is described in the invocation arguments (or, if absent, take the
most recent completed run/study discussed in the conversation).

## Steps

1. **Verify before writing.** Re-derive every number you are about to record
   from its artifact (CSV, log, RDS), never from memory of the conversation.
   If a number cannot be traced to a file in the repo, do not record it.

2. **Write into `methods/modeling_findings.md`:**
   - If the finding extends an existing numbered section, add to it (as the
     calibration note in §1 does). Otherwise append a new numbered section.
   - Structure: WHAT was compared (configurations, data, years), the numbers
     (held-out wherever available; both frame-relative and any-error when the
     metric is precision/recall), the conclusion in one sentence, then
     *Artifacts:* with relative paths to the CSVs/figures/scripts.
   - Include the caveats that survive scrutiny (e.g., in-sample flattering,
     era confounds, small support). A finding without its caveat is a claim
     we will have to walk back later.
   - Date the entry (YYYY-MM-DD).

3. **Propagate:**
   - If the finding changes a default (engine setting, floor, stratum scheme),
     update the CLAUDE.md knob table / architecture notes to match.
   - Check whether the decks (`how_to_use_*.pptx`, `workshop_*.pptx`,
     `lessons_*.pptx`) quote numbers this finding supersedes. Do NOT edit
     decks silently; list the affected slides for the user.

4. **Style:** follow the "Presentations and write-ups" section of CLAUDE.md,
   modeling conclusions only, every claim carries its measurement, no
   slogans, plain English. Entries should be diff-friendly (wrap lines,
   no reflowing of untouched text).
