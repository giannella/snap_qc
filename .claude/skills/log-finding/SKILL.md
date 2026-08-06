---
name: log-finding
description: Record an empirical finding from a run or study into the project's findings docs — a plain-language takeaway in methods/modeling_findings.md plus the full evidence in methods/modeling_findings_detailed.md — using the repo's evidence conventions: verified numbers, every arm of every comparison shown, artifact pointers, honest caveats.
---

You are recording an empirical finding for the SNAP QC rule-mining project.
The finding is described in the invocation arguments (or, if absent, take the
most recent completed run/study discussed in the conversation).

The audience for what you write is a data scientist with a few years of experience
at a state SNAP agency who did NOT watch the run and has NOT read the code. Write so
that person can reconstruct the result — every comparison, on what data, how sure —
without asking you a follow-up question.

## Where it goes (two files, kept in sync)

- **`methods/modeling_findings_detailed.md`** — the complete evidence log: full
  numbers, every arm of every comparison, tables, caveats, and artifact paths. This
  is the source of truth. Write it first.
- **`methods/modeling_findings.md`** — the plain-language version: a tagged
  **Takeaway**, the load-bearing numbers only, and a link to the detailed section.
  No artifact-path dumps here.

The Takeaway blockquote text is identical in both files.

## Steps

1. **Verify before writing.** Re-derive every number from its artifact (CSV, log,
   RDS), never from memory of the conversation. If a number cannot be traced to a
   file in the repo, do not record it.

2. **Draft the detailed entry** in `methods/modeling_findings_detailed.md` — extend
   an existing numbered section if the finding belongs to one, else append a new
   numbered section. Then run the completeness checklist below over it and fill every
   gap before moving on.

3. **Write the plain-language layer** in `methods/modeling_findings.md`: the same
   section number and header, a 1-3 sentence **Takeaway** tagged either *about the
   data* (a result likely to hold regardless of pipeline) or *about our pipeline* (a
   modeling choice that may not generalize), the few numbers that carry the point, and
   a `→ detailed record §N` link. Keep the load-bearing comparison complete here too —
   if a number would make the reader ask "compared to what?", it belongs in the
   takeaway layer, not just the record.

4. **Propagate (this is how the docs avoid drift):**
   - **Update the ledger.** `methods/findings_ledger.md` carries one row per claim
     (status, tested scope, section citation). Add or update the row(s) this finding
     touches: a new claim gets a row, a replication updates the scope column, a
     retirement flips the status. A finding without a ledger row is invisible at
     planning time.
   - **Update the constraints file if the finding is operational.** If the finding
     adds, changes, or retires a do/don't on one of the protected pipeline files,
     reconcile the matching section of `methods/known_constraints.md` (the hook in
     `.claude/settings.json` injects it on every edit to those files).
   - **Update GUIDANCE.md if the row is in its scope.** GUIDANCE.md renders the
     ledger's deployment-relevant settled / option / hazard rows for state
     analysts. After updating the ledger row, ask one question: is this row in
     that scope? If yes, add or reconcile the matching GUIDANCE point (each point
     carries its `§N` and closes with status + tested scope); if the row was
     retired, the point must come out and its phrasing goes to
     `methods/retired_claims.txt`.
   - **Cite, don't restate.** The findings docs own every number. When a reader doc
     (README.md, GUIDANCE.md, a deck) states a number, it must carry its source
     `§N` citation. Never let a derived doc originate a number or silently diverge
     from the findings value.
   - If the finding changes a default (engine setting, floor, stratum scheme), update
     the CLAUDE.md knob table / architecture notes to match.
   - **If the finding supersedes or retires a prior claim,** grep the derived docs for
     the old claim, number, or term and reconcile each: run `grep -n` across
     README.md, GUIDANCE.md, and the deck text. A retired claim left standing in a
     derived doc is the main way these docs drift (it is exactly how a strata claim
     retired in the findings stayed asserted in GUIDANCE). Add the old assertion to
     `methods/retired_claims.txt` so the consistency checker guards against its return.
   - Check whether the decks (`slides/how_to_use_*.pptx`, `slides/workshop_*.pptx`,
     `slides/lessons_*.pptx`) quote numbers this finding supersedes. Do NOT edit decks
     silently; list the affected slides for the user.
   - Before finishing, run `methods/check_doc_consistency.sh` (flags em-dashes and any
     retired phrasing that reached a reader doc). At each validated-change or release,
     also re-run the full reader-expectation audit (numbers-trace, completeness,
     right-altitude), not just this checker.

5. **Date** the entry (YYYY-MM-DD) and add a one-line breadcrumb to §0's chronology.

## Completeness checklist — write these or the finding is incomplete

These are the omissions that have repeatedly made past findings unreadable to someone
who wasn't at the console. Every item is mandatory for the detailed entry; carry the
ones that bear on the headline into the plain-language takeaway too.

- [ ] **Every arm, standalone.** Name each configuration compared and give each one's
      own number — including the baseline and *each component* of any combined /
      pooled / blended / best-of / paired result. If you report "combined = A + B,"
      report A alone and B alone. (The bug that started this checklist: a table showed
      "typed → combined" and never named or scored the any-error arm that "combined"
      was combining.)
- [ ] **Both endpoints of every comparison.** No bare deltas or one-sided numbers.
      "A beats B" records A's value and B's value; "2x", "~1/3", "+3.5pp", "N× lift"
      each record the two raw numbers behind them.
- [ ] **The metric's expected companions.** Precision with recall AND the base rate
      (so lift is legible); error dollars with case counts; a filter-floor result with
      the review-budget (5% / 10% of caseload) result; frame-relative with any-error
      precision.
- [ ] **Define every option/term at first use** in the entry — qualification bars
      ("the bar"), pool names (national-as-is / LOO / own-pool / NB / fire / IDF /
      policy), core/buffer, stratum labels. Don't make the reader look elsewhere.
- [ ] **Test design, stated.** Train year(s) / test year; whether the test year is
      *interpolated* (sits between the training years, which flatters every option) or
      a true future / held-out year; sample sizes and rule support.
- [ ] **Replication & falsification status.** Pre-registered? Replicated on a second
      era or year? What was the pre-set bar, and did it clear it? Record
      non-replications and retractions as first-class results, not footnotes.
- [ ] **What did NOT move.** The null, refuted, and "within noise" arms belong in the
      record too — omitting them is how the winner's curse creeps back in.
- [ ] **Caveats that survive scrutiny**, plus any supersession pointer
      ("superseded by §N").
- [ ] **Artifact path + regenerating script** for every number.

**Smell test before saving:** imagine the state data scientist reading only this
entry. Can they answer "compared to what?", "what did each piece do on its own?", and
"how sure are we, and on what data?" without asking you? If any answer requires
information you didn't write down, a number is missing.

## Style

Follow the "Presentations and write-ups" section of CLAUDE.md: modeling conclusions
only, every claim carries its measurement, no slogans, plain English, define terms
inline once. Entries are diff-friendly — wrap lines, don't reflow untouched text.
