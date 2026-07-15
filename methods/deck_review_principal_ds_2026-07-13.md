# Principal data scientist review of RevC (2026-07-13)

Reviewer persona: principal-level applied-ML / decision-science IC with
public-sector presentation experience; brief was organization, decision-
usefulness, chart craft, framing, and gaps fillable without new runs.
Review produced against viz_exploration/revC_text_dump.txt +
methods/modeling_findings.md.

## 1. The one-sentence test

As built, the takeaway is: "We learned rigorous modeling lessons and
honestly retired the ones that didn't replicate." That is a
methods-retrospective takeaway, and it's the wrong one for the adoption
goal. The right takeaway — supported by numbers you already have — is:
"Here is a frozen, ranked rule list for your state that beat your base
rate 1.5-3.4x on a year it never saw (slide 24); running it against your
FY25/26 files takes no outcome data and no modeling." The deliverable and
the ask don't appear until slides 31 and 39 — slide 39 of a 40-slide
body. State the ask on slide 2 ("we are asking you to run one check;
here's what it showed on 2024"), then let the methods slides serve as
evidence for why the list can be trusted. Right now the deck asks the
audience to sit through the winner's curse before learning why they're in
the room.

## 2. Organization & momentum

The arc is Parts 1-2 strong, Part 3 sagging, Part 4 rushed.

- Move slide 17 (evaluation ladder) to ~slide 5. Stranded mid-Part-2 it
  can't do its job as a reading key. Up front, every later slide can
  carry a small "rung" tag.
- Part 3 (slides 19-29) is where a practitioner audience checks out.
  Eight-plus slides walk through SUPERSEDED experiments: Louisiana
  neighbors (21), similarity that "vanished" (22), the two-regime
  "precursor" (26), three adaptation schemes that don't beat national
  (27-29). The audience needs one slide: "We tested own-state mining,
  neighbor transfer, and three adaptation schemes head-to-head on 2024;
  none beat the national ordering for the median state (table);
  own-state is high-variance (CT 0.416, WA below base rate)." Findings
  #14's table is that slide. Keep slide 20 (support-floor lesson). Cut 26
  and 29 outright (29 is a schematic drawn after its conclusion slide).
- Merge slides 15 and 16 (15 is pure setup and repeats the +47%).
- Slide 5 ("What we ran") is a laundry list; fold it into the ladder.
- Slide 14 (mtry, eta) -> appendix; it changes no state decision.
- Reorder so 24-25 (state-by-state deployment + intervals) arrive right
  after Part 2 — the single most persuasive content in the deck.
- Net: the 40-slide body should be ~28-30. Slides 10-11 and 24-25 are
  the spine; protect them.

## 3. Decision-usefulness

Slide 39 is the Monday-morning slide and it's underpowered. A state
analyst still doesn't know:

- What the rules physically are and how hard mapping is. Rules are in
  QC-variable vocabulary; mapping to a state's eligibility-system fields
  is the real cost. A "typical rule uses 4-6 of these ~N variables"
  statement is producible today from the frozen list CSVs.
- A quantified success bar. "Comfortably above base rate" is vague.
  Pre-register the ~1/3 deflation expectation (findings #9: train 0.33 ->
  holdout 0.21) and give an interval-based pass/fail.
- Their own state's expectation. Slide 37's tables show rule COUNTS —
  the least decision-relevant numbers available. Replace with per-state
  expected precision and dollar share vs base rate
  (frozen_list_results.csv / blended_frozen_results.csv have all 18).
- Who to contact and in what format results come back. One line fixes it.

## 4. Chart & table craft

- Single highest-value annotation: on the slide-24 dot plot, connect each
  state's base rate to its delivered precision (dumbbell), labeled once:
  "open dot = what random review gets." Makes 1.5-3.4x legible without
  the word "lift" and makes slide 25's interval argument visual.
- Slide 34's table: drop the "rules admitted by the scan" column — the
  ~45k figure invites confusion you then spend a bullet defusing. Lead
  with "23 rules at 5%."
- Slide 8: 6-row table when visibility_by_state_2022_2024.csv has every
  state — show the full sorted dot plot; every state will look for
  itself.
- Slide 13: show the 2024 replication, footnote the original (two
  stacked figures dilute).
- Slide 23: the medians belong in a 2x3 table, not prose.
- Slide 37: two count tables -> one table of expected performance per
  state.

## 5. Language & framing

- Slide 3: cut "Claude (probably the others) makes this easy" — a vendor
  aside undercuts credibility; "old school decision trees" is sloppy.
- Slide 30 is a framing landmine: "benefits-near-max and deduction
  levels dominate" — the house style itself says states treat deduction
  errors as low-value. Pre-empt: show dollar share alongside, or note
  which rules catch which error types.
- "Honest" appears ~7 times; once is credible, seven reads as
  protesting. Let the replication and the retired claim do the talking.
- "Lift over random review" is quietly naive: states don't review
  randomly — many already target. The honest comparison is "vs your
  current targeting," which only their internal check supplies. Saying
  so STRENGTHENS the validation ask. Add to slide 38.
- Jargon earning its place: "lower confidence bound" (slide 11's 7-of-10
  example is excellent). Not earning it: "mtry," "eta," "frame,"
  "budget-filled" (say "filled to review capacity"), and slide 10's
  headline "delivered half of it" (unclear antecedent — say "delivered
  half the promised precision").

## 6. Missing content (producible now)

1. An actual rule, early. Nowhere before slide 30 does the audience see
   a rule. Put one plain-English example on slide ~3 from
   frozen_lists/ — program experts judge face validity before
   statistics.
2. Full-state visibility table + findings #10's guidance: states below
   ~60% visibility (NJ 43, TN 51, AR/MO/UT ~53) should treat these rules
   as a supplement and mine internally. Currently only NJ is mentioned,
   as a blend footnote.
3. The elderly/non-elderly coverage gap (findings #8): union recall
   26.7% of elderly-household errors vs 7.2% of others. The audience
   WILL notice; disclose on slide 38.
4. Which precision basis is quoted (findings #6: frame-relative
   understates deployed ~2x). One footnote prevents a sharp analyst from
   catching an apparent inconsistency.
5. Practicality of internal mining (findings #2): a few GB, 16 GB
   laptop, ~45 min — directly relevant to the low-visibility states told
   to mine internally.
6. Pre-registered deflation expectation for the validation bar (#9's
   ~1/3 figure).

## 7. Top 5 changes (ranked by leverage)

1. Put the ask and the 2024 state-by-state result on slide 2 — make
   adoption, not methods, the through-line.
2. Collapse Part 3's superseded experiments (21-22, 26-29) into one
   head-to-head slide using findings #14's table.
3. Rebuild slide 39 into a real protocol: fields needed, effort
   estimate, quantified pass/fail with the ~1/3 deflation expectation,
   contact.
4. Add base-rate dumbbells to the slide-24 dot plot — the one annotation
   that makes the whole case visual.
5. Show a plain-English rule by slide 3 and fix the slide-30 deduction
   framing — face validity is what this audience trusts first.

"The statistician made the deck rigorous; these changes make it
adoptable. The evidence is already strong enough — the deck just leads
with the journey instead of the destination."
