# Design note: utilities SUA-tier vocabulary variant, one-year-ahead (2026-08-21)
# (folder slug 'utilrel' kept for continuity with the review history)

**Question (one sentence).** Does encoding utilities as a 3-level SUA TIER
against the state-year's MODAL positive utility value (utilities_sua:
0 = none, 1 = positive below mode - 200, 2 = at or above mode - 200, the
HIGH-SUA cluster) remove the utilities-family reach collapse that dollar
thresholds carry across SUA level resets, without costing budget-level
list performance one year ahead?

**The $200 HIGH band: a v1 engineering parameter, not a study subject
(project lead, 2026-08-21).** Tier 2 opens $200 below the mode so that
within-state HIGH-SUA variants land in the high tier: several states run
household-size schedules (Virginia heating/cooling 375 for hh 1-3 vs 476
for 4+) or regional schedules (New York 877 / 988 / 1,062), where the
variants sit close to each other relative to the lower allowances; any
state with multiple schedule rows has the pattern (published state SUA
structures, checked 2026-08-21). Why $200 looked sensible: the band
census (methods/vocab_hygiene_census/utilities_high_band_census.csv, 147
state-years) shows [mode - 200, mode) is essentially empty in most states
(median 0.8% of positive cases) and carries 10-47% exactly in the
multi-schedule states (NY 37-41%; LA, MA, MI, IN, GA, ME, MD, FL over
10%), so a uniform band captures the variants where they exist and is a
no-op elsewhere. Known imperfections, recorded as facts: in a few states
the band could absorb a genuinely lower allowance within $200 of the
mode, and a fixed dollar band is proportionally looser where the mode is
low (73 of 147 era-2 cells have mode <= 400 vs 39 of 147 in era 1; no
cell in either era has mode <= 200, so tier 1 never empties by
definition). A program-expert colleague is doing a deeper dive on the SUA
structure; that work supersedes this parameter when it lands, and the
published FNS SUA tables are the natural anchor for it. The era-2
replication uses this same v1 construction for comparability. Readout 3
also reports per-state tier-1 mass beside family representation (the band
thins tier 1 exactly in the multi-schedule states).

**Form decision (project lead, 2026-08-21, option 2 of three offered).**
The mined variable is the TIER, not the continuous ratio. Recorded
trade-off: the tier admits only two possible cuts (>= 1: any allowance;
>= 2: at/above the dominant SUA), so interval fishing on it is impossible
by construction and the shipped width floor needs no extension to cover
it; the cost is discarding within-tier variation (median modal share 0.88
of positive-utilities cases, so ~12% spread across non-modal tiers). The
continuous-ratio form was NOT mined tonight and remains an untested
option; the translation test's modeA arm (re-anchored continuous cuts,
collapse 1.35%) is evidence about the ANCHOR and partially about ratio
viability, not about the tier. If the tier fails its bars, the ratio form
is the designed fallback, not a post-hoc rescue: it would get its own
design note and review.

**Motivation, cited.** Census 2026-08-21 (methods/vocab_hygiene_census):
utilities appears in 1,000 of 3,214 delivered core rules, and ~73% of the
~900 in tier-valued states (2-10 distinct positive values per state-year)
carry a threshold whose tier position changes across FY2022-24. The
decay-by-variable exploratory cut (methods/interval_width_decay/
decay_by_variable_exploratory.csv): utilities rules show NO excess
precision decay on either era (excess -0.003 / +0.004) but reach-collapse
at roughly twice the reference rate on both eras (4.2% vs 2.2% on
2022-23 -> 2024; 2.9% vs 1.6% on 2017-18 -> 2019). The remedy pattern is
the frame's own rawben_rel_max construction (munging line ~916).

**Anchor retarget (2026-08-21, same day, BEFORE launch).** The denominator
is the state-year MODE of positive utilities (the dominant SUA), not the
max. Two pre-mining assessments decided it: (a) anchor census
(methods/vocab_hygiene_census/utilities_anchor_census.csv, 294 state-years
across both eras): the mode is well-identified (median modal share 0.88 of
positive-utilities cases) and stable (year-over-year log-ratio sd 0.077 vs
0.391 for the max; the max moved >20% in 45 of 196 transitions vs 14);
(b) translation test (methods/interval_width_decay/
anchor_translation_summary_2024.csv, 4,159 in-band utilities rules,
thresholds re-anchored by each anchor's growth and evaluated on FY2024):
reach-collapse raw 4.18% -> max-anchored 3.05% -> MODE-ANCHORED 1.35%
(below the 2.24% non-utilities reference), with median precision decay at
baseline for the mode (-0.0217 vs raw -0.0230) and WORSE for the max
(-0.0283). External structure check (USDA state-options panel, 2024
edition): 48 of 53 jurisdictions mandate SUAs; the two most many-valued
states in our frame (Virginia, 54 distinct values; Tennessee, 18) are
among the 5 non-mandatory, while several mandatory states carry 12-17
values (FNS permits household-size/region variation), where the mode is
the dominant level and other levels sit at ratios of it. The published FNS
FY SUA tables are the later refinement path for an externally-anchored
denominator; v1 self-computes the mode per state-year from the data, which
is also the deployment semantics.

**What varies, with exactly one component varying.** One arm is mined:
the v2.5.0 benchmark recipe verbatim (FY2022-23 national + 49 state
any-error mines, seed 117, joint BH FDR 10% + n >= 30, 99% LCB ordering,
artifact gates, shipped blend/fill/fresh-share walk, FY2024 cap-walk
scoring) with `utilities` replaced by `utilities_sua` in the 19-variable
vocabulary. Nothing else moves. The comparison arm is FREE: the committed
baseline benchmark (methods/v250_benchmark_2024/v250_benchmark_2024.csv,
same recipe, same seed, same frame, raw utilities). The variant mines into
its own OUT_DIR/cache (methods/v250_benchmark_2024_utilrel/); baseline
caches, candidate lists and state_delivery_lists/ are untouched. The
driver's inherited v2.4.0 paired-delta section is NOT this study's readout
(it spans many components); it is ignored.

**Support after the split (computed before the run).** Feasibility checked
on the FY2022-24 frame slice (115,559 rows): utilities has 0 NAs, 0
negatives, 32,281 zeros; all 147 state-year cells have a finite positive
max (313 to 1,775), so utilities_sua is well-defined everywhere (no NA tiers) and no row loss occurs.
Train FY2022-23: 76,031 rows, 8,397 errors; held-out FY2024: 39,528 rows,
4,764 errors (asserted in the driver). Per-unit pool sizes and admitted
counts print per state as the mine runs; the prep_features vocabulary
assert halts if utilities_sua goes missing or constant.

**Readouts and pre-registered bars (decided BEFORE the run).**
1. LIST NON-INFERIORITY (the shipping gate for the vocabulary change,
   same template as the width-floor study): variant vs baseline benchmark,
   paired per state x budget. Ship-eligible iff, at BOTH budgets and for
   BOTH precision and dollar recall: median paired change >= -0.005,
   mean >= -0.01, zero states harmed (< -0.05). Positive medians are
   one-era observations, never improvement claims.
2. TARGETED MECHANISM CLAIM (reference definition pinned per the
   2026-08-21 pre-run review, which caught that the earlier ~1.9x anchor
   mixed reference definitions): the FAMILY is the variant national pool's
   admitted rules in the train-n band [30, 300] that condition on
   utilities_sua; the REFERENCE is the same pool's admitted in-band rules
   that do NOT condition on utilities_sua, both evaluated on the national
   FY2024 slice. Bar: family reach-collapse rate (share flagging < 10
   FY2024 cases) BELOW 1.5x the reference rate. On this definition the
   raw-encoding baseline is ~3.0x on BOTH eras (era 1: 4.18% vs 1.41% =
   2.96x; era 2: 3.31x), so 1.5x requires the encoding to at least halve
   the mechanism. If the family stays >= 1.5x the reference, the encoding
   did not fix the mechanism and the change does not ship regardless of
   readout 1. (Computed by a follow-up per-rule script on the variant
   cache, the section-40 machinery reused; the definition above is
   non-negotiable after the run.)
   INTERPRETATION UNDER THE TIER FORM (added per the second addendum
   ruling, before the run): two-cut tiers cannot drift across an SUA
   reset, so threshold drift is removed BY CONSTRUCTION and a pass here is
   weaker evidence than the same pass would have been for the ratio form.
   The bar is read as a residual-fragility check: a failure implicates
   thin tier-1 slices or tier RELABELING where the mode itself moved
   (14 of 196 census transitions moved >20%), not dollar-threshold drift.
   The follow-up script must report the family's in-band rule count with
   an exact binomial CI on its collapse rate (the family may be far
   smaller than the raw encoding's 4,159 rules; the bar stays the
   pre-registered point comparison, but a small-family rate must be
   visible as such). Readout 3 is consequently the primary lens on the
   coarsening cost.
3. FAMILY REPRESENTATION MONITOR (added 2026-08-21 at the project lead's
   direction: the goal is keeping utilities rules in the lists, not just
   fixing fragility): report the utilities_sua-conditioning share of the
   admitted national pool and of the blended core lists vs the baseline's
   utilities shares (pool 26.9%, delivered core 31.1%, census 2026-08-21).
   A monitored readout, not a ship/no-ship bar; an order-of-magnitude
   collapse in family admission is a design failure to investigate even if
   readout 1 passes (substitution by other variables would mask it).
4. Companions and conventions: within-state median decides; mean and
   harmed-tail accompany; any-error = frame-relative by construction on
   this pool.

**Deployment-consistency note (recorded per the review; restated for the
tier form).** The study encodes the anchor PER STATE-YEAR. A workbook
implementation must do the same: one anchor computed over a pasted window
that spans an SUA reset would mis-tier the older period. Tier mass on the
FY2022-24 slice: 27.9% of rows at tier 0 (no utility amount), and among
positive rows the modal cases (58.7% of all rows sit at exactly the mode)
land in tier 2 with the above-mode remainder; tier 1 carries the sub-modal
tail. The ratio-form mass-point monitor from the addendum ruling
([0.995, 1.005] thresholds) is moot for the tier: thresholds on a 0/1/2
variable are canonicalized integer cuts, and the mm-share artifact audit
and gates stay on as the guard. mode_pos semantics,
pinned: positive values only, rounded to integer, ties broken to the
SMALLEST tied value (R's stable sort over ascending levels); verified on
FY2022-24 that utilities values are integral and no state-year cell has a
tied top count, so the tie rule never fires in this run. Deployment
caveat: Excel's MODE.SNGL breaks ties by first occurrence in data order,
so a workbook implementation must replicate positive-only + integer-round
+ a documented tie rule. The era-2 design note must re-run the
tie/integrality check on the FY2017-19 cells before that launch.

**Reading limits, pre-stated (wording settled with the project lead,
2026-08-21).** The mode anchor was chosen on outcome-free grounds: the
project lead's SUA-structure reasoning plus the descriptive anchor census
(modal share and year-over-year stability, no error outcomes involved),
which alone favored the mode. HOWEVER, a FY2024-outcome comparison of the
anchors (the translation test) was also run and read before launch, on the
same holdout tonight's bars judge. So tonight's FY2024 read is not a first
look at this holdout for this design area, and a cleared bar is read with
that in mind: the FY2017-18 -> FY2019 replication, where no outcome ever
raced the anchors, is the clean confirmatory test before anything ships.
Single seed (117), single era: this run
prices the encoding one year ahead on 2022-23 -> 2024 only. Per the
two-era discipline (findings 20), the vocabulary change does NOT ship from
this run alone; if both bars clear, the era-2 replication (FY2017-18 mine
-> FY2019) is the next overnight before any delivery-list change. Ledger
hazard row on rawben_rel_max (max-relative constructions near a
reconstruction boundary) is noted: the artifact tag/gate system stays on
and utilities_sua gets the same mm-share audit as every feature.

**What the ledger already says.** Vocabulary rows: 19-var per-size vocab
settled (v2.5.0, section 39); bbce_state_i regime-flag precedent for
state-year-derived features (section 39); count_divisible_by_100 dropped
from workbooks at build time (2026-08-18, engineering). Nothing retired is
re-opened: ordering, admission, strata, walk all held at settled values.

**Runtime and scheduling.** Fresh mine, ~4 h on this host (v250_cycle2
precedent: 3h44m). Mining is loud: launch ~22:00 per the overnight policy.
Checkpointed per unit; RESUME_FROM_CHECKPOINT resumes a killed run.

