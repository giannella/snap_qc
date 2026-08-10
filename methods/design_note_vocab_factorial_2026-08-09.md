# Design note: national vocabulary factorial, 2x2 (2026-08-09, evening run)

Follows `methods/design_note_vocab_attribution_2026-08-08.md` (the two-arm
study) and its results (`methods/vocab_attribution_v2/README.md`). Eric's
decisions of 2026-08-09, made interactively: the 26-feature package is
adopted on artifact-independence grounds (its deployment cost ruled worth
it); tonight runs the NATIONAL factorial only (the state-typed arm is cut
from tonight by Eric's instruction); the outlier arm reduces to ONE
variable, `shelter_expenses_p`, because the package already carries
continuous percentiles for four of Ben's five issue-7 outlier variables
and a depth-4 tree expresses ">= p99" on a continuous percentile by
itself.

## 1. The question (one sentence)

Does adding a frozen train-year shelter-expense percentile
(`shelter_expenses_p`, from post-adjustment rent + utilities - the one
issue-7 outlier variable the package cannot express) improve delivered
budget-list performance on FY2024, on top of the 16-feature baseline
and/or on top of the adopted 26-feature package?

## 2. What varies (2x2, each contrast one component)

| arm | vocabulary |
|---|---|
| base | the 16 shipped-in-practice features |
| cand | 16 + the 10-package (adopted 2026-08-09) |
| ps_pure | 16 + the 3 per-size income features: ALL FIVE _by_hh_size fields (total_deductions, shelter_expenses, gross, earned, unearned per size), zero percentiles |
| pct_pure | 16 MINUS its two _by_hh_size features, PLUS percentile counterparts of all five fields (rawgrinc_p, rawearn_p, rawunearn_p, total_deductions_p, shelter_expenses_p) and the remaining four percentiles (rawrent_p, rawmedded_p, rawcsded_p, rawdepded_p): 23 features, zero _by_hh_size |
| base_slt | 16 + shelter_expenses_p |
| cand_slt | 16 + package + shelter_expenses_p |

The pct_pure arm removes two incumbents AND adds nine columns; that is
deliberate and still one component: the component under test is the
NORMALIZATION REPRESENTATION of the same underlying dollar fields
(divide-by-household-size vs rank-within-state-x-size-cell). Eric
corrected the earlier additive design on exactly this point (2026-08-09):
the 16 already carries two per-size features, so an additive percentile
arm would compare percentiles-on-top-of-per-size, not the
representations as options. `total_deductions_p` is new, built by the
same frozen train-year machinery from `total_deductions`.

Pre-registered contrasts, paired by seed: (1) base_slt - base and
(2) cand_slt - cand isolate the shelter feature; (3) cand - base is the
free same-seed replication of last night; (4) ps_pure - base isolates
the per-size income additions; (5) pct_pure - base tests the full
percentile representation as a replacement; (6) pct_pure - ps_pure is
the head-to-head between the two normalization PACKAGES (Eric's
request). Pre-stated before results exist (delta review flag 1,
2026-08-09): this contrast is packages, not representation per se —
pct_pure additionally carries four component-level percentiles
(rawrent_p, rawmedded_p, rawcsded_p, rawdepded_p) with no per-size
counterpart in ps_pure, so a pct_pure win could be the representation
or the component granularity. Attribution to representation requires
checking the usage table and deployed-rule text for whether those
counterpart-less percentiles carry the difference; if they dominate,
the finding is logged as "percentile package wins," never
"representation wins." The
family arms carry the same reading rules as the package: deltas judged
against seed noise with the full companion set; the percentile
representation additionally carries the positive-bar rule (it alone
requires per-state cutoff tables; the per-size representation is
table-free arithmetic). Everything else held fixed
exactly as the 2026-08-08 note: rebuilt frame, engines, strata,
admission, ordering, legacy findings-31 walk, 20k window, budgets 5/10%,
ten-state panel, seeds 117 / 20260805 / 31415.

**Cache reuse**: the base and cand mines and scored pools are last
night's checkpoints (identical frame, config, vocabulary, seeds), per
review flag 4's condition - configs unchanged, reuse documented here.
Tonight mines only the six outlier-arm variants.

**Construction note (corrected at review)**: `rawsltexp` and
`shelter_expenses` are byte-identical on the current frame in all
115,559 rows of 2022-2024 - `calculate_raw_benefits()` recomputes
`rawsltexp` on every call and its last call postdates every adjustment,
so the earlier claim that it is pre-adjustment-stale was wrong
(review flag 1, 2026-08-09). `shelter_expenses` is used as the source
purely for naming consistency with the shipped
`shelter_expenses_by_hh_size`, and the feature is named
`shelter_expenses_p` accordingly. Same frozen fit as the validated
seven: FY2022-23 CPI-deflated non-zero values per state x reported-size
cell, zeros pinned to 0.

## 3. Support (computed; unchanged frame)

Train FY2022-23 76,031 rows / 8,397 errors (strata 45,165/3,423,
20,162/2,898, 10,704/2,076); test FY2024 39,528 / 4,764. Pre-screen for
the new variable (2026-08-09, screen 1 of the proposal): standalone
`shelter_expenses_p > 0.99` on train n=932, k=199, precision 0.214 vs
base 0.110, 99% LCB 0.184; FY2024 holdout 0.230 vs base 0.121 (no
collapse); every stratum clears n >= 30. Gate passed.

## 4. What the record says (cited)

Everything cited in the 2026-08-08 note carries over (findings 19, 20,
27, 28, 31, 33-34; hazard rows). New since: the two-arm result
(redistribution inside a flat median; package adopted on
artifact-independence grounds, Eric 2026-08-09); the results-review
recommendations now encoded here - per-state SIGN-CONSISTENCY counts and
per-arm seed spread join the mandatory companions in the readout, and
the shelter feature (which extends the per-state cutoff tables) must
clear a POSITIVE bar: median paired delta > 0 at the 5% budget, not
contradicted by the mean and harmed-tail companions, with real deployed
usage. A flat or negative shelter result drops the feature and costs the
package nothing.

## Mechanics

`methods/vocab_factorial_v2.R` (evolves `methods/vocab_attribution_v2.R`;
same machinery, six arms) + `runners/run_vocab_factorial.R`. Cache:
`methods/vocab_attribution_v2/cache/` (shared, so base/cand resume; the
four existing arms' configs are untouched and the new arms use new
cache keys, satisfying the review's cache-editing rule); outputs ->
`methods/vocab_factorial_v2/`. Twelve new mines x ~32 min plus 360 walk
cells: ~7.5-8 h, 18:00 launch, results by ~2 am. No writes
to `state_delivery_lists/`, no CHANGELOG entry, no version bump. The
state-typed arm and the five-arm proposal's remaining pieces are
unaffected and stay queued.
