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
| base_slt | 16 + shelter_expenses_p |
| cand_slt | 16 + package + shelter_expenses_p |

Pre-registered contrasts, paired by seed: (1) base_slt - base and
(2) cand_slt - cand isolate the shelter feature; (3) cand - base is the
free same-seed replication of last night. Everything else held fixed
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
same machinery, four arms) + `runners/run_vocab_factorial.R`. Cache:
`methods/vocab_attribution_v2/cache/` (shared, so base/cand resume);
outputs -> `methods/vocab_factorial_v2/`. Six new mines x ~32 min plus
240 walk cells: ~4.5-5.5 h, evening launch, results overnight. No writes
to `state_delivery_lists/`, no CHANGELOG entry, no version bump. The
state-typed arm and the five-arm proposal's remaining pieces are
unaffected and stay queued.
