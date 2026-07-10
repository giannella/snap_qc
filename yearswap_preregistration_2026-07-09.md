# Pre-registration: year-swap replication of the model-selection studies

**Written 2026-07-09, BEFORE running `run_selection_yearswap.R`.**
Purpose: the production configuration (engines, subsample, filter z) was
selected using 2023 as the judge for every comparison (critique V2). This
replication re-runs the decisive studies with train 2022+2023 / test 2024 —
a year that never influenced any design decision — with identical seeds,
grids, and screens. Expectations are recorded here first so the replication
cannot become another selection step.

**Known confound, accepted:** the original studies ran on the pre-rebuild
(single-element) frame; the replication runs on the rebuilt frame (~11% base
rate vs ~8.4%). LEVELS will therefore shift. All predictions below are
ORDERING and MARGIN claims, which is what the selection decisions rested on.

## Claims under test (original numbers: train 2022+2024, test 2023, old frame)

**C1 — engine pairing** (`compare_engines_v2/combo_summary.csv`):
xgboost+ranger had the best recall at the 0.20 floor (0.548 vs 0.501 ranger,
0.498 xgboost, 0.473 rpart) at comparable delivered precision (0.171 vs
0.175-0.181).
*Prediction:* the pair again posts the highest recall_at_020, by >= 3pp over
the best single engine, with precision_at_020 within 0.015 of the singles.
*Falsified if:* the pair is not the recall leader, or trails a single engine.

**C2 — subsample plateau** (`parameter_tuning_v2/v2_subsample_fine_summary.csv`):
0.15-0.30 were indistinguishable (precision_at_020 0.159-0.166) and beat the
0.60-0.80 band (0.155-0.159) on precision at the floor.
*Prediction:* the 0.15-0.30 band stays within 0.010 of itself and its worst
member is >= the best 0.60+ member on precision_at_020.
*Falsified if:* any subsample >= 0.60 beats the low band by > 0.010.

**C3 — filter stringency** (`parameter_tuning_v2/v2_lcbz_summary.csv`,
nrounds=1000 block): delivered precision at the floor rose monotonically in z
(0.138 / 0.146 / 0.152 / 0.170 for z = 0.84 / 1.28 / 1.64 / 2.33) while
recall fell (0.737 -> 0.550).
*Prediction:* the same monotone pattern on 2024; z=2.33 beats z=0.84 on
precision_at_020 by >= 0.020.
*Falsified if:* non-monotone ordering, or the z=2.33 advantage is < 0.010.

**C4 — "mine big widens the menu, not the frontier"** (same file): the
1000-round pools carried ~8x the filtered inventory of 100-round pools
(29-32k vs 3.7-3.8k) on a similar frontier.
*Prediction:* inventory ratio >= 5x persists; the large pool's
precision_at_020 deficit vs the small pool at matched z is <= 0.025.
*Falsified if:* the ratio collapses below 3x or the deficit exceeds 0.04.

## Decision rule

Claims that replicate: the production configuration stands, and the
replication chart goes in the decks. Any falsified claim: the corresponding
setting goes back to exploration and is NOT quoted as a finding until
resolved on a third split.

---

## RESULTS (added 2026-07-09 after the run; predictions above unchanged)

Artifacts: `compare_engines_v2/yearswap_train2223_test24/`,
`parameter_tuning_v2/yearswap_train2223_test24/`.

**C1 — engine pairing: REPLICATED (ordering), margin attenuated.**
xgboost+ranger again leads recall_at_020 (0.794 vs 0.773 xgboost, 0.757
ranger, 0.724 rpart) at a small precision cost (0.185 vs 0.189-0.202).
The predicted >= 3pp margin came in at 2.1pp; the falsification condition
(pair not the leader) was not approached. rpart+ranger is closer on 2024
(0.780) than on 2023 — the pair's edge is real but thinner.

**C2 — subsample plateau: PREDICTION FAILED (finding downgraded).**
The low band (0.15-0.30) no longer beats the high band (0.60-0.80):
precision_at_020 spans 0.181-0.186 across ALL nine settings — one flat
plateau. The falsification trigger (high beats low by > 0.010) was not hit,
but the predicted ordering (worst low >= best high) failed (0.182 < 0.186).
Per the decision rule, "low subsample beats high" is retired as a quotable
finding; the surviving claim is only "subsample in 0.15-0.80 does not
matter much." Production stays at 0.20 (it leads mean precision, 0.303,
and nothing beats it meaningfully).

**C3 — filter stringency: REPLICATED.**
Within nrounds=1000, precision_at_020 is again monotone in z
(0.169 / 0.175 / 0.179 / 0.188) with recall falling (0.873 -> 0.776).
The z=2.33 advantage over z=0.84 is 0.019 — a hair under the predicted
0.020, well above the 0.010 falsification line.

**C4 — menu, not frontier: REPLICATED.**
Inventory ratio 7.3-7.9x (26.6-29.1k vs 3.6-3.7k rules), large-pool
precision deficit at matched z 0.020-0.022 (<= 0.025 predicted), with the
big pool buying +7pp recall at the floor. The production configuration
stands.

**Level shift, as accepted in the confound note:** 2024 test levels run far
above 2023 (recall_at_020 ~0.78 vs ~0.55) — rebuilt frame + different year;
only orderings/margins were under test.

Net: 3 of 4 selection findings replicate on a year that never judged any
design decision; the subsample band claim is retired. No production setting
changes.
