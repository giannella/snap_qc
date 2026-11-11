# Review of Ben's reconstruction rewrite (6006cba..2570391): swaps, smoothing, AMOUNTi, and how to test accuracy

Senior-statistician agent, 2026-10-30, on frame f1030 (sha256 e3714039) against
eb989fda. Saved by the main session from the agent's hand-back. Read-only: no
tracked file edited. Scripts in this folder; outputs in scratch/. Exploratory
results are marked as such.

## Summary

- **Bound smoothing** (`impute_at_bound`, `impute_past_shelter_cap`) is the "swap to improve smoothing": a donor's ratio or dollar difference replaces Step 7's stop point. It touches 10,654 rows; 21% of all error cases (5,079 of 24,078) now carry a random draw. It removes a measurable label artifact and beat the stop point in two small synthetic tests, but the truth at a bound is not identified by the QC file.
- **The nature 97 / agency 26 FS-side swap** is a label operation, not smoothing. It accounts for all 118 label changes (58 error->clean, 60 clean->error), rewrites FSBEN/AMTERR/STATUS on 40,677 rows, and for ~85% of the affected partial cases rests on an unidentified split of the error between elements.
- **AMOUNTi magnitudes are used only on the "capped" path** (670 cases, 3-5% of multi-element cases). Elsewhere they only set the order; with zero, copied or AMOUNT1-only reporting that order is slot order, and the first variable absorbs the gap. That covers 83% of multi-element error cases, where reaching RAWBEN says little about whether the split is right.

## A. Review

### A1. What is swapped or redrawn (line numbers at 2570391)

1. **`impute_at_bound` (1029-1115, called at 1164).** RAWBEN at the maximum allotment (or minimum benefit), and Step 7 closed the gap with a continuous variable whose further movement keeps the benefit on the limit (at max: income down or deductions up; at min: the reverse). Step 7 stops at the smallest move that reaches the limit, a bound rather than an estimate. The last continuous variable moved is redrawn past `edge_past()`. Donors (1079): same closing move, between the limits, reached RAWBEN; national pool, all six years. Moves toward $0: floor(donor raw/QC ratio x QC value), ratios truncated to <= edge/QC. Moves up: QC value + donor's raw-minus-QC difference, truncated to >= edge (1091). Fallbacks (1094-1101): uniform on [0, edge] or edge + a random positive difference (1-3 cases). Zero rule (1103): a draw <= $25 or <= 10% of the QC value becomes $0. Guard (1112): stopifnot that the recreated benefit did not change. Replaces the deterministic `scale_by_ratio` and the "cut >75% at max -> $0" rule.
2. **`impute_past_shelter_cap` (1121-1160).** `rent_up` cases whose uncapped shelter deduction is within $10 below the cap, where rent +$25 past the cap leaves the benefit unchanged. Rent redrawn past the cap from `rent_up` donors not near the cap (1141).
3. **97/26 block (1186-1256).** Writes the reconstructed agency value into the FS (reviewer) side. Required (only 97/26 elements; 1245-1254): FS fields := raw, FSBEN := rawben_recreated, AMTERR/bendiff/over_threshold := 0, STATUS := 1, element fields NA. Partial (97/26 with real elements; 1209-1243): variables cited only by 97/26 elements take their reconstructed value on the FS side; FSBEN moves by the benefit change (1228); bendiff, AMTERR, STATUS, over_threshold recomputed (1235).

### A2. Cases touched (new frame)

| Smoothing path | Rows | Per FY | Errors |
|---|---|---|---|
| max | 9,254 | 1,433-1,744 | 4,608 |
| min | 1,105 | 166-207 | 331 |
| shelter_cap | 295 | 29-62 | 140 |

- 4.30% of rows in FY2017 rising to 5.03% in FY2024; by state 2.4% (WY) to 8.7% (DC); DC, RI, MI, OR, GA, CA above 6% (scratch/imputed_by_state.csv).
- At max: earn_down 4,058, unearn_down 2,990, rent_up 1,797, med_up 250, dep_up 90, cs_up 69. At min: rent_down 402, earn_up 305, unearn_up 292, med_down 75, cs_down 22, dep_down 9.
- At max, earned income moves a median -$216 (p10 -$895) from the stop point; the redraw puts earned income at $0 in 40.7% (stop point 0.2%); unearned 28.9%. Pure earn_down at max: share at $0 14.4% (old frame) -> 40.5%.

AMOUNT patterns, 19,049 multi-element cases:

| FY | all_zero | amount1_only | copied | other | clean_split | even_split |
|---|---|---|---|---|---|---|
| 2017 | 65.4% | 15.3% | 9.0% | 6.1% | 4.0% | 0.2% |
| 2024 | 51.6% | 20.3% | 11.7% | 9.8% | 6.0% | 0.6% |

- GA, OH, LA, MI, TX, CO, WI, KY are 98-100% all_zero. clean_split comes mostly from WV, MT, VA, ND, IL, NE, KS, NH.
- In 20 of 39 states the dominant pattern changes across years (MN all_zero -> copied from FY2022; OR and WA all_zero -> amount1_only from FY2022; TN the reverse; VA clean_split -> other; capped cases ND 2 -> 23, KS 1 -> 22).
- AMOUNT1 is $0 in 48.7% of multi-element cases; 69.2% of present slots 2-9 are $0. In amount1_only, AMOUNT1 = AMTERR in 94.4% (the case total, not element 1's share). In "other", AMOUNTs sum to more than AMTERR in 68%.
- Multi-element errors are 7,027 of 24,076 (29%): all_zero 2,530, amount1_only 1,854, copied 1,447, other 765, clean_split 401, even_split 30.

Reaching RAWBEN exactly, old -> new: multi-element errors 70.7-75.6% -> 89.9-92.1%; single-element errors 83.8-86.7% -> 85.4-88.1%; clean about 94% both. Within $5, multi-element errors 76.1% -> 92.6%. "_error" notes 1,975 -> 3,013 rows, error cases among them unchanged (1,081 vs 1,082).

97/26: required 37,171 rows (16% of the frame; 5,763-6,565 per FY); partial 3,506. All 118 matched-row label changes come from this block: required 49 error->clean; partial 9 error->clean and 60 clean->error; no other row changes label.

### A3. Assumptions and failure cases

Bound redraws:
- **Donors resemble stuck cases (contradicted).** Among earn_down donors, 21% in the lowest QC-earnings quintile have ratio <= 0.1 vs 1-3% in other quintiles; 14% for one-person vs ~3% for 4+; 2-18% by state (Kruskal-Wallis p < 1e-30 for amount and state; unearned the same). Stuck cases are mostly small, low-income households; truncation corrects only part.
- **Donor pool selected on the outcome.** Donors are between the limits because their cuts were small, so large cuts are under-represented.
- **One draw per case.** The file bounds the truth to [0, edge] or [edge, inf). A single draw gets the distribution right but has larger per-case error than a conditional mean; everything rests on one seed (20261029).
- **Only the closing variable is uncertain.** Earlier variables keep their Step 7 values, including the greedy split (A4).
- **Failure.** If at-bound agency errors are usually "just enough", the stop point is closer; if usually a whole omitted amount ($0), the redraw is closer but still sets too few to $0. Nothing in the file separates these (96% of stuck earn_down cases have one earner; member_anchor.R).
- Interaction with 97/26: partial swap on stop-point vs redrawn values changes FSBEN on 5 rows and no labels (partial_swap_seed.R).

97/26 swap:
- **QC rules** (97/26 variances are not errors) look consistent with the file: 99.87% of required rows are already under tolerance (vs 52% of other STATUS 2/3 rows); p99.9 of |RAWBEN-FSBEN| is $56; most carry N97 with AGENCY missing (28,517). Label effect for required rows is small (49).
- **Separating the 97/26 share from the real share** depends on slot order under the greedy cascade: first 97/26 slot 1: 1,223 cases, FSBEN shifted in 918 (75%); slot 2: 1,900, shifted in 453 (24%). Where FSBEN shifted, 1,225 of 1,439 (85%) end with |RAWBEN-FSBEN| <= $2: the 97/26 element absorbed the whole error. Of the 60 clean->error rows: 34 all_zero, 14 amount1_only, 8 clean_split, 4 other. Error $ on partial rows $86,256 -> $65,787 (-24%).

### A4. AMOUNTi decomposition

- `amount_pattern()` (582-603). Capped path (676, 699-707) requires clean_split, every element a Step 7 element, and no household-size or homeless shift; each element moves the benefit by its own AMOUNT. Otherwise AMOUNT only sets order (688: utilities first, then largest AMOUNT, ties by slot), and each variable moves as far as it can toward RAWBEN (708-768). Natures set direction for slots 2-9 (678).
- Switching among all_zero, copied and amount1_only changes nothing (identical in the synthetic test). Switching between clean_split and anything else, or "other" where AMOUNTs set the order, changes the method, so reconstruction quality varies by state-year (ND, KS, NH, VA, WV) with no change in error behavior. even_split is treated as uninformative (67 cases; minor).
- The greedy fallback is a parsimony rule. In the 820 real clean_split cases slot 1 carries a median 66% of the AMOUNTs, >= 90% in only 21.8%, and the largest amount in 72%: errors are usually shared.

### A5. Bugs and inconsistencies

1. Required path (1248-1249): FSBEN := rawben_recreated even where reconstruction missed RAWBEN (1,840 rows; 13 by more than the tolerance), while bendiff/over_threshold are forced to 0. Recomputing |RAWBEN-FSBEN| from the frame gives 13 errors the frame calls clean. **Not a bug (Eric, 2026-10-30): intentional, discussed with Ben.**
2. The original FSBEN, AMTERR, STATUS and over_threshold are not preserved, so the file-defined label can no longer be recomputed from the frame (118 rows differ).
3. STATUS := 1 on 37,171 required rows changes any STATUS-based "any error" label for those rows. (The agent cited the snap-qc-per-experiments conventions here; that applies only if that repo reads this frame.)
4. The partial-swap log line reports "fell under the threshold" (9) but not the 60 that rose above it.
5. Donor pool is national and conditioned only on the closing move (design, not a bug; A3).
6. frame_check_f1030.R pointed at additional_data/ for the old frame, which is now the new frame. FIXED by the main session 2026-10-30 (reads archive_data/reg_model_data_f1027_eb989fda.rds).

## B. Tests of reconstruction accuracy, ranked by value per cost

1. **Plasmode (synthetic injection) through the full pipeline, old vs new code and each toggle.** Accuracy, conditional on the injection's realism. 1-2 days of design; compute trivial (3,600 recreations in 10 s). Inject into clean cases (between the limits and at the bounds), drawing from real data: number and mix of elements; magnitudes from identified single-element cases by household size and amount band; slot shares from clean_split; each state-year's AMOUNT-pattern mix and E_FINDG copying; 97/26 elements with known $0 contribution. Metrics: per-variable MAE, recovery within $10 or 2%, attribution, KS/Wasserstein to truth, pile share, edge signature, 97/26 label recovery. At the bounds run at least three truth mechanisms (donor-like, whole amount omitted, minimal error). Pre-registered "better": multi-element MAE >= 10% below both the old code and no reconstruction under every mechanism; KS of imputed variables <= 0.05 under donor-like and no worse than the stop point under the others; no rise in 97/26 label error. "Worse": any reversal under any mechanism.
2. **Mask AMOUNTs on real clean_split cases.** Accuracy proxy if clean_split AMOUNTs are right. Cheap (run tonight, below). Extend to compare fallbacks: greedy, proportional to empirical slot shares, multiple imputation over splits. Better if agreement with the AMOUNT-guided result exceeds 47% overall and 38% in shared-split cases.
3. **Linkage with a partner state.** Direct accuracy, the gold standard: match the state's QC sample cases to its eligibility-system budget for the sample month. The only test that settles the at-bound question. Small n (~1,000 cases per state-year).
4. **Artifact and signature checks.** Measure leak-like artifacts, not accuracy. Edge signature (share of at-max cases with uncapped benefit within 1% of the max, errors vs clean); piles; error vs clean distributions within state-year-size cells; a "reconstruction detector" classifier. Criterion: on-edge error-rate lift <= 1.2x.
5. **Seed sensitivity / multiple imputation (5-10 seeds) through mining.** Robustness. Overnight runs. Criterion: top-k rule Jaccard >= 0.8 across seeds; precision spread small relative to any claimed difference.
6. **Downstream toggles on held-out precision.** Usefulness only: an artifact raises held-out precision inside QC data (the piles were nearly all errors), so higher precision is not evidence of accuracy. Informative only when scored where raw values are true (plasmode truth or state data).

External aggregates add little: FNS characteristics tables use reviewer values, and RAWBEN is already observed.

## Exploratory results (not pre-registered)

(a) Synthetic two-element errors (synthetic_multielement.R; 600 clean cases, two Step 7 variables perturbed with real single-element ratios; recovered = within $10 or 2%):

| AMOUNTs written as | Reached RAWBEN | Both values right | Both moved right way | MAE per variable |
|---|---|---|---|---|
| clean_split | 99.0% | 48.2% | 65.5% | $144 |
| all_zero / copied / amount1_only / even_split / no natures | 99.3% | 19.3% | 17.7% | $186 |
| No reconstruction (raw = FS) | 0% | 1.5% | n/a | $278 |

With uninformative AMOUNTs, 80% reach RAWBEN with at least one value wrong. Earned + unearned with clean AMOUNTs: 93% both right; pairs with utilities 32-38%. Caveat: AMOUNTs were written in slot order while the code applies utilities first then largest first; probably understates the capped path.

(b) Masking AMOUNTs on the 820 real clean_split cases (mask_amounts_real.R): agreement with the AMOUNT-guided result 47.3%; 82.1% where slot 1 carries >= 90%, 37.6% otherwise; RAWBEN reached 90.9% (93.8% with AMOUNTs).

(c) Synthetic income cuts landing on the maximum (synthetic_bound.R; 600 per cell; real donors):

| Truth | Variable | MAE stop -> redraw | Bias stop -> redraw | KS stop -> redraw |
|---|---|---|---|---|
| Donor ratios (favors redraw) | Earned | $437 -> $315 | +$436 -> +$9 | 0.49 -> 0.04 |
| Donor ratios | Unearned | $281 -> $245 | | 0.36 -> 0.03 |
| 50% $0 / 50% uniform | Earned | $635 -> $290 | +$635 -> +$172 | 0.73 -> 0.26 |
| 50% $0 / 50% uniform | Unearned | $490 -> $272 | +$490 -> +$184 | 0.74 -> 0.34 |

Earned-income pile share 6-11% -> 0.

(d) Edge signature (bound_signature.R): at-max cases with uncapped benefit within 1% of the max, error rate 26.6% vs 6.9% for all at-max (3.9x) on the old frame; 10.3% vs 7.3% (1.4x) on the new. Measures the artifact, not accuracy.

## C. Verdict

- **Keep:** multi-element recreation and direction logic (beats leaving FS values; multi-element errors reaching RAWBEN ~74% -> ~91%); the capped path; bound smoothing as default (edge artifact 3.9x -> 1.4x, piles 1,399 -> 208, beat the stop point in both synthetic mechanisms), on two conditions: condition donors on household size, QC-amount band and era; check rule lists across several seeds.
- **Gate behind a flag (default off for labels):** the 97/26 FS-side rewrite of FSBEN, AMTERR, STATUS, over_threshold. Keep the 97/26 raw-side reconstruction. Preserve the file's original FSBEN, AMTERR, STATUS, over_threshold. (A5.1 is intentional per Eric; no fix.)
- **Test before relying on it:** the greedy split for uninformative AMOUNTs (83% of multi-element error cases) with the plasmode (B1); alternative fallbacks via B2; meanwhile a QA flag marking multi-element cases on the uncapped path for sensitivity checks.

## Files

Scripts: describe_paths.R, check_9726.R, synthetic_multielement.R (builds scratch/pre7.rds and pre7_env.RData, ~36 MB each), mask_amounts_real.R, synthetic_bound.R, member_anchor.R, donor_heterogeneity.R, partial_swap_seed.R, amount_consistency.R, bound_signature.R. scratch/: logs, synthetic_*_results, imputed_by_state.csv, amount_pattern_by_state.csv, amount_pattern_dominant_state_year.csv, old_munging.R, oldnew_bound.R, dbg.R. The agent read the root checkpoints all_years.rds, corrected.rds, final.rds without changing them.
