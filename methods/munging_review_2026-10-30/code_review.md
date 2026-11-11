# Code review and /simplify of Ben's munging rewrite (6006cba..2570391)

Agent review, 2026-10-30. Saved by the main session from the agent's hand-back.
Review worktree: C:/Users/ericg/snap_qc_review_ben, branch review/ben-munging-simplify
at 2570391, simplify edits uncommitted there. Nothing written to snap_qc_v27 by
the agent. Line numbers are the munging script at 2570391.

## Findings, most severe first

1. **No-action-required 97/26 cases keep a nonzero benefit gap** (munging:1248). **Not a bug (Eric, 2026-10-30): intentional, discussed with Ben.** FSBEN := rawben_recreated rather than RAWBEN while bendiff/absbendiff/AMTERR/over_threshold := 0. 1,840 of 37,171 such cases have FSBEN != RAWBEN; 13 exceed the tolerance, and these are exactly the 13 frame rows where over_threshold disagrees with |RAWBEN - FSBEN| > threshold. Likely fix: FSBEN := RAWBEN (and the FS benefit columns) for these rows. Same as the statistician's A5.1.
2. **Agency-26 household-size and homeless variances stay in the error** (munging:1220). The partial block separates only step7_vars; a 97/26 size (150/111/...) or homeless (362) element beside a real error is never removed. Up to 30 cases.
3. **Removing the percentile features (6006cba) breaks other scripts**: methods/state_remine_v2.R (~l.60), vocab_factorial_v2.R, vocab_attribution_v2.R, state_percentile_runoff_v2.R and build_percentile_value_map.R read rawgrinc_p, rawearn_p, rawrent_p and other *_p columns. They fail or lose their percentile arms on the new frame. (Main session checked: none of the 2026-10-30 night chain's scripts use them.)
4. **Over-max household-size correction lost from the audit note** (munging:373-377 vs 796). When ELEMENT1 is a Step 7 element, Step 7 overwrites the "hhsize_down" note and amount; hhsize_secondary_shift was computed before over_max. 48 rows with rawusize > FSUSIZE and no hhsize note.
5. **Excluded-member search can give negative income** (find_member, munging:443/498): rawearn = FSEARN - member income with no $0 floor; the two-person fallback (:901) clamps. Latent: 0 negative raw values in the 2570391 output.
6. **Two definitions of "reached RAWBEN"** (munging:865 exact vs Step 7's $5 zero_margin): a case Step 7 counts as reached can be rewritten as "hhsize_up_2" if a second person matches exactly (possible among the 38 hhsize_up_2 rows).
7. **Step 6 member search runs before the ELEMENT2-9 homeless (362) correction** (:455 vs :836). Latent: 0 cases.
8. **Note direction conventions are mixed**: hhsize_* uses the QC direction, earn/util/homeless/income_* the raw direction ("hhsize_up+income_down" = raw size and raw income both went down). Labelling only.
9. **Documentation**: the two new methods/known_constraints.md lines (~106-111) lack a findings-ledger citation; CHANGELOG says element 150 natures 13 and 15, the code shifts natures 12-16.
10. **Efficiency**: candidate_values/move_toward try every $1 value up to +$11,000 (income) or +$3,000 per variable and case; bisection or a closed-form inverse would be much faster. Full run ~7.5 minutes.

Checked clean: slot indexing (slot_matrix + %in% re-wrapped with matrix(ncol = 9)); capped branch only when all E_FINDG are 2/3; RNG (set.seed at the start of each impute function; rerun reproduced the reference byte for byte); no row imputed twice; over_threshold factor assignments valid.

Leakage: no new reviewer-field/RAWBEN pairing in model inputs. Review-derived QA columns now in reg_model_data.rds must never be features: correctednotes, amount_pattern, cascade_*, imputed_at_bound, *_secondary_shift, no_action_*_i. Pre-existing: n_income_types, n_deduction_types and count_divisible_by_100 are built from FS* fields, and the partial 97/26 block now rewrites FSEARN and similar fields while leaving FSWAGES and other components unchanged.

## Simplify (behavior-preserving, uncommitted in the review worktree)

One file, 127 insertions, 135 deletions:
- slot_matrix moved above Step 6 and used in the household-size loop; new first_slot_nature() replaces two hand-written ELEMENT/NATURE 1:9 loops.
- candidate_values and util_nature_step moved up; the Step 6 utility retry calls util_nature_step.
- One num() (NA -> 0) helper replaces four inline versions.
- member_matrices() shared by find_member (computed once) and the two-person fallback.
- The two-person inputs vector replaced by benefit_inputs; pre_vars from unique(step7_vars).
- benefit_at gains an `out` argument, used by edge_past.
- bound_state() holds the setup shared by both impute functions.
- fs_cols <- c(impute_qc, rawutil = "UTIL"); unused `ag` matrix removed.
- Prior-year sets via combn (same order, identical()); second apply loop folded into the first.
- Secondary-shift notes via shift_note and Reduce(join_notes).

Skipped as not behavior-preserving or too risky: merging find_member with the fallback search; whole-row restore in the fallback; restructuring recreate_elements' lean/rerun state; a size_tables helper; a Step 7 censoring flag.

**Verified identical**: baseline run at 2570391 in the review worktree gave sha256 e3714039 (matches staging); after the edits the run gave the same sha256; identical() TRUE on 231,602 rows x 1,296 columns; console log matches.

## Efficiency sub-review (finished later; nothing applied)

Benchmarked on a pre-Step-7 snapshot built from 2570391; every replacement returned identical() == TRUE against the reference output and none touches the RNG. Step 7 alone 159.3 s -> 70.7 s (items 1-4); smoothing 19.2 s -> 2.9 s (items 5-6); about 105 s off the ~7-minute run. Line numbers at 2570391.

1. move_toward (643): replace the two-key order() over ~11,000 candidates with which(gap == min) then which.min(abs(cand - cur)); same tie-breaking. ~11-16 s.
2. candidate_values (630): the sorted integer grid needs no sort(unique()); write cur into its slot when cur is a non-negative whole number, else fall back. ~27 s.
3. benefit_at (611-615): compute the benefit on scalars with recycling instead of the full calculate_raw_benefits on 12 replicated inputs. ~30 s. Duplicates the benefit formula (any change must be made in both places); needs rep_len and a no-NA check on bi.
4. util_tiers (396-404): memoize the 735-row table lookup (cache errors too). A few seconds.
5. closing_move (1023-1027): run the regex on the 288 distinct notes and match back, instead of 231k notes three times. ~7-12 s.
6. impute_at_bound loop (1087-1088, 1104): assign into a plain vector and write the column back once, instead of tibble subassignment that copies the column ~11,000 times. ~2-3 s.
7. amount_pattern (591-592): pmax/pmin over columns instead of apply(..., 1, max/min). 1.4 s -> 0.4 s.

Not tested: a monotone search in move_toward (riskier for tie-breaking). Files: session scratchpad eff/ (head.R, build_pre7_head.R, opt.R ABCD, smooth_funs2.R, smooth2.R, smooth.R).
