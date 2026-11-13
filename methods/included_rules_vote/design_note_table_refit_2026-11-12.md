# Refitting the vote table with the included rules voting: pre-run design note

2026-11-12. Follows findings §52 (included rules vote, v2.8.1) and its caveat that the fixed table and size model
were fit on votes from the 300 national vote rules only (ledger row "Included rules vote").

Decisions of 2026-11-12 (Eric):
- The goal is error dollars found, not precision or error counts.
- Refit the table; drop the partial-pooling idea (state x household-size terms). Tested the same day: the state
  pattern in error dollars did not carry from FY2017-19 to FY2022-24, and neither held-out nor in-sample versions
  raised error dollars at 10%.
- Run window A, with window B added because it needs no new mining. Window C (new FY2017-18 mines, FY2019
  cross-fit) is not run.
- There is no FY2025 case data, so there is no confirmation year.

## 1. The question

Since v2.8.1, a case's votes on tab 5.2.b count the state's included list rules as well as the 300 national vote
rules. The table that turns votes into a relative error rate (by household-size group 1 / 2-3 / 4+) and the size
model that turns benefit, household size and votes into an expected error size were fit on national votes only.
Does refitting both on the combined votes raise the error dollars found at the 5% and 10% workloads?

Evidence so far (both looks are in-sample for FY2023 or FY2024, which is why they do not decide anything; both
counted core list rules that are also vote rules twice, see section 2):
- On FY2023, households of 4+ with 4-6 and 7+ combined votes produced 1.21 and 1.44 times the error dollars the
  table predicts; the 4+ curve is flat at 0.25 from 2 votes up while those cases erred at 0.28-0.32 in FY2023-24
  (senior statistician, 2026-11-12).
- A quick refit on FY2023 combined votes, scored on FY2024 (principal data scientist, 2026-11-12), times random,
  errors / dollars: map setting 2.68 / 3.34 against 2.51 / 3.13 shipped at 5%, 2.33 / 2.77 against 2.34 / 2.81 at
  10%; year-to-year FY2024 2.59 / 3.30 against 2.50 / 3.12 at 5%, 2.36 / 2.82 against 2.30 / 2.80 at 10%.

## 2. What varies and what is held fixed

One component varies: the fixed table (curve) and the size model are fit on combined votes (national vote rules +
included list rules) instead of national votes alone.

Held fixed, in both arms:
- the vote rules: per seed, the 300 national vote rules allotted ("rep") from that seed's FY2017-19 pool after
  dropping rules that use `count_divisible_by_100`, exactly as `deploy_table.R` and `wb_ref_f1109.R` allot them;
- the list rules: per seed and state, the core rules of the 10% national list walked from the same pool
  (the gate script's walk, artifact gate on, pile gate on);
- the combined vote count used for ranking: national votes plus the core list rules that are not among the 300
  vote rules, so each rule counts once, as in the workbook (`rule_selection.py` merges a list rule that duplicates a
  vote rule into one row that votes once). Both arms rank on the same votes;
- the fitting rules: isotonic curve by size group with bins pooled from the top to 150 errors (three seeds
  stacked), gamma size model `ed ~ lben + hsz + vote` on error cases with votes capped at 7, relative rate over
  the fit year's national error rate;
- the evaluation: kept cases (the s46 keep rule), flags = max(1, round(budget x the state's cases)), ranked by
  expected error $ within state, at 5% and 10%.

Baseline arm, window A: the shipped curve (`methods/vote_scoring/deploy/vote_table.csv`) with §52's size model
(`scorer_W3.rds`) applied to the combined votes. This is the v2.8.1 design.
Baseline arm, window B: a national-only table fit on FY2023 by the same rules, so that in each window the two arms
differ only in whether the table was fit on combined or national votes. The shipped table's curve is printed beside it.

Counting correction (fresh statistician review, 2026-11-12). The first draft added every core list rule to the
national votes. On the seed-117 FY2017-19 pool, 64-74% of a state's core rules are also vote rules and carry
54-72% of the core rules' training flags (Vermont, Maine, California, Texas), so the draft double-counted them.
§52's each_core10 has the same double count: its vote rules and lists came from one pool in every setting. A
corrected §52 run (`methods/included_rules_vote/dedup/`) re-scores it with each rule counted once.

## 3. Data and rules

- Frame f1109 (`reg_model_data.rds`, sha256 08e0e825...).
- Pools: `methods/v270_seed_replicates/fy2022/pools/admitted_f1109_seed{117,118,119}.rds`, mined on FY2017-19
  (2026-11-10). No mining in this study.
- National lists only: no FY2017-19 state pools exist on f1109. In §52 part 1, national-only and blended lists
  were within 0.02 times random of each other.
- The workbook's build-time filters (`rule_selection.py`) are not applied to the list rules, as in §52.
- Votes are counted on every FY2022, FY2023 and FY2024 case. Fits use all cases of the fit year, as the shipped
  fit did; scoring uses kept cases.

## 4. Windows

| Window | Rules | Fit year | Test years |
|---|---|---|---|
| A | FY2017-19, seeds 117-119 | FY2022 | FY2023, FY2024 |
| B (year swap) | the same | FY2023 | FY2022, FY2024 |

Window A is the deployment-like order and is the shipping candidate. Window B checks that the result does not
depend on the fit year. Also reported, not judged: the shipped pair (curve and `size_coefs.csv`, issued benefit)
against window A's refit on the issued benefit, the pair that would ship.

Support (computed by the reviewer): the FY2022 fit year has 37,680 cases and 4,022 errors (size 1: 22,444 / 1,678;
2-3: 9,952 / 1,362; 4+: 5,284 / 982). The top combined-vote cells are thin (one seed, FY2023: size 2-3 at 4-6 votes
142 cases / 36 errors, at 7+ 130 / 35; size 1 at 7+ 208 / 70), so the 150-error floor on three stacked seeds will
merge them. These cells were counted with the double count; counting each rule once thins them further. A test year's median state has 857 cases, 43 and 86 flags at 5% and 10%, and 75 kept errors.

## 5. Checks before any readout (the run stops if one fails)

1. With list votes set to zero, the window A fit reproduces the shipped `vote_table.csv` (rel_rate, tolerance
   1e-9); its size model reproduces `scorer_W3.rds` on the recreated benefit (`raw_benefit_amount`, the
   evaluation basis of §52) and the shipped `size_coefs.csv` on the issued benefit (`rawben`, the workbook's
   basis), each to 1e-6. The two size models differ (lben 0.145 against 0.205); the curve is the same.
2. Seed 117's national votes on FY2022 equal `methods/case_predictions/s46/out_f1109_wb/wb/votes_W2.rds`.
3. Each state's 10% core list, walked on its FY2017-19 training cases, fills to the budget as the walk defines it
   (asserted by construction in the walk; the core and buffer shortfalls are reported).
4. The three seeds' vote files and list files hold the same cases in the same order, and their kept flags agree.

Evaluation uses the recreated benefit for both arms, as §52 did, so the baseline arm is `scorer_W3.rds` on the
combined votes. If the refit ships, it is fit again on the issued benefit, as `deploy_table.R` fits the shipped
pair.

## 6. Readout

For each window, each test year alone (FY2022, the one year not yet read for this question, on its own) and the
two test years pooled, per seed and averaged over the three seeds:
- error dollars found, times random (dollar recall over the budget share), pooled over the 49 states, at 5% and
  10%, refit against baseline, with a paired 95% interval (2,000 bootstrap draws of kept cases within state and
  test year, seeds averaged inside each draw). The interval is conditional on the fitted tables: it leaves out
  fit-year and mining variance;
- errors found, times random, as a guard;
- companions: the within-state mean and median paired change in dollar recall; the harmed-tail count (states whose
  dollar recall falls by more than 0.05); same-sign counts; California and Texas named;
- context arms: the rule lists alone and the national-only vote score.

## 7. Bars (set before the run, on error dollars)

The refit passes a window if, on the window's two test years pooled and averaged over seeds:
1. error dollars, times random, are no worse than -0.05 at either 5% or 10%;
2. error dollars rise at 5% or at 10% by at least +0.05, with the paired 95% interval above zero;
3. errors, times random, are no worse than -0.05 at either budget;
4. at the budget that rises, the within-state mean and median change in dollar recall are each at least 0;
5. at both budgets, the harmed tail (states whose dollar recall falls by more than 0.05) is at most 3 states.

In about 8% of states one error exceeds 0.05 of two years' error dollars, so a single case crossing the flag margin
can put such a state in the harmed tail.

Decision, stated now:
- passes A and B: ship the window A fit (replace `vote_table.csv` and `size_coefs.csv`, archive the old pair,
  rebuild the 49 workbooks);
- passes A only: report; Eric decides;
- fails A: do not ship; log the result.

These bars were set after the principal data scientist's quick look (section 1), which showed a gain at 5% and no
change at 10%. That look refit on FY2023 and scored FY2024, so window B's FY2024 read repeats part of it.

## 8. Caveats

- FY2023 and FY2024 have both been read for this question. Neither window is confirmatory. FY2022, window B's
  first test year, has not been used to judge a refit.
- The shipped workbooks' list rules are blended lists mined on FY2022-24 and filtered by `rule_selection.py`; the
  refit learns from national lists mined on FY2017-19, unfiltered. A list vote is assumed to mean the same thing
  in both.
- The top vote bins are thin: 29-77 cases per size group per year in the quick look, before stacking three seeds.
- One frame (f1109); three seeds of one mining recipe.

## 9. Run plan

Scripts in `methods/included_rules_vote/refit/`:
1. `refit_votes.R SEED`: the 300 national vote rules' votes on every FY2022-24 case and their keys (minutes). It
   must finish before `refit_lists.R` for the same seed, which stops if the key file is missing.
2. `refit_lists.R SEED`: the gate script's head (window w1719 on f1109, national only) and its per-state walk,
   writing each case's core-list vote count for FY2022-24 and the rule-list arm's results (about an hour).
3. `refit_fit_score.R`: the fits for A and B, the checks in section 5, scoring, bootstrap and readout (minutes).

Each list run takes about 13 GB at its peak, so two run at a time. Fresh senior-statistician review, 2026-11-12:
revise (window B's baseline, the double count, the companions at both budgets, the median, the breakdowns, scoring
the shipped pair); this version makes those changes, and the re-check approved it the same day.

## 10. Result and decision (2026-11-12)

The run finished with every check in section 5 passing (log: `refit/fit_score.log`). Times random on error dollars,
kept cases, the window's two test years pooled, three seeds averaged; refit minus baseline with its paired 95%
interval:

| Window | 5% | 10% |
|---|---|---|
| A (fit FY2022; test FY2023, FY2024) | 3.138 to 3.168 (+0.030; -0.049 to +0.091) | 2.734 to 2.714 (-0.021; -0.029 to +0.012) |
| B (fit FY2023; test FY2022, FY2024) | 3.124 to 3.124 (+0.001; -0.082 to +0.097) | 2.756 to 2.760 (+0.004; -0.028 to +0.021) |

- **Bars.** Both windows pass bars 1, 3 and 5 and fail bar 2: no budget gains +0.05 with an interval above 0. Bar
  4 is read at the budget that passes bar 2, so it fails with it. By section 7, window A fails: do not ship.
- **Window B's FY2022 alone:** -0.044 at 5% and -0.020 at 10%.
- **The table.** One-person and 2-3 person rates move by at most 1.1 points at any vote count, because the list
  rules add almost no votes to one-person cases (478, 303, 181, 174 cases per seed at 1, 2-3, 4-6, 7+ votes,
  against 478, 306, 183, 186 refit). In 4+ households about 680 cases per seed leave the 0-vote bin, and the 0- and
  1-vote rates fall from 16.6% and 24.6% to 15.3% and 22.5%. Refit B, fit on FY2023, differs from refit A by more
  than A differs from the shipped table.
- **California and Texas** gain nothing: at 10% in window A, California 37.0 errors / $7,821 to 37.3 / $7,916 and
  Texas 45.7 / $8,002 to 45.3 / $7,865 (two test years summed, seeds averaged); at 5% both lose dollars.
- **Companion arms** (window A, each rule counted once): at 10% the included rules voting give 2.27 / 2.73 (errors /
  dollars), the rule lists 2.30 / 2.52, the national votes alone 2.18 / 2.66; at 5%, 2.47 / 3.14, 2.47 / 2.78 and
  2.44 / 3.02.

**Decision.** Eric, 2026-11-12: "Okay, let's not ship it." `deploy/vote_table.csv` and `deploy/size_coefs.csv`
stay as shipped in v2.8.1; the refit scorers stay in `refit/` for the record.
