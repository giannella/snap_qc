# v2.7 frames, fy2022: the CPI step and the three ordered steps
Generated 2026-10-06 03:08. Window: mined FY2017-19, scored FY2022 (three years ahead across the excluded FY2020-21). National pool only, 49 state lists walked with the shipped fill, 5% and 10% budgets. The CPI comparison (final, CPI-off) uses seeds 117 / 118 / 119; the step frames start at seed 117. Seed 117 pools for the final and 09-28 frames are the cached FY2022-window benchmark pools (same recipe).
Finished: cpioff seed 117, f0925 seed 117, f0928 seed 117, f0929 seed 117, final seed 117, cpioff seed 118, f0925 seed 118, f0928 seed 118, f0929 seed 118, final seed 118, cpioff seed 119, f0929 seed 119, final seed 119.

## Pooled precision of the 49 state lists, by frame and seed (errors caught / cases flagged)
```
 budget  frame          seed_117          seed_118          seed_119
   0.05 cpioff 0.2503 (456/1822) 0.2563 (467/1822) 0.2640 (481/1822)
   0.05  f0925 0.2707 (493/1821) 0.2585 (471/1822)              <NA>
   0.05  f0928 0.2684 (489/1822) 0.2663 (485/1821)              <NA>
   0.05  f0929 0.2801 (510/1821) 0.2717 (495/1822) 0.2816 (513/1822)
   0.05  final 0.2794 (509/1822) 0.2680 (488/1821) 0.2794 (509/1822)
   0.10 cpioff 0.2377 (870/3660) 0.2377 (870/3660) 0.2361 (864/3660)
   0.10  f0925 0.2497 (914/3660) 0.2415 (884/3660)              <NA>
   0.10  f0928 0.2508 (918/3660) 0.2456 (899/3660)              <NA>
   0.10  f0929 0.2489 (911/3660) 0.2445 (895/3660) 0.2601 (952/3660)
   0.10  final 0.2585 (946/3660) 0.2467 (903/3660) 0.2557 (936/3660)
```

## The CPI step alone, and the three steps (after minus before; values by seed in the order listed)
```
                                                                           comparison budget       seeds  d_pooled_precision_by_seed  d_mean d_errors_by_seed        d_state_mean_by_seed      d_state_median_by_seed        states_up_down
                    CPI step alone: final minus CPI-off (same code, CPI switched off)   0.05 117/118/119 +0.0291 / +0.0117 / +0.0154  0.0187  +53 / +21 / +28 +0.0240 / +0.0121 / +0.0221 +0.0227 / +0.0217 / +0.0000 26/10 , 28/12 , 23/16
                    CPI step alone: final minus CPI-off (same code, CPI switched off)   0.10 117/118/119 +0.0208 / +0.0090 / +0.0197  0.0165  +76 / +33 / +72 +0.0207 / +0.0098 / +0.0240 +0.0230 / +0.0000 / +0.0147 28/14 , 23/17 , 34/13
 step 1: fiscal-year CPI keying + SUA tier on review-year amounts (09-28 minus 09-25)   0.05     117/118           -0.0023 / +0.0078  0.0027         -4 / +14           -0.0021 / +0.0080           +0.0000 / +0.0000         16/19 , 21/12
 step 1: fiscal-year CPI keying + SUA tier on review-year amounts (09-28 minus 09-25)   0.10     117/118           +0.0011 / +0.0041  0.0026         +4 / +15           -0.0009 / +0.0006           +0.0000 / +0.0000         23/18 , 22/22
                                       step 2: October CPI values (09-29 minus 09-28)   0.05     117/118           +0.0117 / +0.0053  0.0085        +21 / +10           +0.0068 / +0.0039           +0.0000 / +0.0000         24/16 , 18/16
                                       step 2: October CPI values (09-29 minus 09-28)   0.10     117/118           -0.0019 / -0.0011 -0.0015          -7 / -4           -0.0022 / +0.0020           +0.0000 / +0.0000         19/21 , 24/20
                                   step 3: $10 SUA-tier tolerance (final minus 09-29)   0.05 117/118/119 -0.0007 / -0.0037 / -0.0022 -0.0022     -1 / -7 / -4 +0.0034 / -0.0025 / -0.0016 +0.0000 / +0.0000 / +0.0000 14/17 , 18/18 , 15/17
                                   step 3: $10 SUA-tier tolerance (final minus 09-29)   0.10 117/118/119 +0.0096 / +0.0022 / -0.0044  0.0025   +35 / +8 / -16 +0.0101 / +0.0045 / -0.0058 +0.0115 / +0.0000 / +0.0000 27/13 , 21/17 , 17/20
                                                steps 1-3 together: final minus 09-25   0.05     117/118           +0.0086 / +0.0095  0.0091        +16 / +17           +0.0081 / +0.0094           +0.0000 / +0.0217         23/15 , 25/16
                                                steps 1-3 together: final minus 09-25   0.10     117/118           +0.0087 / +0.0052  0.0070        +32 / +19           +0.0071 / +0.0071           +0.0000 / +0.0000         22/19 , 24/20
      harmed_helped d_mean_dollar_recall top1000_shared_same_seed                                 reading
 5/16 , 9/11 , 5/16               0.0207   n/a (cutoffs rescaled) all three final above all three CPI-off
  2/14 , 5/8 , 2/11               0.0362   n/a (cutoffs rescaled) all three final above all three CPI-off
        10/5 , 8/11               0.0036   n/a (cutoffs rescaled)       only seed(s) 117, 118: no reading
          5/4 , 5/6               0.0035   n/a (cutoffs rescaled)       only seed(s) 117, 118: no reading
          6/7 , 7/9               0.0021   n/a (cutoffs rescaled)       only seed(s) 117, 118: no reading
          6/4 , 6/4               0.0004   n/a (cutoffs rescaled)       only seed(s) 117, 118: no reading
    4/6 , 6/5 , 7/6               0.0017       0.70 / 0.69 / 0.72                          ranges overlap
    2/5 , 1/3 , 2/1               0.0040       0.70 / 0.69 / 0.72                          ranges overlap
         8/9 , 7/11               0.0088   n/a (cutoffs rescaled)       only seed(s) 117, 118: no reading
          2/8 , 2/8               0.0131   n/a (cutoffs rescaled)       only seed(s) 117, 118: no reading
```

## Top 1,000 rules: share below their own confidence bound on the test year
```
  frame seed top1000_below_bound top1000_flagwt_test_precision
 cpioff  117              0.6564                        0.2524
 cpioff  118              0.6329                        0.2563
 cpioff  119              0.6191                        0.2514
  f0925  117              0.5613                        0.2621
  f0925  118              0.5822                        0.2608
  f0928  117              0.5844                        0.2635
  f0928  118              0.5666                        0.2641
  f0929  117              0.5762                        0.2642
  f0929  118              0.5792                        0.2618
  f0929  119              0.5547                        0.2638
  final  117              0.5572                        0.2663
  final  118              0.5632                        0.2633
  final  119              0.5414                        0.2629
```

Reading: per-seed differences are paired over the same 49 states; a step measured at one seed only cannot be judged against seed variation.
Seed yardstick in this window (range of errors caught across the three seeds of one frame): CPI-off 5%: 25; 09-29 5%: 18; final 5%: 21; CPI-off 10%: 6; 09-29 10%: 57; final 10%: 43.
In the FY2024 window the same range at 10% was 5 to 54 errors depending on the frame.
