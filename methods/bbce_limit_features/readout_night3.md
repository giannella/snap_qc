# BBCE-limit features, night 3: three seeds of A and E at mtry 2 and 3, two windows
Generated 2026-10-25 18:06. Ranger alone (1000 trees, depth 4). National pool only, mismatch-tagged and income-pile-tagged rules dropped, the state lists walked with the shipped fill and scored at 5% and 10% budgets. A = the shipped 19 features; E = A + income as a percent of poverty + the state's BBCE gross limit. Runs on disk: 9 of 24.

## 0. How to read this
Each cell is three independent ranger draws (seeds 117, 118, 119). A contrast is the difference of two cells' seed means; its standard error comes from the spread of the three seeds in each cell, so with two degrees of freedom per cell it is a rough yardstick, not a test.
A contrast holds in a window when, at both budgets, the seed-mean gain in errors caught is positive and larger than twice its standard error (the larger of the cells' own and the within-mtry pooled one), the gain on non-pile cases is positive, the median and mean of the state-by-state differences (each state's precision averaged over its three seeds) are positive, and no more states are worse by more than 0.05 than are better by that much. A change goes on to the xgboost + ranger test only if it holds in both windows and its FY2022 gain on seeds 118-119 alone is positive at both budgets.
Selection: night 2 picked E at mtry 3 as the lead because its seed-117 draw was the best of seven arms against A at mtry 2. That draw is one of the three FY2022 seeds here, so the FY2022 three-seed reading carries some of its luck. The FY2022 reading on seeds 118-119 leaves it out, and the FY2024 window is wholly new: it is the only reading free of the selection.

## 1. Every run, pooled over the state lists

mine FY2017-19, score FY2022 (three years ahead)
```
              cell seed budget states errors flagged pooled_precision errors_np median_state mean_state mean_dollar_recall visible pile_dropped
 A, mtry 2 (ships)  117   0.05     49    504    1822           0.2766       497       0.2927     0.2720             0.1263   26180          822
 A, mtry 2 (ships)  118   0.05     49    498    1821           0.2735       491       0.2619     0.2722             0.1228   25758          799
         A, mtry 3  117   0.05     49    497    1822           0.2728       477       0.2766     0.2794             0.1281   25656         1304
         A, mtry 3  118   0.05     49    494    1822           0.2711       484       0.2766     0.2717             0.1222   25672         1307
         A, mtry 3  119   0.05     49    495    1822           0.2717       481       0.2683     0.2684             0.1219   25915         1270
         E, mtry 2  117   0.05     49    502    1821           0.2757       490       0.2647     0.2740             0.1227   25003          828
         E, mtry 3  117   0.05     49    514    1822           0.2821       505       0.2727     0.2764             0.1288   24309         1264
         E, mtry 3  118   0.05     49    485    1822           0.2662       477       0.2683     0.2634             0.1226   24592         1211
         E, mtry 3  119   0.05     49    498    1822           0.2733       480       0.2857     0.2709             0.1236   24334         1239
 A, mtry 2 (ships)  117   0.10     49    902    3660           0.2464       891       0.2526     0.2453             0.2360   26180          822
 A, mtry 2 (ships)  118   0.10     49    911    3660           0.2489       899       0.2472     0.2474             0.2399   25758          799
         A, mtry 3  117   0.10     49    930    3660           0.2541       909       0.2644     0.2541             0.2404   25656         1304
         A, mtry 3  118   0.10     49    911    3660           0.2489       898       0.2414     0.2494             0.2371   25672         1307
         A, mtry 3  119   0.10     49    904    3660           0.2470       880       0.2584     0.2469             0.2358   25915         1270
         E, mtry 2  117   0.10     49    897    3660           0.2451       878       0.2500     0.2445             0.2314   25003          828
         E, mtry 3  117   0.10     49    972    3660           0.2656       945       0.2821     0.2634             0.2499   24309         1264
         E, mtry 3  118   0.10     49    909    3660           0.2484       891       0.2593     0.2471             0.2405   24592         1211
         E, mtry 3  119   0.10     49    941    3660           0.2571       925       0.2697     0.2597             0.2378   24334         1239
```

## 2. Cells: seed mean and range of errors caught
```
 window              cell budget seeds errors_mean errors_sd errors_min errors_max errors_np_mean
 fy2022 A, mtry 2 (ships)   0.05     2       501.0       4.2        498        504          494.0
 fy2022         A, mtry 3   0.05     3       495.3       1.5        494        497          480.7
 fy2022         E, mtry 2   0.05     1       502.0        NA        502        502          490.0
 fy2022         E, mtry 3   0.05     3       499.0      14.5        485        514          487.3
 fy2022 A, mtry 2 (ships)   0.10     2       906.5       6.4        902        911          895.0
 fy2022         A, mtry 3   0.10     3       915.0      13.5        904        930          895.7
 fy2022         E, mtry 2   0.10     1       897.0        NA        897        897          878.0
 fy2022         E, mtry 3   0.10     3       940.7      31.5        909        972          920.3
```

## 3. Contrasts
gain = difference of seed means in errors caught; se_cell from each cell's own seeds, se_pooled from the seed variance pooled over the A and E cells at the same mtry; the rule uses the larger. State columns use each state's precision (and dollar recall) averaged over the reading's seeds. pass = the reading rule in section 0 at that budget (NA for the interaction, which is descriptive); median_zero_only = the budget fails only because the median state change is exactly 0.

mine FY2017-19, score FY2022 (three years ahead), 3 seeds
```
                                       contrast budget gain se_cell se_pooled gain_over_2se gain_np median_state mean_state median_state_np up down worse_05 better_05 dollar_recall  pass median_zero_only
 E, mtry 3 minus A, mtry 3 (features at mtry 3)   0.05  3.7     8.4       8.4         FALSE     6.7        0.000    -0.0030          0.0000 21   22        5         5         0.001 FALSE            FALSE
 E, mtry 3 minus A, mtry 3 (features at mtry 3)   0.10 25.7    19.8      19.8         FALSE    24.7        0.006     0.0066          0.0076 27   20        0         3         0.005 FALSE            FALSE
```

mine FY2017-19, score FY2022 (three years ahead), seeds 118-119 (leaves out the seed-117 runs, among them the draw that suggested the question; one degree of freedom per cell, so read the signs)
```
                                       contrast budget gain se_cell se_pooled gain_over_2se gain_np median_state mean_state median_state_np up down worse_05 better_05 dollar_recall  pass median_zero_only
 E, mtry 3 minus A, mtry 3 (features at mtry 3)   0.05 -3.0     6.5       6.5         FALSE      -4            0    -0.0029               0 21   21        8         4        0.0011 FALSE            FALSE
 E, mtry 3 minus A, mtry 3 (features at mtry 3)   0.10 17.5    16.4      16.4         FALSE      19            0     0.0052               0 24   22        1         3        0.0027 FALSE            FALSE
```

Holds, by window. The FY2022 three-seed reading includes the seed-117 runs, so it carries the selection of the draw that suggested the question; FY2024 is the only reading free of it. A change goes on to the xgboost + ranger test only if it holds in both windows and its FY2022 gain on seeds 118-119 alone is positive at both budgets.
```
                                       contrast fy2022_holds fy2022_seeds_118_119_gain_positive fy2024_holds goes_on
         E, mtry 3 minus A, mtry 2 (question 1)           NA                                 NA           NA      NA
         A, mtry 3 minus A, mtry 2 (mtry alone)           NA                                 NA           NA      NA
 E, mtry 2 minus A, mtry 2 (features at mtry 2)           NA                                 NA           NA      NA
 E, mtry 3 minus A, mtry 3 (features at mtry 3)        FALSE                              FALSE           NA   FALSE
```

## 3b. Subgroups: errors caught, seed mean, each cell minus A at mtry 2

mine FY2017-19, score FY2022 (three years ahead): minority regimes in FY2022 (no BBCE, or BBCE at 130%) (15 states)
```
              cell budget seeds errors errors_np minus_A2 minus_A2_np
 A, mtry 2 (ships)   0.05     2  143.0     141.5      0.0         0.0
         A, mtry 3   0.05     3  137.3     134.7     -5.7        -6.8
         E, mtry 2   0.05     1  139.0     142.0     -4.0         0.5
         E, mtry 3   0.05     3  147.0     144.3      4.0         2.8
 A, mtry 2 (ships)   0.10     2  259.0     252.5      0.0         0.0
         A, mtry 3   0.10     3  252.0     247.7     -7.0        -4.8
         E, mtry 2   0.10     1  259.0     252.0      0.0        -0.5
         E, mtry 3   0.10     3  274.7     267.7     15.7        15.2
```

mine FY2017-19, score FY2022 (three years ahead): limit or BBCE status in FY2022 differs from a training year (10 states)
```
              cell budget seeds errors errors_np minus_A2 minus_A2_np
 A, mtry 2 (ships)   0.05     2  125.5     126.0      0.0         0.0
         A, mtry 3   0.05     3  124.3     119.7     -1.2        -6.3
         E, mtry 2   0.05     1  119.0     114.0     -6.5       -12.0
         E, mtry 3   0.05     3  119.7     117.7     -5.8        -8.3
 A, mtry 2 (ships)   0.10     2  212.5     208.5      0.0         0.0
         A, mtry 3   0.10     3  211.3     208.3     -1.2        -0.2
         E, mtry 2   0.10     1  215.0     210.0      2.5         1.5
         E, mtry 3   0.10     3  217.0     213.7      4.5         5.2
```

## 4. Rules: precision of the top 1,000 on the test year, seed means
```
 window              cell seeds visible pile_dropped top1000_precision top1000_added added_precision other_precision regime_below_own_top1000
 fy2022 A, mtry 2 (ships)     2   25969          810            0.2709             0              NA          0.2709                        0
 fy2022         A, mtry 3     3   25748         1294            0.2855             0              NA          0.2855                        0
 fy2022         E, mtry 2     1   25003          828            0.2661           354          0.2616          0.2695                        0
 fy2022         E, mtry 3     3   24412         1238            0.2788           323          0.2749          0.2806                        0
```
added / other: the top 1,000 rules using income as a percent of poverty or the BBCE limit, against the rest (flag-weighted test-year precision). regime_below_own_top1000: summed over seeds.
