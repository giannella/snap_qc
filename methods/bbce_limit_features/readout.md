# BBCE-limit features: readout
Generated 2026-10-23 00:59. Mined FY2017-19 with ranger alone (1000 trees, depth 4, mtry 2), seed 117 (arm A also at 118); scored FY2022. National pool only, mismatch-tagged and income-pile-tagged rules dropped (the shipped gates), 49 state lists, shipped fill, 5% and 10% budgets. Arms finished: A, A_seed118, B, C, D, E.

## 0. How to read one seed
A shared seed does not pair the arms: adding a feature changes ranger's random draws, so each arm-minus-A difference is a difference of two independent draws, and the best of four arms is a selected maximum. The thresholds below screen for a second night; they are not evidence that an arm is better, and a difference inside them is no evidence that it is not.
Thresholds, errors caught on all cases: 5% budget 25, 10% budget 57 (the larger of the three-seed range on xgboost + ranger pools in this window, 25 / 57, and arm A's own seed-118 minus seed-117 difference, 6 / 9).
Second night (three seeds for that arm, then both engines) if an arm beats A by more than both thresholds, its non-pile change has the same sign at both budgets, its median and mean changes are positive, and it harms no more states than it helps.

## 1. Pooled over the 49 state lists
```
                              arm budget errors flagged pooled_precision errors_np precision_np median_state mean_state mean_dollar_recall visible pile_dropped deepest_rank sel_new sel_regime sel_regime_below_own
       A at seed 118 (seed noise)   0.05    498    1821           0.2735       491       0.2732       0.2619     0.2722             0.1228   25758          799         2748       0        549                    0
                    A: shipped 19   0.05    504    1822           0.2766       497       0.2766       0.2927     0.2720             0.1263   26180          822         2721       0        608                    0
           B: + distance to limit   0.05    480    1821           0.2636       468       0.2606       0.2791     0.2654             0.1201   25920          876         2551    1138        354                    0
                  C: + BBCE limit   0.05    475    1822           0.2607       462       0.2571       0.2778     0.2581             0.1209   25676          777         3931     503        842                    0
            D: + distance + limit   0.05    491    1821           0.2696       476       0.2650       0.2750     0.2680             0.1218   25201          797         2805    1378        750                    0
 E: + income % of poverty + limit   0.05    502    1821           0.2757       490       0.2728       0.2647     0.2740             0.1227   25003          828         3586    1297        800                    0
       A at seed 118 (seed noise)   0.10    911    3660           0.2489       899       0.2482       0.2472     0.2474             0.2399   25758          799        12611       0        836                    9
                    A: shipped 19   0.10    902    3660           0.2464       891       0.2460       0.2526     0.2453             0.2360   26180          822        10822       0        930                    3
           B: + distance to limit   0.10    923    3660           0.2522       910       0.2512       0.2614     0.2531             0.2382   25920          876        11560    2066        586                    2
                  C: + BBCE limit   0.10    917    3660           0.2505       895       0.2471       0.2564     0.2486             0.2427   25676          777        11118     915       1434                    3
            D: + distance + limit   0.10    881    3660           0.2407       867       0.2394       0.2375     0.2388             0.2309   25201          797        10510    2424       1230                   10
 E: + income % of poverty + limit   0.10    897    3660           0.2451       878       0.2424       0.2500     0.2445             0.2314   25003          828        10021    2246       1366                   12
```
errors_np / precision_np: the same lists scored on FY2022 cases that are not income-pile cases. sel_* count rule slots across the 49 lists: using a new feature; limited by a state-level feature (BBCE flag or limit); and of those, with a 99% bound at or below their own regime's training error rate.

## 2. Each arm minus A, paired by state
| arm | budget | cases | errors caught | pooled precision | median | mean | states up / down | worse by > 0.05 / better by > 0.05 | mean dollar recall |
|---|---|---|---|---|---|---|---|---|---|
| A at seed 118 (seed noise) | 5% | all | -6 | -0.0031 | +0.0000 | +0.0002 | 18 / 22 | 9 / 10 | -0.0036 |
| A at seed 118 (seed noise) | 5% | nonpile | -6 | -0.0033 | +0.0000 | -0.0008 | 18 / 21 | 9 / 9 | -0.0025 |
| A at seed 118 (seed noise) | 10% | all | +9 | +0.0025 | +0.0000 | +0.0021 | 19 / 19 | 2 / 5 | +0.0039 |
| A at seed 118 (seed noise) | 10% | nonpile | +8 | +0.0022 | +0.0000 | +0.0025 | 22 / 19 | 2 / 2 | +0.0058 |
| B: + distance to limit | 5% | all | -24 | -0.0130 | +0.0000 | -0.0067 | 19 / 24 | 11 / 9 | -0.0062 |
| B: + distance to limit | 5% | nonpile | -29 | -0.0160 | +0.0000 | -0.0111 | 18 / 24 | 15 / 8 | -0.0083 |
| B: + distance to limit | 10% | all | +21 | +0.0057 | +0.0000 | +0.0078 | 21 / 17 | 4 / 7 | +0.0022 |
| B: + distance to limit | 10% | nonpile | +19 | +0.0052 | +0.0000 | +0.0077 | 22 / 16 | 2 / 5 | +0.0028 |
| C: + BBCE limit | 5% | all | -29 | -0.0159 | +0.0000 | -0.0139 | 14 / 23 | 12 / 6 | -0.0055 |
| C: + BBCE limit | 5% | nonpile | -35 | -0.0195 | +0.0000 | -0.0193 | 12 / 23 | 14 / 5 | -0.0071 |
| C: + BBCE limit | 10% | all | +15 | +0.0041 | +0.0000 | +0.0033 | 22 / 19 | 4 / 6 | +0.0066 |
| C: + BBCE limit | 10% | nonpile | +4 | +0.0011 | +0.0000 | -0.0011 | 20 / 17 | 5 / 3 | +0.0015 |
| D: + distance + limit | 5% | all | -13 | -0.0070 | -0.0208 | -0.0040 | 20 / 25 | 10 / 9 | -0.0045 |
| D: + distance + limit | 5% | nonpile | -21 | -0.0115 | -0.0213 | -0.0090 | 18 / 26 | 11 / 9 | -0.0068 |
| D: + distance + limit | 10% | all | -21 | -0.0057 | -0.0102 | -0.0065 | 19 / 25 | 5 / 3 | -0.0051 |
| D: + distance + limit | 10% | nonpile | -24 | -0.0066 | +0.0000 | -0.0069 | 18 / 23 | 6 / 4 | -0.0052 |
| E: + income % of poverty + limit | 5% | all | -2 | -0.0009 | +0.0000 | +0.0020 | 23 / 19 | 7 / 11 | -0.0037 |
| E: + income % of poverty + limit | 5% | nonpile | -7 | -0.0037 | +0.0000 | -0.0012 | 23 / 17 | 9 / 6 | -0.0046 |
| E: + income % of poverty + limit | 10% | all | -5 | -0.0014 | +0.0000 | -0.0008 | 21 / 18 | 5 / 3 | -0.0047 |
| E: + income % of poverty + limit | 10% | nonpile | -13 | -0.0036 | +0.0000 | -0.0025 | 20 / 21 | 5 / 3 | -0.0052 |

Subgroup: minority regimes in FY2022 (no BBCE, or BBCE at 130%). Errors caught, each arm minus A.
```
                              arm budget states errors flagged errors_np minus_A minus_A_np
       A at seed 118 (seed noise)   0.05     15    141     589       141      -4         -1
                    A: shipped 19   0.05     15    145     589       142       0          0
           B: + distance to limit   0.05     15    144     589       144      -1          2
                  C: + BBCE limit   0.05     15    140     589       140      -5         -2
            D: + distance + limit   0.05     15    135     589       133     -10         -9
 E: + income % of poverty + limit   0.05     15    139     589       142      -6          0
       A at seed 118 (seed noise)   0.10     15    258    1183       251      -2         -3
                    A: shipped 19   0.10     15    260    1183       254       0          0
           B: + distance to limit   0.10     15    263    1183       261       3          7
                  C: + BBCE limit   0.10     15    265    1183       264       5         10
            D: + distance + limit   0.10     15    248    1183       245     -12         -9
 E: + income % of poverty + limit   0.10     15    259    1183       252      -1         -2
```

Subgroup: limit or BBCE status in FY2022 differs from some FY2017-19 year. Errors caught, each arm minus A.
```
                              arm budget states errors flagged errors_np minus_A minus_A_np
       A at seed 118 (seed noise)   0.05     10    127     409       127       3          2
                    A: shipped 19   0.05     10    124     409       125       0          0
           B: + distance to limit   0.05     10    115     409       113      -9        -12
                  C: + BBCE limit   0.05     10    114     409       112     -10        -13
            D: + distance + limit   0.05     10    121     409       116      -3         -9
 E: + income % of poverty + limit   0.05     10    119     409       114      -5        -11
       A at seed 118 (seed noise)   0.10     10    217     823       212       9          7
                    A: shipped 19   0.10     10    208     823       205       0          0
           B: + distance to limit   0.10     10    214     823       212       6          7
                  C: + BBCE limit   0.10     10    225     823       217      17         12
            D: + distance + limit   0.10     10    207     823       201      -1         -4
 E: + income % of poverty + limit   0.10     10    215     823       210       7          5
```

## 3. Rules: how often the new features are used, and the regime check
```
                              arm visible tagged pile_dropped uses_dist uses_limit uses_pov uses_bbce_flag top1000_new top1000_test_precision top1000_below_bound regime_limited regime_below_own regime_below_own_top1000
                    A: shipped 19   26180     89          822         0          0        0           3633           0                 0.2705              0.4611           3633              426                        0
       A at seed 118 (seed noise)   25758     65          799         0          0        0           3567           0                 0.2713              0.4450           3567              417                        0
           B: + distance to limit   25920     73          876      5749          0        0           3176         178                 0.2720              0.4342           3176              364                        0
                  C: + BBCE limit   25676     81          777         0       4984        0           2924         179                 0.2706              0.4498           7699             1227                        0
            D: + distance + limit   25201     47          797      5114       4451        0           2649         341                 0.2678              0.4200           6921             1090                        0
 E: + income % of poverty + limit   25003     51          828         0       4434     5562           2726         354                 0.2661              0.4526           6978             1067                        0
```
uses_* count visible rules with a condition on that feature. regime_below_own: regime-limited rules whose 99% bound is at or below their own regime's training error rate in their household-size group (they pass admission on the regime's base rate alone).

Top 1,000 rules on FY2022, those using a new feature against the rest: flag-weighted test precision.
```
                              arm rules test_precision_new test_precision_other
                    A: shipped 19     0                 NA               0.2705
       A at seed 118 (seed noise)     0                 NA               0.2713
           B: + distance to limit   178             0.2696               0.2724
                  C: + BBCE limit   179             0.2601               0.2737
            D: + distance + limit   341             0.2650               0.2699
 E: + income % of poverty + limit   354             0.2616               0.2695
```
