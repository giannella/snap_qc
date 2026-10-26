# BBCE-limit features, night 2: mtry 3 and 4, and the light state options
Generated 2026-10-24 01:00. Mined FY2017-19 with ranger alone (1000 trees, depth 4), seed 117 unless noted; scored FY2022. National pool only, mismatch-tagged and income-pile-tagged rules dropped, 49 state lists, shipped fill, 5% and 10% budgets. Arms on disk: A, A_seed118, D, E, A_mtry3, D_mtry3, E_mtry3, A_mtry4, D_mtry4, E_mtry4, A_mtry4_seed118, T1.

## 0. How to read one seed
A shared seed does not pair the arms: a different feature list or mtry changes ranger's random draws, so each difference is a difference of two independent draws, and the best of several arms is a selected maximum. The thresholds screen for more seeds; they are not evidence that an arm is better, and a difference inside them is no evidence that it is not.
Thresholds, errors caught on all cases: 5% budget 25, 10% budget 57 (the larger of the three-seed range on xgboost + ranger pools in this window, 25 / 57, and arm A's seed-118 minus seed-117 difference at mtry 2, 6 / 9, and at mtry 4, 20 / 44).
There is no seed-noise reading at mtry 3; the mtry-2 and mtry-4 readings stand in for it.
More seeds for an arm only if it beats arm A at mtry 2 (what ships) by more than both thresholds, its non-pile change has the same sign at both budgets, its median and mean changes are positive, and it harms no more states than it helps.

## 1. Pooled over the 49 state lists
```
                                      arm budget errors flagged pooled_precision errors_np precision_np median_state mean_state mean_dollar_recall visible pile_dropped deepest_rank sel_new sel_regime
         A, mtry 2, seed 118 (seed noise)   0.05    498    1821           0.2735       491       0.2732       0.2619     0.2722             0.1228   25758          799         2748       0        549
         A, mtry 4, seed 118 (seed noise)   0.05    512    1822           0.2810       506       0.2816       0.2667     0.2840             0.1313   24892         1648         3705       0        468
                    A: shipped 19, mtry 2   0.05    504    1822           0.2766       497       0.2766       0.2927     0.2720             0.1263   26180          822         2721       0        608
                    A: shipped 19, mtry 3   0.05    497    1822           0.2728       477       0.2654       0.2766     0.2794             0.1281   25656         1304         3390       0        496
                    A: shipped 19, mtry 4   0.05    492    1822           0.2700       476       0.2649       0.2619     0.2684             0.1243   24958         1637         3448       0        415
            D: + distance + limit, mtry 2   0.05    491    1821           0.2696       476       0.2650       0.2750     0.2680             0.1218   25201          797         2805    1378        750
            D: + distance + limit, mtry 3   0.05    502    1822           0.2755       497       0.2767       0.2955     0.2730             0.1259   24802         1214         2935    1440        737
            D: + distance + limit, mtry 4   0.05    508    1822           0.2788       503       0.2799       0.2826     0.2789             0.1301   24033         1615         3804    1630        564
 E: + income % of poverty + limit, mtry 2   0.05    502    1821           0.2757       490       0.2728       0.2647     0.2740             0.1227   25003          828         3586    1297        800
 E: + income % of poverty + limit, mtry 3   0.05    514    1822           0.2821       505       0.2812       0.2727     0.2764             0.1288   24309         1264         3942    1351        889
 E: + income % of poverty + limit, mtry 4   0.05    520    1822           0.2854       506       0.2816       0.2889     0.2839             0.1329   23356         1701         3624    1587        693
    T1: + six light state options, mtry 2   0.05    483    1822           0.2651       469       0.2610       0.2857     0.2648             0.1165   25859          474         2280     903       1122
         A, mtry 2, seed 118 (seed noise)   0.10    911    3660           0.2489       899       0.2482       0.2472     0.2474             0.2399   25758          799        12611       0        836
         A, mtry 4, seed 118 (seed noise)   0.10    947    3660           0.2587       931       0.2570       0.2716     0.2602             0.2424   24892         1648        14819       0        747
                    A: shipped 19, mtry 2   0.10    902    3660           0.2464       891       0.2460       0.2526     0.2453             0.2360   26180          822        10822       0        930
                    A: shipped 19, mtry 3   0.10    930    3660           0.2541       909       0.2510       0.2644     0.2541             0.2404   25656         1304        12570       0        773
                    A: shipped 19, mtry 4   0.10    903    3660           0.2467       884       0.2441       0.2500     0.2458             0.2352   24958         1637        11979       0        583
            D: + distance + limit, mtry 2   0.10    881    3660           0.2407       867       0.2394       0.2375     0.2388             0.2309   25201          797        10510    2424       1230
            D: + distance + limit, mtry 3   0.10    954    3660           0.2607       936       0.2584       0.2639     0.2599             0.2507   24802         1214        11232    2529       1152
            D: + distance + limit, mtry 4   0.10    908    3660           0.2481       890       0.2457       0.2500     0.2468             0.2358   24033         1615        13640    2798       1005
 E: + income % of poverty + limit, mtry 2   0.10    897    3660           0.2451       878       0.2424       0.2500     0.2445             0.2314   25003          828        10021    2246       1366
 E: + income % of poverty + limit, mtry 3   0.10    972    3660           0.2656       945       0.2609       0.2821     0.2634             0.2499   24309         1264        10746    2295       1363
 E: + income % of poverty + limit, mtry 4   0.10    913    3660           0.2495       899       0.2482       0.2632     0.2504             0.2381   23356         1701        11806    2601       1159
    T1: + six light state options, mtry 2   0.10    888    3660           0.2426       875       0.2416       0.2500     0.2417             0.2376   25859          474        12487    1627       1965
 sel_regime_below_own
                    0
                    0
                    0
                    0
                    0
                    0
                    0
                    0
                    0
                    0
                    0
                    2
                    9
                    4
                    3
                    5
                    3
                   10
                   11
                    6
                   12
                   22
                    3
                   19
```
errors_np / precision_np: the same lists scored on FY2022 cases that are not income-pile cases. sel_* count rule slots across the 49 lists: using a feature the arm adds; limited by a state-level feature (BBCE flag or limit, or a light option); and of those, with a 99% bound at or below their own regime's training error rate.

## 2. Paired by state

### Seed noise
| arm | minus | budget | cases | errors caught | pooled precision | median | mean | states up / down | worse by > 0.05 / better by > 0.05 | mean dollar recall |
|---|---|---|---|---|---|---|---|---|---|---|
| A, mtry 2, seed 118 (seed noise) | A | 5% | all | -6 | -0.0031 | +0.0000 | +0.0002 | 18 / 22 | 9 / 10 | -0.0036 |
| A, mtry 2, seed 118 (seed noise) | A | 5% | nonpile | -6 | -0.0033 | +0.0000 | -0.0008 | 18 / 21 | 9 / 9 | -0.0025 |
| A, mtry 2, seed 118 (seed noise) | A | 10% | all | +9 | +0.0025 | +0.0000 | +0.0021 | 19 / 19 | 2 / 5 | +0.0039 |
| A, mtry 2, seed 118 (seed noise) | A | 10% | nonpile | +8 | +0.0022 | +0.0000 | +0.0025 | 22 / 19 | 2 / 2 | +0.0058 |
| A, mtry 4, seed 118 (seed noise) | A_mtry4 | 5% | all | +20 | +0.0110 | +0.0000 | +0.0156 | 21 / 15 | 4 / 13 | +0.0070 |
| A, mtry 4, seed 118 (seed noise) | A_mtry4 | 5% | nonpile | +30 | +0.0167 | +0.0000 | +0.0208 | 24 / 13 | 5 / 13 | +0.0118 |
| A, mtry 4, seed 118 (seed noise) | A_mtry4 | 10% | all | +44 | +0.0120 | +0.0102 | +0.0144 | 25 / 15 | 2 / 9 | +0.0072 |
| A, mtry 4, seed 118 (seed noise) | A_mtry4 | 10% | nonpile | +47 | +0.0130 | +0.0000 | +0.0141 | 24 / 15 | 2 / 7 | +0.0046 |

### mtry alone (the shipped 19 features)
| arm | minus | budget | cases | errors caught | pooled precision | median | mean | states up / down | worse by > 0.05 / better by > 0.05 | mean dollar recall |
|---|---|---|---|---|---|---|---|---|---|---|
| A: shipped 19, mtry 3 | A | 5% | all | -7 | -0.0038 | +0.0000 | +0.0074 | 19 / 20 | 9 / 12 | +0.0018 |
| A: shipped 19, mtry 3 | A | 5% | nonpile | -20 | -0.0111 | +0.0000 | -0.0017 | 15 / 21 | 10 / 9 | -0.0022 |
| A: shipped 19, mtry 3 | A | 10% | all | +28 | +0.0077 | +0.0100 | +0.0089 | 25 / 18 | 2 / 8 | +0.0044 |
| A: shipped 19, mtry 3 | A | 10% | nonpile | +18 | +0.0050 | +0.0113 | +0.0064 | 25 / 18 | 2 / 5 | +0.0015 |
| A: shipped 19, mtry 4 | A | 5% | all | -12 | -0.0066 | +0.0000 | -0.0037 | 17 / 21 | 11 / 8 | -0.0020 |
| A: shipped 19, mtry 4 | A | 5% | nonpile | -21 | -0.0117 | +0.0000 | -0.0092 | 15 / 23 | 14 / 9 | -0.0049 |
| A: shipped 19, mtry 4 | A | 10% | all | +1 | +0.0003 | +0.0000 | +0.0005 | 22 / 20 | 5 / 5 | -0.0008 |
| A: shipped 19, mtry 4 | A | 10% | nonpile | -7 | -0.0019 | +0.0000 | -0.0010 | 17 / 20 | 4 / 5 | -0.0003 |

### Limit features at the same mtry
| arm | minus | budget | cases | errors caught | pooled precision | median | mean | states up / down | worse by > 0.05 / better by > 0.05 | mean dollar recall |
|---|---|---|---|---|---|---|---|---|---|---|
| D: + distance + limit, mtry 2 | A | 5% | all | -13 | -0.0070 | -0.0208 | -0.0040 | 20 / 25 | 10 / 9 | -0.0045 |
| D: + distance + limit, mtry 2 | A | 5% | nonpile | -21 | -0.0115 | -0.0213 | -0.0090 | 18 / 26 | 11 / 9 | -0.0068 |
| D: + distance + limit, mtry 2 | A | 10% | all | -21 | -0.0057 | -0.0102 | -0.0065 | 19 / 25 | 5 / 3 | -0.0051 |
| D: + distance + limit, mtry 2 | A | 10% | nonpile | -24 | -0.0066 | +0.0000 | -0.0069 | 18 / 23 | 6 / 4 | -0.0052 |
| E: + income % of poverty + limit, mtry 2 | A | 5% | all | -2 | -0.0009 | +0.0000 | +0.0020 | 23 / 19 | 7 / 11 | -0.0037 |
| E: + income % of poverty + limit, mtry 2 | A | 5% | nonpile | -7 | -0.0037 | +0.0000 | -0.0012 | 23 / 17 | 9 / 6 | -0.0046 |
| E: + income % of poverty + limit, mtry 2 | A | 10% | all | -5 | -0.0014 | +0.0000 | -0.0008 | 21 / 18 | 5 / 3 | -0.0047 |
| E: + income % of poverty + limit, mtry 2 | A | 10% | nonpile | -13 | -0.0036 | +0.0000 | -0.0025 | 20 / 21 | 5 / 3 | -0.0052 |
| D: + distance + limit, mtry 3 | A_mtry3 | 5% | all | +5 | +0.0027 | +0.0000 | -0.0064 | 22 / 18 | 13 / 8 | -0.0022 |
| D: + distance + limit, mtry 3 | A_mtry3 | 5% | nonpile | +20 | +0.0113 | +0.0000 | +0.0039 | 23 / 15 | 8 / 12 | +0.0019 |
| D: + distance + limit, mtry 3 | A_mtry3 | 10% | all | +24 | +0.0066 | +0.0000 | +0.0057 | 24 / 16 | 2 / 4 | +0.0104 |
| D: + distance + limit, mtry 3 | A_mtry3 | 10% | nonpile | +27 | +0.0075 | +0.0103 | +0.0060 | 25 / 16 | 5 / 5 | +0.0100 |
| E: + income % of poverty + limit, mtry 3 | A_mtry3 | 5% | all | +17 | +0.0093 | +0.0000 | -0.0030 | 24 / 19 | 10 / 11 | +0.0007 |
| E: + income % of poverty + limit, mtry 3 | A_mtry3 | 5% | nonpile | +28 | +0.0157 | +0.0209 | +0.0049 | 25 / 13 | 8 / 14 | +0.0037 |
| E: + income % of poverty + limit, mtry 3 | A_mtry3 | 10% | all | +42 | +0.0115 | +0.0128 | +0.0093 | 28 / 13 | 7 / 8 | +0.0095 |
| E: + income % of poverty + limit, mtry 3 | A_mtry3 | 10% | nonpile | +36 | +0.0099 | +0.0218 | +0.0077 | 28 / 15 | 6 / 4 | +0.0086 |
| D: + distance + limit, mtry 4 | A_mtry4 | 5% | all | +16 | +0.0088 | +0.0000 | +0.0106 | 20 / 16 | 8 / 14 | +0.0058 |
| D: + distance + limit, mtry 4 | A_mtry4 | 5% | nonpile | +27 | +0.0150 | +0.0000 | +0.0168 | 22 / 18 | 6 / 16 | +0.0069 |
| D: + distance + limit, mtry 4 | A_mtry4 | 10% | all | +5 | +0.0014 | +0.0000 | +0.0010 | 17 / 20 | 1 / 5 | +0.0006 |
| D: + distance + limit, mtry 4 | A_mtry4 | 10% | nonpile | +6 | +0.0017 | +0.0000 | +0.0013 | 21 / 21 | 3 / 4 | -0.0025 |
| E: + income % of poverty + limit, mtry 4 | A_mtry4 | 5% | all | +28 | +0.0154 | +0.0217 | +0.0156 | 26 / 17 | 7 / 13 | +0.0086 |
| E: + income % of poverty + limit, mtry 4 | A_mtry4 | 5% | nonpile | +30 | +0.0167 | +0.0000 | +0.0178 | 23 / 14 | 8 / 15 | +0.0094 |
| E: + income % of poverty + limit, mtry 4 | A_mtry4 | 10% | all | +10 | +0.0027 | +0.0000 | +0.0046 | 20 / 19 | 5 / 7 | +0.0029 |
| E: + income % of poverty + limit, mtry 4 | A_mtry4 | 10% | nonpile | +15 | +0.0041 | +0.0000 | +0.0055 | 19 / 18 | 3 / 6 | +0.0022 |

### Limit features at mtry 3 or 4 against the shipped setting (A, mtry 2)
| arm | minus | budget | cases | errors caught | pooled precision | median | mean | states up / down | worse by > 0.05 / better by > 0.05 | mean dollar recall |
|---|---|---|---|---|---|---|---|---|---|---|
| D: + distance + limit, mtry 3 | A | 5% | all | -2 | -0.0011 | +0.0000 | +0.0010 | 16 / 17 | 6 / 7 | -0.0005 |
| D: + distance + limit, mtry 3 | A | 5% | nonpile | +0 | +0.0002 | +0.0000 | +0.0022 | 18 / 19 | 6 / 8 | -0.0002 |
| D: + distance + limit, mtry 3 | A | 10% | all | +52 | +0.0142 | +0.0139 | +0.0146 | 29 / 14 | 1 / 9 | +0.0147 |
| D: + distance + limit, mtry 3 | A | 10% | nonpile | +45 | +0.0124 | +0.0119 | +0.0124 | 27 / 11 | 2 / 7 | +0.0115 |
| E: + income % of poverty + limit, mtry 3 | A | 5% | all | +10 | +0.0055 | +0.0000 | +0.0043 | 23 / 16 | 6 / 9 | +0.0024 |
| E: + income % of poverty + limit, mtry 3 | A | 5% | nonpile | +8 | +0.0046 | +0.0000 | +0.0032 | 20 / 18 | 8 / 9 | +0.0015 |
| E: + income % of poverty + limit, mtry 3 | A | 10% | all | +70 | +0.0191 | +0.0233 | +0.0181 | 31 / 13 | 4 / 11 | +0.0138 |
| E: + income % of poverty + limit, mtry 3 | A | 10% | nonpile | +54 | +0.0149 | +0.0206 | +0.0140 | 29 / 13 | 3 / 9 | +0.0101 |
| D: + distance + limit, mtry 4 | A | 5% | all | +4 | +0.0022 | +0.0000 | +0.0069 | 23 / 17 | 11 / 11 | +0.0038 |
| D: + distance + limit, mtry 4 | A | 5% | nonpile | +6 | +0.0033 | +0.0000 | +0.0076 | 24 / 18 | 8 / 12 | +0.0021 |
| D: + distance + limit, mtry 4 | A | 10% | all | +6 | +0.0016 | +0.0000 | +0.0015 | 20 / 20 | 2 / 4 | -0.0002 |
| D: + distance + limit, mtry 4 | A | 10% | nonpile | -1 | -0.0003 | +0.0000 | +0.0003 | 19 / 24 | 2 / 3 | -0.0028 |
| E: + income % of poverty + limit, mtry 4 | A | 5% | all | +16 | +0.0088 | +0.0232 | +0.0119 | 26 / 17 | 8 / 15 | +0.0066 |
| E: + income % of poverty + limit, mtry 4 | A | 5% | nonpile | +9 | +0.0050 | +0.0000 | +0.0086 | 24 / 16 | 8 / 14 | +0.0045 |
| E: + income % of poverty + limit, mtry 4 | A | 10% | all | +11 | +0.0030 | +0.0000 | +0.0051 | 20 / 16 | 1 / 4 | +0.0021 |
| E: + income % of poverty + limit, mtry 4 | A | 10% | nonpile | +8 | +0.0022 | +0.0000 | +0.0045 | 21 / 19 | 2 / 4 | +0.0019 |

### Tier 1: the six light state options
| arm | minus | budget | cases | errors caught | pooled precision | median | mean | states up / down | worse by > 0.05 / better by > 0.05 | mean dollar recall |
|---|---|---|---|---|---|---|---|---|---|---|
| T1: + six light state options, mtry 2 | A | 5% | all | -21 | -0.0115 | -0.0208 | -0.0072 | 15 / 25 | 12 / 9 | -0.0098 |
| T1: + six light state options, mtry 2 | A | 5% | nonpile | -28 | -0.0156 | -0.0238 | -0.0118 | 15 / 28 | 12 / 7 | -0.0114 |
| T1: + six light state options, mtry 2 | A | 10% | all | -14 | -0.0038 | +0.0000 | -0.0036 | 20 / 23 | 6 / 6 | +0.0016 |
| T1: + six light state options, mtry 2 | A | 10% | nonpile | -16 | -0.0044 | +0.0000 | -0.0041 | 20 / 22 | 5 / 5 | +0.0012 |

Subgroup (as night 1): minority regimes in FY2022 (no BBCE, or BBCE at 130%) (15 states). Errors caught, each arm minus A at mtry 2.
```
                                      arm budget states errors flagged errors_np minus_A minus_A_np
         A, mtry 2, seed 118 (seed noise)   0.05     15    141     589       141      -4         -1
         A, mtry 4, seed 118 (seed noise)   0.05     15    145     589       146       0          4
                    A: shipped 19, mtry 2   0.05     15    145     589       142       0          0
                    A: shipped 19, mtry 3   0.05     15    133     589       129     -12        -13
                    A: shipped 19, mtry 4   0.05     15    147     589       144       2          2
            D: + distance + limit, mtry 2   0.05     15    135     589       133     -10         -9
            D: + distance + limit, mtry 3   0.05     15    141     589       139      -4         -3
            D: + distance + limit, mtry 4   0.05     15    144     589       142      -1          0
 E: + income % of poverty + limit, mtry 2   0.05     15    139     589       142      -6          0
 E: + income % of poverty + limit, mtry 3   0.05     15    143     589       143      -2          1
 E: + income % of poverty + limit, mtry 4   0.05     15    152     589       146       7          4
         A, mtry 2, seed 118 (seed noise)   0.10     15    258    1183       251      -2         -3
         A, mtry 4, seed 118 (seed noise)   0.10     15    258    1183       254      -2          0
                    A: shipped 19, mtry 2   0.10     15    260    1183       254       0          0
                    A: shipped 19, mtry 3   0.10     15    258    1183       253      -2         -1
                    A: shipped 19, mtry 4   0.10     15    259    1183       259      -1          5
            D: + distance + limit, mtry 2   0.10     15    248    1183       245     -12         -9
            D: + distance + limit, mtry 3   0.10     15    267    1183       265       7         11
            D: + distance + limit, mtry 4   0.10     15    255    1183       248      -5         -6
 E: + income % of poverty + limit, mtry 2   0.10     15    259    1183       252      -1         -2
 E: + income % of poverty + limit, mtry 3   0.10     15    279    1183       273      19         19
 E: + income % of poverty + limit, mtry 4   0.10     15    251    1183       250      -9         -4
```

Subgroup (as night 1): limit or BBCE status in FY2022 differs from some FY2017-19 year (10 states). Errors caught, each arm minus A at mtry 2.
```
                                      arm budget states errors flagged errors_np minus_A minus_A_np
         A, mtry 2, seed 118 (seed noise)   0.05     10    127     409       127       3          2
         A, mtry 4, seed 118 (seed noise)   0.05     10    123     409       120      -1         -5
                    A: shipped 19, mtry 2   0.05     10    124     409       125       0          0
                    A: shipped 19, mtry 3   0.05     10    121     409       117      -3         -8
                    A: shipped 19, mtry 4   0.05     10    129     409       126       5          1
            D: + distance + limit, mtry 2   0.05     10    121     409       116      -3         -9
            D: + distance + limit, mtry 3   0.05     10    132     409       132       8          7
            D: + distance + limit, mtry 4   0.05     10    128     409       126       4          1
 E: + income % of poverty + limit, mtry 2   0.05     10    119     409       114      -5        -11
 E: + income % of poverty + limit, mtry 3   0.05     10    132     409       128       8          3
 E: + income % of poverty + limit, mtry 4   0.05     10    125     409       120       1         -5
         A, mtry 2, seed 118 (seed noise)   0.10     10    217     823       212       9          7
         A, mtry 4, seed 118 (seed noise)   0.10     10    219     823       214      11          9
                    A: shipped 19, mtry 2   0.10     10    208     823       205       0          0
                    A: shipped 19, mtry 3   0.10     10    213     823       211       5          6
                    A: shipped 19, mtry 4   0.10     10    210     823       203       2         -2
            D: + distance + limit, mtry 2   0.10     10    207     823       201      -1         -4
            D: + distance + limit, mtry 3   0.10     10    230     823       225      22         20
            D: + distance + limit, mtry 4   0.10     10    217     823       209       9          4
 E: + income % of poverty + limit, mtry 2   0.10     10    215     823       210       7          5
 E: + income % of poverty + limit, mtry 3   0.10     10    229     823       225      21         20
 E: + income % of poverty + limit, mtry 4   0.10     10    218     823       212      10          7
```

Subgroup for T1: states whose light options in FY2022 differ from some FY2017-19 year (20). Errors caught, T1 minus A.
```
                                   arm budget states errors flagged errors_np minus_A minus_A_np
                 A: shipped 19, mtry 2   0.05     20    208     743       205       0          0
 T1: + six light state options, mtry 2   0.05     20    198     743       191     -10        -14
                 A: shipped 19, mtry 2   0.10     20    378    1490       375       0          0
 T1: + six light state options, mtry 2   0.10     20    375    1490       370      -3         -5
```

Subgroup for T1: states whose SMD is below Part B minus $35 in FY2022 (8). Errors caught, T1 minus A.
```
                                   arm budget states errors flagged errors_np minus_A minus_A_np
                 A: shipped 19, mtry 2   0.05      8     81     282        81       0          0
 T1: + six light state options, mtry 2   0.05      8     74     282        74      -7         -7
                 A: shipped 19, mtry 2   0.10      8    143     568       141       0          0
 T1: + six light state options, mtry 2   0.10      8    140     568       140      -3         -1
```

## 3. Rules: how often the added features are used, and the regime check
```
                                      arm visible tagged pile_dropped uses_dist uses_limit uses_pov uses_bbce_flag top1000_added top1000_test_precision regime_limited regime_below_own regime_below_own_top1000
                    A: shipped 19, mtry 2   26180     89          822         0          0        0           3633             0                 0.2705           3633              426                        0
         A, mtry 2, seed 118 (seed noise)   25758     65          799         0          0        0           3567             0                 0.2713           3567              417                        0
            D: + distance + limit, mtry 2   25201     47          797      5114       4451        0           2649           341                 0.2678           6921             1090                        0
 E: + income % of poverty + limit, mtry 2   25003     51          828         0       4434     5562           2726           354                 0.2661           6978             1067                        0
                    A: shipped 19, mtry 3   25656     56         1304         0          0        0           3466             0                 0.2858           3466              353                        0
            D: + distance + limit, mtry 3   24802     46         1214      5994       4660        0           2146           289                 0.2804           6658              958                        0
 E: + income % of poverty + limit, mtry 3   24309     43         1264         0       4778     6619           2236           308                 0.2800           6850              968                        0
                    A: shipped 19, mtry 4   24958     35         1637         0          0        0           2834             0                 0.2916           2834              316                        0
            D: + distance + limit, mtry 4   24033     43         1615      5834       4981        0           1628           295                 0.2861           6487              854                        0
 E: + income % of poverty + limit, mtry 4   23356     44         1701         0       5134     7213           1851           311                 0.2865           6842              880                        0
         A, mtry 4, seed 118 (seed noise)   24892     57         1648         0          0        0           3084             0                 0.2931           3084              355                        0
    T1: + six light state options, mtry 2   25859     54          474         0          0        0           2867           392                 0.2598          14908             2314                       22
```
uses_* count visible rules with a condition on that feature. regime_below_own: regime-limited rules whose 99% bound is at or below their own regime's training error rate in their household-size group.

Light options: visible rules using each.
```
                                   arm smd_gap_dollars smd_i esap_i cap_standard_i cap_modified_i county_admin_i se_other_method_i tba_i
 T1: + six light state options, mtry 2            3541  1686   1995           2040           1450           1413              2393  2277
```

Top 1,000 rules on FY2022, those using an added feature against the rest: flag-weighted test precision.
```
                                      arm rules test_precision_added test_precision_other
                    A: shipped 19, mtry 2     0                   NA               0.2705
         A, mtry 2, seed 118 (seed noise)     0                   NA               0.2713
            D: + distance + limit, mtry 2   341               0.2650               0.2699
 E: + income % of poverty + limit, mtry 2   354               0.2616               0.2695
                    A: shipped 19, mtry 3     0                   NA               0.2858
            D: + distance + limit, mtry 3   289               0.2737               0.2833
 E: + income % of poverty + limit, mtry 3   308               0.2759               0.2818
                    A: shipped 19, mtry 4     0                   NA               0.2916
            D: + distance + limit, mtry 4   295               0.2773               0.2891
 E: + income % of poverty + limit, mtry 4   311               0.2857               0.2868
         A, mtry 4, seed 118 (seed noise)     0                   NA               0.2931
    T1: + six light state options, mtry 2   392               0.2519               0.2645
```
