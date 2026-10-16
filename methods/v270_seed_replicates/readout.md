# v2.7 seed replicates: frame difference against seed movement
Generated 2026-10-04 11:35. National FY2022-23 mines on the 09-25 frame and the final frame at seeds 117, 118, 119; scored on FY2024. National pool only: no state pools, so these numbers do not carry to the blended benchmark.
The 09-25 and final frames differ in four ways at once: the fiscal-year keying of the CPI, the SUA tier computed on review-year amounts (439 FY2022-24 rows), October CPI values, and the $10 SUA-tier tolerance (593 rows). Sections 1-4 read them together; section 5 splits them into three steps, the first of which still carries the first two changes.

## Anchors and bookkeeping
- 09-25 frame, seed 117 re-mined vs the benchmark's cached pool: identical (54596 vs 54596 rules; 100.0% of rules shared).
- final frame, seed 117 re-mined vs the benchmark's cached pool: identical (54283 vs 54283 rules; 100.0% of rules shared).
- Both seed-117 pools reproduce the benchmark's, so seed 117 is the draw that raised the question; seeds 118 and 119 are the new draws.
```
 frame seed visible_rules tagged_and_dropped states cells_with_fill_gap deepest_rank_used
 f0925  117         53230               1366     49                   0             20838
 f0925  118         53071               1354     49                   0             21497
 f0925  119         53339               1337     49                   0             19848
 final  117         52977               1306     49                   0             20601
 final  118         53397               1304     49                   0             19636
 final  119         53039               1346     49                   0             20804
```

## 1. The national pool's rules in rank order: precision when the flagged share of FY2024 cases first reaches each line
A rule can carry the union past a line, so the flagged counts differ a little by pool; precision is the like-for-like figure, and errors and flagged counts are in pooled_curve_*.csv.
```
 share f0925 s117 f0925 s118 f0925 s119 final s117 final s118 final s119
  0.01     0.4621     0.4715     0.5037     0.4678     0.4978     0.5025
  0.02     0.4246     0.4179     0.4273     0.4377     0.4167     0.4257
  0.03     0.3740     0.3662     0.3790     0.3809     0.3650     0.3721
  0.04     0.3512     0.3285     0.3538     0.3392     0.3435     0.3538
  0.05     0.3353     0.3199     0.3260     0.3143     0.3316     0.3235
  0.06     0.3135     0.3029     0.3181     0.3131     0.3120     0.3092
  0.08     0.2930     0.2865     0.2893     0.2896     0.2890     0.2890
  0.10     0.2824     0.2838     0.2770     0.2816     0.2759     0.2817
  0.12     0.2722     0.2683     0.2710     0.2753     0.2728     0.2745
  0.15     0.2642     0.2595     0.2636     0.2682     0.2635     0.2636
```
```
 share          f0925_precision          final_precision final_minus_f0925_mean seed_sd_precision       f0925_errors       final_errors flagged_range                                             reading
  0.01 0.4621 / 0.4715 / 0.5037 0.4678 / 0.4978 / 0.5025                 0.0103            0.0204    201 / 190 / 204    196 / 225 / 204       403-452                                      ranges overlap
  0.02 0.4246 / 0.4179 / 0.4273 0.4377 / 0.4167 / 0.4257                 0.0034            0.0082    338 / 331 / 338    351 / 330 / 338       791-802                                      ranges overlap
  0.03 0.3740 / 0.3662 / 0.3790 0.3809 / 0.3650 / 0.3721                -0.0004            0.0072    460 / 438 / 451    454 / 446 / 442     1188-1230                                      ranges overlap
  0.04 0.3512 / 0.3285 / 0.3538 0.3392 / 0.3435 / 0.3538                 0.0010            0.0112    558 / 520 / 566    537 / 552 / 566     1583-1607                                      ranges overlap
  0.05 0.3353 / 0.3199 / 0.3260 0.3143 / 0.3316 / 0.3235                -0.0039            0.0082    674 / 633 / 649    633 / 656 / 646     1978-2014                                      ranges overlap
  0.06 0.3135 / 0.3029 / 0.3181 0.3131 / 0.3120 / 0.3092                -0.0001            0.0057    744 / 721 / 760    748 / 742 / 738     2373-2389                                      ranges overlap
  0.08 0.2930 / 0.2865 / 0.2893 0.2896 / 0.2890 / 0.2890                -0.0004            0.0023    928 / 908 / 916    926 / 943 / 921     3166-3263                                      ranges overlap
  0.10 0.2824 / 0.2838 / 0.2770 0.2816 / 0.2759 / 0.2817                -0.0013            0.0035 1118 / 1122 / 1096 1114 / 1092 / 1115     3954-3959                                      ranges overlap
  0.12 0.2722 / 0.2683 / 0.2710 0.2753 / 0.2728 / 0.2745                 0.0037            0.0017 1294 / 1274 / 1288 1307 / 1305 / 1303     4746-4783 all three final values above all three 09-25 values
  0.15 0.2642 / 0.2595 / 0.2636 0.2682 / 0.2635 / 0.2636                 0.0027            0.0026 1567 / 1541 / 1564 1591 / 1563 / 1571     5931-5959                                      ranges overlap
```

## 2. State lists from the national pool alone (shipped fill walk, 5% and 10% budgets)
```
 budget frame seed states errors flagged pooled_precision median_precision mean_precision mean_dollar_recall below_base core_rules_median
   0.05 f0925  117     49    660    1949           0.3386           0.3488         0.3349             0.1667          1                39
   0.05 f0925  118     49    667    1949           0.3422           0.3488         0.3382             0.1646          0                36
   0.05 f0925  119     49    674    1949           0.3458           0.3571         0.3414             0.1694          0                36
   0.05 final  117     49    661    1949           0.3391           0.3529         0.3351             0.1570          1                37
   0.05 final  118     49    665    1949           0.3412           0.3333         0.3351             0.1648          0                36
   0.05 final  119     49    661    1949           0.3391           0.3333         0.3342             0.1654          0                38
   0.10 f0925  117     49   1180    3930           0.3003           0.2903         0.2950             0.2906          0                73
   0.10 f0925  118     49   1185    3930           0.3015           0.3086         0.2968             0.2840          0                72
   0.10 f0925  119     49   1182    3930           0.3008           0.2985         0.2961             0.2854          0                72
   0.10 final  117     49   1145    3930           0.2913           0.2836         0.2855             0.2745          0                71
   0.10 final  118     49   1166    3930           0.2967           0.2874         0.2924             0.2868          0                71
   0.10 final  119     49   1173    3930           0.2985           0.2826         0.2950             0.2901          0                74
```
- PRIMARY. Budget 10%, pooled precision: 09-25 frame 0.3003 / 0.3015 / 0.3008; final frame 0.2913 / 0.2967 / 0.2985; all three final values below all three 09-25 values.
- Budget 5%, pooled precision: 09-25 frame 0.3386 / 0.3422 / 0.3458; final frame 0.3391 / 0.3412 / 0.3391; ranges overlap.
- A median state's 5% list is about 40 FY2024 cases (binomial SE about 0.07), so the 0.05 harmed / helped cut sits inside one standard error; the pooled figures rest on about 1,950 flags at 5% (SE about 0.011) and 3,930 at 10% (SE about 0.007).

Paired state changes (pool b minus pool a; frame contrasts are final minus 09-25; n_pos / n_neg = states up / down; harmed / helped = states beyond -0.05 / +0.05). For seed-only rows the sign is arbitrary.
```
                   kind         a         b budget states d_precision_median d_precision_mean n_pos n_neg harmed helped d_dollar_recall_mean d_errors
 frame, different seeds f0925 117 final 118   0.05     49             0.0000           0.0002    24    19      9      7              -0.0019        5
 frame, different seeds f0925 117 final 119   0.05     49             0.0000          -0.0007    22    22      9      8              -0.0013        1
 frame, different seeds f0925 118 final 117   0.05     49             0.0000          -0.0031    18    19      8      6              -0.0076       -6
 frame, different seeds f0925 118 final 119   0.05     49             0.0000          -0.0041    17    21      8      7               0.0008       -6
 frame, different seeds f0925 119 final 117   0.05     49             0.0000          -0.0063    19    20      8      9              -0.0125      -13
 frame, different seeds f0925 119 final 118   0.05     49             0.0000          -0.0063    17    24      9      8              -0.0047       -9
       frame, same seed f0925 117 final 117   0.05     49             0.0000           0.0002    19    21      8      5              -0.0097        1
       frame, same seed f0925 118 final 118   0.05     49             0.0000          -0.0032    17    15      7      6               0.0002       -2
       frame, same seed f0925 119 final 119   0.05     49             0.0000          -0.0072    18    20      6      4              -0.0040      -13
              seed only f0925 117 f0925 118   0.05     49             0.0196           0.0033    25    17      8      8              -0.0021        7
              seed only f0925 117 f0925 119   0.05     49             0.0217           0.0065    25    17     10      8               0.0028       14
              seed only f0925 118 f0925 119   0.05     49             0.0000           0.0032    20    21      7      9               0.0049        7
              seed only final 117 final 118   0.05     49             0.0000           0.0000    19    18      8      9               0.0078        4
              seed only final 117 final 119   0.05     49             0.0000          -0.0009    19    24     10     10               0.0084        0
              seed only final 118 final 119   0.05     49             0.0000          -0.0009    21    21      9     10               0.0006       -4
 frame, different seeds f0925 117 final 118   0.10     49             0.0000          -0.0026    23    19      5      2              -0.0037      -14
 frame, different seeds f0925 117 final 119   0.10     49             0.0000           0.0000    22    20      5      4              -0.0004       -7
 frame, different seeds f0925 118 final 117   0.10     49             0.0000          -0.0113    18    24      8      1              -0.0095      -40
 frame, different seeds f0925 118 final 119   0.10     49             0.0000          -0.0017    21    21      7      2               0.0062      -12
 frame, different seeds f0925 119 final 117   0.10     49            -0.0132          -0.0106    13    30      6      3              -0.0109      -37
 frame, different seeds f0925 119 final 118   0.10     49            -0.0102          -0.0038    18    25      5      4               0.0014      -16
       frame, same seed f0925 117 final 117   0.10     49            -0.0110          -0.0095    16    27      6      2              -0.0161      -35
       frame, same seed f0925 118 final 118   0.10     49             0.0000          -0.0044    21    23      6      3               0.0028      -19
       frame, same seed f0925 119 final 119   0.10     49             0.0000          -0.0011    19    24      3      5               0.0048       -9
              seed only f0925 117 f0925 118   0.10     49             0.0000           0.0018    19    22      2      3              -0.0066        5
              seed only f0925 117 f0925 119   0.10     49             0.0000           0.0011    16    21      1      5              -0.0052        2
              seed only f0925 118 f0925 119   0.10     49             0.0000          -0.0006    22    18      5      4               0.0014       -3
              seed only final 117 final 118   0.10     49             0.0102           0.0069    25    17      3      5               0.0123       21
              seed only final 117 final 119   0.10     49             0.0115           0.0096    26    18      6      9               0.0156       28
              seed only final 118 final 119   0.10     49             0.0103           0.0027    25    19      2      4               0.0033        7
```

How large is a frame contrast next to a seed contrast (absolute size of the mean paired change, and the tails):
```
 budget                                  kind2 mean_abs_d_precision_mean range_d_precision_mean harmed_range helped_range abs_d_errors_mean
   0.05 frame, final minus 09-25 (9 contrasts)                    0.0035     -0.0072 to +0.0002          6-9          4-9               6.2
   0.05                seed only (6 contrasts)                    0.0025     -0.0009 to +0.0065         7-10         8-10               6.0
   0.10 frame, final minus 09-25 (9 contrasts)                    0.0050     -0.0113 to +0.0000          3-8          1-5              21.0
   0.10                seed only (6 contrasts)                    0.0038     -0.0006 to +0.0096          1-6          3-9              11.0
```
The mean over the nine frame contrasts equals the difference of the two frames' three-seed means, so it is one number, not nine confirmations:
```
 budget final_minus_f0925_mean_of_means d_errors_mean
   0.05                         -0.0034          -4.7
   0.10                         -0.0050         -21.0
```

## 3. Rule level: the top of each pool on FY2024
```
 frame seed topK median_train_n flagwt_test_precision share_under_bound median_margin median_abs_log_reach share_lt10_test_flags
 f0925  117  200           92.0                0.4130            0.4133        0.0237               0.1700                0.0200
 f0925  117 1000          103.5                0.3546            0.2500        0.0471               0.1429                0.0200
 f0925  117 3000          190.0                0.3067            0.1664        0.0506               0.1161                0.0163
 f0925  118  200          115.0                0.4178            0.4394        0.0159               0.1577                0.0100
 f0925  118 1000          114.0                0.3648            0.2533        0.0502               0.1382                0.0090
 f0925  118 3000          202.0                0.3085            0.1627        0.0496               0.1139                0.0123
 f0925  119  200           82.0                0.4111            0.4184        0.0174               0.1609                0.0200
 f0925  119 1000          105.0                0.3587            0.2594        0.0474               0.1402                0.0130
 f0925  119 3000          183.0                0.3070            0.1759        0.0498               0.1171                0.0147
 final  117  200           91.0                0.4005            0.5054       -0.0012               0.1756                0.0700
 final  117 1000          107.5                0.3530            0.3058        0.0407               0.1483                0.0320
 final  117 3000          186.0                0.3057            0.1859        0.0490               0.1100                0.0173
 final  118  200          111.0                0.3978            0.5288       -0.0052               0.1477                0.0450
 final  118 1000          113.5                0.3629            0.3061        0.0431               0.1382                0.0200
 final  118 3000          190.0                0.3087            0.1860        0.0485               0.1112                0.0143
 final  119  200           93.0                0.3998            0.5105       -0.0011               0.1478                0.0500
 final  119 1000          105.5                0.3566            0.2989        0.0408               0.1388                0.0230
 final  119 3000          188.0                0.3063            0.1884        0.0480               0.1089                0.0163
```
- Top 200, share of rules below their bound: 09-25 frame 0.4133 / 0.4394 / 0.4184; final frame 0.5054 / 0.5288 / 0.5105; all three final values above all three 09-25 values.
- Top 1000, share of rules below their bound: 09-25 frame 0.2500 / 0.2533 / 0.2594; final frame 0.3058 / 0.3061 / 0.2989; all three final values above all three 09-25 values.

## 4. Pooling seeds: one seed's candidates, two seeds', all three (one BH pass per arm, same LCB ordering)
Like-for-like: precision when the union first reaches a share of FY2024 cases.
```
 frame share seeds arms candidates admitted rules_used flagged precision_mean precision_range errors_mean errors_range
 f0925  0.01     1    3     148677    54566         18     414         0.4791   0.4621-0.5037       198.3      190-204
 f0925  0.01     2    3     290986   106661         23     403         0.4945   0.4623-0.5176       199.3      184-208
 f0925  0.01     3    1     430113   157548         30     421         0.4988   0.4988-0.4988       210.0      210-210
 f0925  0.02     1    3     148677    54566         55     793         0.4233   0.4179-0.4273       335.7      331-338
 f0925  0.02     2    3     290986   106661         80     802         0.4197   0.4131-0.4280       336.7      328-351
 f0925  0.02     3    1     430113   157548         96     800         0.4188   0.4188-0.4188       335.0      335-335
 f0925  0.05     1    3     148677    54566        232    1993         0.3270   0.3199-0.3353       652.0      633-674
 f0925  0.05     2    3     290986   106661        352    1983         0.3248   0.3197-0.3323       644.0      634-658
 f0925  0.05     3    1     430113   157548        431    1986         0.3273   0.3273-0.3273       650.0      650-650
 f0925  0.10     1    3     148677    54566        432    3957         0.2810   0.2770-0.2838      1112.0    1096-1122
 f0925  0.10     2    3     290986   106661        685    3963         0.2758   0.2742-0.2789      1093.0    1085-1103
 f0925  0.10     3    1     430113   157548        883    3975         0.2765   0.2765-0.2765      1099.0    1099-1099
 final  0.01     1    3     148672    54456         20     426         0.4893   0.4678-0.5025       208.3      196-225
 final  0.01     2    3     290928   106423         24     415         0.5012   0.4826-0.5160       208.0      206-210
 final  0.01     3    1     430019   157187         31     445         0.4944   0.4944-0.4944       220.0      220-220
 final  0.02     1    3     148672    54456         52     796         0.4267   0.4167-0.4377       339.7      330-351
 final  0.02     2    3     290928   106423         71     792         0.4182   0.4121-0.4268       331.3      326-338
 final  0.02     3    1     430019   157187         81     795         0.4151   0.4151-0.4151       330.0      330-330
 final  0.05     1    3     148672    54456        221    1996         0.3231   0.3143-0.3316       645.0      633-656
 final  0.05     2    3     290928   106423        343    1991         0.3230   0.3197-0.3254       643.0      632-654
 final  0.05     3    1     430019   157187        423    2009         0.3250   0.3250-0.3250       653.0      653-653
 final  0.10     1    3     148672    54456        447    3957         0.2797   0.2759-0.2817      1107.0    1092-1115
 final  0.10     2    3     290928   106423        702    3960         0.2777   0.2761-0.2796      1099.7    1093-1109
 final  0.10     3    1     430019   157187        903    3959         0.2781   0.2781-0.2781      1101.0    1101-1101
```

NOT like-for-like across arms: the top K rules reach less deep into a pooled ranking, so read this table for what the top is made of (support, bound, share below bound), not as a performance comparison.
```
 frame topK seeds median_train_n share_train_n_lt50 median_lcb flagwt_test_precision share_under_bound median_margin
 f0925  200     1           96.3              0.200     0.3833                0.4140            0.4237        0.0190
 f0925  200     2          106.7              0.157     0.4125                0.4432            0.4396        0.0270
 f0925  200     3          102.5              0.140     0.4272                0.4605            0.4242        0.0402
 f0925 1000     1          107.5              0.186     0.3021                0.3594            0.2542        0.0482
 f0925 1000     2           93.5              0.211     0.3315                0.3851            0.3317        0.0362
 f0925 1000     3           92.0              0.217     0.3524                0.3970            0.3853        0.0254
 final  200     1           98.3              0.225     0.3913                0.3994            0.5149       -0.0025
 final  200     2           99.5              0.193     0.4192                0.4285            0.5337       -0.0080
 final  200     3           95.0              0.195     0.4341                0.4536            0.4785        0.0105
 final 1000     1          108.8              0.182     0.3062                0.3575            0.3036        0.0415
 final 1000     2           96.8              0.208     0.3410                0.3799            0.3847        0.0290
 final 1000     3           94.0              0.224     0.3624                0.3882            0.4429        0.0163
```

Overlap of the errors caught (Jaccard) between lists. Lists that share a seed overlap partly by construction:
```
 frame share                   kind pairs jaccard_mean jaccard_range
 f0925  0.05 1 vs 1 seeds, 0 shared     3        0.680   0.665-0.706
 f0925  0.05 1 vs 2 seeds, 0 shared     3        0.687   0.682-0.698
 f0925  0.05 1 vs 2 seeds, 1 shared     6        0.803   0.777-0.831
 f0925  0.05 1 vs 3 seeds, 1 shared     3        0.762   0.751-0.775
 f0925  0.05 2 vs 2 seeds, 1 shared     3        0.828   0.810-0.841
 f0925  0.05 2 vs 3 seeds, 2 shared     3        0.882   0.872-0.900
 f0925  0.10 1 vs 1 seeds, 0 shared     3        0.715   0.698-0.747
 f0925  0.10 1 vs 2 seeds, 0 shared     3        0.720   0.710-0.727
 f0925  0.10 1 vs 2 seeds, 1 shared     6        0.821   0.801-0.838
 f0925  0.10 1 vs 3 seeds, 1 shared     3        0.787   0.764-0.810
 f0925  0.10 2 vs 2 seeds, 1 shared     3        0.836   0.822-0.850
 f0925  0.10 2 vs 3 seeds, 2 shared     3        0.886   0.870-0.916
 final  0.05 1 vs 1 seeds, 0 shared     3        0.662   0.657-0.667
 final  0.05 1 vs 2 seeds, 0 shared     3        0.689   0.667-0.726
 final  0.05 1 vs 2 seeds, 1 shared     6        0.802   0.754-0.871
 final  0.05 1 vs 3 seeds, 1 shared     3        0.768   0.738-0.822
 final  0.05 2 vs 2 seeds, 1 shared     3        0.816   0.806-0.824
 final  0.05 2 vs 3 seeds, 2 shared     3        0.884   0.862-0.895
 final  0.10 1 vs 1 seeds, 0 shared     3        0.695   0.689-0.706
 final  0.10 1 vs 2 seeds, 0 shared     3        0.697   0.689-0.707
 final  0.10 1 vs 2 seeds, 1 shared     6        0.808   0.791-0.831
 final  0.10 1 vs 3 seeds, 1 shared     3        0.759   0.744-0.768
 final  0.10 2 vs 2 seeds, 1 shared     3        0.812   0.796-0.826
 final  0.10 2 vs 3 seeds, 2 shared     3        0.865   0.853-0.880
```

## 5. Decomposition: ordered steps from no CPI step (nominal) to the final frame (descriptive; no step has a primary statistic)
Step 1 carries two changes: the fiscal-year keying of the CPI (October-December rows' dollar features) and the SUA tier computed on review-year amounts (439 FY2022-24 rows move from tier 2 to tier 1, 229 of them in the training years, 207 of them among the 593 rows step 3 moves back). Step 2 changes only the six CPI-adjusted dollar features. Step 3 changes only the tier on 593 rows (1 to 2) and the utilities, shelter and total-deduction amounts on those rows. The steps are measured in this order, each with the earlier changes in place, so they add up to the 09-25-to-final total.
Step 0, when the no-CPI (nominal) frame is present, is itself a bundle: dollar amounts inflated to 2026 by the review month's calendar year with annual-average CPI and 2026 deduction tables, total deductions redefined to include the standard and homeless deductions, the ABAWD share over the reconstructed unit size (81 rows), and the SUA tier computed on inflated amounts (439 rows, reverted in step 1).
The clean comparison, when the CPI-off frame is present: the CPI-off frame is the final frame's own munging code run with its CPI switch (cpi_inflate_vars) off, so 'final minus CPI-off' is the CPI step alone, and 'CPI-off minus nominal' is every other change since the 09-21 frame. The two add up to the nominal-to-final total.
Frames with all three seeds scored: cpioff, f0925, f0928, f0929, final.
Pooled precision of the 49 national-only state lists, by frame and seed:
```
 budget  frame seed_117 seed_118 seed_119
   0.05  f0925   0.3386   0.3422   0.3458
   0.05  f0928   0.3445   0.3417   0.3434
   0.05  f0929   0.3443   0.3448   0.3309
   0.05  final   0.3391   0.3412   0.3391
   0.05 cpioff   0.3345   0.3335   0.3402
   0.10  f0925   0.3003   0.3015   0.3008
   0.10  f0928   0.2911   0.3048   0.3048
   0.10  f0929   0.2969   0.3005   0.2982
   0.10  final   0.2913   0.2967   0.2985
   0.10 cpioff   0.2929   0.3013   0.2936
```
Each step, after minus before. Columns with three values give seeds 117 / 118 / 119 in order. same_sign_of_3 counts how many of the three same-seed pooled differences share a sign. top1000_shared gives the share of the top 1,000 rules the two frames have in common at the same seed, beside the share two different seeds have in common on the later frame: where the same-seed share is high the two pools are nearly the same pool and the same-seed differences are close to paired; where it is near the cross-seed share they are nearly independent draws.
```
                                                                                 step budget  pooled_precision_before   pooled_precision_after            d_pooled_by_seed d_pooled_mean same_sign_of_3 d_errors_by_seed        d_state_mean_by_seed
 step 1: fiscal-year CPI keying + SUA tier on review-year amounts (09-28 minus 09-25)   0.05 0.3386 / 0.3422 / 0.3458 0.3445 / 0.3417 / 0.3434 +0.0059 / -0.0005 / -0.0024        0.0010              2    +11 / -1 / -5 +0.0047 / -0.0031 / -0.0039
 step 1: fiscal-year CPI keying + SUA tier on review-year amounts (09-28 minus 09-25)   0.10 0.3003 / 0.3015 / 0.3008 0.2911 / 0.3048 / 0.3048 -0.0092 / +0.0033 / +0.0040       -0.0006              2  -36 / +13 / +16 -0.0097 / +0.0022 / +0.0025
                                       step 2: October CPI values (09-29 minus 09-28)   0.05 0.3445 / 0.3417 / 0.3434 0.3443 / 0.3448 / 0.3309 -0.0002 / +0.0031 / -0.0125       -0.0032              2    +0 / +6 / -24 +0.0015 / +0.0042 / -0.0114
                                       step 2: October CPI values (09-29 minus 09-28)   0.10 0.2911 / 0.3048 / 0.3048 0.2969 / 0.3005 / 0.2982 +0.0058 / -0.0043 / -0.0066       -0.0017              2  +23 / -17 / -26 +0.0058 / -0.0035 / -0.0036
                                   step 3: $10 SUA-tier tolerance (final minus 09-29)   0.05 0.3443 / 0.3448 / 0.3309 0.3391 / 0.3412 / 0.3391 -0.0052 / -0.0036 / +0.0082       -0.0002              2   -10 / -7 / +16 -0.0059 / -0.0042 / +0.0081
                                   step 3: $10 SUA-tier tolerance (final minus 09-29)   0.10 0.2969 / 0.3005 / 0.2982 0.2913 / 0.2967 / 0.2985 -0.0056 / -0.0038 / +0.0003       -0.0030              2   -22 / -15 / +1 -0.0056 / -0.0031 / -0.0000
 clean: the CPI step alone (final minus CPI-off; same munging code, CPI switched off)   0.05 0.3345 / 0.3335 / 0.3402 0.3391 / 0.3412 / 0.3391 +0.0046 / +0.0077 / -0.0011        0.0037              2    +9 / +15 / -2 +0.0073 / +0.0073 / +0.0001
 clean: the CPI step alone (final minus CPI-off; same munging code, CPI switched off)   0.10 0.2929 / 0.3013 / 0.2936 0.2913 / 0.2967 / 0.2985 -0.0016 / -0.0046 / +0.0049       -0.0004              2   -6 / -18 / +19 -0.0025 / -0.0028 / +0.0059
      d_state_median_by_seed        states_up_down    harmed_helped d_mean_dollar_recall top1000_shared_same_seed top1000_shared_across_seeds        reading
 +0.0000 / +0.0000 / +0.0000 21/20 , 19/24 , 19/21 6/9 , 10/9 , 7/6              -0.0028       0.11 / 0.13 / 0.13                        0.02 ranges overlap
 -0.0097 / +0.0000 / +0.0000 19/25 , 21/21 , 21/19  7/1 , 3/5 , 1/4              -0.0005       0.11 / 0.13 / 0.13                        0.02 ranges overlap
 +0.0000 / +0.0000 / -0.0217 19/20 , 23/14 , 14/26 5/8 , 8/9 , 10/5              -0.0011       0.07 / 0.06 / 0.07                        0.02 ranges overlap
 +0.0103 / +0.0000 / +0.0000 25/16 , 21/22 , 17/24  4/3 , 7/4 , 4/4              -0.0026       0.07 / 0.06 / 0.07                        0.02 ranges overlap
 +0.0000 / +0.0000 / +0.0000  9/19 , 12/17 , 14/14  1/2 , 3/3 , 0/7              -0.0006       0.84 / 0.82 / 0.82                        0.02 ranges overlap
 +0.0000 / +0.0000 / +0.0000  7/22 , 12/18 , 19/19  0/1 , 0/0 , 1/1               0.0002       0.84 / 0.82 / 0.82                        0.02 ranges overlap
 +0.0000 / +0.0000 / +0.0000 24/19 , 18/18 , 20/18 5/12 , 2/8 , 8/7               0.0011       0.04 / 0.03 / 0.04                        0.02 ranges overlap
 +0.0000 / +0.0000 / +0.0000 21/19 , 18/20 , 21/21  4/4 , 4/4 , 3/6              -0.0001       0.04 / 0.03 / 0.04                        0.02 ranges overlap
```
Top 1,000 rules: share below their own bound on FY2024, by step:
```
                                                                                 step top1000_below_bound_before top1000_below_bound_after                   d_by_seed  d_mean                                               reading
 step 1: fiscal-year CPI keying + SUA tier on review-year amounts (09-28 minus 09-25)   0.2500 / 0.2533 / 0.2594  0.3016 / 0.3378 / 0.3156 +0.0516 / +0.0845 / +0.0562  0.0641   all three 09-28 values above all three 09-25 values
                                       step 2: October CPI values (09-29 minus 09-28)   0.3016 / 0.3378 / 0.3156  0.3044 / 0.2974 / 0.3005 +0.0028 / -0.0404 / -0.0151 -0.0176                                        ranges overlap
                                   step 3: $10 SUA-tier tolerance (final minus 09-29)   0.3044 / 0.2974 / 0.3005  0.3058 / 0.3061 / 0.2989 +0.0014 / +0.0087 / -0.0016  0.0028                                        ranges overlap
 clean: the CPI step alone (final minus CPI-off; same munging code, CPI switched off)   0.3337 / 0.3132 / 0.3095  0.3058 / 0.3061 / 0.2989 -0.0279 / -0.0071 / -0.0106 -0.0152 all three final values below all three CPI-off values
```
- Budget 5%: steps 1-3 sum to -0.0024; the 09-25-to-final difference of means is -0.0024.
- Budget 10%: steps 1-3 sum to -0.0053; the 09-25-to-final difference of means is -0.0054.
Reading with care: this section prints three separation readings per step (two budgets and the top-1,000 bound). With no real effect each shows full separation about 1 time in 10, so across all steps one or more will often do so by chance. Seed 117's column also adds up to the 09-25-to-final gap at seed 117, which was already known.

Limits: one test year (FY2024); three seeds per frame (for the 09-25 and final frames seed 117 is the draw already seen); national pool only; three against three cannot establish a small effect or rule one out; step 1 bundles two changes.
