# f1016 frame vs v2.7 final frame: FY2022-23 mined, FY2024 scored

National pool only, 49 national-only state lists, same seeds paired. Generated 2026-10-17 23:01 PDT.
Seeds with both arms: 117, 118, 119.

Caveat: this is the one-year-ahead window, the secondary one. The primary window is three years ahead (FY2017-19 -> FY2022; process rule of 2026-10-06), and a one-year-ahead dip alone does not re-open a correctness fix.

## 0. Reproduction check
Final frame, seed-117 pool re-scored today vs the 2026-10-02 record: 98 of 98 state-budget cells identical (REPRODUCED).

## 1. Pooled over 49 states (any-error precision; the target is any error, so frame-relative = any-error)

| budget | seed | final errors / flagged | final precision | new errors / flagged | new precision | change in errors | mean dollar recall, final / new |
|---|---|---|---|---|---|---|---|
| 5% | 117 | 661 / 1949 | 0.3391 | 651 / 1949 | 0.3340 | -10 | 0.1570 / 0.1571 |
| 5% | 118 | 665 / 1949 | 0.3412 | 641 / 1949 | 0.3289 | -24 | 0.1648 / 0.1542 |
| 5% | 119 | 661 / 1949 | 0.3391 | 646 / 1949 | 0.3315 | -15 | 0.1654 / 0.1593 |
| 10% | 117 | 1145 / 3930 | 0.2913 | 1145 / 3930 | 0.2913 | +0 | 0.2745 / 0.2716 |
| 10% | 118 | 1166 / 3930 | 0.2967 | 1154 / 3930 | 0.2936 | -12 | 0.2868 / 0.2682 |
| 10% | 119 | 1173 / 3930 | 0.2985 | 1162 / 3930 | 0.2957 | -11 | 0.2901 / 0.2829 |

- 5%: final-frame seed range 0.3391-0.3412 (errors 661-665); new frame 0.3289-0.3340 (errors 641-651). New frame above every final seed: FALSE; below every final seed: TRUE.
- 10%: final-frame seed range 0.2913-0.2985 (errors 1145-1173); new frame 0.2913-0.2957 (errors 1145-1162). New frame above every final seed: FALSE; below every final seed: FALSE.

## 2. Paired by state (new minus final, same seed)

| budget | seed | median change | mean change | states up / down | harmed (< -0.05) / helped (> +0.05) | dollar recall: median / mean change, harmed (< -0.05) | states below base rate, final / new |
|---|---|---|---|---|---|---|---|
| 5% | 117 | +0.0000 | -0.0060 | 12 / 20 | 6 / 3 | -0.0019 / +0.0002, 0 | 1 / 1 |
| 5% | 118 | +0.0000 | -0.0116 | 11 / 20 | 10 / 5 | -0.0010 / -0.0105, 3 | 0 / 0 |
| 5% | 119 | +0.0000 | -0.0079 | 16 / 18 | 9 / 4 | -0.0070 / -0.0061, 2 | 0 / 0 |
| 10% | 117 | +0.0000 | +0.0000 | 20 / 17 | 1 / 1 | +0.0071 / -0.0029, 5 | 0 / 0 |
| 10% | 118 | +0.0000 | -0.0042 | 16 / 23 | 3 / 3 | -0.0087 / -0.0186, 8 | 0 / 0 |
| 10% | 119 | +0.0000 | -0.0040 | 23 / 20 | 3 / 1 | +0.0031 / -0.0072, 7 | 0 / 0 |

Per-state change averaged over the 3 seeds:

| budget | median | mean | harmed (< -0.05) / helped (> +0.05) | states down at every seed / up at every seed |
|---|---|---|---|---|
| 5% | -0.0074 | -0.0085 | 4 / 0 | 5 / 0 |
| 10% | -0.0041 | -0.0027 | 1 / 1 | 3 / 3 |

## 3. For scale: the same paired statistics between two final-frame seeds (no frame change)

| budget | seeds | median change | mean change | harmed / helped |
|---|---|---|---|---|
| 5% | 117 -> 118 | +0.0000 | -0.0000 | 8 / 9 |
| 5% | 117 -> 119 | +0.0000 | -0.0009 | 10 / 10 |
| 5% | 118 -> 119 | +0.0000 | -0.0009 | 9 / 10 |
| 10% | 117 -> 118 | +0.0102 | +0.0069 | 3 / 5 |
| 10% | 117 -> 119 | +0.0115 | +0.0096 | 6 / 9 |
| 10% | 118 -> 119 | +0.0103 | +0.0027 | 2 / 4 |

## 4. Rule level: top of the national ranking, all FY2024 rows

| frame | seed | admitted visible rules | top 200 precision | top 1,000 | top 3,000 | top 1,000 below their 99% bound |
|---|---|---|---|---|---|---|
| final | 117 | 52977 | 0.4005 | 0.3530 | 0.3057 | 0.311 |
| f1016 | 117 | 52855 | 0.3968 | 0.3540 | 0.3062 | 0.315 |
| final | 118 | 53397 | 0.3978 | 0.3629 | 0.3087 | 0.312 |
| f1016 | 118 | 53435 | 0.4041 | 0.3660 | 0.3102 | 0.312 |
| final | 119 | 53039 | 0.3998 | 0.3566 | 0.3063 | 0.302 |
| f1016 | 119 | 53263 | 0.4071 | 0.3593 | 0.3080 | 0.278 |
