# Which characterization fields carry signal

Fields describing what each rule finds, so a state can judge for itself
whether a rule is worth using given what it can catch and fix. Nothing
here sorts rules into categories; that judgement belongs to the state.

Every share is reported with a Wilson interval in
`rule_characterization.csv`. A share on 20 variances and a share on 400
are different objects and the interval is what says so.

Rules characterized: 543, over 2390 deployed instances in 98 lists.

## Code coverage on error-case variances (FY2024)

- NATURE   populated and mapped: 1.000
- AGENCY   populated and mapped: 0.869
- TIMEPER  populated and mapped: 0.690
- DISCOV   populated and mapped: 0.690

National mix on FY2024 error-case variances, so a rule's share reads as lift:

- earned income                        0.241
- unearned income                      0.222
- shelter deduction                    0.327
- utility allowance                    0.075
- medical deduction                    0.048
- dep care or child support deduction  0.026
- other element                        0.061

- wrong amount, known item             0.237
- wrong include/exclude decision       0.173
- unreported source of income          0.061
- household composition                0.025
- change in circumstances              0.030
- method or computation                0.032
- reporting system or process          0.407
- other                                0.030

- of variances reporting a timing: 0.555 at the agency's action, 0.076 before, 0.360 after
- cause: agency 0.359, client 0.246, third_party 0.004, no_fault 0.243, other 0.017

## Per field: does it carry rule-level signal, and does it travel?

Reliability is the share of the spread across rules that is real
between-rule difference rather than sampling error. The split-half columns
compare the same rule computed on two random halves of the 49 states
against the difference sampling alone would produce; a ratio near 1 means
the field is as stable as its support allows.

| field | median share | 10th-90th | reliability | split-half obs | floor | ratio |
|---|---|---|---|---|---|---|
| earned income | 0.330 | 0.034-0.425 | 0.94 | 0.034 | 0.032 | 1.06 |
| unearned income | 0.188 | 0.088-0.379 | 0.94 | 0.030 | 0.031 | 0.96 |
| shelter deduction | 0.182 | 0.128-0.332 | 0.92 | 0.030 | 0.038 | 0.79 |
| utility allowance | 0.050 | 0.017-0.131 | 0.90 | 0.018 | 0.019 | 0.92 |
| medical deduction | 0.013 | 0.000-0.304 | 0.98 | 0.008 | 0.012 | 0.73 |
| dep care or child support deduction | 0.043 | 0.000-0.140 | 0.92 | 0.013 | 0.014 | 0.97 |
| wrong amount, known item | 0.298 | 0.164-0.352 | 0.75 | 0.041 | 0.039 | 1.04 |
| wrong include/exclude decision | 0.287 | 0.207-0.584 | 0.93 | 0.039 | 0.046 | 0.86 |
| unreported source of income | 0.104 | 0.039-0.153 | 0.70 | 0.026 | 0.025 | 1.05 |
| household composition | 0.059 | 0.000-0.102 | 0.79 | 0.012 | 0.016 | 0.79 |
| change in circumstances | 0.078 | 0.026-0.120 | 0.63 | 0.025 | 0.023 | 1.11 |
| arose at the agency's action | 0.604 | 0.418-0.680 | 0.85 | 0.057 | 0.046 | 1.23 |
| arose after the agency's action | 0.298 | 0.208-0.480 | 0.88 | 0.058 | 0.044 | 1.33 |
| coded agency-caused | 0.566 | 0.471-0.676 | 0.72 | 0.062 | 0.047 | 1.32 |
| coded client-caused | 0.395 | 0.277-0.493 | 0.75 | 0.061 | 0.047 | 1.30 |
| surfaced from the case record | 0.409 | 0.292-0.510 | 0.75 | 0.061 | 0.045 | 1.35 |
| overissuance (case level) | 0.437 | 0.141-0.898 | 0.96 | n/a | n/a | n/a |

## Era drift, FY2022-23 against FY2024

| field | median abs difference | sampling floor | ratio |
|---|---|---|---|
| earned income | 0.029 | 0.033 | 0.90 |
| unearned income | 0.036 | 0.031 | 1.16 |
| shelter deduction | 0.028 | 0.037 | 0.77 |
| utility allowance | 0.015 | 0.019 | 0.80 |
| medical deduction | 0.009 | 0.011 | 0.79 |
| dep care or child support deduction | 0.012 | 0.014 | 0.89 |
| wrong amount, known item | 0.138 | 0.038 | 3.60 |
| wrong include/exclude decision | 0.070 | 0.042 | 1.68 |
| unreported source of income | 0.023 | 0.024 | 0.98 |
| household composition | 0.011 | 0.015 | 0.75 |
| change in circumstances | 0.021 | 0.023 | 0.89 |
| arose at the agency's action | 0.051 | 0.044 | 1.15 |
| arose after the agency's action | 0.053 | 0.041 | 1.28 |
| coded agency-caused | 0.041 | 0.045 | 0.91 |
| coded client-caused | 0.045 | 0.045 | 1.00 |
| surfaced from the case record | 0.047 | 0.043 | 1.10 |
| overissuance (case level) | 0.035 | 0.039 | 0.90 |

## Does knowing the rule tell you anything about the error?

- element group: MI 0.1360 nats, NMI 0.0435; permutation null 0.0064 +/- 0.0001, so observed is 985.3 sd above chance
- nature group: MI 0.0510 nats, NMI 0.0159; permutation null 0.0096 +/- 0.0002, so observed is 233.1 sd above chance

(A case can trip several rules, so the units here are (rule, variance)
pairs rather than a partition. The permutation null has the same
structure, so the comparison holds.)

## Support

- median variances per rule, FY2022-24 pooled: 203
- rules with at least 20 variances: 543 of 543
- median error cases per rule: 126
- direction, case level: 73093 overissuance, 29104 underissuance, 67009 other_error
