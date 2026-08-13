# Raw-benefit reconstruction: debugging diagnostics (2026-08-12)

Read-only diagnostics behind the reconstruction clues sent to Ben.
"Clean" throughout means cases QC marked error-free, where the error label
cannot explain a recreated-vs-recorded discrepancy. All scripts read
`reg_model_data.rds` from the repo root and print to console; nothing is
written or mined.

Run from the repo root:
`"C:\Program Files\R\R-4.5.1\bin\Rscript.exe" methods/recon_debug/<script>`

| script | what it measures |
|---|---|
| `recon_agreement_audit.R` | the yardstick: recreated (`raw_benefit_amount`) vs recorded (`rawben`) agency benefit; agreement overall / by error status; clean-case mismatch rate by state; the recorded-at-max slice whose reconstruction lands below max |
| `recon_gap_signatures.R` | signed-gap distribution on clean mismatches; quantized gap values (+3, +2, -2, -9, -15, -39...); component fingerprints; per-state structure; Illinois by year |
| `recon_failure_families.R` | the two-family split: (1) cases where the file itself records a sub-threshold raw-vs-corrected benefit difference our un-correction failed to reverse (87.6% of clean mismatches; quantized gaps at 0.3 x round-dollar amounts; the -39 group is 88% earned-income cases); (2) Illinois's +$2/+3 drift on cases with NO file difference (95% of its +2/+3 rows) - a reversal applied where it should not be |

Headline numbers (FY2022-24, 115,559 rows): 92.5% of cases reconstruct
within $1 (95.6% of clean cases); 4,470 clean mismatches; Illinois clean
failure rate 27% FY22-23 falling to 18% FY24 vs a ~2.3% floor elsewhere;
3,935 recorded-at-max cases reconstruct below max (median ratio 0.887,
half are error cases) - the population that made the near-max features
minable-but-misleading (findings §28, §35).

Caveats: the field named `RAWNET` is CORRECTED net income despite its name
(Eric, 2026-08-12) - do not debug against it as agency-side truth. The
munging AMTERR filter already dropped rows whose benefit difference does
not reconcile with the reported error amount. Success metric for a fix:
the clean-case within-$1 agreement rate, overall and per state.
