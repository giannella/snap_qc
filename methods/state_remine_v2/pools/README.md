# State pools (stage 2 of the 2026-08-09 state re-mine)

One `pool_<state>_<vocab>.rds` per state: the admitted rules of the four
typed frames (mined pooled across household sizes, `hh == "all"`) plus
the any-error frame by HH strata (`hh` in 1 / 2-3 / 4+). Built by
`methods/state_remine_v2.R` with the runoff-winning vocabulary
(`runoff_winner.txt`); admission BH FDR 10% + n >= 30 per frame
(findings 19), seed 117.

**Read this before consuming:**

- `n`, `k`, `lcb` are computed on the MINED FRAME's own scale, recorded
  in `lcb_scale` (`typed:<frame>` vs `anyerror_stratum`). Sorting the
  whole pool on `lcb` across scales walks into the
  rule-pool-incomparability hazard - a blend consumer must rescale
  (e.g., re-score typed rules on the any-error universe) or keep scales
  separate.
- Cross-frame duplicate rule texts are KEPT (the four typed frames are
  mined separately; §31 lineage, no dedup). The walk is immune
  (zero-new-case rules are skipped); list consumers must dedup with
  provenance priority.
- These pools are research/production INPUTS for the v2.5.0 blend; they
  are not delivery lists and nothing here ships by itself.
