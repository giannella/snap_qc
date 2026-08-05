"""Diagnostic for issue #1: do delivered rules key on a benefit-reconstruction
artifact near the maximum benefit?

Background measured earlier on reg_model_data.csv (FY2022-24, 118,263 rows):
`rawben_rel_max` is exactly 1.0 for 37.04% of rows and 37.37% of rows are truly
at max (`rawben == benmax`). Among truly-at-max households 96.06% land exactly
on 1 but 2.39% land in [0.987, 1). Those 2.39% are the artifact: in real state
data the value would be exactly 1, so a rule that confines `rawben_rel_max` to
just below 1 would flag them here and nothing there.

This script measures the delivered exposure to that artifact. It changes no
feature definition and no rule; it only reads `state_delivery_lists/*.csv` and
the modelling frame.

Definitions used throughout (stated once, used everywhere):

  truly at max       rawben == benmax
  exact-1            rawben_rel_max == 1
  artifact row       rawben == benmax AND rawben_rel_max < 1   (mis-recreated:
                     truly at max, but the ratio did not land on 1)
  suspect band       [0.987, 1)

Rule classification. Each rule is a conjunction of numeric comparisons, so the
clauses on any one feature imply an interval. For `rawben_rel_max` we compute
that interval [lo, hi) and label the rule:

  excludes_exact1    hi <= 1 and the bound is strict below 1, i.e. the rule
                     cannot flag a household whose ratio is exactly 1. Every
                     truly-at-max household in real state data is excluded.
  band_confined      excludes_exact1 AND lo >= 0.987: the interval sits inside
                     the suspect band, so among at-max households the rule can
                     only ever match artifact rows.
  unc_capped         `unc_rawben_rel_max` is bounded above by something < 1.
                     95.72% of truly-at-max cases have unc > 1, so this clause
                     also excludes at-max households.

Outputs (methods/at_max_benefit_diagnostic/):
  rule_classification.csv   one row per delivered rule instance (98 lists)
  affected_rule_eval.csv    one row per unique (stratum, rule) evaluated
  ben_examples.csv          the three example rules from the issue
  README.md                 the numbers, written by this script

Run: python methods/at_max_benefit_diagnostic.py
"""

import csv
import glob
import json
import os
import re
import sys

import numpy as np
import pandas as pd

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
LISTS = os.path.join(ROOT, "state_delivery_lists")
FRAME = os.path.join(ROOT, "reg_model_data.csv")
OUT = os.path.join(ROOT, "methods", "at_max_benefit_diagnostic")
os.makedirs(OUT, exist_ok=True)

BAND_LO = 0.987

FEATURES = [
    "rawben_rel_max", "total_deductions_by_hh_size", "unc_rawben_rel_max",
    "utilities", "shelter_expenses_by_hh_size", "months_since_cert_n",
    "HH_size_n", "elderly_disabled_i", "medical_deductions", "cat_elig",
    "expedited_i", "percent_abawd", "married", "count_divisible_by_100",
    "children_i", "homeless",
]
EXTRA = ["state_name", "fiscal_year", "over_threshold", "cert_HH_size_FS_n",
         "total_error_amount", "rawben", "benmax"]

CLAUSE = re.compile(r"([A-Za-z_][A-Za-z0-9_]*)\s*(<=|>=|<|>)\s*(-?[0-9.]+)")


def interval(rule, var):
    """Interval [lo, hi] implied for `var` by a conjunction, with strictness."""
    lo, hi = -np.inf, np.inf
    lo_strict = hi_strict = False
    for name, op, val in CLAUSE.findall(rule):
        if name != var:
            continue
        v = float(val)
        if op in ("<", "<="):
            if v < hi or (v == hi and op == "<"):
                hi, hi_strict = v, op == "<"
        else:
            if v > lo or (v == lo and op == ">"):
                lo, lo_strict = v, op == ">"
    return lo, hi, lo_strict, hi_strict


def classify(rule):
    lo, hi, lo_s, hi_s = interval(rule, "rawben_rel_max")
    # the rule cannot admit rawben_rel_max == 1
    excludes_exact1 = (hi < 1) or (hi == 1 and hi_s)
    band_confined = bool(excludes_exact1 and lo >= BAND_LO)
    ulo, uhi, _, uhi_s = interval(rule, "unc_rawben_rel_max")
    unc_capped = (uhi < 1) or (uhi == 1 and uhi_s)
    return dict(rel_lo=lo, rel_hi=hi, excludes_exact1=bool(excludes_exact1),
                band_confined=band_confined, unc_hi=uhi,
                unc_capped=bool(unc_capped),
                mentions_rel_max=("rel_max" in rule))


def hh_group_of(n):
    # same mapping as methods/add_refill_metrics_v2.R
    try:
        n = float(n)
    except (TypeError, ValueError):
        return None
    if np.isnan(n):
        return None
    if n <= 1:
        return "1"
    if n <= 3:
        return "2-3"
    return "4+"


def main():
    # ---------------------------------------------------------------- lists
    rows = []
    for path in sorted(glob.glob(os.path.join(LISTS, "*.csv"))):
        base = os.path.basename(path)
        m = re.match(r"blended_delivery_(.+)_2022_2024_budget(\d+)\.csv$", base)
        if not m:
            print("skipping unrecognised list name: %s" % base, file=sys.stderr)
            continue
        state = m.group(1).replace("_", " ")
        budget = int(m.group(2))
        with open(path, newline="", encoding="utf-8") as fh:
            for r in csv.DictReader(fh):
                c = classify(r["rule"])
                c.update(state=state, budget=budget, rule=r["rule"], hh=r["hh"],
                         pool=r["pool"], role=r["role"], rank=int(r["rank"]),
                         n_flagged_train=int(r["n_flagged_train"]),
                         n_flagged_state=int(r["n_flagged_state"]),
                         n_new_at_rank=int(r["n_new_at_rank"]),
                         precision_train=float(r["precision_train"]))
                rows.append(c)
    lst = pd.DataFrame(rows)
    lst.to_csv(os.path.join(OUT, "rule_classification.csv"), index=False)

    # ---------------------------------------------------------------- frame
    usecols = FEATURES + EXTRA
    df = pd.read_csv(FRAME, usecols=usecols, low_memory=False)
    # reg_model_data.csv holds every fiscal year on file; the delivered lists
    # were built on FY2022-24, so restrict to the modelling frame the rules saw.
    # 118,263 is the row count the mining runs report ("frame: 118263 rows").
    df = df[df["fiscal_year"].astype(str).isin(["2022", "2023", "2024"])].copy()
    assert len(df) == 118263, "expected the 118,263-row FY2022-24 frame, got %d" % len(df)
    for f in FEATURES:
        if df[f].dtype == object:
            df[f] = (df[f].astype(str).str.upper()
                     .map({"TRUE": 1.0, "FALSE": 0.0}).astype(float))
        else:
            df[f] = df[f].astype(float)
    df["hh"] = df["cert_HH_size_FS_n"].map(hh_group_of)
    df["is_error"] = (df["over_threshold"].fillna(0) != 0)
    df["truly_at_max"] = (df["rawben"] == df["benmax"])
    df["exact1"] = (df["rawben_rel_max"] == 1)
    df["artifact"] = df["truly_at_max"] & (df["rawben_rel_max"] < 1)

    n = len(df)
    frame_stats = dict(
        rows=n,
        exact1_share=float(df["exact1"].mean()),
        truly_at_max_share=float(df["truly_at_max"].mean()),
        artifact_share=float(df["artifact"].mean()),
        artifact_rows=int(df["artifact"].sum()),
        band_share=float(((df["rawben_rel_max"] >= BAND_LO) &
                          (df["rawben_rel_max"] < 1)).mean()),
        at_max_on_exact1=float(df.loc[df["truly_at_max"], "exact1"].mean()),
        # of truly-at-max households, the share whose ratio fell short of 1 at
        # all, and the share that landed inside the suspect band
        at_max_below1=float(df.loc[df["truly_at_max"], "artifact"].mean()),
        at_max_in_band=float(
            (df.loc[df["truly_at_max"], "rawben_rel_max"].between(
                BAND_LO, 1, inclusive="left")).mean()),
        # the clause Ben flagged: unc > 1 among truly-at-max households
        at_max_unc_above1=float(
            (df.loc[df["truly_at_max"], "unc_rawben_rel_max"] > 1).mean()),
    )
    print(json.dumps(frame_stats, indent=2))

    # ------------------------------------------------- evaluate the affected
    # one evaluation per unique (stratum, rule); the lists repeat rules across
    # states and budgets
    # two independent mechanisms exclude a real at-max household: an upper bound
    # below 1 on the ratio itself, and a cap below 1 on the uncapped ratio
    # (95.72% of truly-at-max cases have unc > 1). Evaluate the union.
    lst["exposed"] = lst["excludes_exact1"] | lst["unc_capped"]
    aff = lst[lst["exposed"]][["hh", "rule"]].drop_duplicates()
    print("unique (stratum, rule) to evaluate: %d" % len(aff))

    # rule strings are R-style conjunctions over these 16 numeric features and
    # evaluate directly in pandas; NaN comparisons yield False, which matches
    # the R flag evaluator's treatment of NA conditions as FALSE
    fdf = df[FEATURES]
    hh_mask = {h: (df["hh"] == h).values for h in ("1", "2-3", "4+")}
    art = df["artifact"].values
    atmax = df["truly_at_max"].values
    err = df["is_error"].values

    out = []
    for i, (hh, rule) in enumerate(aff.itertuples(index=False, name=None), 1):
        if i % 500 == 0:
            print("  evaluated %d / %d" % (i, len(aff)), flush=True)
        flag = np.asarray(fdf.eval(rule), dtype=bool) & hh_mask.get(
            hh, np.zeros(n, bool))
        nf = int(flag.sum())
        out.append(dict(hh=hh, rule=rule, n_flagged_frame=nf,
                        n_errors=int(err[flag].sum()),
                        n_artifact=int(art[flag].sum()),
                        n_at_max=int(atmax[flag].sum())))
    ev = pd.DataFrame(out)
    ev["share_artifact"] = np.where(ev["n_flagged_frame"] > 0,
                                    ev["n_artifact"] / ev["n_flagged_frame"],
                                    np.nan)
    ev.to_csv(os.path.join(OUT, "affected_rule_eval.csv"), index=False)

    # ------------------------------------------------- delivered footprint
    # An "artifact-dependent" rule is one where the majority of what it flags in
    # the frame is an artifact row, so most of its flags would not exist in state
    # data. This is measured, not inferred from the rule's text.
    ev["artifact_dependent"] = ev["share_artifact"] >= 0.5
    key = {(h, r): (sa, ad) for h, r, sa, ad in
           zip(ev["hh"], ev["rule"], ev["share_artifact"],
               ev["artifact_dependent"])}
    lst["share_artifact"] = [key.get((h, r), (np.nan, False))[0]
                             for h, r in zip(lst["hh"], lst["rule"])]
    lst["artifact_dependent"] = [key.get((h, r), (np.nan, False))[1]
                                 for h, r in zip(lst["hh"], lst["rule"])]
    lst.to_csv(os.path.join(OUT, "rule_classification.csv"), index=False)

    # per delivered list: what share of the cases the walk actually picks up
    # come from artifact-dependent rules. n_new_at_rank is the marginal new
    # cases that rule contributed at its rank, so these DO partition the list.
    foot = (lst.groupby(["state", "budget"])
            .apply(lambda g: pd.Series({
                "n_rules": len(g),
                "n_artifact_dependent": int(g["artifact_dependent"].sum()),
                "cases": int(g["n_new_at_rank"].sum()),
                "cases_from_artifact_dependent":
                    int(g.loc[g["artifact_dependent"], "n_new_at_rank"].sum()),
            }), include_groups=False)
            .reset_index())
    foot["share_cases"] = foot["cases_from_artifact_dependent"] / foot["cases"]
    foot.to_csv(os.path.join(OUT, "delivered_footprint.csv"), index=False)
    print("\ndelivered footprint of artifact-dependent rules, by budget:")
    print(foot.groupby("budget")[["share_cases"]]
          .describe(percentiles=[.5, .9]).round(4).to_string())

    # ------------------------------------------------------- Ben's examples
    examples = [
        "elderly_disabled_i > 0.500 & rawben_rel_max > 0.993 & "
        "total_deductions_by_hh_size > 348.000 & unc_rawben_rel_max <= 0.997",
        "rawben_rel_max >= 0.987 & rawben_rel_max < 0.997 & "
        "shelter_expenses_by_hh_size >= 850.000 & utilities < 576.000",
        "rawben_rel_max >= 0.987 & rawben_rel_max < 0.991 & "
        "total_deductions_by_hh_size >= 276.000 & utilities < 578.000",
    ]
    ex = []
    for rule in examples:
        flag_all = np.asarray(fdf.eval(rule), dtype=bool)
        hits = lst[lst["rule"].str.replace(r"\s+", " ", regex=True) ==
                   re.sub(r"\s+", " ", rule)]
        strata = sorted(hits["hh"].unique()) or ["(not in any list)"]
        for hh in strata:
            f = flag_all & hh_mask.get(hh, np.zeros(n, bool))
            sub = hits[hits["hh"] == hh]
            ex.append(dict(
                rule=rule, hh=hh, n_lists=len(sub),
                states=", ".join(sorted(sub["state"].unique()))[:200],
                n_flagged_frame=int(f.sum()), n_errors=int(err[f].sum()),
                n_artifact=int(art[f].sum()), n_at_max=int(atmax[f].sum()),
                n_flagged_train=int(sub["n_flagged_train"].iloc[0]) if len(sub) else -1,
                **classify(rule)))
    exdf = pd.DataFrame(ex)
    exdf.to_csv(os.path.join(OUT, "ben_examples.csv"), index=False)

    # ------------------------------------------------------------- summary
    summary = dict(frame=frame_stats,
                   lists=len(glob.glob(os.path.join(LISTS, "*.csv"))),
                   rule_instances=len(lst),
                   mentions_rel_max=int(lst["mentions_rel_max"].sum()),
                   excludes_exact1=int(lst["excludes_exact1"].sum()),
                   band_confined=int(lst["band_confined"].sum()),
                   unc_capped=int(lst["unc_capped"].sum()),
                   unique_affected=len(aff))
    with open(os.path.join(OUT, "summary.json"), "w", encoding="utf-8") as fh:
        json.dump(summary, fh, indent=2)
    print(json.dumps(summary, indent=2))


if __name__ == "__main__":
    main()
