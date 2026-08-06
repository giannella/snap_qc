"""Variance join for the profile-distance diagnostic (instrument 4).

Called by methods/profile_distance_diagnostic_v2.R. For every rule in the
supplied universe (the distinct rules the FY2024 refill walk deploys from the
bench lists), this evaluates the rule's flags on the FY2022-23 NATIONAL
training caseload, restricts to error cases, joins the SNAP QC variance
records on (STATE fips, YRMONTH, HHLDNO, fiscal_year), and writes CASE-LEVEL
element/nature/timing/cause counts. The R script aggregates these into
train-side profiles (plan instrument 4) and into per-rule error-case halves
for the split-half stability certificate.

Conventions are copied from methods/rule_error_profiles.py (section 29
machinery): element groups follow the munging script's reconstruction groups,
nature groups are element-independent, FY2024 tech-doc bucketings for
AGENCY/DISCOV/TIMEPER. FY2022-23 ONLY: the profile instrument never sees the
holdout year.

RECONCILIATION (hard assertion, per the plan): each rule's recomputed
n_cases_flagged / n_error_cases / n_variances / element-group counts must
equal the committed train-era rows of
methods/rule_error_profiles/rule_profiles.csv (era == train_2022_23).
On any mismatch this exits non-zero and the R driver stops.

Usage:
    python methods/profile_distance_variance_join.py \
        --rules <rule_universe.csv> --out-cases <csv> --out-rules <csv>

rule_universe.csv columns: rule_id, hh, rule.
"""

import argparse
import os
import sys

import numpy as np
import pandas as pd
import pyreadstat

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
FRAME = os.path.join(ROOT, "reg_model_data.csv")
QCDIR = os.path.join(ROOT, "qc_data")
PROFILES = os.path.join(ROOT, "methods", "rule_error_profiles", "rule_profiles.csv")
TRAIN_YEARS = ["2022", "2023"]
VAR_SLOTS = range(1, 10)

# The 16 features that appear in a clause in any of the 98 bench lists (the
# verified vocabulary of methods/rule_error_profiles.py). reg_model_data.csv
# does not carry the three raw-income by-hh-size columns, and no bench rule
# references them, so rule evaluation needs exactly these.
FEATURES = [
    "HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size",
    "expedited_i", "cat_elig", "rawben_rel_max", "medical_deductions",
    "shelter_expenses_by_hh_size", "utilities", "married", "homeless",
    "percent_abawd", "unc_rawben_rel_max", "months_since_cert_n",
    "count_divisible_by_100",
]
KEYS = ["state_name", "yrmonth", "hhldno", "fiscal_year",
        "over_threshold", "cert_HH_size_FS_n"]

# ---- code groupings, copied verbatim from methods/rule_error_profiles.py ----
ELEMENT_GROUP = {}
for c in (311, 312, 314, 321):
    ELEMENT_GROUP[c] = "earned income"
for c in (331, 332, 333, 334, 335, 336, 342, 343, 344, 345, 346, 350):
    ELEMENT_GROUP[c] = "unearned income"
ELEMENT_GROUP[363] = "shelter deduction"
ELEMENT_GROUP[364] = "utility allowance"
ELEMENT_GROUP[365] = "medical deduction"
for c in (323, 366):
    ELEMENT_GROUP[c] = "dep care or child support deduction"
ELEMENT_GROUPS = ["earned income", "unearned income", "shelter deduction",
                  "utility allowance", "medical deduction",
                  "dep care or child support deduction", "other element"]
EG_COL = {g: "eg_" + g.replace(" ", "_") for g in ELEMENT_GROUPS}

NATURE_GROUP = {}
for c in (38, 44, 56, 57, 54):
    NATURE_GROUP[c] = "wrong amount, known item"
for c in (52, 53, 37, 58, 32, 33, 24, 30, 51):
    NATURE_GROUP[c] = "wrong include/exclude decision"
NATURE_GROUP[35] = "unreported source of income"
for c in (39, 40, 41, 64, 65):
    NATURE_GROUP[c] = "change in circumstances"
for c in (6, 7, 12, 13, 14, 15, 16, 200, 201):
    NATURE_GROUP[c] = "household composition"
for c in (36, 42, 43, 45, 46, 75, 79, 80, 98, 123):
    NATURE_GROUP[c] = "method or computation"
for c in (20, 28, 29):
    NATURE_GROUP[c] = "limits and thresholds"
for c in (111, 112, 127):
    NATURE_GROUP[c] = "child support handling"
for c in (77, 97, 120, 124, 301, 302, 303, 304, 305, 306, 307, 308, 309,
          310, 311, 312, 313, 314):
    NATURE_GROUP[c] = "reporting system or process"
NATURE_GROUP[99] = "other"
NATURE_GROUPS = ["wrong amount, known item", "wrong include/exclude decision",
                 "unreported source of income", "household composition",
                 "change in circumstances", "method or computation",
                 "reporting system or process", "limits and thresholds",
                 "child support handling", "other"]
NG_COL = {g: "ng_" + g.split(",")[0].replace(" ", "_").replace("/", "_")
          for g in NATURE_GROUPS}

AGENCY_BUCKET_2024 = {1: "client", 2: "client", 3: "client", 4: "client",
                      7: "third_party", 8: "third_party", 26: "no_fault",
                      99: "other"}
for c in (10, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25):
    AGENCY_BUCKET_2024[c] = "agency"
CAUSES = ["agency", "client", "third_party", "no_fault", "other", "unpopulated"]
TIMEPER_2024 = {1: "before_action", 2: "at_action", 3: "after_action",
                9: "undetermined"}
TIMINGS = ["before_action", "at_action", "after_action", "undetermined",
           "unpopulated"]


def hh_group_of(n):
    """Stratum from cert_HH_size_FS_n (methods/add_refill_metrics_v2.R)."""
    n = pd.to_numeric(n, errors="coerce")
    return np.where(n.isna(), None,
                    np.where(n <= 1, "1", np.where(n <= 3, "2-3", "4+")))


def load_frame():
    df = pd.read_csv(FRAME, usecols=FEATURES + KEYS, low_memory=False)
    df = df[df["fiscal_year"].astype(str).isin(TRAIN_YEARS)].copy()
    assert len(df) > 70000, "expected the FY2022-23 train frame, got %d rows" % len(df)
    for f in FEATURES:
        if df[f].dtype == object or str(df[f].dtype).startswith("str"):
            df[f] = (df[f].astype(str).str.upper()
                     .map({"TRUE": 1.0, "FALSE": 0.0}).astype(float))
        else:
            df[f] = df[f].astype(float)
    df["hh"] = hh_group_of(df["cert_HH_size_FS_n"])
    df["is_error"] = df["over_threshold"].fillna(0) != 0
    df["fiscal_year"] = df["fiscal_year"].astype(str)
    return df.reset_index(drop=True)


def load_variances():
    frames = []
    for y in TRAIN_YEARS:
        path = os.path.join(QCDIR, "qc_pub_fy%s.sav" % y)
        cols = (["STATE", "YRMONTH", "HHLDNO"] +
                ["%s%d" % (p, i) for p in
                 ("ELEMENT", "NATURE", "AGENCY", "DISCOV", "TIMEPER")
                 for i in VAR_SLOTS])
        raw, _ = pyreadstat.read_sav(path, usecols=cols)
        long = []
        for i in VAR_SLOTS:
            sub = raw[["STATE", "YRMONTH", "HHLDNO"]].copy()
            for p in ("ELEMENT", "NATURE", "AGENCY", "TIMEPER"):
                sub[p] = raw["%s%d" % (p, i)]
            long.append(sub)
        long = pd.concat(long, ignore_index=True)
        long = long[long["ELEMENT"].notna()].copy()
        long["fiscal_year"] = y
        frames.append(long)
    v = pd.concat(frames, ignore_index=True)
    v["element_group"] = v["ELEMENT"].map(ELEMENT_GROUP).fillna("other element")
    v["nature_group"] = v["NATURE"].map(NATURE_GROUP).fillna("UNMAPPED")
    v["cause"] = v["AGENCY"].map(AGENCY_BUCKET_2024).fillna("unpopulated")
    v["timeper_b"] = v["TIMEPER"].map(TIMEPER_2024).fillna("unpopulated")
    return v


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--rules", required=True)
    ap.add_argument("--out-cases", required=True)
    ap.add_argument("--out-rules", required=True)
    args = ap.parse_args()

    rules = pd.read_csv(args.rules)
    assert {"rule_id", "hh", "rule"} <= set(rules.columns)
    print("[helper] rule universe: %d rules" % len(rules), flush=True)

    df = load_frame()
    print("[helper] FY2022-23 train frame: %d rows, %d errors"
          % (len(df), int(df["is_error"].sum())), flush=True)

    v = load_variances()
    unmapped = v[v["nature_group"] == "UNMAPPED"]["NATURE"].unique()
    print("[helper] FY2022-23 variance records: %d | unmapped natures: %s"
          % (len(v), sorted(int(x) for x in unmapped) or "none"), flush=True)

    # frame-row join on (fips, yrmonth, hhldno, fiscal_year), as in
    # methods/rule_error_profiles.py
    fips = pd.read_csv(os.path.join(ROOT, "additional_data", "state_data.csv"),
                       encoding="utf-8-sig")
    name2fips = dict(zip(fips["state"], fips["fips"].astype(float)))
    dfk = df[["state_name", "yrmonth", "hhldno", "fiscal_year"]].copy()
    dfk["fips"] = dfk["state_name"].map(name2fips)
    assert dfk["fips"].notna().all(), "state_name -> fips mapping incomplete"
    dfk["frame_row"] = np.arange(len(df))
    v = v.rename(columns={"STATE": "fips", "YRMONTH": "yrmonth",
                          "HHLDNO": "hhldno"})
    key = ["fips", "yrmonth", "hhldno"]
    for c in key:
        dfk[c] = pd.to_numeric(dfk[c], errors="coerce")
        v[c] = pd.to_numeric(v[c], errors="coerce")
    v = v.merge(dfk[key + ["fiscal_year", "frame_row"]],
                on=key + ["fiscal_year"], how="inner")
    v = v.sort_values("frame_row").reset_index(drop=True)
    print("[helper] variance records matched to a train frame row: %d" % len(v),
          flush=True)
    vrow = v["frame_row"].to_numpy()
    starts = np.searchsorted(vrow, np.arange(len(df)), side="left")
    stops = np.searchsorted(vrow, np.arange(len(df)), side="right")

    hh_all = df["hh"].to_numpy()
    is_err = df["is_error"].to_numpy()

    long_parts, rule_rows = [], []
    for t in rules.itertuples(index=False):
        flag = np.asarray(df[FEATURES].eval(t.rule), dtype=bool) & (hh_all == t.hh)
        err_rows = np.flatnonzero(flag & is_err)
        parts = [np.arange(starts[r], stops[r]) for r in err_rows
                 if stops[r] > starts[r]]
        vi = np.concatenate(parts) if parts else np.empty(0, dtype=int)
        sub = v.iloc[vi][["frame_row", "element_group", "nature_group",
                          "cause", "timeper_b"]].copy()
        sub["rule_id"] = t.rule_id
        long_parts.append(sub)
        rule_rows.append(dict(rule_id=t.rule_id, hh=t.hh, rule=t.rule,
                              n_flagged_train=int(flag.sum()),
                              n_error_cases=int(err_rows.size),
                              n_error_cases_with_var=int(len(parts)),
                              n_variances=int(vi.size)))
    rl = pd.DataFrame(rule_rows)
    lg = pd.concat(long_parts, ignore_index=True)

    # case-level counts: one row per (rule_id, frame_row)
    def counts(col, values, prefix_map):
        c = (lg.groupby(["rule_id", "frame_row"])[col]
             .value_counts().unstack(fill_value=0))
        for g in values:
            if g not in c.columns:
                c[g] = 0
        c = c[values].rename(columns=prefix_map)
        return c

    eg = counts("element_group", ELEMENT_GROUPS, EG_COL)
    ng = counts("nature_group", NATURE_GROUPS, NG_COL)
    cz = counts("cause", CAUSES, {c: "cz_" + c for c in CAUSES})
    tm = counts("timeper_b", TIMINGS, {t_: "tm_" + t_ for t_ in TIMINGS})
    cases = pd.concat([eg, ng, cz, tm], axis=1).reset_index()
    cases["n_var"] = cases[[EG_COL[g] for g in ELEMENT_GROUPS]].sum(axis=1)
    keys_by_row = df[["state_name", "yrmonth", "hhldno"]].copy()
    keys_by_row["frame_row"] = np.arange(len(df))
    cases = cases.merge(keys_by_row, on="frame_row", how="left")

    # per-rule element-group totals (for the reconciliation and for R)
    tot = cases.groupby("rule_id")[[EG_COL[g] for g in ELEMENT_GROUPS]].sum()
    rl = rl.merge(tot, on="rule_id", how="left")
    for c in EG_COL.values():
        rl[c] = rl[c].fillna(0).astype(int)

    # ---- reconciliation against the committed section 29 artifact ----------
    prof = pd.read_csv(PROFILES)
    tr = prof[prof["era"] == "train_2022_23"].set_index(["hh", "rule"])
    n_ok, bad = 0, []
    for t in rl.itertuples(index=False):
        if (t.hh, t.rule) not in tr.index:
            bad.append((t.rule_id, "rule absent from rule_profiles.csv train era"))
            continue
        p = tr.loc[(t.hh, t.rule)]
        checks = [("n_cases_flagged", t.n_flagged_train),
                  ("n_error_cases", t.n_error_cases),
                  ("n_variances", t.n_variances)]
        for g in ELEMENT_GROUPS:
            checks.append(("n_elem_" + g.replace(" ", "_"),
                           getattr(t, EG_COL[g])))
        mism = [(c, int(p[c]), int(x)) for c, x in checks if int(p[c]) != int(x)]
        if mism:
            bad.append((t.rule_id, str(mism)))
        else:
            n_ok += 1
    print("[helper] RECONCILIATION vs rule_profiles.csv (train era): %d/%d rules exact"
          % (n_ok, len(rl)), flush=True)
    if bad:
        for b in bad[:10]:
            print("[helper]   MISMATCH rule_id=%s: %s" % b, file=sys.stderr)
        raise SystemExit("profile reconciliation failed for %d rules; the "
                         "variance join does not reproduce the committed "
                         "artifact, no downstream number can be trusted" % len(bad))

    cases.to_csv(args.out_cases, index=False)
    rl.to_csv(args.out_rules, index=False)
    print("[helper] wrote %s (%d case rows) and %s (%d rules)"
          % (args.out_cases, len(cases), args.out_rules, len(rl)), flush=True)


if __name__ == "__main__":
    main()
