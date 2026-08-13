"""Characterization columns for the STAGED v2.5.0 candidate lists.

Step 2 of runners/run_v250_build.R. Reuses the validated section-29 machinery
(methods/rule_error_profiles.py: code maps, load_variances, characterize,
wilson) on the NEW staged lists' rules. Differences from that pipeline, all
deliberate:
  - frame: reads methods/v250_candidate_lists/frame_for_profiles.csv, a
    full-precision (round-trip) export written by the build driver from
    reg_model_data.rds (the source of truth; the stale top-level CSV predates
    the 2026-08-08 frame rebuild and lacks the per-size income features);
  - rules: the distinct (hh, rule) pairs across the staged lists, evaluated
    with pandas.eval exactly as stage 1 does;
  - no stage-1 scorecard assertions (they pin the OLD shipped lists) and no
    field_evidence stage; era all_2022_24 only (the pooled era the shipped
    characterization sheet reports).
Output: methods/v250_candidate_lists/rule_characterization_v250.csv
(one row per (hh, rule), every share with its Wilson interval).
"""
import glob
import os
import sys

import numpy as np
import pandas as pd

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(ROOT, "methods"))
import rule_error_profiles as rep  # noqa: E402

STAGE_DIR = os.environ.get(
    "V250_STAGE_DIR", os.path.join(ROOT, "methods", "v250_candidate_lists"))
YEARS = ["2022", "2023", "2024"]
# the CURRENT mining vocabulary (do not reuse rep.FEATURES: that constant
# describes the shipped v2.4.0 lists, which still carried cat_elig; the
# 2026-08-13 vocabulary replaces it with bbce_state_i)
FEATURES19 = [
    "HH_size_n", "children_i", "elderly_disabled_i",
    "total_deductions_by_hh_size", "expedited_i", "bbce_state_i",
    "rawben_rel_max", "medical_deductions", "shelter_expenses_by_hh_size",
    "utilities", "married", "homeless", "percent_abawd",
    "unc_rawben_rel_max", "months_since_cert_n", "count_divisible_by_100",
    "gross_by_hh_size", "earned_by_hh_size", "unearned_by_hh_size",
]


def main():
    frame_path = os.path.join(STAGE_DIR, "frame_for_profiles.csv")
    df = pd.read_csv(frame_path, low_memory=False,
                     float_precision="round_trip")
    assert len(df) == 115559, "expected the 115,559-row FY2022-24 frame, got %d" % len(df)
    for f in FEATURES19:
        if df[f].dtype == object:
            df[f] = (df[f].astype(str).str.upper()
                     .map({"TRUE": 1.0, "FALSE": 0.0}).astype(float))
        else:
            df[f] = df[f].astype(float)
    df["hh"] = rep.hh_group_of(df["cert_HH_size_FS_n"])
    df["is_error"] = df["over_threshold"].fillna(0) != 0
    df["fiscal_year"] = df["fiscal_year"].astype(str)
    df = df.reset_index(drop=True)

    lists = sorted(glob.glob(os.path.join(STAGE_DIR, "blended_delivery_*.csv")))
    assert lists, "no staged lists found in %s" % STAGE_DIR
    dep = pd.concat([pd.read_csv(p, usecols=["hh", "rule"]) for p in lists],
                    ignore_index=True)
    # usecols returns FILE-ordered columns; select explicitly so itertuples
    # yields (hh, rule) and not (rule, hh)
    rules = dep[["hh", "rule"]].drop_duplicates().reset_index(drop=True)
    print("staged lists: %d | distinct (hh, rule): %d" % (len(lists), len(rules)))

    v = rep.load_variances(YEARS)
    unmapped = v[v.nature_group == "UNMAPPED"]["NATURE"].unique()
    print("variance records: %d | unmapped natures: %s"
          % (len(v), sorted(int(x) for x in unmapped) or "none"))

    fips = pd.read_csv(os.path.join(ROOT, "additional_data", "state_data.csv"),
                       encoding="utf-8-sig")
    name2fips = dict(zip(fips["state"], fips["fips"].astype(float)))
    key = ["fips", "yrmonth", "hhldno"]
    dfk = df[["state_name", "yrmonth", "hhldno"]].copy()
    dfk["fips"] = dfk["state_name"].map(name2fips)
    assert dfk["fips"].notna().all()
    dfk["frame_row"] = np.arange(len(df))
    dfk["fiscal_year"] = df["fiscal_year"].values
    v = v.rename(columns={"STATE": "fips", "YRMONTH": "yrmonth",
                          "HHLDNO": "hhldno"})
    for c in key:
        dfk[c] = pd.to_numeric(dfk[c], errors="coerce")
        v[c] = pd.to_numeric(v[c], errors="coerce")
    v = v.merge(dfk[key + ["fiscal_year", "frame_row"]],
                on=key + ["fiscal_year"], how="inner")
    v = v.sort_values("frame_row").reset_index(drop=True)
    print("variance records matched to a frame row: %d" % len(v))
    vrow = v["frame_row"].to_numpy()
    starts = np.searchsorted(vrow, np.arange(len(df)), side="left")
    stops = np.searchsorted(vrow, np.arange(len(df)), side="right")

    def gather(rows):
        parts = [np.arange(starts[r], stops[r]) for r in rows
                 if stops[r] > starts[r]]
        return np.concatenate(parts) if parts else np.empty(0, dtype=int)

    is_err = df["is_error"].to_numpy()
    status = df["error_status"].to_numpy()
    direction_code = pd.to_numeric(df["status"], errors="coerce").to_numpy()
    hh_all = df["hh"].to_numpy()

    out = []
    for i, (hh, rule) in enumerate(rules.itertuples(index=False, name=None), 1):
        if i % 100 == 0:
            print("  characterized %d / %d" % (i, len(rules)), flush=True)
        flag = np.asarray(df[FEATURES19].eval(rule), dtype=bool) & (hh_all == hh)
        rows = np.flatnonzero(flag & is_err)
        o = int((direction_code[rows] == 2).sum())
        u = int((direction_code[rows] == 3).sum())
        ot = int((status[rows] == "other_error").sum())
        rec = rep.characterize(hh, rule, "all_2022_24", rows, gather, v,
                               int(flag.sum()), o, u, ot)
        out.append(rec)
    prof = pd.DataFrame(out).drop(columns=["era"])
    out_path = os.path.join(STAGE_DIR, "rule_characterization_v250.csv")
    prof.to_csv(out_path, index=False)
    print("characterization sheet: %d rules, %d columns -> %s"
          % (len(prof), prof.shape[1], out_path))


if __name__ == "__main__":
    main()
