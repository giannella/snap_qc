"""Add four descriptive columns to every delivery list.

STATUS: written 2026-08-04 and deliberately NOT run. Staged for the next
delivery-list build rather than applied to the shipped lists mid-cycle. Running it
rewrites all 98 files in state_delivery_lists/ and the 98 untracked bench lists, and
adding columns to the shipped lists is a MINOR version bump per VERSIONING.md.
See the open roadmap in RESUME.md.

The columns describe what each rule tends to surface, so a state can judge whether
a rule is worth using given what it can catch and fix. They characterize, they do
not recommend: nothing here marks a rule as suitable or unsuitable.

  element_groups_to_75      the element groups accounting for 75% of the rule's
                            error variances, in order, with shares
  nature_groups_to_75       the same for nature groups, which is closer to HOW the
                            error happened than to what it was about
  agency_caused_percent     of the rule's variances with a documented cause, the
                            percent coded agency-caused rather than client, third
                            party or no-fault
  at_certification_percent  of the rule's variances that report a timing, the
                            percent that arose at the agency's most recent action,
                            which for most cases is the certification or
                            recertification action

All four are computed on the NATIONAL FY2022-24 pool, not per state. Per-state
characterization does not work: the median deployed (rule, state) pair flags 3
cases. See methods/modeling_findings.md section 29.

Two cautions the section records and a reader of these files should carry:

  - A rule's mix converges on the national mix as it gets broader. Rules under 50
    error cases sit a median 0.264 from the national element mix, rules over 1,000
    only 0.057. The high-volume core rules will look alike here; the distinctive
    profiles belong to narrow rules, which are also the least precisely estimated.
  - n_error_cases_national is written alongside, so the shares can be read with
    their support. Rules with very few national error cases carry wide intervals;
    the full intervals are in methods/rule_error_profiles/rule_characterization.csv.

Run: python runners/run_annotate_delivery_lists.py
"""

import csv
import glob
import os
import sys

import numpy as np
import pandas as pd

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import rule_error_profiles as R  # noqa: E402

ROOT = R.ROOT
TARGETS = [os.path.join(ROOT, "state_delivery_lists", "*.csv"),
           os.path.join(ROOT, "methods", "anyerror_blended_holdout_2024",
                        "bench_list_*.csv")]
NEW_COLS = ["element_groups_to_75", "nature_groups_to_75",
            "agency_caused_percent", "at_certification_percent",
            "n_error_cases_national"]


def all_rules():
    seen = []
    for pat in TARGETS:
        for f in sorted(glob.glob(pat)):
            with open(f, newline="", encoding="utf-8") as fh:
                for r in csv.DictReader(fh):
                    seen.append((r["hh"], r["rule"]))
    return pd.DataFrame(seen, columns=["hh", "rule"]).drop_duplicates()


def main():
    df = R.load_frame()
    v = R.load_variances(R.TRAIN_YEARS + [R.TEST_YEAR])

    fips = pd.read_csv(os.path.join(ROOT, "additional_data", "state_data.csv"),
                       encoding="utf-8-sig")
    n2f = dict(zip(fips["state"], fips["fips"].astype(float)))
    dfk = df[["state_name", "yrmonth", "hhldno"]].copy()
    dfk["fips"] = dfk["state_name"].map(n2f)
    assert dfk["fips"].notna().all()
    dfk["frame_row"] = np.arange(len(df))
    dfk["fiscal_year"] = df["fiscal_year"].values
    v = v.rename(columns={"STATE": "fips", "YRMONTH": "yrmonth", "HHLDNO": "hhldno"})
    for c in ("fips", "yrmonth", "hhldno"):
        dfk[c] = pd.to_numeric(dfk[c], errors="coerce")
        v[c] = pd.to_numeric(v[c], errors="coerce")
    v = v.merge(dfk[["fips", "yrmonth", "hhldno", "fiscal_year", "frame_row"]],
                on=["fips", "yrmonth", "hhldno", "fiscal_year"], how="inner")
    v = v.sort_values("frame_row").reset_index(drop=True)
    vrow = v["frame_row"].to_numpy()
    starts = np.searchsorted(vrow, np.arange(len(df)), side="left")
    stops = np.searchsorted(vrow, np.arange(len(df)), side="right")

    def gather(rows):
        parts = [np.arange(starts[r], stops[r]) for r in rows if stops[r] > starts[r]]
        return np.concatenate(parts) if parts else np.empty(0, dtype=int)

    rules = all_rules()
    print("distinct (stratum, rule) across all lists: %d" % len(rules))
    is_err = df["is_error"].to_numpy()
    hh_all = df["hh"].to_numpy()
    eg = v["element_group"].to_numpy()
    ng = v["nature_group"].to_numpy()
    cause = v["cause"].to_numpy()
    tp = v["timeper_b"].to_numpy()

    ann = {}
    for i, (hh, rule) in enumerate(rules.itertuples(index=False, name=None), 1):
        if i % 500 == 0:
            print("  %d / %d" % (i, len(rules)), flush=True)
        flag = np.asarray(df[R.FEATURES].eval(rule), dtype=bool) & (hh_all == hh)
        rows = np.flatnonzero(flag & is_err)
        vi = gather(rows)
        sub_eg, sub_ng = eg[vi], ng[vi]
        sub_c, sub_t = cause[vi], tp[vi]
        doc = sub_c[~np.isin(sub_c, ("UNDOCUMENTED", "unpopulated", "no_fault"))]
        kt = sub_t[sub_t != "unpopulated"]
        ann[(hh, rule)] = dict(
            element_groups_to_75=R._to_75(pd.Series(sub_eg))[0],
            nature_groups_to_75=R._to_75(pd.Series(sub_ng))[0],
            agency_caused_percent=(round(100 * (doc == "agency").mean(), 1)
                                   if doc.size else ""),
            at_certification_percent=(round(100 * (kt == "at_action").mean(), 1)
                                      if kt.size else ""),
            n_error_cases_national=int(len(rows)))

    written = 0
    for pat in TARGETS:
        for f in sorted(glob.glob(pat)):
            with open(f, newline="", encoding="utf-8") as fh:
                rd = csv.DictReader(fh)
                base = [c for c in rd.fieldnames if c not in NEW_COLS]
                rows = list(rd)
            out = base + NEW_COLS
            with open(f, "w", newline="", encoding="utf-8") as fh:
                w = csv.DictWriter(fh, fieldnames=out, quoting=csv.QUOTE_ALL)
                w.writeheader()
                for r in rows:
                    a = ann.get((r["hh"], r["rule"]))
                    rec = {k: r.get(k, "") for k in base}
                    rec.update({k: (a[k] if a else "") for k in NEW_COLS})
                    w.writerow(rec)
            written += 1
    print("annotated %d files" % written)
    miss = sum(1 for k, a in ann.items() if a["n_error_cases_national"] == 0)
    print("rules with no national error cases (blank shares): %d of %d"
          % (miss, len(ann)))


if __name__ == "__main__":
    main()
