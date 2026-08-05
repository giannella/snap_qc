"""Per-rule error profiles for the delivered lists, nationally across 49 states.

Issue #2 (mode ELEMENT per rule) plus Ben's over/under follow-up plus the
review-mode extension. For every rule a delivered list actually deploys, this
characterises the errors it catches on three axes:

  1. what the error was about        ELEMENT (mode + concentration), NATURE
  2. who caused it, how costly       AGENCY bucketed; DISCOV + VERIF bucketed to
     it is to resolve                "closable from the case record" vs "needed
                                      outside contact"
  3. can a pre-authorization         TIMEPER: 1 (before the most recent action)
     review catch it at all          and 2 (at the action) are catchable at the
                                      agency's action; 3 (after) is structurally
                                      post-authorization work

  plus E_FINDG: 2 overissuance, 3 underissuance, 4 ineligible.

These combine into a per-rule review-mode indicator (pre-authorization desk work
versus post-authorization fieldwork) rather than being left as raw columns.

STAGES
  1  flagged sets: the refill walk from methods/add_refill_metrics_v2.R, per
     state and budget, asserted against holdout_metrics.json so drift fails loudly
  2  profiles: join to the QC variance records, explode to one row per variance,
     tabulate per rule on two eras (FY2022-23 and FY2024)
  3  promotion criteria: concentration, era-stability, discrimination

Run stage 1 alone to validate:  python methods/rule_error_profiles.py --stage 1 --states Michigan
Full national build:            python runners/run_rule_error_profiles.py

LEVELS. AGENCY/DISCOV/VERIF/TIMEPER/ELEMENT are recorded PER VARIANCE and a case
carries up to nine, so every tabulation below is variance-level unless the column
name says cases. A case also trips several rules, so per-rule counts sum to more
than the flagged total; they are not a partition.
"""

import argparse
import json
import os
import re
import sys
from collections import Counter

import numpy as np
import pandas as pd
import pyreadstat

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
BENCH = os.path.join(ROOT, "methods", "anyerror_blended_holdout_2024")
FRAME = os.path.join(ROOT, "reg_model_data.csv")
QCDIR = os.path.join(ROOT, "qc_data")
OUT = os.path.join(ROOT, "methods", "rule_error_profiles")

BUDGETS = [5, 10]
TEST_YEAR = "2024"
TRAIN_YEARS = ["2022", "2023"]
VAR_SLOTS = range(1, 10)

# The feature vocabulary the delivered rules are actually written over. The
# mining frame carries more features than this; these 16 are the ones that appear
# in a clause in any of the 98 bench lists, verified by parsing them.
FEATURES = [
    "HH_size_n", "children_i", "elderly_disabled_i", "total_deductions_by_hh_size",
    "expedited_i", "cat_elig", "rawben_rel_max", "medical_deductions",
    "shelter_expenses_by_hh_size", "utilities", "married", "homeless",
    "percent_abawd", "unc_rawben_rel_max", "months_since_cert_n",
    "count_divisible_by_100",
]
KEYS = ["state", "state_name", "yrmonth", "hhldno", "fiscal_year",
        "over_threshold", "total_error_amount", "cert_HH_size_FS_n"]

HH_LEVELS = ("1", "2-3", "4+")


def hh_group_of(n):
    """Same mapping as methods/add_refill_metrics_v2.R. HH_size_n is the wrong
    column to use here; the stratum comes from cert_HH_size_FS_n."""
    n = pd.to_numeric(n, errors="coerce")
    return np.where(n.isna(), None, np.where(n <= 1, "1", np.where(n <= 3, "2-3", "4+")))


# ---------------------------------------------------------------- code buckets
# All definitions are read from the FY2023 SNAP QC Technical Documentation
# (Mathematica), Chapter V detailed codebook, not inferred. The .sav files carry
# no value labels for these variables.
#
# AGENCY, the primary cause of the variance:
#   1 information not reported                   2 incomplete/incorrect information
#   3 information withheld (IPV referral)           provided, agency not required
#   4 incorrect information provided (IPV)          to verify
#   7 inaccurate information from collateral contact
#   8 acted on incorrect Federal computer match not requiring verification
#   10 policy incorrectly applied                12 reported information disregarded
#   14 failed to follow up on inconsistent/incomplete information
#   15 failed to follow up on impending changes  16 failed to verify required info
#   17 computer programming error                18 data entry or coding error
#   19 mass change                               20 arithmetic computation error
#   21 computer user error                       99 other
# Codes 22-26 appear in the FY2023 and FY2024 data but are defined in no technical
# documentation through FY2023, so they are bucketed as UNDOCUMENTED rather than
# guessed at. Code 26 alone is the single most common value nationally, which is a
# stated limit on every cause figure here.
AGENCY_BUCKET = {1: "client", 2: "client", 3: "client", 4: "client",
                 7: "third_party", 8: "third_party",
                 10: "agency", 12: "agency", 14: "agency", 15: "agency",
                 16: "agency", 17: "agency", 18: "agency", 19: "agency",
                 20: "agency", 21: "agency",
                 22: "UNDOCUMENTED", 23: "UNDOCUMENTED", 24: "UNDOCUMENTED",
                 25: "UNDOCUMENTED", 26: "UNDOCUMENTED", 99: "other"}

# DISCOV, how the variance was discovered. 1 and 2 are the case record (2 is
# documentation from an automated match, still in the record). 3 is a recipient
# interview, which is contacting someone, so it groups with 4-7.
DISCOV_BUCKET = {1: "case_record", 2: "case_record",
                 3: "outside_contact", 4: "outside_contact", 5: "outside_contact",
                 6: "outside_contact", 7: "outside_contact",
                 8: "external_match", 9: "other"}
# VERIF, how it was verified. Same logic: 1 and 2 from the case record, 3 is
# information provided by the recipient, 8 is an automated government match.
VERIF_BUCKET = {1: "case_record", 2: "case_record",
                3: "outside_contact", 4: "outside_contact", 5: "outside_contact",
                6: "outside_contact", 7: "outside_contact",
                8: "external_match", 9: "other"}
# TIMEPER, when the variance occurred relative to the agency's most recent action.
TIMEPER_BUCKET = {1: "before_action", 2: "at_action", 3: "after_action",
                  9: "undetermined"}
FINDG_BUCKET = {2: "overissuance", 3: "underissuance", 4: "ineligible"}


def bucket(series, mapping, unknown="unknown"):
    return series.map(mapping).fillna(unknown)


# ------------------------------------------------------------------ stage 1
def refill(idx_list, n_rows, budget_frac):
    """Core then buffer in rank order: take a rule when it adds unflagged cases
    and the running total still fits floor(budget * n_rows). Mirrors the walk in
    methods/add_refill_metrics_v2.R exactly."""
    cap = int(np.floor(budget_frac * n_rows))
    un = np.zeros(n_rows, dtype=bool)
    taken = []
    n_un = 0
    for i, ix in enumerate(idx_list):
        if ix.size == 0:
            continue
        add = int((~un[ix]).sum())
        if add > 0 and n_un + add <= cap:
            un[ix] = True
            n_un += add
            taken.append(i)
    return un, taken


def load_frame():
    df = pd.read_csv(FRAME, usecols=FEATURES + KEYS, low_memory=False)
    df = df[df["fiscal_year"].astype(str).isin(TRAIN_YEARS + [TEST_YEAR])].copy()
    assert len(df) == 118263, "expected the 118,263-row FY2022-24 frame, got %d" % len(df)
    for f in FEATURES:
        if df[f].dtype == object:
            df[f] = (df[f].astype(str).str.upper()
                     .map({"TRUE": 1.0, "FALSE": 0.0}).astype(float))
        else:
            df[f] = df[f].astype(float)
    df["hh"] = hh_group_of(df["cert_HH_size_FS_n"])
    df["is_error"] = df["over_threshold"].fillna(0) != 0
    df["fiscal_year"] = df["fiscal_year"].astype(str)
    return df.reset_index(drop=True)


def stage1(df, states, expect):
    """Return (deployed, flagged): deployed rules per state/budget, and the
    flagged FY2024 cases behind them."""
    dep_rows, flag_rows, checks = [], [], []
    for state in states:
        ho = df.index[(df["state_name"] == state) &
                      (df["fiscal_year"] == TEST_YEAR)].to_numpy()
        if ho.size == 0:
            print("  no FY2024 rows for %s, skipping" % state, file=sys.stderr)
            continue
        # evaluate on the state's own holdout rows, not the whole frame
        sdf = df.loc[ho, FEATURES].reset_index(drop=True)
        hh_ho = df["hh"].to_numpy()[ho]
        for b in BUDGETS:
            path = os.path.join(BENCH, "bench_list_%s_budget%02d.csv"
                                % (state.replace(" ", "_"), b))
            lst = pd.read_csv(path).sort_values("rank").reset_index(drop=True)
            idx_list = []
            for rule, hh in zip(lst["rule"], lst["hh"]):
                m = np.asarray(sdf.eval(rule), dtype=bool) & (hh_ho == hh)
                idx_list.append(np.flatnonzero(m))
            un, taken = refill(idx_list, ho.size, b / 100.0)
            hit = ho[un]
            err = df["is_error"].to_numpy()[hit]
            prec = err.sum() / max(len(hit), 1)

            e = expect.get((state, b))
            checks.append(dict(
                state=state, budget=b,
                n_rules=len(taken), n_rules_exp=e["n_rules_deployed_refill"],
                n_cases=len(hit), n_cases_exp=e["n_cases_flagged_refill"],
                n_errors=int(err.sum()), n_errors_exp=e["n_errors_flagged_refill"],
                precision=prec, precision_exp=e["precision_refill"],
                n_available=len(lst), n_available_exp=e["n_rules_available_refill"]))

            for i in taken:
                m = idx_list[i]
                dep_rows.append(dict(state=state, budget=b, rank=int(lst["rank"][i]),
                                     role=lst["role"][i], hh=lst["hh"][i],
                                     rule=lst["rule"][i], n_flagged_state_2024=int(m.size)))
                for local in m:
                    flag_rows.append((state, b, lst["hh"][i], lst["rule"][i], ho[local]))
    dep = pd.DataFrame(dep_rows)
    flg = pd.DataFrame(flag_rows,
                       columns=["state", "budget", "hh", "rule", "frame_row"])
    chk = pd.DataFrame(checks)
    return dep, flg, chk


def report_checks(chk):
    bad = chk[(chk.n_rules != chk.n_rules_exp) | (chk.n_cases != chk.n_cases_exp) |
              (chk.n_errors != chk.n_errors_exp) |
              (chk.n_available != chk.n_available_exp) |
              ((chk.precision - chk.precision_exp).abs() > 1e-6)]
    print("stage 1: %d of %d state-budget combinations reproduce holdout_metrics.json"
          % (len(chk) - len(bad), len(chk)))
    if len(bad):
        print(bad.to_string(index=False))
        raise SystemExit("stage 1 does not reproduce the shipped scorecard")


# ------------------------------------------------------------------ stage 2
def load_variances(years):
    """One row per (case, variance slot) with the coded fields, for the given
    fiscal years. Keyed by (STATE fips, YRMONTH, HHLDNO)."""
    frames = []
    for y in years:
        path = os.path.join(QCDIR, "qc_pub_fy%s.sav" % y)
        cols = (["STATE", "YRMONTH", "HHLDNO", "AMTERR", "FYWGT"] +
                ["%s%d" % (p, i) for p in
                 ("ELEMENT", "NATURE", "AGENCY", "DISCOV", "VERIF", "TIMEPER",
                  "E_FINDG", "AMOUNT") for i in VAR_SLOTS])
        raw, _ = pyreadstat.read_sav(path, usecols=cols)
        long = []
        for i in VAR_SLOTS:
            sub = raw[["STATE", "YRMONTH", "HHLDNO", "AMTERR"]].copy()
            for p in ("ELEMENT", "NATURE", "AGENCY", "DISCOV", "VERIF",
                      "TIMEPER", "E_FINDG", "AMOUNT"):
                sub[p] = raw["%s%d" % (p, i)]
            sub["slot"] = i
            long.append(sub)
        long = pd.concat(long, ignore_index=True)
        # a slot is populated when it carries an element or a finding
        long = long[long["ELEMENT"].notna() | long["E_FINDG"].notna()].copy()
        long["fiscal_year"] = y
        frames.append(long)
    v = pd.concat(frames, ignore_index=True)
    v["agency_b"] = bucket(v["AGENCY"], AGENCY_BUCKET)
    v["discov_b"] = bucket(v["DISCOV"], DISCOV_BUCKET)
    v["verif_b"] = bucket(v["VERIF"], VERIF_BUCKET)
    v["timeper_b"] = bucket(v["TIMEPER"], TIMEPER_BUCKET)
    v["findg_b"] = bucket(v["E_FINDG"], FINDG_BUCKET)
    v["desk_closable"] = (v["discov_b"] == "case_record") & (v["verif_b"] == "case_record")
    v["catchable_at_action"] = v["timeper_b"].isin(["before_action", "at_action"])
    return v


def _mode(series):
    """Modal value and its share. Returns (value, share, n)."""
    s = series.dropna()
    if not len(s):
        return (None, np.nan, 0)
    c = Counter(s)
    val, k = c.most_common(1)[0]
    return (val, k / len(s), len(s))


def profile(hh, rule, era, basis, error_rows, gather, v, n_cases_flagged):
    """One profile row: what the errors this rule catches are about, who caused
    them, whether they close at the desk, and whether a pre-authorization review
    could reach them. All shares are variance-level except n_error_cases."""
    vi = gather(error_rows)
    sub = v.iloc[vi]
    el, el_share, n_var = _mode(sub["ELEMENT"])
    nat, nat_share, _ = _mode(sub["NATURE"])
    n = max(n_var, 1)
    row = dict(hh=hh, rule=rule, era=era, basis=basis,
               n_cases_flagged=n_cases_flagged,
               n_error_cases=int(len(error_rows)), n_variances=n_var,
               mode_element=el, mode_element_share=el_share,
               mode_nature=nat, mode_nature_share=nat_share)
    for col, vals in (("agency_b", ("agency", "client", "third_party",
                                    "other", "UNDOCUMENTED", "unknown")),
                      ("timeper_b", ("before_action", "at_action", "after_action",
                                     "undetermined")),
                      ("findg_b", ("overissuance", "underissuance", "ineligible"))):
        cnt = sub[col].value_counts()
        for val in vals:
            row["%s_%s" % (col[:-2], val)] = int(cnt.get(val, 0))
    row["n_desk_closable"] = int(sub["desk_closable"].sum()) if n_var else 0
    row["n_catchable_at_action"] = int(sub["catchable_at_action"].sum()) if n_var else 0
    row["share_agency"] = row["agency_agency"] / n
    row["share_desk_closable"] = row["n_desk_closable"] / n
    row["share_catchable_at_action"] = row["n_catchable_at_action"] / n
    row["amterr_total"] = float(sub["AMTERR"].fillna(0).sum())
    row["amount_total"] = float(sub["AMOUNT"].fillna(0).sum())
    # Review mode. Pre-authorization work needs all three: the error existed at
    # or before the agency's action, the evidence is in the case record, and the
    # agency caused it. Anything else is post-authorization fieldwork.
    pre = (sub["catchable_at_action"] & sub["desk_closable"] &
           (sub["agency_b"] == "agency")).sum() if n_var else 0
    row["n_pre_auth"] = int(pre)
    row["share_pre_auth"] = pre / n
    row["review_mode"] = ("insufficient" if n_var < 5 else
                          "pre_authorization" if pre / n >= 0.5 else
                          "post_authorization" if pre / n < 0.25 else "mixed")
    return row


def promotion_report(prof, dep, path):
    """The three criteria Eric set: concentration, era-stability, discrimination."""
    nat = prof[prof.basis == "national"]
    tr = nat[nat.era == "train_2022_23"].set_index(["hh", "rule"])
    te = nat[nat.era == "test_2024"].set_index(["hh", "rule"])
    both = tr.join(te, lsuffix="_tr", rsuffix="_te", how="inner")
    MIN = 5   # a rule characterised off fewer variances than this is not characterised
    ok = both[(both.n_variances_tr >= MIN) & (both.n_variances_te >= MIN)]

    lines = ["# Per-rule error profiles: do they clear the promotion bar?", "",
             "Generated by `methods/rule_error_profiles.py`. Every tabulation is",
             "variance-level: a case carries up to nine variances, and a case also",
             "trips several rules, so per-rule counts are not a partition of the",
             "flagged total.", "",
             "Rules profiled: %d. Both eras with at least %d variances: %d."
             % (len(both), MIN, len(ok)), ""]

    # 1. concentration
    lines += ["## 1. Concentration: does the mode element dominate a rule's errors?", ""]
    for era, lab in (("_te", "FY2024"), ("_tr", "FY2022-23")):
        s = ok["mode_element_share" + era]
        lines.append("- %s: median top-element share %.3f, quartiles %.3f / %.3f, "
                     "share of rules above 0.50: %.3f"
                     % (lab, s.median(), s.quantile(.25), s.quantile(.75),
                        (s > 0.5).mean()))
    lines.append("")

    # 2. era-stability
    agree = (ok["mode_element_tr"] == ok["mode_element_te"])
    lines += ["## 2. Era-stability: does the FY2022-23 profile describe FY2024?", "",
              "This is the real test. If a rule's mode element flips between eras,",
              "the column is a training-set artifact and shipping it would mislead.", "",
              "- mode element agrees across eras: %d of %d rules (%.3f)"
              % (agree.sum(), len(ok), agree.mean())]
    if len(ok):
        base = ok["mode_element_te"].value_counts(normalize=True)
        lines.append("- chance agreement if the mode were drawn from the FY2024 "
                     "marginal: %.3f" % (base ** 2).sum())
        for col, lab in (("share_agency", "agency-caused share"),
                         ("share_desk_closable", "desk-closable share"),
                         ("share_catchable_at_action", "catchable-at-action share"),
                         ("share_pre_auth", "pre-authorization share")):
            r = ok[col + "_tr"].corr(ok[col + "_te"])
            lines.append("- %s, correlation across eras: %.3f" % (lab, r))
    lines.append("")

    # 3. discrimination
    lines += ["## 3. Discrimination: does the profile vary enough across rules?", ""]
    for col, lab in (("share_agency", "agency-caused share"),
                     ("share_desk_closable", "desk-closable share"),
                     ("share_catchable_at_action", "catchable-at-action share"),
                     ("share_pre_auth", "pre-authorization share")):
        s = ok[col + "_te"]
        lines.append("- %s on FY2024: median %.3f, 10th-90th %.3f to %.3f, sd %.3f"
                     % (lab, s.median(), s.quantile(.1), s.quantile(.9), s.std()))
    if len(ok):
        lines.append("")
        lines.append("Review-mode assignment on FY2024 (national basis):")
        for k, n in ok["review_mode_te"].value_counts().items():
            lines.append("- %s: %d rules" % (k, n))
        lines.append("")
        lines.append("Distinct mode elements across rules: %d"
                     % ok["mode_element_te"].nunique())
    lines.append("")

    dep_prof = prof[prof.basis == "deployed"]
    lines += ["## Deployed basis (what the lists actually pull on FY2024)", "",
              "- deployed rule instances: %d across %d state-and-budget lists"
              % (len(dep), dep.groupby(["state", "budget"]).ngroups),
              "- distinct rules: %d" % len(dep_prof),
              "- rules with at least %d variances: %d"
              % (MIN, int((dep_prof.n_variances >= MIN).sum())),
              "- median variances per deployed rule: %.1f"
              % dep_prof.n_variances.median(),
              "- over/under on the deployed basis: %d overissuance, %d "
              "underissuance, %d ineligible variances"
              % (dep_prof.findg_overissuance.sum(),
                 dep_prof.findg_underissuance.sum(),
                 dep_prof.findg_ineligible.sum())]
    with open(path, "w", encoding="utf-8") as fh:
        fh.write("\n".join(lines) + "\n")
    print("\n".join(lines))


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--stage", type=int, default=3,
                    help="run stages 1..N (default 3, the whole build)")
    ap.add_argument("--states", default="",
                    help="comma-separated state names; default all in the scorecard")
    args = ap.parse_args()
    os.makedirs(OUT, exist_ok=True)

    meta = json.load(open(os.path.join(BENCH, "holdout_metrics.json"),
                          encoding="utf-8"))
    expect = {(r["state"], int(r["budget_pct"])): r for r in meta["records"]}
    states = ([s.strip() for s in args.states.split(",") if s.strip()]
              or sorted({s for s, _ in expect}))
    print("states: %d | stages through %d" % (len(states), args.stage))

    df = load_frame()
    dep, flg, chk = stage1(df, states, expect)
    report_checks(chk)
    chk.to_csv(os.path.join(OUT, "stage1_checks.csv"), index=False)
    dep.to_csv(os.path.join(OUT, "deployed_rules.csv"), index=False)
    print("deployed rule instances: %d | distinct (stratum, rule): %d | flagged case rows: %d"
          % (len(dep), dep.groupby(["hh", "rule"]).ngroups, len(flg)))
    if args.stage < 2:
        return

    # -------------------------------------------------------------- stage 2
    v = load_variances(TRAIN_YEARS + [TEST_YEAR])
    print("variance records loaded: %d (%s)"
          % (len(v), ", ".join("%s: %d" % (y, int((v.fiscal_year == y).sum()))
                               for y in TRAIN_YEARS + [TEST_YEAR])))

    # The frame's `state` column holds the state NAME; the QC files key on FIPS.
    # additional_data/state_data.csv is the lookup (it carries a UTF-8 BOM).
    fips = pd.read_csv(os.path.join(ROOT, "additional_data", "state_data.csv"),
                       encoding="utf-8-sig")
    name2fips = dict(zip(fips["state"], fips["fips"].astype(float)))
    key = ["fips", "yrmonth", "hhldno"]
    dfk = df[["state_name", "yrmonth", "hhldno"]].copy()
    dfk["fips"] = dfk["state_name"].map(name2fips)
    assert dfk["fips"].notna().all(), "unmapped state names: %s" % sorted(
        set(dfk.loc[dfk["fips"].isna(), "state_name"]))
    dfk["frame_row"] = np.arange(len(df))
    v = v.rename(columns={"STATE": "fips", "YRMONTH": "yrmonth",
                          "HHLDNO": "hhldno"})
    for c in key:
        dfk[c] = pd.to_numeric(dfk[c], errors="coerce")
        v[c] = pd.to_numeric(v[c], errors="coerce")
    # only match within the same fiscal year, so a repeated household number in
    # a later year cannot bind to an earlier frame row
    dfk["fiscal_year"] = df["fiscal_year"].values
    v = v.merge(dfk[key + ["fiscal_year", "frame_row"]],
                on=key + ["fiscal_year"], how="inner")
    print("variance records matched to a frame row: %d" % len(v))
    matched_cases = df.index.isin(v["frame_row"].unique())
    err_rows = df["is_error"].to_numpy()
    print("error cases in the frame with at least one matched variance: %d of %d"
          % (int((matched_cases & err_rows).sum()), int(err_rows.sum())))
    v = v.sort_values("frame_row").reset_index(drop=True)
    # start/stop offsets so a rule's variance rows are gathered without merging
    vrow = v["frame_row"].to_numpy()
    starts = np.searchsorted(vrow, np.arange(len(df)), side="left")
    stops = np.searchsorted(vrow, np.arange(len(df)), side="right")

    def gather(frame_rows):
        parts = [np.arange(starts[r], stops[r]) for r in frame_rows
                 if stops[r] > starts[r]]
        return np.concatenate(parts) if parts else np.empty(0, dtype=int)

    # the distinct rules to profile, and the cases each one deploys against
    rules = dep[["hh", "rule"]].drop_duplicates().reset_index(drop=True)
    deployed_rows = (flg.groupby(["hh", "rule"])["frame_row"]
                     .apply(lambda s: np.unique(s.to_numpy())).to_dict())

    is_err = df["is_error"].to_numpy()
    fy = df["fiscal_year"].to_numpy()
    hh_all = df["hh"].to_numpy()
    era_mask = {"train_2022_23": np.isin(fy, TRAIN_YEARS),
                "test_2024": fy == TEST_YEAR}

    prof_rows = []
    for i, (hh, rule) in enumerate(rules.itertuples(index=False, name=None), 1):
        if i % 200 == 0:
            print("  profiled %d / %d" % (i, len(rules)), flush=True)
        flag = np.asarray(df[FEATURES].eval(rule), dtype=bool) & (hh_all == hh)
        # basis "national": the same rule on every national row of the era, so the
        # two eras are computed the same way and are comparable
        for era, m in era_mask.items():
            rows = np.flatnonzero(flag & m & is_err)
            prof_rows.append(profile(hh, rule, era, "national", rows, gather, v,
                                     n_cases_flagged=int((flag & m).sum())))
        # basis "deployed": the FY2024 cases the delivered lists actually pull
        drows = deployed_rows.get((hh, rule), np.empty(0, dtype=int))
        derr = drows[is_err[drows]] if drows.size else drows
        prof_rows.append(profile(hh, rule, "test_2024", "deployed", derr, gather,
                                 v, n_cases_flagged=int(drows.size)))
    prof = pd.DataFrame(prof_rows)
    prof.to_csv(os.path.join(OUT, "rule_profiles.csv"), index=False)
    print("wrote %d profile rows (%d rules x 2 eras + deployed basis)"
          % (len(prof), len(rules)))
    if args.stage < 3:
        return

    # -------------------------------------------------------------- stage 3
    promotion_report(prof, dep, os.path.join(OUT, "promotion_criteria.md"))


if __name__ == "__main__":
    main()
