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
import math
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
        "over_threshold", "total_error_amount", "cert_HH_size_FS_n",
        "error_status", "status"]

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
# VERIF is deliberately NOT used to decide anything. It records the evidence the
# QC reviewer needed to substantiate a finding after the fact, which is QC's
# evidentiary burden rather than a statement about what a caseworker could resolve
# from the file at certification.
# TIMEPER, when the variance occurred relative to the agency's most recent action.
TIMEPER_BUCKET = {1: "before_action", 2: "at_action", 3: "after_action",
                  9: "undetermined"}
# Direction (over vs under) comes from the frame's error_status at case level,
# not from E_FINDG: E_FINDG is populated for 57% of variances and three states
# report none of it.


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
    # float_precision="round_trip" so the parser adds no error of its own (the
    # default and "high" C parsers land 1 ULP low on many decimals; see
    # methods/known_constraints.md#munging and the erratum in
    # methods/rule_error_profiles/README.md). NOTE: reg_model_data.rds is the
    # source of truth; the on-disk CSV is a 15-significant-digit export, so a
    # regeneration from it will still miss cases sitting 1-2 ULP above a rule
    # threshold (the 2026-08-06 erratum's two cases) until the CSV is
    # re-exported at full precision (%.17g) from the rds.
    df = pd.read_csv(FRAME, usecols=FEATURES + KEYS, low_memory=False,
                     float_precision="round_trip")
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
# All code definitions come from the FY2024 SNAP QC Technical Documentation
# (additional_data/FY-2024-Tech-Doc.pdf) and the element/nature nesting in
# additional_data/FNS380-1WithInstructions.pdf. Nothing here is inferred from the
# data. The FY2023 doc is superseded: it defines neither AGENCY 22-26 nor NATURE
# 33, 56, 57, 58, all of which appear in FY2024 data.

# Element groups, taken from the munging script's own reconstruction groups
# (1_data_munging..., lines 603-627) so the characterization and the frame agree
# on what an "earned income error" is. 321 sits with earned income.
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

# Nature groups. The tech doc lists natures flat, and FNS-380-1 nests them under
# elements only to say which are PERMITTED where: a nature code carries the same
# meaning wherever it appears (52/53/56/57 mean the same under earned income
# deductions, dependent care, shelter, medical and child support; 35/37/38/44 mean
# the same under every income element). So the grouping is element-independent and
# describes how the error happened.
NATURE_GROUP = {}
for c in (38, 44, 56, 57, 54):
    # right item, wrong figure
    NATURE_GROUP[c] = "wrong amount, known item"
for c in (52, 53, 37, 58, 32, 33, 24, 30, 51):
    # the information was in hand; it was counted when it should not have been, or
    # not counted when it should have been
    NATURE_GROUP[c] = "wrong include/exclude decision"
NATURE_GROUP[35] = "unreported source of income"
for c in (39, 40, 41, 64, 65):
    # coded as arising FROM a change in employment, residence or household size
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

# AGENCY, FY2024 definitions. 26 is not a fault code: "change was not required to
# be reported by the client or acted upon by the State".
AGENCY_BUCKET_2024 = {1: "client", 2: "client", 3: "client", 4: "client",
                      7: "third_party", 8: "third_party", 26: "no_fault",
                      99: "other"}
for c in (10, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25):
    AGENCY_BUCKET_2024[c] = "agency"
CAUSES = ["agency", "client", "third_party", "no_fault", "other"]

DISCOV_2024 = {1: "case_record", 2: "case_record", 3: "outside_contact",
               4: "outside_contact", 5: "outside_contact", 6: "outside_contact",
               7: "outside_contact", 8: "external_match", 9: "other"}
TIMEPER_2024 = {1: "before_action", 2: "at_action", 3: "after_action",
                9: "undetermined"}


def wilson(k, n, z=1.96):
    """Two-sided Wilson interval. Shipped next to every share, because a share on
    20 variances and a share on 400 are different objects."""
    if not n:
        return (np.nan, np.nan)
    p = k / n
    d = 1 + z * z / n
    c = (p + z * z / (2 * n)) / d
    h = z * math.sqrt(p * (1 - p) / n + z * z / (4 * n * n)) / d
    return (max(0.0, c - h), min(1.0, c + h))


def load_variances(years):
    """One row per (case, variance slot). A slot is a variance when ELEMENT is
    populated; every such slot also carries a NATURE."""
    frames = []
    for y in years:
        path = os.path.join(QCDIR, "qc_pub_fy%s.sav" % y)
        cols = (["STATE", "YRMONTH", "HHLDNO", "AMTERR"] +
                ["%s%d" % (p, i) for p in
                 ("ELEMENT", "NATURE", "AGENCY", "DISCOV", "TIMEPER")
                 for i in VAR_SLOTS])
        raw, _ = pyreadstat.read_sav(path, usecols=cols)
        long = []
        for i in VAR_SLOTS:
            sub = raw[["STATE", "YRMONTH", "HHLDNO", "AMTERR"]].copy()
            for p in ("ELEMENT", "NATURE", "AGENCY", "DISCOV", "TIMEPER"):
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
    v["discov_b"] = v["DISCOV"].map(DISCOV_2024).fillna("unpopulated")
    v["timeper_b"] = v["TIMEPER"].map(TIMEPER_2024).fillna("unpopulated")
    return v


def _to_75(series):
    s = series.dropna()
    if not len(s):
        return "", 0
    vc = s.value_counts(normalize=True)
    keep, run = [], 0.0
    for val, sh in vc.items():
        keep.append((val, sh))
        run += sh
        if run >= 0.75:
            break
    numeric = pd.api.types.is_numeric_dtype(s)
    fmt = (lambda x: "%d" % x) if numeric else str
    return "; ".join("%s %.2f" % (fmt(a), b) for a, b in keep), len(keep)


def characterize(hh, rule, era, rows, gather, v, n_flagged, over, under, other):
    """Descriptive fields for one rule, each share with its Wilson interval.
    No categories, no verdicts: the state decides which rules suit it."""
    sub = v.iloc[gather(rows)]
    n = len(sub)
    r = dict(hh=hh, rule=rule, era=era, n_cases_flagged=n_flagged,
             n_error_cases=int(len(rows)), n_variances=n)

    def share(mask, denom_mask=None, name=None):
        d = sub if denom_mask is None else sub[denom_mask]
        nd = len(d)
        k = int(mask[d.index].sum()) if nd else 0
        lo, hi = wilson(k, nd)
        r["n_" + name] = k
        r["denom_" + name] = nd
        r[name] = k / nd if nd else np.nan
        r[name + "_lo"], r[name + "_hi"] = lo, hi

    for g in ELEMENT_GROUPS:
        share(sub["element_group"] == g, name="elem_" + g.replace(" ", "_"))
    for g in NATURE_GROUPS:
        share(sub["nature_group"] == g, name="nat_" + g.split(",")[0].replace(" ", "_"))
    kt = sub["timeper_b"] != "unpopulated"
    # TIMEPER is coded against the agency's most recent action, which is the
    # certification or recertification action for most cases.
    for code, nm in (("at_action", "at_certification"),
                     ("before_action", "before_certification"),
                     ("after_action", "after_certification")):
        share(sub["timeper_b"] == code, kt, "timing_" + nm)
    kc = sub["cause"] != "unpopulated"
    for c in CAUSES:
        share(sub["cause"] == c, kc, "cause_" + c)
    kd = sub["discov_b"] != "unpopulated"
    share(sub["discov_b"] == "case_record", kd, "found_in_case_record")

    r["elements_to_75"], r["n_elements_to_75"] = _to_75(sub["ELEMENT"])
    r["natures_to_75"], r["n_natures_to_75"] = _to_75(sub["NATURE"])
    r["element_groups_to_75"], _ = _to_75(sub["element_group"])
    r["nature_groups_to_75"], _ = _to_75(sub["nature_group"])

    tot = over + under + other
    r["n_cases_overissuance"], r["n_cases_underissuance"] = over, under
    r["n_cases_other_error"] = other
    # Direction is case-level and comes from `status` (2 overissuance,
    # 3 underissuance), which is populated for every error case. NOT from
    # error_status: its "other_error" category is a residual error TYPE carrying
    # both directions, so putting it in the denominator would make the field a
    # measure of how little other_error a rule catches rather than of direction.
    r["denom_share_overissuance"] = over + under
    r["share_overissuance"] = over / (over + under) if (over + under) else np.nan
    lo, hi = wilson(over, over + under)
    r["share_overissuance_lo"], r["share_overissuance_hi"] = lo, hi
    r["share_underissuance"] = under / (over + under) if (over + under) else np.nan
    # kept as a separate descriptive field, not folded into the direction one
    r["n_cases_other_error_type"] = other
    r["share_other_error_type"] = other / tot if tot else np.nan
    r["denom_share_other_error_type"] = tot
    r["amterr_total"] = float(sub["AMTERR"].fillna(0).sum())
    return r


# fields carried into the field-level evidence
EVID = ([("elem_" + g.replace(" ", "_"), g) for g in ELEMENT_GROUPS[:6]] +
        [("nat_" + g.split(",")[0].replace(" ", "_"), g) for g in NATURE_GROUPS[:5]] +
        [("timing_at_certification", "arose at certification"),
         ("timing_after_certification", "arose after certification"),
         ("cause_agency", "coded agency-caused"),
         ("cause_client", "coded client-caused"),
         ("found_in_case_record", "surfaced from the case record"),
         ("share_overissuance", "overissuance, of directional error cases"),
         ("share_other_error_type", "error_status is other_error")])


def reliability(df, col, minn=20):
    """Share of the spread across rules that is real between-rule difference
    rather than sampling error. This is the question a state has: does this number
    tell me something about THIS rule, or is it noise?"""
    d = df[(df["denom_" + col] >= minn)] if ("denom_" + col) in df else df
    p = d[col].dropna()
    if len(p) < 10:
        return np.nan, len(p)
    nn = d.loc[p.index, "denom_" + col] if ("denom_" + col) in d else np.nan
    v_obs = p.var(ddof=1)
    v_samp = float((p * (1 - p) / nn).mean())
    return (max(0.0, v_obs - v_samp) / v_obs if v_obs > 0 else np.nan), len(p)


def mutual_info(rule_id, lab, n_rules, n_lab):
    c = np.bincount(rule_id * n_lab + lab, minlength=n_rules * n_lab
                    ).reshape(n_rules, n_lab).astype(float)
    N = c.sum()
    if N == 0:
        return 0.0
    a, b = c.sum(1, keepdims=True), c.sum(0, keepdims=True)
    with np.errstate(divide="ignore", invalid="ignore"):
        t = c / N * np.log((c * N) / (a * b))
    return float(np.nansum(t))


def entropy(counts):
    p = counts / counts.sum()
    p = p[p > 0]
    return float(-(p * np.log(p)).sum())


def field_evidence(prof, v, rules_vi, state_of_var, path, dep, reps=40, seed=11):
    """Evidence about the FIELDS, not verdicts about individual rules.

    Three things a state's analyst should be able to check before trusting a
    column: does it carry rule-level signal (reliability), does the same story
    appear in a different set of states (split-half over states, against the
    sampling floor), and does knowing the rule tell you anything about the error
    at all (mutual information against a permutation null)."""
    rng = np.random.default_rng(seed)
    allp = prof[prof.era == "all_2022_24"].reset_index(drop=True)
    tr = prof[prof.era == "train_2022_23"].set_index(["hh", "rule"])
    te = prof[prof.era == "test_2024"].set_index(["hh", "rule"])
    L = []

    def add(*x):
        L.extend(x)

    add("# Which characterization fields carry signal", "",
        "Fields describing what each rule finds, so a state can judge for itself",
        "whether a rule is worth using given what it can catch and fix. Nothing",
        "here sorts rules into categories; that judgement belongs to the state.",
        "", "Every share is reported with a Wilson interval in",
        "`rule_characterization.csv`. A share on 20 variances and a share on 400",
        "are different objects and the interval is what says so.", "",
        "Rules characterized: %d, over %d deployed instances in %d lists."
        % (len(allp), len(dep), dep.groupby(["state", "budget"]).ngroups), "")

    add("## Code coverage on error-case variances (FY2024)", "")
    t = v[v.fiscal_year == TEST_YEAR]
    for col, lab in (("nature_group", "NATURE"), ("cause", "AGENCY"),
                     ("timeper_b", "TIMEPER"), ("discov_b", "DISCOV")):
        bad = t[col].isin(["unpopulated", "UNMAPPED"]).mean()
        add("- %-8s populated and mapped: %.3f" % (lab, 1 - bad))
    add("", "National mix on FY2024 error-case variances, so a rule's share reads "
        "as lift:", "")
    for col, groups in (("element_group", ELEMENT_GROUPS),
                        ("nature_group", NATURE_GROUPS)):
        m = t[col].value_counts(normalize=True)
        for g in groups:
            if g in m and m[g] >= 0.004:
                add("- %-36s %.3f" % (g, m[g]))
        add("")
    kt = t[t.timeper_b != "unpopulated"]
    add("- of variances reporting a timing: %.3f at the agency's action, %.3f "
        "before, %.3f after"
        % ((kt.timeper_b == "at_action").mean(), (kt.timeper_b == "before_action").mean(),
           (kt.timeper_b == "after_action").mean()),
        "- cause: %s"
        % ", ".join("%s %.3f" % (c, (t.cause == c).mean()) for c in CAUSES), "")

    # ---- split-half over states, against the sampling floor
    states = np.unique(state_of_var[~np.isnan(state_of_var)])
    obs = {c: [] for c, _ in EVID}
    exp = {c: [] for c, _ in EVID}
    num = {c: None for c, _ in EVID}
    # numerator masks over the variance table, once
    masks = {}
    for c, _ in EVID:
        if c.startswith("elem_"):
            g = c[5:].replace("_", " ")
            masks[c] = ((v["element_group"] == g).to_numpy(),
                        np.ones(len(v), bool))
        elif c.startswith("nat_"):
            g = [x for x in NATURE_GROUPS
                 if x.split(",")[0].replace(" ", "_") == c[4:]][0]
            masks[c] = ((v["nature_group"] == g).to_numpy(), np.ones(len(v), bool))
        elif c.startswith("timing_"):
            code = {"timing_at_certification": "at_action",
                    "timing_after_certification": "after_action"}[c]
            d = (v["timeper_b"] != "unpopulated").to_numpy()
            masks[c] = ((v["timeper_b"] == code).to_numpy(), d)
        elif c.startswith("cause_"):
            d = (v["cause"] != "unpopulated").to_numpy()
            masks[c] = ((v["cause"] == c[6:]).to_numpy(), d)
        elif c == "found_in_case_record":
            d = (v["discov_b"] != "unpopulated").to_numpy()
            masks[c] = ((v["discov_b"] == "case_record").to_numpy(), d)
        else:
            masks[c] = (None, None)

    MINH = 20
    for rep in range(reps):
        half = set(rng.choice(states, size=len(states) // 2, replace=False).tolist())
        inA = np.isin(state_of_var, list(half))
        for c, _ in EVID:
            nm, dm = masks[c]
            if nm is None:
                continue
            for vi in rules_vi:
                if vi.size < 2 * MINH:
                    continue
                a = vi[inA[vi]]
                b = vi[~inA[vi]]
                da, db = int(dm[a].sum()), int(dm[b].sum())
                if da < MINH or db < MINH:
                    continue
                ka, kb = int(nm[a].sum()), int(nm[b].sum())
                pa, pb = ka / da, kb / db
                pp = (ka + kb) / (da + db)
                obs[c].append(abs(pa - pb))
                exp[c].append(math.sqrt(2 / math.pi) *
                              math.sqrt(max(pp * (1 - pp), 0) * (1 / da + 1 / db)))

    add("## Per field: does it carry rule-level signal, and does it travel?", "",
        "Reliability is the share of the spread across rules that is real",
        "between-rule difference rather than sampling error. The split-half columns",
        "compare the same rule computed on two random halves of the 49 states",
        "against the difference sampling alone would produce; a ratio near 1 means",
        "the field is as stable as its support allows.", "",
        "| field | median share | 10th-90th | reliability | split-half obs | floor | ratio |",
        "|---|---|---|---|---|---|---|")
    for c, lab in EVID:
        if c not in allp:
            continue
        s = allp[c].dropna()
        if not len(s):
            continue
        rel, nrel = reliability(allp, c)
        o = np.median(obs[c]) if obs[c] else np.nan
        e = np.median(exp[c]) if exp[c] else np.nan
        add("| %s | %.3f | %.3f-%.3f | %s | %s | %s | %s |"
            % (lab, s.median(), s.quantile(.1), s.quantile(.9),
               "%.2f" % rel if rel == rel else "n/a",
               "%.3f" % o if o == o else "n/a",
               "%.3f" % e if e == e else "n/a",
               "%.2f" % (o / e) if (o == o and e == e and e > 0) else "n/a"))
    add("")

    # ---- era drift, same floor logic
    both = tr.join(te, lsuffix="_tr", rsuffix="_te", how="inner")
    add("## Era drift, FY2022-23 against FY2024", "",
        "| field | median abs difference | sampling floor | ratio |",
        "|---|---|---|---|")
    for c, lab in EVID:
        if (c + "_tr") not in both or ("denom_" + c + "_tr") not in both:
            continue
        d = both[[c + "_tr", c + "_te", "denom_" + c + "_tr", "denom_" + c + "_te"]].dropna()
        d = d[(d["denom_" + c + "_tr"] >= MINH) & (d["denom_" + c + "_te"] >= MINH)]
        if len(d) < 10:
            continue
        o = float((d[c + "_tr"] - d[c + "_te"]).abs().median())
        pp = ((d[c + "_tr"] * d["denom_" + c + "_tr"] + d[c + "_te"] * d["denom_" + c + "_te"]) /
              (d["denom_" + c + "_tr"] + d["denom_" + c + "_te"]))
        e = float((np.sqrt(2 / np.pi) * np.sqrt(pp * (1 - pp) *
                   (1 / d["denom_" + c + "_tr"] + 1 / d["denom_" + c + "_te"]))).median())
        add("| %s | %.3f | %.3f | %.2f |" % (lab, o, e, o / e if e > 0 else np.nan))
    add("")

    # ---- does the rule tell you anything at all
    add("## Does knowing the rule tell you anything about the error?", "")
    for col, groups, lab in (("element_group", ELEMENT_GROUPS, "element group"),
                             ("nature_group", NATURE_GROUPS, "nature group")):
        codes = {g: i for i, g in enumerate(groups)}
        lab_arr = v[col].map(codes).fillna(len(groups)).astype(int).to_numpy()
        G = len(groups) + 1
        rid, idx = [], []
        for i, vi in enumerate(rules_vi):
            if vi.size:
                rid.append(np.full(vi.size, i))
                idx.append(vi)
        rid = np.concatenate(rid)
        idx = np.concatenate(idx)
        lab_r = lab_arr[idx]
        mi = mutual_info(rid, lab_r, len(rules_vi), G)
        hu = entropy(np.bincount(rid, minlength=len(rules_vi)).astype(float))
        hv = entropy(np.bincount(lab_r, minlength=G).astype(float))
        nmi = mi / math.sqrt(hu * hv) if hu > 0 and hv > 0 else np.nan
        null = []
        for _ in range(60):
            null.append(mutual_info(rid, rng.permutation(lab_r), len(rules_vi), G))
        null = np.array(null)
        add("- %s: MI %.4f nats, NMI %.4f; permutation null %.4f +/- %.4f, "
            "so observed is %.1f sd above chance"
            % (lab, mi, nmi, null.mean(), null.std(),
               (mi - null.mean()) / null.std() if null.std() > 0 else np.nan))
    add("", "(A case can trip several rules, so the units here are (rule, variance)",
        "pairs rather than a partition. The permutation null has the same",
        "structure, so the comparison holds.)", "")

    add("## Support", "",
        "- median variances per rule, FY2022-24 pooled: %.0f" % allp.n_variances.median(),
        "- rules with at least 20 variances: %d of %d"
        % (int((allp.n_variances >= 20).sum()), len(allp)),
        "- median error cases per rule: %.0f" % allp.n_error_cases.median(),
        "- direction, case level: %d overissuance, %d underissuance, %d other_error"
        % (allp.n_cases_overissuance.sum(), allp.n_cases_underissuance.sum(),
           allp.n_cases_other_error.sum()))
    open(path, "w", encoding="utf-8", newline="\n").write("\n".join(L) + "\n")
    print("\n".join(L))


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--stage", type=int, default=3)
    ap.add_argument("--states", default="")
    args = ap.parse_args()
    os.makedirs(OUT, exist_ok=True)

    meta = json.load(open(os.path.join(BENCH, "holdout_metrics.json"), encoding="utf-8"))
    expect = {(r["state"], int(r["budget_pct"])): r for r in meta["records"]}
    states = ([s.strip() for s in args.states.split(",") if s.strip()]
              or sorted({s for s, _ in expect}))
    print("states: %d | stages through %d" % (len(states), args.stage))

    df = load_frame()
    dep, flg, chk = stage1(df, states, expect)
    report_checks(chk)
    chk.to_csv(os.path.join(OUT, "stage1_checks.csv"), index=False)
    dep.to_csv(os.path.join(OUT, "deployed_rules.csv"), index=False)
    if args.stage < 2:
        return

    v = load_variances(TRAIN_YEARS + [TEST_YEAR])
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
    v = v.rename(columns={"STATE": "fips", "YRMONTH": "yrmonth", "HHLDNO": "hhldno"})
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
        parts = [np.arange(starts[r], stops[r]) for r in rows if stops[r] > starts[r]]
        return np.concatenate(parts) if parts else np.empty(0, dtype=int)

    rules = dep[["hh", "rule"]].drop_duplicates().reset_index(drop=True)
    is_err = df["is_error"].to_numpy()
    status = df["error_status"].to_numpy()
    direction_code = pd.to_numeric(df["status"], errors="coerce").to_numpy()
    fy = df["fiscal_year"].to_numpy()
    hh_all = df["hh"].to_numpy()
    eras = {"all_2022_24": np.ones(len(df), bool),
            "train_2022_23": np.isin(fy, TRAIN_YEARS),
            "test_2024": fy == TEST_YEAR}

    out, rules_vi = [], []
    for i, (hh, rule) in enumerate(rules.itertuples(index=False, name=None), 1):
        if i % 200 == 0:
            print("  characterized %d / %d" % (i, len(rules)), flush=True)
        flag = np.asarray(df[FEATURES].eval(rule), dtype=bool) & (hh_all == hh)
        for era, m in eras.items():
            rows = np.flatnonzero(flag & m & is_err)
            o = int((direction_code[rows] == 2).sum())
            u = int((direction_code[rows] == 3).sum())
            ot = int((status[rows] == "other_error").sum())
            rec = characterize(hh, rule, era, rows, gather, v,
                               int((flag & m).sum()), o, u, ot)
            out.append(rec)
            if era == "all_2022_24":
                rules_vi.append(gather(rows))
    prof = pd.DataFrame(out)
    prof.to_csv(os.path.join(OUT, "rule_profiles.csv"), index=False)
    ship = prof[prof.era == "all_2022_24"].drop(columns=["era"])
    ship.to_csv(os.path.join(OUT, "rule_characterization.csv"), index=False)
    print("characterization sheet: %d rules, %d columns" % (len(ship), ship.shape[1]))
    if args.stage < 3:
        return
    field_evidence(prof, v, rules_vi, v["fips"].to_numpy(),
                   os.path.join(OUT, "characterization.md"), dep)


if __name__ == "__main__":
    main()
