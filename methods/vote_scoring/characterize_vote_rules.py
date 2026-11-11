"""Characterization columns for the national vote rules (Eric, 2026-11-05).

The vote workbooks list the vote-only rules on Step 3 beside the state's list
rules, but export_vote_rules.R writes only training precision and its lower
bound, so the characterization columns (error $ per flag, national error cases,
elements, natures, overissuance / agency / case-file / certification shares)
were blank for them. This runs the same characterization the delivery lists get
(methods/v250_characterize_lists.py, FY2022-24 national frame, merged as in
runners/run_f1104_build.R) on the vote rules, and adds dollars_per_flag_train:
over-threshold error $ caught per flagged case on that frame, in the rule's
household-size group. Descriptive only: no rule, flag or vote changes.

Input:  deploy/vote_rules_fy2022_2024.csv (export_vote_rules.R)
Output: deploy/vote_rules_fy2022_2024_char.csv (same rows, eight columns added);
        the work folder deploy/vote_char/ keeps the characterization sheet.
Check:  dollars_per_flag_train computed this way must reproduce the mined value
        on the national-pool rules of the f1104 blended lists.
Run:    python methods/vote_scoring/characterize_vote_rules.py
"""
import glob
import os
import shutil
import subprocess
import sys

import numpy as np
import pandas as pd

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
DEPLOY = os.path.join(ROOT, "methods", "vote_scoring", "deploy")
LISTS = os.path.join(ROOT, "methods", "f1104_lists")
SRC = os.environ.get("VOTE_RULES_IN", os.path.join(DEPLOY, "vote_rules_fy2022_2024.csv"))
OUT = os.environ.get("VOTE_RULES_OUT", SRC.replace(".csv", "_char.csv"))
WORK = os.path.join(DEPLOY, "vote_char")
CURATED = ["n_error_cases", "element_groups_to_75", "nature_groups_to_75",
           "found_in_case_record", "share_overissuance",
           "timing_at_certification", "cause_agency"]
sys.path.insert(0, os.path.join(ROOT, "methods"))
import rule_error_profiles as rep  # noqa: E402
from v250_characterize_lists import FEATURES19  # noqa: E402

FEATS = ["utilities_sua" if f == "utilities" else f for f in FEATURES19]


def frame():
    df = pd.read_csv(os.path.join(LISTS, "frame_for_profiles.csv"), low_memory=False,
                     float_precision="round_trip")
    assert len(df) == 115528, len(df)
    for f in FEATS:
        if df[f].dtype == object:
            df[f] = df[f].astype(str).str.upper().map({"TRUE": 1.0, "FALSE": 0.0}).astype(float)
        else:
            df[f] = df[f].astype(float)
    df["hh"] = rep.hh_group_of(df["cert_HH_size_FS_n"])
    return df


def dollars_per_flag(df, rules):
    ed = np.where(df["over_threshold"].fillna(0).to_numpy() != 0,
                  pd.to_numeric(df["total_error_amount"], errors="coerce").fillna(0).abs().to_numpy(), 0.0)
    hh = df["hh"].to_numpy()
    out = []
    for h, rule in rules[["hh", "rule"]].itertuples(index=False, name=None):
        flag = np.asarray(df[FEATS].eval(rule), dtype=bool) & (hh == str(h))
        out.append((int(flag.sum()), round(float(ed[flag].sum()) / max(int(flag.sum()), 1), 2)))
    return out


def main():
    vr = pd.read_csv(SRC, dtype={"hh": str})
    assert len(vr) == 300 and not any(c in vr for c in CURATED + ["dollars_per_flag_train"]), SRC
    os.makedirs(WORK, exist_ok=True)
    shutil.copy(os.path.join(LISTS, "frame_for_profiles.csv"), os.path.join(WORK, "frame_for_profiles.csv"))
    vr[["hh", "rule"]].to_csv(os.path.join(WORK, "vote_rules_for_char.csv"), index=False)
    env = dict(os.environ, V250_STAGE_DIR=WORK, V250_LIST_GLOB="vote_rules_for_char.csv",
               V250_UTIL_FEATURE="utilities_sua")
    subprocess.run([sys.executable, os.path.join(ROOT, "methods", "v250_characterize_lists.py")],
                   env=env, check=True)
    prof = pd.read_csv(os.path.join(WORK, "rule_characterization_v250.csv"), dtype={"hh": str})
    os.remove(os.path.join(WORK, "frame_for_profiles.csv"))

    df = frame()
    # the check: the dollar definition reproduces the mined dollars_per_flag_train on national-pool list rules
    lst = pd.concat([pd.read_csv(p, dtype={"hh": str}) for p in glob.glob(os.path.join(LISTS, "blended_delivery_*.csv"))])
    nat = lst[lst["pool"] == "national"].drop_duplicates(["hh", "rule"]).sample(60, random_state=1)
    got = dollars_per_flag(df, nat)
    n_ok = np.array([g[0] for g in got]) == nat["n_flagged_train"].to_numpy()
    d_ok = np.abs(np.array([g[1] for g in got]) - nat["dollars_per_flag_train"].to_numpy()) < 0.011
    print(f"check on {len(nat)} national-pool list rules: flags match {n_ok.sum()}, $ per flag match {d_ok.sum()}")
    assert n_ok.all() and d_ok.all(), "dollars_per_flag_train does not reproduce the mined value"

    got = dollars_per_flag(df, vr)
    m = vr.merge(prof[["hh", "rule", "n_cases_flagged"] + CURATED], on=["hh", "rule"], how="left", sort=False)
    assert len(m) == len(vr) and m["n_error_cases"].notna().all()
    m["dollars_per_flag_train"] = [g[1] for g in got]
    # the vote rules were mined on this frame (FY2022-24, seed 117), so their training counts must match
    assert (m["n_cases_flagged"].to_numpy() == np.array([g[0] for g in got])).all()
    n_mis = int((m["n_cases_flagged"] != m["n_flagged_train"]).sum())
    print(f"vote rules: {len(m)} characterized; training flag counts differ from the mine on {n_mis}")
    assert n_mis == 0, "vote rules' training counts do not match the FY2022-24 frame"
    m = m.drop(columns=["n_cases_flagged"]).rename(columns={"n_error_cases": "n_error_cases_national"})
    m.to_csv(OUT, index=False)
    print(f"wrote {OUT} ({len(m)} rules, {m.shape[1]} columns)")


if __name__ == "__main__":
    main()
