# State-by-state reading on the three validation years (2026-11-14), as a check on 09_state_medians.py, where the
# promoted xgboost pieces (ratio inputs, era indicator) raised the typical state at 5% on the test years although the
# national figure was flat. Same statistics, arm minus the B-recipe twin (5 fits each), on FY2017 (backward fits from
# 08_blend_validation.py), FY2018 and FY2023 (saved held-back predictions). FY2018 and FY2023 chose the pieces, on log
# loss and gamma deviance; FY2017 chose nothing. Descriptive: no rule was written for this reading.
#   .venv/Scripts/python.exe -u 09b_state_medians_validation.py
import sys, os, json, numpy as np, pandas as pd
HERE = os.path.dirname(os.path.abspath(__file__)); sys.path.insert(0, os.path.join(HERE, "reviews", "scratch_blend_statistician"))
import lib as L
NB = 1000; b = L.B_EXP; WTS = {"FY2017": 0.25, "FY2018": 0.25, "FY2023": 0.5}
def shares(fr, perm, c=None):
    got, tot = fr.state_caught(perm, c); ok = tot > 0; return got[0], got[2], tot, ok
def summ(fr, pa, pb, c=None):
    a5, a10, tot, ok = shares(fr, pa, c); b5, b10, _, _ = shares(fr, pb, c); o = {}
    for k, a, bb in (("5", a5, b5), ("10", a10, b10)):
        d = a[ok] / tot[ok] - bb[ok] / tot[ok]
        o.update({"medch" + k: np.median(d), "meanch" + k: d.mean(), "ahead" + k: (d > 1e-12).sum(), "behind" + k: (d < -1e-12).sum(), "harmed" + k: (d < -0.05).sum()})
    return o
z = lambda f: np.load(os.path.join(L.HB, f + ".npz")); Y = {}
d = pd.read_csv(os.path.join(HERE, "data", "W1.csv"), usecols=["role", "yr", "st", "y", "d", "keep"], dtype={"role": str, "st": str, "yr": str}); d = d[d.yr == "2017"].reset_index(drop=True)
A = np.load(os.path.join(HERE, "followup", "blend_validation", "fy2017_fits.npz")); assert (A["st"] == d.st.to_numpy()).all()
Y["FY2017"] = (L.Frame(d.st.to_numpy(), d.d.to_numpy(), d.keep.to_numpy() == 1, 2017, d.y.to_numpy()), A["p_xr"], A["s_x"], A["p_c"], A["s_c"], A["p_xb"], A["s_x"])
Y["FY2018"] = (L.heldback_frame("W1"), z("xgb_W1_ratio")["p"], z("xgb_W1_base")["size"], z("cb_W1_base")["p"], z("cb_W1_base")["size"], z("xgb_W1_base")["p"], z("xgb_W1_base")["size"])
Y["FY2023"] = (L.heldback_frame("W3"), z("xgb_W3_ratio_era")["p"], z("xgb_W3_era")["size"], z("cb_W3_era")["p"], z("cb_W3_era")["size"], z("xgb_W3_base")["p"], z("xgb_W3_base")["size"])
VAL = {}; BOOT = {}; KEYS = None
for y, (fr, Px, Sx, Pc, Sc, Pb, Sb) in Y.items():
    arms = {"xgboost, promoted pieces, no exponent": (Px * Sx).mean(axis=0), "xgboost, promoted pieces": (Px * Sx ** b).mean(axis=0),
            "CatBoost": (Pc * Sc).mean(axis=0), "xgboost-CatBoost blend": (0.5 * (Px + Pc) * (0.5 * (Sx + Sc)) ** b).mean(axis=0)}
    pb = fr.perm((Pb * Sb).mean(axis=0)); perms = {a: fr.perm(v) for a, v in arms.items()}
    for a in arms: VAL[(a, y)] = summ(fr, perms[a], pb)
    KEYS = list(VAL[(a, y)]); rng = np.random.default_rng(int(y[2:])); R = {a: np.empty((NB, len(KEYS))) for a in arms}
    for r in range(NB):
        c = fr.boot_counts(rng)
        for a in arms: s = summ(fr, perms[a], pb, c); R[a][r] = [s[k] for k in KEYS]
    BOOT[y] = R; print(y, "done", flush=True)
ix = {k: i for i, k in enumerate(KEYS)}; rows = []
for a in arms:
    o = {"arm": a}
    for k in ("5", "10"):
        for stat, lab in (("medch", "median_paired_change_"), ("meanch", "mean_paired_change_")):
            dd = 100 * sum(WTS[y] * VAL[(a, y)][stat + k] for y in WTS); s = 100 * np.sqrt(sum(WTS[y] ** 2 * BOOT[y][a][:, ix[stat + k]].var(ddof=1) for y in WTS))
            o[lab + k] = "%+.2f (%.2f) z %+.1f" % (dd, s, dd / s)
            o[lab + k + "_by_year"] = " ".join("%+.2f" % (100 * VAL[(a, y)][stat + k]) for y in WTS)
        o["ahead_behind_" + k] = " ".join("%d/%d" % (VAL[(a, y)]["ahead" + k], VAL[(a, y)]["behind" + k]) for y in WTS); o["harmed_" + k] = " ".join(str(int(VAL[(a, y)]["harmed" + k])) for y in WTS)
    rows.append(o)
T = pd.DataFrame(rows); T.to_csv(os.path.join(HERE, "followup", "results", "state_medians_validation.csv"), index=False); pd.set_option("display.width", 330); pd.set_option("display.max_columns", 30)
for k in ("5", "10"):
    print("\nAT %s%%, arm minus the B-recipe twin, state by state: points (bootstrap SE) z; by year in the order FY2017 FY2018 FY2023; weights 0.25 / 0.25 / 0.5" % k)
    print(T[["arm"] + [c for c in T.columns if c.endswith("_" + k) or c.endswith("_" + k + "_by_year")]].to_string(index=False))
