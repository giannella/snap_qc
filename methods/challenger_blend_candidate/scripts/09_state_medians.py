# State-by-state reading of every arm (2026-11-14). The national figures weight states by error dollars; this asks the
# other question: does an approach raise the typical state? For each arm, on the three test years (already read):
#   the median over states of the state's share of error dollars caught, at 5% and at 10%, against B's median;
#   the median and the mean of the paired state changes (arm minus B, state by state);
#   states ahead and behind, and states harmed (share down by more than 5 points).
# Each state flags the top 5% or 10% of its own cases (B's flag rule), 49 states (48 and DC). Paired bootstrap of
# cases within state, 1,000 draws. Three tests combined 0.2 / 0.4 / 0.4. Reads saved score files only.
#   .venv/Scripts/python.exe -u 09_state_medians.py
import sys, os, glob, numpy as np, pandas as pd
HERE = os.path.dirname(os.path.abspath(__file__)); sys.path.insert(0, os.path.join(HERE, "reviews", "scratch_blend_statistician"))
import lib as L
NB = 1000; OUT = os.path.join(HERE, "followup", "results"); WTS = L.WEIGHTS
ARMS = [("lgbm", L.SC1, "LightGBM"), ("catboost", L.SC1, "CatBoost (first run)"), ("ebm_pairs", L.SC1, "EBM with pairs"), ("ebm_main", L.SC1, "EBM main effects"), ("tabnet", L.SC1, "TabNet"),
        ("bart", L.SC1, "BART (first run)"), ("xgb_tuned", L.SC1, "xgboost re-tuned"), ("xgb_es", L.SC2, "xgboost twin of B"), ("xgb_rev", L.SC2, "xgboost, promoted pieces"),
        ("xgb_rev_b1", L.SC2, "same, no exponent"), ("cb_rev", L.SC2, "CatBoost with era"), ("blend", L.SC2, "xgboost-CatBoost blend"), ("bart_long", L.SC2, "BART, long run")]
def bag(d, name, w, fr):
    fs = sorted(glob.glob(os.path.join(d, "%s_%s_*.csv" % (name, w)))); assert fs, (name, w); e = []
    for f in fs: z = pd.read_csv(f); assert (z.st.to_numpy() == fr.st).all(); e.append(z.ed.to_numpy())
    return np.mean(e, axis=0), len(fs)
def shares(fr, perm, c=None):
    got, tot = fr.state_caught(perm, c); ok = tot > 0; return got[0], got[2], tot, ok          # dollars caught at 5% and at 10%, by state
def summ(fr, pa, pb, c=None):
    a5, a10, tot, ok = shares(fr, pa, c); b5, b10, _, _ = shares(fr, pb, c); o = {}
    for k, a, b in (("5", a5, b5), ("10", a10, b10)):
        sa, sb = a[ok] / tot[ok], b[ok] / tot[ok]; d = sa - sb
        o.update({"med" + k: np.median(sa), "medB" + k: np.median(sb), "dmed" + k: np.median(sa) - np.median(sb), "medch" + k: np.median(d), "meanch" + k: d.mean(),
                  "ahead" + k: (d > 1e-12).sum(), "behind" + k: (d < -1e-12).sum(), "harmed" + k: (d < -0.05).sum()})
    return o
KEYS = None; VAL = {}; BOOT = {}; FITS = {}
for w in WTS:
    fr = L.test_frame(w); Bm = pd.read_csv(os.path.join(HERE, "data", w + "_B_scores.csv")).to_numpy().mean(axis=1); pb = fr.perm(Bm); perms = {}
    for name, d, _ in ARMS: sc, FITS[(name, w)] = bag(d, name, w, fr); perms[name] = fr.perm(sc)
    for name in perms: VAL[(name, w)] = summ(fr, perms[name], pb)
    KEYS = list(VAL[(ARMS[0][0], w)]); rng = np.random.default_rng(909 + int(w[1])); R = {name: np.empty((NB, len(KEYS))) for name in perms}
    for r in range(NB):
        c = fr.boot_counts(rng)
        for name in perms: s = summ(fr, perms[name], pb, c); R[name][r] = [s[k] for k in KEYS]
    BOOT[w] = R; print(w, "done:", int((shares(fr, pb)[3]).sum()), "states", flush=True)
# check against the scorer's own by-state file
bs = pd.read_csv(os.path.join(OUT, "by_state.csv")); chk = bs[(bs.learner == "blend") & (bs.budget == 0.10)].groupby("window").dollar_recall.median()
for w in WTS: assert abs(chk[w] - VAL[("blend", w)]["med10"]) < 1e-9, (w, chk[w], VAL[("blend", w)]["med10"])
ix = {k: i for i, k in enumerate(KEYS)}
def comb(name, k): return sum(WTS[w] * VAL[(name, w)][k] for w in WTS)
def se(name, k): return np.sqrt(sum(WTS[w] ** 2 * BOOT[w][name][:, ix[k]].var(ddof=1) for w in WTS))
rows = []
for name, _, label in ARMS:
    o = {"arm": label, "fits": "/".join(str(FITS[(name, w)]) for w in WTS)}
    for k in ("5", "10"):
        o["median_state_share_%s" % k] = round(100 * comb(name, "med" + k), 2); o["B_median_%s" % k] = round(100 * comb(name, "medB" + k), 2)
        d, s = 100 * comb(name, "dmed" + k), 100 * se(name, "dmed" + k); o["median_minus_B_%s" % k] = "%+.2f (%.2f) z %+.1f" % (d, s, d / s)
        d, s = 100 * comb(name, "medch" + k), 100 * se(name, "medch" + k); o["median_paired_change_%s" % k] = "%+.2f (%.2f) z %+.1f" % (d, s, d / s)
        d, s = 100 * comb(name, "meanch" + k), 100 * se(name, "meanch" + k); o["mean_paired_change_%s" % k] = "%+.2f (%.2f) z %+.1f" % (d, s, d / s)
        o["ahead_behind_%s" % k] = " ".join("%d/%d" % (VAL[(name, w)]["ahead" + k], VAL[(name, w)]["behind" + k]) for w in WTS)
        o["harmed_%s" % k] = " ".join(str(int(VAL[(name, w)]["harmed" + k])) for w in WTS)
    rows.append(o)
T = pd.DataFrame(rows); T.to_csv(os.path.join(OUT, "state_medians.csv"), index=False); pd.set_option("display.width", 320); pd.set_option("display.max_columns", 30)
for k in ("5", "10"):
    print("\nAT %s%%: median over 49 states of the state share of error dollars caught (percent); changes in points (bootstrap SE) z; ahead/behind and harmed by test (FY2019 FY2022 FY2024)" % k)
    print(T[["arm", "fits"] + [c for c in T.columns if c.endswith("_" + k)]].to_string(index=False))
