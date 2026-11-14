# Blend validation (2026-11-14). See design_note_blend_validation.md.
#   .venv/Scripts/python.exe -u 08_blend_validation.py dry|fit17|score
# dry    the scoring code on the three spent test years (FY2019, FY2022, FY2024), as a check of the machinery
# fit17  new fits for FY2017 scored backward: train FY2018 and FY2019, the first test's settings, scaled rounds
# score  the one read: FY2017 (new fits), FY2018 and FY2023 (saved held-back predictions), and the written rule
import sys, os, json, math, types, time, itertools
import numpy as np, pandas as pd
HERE = os.path.dirname(os.path.abspath(__file__)); MODE = sys.argv[1]
OUT = os.path.join(HERE, "followup", "blend_validation"); os.makedirs(OUT, exist_ok=True)
sys.path.insert(0, os.path.join(HERE, "reviews", "scratch_blend_statistician"))
import lib as L                                    # the statistician's share functions (they reproduce the run's figures)
B_EXP = json.load(open(os.path.join(HERE, "followup", "promoted.json")))["xgb"]["exponent"]["b"]; assert abs(B_EXP - L.B_EXP) < 1e-12
SEEDS = [117, 118, 119, 120, 121]; NB = 2000
logf = open(os.path.join(OUT, f"08_{MODE}.log"), "a")
def say(msg):
    line = f"[{time.strftime('%m-%d %H:%M:%S')}] {msg}"; print(line, flush=True); logf.write(line + "\n"); logf.flush()

# ---------------- FY2017 backward fits ----------------
def fit17():
    out = os.path.join(OUT, "fy2017_fits.npz")
    if os.path.exists(out): say("fy2017_fits.npz exists; nothing fitted"); return
    argv = sys.argv; sys.argv = ["05_followup.py", "blendval"]
    src = open(os.path.join(HERE, "05_followup.py"), encoding="utf-8").read().split('\nsay(f"=== 05_followup')[0]
    M = types.ModuleType("fu"); M.__file__ = os.path.join(HERE, "05_followup.py"); exec(compile(src, M.__file__, "exec"), M.__dict__); sys.argv = argv
    import xgboost as xgb
    W = M.load("W1")
    # the loader blanks FY2019's labels (it is the first test's test year). Here FY2019 is a training year, so they are read back.
    raw = pd.read_csv(os.path.join(HERE, "data", "W1.csv"), usecols=["y", "d", "dd"], float_precision="round_trip"); assert len(raw) == len(W.y)
    assert np.array_equal(raw.y.to_numpy(float)[W.tr], W.y[W.tr]) and np.array_equal(raw.dd.to_numpy(float)[W.tr], W.dd[W.tr])
    n_tr0, n_fit0 = len(W.tr), len(W.fit); e_tr0, e_fit0 = len(W.err(W.tr)), len(W.err(W.fit))      # the first test's own counts, before the labels are read back
    W.y = raw.y.to_numpy(float); W.dd = raw.dd.to_numpy(float); W.dn = raw.d.to_numpy(float)
    yr = W.yr.astype(int); tr = np.where(np.isin(yr, [2018, 2019]))[0]; te = np.where(yr == 2017)[0]; e = W.err(tr)
    assert len(tr) == 78296 and len(te) == 41014 and n_tr0 == 80071 and not np.isnan(W.y[tr]).any()
    SB = M.settings_B("W1"); CS = M.cb_settings("W1"); R = json.load(open(os.path.join(HERE, "followup", "promoted.json")))["rounds"]["W1"]
    assert (R["xgb_P_arm"], R["xgb_S_arm"]) == ("ratio", "base")
    k = len(tr) / n_tr0; sc = lambda r: int(math.ceil(r * k))      # the first test's test-fit round counts times 78,296 / 80,071
    nPr = sc(M.scale_rounds(R["xgb_P"], n_tr0, n_fit0)); nPb = sc(M.scale_rounds(R["xgb_base_P"], n_tr0, n_fit0)); nS = sc(M.scale_rounds(R["xgb_S"], e_tr0, e_fit0))
    cP = sc(CS["P_nr"]); cS = sc(CS["S_nr"])
    say(f"FY2017 backward: train {len(tr)} rows (FY2018-19), {len(e)} error rows; score {len(te)} rows. Rounds: xgboost P ratio {nPr}, P base {nPb}, size {nS}; CatBoost P {cP}, size {cS}")
    off = M.gamma_glm_offset(W, tr) if SB["S_off"] else None; o = M.cb_off(W, CS, tr)
    res = {k2: [] for k2 in ("p_xr", "p_xb", "s_x", "p_c", "s_c")}
    for seed in SEEDS:
        t0 = time.time()
        m, X = M.xgb_fit_P(W, SB, "ratio", tr, seed, nPr); res["p_xr"].append(m.predict(xgb.DMatrix(X[te])))
        m, X = M.xgb_fit_P(W, SB, "base", tr, seed, nPb); res["p_xb"].append(m.predict(xgb.DMatrix(X[te])))
        m, X = M.xgb_fit_S(W, SB, "base", e, off, seed, nS); res["s_x"].append(m.predict(xgb.DMatrix(X[te], base_margin=M.io(off, te))))
        t1 = time.time()
        m, pool = M.cb_fit_P(W, CS, "base", tr, seed, cP); res["p_c"].append(m.predict_proba(pool(te))[:, 1])
        res["s_c"].append(M.cb_fit_S(W, CS, e, o, seed, cS)[1](te))
        say(f"  seed {seed}: xgboost {t1 - t0:.0f} s, CatBoost {time.time() - t1:.0f} s; mean p xgboost {res['p_xr'][-1].mean():.4f}, CatBoost {res['p_c'][-1].mean():.4f}; "
            f"median size {np.median(res['s_x'][-1]):.0f}, {np.median(res['s_c'][-1]):.0f}")
    A = {k2: np.array(v) for k2, v in res.items()}
    for k2, v in A.items(): assert v.shape == (5, len(te)) and np.all(np.isfinite(v)) and np.all(v > 0)
    np.savez_compressed(out, st=W.st[te].astype(str), **A); say("fy2017_fits.npz written")

# ---------------- scoring ----------------
def arms_of(Px, Sx, Pc, Sc, Pb, Sb):
    i = np.arange(5); j = (i + 1) % 5; Pa, Sa = 0.5 * (Px[i] + Pc[i]), 0.5 * (Sx[i] + Sc[i]); Pr, Sr = 0.5 * (Px[i] + Px[j]), 0.5 * (Sx[i] + Sx[j])
    return {"blend": Pa * Sa ** B_EXP, "ref": Pr * Sr ** B_EXP, "blend_b1": Pa * Sa, "ref_b1": Pr * Sr, "twin": Pb * Sb}       # members x cases
STATS = ["dr5", "band", "dr10", "drm", "pooled10"]; PAIRS = [("blend", "ref"), ("blend_b1", "ref_b1"), ("blend", "twin")]
SPLITS = [(a, b) for a in itertools.combinations(range(5), 2) for b in itertools.combinations(sorted(set(range(5)) - set(a)), 2) if a < b]   # 15 pairs of disjoint half-bags
def one_year(label, fr, mem, seed):
    bag = {a: v.mean(axis=0) for a, v in mem.items()}; perm = {a: fr.perm(v) for a, v in bag.items()}
    val = {a: fr.stats(perm[a]) for a in bag}
    rng = np.random.default_rng(seed); boot = {p: np.empty((NB, len(STATS))) for p in PAIRS}
    for r in range(NB):
        c = fr.boot_counts(rng); s = {a: fr.stats(perm[a], c) for a in bag}
        for p in PAIRS: boot[p][r] = [s[p[0]][x] - s[p[1]][x] for x in STATS]
    # seed term on the difference: half-bags of two members; E[(half A - half B)^2] / 2 is a half-bag's variance, an upper bound for the full bag's
    seedv = {}
    for p in PAIRS:
        if p[1] == "twin": seedv[p] = np.zeros(len(STATS)); continue
        dd = []
        for a, b in SPLITS:
            h = [{x: fr.stats(fr.perm(mem[x][list(g)].mean(axis=0))) for x in p} for g in (a, b)]
            dd.append([(h[0][p[0]][x] - h[0][p[1]][x]) - (h[1][p[0]][x] - h[1][p[1]][x]) for x in STATS])
        seedv[p] = (np.array(dd) ** 2).mean(axis=0) / 2
    out = {"levels": {a: {x: val[a][x] for x in STATS} for a in bag}}
    for p in PAIRS:
        out["%s-%s" % p] = {x: dict(diff=val[p[0]][x] - val[p[1]][x], var_boot=float(boot[p][:, k].var(ddof=1)), var_seed=float(seedv[p][k])) for k, x in enumerate(STATS)}
    say(f"{label} scored: {fr.n} cases, {int(fr.hasw.sum())} states with a weight")
    return out
def combine(res, weights, pair, x):
    d = sum(weights[y] * res[y][pair][x]["diff"] for y in weights)
    se = math.sqrt(sum(weights[y] ** 2 * (res[y][pair][x]["var_boot"] + res[y][pair][x]["var_seed"]) for y in weights)); return 100 * d, 100 * se
def report(res, weights, order, title):
    lines = [title, ""]
    lines.append("Levels, percent of error dollars caught (map weights):")
    for y in order:
        lv = res[y]["levels"]; lines.append("  %-7s " % y + "   ".join("%s: at 5%% %.2f, at 10%% %.2f, mean %.2f" % (a, 100 * lv[a]["dr5"], 100 * lv[a]["dr10"], 100 * lv[a]["drm"]) for a in ("blend", "ref", "twin")))
    names = {"blend-ref": "blend minus matched two-fit xgboost (the contrast the rule reads)", "blend_b1-ref_b1": "the same with exponent 1", "blend-twin": "blend minus the B-recipe twin (5 fits, B inputs, no exponent)"}
    lab = {"dr5": "at 5%", "band": "5-10% band", "dr10": "at 10%", "drm": "mean of 5/7.5/10%", "pooled10": "sample-pooled at 10%"}
    for pair in ("blend-ref", "blend_b1-ref_b1", "blend-twin"):
        lines += ["", names[pair] + ": points (SE) z"]
        for x in STATS:
            d, se = combine(res, weights, pair, x)
            by = "   ".join("%s %+.2f (%.2f)" % (y, 100 * res[y][pair][x]["diff"], 100 * math.sqrt(res[y][pair][x]["var_boot"] + res[y][pair][x]["var_seed"])) for y in order)
            lines.append("  %-20s combined %+.2f (%.2f) z %+.1f   |   %s" % (lab[x], d, se, d / se, by))
    d10, se10 = combine(res, weights, "blend-ref", "dr10"); dm, sem = combine(res, weights, "blend-ref", "drm")
    verdict = "PROMISING" if (d10 >= 0.25 and dm > 0) else "POSITIVE BUT SMALL" if (d10 > 0 and dm > 0) else "NOT PROMISING"
    lines += ["", "READING by the written rule (at least +0.25 at 10%% and above zero on the mean share): %s" % verdict,
              "  blend minus matched xgboost: at 10%% %+.2f (SE %.2f); mean of shares %+.2f (SE %.2f); weights %s" % (d10, se10, dm, sem, json.dumps(weights))]
    return "\n".join(lines), verdict

def dry():
    res = {}; yr = {"W1": "FY2019", "W2": "FY2022", "W3": "FY2024"}
    for w in ("W1", "W2", "W3"):
        fr = L.test_frame(w); xp, xs = L.fits(L.SC2, "xgb_rev", w, SEEDS, fr); cp, cs = L.fits(L.SC2, "cb_rev", w, SEEDS, fr); ep, es = L.fits(L.SC2, "xgb_es", w, SEEDS, fr)
        mem = arms_of(xp, xs, cp, cs, ep, es)
        bl = np.array([pd.read_csv(os.path.join(L.SC2, "blend_%s_%d.csv" % (w, s))).ed.to_numpy() for s in SEEDS]); assert np.allclose(mem["blend"], bl, rtol=1e-9)   # the blend as scored in the run
        res[yr[w]] = one_year(yr[w], fr, mem, 4646 + int(w[1]))
    txt, _ = report(res, {"FY2019": 0.2, "FY2022": 0.4, "FY2024": 0.4}, ["FY2019", "FY2022", "FY2024"], "DRY RUN on the three spent test years (the machinery check; statistician: +0.74 at 10% with xgboost pairs i, i+5)")
    print(txt); open(os.path.join(OUT, "dry_run.txt"), "w").write(txt + "\n")

def score():
    assert not os.path.exists(os.path.join(OUT, "result.txt")), "result.txt exists: the validation years have been read once already"
    z = lambda f: np.load(os.path.join(L.HB, f + ".npz")); res = {}
    f18 = L.heldback_frame("W1"); assert f18.n == 39057
    res["FY2018"] = one_year("FY2018", f18, arms_of(z("xgb_W1_ratio")["p"], z("xgb_W1_base")["size"], z("cb_W1_base")["p"], z("cb_W1_base")["size"], z("xgb_W1_base")["p"], z("xgb_W1_base")["size"]), 5018)
    f23 = L.heldback_frame("W3"); assert f23.n == 40070
    res["FY2023"] = one_year("FY2023", f23, arms_of(z("xgb_W3_ratio_era")["p"], z("xgb_W3_era")["size"], z("cb_W3_era")["p"], z("cb_W3_era")["size"], z("xgb_W3_base")["p"], z("xgb_W3_base")["size"]), 5023)
    d = pd.read_csv(os.path.join(HERE, "data", "W1.csv"), usecols=["role", "yr", "st", "y", "d", "keep"], dtype={"role": str, "st": str, "yr": str}); d = d[d.yr == "2017"].reset_index(drop=True)
    A = np.load(os.path.join(OUT, "fy2017_fits.npz")); assert (d.role == "train").all() and len(d) == 41014 and (A["st"] == d.st.to_numpy()).all()
    f17 = L.Frame(d.st.to_numpy(), d.d.to_numpy(), d.keep.to_numpy() == 1, 2017, d.y.to_numpy())
    res["FY2017"] = one_year("FY2017", f17, arms_of(A["p_xr"], A["s_x"], A["p_c"], A["s_c"], A["p_xb"], A["s_x"]), 5017)
    W = {"FY2017": 0.25, "FY2018": 0.25, "FY2023": 0.5}
    txt, verdict = report(res, W, ["FY2023", "FY2018", "FY2017"], "BLEND VALIDATION: FY2023 and FY2018 (saved held-back predictions), FY2017 (backward, new fits)")
    print(txt); open(os.path.join(OUT, "result.txt"), "w").write(txt + "\n"); json.dump(res, open(os.path.join(OUT, "result.json"), "w"), indent=1); say(f"reading: {verdict}")

say(f"=== 08_blend_validation {MODE}"); {"dry": dry, "fit17": fit17, "score": score}[MODE](); say(f"=== {MODE} done")
