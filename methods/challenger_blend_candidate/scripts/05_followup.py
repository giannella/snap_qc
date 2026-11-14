# Follow-up run, Python stages (2026-11-13). See design_note_followup.md.
#   .venv\Scripts\python.exe -u 05_followup.py <stage> [--smoke]
# stages:  hb_xgb   held-back-year arms for xgboost at B's settings (no test-year label is read)
#          hb_cb    held-back-year arms for CatBoost at last night's settings
#          promote  applies the promotion rule written in the design note to the held-back criteria; writes promoted.json
#          test_xgb fits the promoted xgboost arms on all training years and scores the test years' cases
#          test_cb  the same for CatBoost, and writes the blend if it was promoted
# The loader, criteria and learner helpers are 02_fit.py's, taken verbatim (everything above its TUNERS table), so
# the test year's labels are blanked at load here too. Nothing in this file reads a test-year label.
import sys, os, time, json, math, types, hashlib
import numpy as np, pandas as pd
HERE = os.path.dirname(os.path.abspath(__file__))
STAGE = sys.argv[1]; SMOKE = "--smoke" in sys.argv
_src = open(os.path.join(HERE, "02_fit.py"), encoding="utf-8").read().split("\nTUNERS = ")[0]
F = types.ModuleType("fit02"); F.__file__ = os.path.join(HERE, "02_fit.py")
_argv = sys.argv; sys.argv = ["02_fit.py", "xgb_ref"] + (["--smoke"] if SMOKE else [])
exec(compile(_src, F.__file__, "exec"), F.__dict__); sys.argv = _argv
Win, wll, gamma_dev, gamma_glm_offset, xgb_par, scale_rounds, clampp, io, ex = (F.Win, F.wll, F.gamma_dev, F.gamma_glm_offset, F.xgb_par,
                                                                                 F.scale_rounds, F.clampp, F.io, F.ex)
NTH = F.NTH; REPO = "C:/Users/ericg/snap_qc_v27"
ROOT = os.path.join(HERE, "smoke" if SMOKE else "", "followup"); HB, SC, LOGS = (os.path.join(ROOT, x) for x in ("heldback", "scores", "logs"))
for p in (HB, SC, LOGS): os.makedirs(p, exist_ok=True)
logf = open(os.path.join(LOGS, f"05_followup_{STAGE}.log"), "a")
def say(msg):
    line = f"[{time.strftime('%m-%d %H:%M:%S')}] {msg}"; print(line, flush=True); logf.write(line + "\n"); logf.flush()
F.say = say
WINS = ["W1", "W2", "W3"]; DECIDE = {"W1": "FY2018", "W3": "FY2023"}      # promotion uses W1's and W3's held-back years only
HB_SEEDS = [117, 118] if SMOKE else [117, 118, 119, 120, 121]; MAXR = 300 if SMOKE else 4000
XGB_TEST_SEEDS = [117] if SMOKE else list(range(117, 127)); CB_TEST_SEEDS = [117] if SMOKE else list(range(117, 122))
TBAR = 2.0     # promotion: the seed-paired mean difference must be more than TBAR standard errors on the good side (lowered from 3 at review)

# ---------------- inputs and weights ----------------
def ratios(X):
    # eight columns built from the permitted inputs (principal data scientist's screen, reviews/scratch_principal_ds/common.py)
    X = X.copy(); g = X["gross_by_hh_size"]; e = X["earned_by_hh_size"]; u = X["unearned_by_hh_size"]
    nz = lambda a, b: np.where(b > 0, a / np.where(b > 0, b, 1), np.nan)
    X["r_earned_share"] = nz(e, e + u); X["r_shelter_gross"] = nz(X["shelter_expenses_by_hh_size"], g); X["r_ded_gross"] = nz(X["total_deductions_by_hh_size"], g)
    X["r_cap_gap"] = X["unc_rawben_rel_max"] - X["rawben_rel_max"]; X["r_net"] = g - X["total_deductions_by_hh_size"]; X["r_gross_total"] = g * X["HH_size_n"]
    X["r_med_any"] = (X["medical_deductions"] > 0).astype(float); X["r_zero_income"] = (g <= 0).astype(float)
    return X
def load(w):
    W = Win(w); yr = W.yr.astype(int)
    if SMOKE and len(set(yr[W.tr])) == 1:      # smoke only: one training year, split by thirds. Count rows from 1, as 03_fit_bart.R does, so both languages hold back the same rows
        k = (np.arange(len(W.tr)) + 1) % 3 == 0; W.fit, W.val = W.tr[~k], W.tr[k]
    # the post-pandemic era starts in FY2022. Smoke runs have no such year in their fit rows, so the last fit year stands in.
    W.post_from = 2022 if not SMOKE else int(yr[W.fit].max())
    W.post = yr >= W.post_from; W.has_era = bool(W.post[W.fit].any() and not W.post[W.fit].all())
    W.Xr = ratios(W.Xn); return W
def spec_of(arm):   # arm name -> (ratio inputs, era indicator, weight on post-pandemic rows)
    parts = arm.split("_"); wt = [int(p[1:]) for p in parts if p[0] == "w" and p[1:].isdigit()]
    return ("ratio" in parts, "era" in parts, wt[0] if wt else None)
def num_block(W, ratio, era):
    X = (W.Xr if ratio else W.Xn).to_numpy(float)
    return np.column_stack([X, W.post.astype(float)]) if era else X
def onehot(W): return np.column_stack([(W.st == s).astype(float) for s in W.states])
def design(W, ratio, era): return np.column_stack([num_block(W, ratio, era), onehot(W)])     # B's design: inputs, then one indicator per state
def era_wt(W, rows, k):
    if k is None: return None
    a = np.where(W.post[rows], float(k), 1.0); return a / a.mean()
def arms_for(W, learner):
    if learner == "xgb":
        a = ["base", "ratio"]
        if W.has_era: a += [f"{r}{e}" for r in ("", "ratio_") for e in ("w2", "w3", "w5", "era")]
    else:
        a = ["base", "reg", "ratio"] + (["w3", "era"] if W.has_era else [])
    return a

# ---------------- criteria on the held-back year ----------------
def pois_dev(mu, yv, g):
    # Poisson deviance of error dollars against the score, the score rescaled to each group's dollars (pois_dev in s46_lib.R)
    mu = mu.astype(float).copy()
    for ix in pd.Series(np.arange(len(g))).groupby(g).indices.values(): mu[ix] *= max(yv[ix].sum(), 1.0) / max(mu[ix].sum(), 1e-9)
    mu = np.maximum(mu, 1e-9); return 2 * np.mean(np.where(yv > 0, yv * np.log(np.where(yv > 0, yv, 1) / mu), 0.0) - (yv - mu))
def state_weights(year):
    try:
        iss = pd.read_csv(os.path.join(REPO, "additional_data", "fns_state_issuance_fy2024.csv"))
        per = pd.read_csv(os.path.join(REPO, "additional_data", f"fns_payment_error_rates_fy{year}.csv"))
        m = per.merge(iss, on="state"); m = m[~m.state.isin(["District of Columbia", "United States"])]
        ws = dict(zip(m.state, m.payment_error_rate / 100 * m.issuance_dollars)); return {k: v for k, v in ws.items() if np.isfinite(v)}
    except Exception: return None
class Val:
    # the held-back year's rows and the criteria read on them. Shares at 5% and 10% are printed and never used to choose.
    def __init__(self, W):
        v = W.val; self.v = v; self.y = W.y[v]; self.dd = W.dd[v]; self.dn = W.dn[v]; self.keep = W.keep[v]; self.st = W.st[v]; self.grp = W.grp[v]
        self.err = (self.y == 1) & (self.dd > 0); self.ws = state_weights(int(W.yr[v][0])); self.year = int(W.yr[v][0])
        self.groups = pd.Series(np.arange(len(v))).groupby(self.st).indices
    def wll(self, p): return wll(p, self.y, self.grp)
    def gdev(self, size): return gamma_dev(size[self.err], self.dd[self.err])
    def pdev(self, sc): k = self.keep; return pois_dev(sc[k], self.dn[k], self.st[k])
    def shares(self, sc):
        num = np.zeros(2); den = 0.0; caught = np.zeros(2); tot = 0.0
        for s, ix in self.groups.items():
            kk = np.maximum(1, np.rint(np.array([0.05, 0.10]) * len(ix)).astype(int)); i2 = ix[self.keep[ix]]
            if len(i2) == 0: continue
            o = i2[np.lexsort((i2, -sc[i2]))]; cs = np.cumsum(self.dn[o]); got = cs[np.minimum(kk, len(o)) - 1]; caught += got; tot += cs[-1]
            wgt = 1.0 if self.ws is None else self.ws.get(s)
            if wgt is not None and cs[-1] > 0: num += wgt * got / cs[-1]; den += wgt
        return dict(map5=100 * num[0] / den, map10=100 * num[1] / den, pooled5=100 * caught[0] / tot, pooled10=100 * caught[1] / tot)
def record(rows, w, learner, arm, V, P, S, bestP, bestS):
    # one row per seed and one for the bag, per criterion. W2's held-back year is FY2019, which the first test scores:
    # it gives W2 its rounds and nothing else, so no criterion or share is computed or printed for it.
    if w not in DECIDE:
        rows.append(dict(window=w, year=V.year, learner=learner, arm=arm, fit="bag", best_P=bestP, best_S=bestS))
        say(f"  {w} {learner} {arm:10s} rounds {bestP}/{bestS} (FY{V.year} criteria not computed: it is the first test's scored year)"); return
    for i in range(P.shape[0]):
        rows.append(dict(window=w, year=V.year, learner=learner, arm=arm, fit=str(i), wll=V.wll(P[i]), gdev=V.gdev(S[i]), pdev=V.pdev(P[i] * S[i]), best_P=bestP, best_S=bestS))
    bag = (P * S).mean(axis=0)
    rows.append(dict(window=w, year=V.year, learner=learner, arm=arm, fit="bag", wll=V.wll(P.mean(axis=0)), gdev=V.gdev(S.mean(axis=0)), pdev=V.pdev(bag),
                     best_P=bestP, best_S=bestS, **V.shares(bag)))
    r = rows[-1]; say(f"  {w} {learner} {arm:10s} bag wll {r['wll']:.5f}  size dev {r['gdev']:.5f}  dollar dev {r['pdev']:.4f}  rounds {bestP}/{bestS}  "
                      f"(printed only: map10 {r['map10']:.2f} pooled10 {r['pooled10']:.2f})")


# ---------------- xgboost at B's settings ----------------
def settings_B(w):
    s = pd.read_csv(os.path.join(HERE, "data", "settings_B.csv")); s = s[s.window == w].iloc[0]
    return dict(P_depth=int(s.P_max_depth), P_mcw=float(s.P_min_child_weight), P_col=float(s.P_colsample_bytree), S_depth=int(s.S_max_depth),
                S_mcw=float(s.S_min_child_weight), S_off=bool(s.S_offset))
def xgb_fit_P(W, SB, arm, rows, seed, rounds, evalrows=None):
    import xgboost as xgb
    ratio, era, k = spec_of(arm); X = design(W, ratio, era); par = xgb_par("binary:logistic", "logloss", seed, SB["P_depth"], SB["P_mcw"], SB["P_col"])
    d = xgb.DMatrix(X[rows], W.y[rows], weight=era_wt(W, rows, k))
    if evalrows is None: return xgb.train(par, d, rounds), X
    return xgb.train(par, d, rounds, evals=[(xgb.DMatrix(X[evalrows], W.y[evalrows]), "v")], early_stopping_rounds=100, verbose_eval=False), X
def xgb_fit_S(W, SB, arm, rows, off, seed, rounds, evalrows=None):
    import xgboost as xgb
    ratio, era, k = spec_of(arm); X = design(W, ratio, era); par = xgb_par("reg:gamma", "gamma-deviance", seed, SB["S_depth"], SB["S_mcw"])
    dm = lambda r, wt=None: xgb.DMatrix(X[r], W.dd[r], base_margin=io(off, r), weight=wt)
    if evalrows is None: return xgb.train(par, dm(rows, era_wt(W, rows, k)), rounds), X
    return xgb.train(par, dm(rows, era_wt(W, rows, k)), rounds, evals=[(dm(evalrows), "v")], early_stopping_rounds=100, verbose_eval=False), X
def hb_xgb():
    import xgboost as xgb
    for w in WINS:
        out = os.path.join(HB, f"criteria_xgb_{w}.csv")
        if os.path.exists(out): continue
        W = load(w); SB = settings_B(w); V = Val(W); f, v = W.fit, W.val; ef, ev = W.err(f), W.err(v); rows = []
        off = gamma_glm_offset(W, f) if SB["S_off"] else None
        say(f"{w} held-back year FY{V.year}: {len(f)} fit rows, {len(v)} read rows; arms {arms_for(W, 'xgb')}")
        for arm in arms_for(W, "xgb"):
            seeds = HB_SEEDS if w in DECIDE else HB_SEEDS[:1]        # W2: one fit, for the rounds
            t0 = time.time(); P = np.zeros((len(seeds), len(v))); S = np.zeros_like(P)
            m, X = xgb_fit_P(W, SB, arm, f, HB_SEEDS[0], MAXR, v); bP = m.best_iteration + 1
            P[0] = m.predict(xgb.DMatrix(X[v]), iteration_range=(0, bP))
            ms, Xs = xgb_fit_S(W, SB, arm, ef, off, HB_SEEDS[0], MAXR, ev); bS = ms.best_iteration + 1
            pv = lambda mod, it=None: mod.predict(xgb.DMatrix(Xs[v], base_margin=io(off, v)), **({} if it is None else {"iteration_range": (0, it)}))
            S[0] = pv(ms, bS)
            for i, sd in enumerate(seeds[1:], 1):             # the other seeds at the first seed's rounds
                P[i] = xgb_fit_P(W, SB, arm, f, sd, bP)[0].predict(xgb.DMatrix(X[v])); S[i] = pv(xgb_fit_S(W, SB, arm, ef, off, sd, bS)[0])
            np.savez_compressed(os.path.join(HB, f"xgb_{w}_{arm}.npz"), p=P, size=S); record(rows, w, "xgb", arm, V, P, S, bP, bS)
            say(f"    {arm}: {time.time() - t0:.0f} s")
        pd.DataFrame(rows).to_csv(out, index=False)

# ---------------- CatBoost at last night's settings ----------------
def cb_settings(w): return json.load(open(os.path.join(HERE, "tuning", f"catboost_{w}.json")))   # chosen last night on the held-back years
def cb_parts(arm): return "reg" in arm.split("_")
def cb_kw(c, seed, its, reg):
    k = F.cb_kw(c, seed, its)
    if reg: k.update(l2_leaf_reg=10, rsm=0.5)
    return k
def cb_X(W, ratio, era):
    X = (W.Xr if ratio else W.Xn).copy()
    if era: X["era"] = W.post.astype(float)
    return X.assign(st=W.st)
def cb_fit_P(W, CS, arm, rows, seed, rounds, evalrows=None):
    import catboost as cb
    ratio, era, k = spec_of(arm); X = cb_X(W, ratio, era); pool = lambda r, lab=None, wt=None: cb.Pool(X.iloc[r], lab, cat_features=["st"], weight=wt)
    if evalrows is None:
        return cb.CatBoostClassifier(loss_function="Logloss", **cb_kw(CS["P"], seed, rounds, cb_parts(arm))).fit(pool(rows, W.y[rows], era_wt(W, rows, k))), pool
    m = cb.CatBoostClassifier(loss_function="Logloss", od_type="Iter", od_wait=100, use_best_model=True, **cb_kw(CS["P"], seed, rounds, cb_parts(arm)))
    m.fit(pool(rows, W.y[rows], era_wt(W, rows, k)), eval_set=pool(evalrows, W.y[evalrows])); return m, pool
def cb_off(W, CS, ref):   # last night's size model: the lben offset where chosen, otherwise the log of the mean error size
    return gamma_glm_offset(W, ref) if CS["S"]["off"] else np.full(len(W.y), math.log(W.dd[W.err(ref)].mean()))
def cb_fit_S(W, CS, rows, o, seed, rounds, evalrows=None):
    import catboost as cb
    X = W.X_str(); pool = lambda r, lab=None, base=None: cb.Pool(X.iloc[r], lab, cat_features=["st"], baseline=base)
    kw = F.cb_kw(CS["S"], seed, rounds)
    if evalrows is None: m = cb.CatBoostRegressor(loss_function="Tweedie:variance_power=1.9", **kw).fit(pool(rows, W.dd[rows], o[rows]))
    else:
        m = cb.CatBoostRegressor(loss_function="Tweedie:variance_power=1.9", od_type="Iter", od_wait=100, use_best_model=True, **kw)
        m.fit(pool(rows, W.dd[rows], o[rows]), eval_set=pool(evalrows, W.dd[evalrows], o[evalrows]))
    return m, (lambda r: np.exp(m.predict(pool(r), prediction_type="RawFormulaVal") + o[r]))    # as cb_score in 02_fit.py: the pool scored carries no baseline
def hb_cb():
    for w in WINS:
        out = os.path.join(HB, f"criteria_cb_{w}.csv")
        if os.path.exists(out): continue
        W = load(w); CS = cb_settings(w); V = Val(W); f, v = W.fit, W.val; ef, ev = W.err(f), W.err(v); rows = []
        seeds = HB_SEEDS if w in DECIDE else HB_SEEDS[:1]        # W2's held-back year enters no promotion decision: one fit, for the rounds
        say(f"{w} held-back year FY{V.year}: CatBoost {CS['P']} / {CS['S']}; arms {arms_for(W, 'cb')}; {len(seeds)} fits each")
        o = cb_off(W, CS, f); S = np.zeros((len(seeds), len(v)))
        ms, pr = cb_fit_S(W, CS, ef, o, seeds[0], MAXR, ev); bS = ms.get_best_iteration() + 1; S[0] = pr(v)
        for i, sd in enumerate(seeds[1:], 1): S[i] = cb_fit_S(W, CS, ef, o, sd, bS)[1](v)
        for arm in arms_for(W, "cb"):
            t0 = time.time(); P = np.zeros((len(seeds), len(v)))
            m, pool = cb_fit_P(W, CS, arm, f, seeds[0], MAXR, v); bP = m.get_best_iteration() + 1; P[0] = m.predict_proba(pool(v))[:, 1]
            for i, sd in enumerate(seeds[1:], 1):
                m2, pool2 = cb_fit_P(W, CS, arm, f, sd, bP); P[i] = m2.predict_proba(pool2(v))[:, 1]
            np.savez_compressed(os.path.join(HB, f"cb_{w}_{arm}.npz"), p=P, size=S); record(rows, w, "cb", arm, V, P, S, bP, bS)
            say(f"    {arm}: {time.time() - t0:.0f} s")
        pd.DataFrame(rows).to_csv(out, index=False)

# ---------------- promotion: the rule in the design note, applied by code ----------------
def tstat(d):
    d = np.asarray(d, float); n = len(d); m = d.mean()
    if n < 2: return m, float("nan")
    se = d.std(ddof=1) / math.sqrt(n); return m, (m / se if se > 0 else (-np.inf if m < 0 else np.inf))
def better(crit, learner, arm, ref, col, need=("W1", "W3")):
    # arm against ref on criterion `col`, seed-paired, in each window of `need` (the era arms exist in W3 only).
    # Promoted when the mean difference is below zero by more than TBAR standard errors in every one of them.
    out = {}; ok = True
    for w in need:
        c = crit[(crit.learner == learner) & (crit.window == w) & (crit.fit != "bag")]
        a = c[c.arm == arm].sort_values("fit")[col].to_numpy(); r = c[c.arm == ref].sort_values("fit")[col].to_numpy()
        if len(a) == 0 or len(a) != len(r): continue
        m, t = tstat(a - r); out[DECIDE[w]] = dict(mean_diff=m, t=t, n=len(a)); ok = ok and len(a) > 1 and m < 0 and t < -TBAR
    return bool(ok and len(out) == len(need)), out
def qpois_b(P, S, V):
    # quasi-Poisson regression of error dollars on log P and log size with state intercepts, kept held-back cases
    # (senior statistician's 04_structure_probe.py). Returns the size exponent relative to P and its standard error.
    k = V.keep; yv = V.dn[k]; st = V.st[k]; lp = np.log(clampp(P[k])); ls = np.log(S[k]); states = sorted(set(st))
    Z = np.column_stack([lp, ls] + [(st == s).astype(float) for s in states]); beta = np.zeros(Z.shape[1]); beta[:2] = 1.0; beta[2:] = np.log(yv.mean()) - (lp + ls).mean()
    for _ in range(100):
        eta = Z @ beta; mu = np.exp(np.clip(eta, -30, 30)); zw = eta + (yv - mu) / mu
        nb = np.linalg.solve(Z.T @ (Z * mu[:, None]) + 1e-9 * np.eye(Z.shape[1]), Z.T @ (mu * zw)); done = np.max(np.abs(nb - beta)) < 1e-8; beta = nb
        if done: break
    mu = np.exp(Z @ beta); A = np.linalg.inv(Z.T @ (Z * mu[:, None]) + 1e-9 * np.eye(Z.shape[1])); Vc = A @ (Z.T @ (Z * ((yv - mu) ** 2)[:, None])) @ A
    g = np.array([-beta[1] / beta[0] ** 2, 1 / beta[0]]); return float(beta[1] / beta[0]), float(np.sqrt(g @ Vc[:2, :2] @ g)), float(beta[0])
def exponent(PS, VALS, label):
    # PS[w] = (P, S) per fit on the held-back year, for W1 and W3. b is estimated on each year from the bag; it is
    # promoted when the OTHER year's b lowers the dollar deviance of every-seed scores in both years (seed-paired).
    est = {w: qpois_b(PS[w][0].mean(axis=0), PS[w][1].mean(axis=0), VALS[w]) for w in PS}
    if len(est) < 2: return dict(promoted=False, note="needs both deciding years")
    ws = list(PS); other = {ws[0]: ws[1], ws[1]: ws[0]}; chk = {}; ok = True
    for w in ws:
        b = est[other[w]][0]; P, S = PS[w]; V = VALS[w]
        m, t = tstat([V.pdev(P[i] * S[i] ** b) - V.pdev(P[i] * S[i]) for i in range(P.shape[0])])
        chk[DECIDE[w]] = dict(b_from_other_year=b, mean_diff=m, t=t); ok = ok and m < 0 and t < -TBAR
    wt = np.array([1 / est[w][1] ** 2 for w in ws]); b = float(sum(wt[i] * est[w][0] for i, w in enumerate(ws)) / wt.sum())
    say(f"  exponent, {label}: " + "; ".join(f"{DECIDE[w]} b {est[w][0]:.3f} (se {est[w][1]:.3f}, log P coefficient {est[w][2]:.3f})" for w in ws) +
        f"; pooled {b:.3f}; cross-year check " + "; ".join(f"{y} diff {c['mean_diff']:+.4f} t {c['t']:+.1f}" for y, c in chk.items()) + f" -> {'PROMOTED' if ok else 'not promoted'}")
    return dict(promoted=bool(ok), b=b, by_year={DECIDE[w]: dict(b=est[w][0], se=est[w][1]) for w in ws}, cross_year=chk)
def promote():
    crit = pd.concat([pd.read_csv(os.path.join(HB, f)) for f in sorted(os.listdir(HB)) if f.startswith("criteria_") and f.endswith(".csv")], ignore_index=True)
    crit["fit"] = crit["fit"].astype(str); R = dict(rule=f"seed-paired mean difference below zero by more than {TBAR} standard errors in FY2018 and FY2023", made=time.strftime("%Y-%m-%d %H:%M:%S"))
    VALS = {w: Val(load(w)) for w in DECIDE}
    npz = lambda learner, w, arm: np.load(os.path.join(HB, f"{learner}_{w}_{arm}.npz"))
    # ---- xgboost: ratio inputs, then the era form, separately for the P model (log loss) and the size model (gamma deviance)
    X = {}
    for model, col in (("P", "wll"), ("S", "gdev")):
        ok, ev = better(crit, "xgb", "ratio", "base", col)
        c3 = crit[(crit.learner == "xgb") & (crit.window == "W3") & (crit.fit != "bag")]; forms = [a for a in ("w2", "w3", "w5", "era") if (c3.arm == a).any()]; mean3 = lambda a: c3[c3.arm == a][col].mean()
        best = min(forms, key=mean3) if forms else None
        ok2, ev2 = better(crit, "xgb", best, "base", col, need=("W3",)) if best else (False, {})
        # both promoted: the era form is stacked on the ratio inputs unless the stacked arm is worse than the ratio inputs alone
        stack = bool(ok and ok2 and (c3.arm == "ratio_" + best).any() and mean3("ratio_" + best) <= mean3("ratio"))
        arm3 = ("ratio_" + best) if stack else "ratio" if ok else best if ok2 else "base"
        X[model] = dict(ratio=ok, ratio_evidence=ev, era=best if (ok2 and (stack or not ok)) else None, era_best_form=best, era_evidence=ev2,
                        era_passed_alone=ok2, stacked_on_ratio=stack, arm_W3=arm3, arm_other="ratio" if ok else "base")
        say(f"xgboost {model} model: ratio inputs {'PROMOTED' if ok else 'not promoted'} {json.dumps(ev)}; era form {best} {'PROMOTED' if ok2 else 'not promoted'} {json.dumps(ev2)}; FY2024 test arm {arm3}")
    arm_of = lambda model, w: X[model]["arm_W3"] if w == "W3" else X[model]["arm_other"]
    PSx = {w: (npz("xgb", w, arm_of("P", w))["p"], npz("xgb", w, arm_of("S", w))["size"]) for w in DECIDE}
    X["exponent"] = exponent(PSx, VALS, "xgboost"); R["xgb"] = X
    # ---- CatBoost: regularisation, ratio inputs, era form, on the P model
    C = {}; PSc = None
    if all(os.path.exists(os.path.join(HB, f"criteria_cb_{w}.csv")) for w in WINS):
        okr, evr = better(crit, "cb", "reg", "base", "wll"); okq, evq = better(crit, "cb", "ratio", "base", "wll")
        c3 = crit[(crit.learner == "cb") & (crit.window == "W3") & (crit.fit != "bag")]; forms = [a for a in ("w3", "era") if (c3.arm == a).any()]
        best = min(forms, key=lambda a: c3[c3.arm == a].wll.mean()) if forms else None; oke, eve = better(crit, "cb", best, "base", "wll", need=("W3",)) if best else (False, {})
        C = dict(reg=okr, reg_evidence=evr, ratio=okq, ratio_evidence=evq, era=best if oke else None, era_best_form=best, era_evidence=eve)
        say(f"CatBoost P model: regularisation {'PROMOTED' if okr else 'not promoted'} {json.dumps(evr)}; ratio inputs {'PROMOTED' if okq else 'not promoted'} {json.dumps(evq)}; "
            f"era form {best} {'PROMOTED' if oke else 'not promoted'} {json.dumps(eve)}")
        # the exponent and the blend are judged on the best single promoted CatBoost arm that was fitted on both years
        cand = [a for a, ok in (("reg", okr), ("ratio", okq)) if ok]; one = min(cand, key=lambda a: crit[(crit.learner == "cb") & (crit.arm == a) & (crit.fit != "bag") & crit.window.isin(list(DECIDE))].wll.mean()) if cand else "base"
        PSc = {w: (npz("cb", w, one)["p"], npz("cb", w, one)["size"]) for w in DECIDE}; C["exponent"] = exponent(PSc, VALS, f"CatBoost ({one})")
        C["any_fit_change"] = bool(okr or okq or oke)
        # ---- blend: P and size each averaged over one xgboost fit and one CatBoost fit, scored with xgboost's exponent.
        # The reference is the same average over two xgboost fits, so the blend is not credited for averaging alone.
        bx = X["exponent"]["b"] if X["exponent"]["promoted"] else 1.0; ev = {}; ok = True
        for w in DECIDE:
            V = VALS[w]; (px, sx), (pc, sc) = PSx[w], PSc[w]; n = min(px.shape[0], pc.shape[0]); d = []
            for i in range(n):
                j = (i + 1) % n; d.append(V.pdev(0.5 * (px[i] + pc[i]) * (0.5 * (sx[i] + sc[i])) ** bx) - V.pdev(0.5 * (px[i] + px[j]) * (0.5 * (sx[i] + sx[j])) ** bx))
            m, t = tstat(d); ev[DECIDE[w]] = dict(mean_diff=m, t=t, n=n); ok = ok and m < 0 and t < -TBAR
        R["blend"] = dict(promoted=bool(ok), evidence=ev); say(f"blend of xgboost and CatBoost (against two xgboost fits): {'PROMOTED' if ok else 'not promoted'} {json.dumps(ev)}")
    else: say("CatBoost held-back criteria incomplete: nothing promoted for CatBoost, no blend")
    R["cb"] = C
    # ---- BART: k chosen on the two deciding years, then its exponent (held-back fits written by 06_bart_proper.R)
    B = {}; need = [f"bart_hb_{w}_k{k}.csv" for w in DECIDE for k in (1, 2, 3)] + [f"bart_hb_{w}_size.csv" for w in DECIDE]
    if all(os.path.exists(os.path.join(HB, f)) for f in need):
        ks = sorted({int(f.split("_k")[1].split(".")[0]) for f in os.listdir(HB) if f.startswith("bart_hb_W1_k")}); tot = {}; PSb = {}
        for k in ks:
            tot[k] = 0.0
            for w in DECIDE:
                d = pd.read_csv(os.path.join(HB, f"bart_hb_{w}_k{k}.csv")); V = VALS[w]; assert len(d) == len(V.st) and (d.st.to_numpy() == V.st).all()
                tot[k] += V.wll(d.filter(like="p_c").to_numpy().mean(axis=1))
        kbest = min(tot, key=tot.get)
        for w in DECIDE:
            d = pd.read_csv(os.path.join(HB, f"bart_hb_{w}_k{kbest}.csv")); s = pd.read_csv(os.path.join(HB, f"bart_hb_{w}_size.csv"))
            PSb[w] = (d.filter(like="p_c").to_numpy().T, s.filter(like="s_c").to_numpy().T)
        B = dict(k=kbest, wll_sum_by_k={str(k): v for k, v in tot.items()}, exponent=exponent(PSb, VALS, f"BART (k {kbest})"))
        say(f"BART: k {kbest} chosen; summed held-back log loss by k {json.dumps(B['wll_sum_by_k'])}")
    else: say("BART held-back stage incomplete (files missing): BART keeps k 2 and no exponent"); B = dict(k=2, exponent=dict(promoted=False), note="held-back stage incomplete")
    R["bart"] = B
    pd.DataFrame([dict(k=B["k"], b=B["exponent"]["b"] if B["exponent"].get("promoted") else 1.0)]).to_csv(os.path.join(ROOT, "bart_choice.csv"), index=False)
    # rounds for every promoted arm, per window, from that window's own held-back year
    rounds = {}
    for w in WINS:
        c = crit[(crit.window == w) & (crit.fit == "bag")]; g = lambda learner, arm, col: int(c[(c.learner == learner) & (c.arm == arm)][col].iloc[0])
        rounds[w] = dict(xgb_base_P=g("xgb", "base", "best_P"), xgb_base_S=g("xgb", "base", "best_S"), xgb_P=g("xgb", arm_of("P", w), "best_P"), xgb_S=g("xgb", arm_of("S", w), "best_S"),
                         xgb_P_arm=arm_of("P", w), xgb_S_arm=arm_of("S", w))
    R["rounds"] = rounds
    txt = json.dumps(R, indent=1, default=lambda o: o.item() if hasattr(o, "item") else str(o)); open(os.path.join(ROOT, "promoted.json"), "w").write(txt)
    say(f"promoted.json written, sha256 {hashlib.sha256(txt.encode()).hexdigest()[:16]}")

# ---------------- test-year fits (promoted arms only; inputs of the test year, never its labels) ----------------
def write(name, w, seed, st, p, size, b=1.0):
    ed = p * size ** b; assert np.all(np.isfinite(ed)) and np.all(size > 0) and np.all((p >= 0) & (p <= 1))
    pd.DataFrame({"st": st, "p": p, "size": size, "ed": ed}).to_csv(os.path.join(SC, f"{name}_{w}_{seed}.csv"), index=False)
def have(name, w, seeds): return all(os.path.exists(os.path.join(SC, f"{name}_{w}_{s}.csv")) for s in seeds)
def test_xgb():
    import xgboost as xgb
    R = json.load(open(os.path.join(ROOT, "promoted.json"))); X = R["xgb"]; b = X["exponent"]["b"] if X["exponent"]["promoted"] else 1.0
    piece = bool(X["P"]["ratio"] or X["S"]["ratio"] or X["P"]["era"] or X["S"]["era"])      # a promoted change to inputs or weights
    # arms written:  xgb_es      B's inputs and settings, rounds by early stopping on the held-back year (the twin)
    #                xgb_rev     every promoted piece: the primary arm (written when anything was promoted)
    #                xgb_rev_b1  the same fits without the exponent (when an input or weight piece and the exponent were both promoted)
    #                xgb_exp     the exponent alone on B's inputs (same condition)
    for w in WINS:
        W = load(w); SB = settings_B(w); tr, te = W.tr, W.te; e = W.err(tr); off = gamma_glm_offset(W, tr) if SB["S_off"] else None; rd = R["rounds"][w]
        nP = lambda best: scale_rounds(best, len(tr), len(W.fit)); nS = lambda best: scale_rounds(best, len(e), len(W.err(W.fit)))
        def fit(Parm, Sarm, bestP, bestS, seed):
            m, Xp = xgb_fit_P(W, SB, Parm, tr, seed, nP(bestP)); ms, Xs = xgb_fit_S(W, SB, Sarm, e, off, seed, nS(bestS))
            return m.predict(xgb.DMatrix(Xp[te])), ms.predict(xgb.DMatrix(Xs[te], base_margin=io(off, te)))
        Prev, Srev = rd["xgb_P_arm"], rd["xgb_S_arm"]; same = (Prev, Srev) == ("base", "base")     # in this window the promoted fit is B's own design
        outs_es = [("xgb_es", 1.0)] + ([("xgb_exp", b)] if piece and b != 1.0 else []) +                   (([("xgb_rev", b)] if (piece or b != 1.0) else []) + ([("xgb_rev_b1", 1.0)] if piece and b != 1.0 else []) if same else [])
        outs_rev = [] if same else [("xgb_rev", b)] + ([("xgb_rev_b1", 1.0)] if b != 1.0 else [])
        say(f"{w}: P arm {Prev}, size arm {Srev}, exponent {b:.3f}; rounds {json.dumps(rd)}")
        for label, Parm, Sarm, bP, bS, outs in (("es", "base", "base", rd["xgb_base_P"], rd["xgb_base_S"], outs_es), ("rev", Prev, Srev, rd["xgb_P"], rd["xgb_S"], outs_rev)):
            if not outs: continue
            for seed in XGB_TEST_SEEDS:
                if all(have(n, w, [seed]) for n, _ in outs): continue
                t0 = time.time(); p, size = fit(Parm, Sarm, bP, bS, seed)
                for n, bb in outs: write(n, w, seed, W.st[te], p, size, bb)
                say(f"  {w} {label} fit seed {seed} -> {[n for n, _ in outs]}: {time.time() - t0:.0f} s, mean p {p.mean():.4f}, median size {np.median(size):.0f}")
def test_cb():
    R = json.load(open(os.path.join(ROOT, "promoted.json"))); C = R.get("cb", {}); X = R["xgb"]
    bc = C["exponent"]["b"] if C.get("exponent", {}).get("promoted") else 1.0; last = os.path.join(HERE, "smoke" if SMOKE else "", "scores")
    if C.get("any_fit_change"):
        arm = "_".join([x for x, ok in (("reg", C["reg"]), ("ratio", C["ratio"]), (C["era"] or "", C["era"] is not None)) if ok])
        for w in WINS:
            W = load(w); CS = cb_settings(w); tr, te, f, v = W.tr, W.te, W.fit, W.val; e = W.err(tr)
            warm = arm if W.has_era else "_".join([x for x in arm.split("_") if x in ("reg", "ratio")]) or "base"
            if have("cb_rev", w, CB_TEST_SEEDS): continue
            m, pool = cb_fit_P(W, CS, warm, f, CB_TEST_SEEDS[0], MAXR, v); nr = scale_rounds(m.get_best_iteration() + 1, len(tr), len(f))   # rounds for the stacked arm, on the held-back year
            say(f"{w} cb_rev arm {warm}: held-back log loss {wll(m.predict_proba(pool(v))[:, 1], W.y[v], W.grp[v]):.5f}, {nr} rounds")
            o = cb_off(W, CS, tr)
            for seed in CB_TEST_SEEDS:
                t0 = time.time(); m2, pool2 = cb_fit_P(W, CS, warm, tr, seed, nr); p = m2.predict_proba(pool2(te))[:, 1]
                size = cb_fit_S(W, CS, e, o, seed, CS["S_nr"])[1](te); write("cb_rev", w, seed, W.st[te], p, size, bc)
                say(f"  {w} cb_rev seed {seed}: {(time.time() - t0) / 60:.1f} min, mean p {p.mean():.4f}")
    elif bc != 1.0:      # the exponent alone: last night's CatBoost fits, rescored
        for w in WINS:
            for seed in CB_TEST_SEEDS:
                d = pd.read_csv(os.path.join(last, f"catboost_{w}_{seed}.csv")); write("cb_rev", w, seed, d.st.to_numpy(), d.p.to_numpy(), d["size"].to_numpy(), bc)
        say(f"cb_rev: last night's CatBoost fits with exponent {bc:.3f}")
    else: say("CatBoost: nothing promoted; last night's scores stand")
    if R.get("blend", {}).get("promoted"):
        bx = X["exponent"]["b"] if X["exponent"]["promoted"] else 1.0
        xname = "xgb_rev" if have("xgb_rev", "W1", CB_TEST_SEEDS) else "xgb_es"; cname, cdir = ("cb_rev", SC) if have("cb_rev", "W1", CB_TEST_SEEDS) else ("catboost", last)
        for w in WINS:
            for seed in CB_TEST_SEEDS:
                a = pd.read_csv(os.path.join(SC, f"{xname}_{w}_{seed}.csv")); c = pd.read_csv(os.path.join(cdir, f"{cname}_{w}_{seed}.csv")); assert (a.st == c.st).all()
                write("blend", w, seed, a.st.to_numpy(), 0.5 * (a.p + c.p).to_numpy(), 0.5 * (a["size"] + c["size"]).to_numpy(), bx)
        say(f"blend written: P and size averaged over {xname} and {cname}, fit by fit, exponent {bx:.3f}")

say(f"=== 05_followup {STAGE}{' (smoke)' if SMOKE else ''}")
{"hb_xgb": hb_xgb, "hb_cb": hb_cb, "promote": promote, "test_xgb": test_xgb, "test_cb": test_cb}[STAGE]()
say(f"=== {STAGE} done")
