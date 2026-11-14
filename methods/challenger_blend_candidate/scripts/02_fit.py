# Challenger benchmark, step 2: fit the Python learners (2026-11-12). See design_note.md.
#   .venv\Scripts\python.exe -u 02_fit.py <learner> [windows, e.g. W1,W2,W3] [--smoke]
# learners: xgb_ref | xgb_tuned | lgbm | ebm_main | ebm_pairs | catboost | tabnet
# Every learner scores a case as P(error over tolerance) x error size, B's structure (findings 46). P is fitted on
# all training rows; size on the training error rows, to error dollars deflated by year (dd). Settings are chosen
# on the last training year held back (B's "holdout" method), never on the test year. The test year's labels are
# not read in this file: only its inputs are, to write a score per test case into scores/.
import sys, os, time, json, math, hashlib, warnings
import numpy as np, pandas as pd
warnings.filterwarnings("ignore")
HERE = os.path.dirname(os.path.abspath(__file__))
DATA, OUT, TUNE, LOGS = (os.path.join(HERE, x) for x in ("data", "scores", "tuning", "logs"))
LEARNER = sys.argv[1]
WINS = sys.argv[2].split(",") if len(sys.argv) > 2 and not sys.argv[2].startswith("--") else ["W1", "W2", "W3"]
SMOKE = "--smoke" in sys.argv
if SMOKE: OUT, TUNE = os.path.join(HERE, "smoke", "scores"), os.path.join(HERE, "smoke", "tuning")
for p in (OUT, TUNE, LOGS): os.makedirs(p, exist_ok=True)
NTH = 16
BUD = [0.02, 0.03, 0.05, 0.075, 0.10]
META = ["role", "yr", "st", "y", "d", "dd", "lben", "keep"]
SEEDS = {"xgb_ref": range(117, 127), "xgb_tuned": range(117, 127), "lgbm": range(117, 127), "catboost": range(117, 122),
         "ebm_main": range(117, 120), "ebm_pairs": range(117, 120), "tabnet": range(117, 120)}[LEARNER]
if SMOKE: SEEDS = [117]
TUNE_SEED = 117
logf = open(os.path.join(LOGS, f"02_fit_{LEARNER}{'_smoke' if SMOKE else ''}.log"), "a")
def say(msg):
    line = f"[{time.strftime('%m-%d %H:%M:%S')}] {msg}"; print(line, flush=True); logf.write(line + "\n"); logf.flush()

# ---------------- data ----------------
class Win:
    def __init__(self, w):
        d = pd.read_csv(os.path.join(DATA, f"{w}.csv"), dtype={"role": str, "yr": str, "st": str}, float_precision="round_trip")
        H = pd.read_csv(os.path.join(DATA, f"{w}_colhash.csv"))     # every numeric column must equal the exported doubles bit for bit
        for c, h in zip(H["column"], H["sha256"]):
            x = d[c].to_numpy(np.float64); x = np.where(np.isnan(x), -9999.5, x)
            assert hashlib.sha256(np.ascontiguousarray(x, "<f8").tobytes()).hexdigest() == h, f"{w}: column {c} differs from the exported values"
        for c in ("y", "d", "dd"): d[c] = d[c].astype(float)
        if not SMOKE: d.loc[d.role == "test", ["y", "d", "dd"]] = np.nan   # the test year's labels are blanked at load
        if SMOKE:   # training years only, every 4th row; the last training year stands in for the test year. No real test-year row is read.
            d = d[d.role == "train"].iloc[::4].reset_index(drop=True)
            d["role"] = np.where(d.yr == d.yr.max(), "test", "train")
            d = pd.concat([d[d.role == "train"], d[d.role == "test"]]).reset_index(drop=True)
        self.w, self.d = w, d
        self.feats = [c for c in d.columns if c not in META]
        self.states = sorted(d.st.unique())
        self.tr = np.where(d.role == "train")[0]; self.te = np.where(d.role == "test")[0]
        yr = d.yr.to_numpy(); last = max(yr[self.tr])
        self.fit = self.tr[yr[self.tr] != last]; self.val = self.tr[yr[self.tr] == last]
        if len(self.fit) == 0:   # smoke only: one training year left, so hold back every third row of it
            assert SMOKE; k = np.arange(len(self.tr)) % 3 == 0; self.fit, self.val = self.tr[~k], self.tr[k]
        self.y = d.y.to_numpy().astype(float); self.dd = d.dd.to_numpy(); self.dn = d.d.to_numpy(); self.lben = d.lben.to_numpy()
        self.keep = d.keep.to_numpy().astype(bool); self.st = d.st.to_numpy(); self.yr = yr
        self.grp = np.char.add(np.char.add(self.st.astype(str), " "), yr.astype(str))
        self.Xn = d[self.feats]                                   # numeric inputs, NaN kept
        self.stcat = pd.Categorical(d.st, categories=self.states)
    def err(self, rows): return rows[(self.y[rows] == 1) & (self.dd[rows] > 0)]
    def X_onehot(self):                                           # B's design: inputs, then one indicator per state
        S = pd.DataFrame({"st_" + s: (self.st == s).astype(float) for s in self.states})
        return np.column_stack([self.Xn.to_numpy(float), S.to_numpy(float)])
    def X_cat(self): return self.Xn.assign(st=self.stcat)        # state as a pandas categorical
    def X_str(self): return self.Xn.assign(st=self.st)           # state as a string column

def gamma_glm_offset(W, ref):
    # gamma regression with log link of error size on lben, fitted on the error rows of `ref` (B's glm_gamma_off_dir)
    e = W.err(ref); X = np.column_stack([np.ones(len(e)), W.lben[e]]); yv = W.dd[e]; beta = np.array([math.log(yv.mean()), 0.0])
    for _ in range(200):
        eta = X @ beta; mu = np.exp(eta); nb = np.linalg.lstsq(X, eta + (yv - mu) / mu, rcond=None)[0]
        done = np.max(np.abs(nb - beta)) < 1e-10; beta = nb
        if done: break
    return beta[0] + beta[1] * W.lben                              # offset for every row of the window

# ---------------- criteria (B's, s46_lib.R) ----------------
def clampp(p): return np.clip(p, 1e-6, 1 - 1e-6)
def wll(p, y, g):
    # within state-year log loss: one intercept shift per group fitted on the validation rows
    z = np.log(clampp(p)) - np.log1p(-clampp(p)); tot = 0.0
    for ix in pd.Series(np.arange(len(g))).groupby(g).indices.values():
        yy, zz = y[ix], z[ix]; s = yy.sum()
        if s == 0 or s == len(ix): continue                       # the fitted shift drives this group's loss to 0
        a = 0.0
        for _ in range(60):
            pr = 1 / (1 + np.exp(-(zz + a))); step = (yy - pr).sum() / max((pr * (1 - pr)).sum(), 1e-12); a += step
            if abs(step) < 1e-10: break
        zz = zz + a; tot -= (yy * -np.logaddexp(0, -zz) + (1 - yy) * -np.logaddexp(0, zz)).sum()
    return tot / len(y)
def gamma_dev(mu, yv): mu = np.maximum(mu, 1e-6); return 2 * np.mean(-np.log(yv / mu) + (yv - mu) / mu)
def lift(sc, dv, keep, st):
    # mean over budgets of the mean state's $/flag, over mean $ per kept case (lift_of in s46_lib.R)
    out = []
    for b in BUD:
        per = []
        for ix in pd.Series(np.arange(len(st))).groupby(st).indices.values():
            kk = max(1, int(np.rint(b * len(ix)))); i2 = ix[keep[ix]]
            sel = i2[np.lexsort((i2, -sc[i2]))[:min(kk, len(i2))]]; per.append(dv[sel].sum() / kk)
        out.append(np.mean(per))
    return np.mean(out) / dv[keep].mean()
def val_lift(W, sc): return lift(sc, W.dn[W.val], W.keep[W.val], W.st[W.val])
def scale_rounds(best, n_all, n_fit): return int(math.ceil(best * n_all / n_fit))
def io(o, r): return None if o is None else o[r]                 # an offset on rows r, or none
def ex(o, r): return 1.0 if o is None else np.exp(o[r])          # its multiplier on a log-link prediction

# ---------------- learners ----------------
# Each learner gives tune(W) -> settings (a dict, saved as JSON with the held-back-year diagnostics) and
# score(W, settings, seed) -> (p, size) for the test rows.

# ---- xgboost with B's frozen settings: a check of this harness, not a challenger ----
def xgb_par(obj, metric, seed, depth, mcw, colsample=None):
    p = dict(objective=obj, eval_metric=metric, nthread=NTH, seed=seed, eta=0.03, subsample=0.8, reg_lambda=5, max_depth=int(depth), min_child_weight=mcw)
    if colsample is not None: p["colsample_bytree"] = colsample
    return p
def xgb_tune(W):
    import xgboost as xgb
    s = pd.read_csv(os.path.join(DATA, "settings_B.csv")); s = s[s.window == W.w].iloc[0]
    S = dict(P_depth=int(s.P_max_depth), P_mcw=float(s.P_min_child_weight), P_col=float(s.P_colsample_bytree), P_nr=int(s.P_rounds),
             S_depth=int(s.S_max_depth), S_mcw=float(s.S_min_child_weight), S_off=bool(s.S_offset), S_nr=int(s.S_rounds))
    if SMOKE: S["P_nr"], S["S_nr"] = 150, 150
    # held-back-year reference: B's settings, early-stopped, fit on the earlier training years
    X = W.X_onehot(); f, v = W.fit, W.val
    m = xgb.train(xgb_par("binary:logistic", "logloss", TUNE_SEED, S["P_depth"], S["P_mcw"], S["P_col"]), xgb.DMatrix(X[f], W.y[f]), 400 if SMOKE else 4000,
                  evals=[(xgb.DMatrix(X[v], W.y[v]), "v")], early_stopping_rounds=100, verbose_eval=False)
    pv = m.predict(xgb.DMatrix(X[v]), iteration_range=(0, m.best_iteration + 1))
    ef, ev = W.err(f), W.err(v); off = gamma_glm_offset(W, f) if S["S_off"] else None
    dm = lambda r: xgb.DMatrix(X[r], W.dd[r], base_margin=None if off is None else off[r])
    ms = xgb.train(xgb_par("reg:gamma", "gamma-deviance", TUNE_SEED, S["S_depth"], S["S_mcw"]), dm(ef), 400 if SMOKE else 4000, evals=[(dm(ev), "v")],
                   early_stopping_rounds=100, verbose_eval=False)
    it = (0, ms.best_iteration + 1)
    S.update(val_wll=wll(pv, W.y[v], W.grp[v]), val_gamma_dev=gamma_dev(ms.predict(dm(ev), iteration_range=it), W.dd[ev]),
             val_lift=val_lift(W, pv * ms.predict(dm(v), iteration_range=it)))
    return S
def xgb_score(W, S, seed):
    import xgboost as xgb
    X = W.X_onehot(); tr, te = W.tr, W.te; e = W.err(tr)
    m = xgb.train(xgb_par("binary:logistic", "logloss", seed, S["P_depth"], S["P_mcw"], S["P_col"]), xgb.DMatrix(X[tr], W.y[tr]), S["P_nr"])
    p = m.predict(xgb.DMatrix(X[te]))
    off = gamma_glm_offset(W, tr) if S["S_off"] else None
    ms = xgb.train(xgb_par("reg:gamma", "gamma-deviance", seed, S["S_depth"], S["S_mcw"]),
                   xgb.DMatrix(X[e], W.dd[e], base_margin=None if off is None else off[e]), S["S_nr"])
    return p, ms.predict(xgb.DMatrix(X[te], base_margin=None if off is None else off[te]))

# ---- xgboost re-tuned on this frame: B's learner and B's own grid, through the same held-back-year path as the
# challengers. Not a challenger. B's shipped settings were tuned on frame f1030 and carried to f1109 unchanged
# (findings 48, 51), while every challenger is tuned on f1109, so this arm is the learner-only comparison.
def xgbt_tune(W):
    import xgboost as xgb
    X = W.X_onehot(); f, v = W.fit, W.val; ef, ev = W.err(f), W.err(v); MAXR = 300 if SMOKE else 4000
    gP = [dict(depth=a, mcw=b, col=c) for a in (3, 4, 5, 6) for b in (5, 25) for c in (0.5, 0.8)]          # G_P in s46_lib.R
    gS = [dict(depth=a, mcw=b, off=o) for a in (2, 3, 4) for b in (5, 25) for o in (False, True)]          # G_SIZE
    if SMOKE: gP, gS = gP[5:7], [gS[2], gS[3]]
    dfit, dval = xgb.DMatrix(X[f], W.y[f]), xgb.DMatrix(X[v], W.y[v]); bestP = None; gridP, gridS = [], []
    for c in gP:
        m = xgb.train(xgb_par("binary:logistic", "logloss", TUNE_SEED, c["depth"], c["mcw"], c["col"]), dfit, MAXR, evals=[(dval, "v")],
                      early_stopping_rounds=100, verbose_eval=False)
        best = m.best_iteration + 1; pv = m.predict(dval, iteration_range=(0, best)); cr = wll(pv, W.y[v], W.grp[v]); gridP.append(dict(c, crit=cr, best=best))
        if bestP is None or cr < bestP[0]: bestP = (cr, c, best, pv)
    off_all = gamma_glm_offset(W, f); bestS = None
    for c in gS:
        o = off_all if c["off"] else None
        dm = lambda r: xgb.DMatrix(X[r], W.dd[r], base_margin=io(o, r))
        m = xgb.train(xgb_par("reg:gamma", "gamma-deviance", TUNE_SEED, c["depth"], c["mcw"]), dm(ef), MAXR, evals=[(dm(ev), "v")],
                      early_stopping_rounds=100, verbose_eval=False)
        best = m.best_iteration + 1; it = (0, best); cr = gamma_dev(m.predict(dm(ev), iteration_range=it), W.dd[ev]); gridS.append(dict(c, crit=cr, best=best))
        if bestS is None or cr < bestS[0]: bestS = (cr, c, best, m.predict(xgb.DMatrix(X[v], base_margin=io(o, v)), iteration_range=it))
    return dict(P_depth=bestP[1]["depth"], P_mcw=bestP[1]["mcw"], P_col=bestP[1]["col"], P_nr=scale_rounds(bestP[2], len(W.tr), len(f)),
                S_depth=bestS[1]["depth"], S_mcw=bestS[1]["mcw"], S_off=bestS[1]["off"], S_nr=scale_rounds(bestS[2], len(W.err(W.tr)), len(ef)),
                val_wll=bestP[0], val_gamma_dev=bestS[0], val_lift=val_lift(W, bestP[3] * bestS[3]), grid_P=gridP, grid_S=gridS)

# ---- LightGBM: leaf-wise trees, the state as a native categorical ----
def lgb_P_par(c, seed): return dict(objective="binary", metric="binary_logloss", learning_rate=0.03, num_leaves=int(c["num_leaves"]),
    min_sum_hessian_in_leaf=c["min_hess"], feature_fraction=c["feature_fraction"], bagging_fraction=0.8, bagging_freq=1, lambda_l2=5,
    num_threads=NTH, seed=seed, verbose=-1)
def lgb_S_par(c, seed): return dict(objective="gamma", metric="gamma_deviance", learning_rate=0.03, num_leaves=int(c["num_leaves"]),
    min_sum_hessian_in_leaf=c["min_hess"], bagging_fraction=0.8, bagging_freq=1, lambda_l2=5, num_threads=NTH, seed=seed, verbose=-1)
def lgb_tune(W):
    import lightgbm as lgb
    X = W.X_cat(); f, v = W.fit, W.val; ef, ev = W.err(f), W.err(v); MAXR = 300 if SMOKE else 4000
    gP = [dict(num_leaves=a, min_hess=b, feature_fraction=c) for a in (8, 16, 32, 64) for b in (5, 25) for c in (0.5, 0.8)]
    gS = [dict(num_leaves=a, min_hess=b, off=o) for a in (4, 8, 16) for b in (5, 25) for o in (False, True)]
    if SMOKE: gP, gS = gP[5:7], [gS[2], gS[3]]
    bestP = None; gridP, gridS = [], []
    for c in gP:
        m = lgb.train(lgb_P_par(c, TUNE_SEED), lgb.Dataset(X.iloc[f], W.y[f]), MAXR, valid_sets=[lgb.Dataset(X.iloc[v], W.y[v])],
                      callbacks=[lgb.early_stopping(100, verbose=False)])
        pv = m.predict(X.iloc[v], num_iteration=m.best_iteration); cr = wll(pv, W.y[v], W.grp[v]); gridP.append(dict(c, crit=cr, best=m.best_iteration))
        if bestP is None or cr < bestP[0]: bestP = (cr, c, m.best_iteration, pv)
    off_all = gamma_glm_offset(W, f); bestS = None
    for c in gS:
        o = off_all if c["off"] else None      # None, not zeros: zeros would switch off LightGBM's starting average
        m = lgb.train(lgb_S_par(c, TUNE_SEED), lgb.Dataset(X.iloc[ef], W.dd[ef], init_score=io(o, ef)), MAXR,
                      valid_sets=[lgb.Dataset(X.iloc[ev], W.dd[ev], init_score=io(o, ev))], callbacks=[lgb.early_stopping(100, verbose=False)])
        pr = lambda r: m.predict(X.iloc[r], num_iteration=m.best_iteration) * ex(o, r)
        cr = gamma_dev(pr(ev), W.dd[ev]); gridS.append(dict(c, crit=cr, best=m.best_iteration))
        if bestS is None or cr < bestS[0]: bestS = (cr, c, m.best_iteration, pr(v))
    assert SMOKE or min(g["best"] for g in gridP + gridS) > 1, "a LightGBM fit stopped at its first round"
    return dict(P=bestP[1], P_nr=scale_rounds(bestP[2], len(W.tr), len(f)), S=bestS[1], S_nr=scale_rounds(bestS[2], len(W.err(W.tr)), len(ef)),
                val_wll=bestP[0], val_gamma_dev=bestS[0], val_lift=val_lift(W, bestP[3] * bestS[3]), grid_P=gridP, grid_S=gridS)
def lgb_score(W, S, seed):
    import lightgbm as lgb
    X = W.X_cat(); tr, te = W.tr, W.te; e = W.err(tr)
    p = lgb.train(lgb_P_par(S["P"], seed), lgb.Dataset(X.iloc[tr], W.y[tr]), S["P_nr"]).predict(X.iloc[te])
    o = gamma_glm_offset(W, tr) if S["S"]["off"] else None
    ms = lgb.train(lgb_S_par(S["S"], seed), lgb.Dataset(X.iloc[e], W.dd[e], init_score=io(o, e)), S["S_nr"])
    return p, ms.predict(X.iloc[te]) * ex(o, te)

# ---- CatBoost: symmetric trees; the state as target statistics or as indicators ----
# CatBoost has no gamma loss; the size model uses Tweedie with variance power 1.9 (log link), the closest it offers.
# Learning rate 0.06 (B's is 0.03) to halve the rounds; the boosting type is the library's choice at this size (Plain).
# With the offset off, the size model starts from the log of the mean error size, not from 0.
CB_LR = 0.06
def cb_kw(c, seed, its): return dict(depth=int(c["depth"]), learning_rate=CB_LR, iterations=int(its), thread_count=NTH, random_seed=seed, verbose=0,
    **({"one_hot_max_size": 255} if c["state"] == "onehot" else {}))
def cb_tune(W):
    import catboost as cb
    X = W.X_str(); f, v = W.fit, W.val; ef, ev = W.err(f), W.err(v); MAXR = 200 if SMOKE else 4000
    pool = lambda r, lab=None, base=None: cb.Pool(X.iloc[r], lab, cat_features=["st"], baseline=base)
    gP = [dict(depth=a, state=s) for a in (4, 6, 8) for s in ("onehot", "ctr")]
    gS = [dict(depth=a, state=s, off=o) for a in (4, 6) for s in ("onehot", "ctr") for o in (False, True)]
    if SMOKE: gP, gS = gP[2:4], [gS[1], gS[2]]
    bestP = None; gridP, gridS = [], []
    for c in gP:
        t0 = time.time()
        m = cb.CatBoostClassifier(loss_function="Logloss", od_type="Iter", od_wait=100, use_best_model=True, **cb_kw(c, TUNE_SEED, MAXR))
        m.fit(pool(f, W.y[f]), eval_set=pool(v, W.y[v])); pv = m.predict_proba(pool(v))[:, 1]; cr = wll(pv, W.y[v], W.grp[v])
        say(f"  {W.w} catboost P {c}: wll {cr:.5f}, best iteration {m.get_best_iteration() + 1}, {time.time() - t0:.0f} s")
        gridP.append(dict(c, crit=cr, best=m.get_best_iteration() + 1))
        if bestP is None or cr < bestP[0]: bestP = (cr, c, m.get_best_iteration() + 1, pv)
    off_all = gamma_glm_offset(W, f); bestS = None
    for c in gS:
        o = off_all if c["off"] else np.full(len(W.y), math.log(W.dd[ef].mean()))
        m = cb.CatBoostRegressor(loss_function="Tweedie:variance_power=1.9", od_type="Iter", od_wait=100, use_best_model=True, **cb_kw(c, TUNE_SEED, MAXR))
        m.fit(pool(ef, W.dd[ef], o[ef]), eval_set=pool(ev, W.dd[ev], o[ev]))
        pr = lambda r: np.exp(m.predict(pool(r), prediction_type="RawFormulaVal") + o[r])
        cr = gamma_dev(pr(ev), W.dd[ev]); gridS.append(dict(c, crit=cr, best=m.get_best_iteration() + 1))
        if bestS is None or cr < bestS[0]: bestS = (cr, c, m.get_best_iteration() + 1, pr(v))
    return dict(P=bestP[1], P_nr=scale_rounds(bestP[2], len(W.tr), len(f)), S=bestS[1], S_nr=scale_rounds(bestS[2], len(W.err(W.tr)), len(ef)),
                val_wll=bestP[0], val_gamma_dev=bestS[0], val_lift=val_lift(W, bestP[3] * bestS[3]), grid_P=gridP, grid_S=gridS)
def cb_score(W, S, seed):
    import catboost as cb
    X = W.X_str(); tr, te = W.tr, W.te; e = W.err(tr)
    pool = lambda r, lab=None, base=None: cb.Pool(X.iloc[r], lab, cat_features=["st"], baseline=base)
    p = cb.CatBoostClassifier(loss_function="Logloss", **cb_kw(S["P"], seed, S["P_nr"])).fit(pool(tr, W.y[tr])).predict_proba(pool(te))[:, 1]
    o = gamma_glm_offset(W, tr) if S["S"]["off"] else np.full(len(W.y), math.log(W.dd[e].mean()))
    ms = cb.CatBoostRegressor(loss_function="Tweedie:variance_power=1.9", **cb_kw(S["S"], seed, S["S_nr"])).fit(pool(e, W.dd[e], o[e]))
    return p, np.exp(ms.predict(pool(te), prediction_type="RawFormulaVal") + o[te])

# ---- Explainable Boosting Machine: an additive model (one curve per input), with or without pair terms ----
# Each fit is already a bag of 14 models with its own internal validation split, so no rounds are carried over.
def ebm_kw(seed): return dict(random_state=seed, n_jobs=-2, **({"interactions": 0} if LEARNER == "ebm_main" else {}),
                              **({"max_rounds": 300, "outer_bags": 2} if SMOKE else {}))
def ebm_fit(W, rows_P, rows_E, off, seed):
    from interpret.glassbox import ExplainableBoostingClassifier, ExplainableBoostingRegressor
    X = W.X_str(); ft = ["continuous"] * len(W.feats) + ["nominal"]
    mp = ExplainableBoostingClassifier(feature_types=ft, **ebm_kw(seed)).fit(X.iloc[rows_P], W.y[rows_P])
    ms = ExplainableBoostingRegressor(feature_types=ft, objective="gamma_deviance", **ebm_kw(seed)).fit(
        X.iloc[rows_E], W.dd[rows_E], init_score=None if off is None else off[rows_E])
    P = lambda r: mp.predict_proba(X.iloc[r])[:, 1]
    Sz = lambda r: ms.predict(X.iloc[r]) * (1.0 if off is None else np.exp(off[r]))   # log link: the offset multiplies the prediction
    return P, Sz, ms
def ebm_tune(W):
    f, v = W.fit, W.val; ef, ev = W.err(f), W.err(v); best = None
    for use_off in (False, True):
        off = gamma_glm_offset(W, f) if use_off else None
        P, Sz, ms = ebm_fit(W, f, ef, off, TUNE_SEED)
        if use_off:   # the offset must enter predictions exactly once: check against the library's own init_score path
            try:
                alt = ms.predict(W.X_str().iloc[ev], init_score=off[ev]); assert np.allclose(alt, Sz(ev), rtol=1e-6), "EBM offset handling differs"
            except TypeError: pass
        cr = gamma_dev(Sz(ev), W.dd[ev]); pv = P(v)
        if best is None or cr < best[0]: best = (cr, use_off, wll(pv, W.y[v], W.grp[v]), val_lift(W, pv * Sz(v)))
    return dict(S_off=best[1], val_wll=best[2], val_gamma_dev=best[0], val_lift=best[3])
def ebm_score(W, S, seed):
    off = gamma_glm_offset(W, W.tr) if S["S_off"] else None
    P, Sz, _ = ebm_fit(W, W.tr, W.err(W.tr), off, seed); return P(W.te), Sz(W.te)

# ---- TabNet: an attention-based neural network (pytorch-tabnet), CPU ----
# Inputs: blanks set to 0, then sign(x) log(1 + |x|), a fixed monotone transform (the dollar inputs are heavily skewed
# and the network's first layer is a batch normalization); the state as a learned embedding. The size model is fitted
# to log error dollars minus the offset, or minus their mean on the fit rows when the offset is off, so both arms
# start centred; a single factor set on the fit rows returns it to the dollar scale.
def tn_X(W):
    Z = W.Xn.fillna(0).to_numpy(np.float64)
    return np.column_stack([(np.sign(Z) * np.log1p(np.abs(Z))).astype(np.float32), W.stcat.codes.astype(np.float32)])
def tn_kw(W, c, seed): return dict(n_d=c["width"], n_a=c["width"], n_steps=3, seed=seed, verbose=0, cat_idxs=[len(W.feats)], cat_dims=[len(W.states)], cat_emb_dim=4)
def tn_tune(W):
    import torch; from pytorch_tabnet.tab_model import TabNetClassifier, TabNetRegressor
    X = tn_X(W); f, v = W.fit, W.val; ef, ev = W.err(f), W.err(v); EP = 4 if SMOKE else 100
    bestP = None
    for c in ([dict(width=8)] if SMOKE else [dict(width=8), dict(width=24)]):
        t0 = time.time(); m = TabNetClassifier(**tn_kw(W, c, TUNE_SEED))
        m.fit(X[f], W.y[f].astype(int), eval_set=[(X[v], W.y[v].astype(int))], eval_metric=["logloss"], max_epochs=EP, patience=15,
              batch_size=2048, virtual_batch_size=256)
        pv = m.predict_proba(X[v])[:, 1]; cr = wll(pv, W.y[v], W.grp[v])
        say(f"  {W.w} tabnet P {c}: wll {cr:.5f}, best epoch {m.best_epoch + 1}, {time.time() - t0:.0f} s")
        if bestP is None or cr < bestP[0]: bestP = (cr, c, m.best_epoch + 1, pv)
    off_all = gamma_glm_offset(W, f); bestS = None
    for use_off in (False, True):
        o = off_all if use_off else np.full(len(W.y), np.log(W.dd[ef]).mean()); m = TabNetRegressor(**tn_kw(W, dict(width=8), TUNE_SEED))
        m.fit(X[ef], (np.log(W.dd[ef]) - o[ef]).reshape(-1, 1), eval_set=[(X[ev], (np.log(W.dd[ev]) - o[ev]).reshape(-1, 1))], eval_metric=["rmse"],
              max_epochs=EP * 2, patience=20, batch_size=512, virtual_batch_size=128)
        raw = lambda r: np.exp(m.predict(X[r]).ravel() + o[r]); k = W.dd[ef].mean() / raw(ef).mean()
        cr = gamma_dev(k * raw(ev), W.dd[ev])
        if bestS is None or cr < bestS[0]: bestS = (cr, use_off, m.best_epoch + 1, k * raw(v))
    return dict(P=bestP[1], P_epochs=int(bestP[2]), S_off=bool(bestS[1]), S_epochs=int(bestS[2]),
                val_wll=bestP[0], val_gamma_dev=bestS[0], val_lift=val_lift(W, bestP[3] * bestS[3]))
def tn_score(W, S, seed):
    import torch; from pytorch_tabnet.tab_model import TabNetClassifier, TabNetRegressor
    X = tn_X(W); tr, te = W.tr, W.te; e = W.err(tr)
    m = TabNetClassifier(**tn_kw(W, S["P"], seed)); m.fit(X[tr], W.y[tr].astype(int), max_epochs=S["P_epochs"], batch_size=2048, virtual_batch_size=256)
    p = m.predict_proba(X[te])[:, 1]
    o = gamma_glm_offset(W, tr) if S["S_off"] else np.full(len(W.y), np.log(W.dd[e]).mean()); ms = TabNetRegressor(**tn_kw(W, dict(width=8), seed))
    ms.fit(X[e], (np.log(W.dd[e]) - o[e]).reshape(-1, 1), max_epochs=S["S_epochs"], batch_size=512, virtual_batch_size=128)
    raw = lambda r: np.exp(ms.predict(X[r]).ravel() + o[r]); k = W.dd[e].mean() / raw(e).mean()
    return p, k * raw(te)

TUNERS = {"xgb_ref": (xgb_tune, xgb_score), "xgb_tuned": (xgbt_tune, xgb_score), "lgbm": (lgb_tune, lgb_score), "catboost": (cb_tune, cb_score),
          "ebm_main": (ebm_tune, ebm_score), "ebm_pairs": (ebm_tune, ebm_score), "tabnet": (tn_tune, tn_score)}
tune, score = TUNERS[LEARNER]

say(f"=== {LEARNER} on {','.join(WINS)}{' (smoke)' if SMOKE else ''}, seeds {list(SEEDS)[0]}-{list(SEEDS)[-1]}")
for w in WINS:
    W = None
    tf = os.path.join(TUNE, f"{LEARNER}_{w}.json")
    todo = [s for s in SEEDS if not os.path.exists(os.path.join(OUT, f"{LEARNER}_{w}_{s}.csv"))]
    if os.path.exists(tf) and not todo: continue
    W = Win(w)
    if os.path.exists(tf): S = json.load(open(tf))
    else:
        t0 = time.time(); S = tune(W); S["tune_minutes"] = round((time.time() - t0) / 60, 2)
        S["rows"] = dict(train=len(W.tr), fit=len(W.fit), val=len(W.val), test=len(W.te))
        json.dump(S, open(tf, "w"), indent=1, default=lambda o: o.item() if hasattr(o, "item") else str(o))
        say(f"{w} {LEARNER} settings: {json.dumps({k: v for k, v in S.items() if k != 'rows'}, default=str)}")
    for seed in todo:
        t0 = time.time(); p, size = score(W, S, seed)
        assert len(p) == len(W.te) and np.all(np.isfinite(p)) and np.all(np.isfinite(size)) and np.all(size > 0) and np.all((p >= 0) & (p <= 1))
        pd.DataFrame({"st": W.st[W.te], "p": p, "size": size, "ed": p * size}).to_csv(os.path.join(OUT, f"{LEARNER}_{w}_{seed}.csv"), index=False)
        extra = f", stand-in lift {lift(p * size, W.dn[W.te], W.keep[W.te], W.st[W.te]):.3f}" if SMOKE else ""   # smoke: a training year, not the test year
        say(f"{w} {LEARNER} seed {seed}: {(time.time() - t0) / 60:.1f} min, mean p {p.mean():.4f}, median size {np.median(size):.0f}{extra}")
say(f"=== {LEARNER} done")
