# Shared helpers for the blend review (2026-11-14). Reads only; writes nothing outside this folder.
# The share functions follow shares_of() in 07_score_followup.R: each state flags max(1, round(b x its cases)) from the
# top of its kept cases; map weights = payment error rate x FY2024 issuance; DC has no weight.
import os, numpy as np, pandas as pd
HERE = r"C:\Users\ericg\snap_qc_challenger_benchmark"
REPO = r"C:\Users\ericg\snap_qc_v27"
SC2 = os.path.join(HERE, "followup", "scores"); SC1 = os.path.join(HERE, "scores"); HB = os.path.join(HERE, "followup", "heldback")
BUD = np.array([0.05, 0.075, 0.10]); WEIGHTS = {"W1": 0.2, "W2": 0.4, "W3": 0.4}; YEAR = {"W1": 2019, "W2": 2022, "W3": 2024}
B_EXP = 1.1529426503560412

def state_weights(year):
    iss = pd.read_csv(os.path.join(REPO, "additional_data", "fns_state_issuance_fy2024.csv"))
    per = pd.read_csv(os.path.join(REPO, "additional_data", "fns_payment_error_rates_fy%d.csv" % year))
    m = per.merge(iss, on="state"); m = m[~m.state.isin(["District of Columbia", "United States"])]
    ws = dict(zip(m.state, m.payment_error_rate / 100 * m.issuance_dollars))
    return {k: v for k, v in ws.items() if np.isfinite(v)}

class Frame:
    """Rows of one scored year: state, error dollars, kept flag, label."""
    def __init__(self, st, d, keep, year, y=None):
        self.st = np.asarray(st); self.d = np.asarray(d, float); self.keep = np.asarray(keep, bool); self.n = len(self.st)
        self.y = (self.d > 0).astype(float) if y is None else np.asarray(y, float)
        self.states, self.code = np.unique(self.st, return_inverse=True)
        self.ns = np.bincount(self.code); self.starts = np.concatenate([[0], np.cumsum(self.ns)[:-1]])
        self.kk = np.maximum(1, np.rint(BUD[:, None] * self.ns[None, :])).astype(float)      # budgets x states
        ws = state_weights(year); self.w = np.array([ws.get(s, np.nan) for s in self.states])
        self.hasw = np.isfinite(self.w); big = np.argsort(-np.where(self.hasw, self.w, -1))[:3]
        self.big3 = np.zeros(len(self.states), bool); self.big3[big] = True
        self.base = np.argsort(self.code, kind="stable")                                       # rows in state order
        self.code_sorted = self.code[self.base]
    def perm(self, sc):
        idx = np.arange(self.n); return np.lexsort((idx, -np.asarray(sc, float), self.code))  # state, then score down, then row
    def state_caught(self, perm, counts=None):
        """per budget and state: dollars caught, and total kept dollars per state"""
        c = self.keep[perm].astype(float) if counts is None else counts[perm] * self.keep[perm]
        ds = self.d[perm]; cum = np.cumsum(c); before = cum - c; off = before[self.starts]; before = before - off[self.code_sorted]
        got = np.empty((len(BUD), len(self.states)))
        for b in range(len(BUD)):
            take = np.clip(self.kk[b][self.code_sorted] - before, 0, c); got[b] = np.add.reduceat(ds * take, self.starts)
        tot = np.add.reduceat(ds * c, self.starts); return got, tot
    def stats(self, perm, counts=None):
        got, tot = self.state_caught(perm, counts); ok = self.hasw & (tot > 0); w = self.w[ok]
        dr = (got[:, ok] / tot[ok] * w).sum(axis=1) / w.sum(); okx = ok & ~self.big3
        return dict(dr5=dr[0], band=dr[2] - dr[0], dr10=dr[2], drm=dr.mean(), dr10x3=(got[2, okx] / tot[okx] * self.w[okx]).sum() / self.w[okx].sum(),
                    pooled5=got[0].sum() / tot.sum(), pooled10=got[2].sum() / tot.sum(),
                    eq10=np.mean(got[2, ok] / tot[ok]))
    def pdev(self, sc, counts=None):
        k = self.keep; c = np.ones(k.sum()) if counts is None else counts[k]; mu = np.asarray(sc, float)[k]; y = self.d[k]; g = self.code[k]; G = len(self.states)
        f = np.maximum(np.bincount(g, c * y, G), 1.0) / np.maximum(np.bincount(g, c * mu, G), 1e-9); mu = np.maximum(mu * f[g], 1e-9)
        return 2 * np.sum(c * (np.where(y > 0, y * np.log(np.where(y > 0, y, 1) / mu), 0.0) - (y - mu))) / c.sum()
    def wll(self, p):
        p = np.clip(np.asarray(p, float), 1e-6, 1 - 1e-6); z = np.log(p) - np.log1p(-p); a = np.zeros(len(self.states)); g = self.code; y = self.y
        for _ in range(60):
            pr = 1 / (1 + np.exp(-(z + a[g]))); step = np.bincount(g, y - pr) / np.maximum(np.bincount(g, pr * (1 - pr)), 1e-12); a += step
            if np.max(np.abs(step)) < 1e-9: break
        zz = z + a[g]; return -np.mean(y * -np.logaddexp(0, -zz) + (1 - y) * -np.logaddexp(0, zz))
    def boot_counts(self, rng):
        u = rng.random(self.n); draw = self.starts[self.code_sorted] + np.floor(u * self.ns[self.code_sorted]).astype(int)
        return np.bincount(self.base[draw], minlength=self.n).astype(float)

def test_frame(w):
    d = pd.read_csv(os.path.join(HERE, "data", w + ".csv"), usecols=["role", "st", "y", "d", "keep"], dtype={"role": str, "st": str})
    d = d[d.role == "test"].reset_index(drop=True); return Frame(d.st.to_numpy(), d.d.to_numpy(), d.keep.to_numpy() == 1, YEAR[w], d.y.to_numpy())

def heldback_frame(w):
    d = pd.read_csv(os.path.join(HERE, "data", w + ".csv"), usecols=["role", "yr", "st", "y", "d", "keep"], dtype={"role": str, "st": str, "yr": str})
    d = d[d.role == "train"]; last = d.yr.max(); d = d[d.yr == last].reset_index(drop=True)
    return Frame(d.st.to_numpy(), d.d.to_numpy(), d.keep.to_numpy() == 1, int(last), d.y.to_numpy())

def fits(dirname, name, w, seeds, fr=None):
    P, S = [], []
    for s in seeds:
        z = pd.read_csv(os.path.join(dirname, "%s_%s_%d.csv" % (name, w, s)))
        if fr is not None: assert (z.st.to_numpy() == fr.st).all()
        P.append(z.p.to_numpy()); S.append(z["size"].to_numpy())
    return np.array(P), np.array(S)
