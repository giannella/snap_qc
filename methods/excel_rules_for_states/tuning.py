"""
Tiered, guarded local tuning of a blended delivery list.

Reference implementation of methods/tuning_principles.md. Given a state's own
internal case frame (data that never helped mine the rules) and the delivery
list it was handed, this decides how much tuning that data can actually pay for
and returns the deployable list plus a full audit trail.

    import tuning
    res = tuning.run(cases_df, rules, tuning.TuningConfig())
    print('\n'.join(res.audit_lines()))

Three tiers, defaulting to the lowest:

  Tier 0  no tuning. The shipped rules at shipped thresholds, re-filled against
          the state's own caseload in delivery-rank order. Outcome-free.
  Tier 1  re-filter and re-rank. Rule text untouched. Admission is
          Benjamini-Hochberg at FDR 10% against the state's own stratum base
          rate with a hard support floor; ordering is the state's own 99%
          Wilson lower bound.
  Tier 2  threshold tuning inside a bracket, for Tier-1-admitted rules only.
          Structure (variables, operators, condition count, stratum) is frozen;
          only numeric thresholds move, inside +/-25% of the shipped value, and
          only to cuts that partition the state's own observed values
          differently. The shipped value is always a candidate and wins ties.

Guards, all enforced here rather than left to the caller:

  * time-based split only (most recent fiscal year(s) held out); the split is
    verified to be disjoint and forward-looking, and there is no random option
  * hard support floor (default n >= 30) on the state's own tuning data,
    applied to the variant actually deployed, not to the shipped rule
  * within-rule search paid for explicitly: the Tier-2 qualification z comes
    from variant_gate_alpha / (number of distinct variants evaluated)
  * Tier 2 refuses to run unless enough rules cleared Tier 1 (findings §9's
    "roughly 30 or more rules qualify" rule of thumb)
  * the holdout decides at most one comparison per tier, never one per rule,
    under a rule fixed before the run
  * the untuned arm is always computed and reported beside the tuned one
  * the number of distinct threshold combinations evaluated is counted and
    reported

Self-check (no state data needed):  python tuning.py --selfcheck
"""
from __future__ import annotations

import itertools
import math
from dataclasses import dataclass, field
from statistics import NormalDist

import numpy as np
import pandas as pd

__all__ = ['TuningConfig', 'TuningResult', 'run', 'wilson_lcb', 'binom_sf_ge',
           'bh_reject', 'condition_grid', 'rule_variants']


# ══════════════════════════════════════════════════════════════════════════════
# CONFIG — every knob is declared before the run and printed in the audit
# ══════════════════════════════════════════════════════════════════════════════
@dataclass(frozen=True)
class TuningConfig:
    # --- what the state is allowed to do ------------------------------------
    max_tier: int = 2               # ceiling; 0 disables tuning entirely
    budget: float = 0.05            # review budget as a share of the caseload

    # --- the split (guard: time-based, never random) -------------------------
    year_col: str = 'fiscal_year'
    holdout_years: int = 1          # most recent N fiscal years are held out
    # Explicit years to hold out, overriding holdout_years. Exists for
    # deliberate year-swap comparisons (state_threshold_gridsearch_v2.R trains
    # on 2022+2024 and tests on 2023). When the held-out year sits BETWEEN the
    # tuning years the test INTERPOLATES rather than extrapolates, which
    # flatters every arm (findings section 9); run() detects that and says so.
    holdout_year_set: tuple = ()

    # --- Tier 1 admission ----------------------------------------------------
    min_support: int = 30           # non-negotiable (findings §9, §19)
    fdr_alpha: float = 0.10         # Benjamini-Hochberg level
    lcb_z_rank: float = 2.326       # 99% one-sided: the ordering statistic

    # --- Tier 2 threshold search --------------------------------------------
    factors_fine: tuple = (0.75, 0.90, 1.00, 1.10, 1.25)
    factors_coarse: tuple = (0.90, 1.00, 1.10)
    fine_max_conds: int = 3         # deeper rules get the coarse bracket
    max_variants: int = 700         # per-rule cap on threshold combinations
    variant_gate_alpha: float = 0.10   # per-rule family alpha, Bonferroni'd
    min_variant_precision: float = 0.20
    select_objective: str = 'dollars'   # 'dollars' or 'counts'; fixed globally
    min_rel_gain: float = 0.0       # required relative gain to move a threshold
    min_admitted_for_tier2: int = 30    # findings §9 rule of thumb

    # --- the holdout gate ----------------------------------------------------
    min_holdout_errors: int = 30    # below this, no tuning is judged at all
    min_holdout_flagged: int = 30   # below this, an arm cannot be promoted
    gate_z: float = 1.2816          # 90% one-sided on the arm's own holdout

    # --- columns -------------------------------------------------------------
    stratum_col: str = 'hh_group'
    error_col: str = 'over_threshold'
    amount_col: str = 'total_error_amount'

    def factors_for(self, n_conds: int) -> tuple:
        return self.factors_fine if n_conds <= self.fine_max_conds else self.factors_coarse


# ══════════════════════════════════════════════════════════════════════════════
# STATISTICS
# ══════════════════════════════════════════════════════════════════════════════
def wilson_lcb(k: int, n: int, z: float) -> float:
    """One-sided Wilson lower confidence bound on a proportion. Matches
    wilson_lcb() in rule_mining_helpers.R (0 when n == 0)."""
    if n <= 0:
        return 0.0
    p = k / n
    z2 = z * z
    num = p + z2 / (2 * n) - z * math.sqrt(max(p * (1 - p) / n + z2 / (4 * n * n), 0.0))
    return max(0.0, num / (1 + z2 / n))


def binom_sf_ge(k: int, n: int, p: float) -> float:
    """P(X >= k) for X ~ Binomial(n, p): the one-sided p-value for "this rule's
    precision is no better than the base rate". Exact, summed in logs."""
    if n <= 0:
        return 1.0
    k = int(max(k, 0))
    if k <= 0:
        return 1.0
    if k > n:
        return 0.0
    if p <= 0.0:
        return 0.0
    if p >= 1.0:
        return 1.0
    lgam = math.lgamma
    ln_p, ln_q = math.log(p), math.log1p(-p)
    ln_n1 = lgam(n + 1)
    total = 0.0
    for i in range(k, n + 1):
        total += math.exp(ln_n1 - lgam(i + 1) - lgam(n - i + 1) + i * ln_p + (n - i) * ln_q)
    return float(min(max(total, 0.0), 1.0))


def bh_reject(pvals, alpha: float):
    """Benjamini-Hochberg at level `alpha`. Returns (rejected mask, critical p).
    The bar sets itself from the number and strength of the tests actually
    conducted, which is the whole point of using it at admission."""
    p = np.asarray(pvals, dtype=float)
    m = p.size
    if m == 0:
        return np.zeros(0, bool), 0.0
    order = np.argsort(p, kind='stable')
    ranks = np.arange(1, m + 1)
    passing = p[order] <= alpha * ranks / m
    rejected = np.zeros(m, bool)
    if not passing.any():
        return rejected, 0.0
    kmax = int(np.max(np.flatnonzero(passing)))
    rejected[order[:kmax + 1]] = True
    return rejected, float(p[order][kmax])


def _z_for_alpha(alpha: float) -> float:
    """One-sided normal quantile for a tail probability, clamped to a sane range."""
    alpha = min(max(alpha, 1e-9), 0.5)
    return float(NormalDist().inv_cdf(1.0 - alpha))


# ══════════════════════════════════════════════════════════════════════════════
# RULE EVALUATION
# ══════════════════════════════════════════════════════════════════════════════
def _cond_mask(x: np.ndarray, op: str, thr: float) -> np.ndarray:
    if op == '>=':
        m = x >= thr
    elif op == '>':
        m = x > thr
    elif op == '<=':
        m = x <= thr
    elif op == '<':
        m = x < thr
    elif op == '==':
        m = x == thr
    else:
        raise ValueError(f'unsupported operator: {op}')
    return np.where(np.isnan(x), False, m)


def rule_mask(frame: pd.DataFrame, rule: dict, thresholds) -> np.ndarray:
    """Cases in `frame` that this rule flags, inside its own stratum. A rule's
    stratum is part of its identity and is never searched over."""
    m = (frame[_STRATUM].values.astype(str) == str(rule['hh']))
    for c, t in zip(rule['conds'], thresholds):
        m &= _cond_mask(frame[c['var']].values.astype(float), c['op'], float(t))
    return m


def score(mask: np.ndarray, is_err: np.ndarray, err_dollars: np.ndarray,
          tot_err: int, tot_dollars: float, tot_n: int) -> dict:
    n = int(mask.sum())
    k = int((mask & is_err).sum())
    d = float(err_dollars[mask].sum())
    return {'n': n, 'k': k, 'dollars': d,
            'prec': (k / n) if n else 0.0,
            'rec': (k / tot_err) if tot_err else 0.0,
            'drec': (d / tot_dollars) if tot_dollars else 0.0,
            'workload': (n / tot_n) if tot_n else 0.0}


# ══════════════════════════════════════════════════════════════════════════════
# TIER 2 SEARCH SPACE — bracket + partition-aware dedup
# ══════════════════════════════════════════════════════════════════════════════
def condition_grid(thr: float, op: str, factors, observed: np.ndarray) -> list:
    """Candidate thresholds for ONE condition. Port of rule_variants()'s inner
    grid in state_threshold_gridsearch_v2.R.

    A candidate survives only if it partitions the state's own observed values
    differently from every candidate already kept: two cuts with no data point
    between them flag the same cases, so they are one test, not two. Within a
    group of equivalent cuts the one closest to the shipped value wins, so the
    search never drifts away from the delivered rule for free. Cuts that can
    never fire in this state are dropped. A shipped threshold of exactly 0 is
    left alone (scaling it is meaningless).
    """
    thr = float(thr)
    if thr == 0.0:
        return [0.0]
    cand = []
    for f in factors:
        v = float(f'{thr * f:.4g}')
        if v not in cand:
            cand.append(v)
    if thr not in cand:
        cand.append(thr)

    uv = np.asarray(observed, dtype=float)
    uv = np.unique(uv[np.isfinite(uv)])
    if uv.size == 0:
        return [thr]

    # side='right' counts observed <= t; side='left' counts observed < t.
    side = 'right' if op in ('>', '<=') else 'left'
    kept, keys = [], set()
    for v in sorted(cand, key=lambda x: (abs(x - thr), x)):   # closest-to-shipped first
        key = int(np.searchsorted(uv, v, side=side))
        if key in keys:
            continue
        never_fires = (key == uv.size) if op in ('>', '>=') else (key == 0)
        if never_fires:
            continue
        keys.add(key)
        kept.append(v)
    return sorted(kept) if kept else [thr]


def rule_variants(rule: dict, frame: pd.DataFrame, cfg: TuningConfig):
    """Distinct threshold combinations for one rule, shipped tuple first.

    Returns (variants, n_conds_grid) where variants[0] is always the shipped
    thresholds, so "leave it alone" is always on the table and the search can
    never do worse than the delivered rule on the tuning data.
    """
    conds = rule['conds']
    factors = cfg.factors_for(len(conds))
    sub = frame[frame[_STRATUM].values.astype(str) == str(rule['hh'])]
    grids = [condition_grid(c['thr'], c['op'], factors,
                            sub[c['var']].values.astype(float)) for c in conds]
    shipped = tuple(float(c['thr']) for c in conds)
    variants = [shipped]
    seen = {shipped}
    # nearest-to-shipped first, so the max_variants cap truncates the far end
    for combo in sorted(itertools.product(*grids),
                        key=lambda t: sum(abs(v - s) / (abs(s) or 1.0)
                                          for v, s in zip(t, shipped))):
        t = tuple(float(v) for v in combo)
        if t in seen:
            continue
        seen.add(t)
        variants.append(t)
        if len(variants) >= cfg.max_variants:
            break
    return variants, [len(g) for g in grids]


# ══════════════════════════════════════════════════════════════════════════════
# THE SPLIT (guard: time-based, verified)
# ══════════════════════════════════════════════════════════════════════════════
def time_split(frame: pd.DataFrame, cfg: TuningConfig):
    """Split by fiscal year. By default the most recent year(s) are held out, so
    the test extrapolates forward the way deployment does. There is deliberately
    no random option: a random split leaks, because the same caseload
    composition appears on both sides.

    `cfg.holdout_year_set` overrides the default with named years. That path
    skips the forward-looking check on purpose, because a year-swap comparison
    needs it; the caller is told when the result interpolates.
    """
    years = sorted(pd.unique(frame[cfg.year_col].astype(int)))
    if len(years) < 2:
        raise ValueError(f'need at least 2 fiscal years to hold one out; found {years}')
    if cfg.holdout_year_set:
        want = {int(y) for y in cfg.holdout_year_set}
        hold_years = [y for y in years if y in want]
        tune_years = [y for y in years if y not in want]
        missing = want - set(hold_years)
        if missing:
            raise ValueError(f'holdout_year_set names years not in the frame: {sorted(missing)}')
        if not tune_years:
            raise ValueError('holdout_year_set leaves no tuning years')
    else:
        n_hold = min(max(int(cfg.holdout_years), 1), len(years) - 1)
        hold_years, tune_years = years[-n_hold:], years[:-n_hold]
        # the forward-looking guard, checked rather than assumed
        assert min(hold_years) > max(tune_years),             'holdout must be strictly later than tuning years'
    assert not set(tune_years) & set(hold_years), 'tune/holdout years overlap'
    yr = frame[cfg.year_col].astype(int).values
    return (np.isin(yr, tune_years), np.isin(yr, hold_years),
            [int(y) for y in tune_years], [int(y) for y in hold_years])


# ══════════════════════════════════════════════════════════════════════════════
# BUDGET FILL — outcome-free, mirrors INCL_build_blended_delivery_list_v2.R
# ══════════════════════════════════════════════════════════════════════════════
def fill_to_budget(order, masks: dict, n_rows: int, budget: float):
    """Walk the ranked list, turning on a rule when the union still fits the
    review budget and skipping it when it does not. Uses only flag counts on
    the tuning caseload, no outcomes, exactly as the delivery builder does."""
    cap = math.floor(budget * n_rows)
    union = np.zeros(n_rows, bool)
    taken, skipped, n_in = [], [], 0
    for j in order:
        ix = masks[j]
        if ix is None:
            continue
        add = int((ix & ~union).sum())
        if add == 0:
            skipped.append(j)
            continue
        if n_in + add <= cap:
            union |= ix
            n_in += add
            taken.append(j)
        else:
            skipped.append(j)
    return taken, skipped, cap


# ══════════════════════════════════════════════════════════════════════════════
# RESULT
# ══════════════════════════════════════════════════════════════════════════════
@dataclass
class TuningResult:
    tier: int
    tier_reason: str
    cfg: TuningConfig
    tune_years: list
    hold_years: list
    rules_table: pd.DataFrame        # one row per shipped rule, full audit
    arms: pd.DataFrame               # one row per tier: tuning + holdout numbers
    deployed_thresholds: dict        # rule index -> tuple, for deployed rules
    deployed: dict                   # rule index -> bool
    comparisons: dict                # the count the validity argument rests on
    notes: list = field(default_factory=list)

    def audit_lines(self) -> list:
        L = [
            'TIERED LOCAL TUNING (methods/tuning_principles.md)',
            f'  split                  tune {self.tune_years} -> holdout {self.hold_years} '
            f'({"INTERPOLATED, not forward" if min(self.hold_years) < max(self.tune_years) else "forward, by year"})',
            f'  tier ceiling requested {self.cfg.max_tier}',
            f'  TIER DEPLOYED          {self.tier}  ({self.tier_reason})',
            f'  rules shipped          {len(self.rules_table)}',
            f'  cleared support floor  {int(self.rules_table.support_ok.sum())} '
            f'(n >= {self.cfg.min_support} on tuning years; widest rule flags '
            f'{int(self.rules_table.tune_n.max()) if len(self.rules_table) else 0})',
            f'  admitted by BH         {int(self.rules_table.admitted.sum())} '
            f'at FDR {self.cfg.fdr_alpha:.0%} vs the stratum base rate',
            f'  thresholds moved       {int(self.rules_table.threshold_moved.sum())}',
            f'  deployed               {sum(self.deployed.values())}',
            '  comparison count (what the winner\'s curse scales with):',
            f'    Tier 1 admission tests      {self.comparisons["tier1_tests"]}',
            f'    Tier 2 distinct variants    {self.comparisons["tier2_variants"]} '
            f'(max {self.comparisons["tier2_max_per_rule"]} for one rule)',
            f'    holdout comparisons         {self.comparisons["holdout"]} '
            f'(pre-declared, one per tier, never per rule)',
            '  arms (union filled to the review budget on tuning years, judged on the holdout):',
        ]
        for _, a in self.arms.iterrows():
            L.append(
                f'    tier {int(a.tier)}: {int(a.n_rules):3d} rules | '
                f'tune prec {a.tune_prec:.3f} -> holdout prec {a.hold_prec:.3f} '
                f'(LCB {a.hold_prec_lcb:.3f}) | holdout flagged {int(a.hold_flagged):5d} '
                f'| recall {a.hold_rec:.3f} | $recall {a.hold_drec:.3f} '
                f'| workload {a.hold_workload:.3f}'
            )
        for n in self.notes:
            L.append(f'  note: {n}')
        return L


# ══════════════════════════════════════════════════════════════════════════════
# THE RUN
# ══════════════════════════════════════════════════════════════════════════════
_STRATUM = 'hh_group'      # set from cfg at the top of run()


def run(cases: pd.DataFrame, rules: list, cfg: TuningConfig | None = None,
        log=print) -> TuningResult:
    """Decide how much tuning the state's own data can pay for, and return it.

    `cases`   the state's internal case frame: one row per QC case, with the
              rule variables, `cfg.year_col`, `cfg.stratum_col`,
              `cfg.error_col` (1 = over-threshold error, any type) and
              `cfg.amount_col`.
    `rules`   the shipped delivery list in delivery-rank order, each entry a
              dict with 'num', 'hh' and 'conds' [{var, op, thr}, ...].
    """
    global _STRATUM
    cfg = cfg or TuningConfig()
    _STRATUM = cfg.stratum_col
    if cfg.select_objective not in ('dollars', 'counts'):
        raise ValueError("select_objective must be 'dollars' or 'counts' and fixed before the run")

    tune_m, hold_m, tune_years, hold_years = time_split(cases, cfg)
    tune = cases.loc[tune_m].reset_index(drop=True)
    hold = cases.loc[hold_m].reset_index(drop=True)

    def targets(fr):
        # the pipeline's convention: an error is over_threshold != 0, NA = no error
        ot = pd.to_numeric(fr[cfg.error_col], errors='coerce')
        is_err = (ot.notna() & (ot != 0)).values
        amt = pd.to_numeric(fr[cfg.amount_col], errors='coerce').fillna(0).abs().values
        return is_err, np.where(is_err, amt, 0.0)

    err_t, ed_t = targets(tune)
    err_h, ed_h = targets(hold)
    tot_t = (int(err_t.sum()), float(ed_t.sum()), len(tune))
    tot_h = (int(err_h.sum()), float(ed_h.sum()), len(hold))

    log(f'tuning years {tune_years}: {len(tune)} cases, {tot_t[0]} errors '
        f'({tot_t[0] / max(len(tune), 1):.1%})')
    log(f'holdout year {hold_years}: {len(hold)} cases, {tot_h[0]} errors '
        f'({tot_h[0] / max(len(hold), 1):.1%})')

    notes = []
    interpolated = min(hold_years) < max(tune_years)
    if interpolated:
        msg = (f'INTERPOLATED SPLIT: the held-out year(s) {hold_years} sit between the '
               f'tuning years {tune_years}, so this test interpolates instead of '
               f'extrapolating forward. It flatters every arm and is not a deployment '
               f'expectation (findings section 9); use it only to compare arms against '
               f'each other')
        log('  ' + msg)
        notes.append(msg)

    # stratum base rates on the TUNING years only: the null every rule is tested against
    strat = tune[cfg.stratum_col].astype(str).values
    base_rate = {}
    for s in np.unique(strat):
        sel = strat == s
        base_rate[s] = float(err_t[sel].mean()) if sel.any() else 0.0

    # ── Tier 1: admission and ordering, rule text untouched ──────────────────
    rows = []
    masks_tune_nat, masks_hold_nat = {}, {}
    for j, rule in enumerate(rules):
        shipped = tuple(float(c['thr']) for c in rule['conds'])
        mt = rule_mask(tune, rule, shipped)
        masks_tune_nat[j] = mt
        masks_hold_nat[j] = rule_mask(hold, rule, shipped)
        st = score(mt, err_t, ed_t, *tot_t)
        br = base_rate.get(str(rule['hh']), 0.0)
        support_ok = st['n'] >= cfg.min_support
        rows.append({
            'rule_idx': j, 'rule_num': rule['num'], 'hh': str(rule['hh']),
            'base_rate': br,
            'tune_n': st['n'], 'tune_k': st['k'], 'tune_prec': st['prec'],
            'tune_dollars': st['dollars'],
            'support_ok': support_ok,
            'pvalue': binom_sf_ge(st['k'], st['n'], br) if support_ok else np.nan,
            'lcb99': wilson_lcb(st['k'], st['n'], cfg.lcb_z_rank),
            'admitted': False, 'bh_crit': np.nan,
            'n_variants': 0, 'variant_gate_z': np.nan,
            'threshold_moved': False,
            'deployed_thresholds': shipped, 'shipped_thresholds': shipped,
        })
    tab = pd.DataFrame(rows)

    tested = tab.index[tab.support_ok].to_numpy()
    if tested.size:
        rej, crit = bh_reject(tab.loc[tested, 'pvalue'].to_numpy(), cfg.fdr_alpha)
        tab.loc[tested, 'admitted'] = rej
        tab.loc[tested, 'bh_crit'] = crit
    n_admitted = int(tab.admitted.sum())
    max_tune_n = int(tab.tune_n.max()) if len(tab) else 0
    log(f'Tier 1: {int(tab.support_ok.sum())}/{len(tab)} rules clear n >= {cfg.min_support}; '
        f'{n_admitted} admitted by BH at FDR {cfg.fdr_alpha:.0%}')
    if tested.size == 0:
        notes.append(f'no shipped rule flags {cfg.min_support}+ cases in the tuning years '
                     f'(the widest flags {max_tune_n}), so Tier 1 and Tier 2 are both off. '
                     'A budget-filled delivery list is made of narrow rules by construction; '
                     'the floor becomes reachable on a larger internal frame, not by lowering it')

    # ── Tier 2: threshold search inside the bracket, admitted rules only ─────
    tier2_ok = (cfg.max_tier >= 2) and (n_admitted >= cfg.min_admitted_for_tier2)
    if cfg.max_tier >= 2 and not tier2_ok:
        notes.append(f'Tier 2 off: {n_admitted} rules admitted, below the '
                     f'{cfg.min_admitted_for_tier2}-rule floor for local tuning (findings section 9)')
    masks_tune_tuned, masks_hold_tuned = dict(masks_tune_nat), dict(masks_hold_nat)
    total_variants, max_variants_one = 0, 0
    if tier2_ok:
        obj = (lambda sc: sc['dollars']) if cfg.select_objective == 'dollars' else (lambda sc: sc['k'])
        for j in tab.index[tab.admitted]:
            rule = rules[j]
            variants, _ = rule_variants(rule, tune, cfg)
            m = len(variants)
            total_variants += m
            max_variants_one = max(max_variants_one, m)
            # pay for the within-rule search: Bonferroni the family alpha
            z_var = _z_for_alpha(cfg.variant_gate_alpha / m)
            shipped = variants[0]
            base_sc = score(masks_tune_nat[j], err_t, ed_t, *tot_t)
            best, best_obj = None, obj(base_sc) * (1.0 + cfg.min_rel_gain)
            for t in variants[1:]:
                mk = rule_mask(tune, rule, t)
                sc = score(mk, err_t, ed_t, *tot_t)
                if sc['n'] < cfg.min_support:
                    continue
                if wilson_lcb(sc['k'], sc['n'], z_var) < cfg.min_variant_precision:
                    continue
                if obj(sc) > best_obj:                  # strict: shipped wins ties
                    best, best_obj = (t, mk, sc), obj(sc)
            tab.at[j, 'n_variants'] = m
            tab.at[j, 'variant_gate_z'] = z_var
            if best is not None:
                t, mk, sc = best
                tab.at[j, 'deployed_thresholds'] = t
                tab.at[j, 'threshold_moved'] = True
                masks_tune_tuned[j] = mk
                masks_hold_tuned[j] = rule_mask(hold, rule, t)
        log(f'Tier 2: {total_variants} distinct threshold combinations evaluated across '
            f'{n_admitted} rules (max {max_variants_one} for one rule); '
            f'{int(tab.threshold_moved.sum())} thresholds moved')

    # ── the three arms, filled to budget on tuning years, judged on holdout ──
    def arm(tier: int):
        if tier == 0:
            order = list(tab.rule_idx)                       # delivery rank order
            mt, mh = masks_tune_nat, masks_hold_nat
        else:
            adm = tab[tab.admitted].sort_values('lcb99', ascending=False)
            order = list(adm.rule_idx)
            mt, mh = ((masks_tune_nat, masks_hold_nat) if tier == 1
                      else (masks_tune_tuned, masks_hold_tuned))
        taken, _, cap = fill_to_budget(order, mt, len(tune), cfg.budget)
        ut = np.zeros(len(tune), bool)
        uh = np.zeros(len(hold), bool)
        for j in taken:
            ut |= mt[j]
            uh |= mh[j]
        st, sh = score(ut, err_t, ed_t, *tot_t), score(uh, err_h, ed_h, *tot_h)
        return {'tier': tier, 'n_rules': len(taken), 'taken': taken, 'cap': cap,
                'tune_prec': st['prec'], 'tune_flagged': st['n'],
                'hold_prec': sh['prec'], 'hold_flagged': sh['n'],
                'hold_errors': sh['k'], 'hold_rec': sh['rec'],
                'hold_drec': sh['drec'], 'hold_workload': sh['workload'],
                'hold_prec_lcb': wilson_lcb(sh['k'], sh['n'], cfg.gate_z),
                'deflation': (sh['prec'] / st['prec']) if st['prec'] else np.nan}

    arm_rows = [arm(0)]
    if n_admitted > 0 and cfg.max_tier >= 1:
        arm_rows.append(arm(1))
    if tier2_ok:
        arm_rows.append(arm(2))
    arms = pd.DataFrame(arm_rows)

    # ── the gate: at most one pre-declared comparison per tier ───────────────
    a0 = arm_rows[0]
    tier, reason, n_gate = 0, 'default: no tuning applied', 0
    if tot_h[0] < cfg.min_holdout_errors:
        reason = (f'holdout year has {tot_h[0]} errors, below the {cfg.min_holdout_errors} '
                  'needed to judge a tuned list; shipping the list untuned')
    elif len(arm_rows) == 1:
        reason = 'no arm above Tier 0 was available'
    else:
        for cand in sorted(arm_rows[1:], key=lambda a: -a['tier']):
            n_gate += 1
            if cand['hold_flagged'] < cfg.min_holdout_flagged:
                continue
            if cand['hold_prec_lcb'] > a0['hold_prec']:
                tier = cand['tier']
                reason = (f"holdout precision LCB {cand['hold_prec_lcb']:.3f} beats "
                          f"Tier 0's {a0['hold_prec']:.3f}")
                break
        if tier == 0:
            reason = (f"no tier beat Tier 0 on the holdout (Tier 0 precision "
                      f"{a0['hold_prec']:.3f}); shipping the list untuned")

    chosen = next(a for a in arm_rows if a['tier'] == tier)
    thr_col = 'deployed_thresholds' if tier >= 2 else 'shipped_thresholds'
    deployed = {j: (j in set(chosen['taken'])) for j in tab.rule_idx}
    deployed_thresholds = {int(r.rule_idx): tuple(getattr(r, thr_col))
                           for r in tab.itertuples()}
    if tier < 2:
        tab['threshold_moved'] = False
    tab['deployed'] = tab.rule_idx.map(deployed)
    tab['deployed_thresholds'] = tab.rule_idx.map(deployed_thresholds)

    res = TuningResult(
        tier=tier, tier_reason=reason, cfg=cfg,
        tune_years=tune_years, hold_years=hold_years,
        rules_table=tab, arms=arms.drop(columns=['taken']),
        deployed_thresholds=deployed_thresholds, deployed=deployed,
        comparisons={'tier1_tests': int(tested.size),
                     'tier2_variants': int(total_variants),
                     'tier2_max_per_rule': int(max_variants_one),
                     'holdout': int(n_gate)},
        notes=notes + [
            'per-rule holdout selection is never performed: the holdout decides '
            'whole-list arms only',
            'scored against ANY over-threshold error, not the type a rule was mined '
            'for: a review finds whatever error is present',
        ] + ([
            'expect roughly a third off the tuning-year precision in a future year '
            '(findings section 9): a tuned list was selected on those years, so its '
            'tuning-year numbers are optimistic by construction'
        ] if tier >= 1 else [
            'no deflation from tuning to holdout is expected here: at Tier 0 nothing '
            'was selected on the tuning years, so there is no winner\'s curse to '
            'unwind. The roughly one-third deflation in findings section 9 applies '
            'to a tuned list'
        ]))
    for line in res.audit_lines():
        log(line)
    return res


# ══════════════════════════════════════════════════════════════════════════════
# SELF-CHECK — the guards are only real if they are tested
# ══════════════════════════════════════════════════════════════════════════════
def _selfcheck():
    ok = 0

    def check(name, cond):
        nonlocal ok
        assert cond, f'FAIL: {name}'
        ok += 1
        print(f'  PASS  {name}')

    # Wilson LCB
    check('wilson_lcb(0, 0) == 0', wilson_lcb(0, 0, 2.326) == 0.0)
    check('wilson_lcb below the point estimate', wilson_lcb(20, 100, 2.326) < 0.20)
    check('wilson_lcb tightens with n',
          wilson_lcb(200, 1000, 2.326) > wilson_lcb(20, 100, 2.326))
    check('wilson_lcb tightens as z falls',
          wilson_lcb(20, 100, 1.2816) > wilson_lcb(20, 100, 2.326))

    # binomial tail
    check('binom_sf_ge(0, n, p) == 1', binom_sf_ge(0, 50, 0.1) == 1.0)
    check('binom_sf_ge(n+1) == 0', binom_sf_ge(51, 50, 0.1) == 0.0)
    check('binom_sf_ge full support == 1', abs(binom_sf_ge(1, 50, 0.1) - (1 - 0.9 ** 50)) < 1e-9)
    check('binom_sf_ge monotone in k', binom_sf_ge(20, 50, 0.1) < binom_sf_ge(10, 50, 0.1))

    # BH
    rej, crit = bh_reject([0.001, 0.2, 0.6, 0.9], 0.10)
    check('BH rejects the small p only', list(rej) == [True, False, False, False] and crit == 0.001)
    rej, _ = bh_reject([0.4, 0.5, 0.6], 0.10)
    check('BH rejects nothing when no p is small', not rej.any())
    rej, _ = bh_reject([0.01] * 10, 0.10)
    check('BH rejects all when all p are small', rej.all())
    pv = [0.005, 0.09] + [0.5] * 8
    check('BH is stricter than an unadjusted 0.10 cut',
          int(bh_reject(pv, 0.10)[0].sum()) == 1
          and sum(p <= 0.10 for p in pv) == 2)

    # partition-aware dedup
    obs = np.array([0.0, 100.0, 200.0, 300.0, 400.0])
    g = condition_grid(200.0, '>', (0.75, 0.90, 1.00, 1.10, 1.25), obs)
    check('grid keeps the shipped value', 200.0 in g)
    check('equivalent cuts collapse to one test', len(g) <= 3)
    check('zero threshold is left alone',
          condition_grid(0.0, '>', (0.75, 1.0, 1.25), obs) == [0.0])
    check('never-firing cuts are dropped',
          all(v < obs.max() for v in condition_grid(400.0, '>', (0.9, 1.0, 1.25), obs)))
    check('binary indicators give a single cut',
          len(condition_grid(1.0, '>=', (0.75, 0.9, 1.0, 1.1, 1.25),
                             np.array([0.0, 1.0]))) == 1)

    # the time split refuses a single-year frame and is forward-looking
    cfg = TuningConfig()
    one = pd.DataFrame({'fiscal_year': [2024] * 5})
    try:
        time_split(one, cfg)
        check('single-year frame is refused', False)
    except ValueError:
        check('single-year frame is refused', True)
    three = pd.DataFrame({'fiscal_year': [2022] * 4 + [2023] * 4 + [2024] * 4})
    tm, hm, ty, hy = time_split(three, cfg)
    check('holdout is the most recent year only', ty == [2022, 2023] and hy == [2024])
    check('split partitions the frame', int(tm.sum()) == 8 and int(hm.sum()) == 4)
    tm2, hm2, ty2, hy2 = time_split(three, TuningConfig(holdout_year_set=(2023,)))
    check('explicit holdout year set is honoured', ty2 == [2022, 2024] and hy2 == [2023])
    check('explicit set still partitions the frame',
          int(tm2.sum()) == 8 and int(hm2.sum()) == 4)
    try:
        time_split(three, TuningConfig(holdout_year_set=(2019,)))
        check('holdout_year_set naming an absent year is refused', False)
    except ValueError:
        check('holdout_year_set naming an absent year is refused', True)
    try:
        time_split(three, TuningConfig(holdout_year_set=(2022, 2023, 2024)))
        check('holdout_year_set leaving no tuning years is refused', False)
    except ValueError:
        check('holdout_year_set leaving no tuning years is refused', True)

    # budget fill is outcome-free and respects the cap
    masks = {0: np.array([True] * 10 + [False] * 90),
             1: np.array([False] * 50 + [True] * 50),
             2: np.array([True] * 5 + [False] * 95)}
    taken, skipped, cap = fill_to_budget([0, 1, 2], masks, 100, 0.10)
    check('fill respects the cap', cap == 10 and taken == [0] and 1 in skipped)
    check('fill skips rules adding nothing new',
          fill_to_budget([0, 0], masks, 100, 0.10)[0] == [0])

    # end to end on a synthetic frame with a real signal
    rng = np.random.default_rng(7)
    n = 4000
    inc = rng.uniform(0, 1200, n)
    frame = pd.DataFrame({
        'fiscal_year': rng.choice([2022, 2023, 2024], n),
        'hh_group': '1',
        'rawearn_by_hh_size': inc,
        'utilities': rng.uniform(0, 900, n),
        'total_error_amount': rng.uniform(50, 500, n),
    })
    p = np.where(inc > 600, 0.45, 0.05)
    frame['over_threshold'] = (rng.uniform(size=n) < p).astype(int)
    rules = [{'num': 1, 'hh': '1',
              'conds': [{'var': 'rawearn_by_hh_size', 'op': '>', 'thr': 500.0}]},
             {'num': 2, 'hh': '1',
              'conds': [{'var': 'utilities', 'op': '>', 'thr': 400.0}]}]
    res = run(frame, rules, TuningConfig(budget=0.30, min_admitted_for_tier2=1,
                                         min_holdout_errors=10), log=lambda *_: None)
    t = res.rules_table
    check('the real rule is admitted', bool(t.loc[t.rule_num == 1, 'admitted'].iloc[0]))
    check('the noise rule is not admitted', not bool(t.loc[t.rule_num == 2, 'admitted'].iloc[0]))
    check('a tier was chosen and reported', res.tier in (0, 1, 2) and res.tier_reason)
    check('Tier 0 is always computed', 0 in set(res.arms.tier))
    check('comparison counts are reported', res.comparisons['tier1_tests'] >= 1)
    check('deployed thresholds cover every rule', len(res.deployed_thresholds) == len(rules))
    check('holdout comparisons are at most one per tier above 0',
          res.comparisons['holdout'] <= 2)

    # max_tier is a real ceiling
    res0 = run(frame, rules, TuningConfig(max_tier=0, budget=0.30), log=lambda *_: None)
    check('max_tier=0 deploys Tier 0', res0.tier == 0)
    check('max_tier=0 moves no thresholds', not res0.rules_table.threshold_moved.any())

    print(f'\n{ok} checks PASS')


if __name__ == '__main__':
    import sys
    if '--selfcheck' in sys.argv:
        _selfcheck()
    else:
        print(__doc__)
