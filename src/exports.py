"""Analysis tables for the app, recomputed from data/processed/.

data/processed/ is gitignored, so the hosted app cannot read it. src/comparator.py
already exports the NB03/NB04 pressure summaries; this module exports everything
else the app needs: the match-level table itself, the pre-Arteta baseline, and
every statistical result from NB05.

Nothing here is scraped out of notebook JSON. Every number is recomputed from
data/processed/all_4teams_processed.csv with the same formulas the notebooks use,
and main() prints a verification block against the values already documented in
PROJECT_MASTER_CONTEXT.md and decisions_log.md.

Run with `python -m src.exports`. Not part of the daily update job: none of this
changes gameweek to gameweek, only the live simulation does.
"""
import numpy as np
import pandas as pd
from scipy import stats

from . import config

PROCESSED_PATH = config.DATA_PROCESSED_DIR / "all_4teams_processed.csv"
PRE_ARTETA_PATH = config.DATA_RAW_DIR / "arsenal_pre_arteta_raw.csv"

N_PERM = 10000
N_BOOT = 10000
PERM_SEED = 11
BOOT_SEED = 12
RUN_IN_START = 29

# The four seasons NB03's killer-matches audit covers: Arsenal's actual title
# challenges. Wider than this is 2021-22, which clears the >=5-high-stakes
# contention filter on a top-four push but was never a title race.
TITLE_CHALLENGE_SEASONS = ["2223", "2324", "2425", "2526"]


def load_processed() -> pd.DataFrame:
    df = pd.read_csv(PROCESSED_PATH)
    df["season"] = df["season"].astype(str)
    df["date"] = pd.to_datetime(df["date"])
    return df


def add_drop_index(df: pd.DataFrame) -> pd.DataFrame:
    """Per-match Drop Index, NB03 cell 7. Baselines are that team-season's own
    mean xG and xGA. Positive means underperformed (created less, conceded more).
    NB03 groups by season alone because it runs on Arsenal only; here the same
    formula has to group by team and season."""
    df = df.copy()
    g = df.groupby(["team", "season"])
    season_xg = g["xG"].transform("mean")
    season_xga = g["xGA"].transform("mean")
    df["match_drop_index"] = ((season_xg - df["xG"]) + (df["xGA"] - season_xga)) / 2
    return df


def build_matches(df: pd.DataFrame) -> pd.DataFrame:
    """The match-level table the app needs for every per-match chart."""
    out = df.copy()
    out["is_big6_opp"] = out["opponent"].isin(config.BIG6).astype(int)
    out["stake_label"] = out["is_high_stakes_retro"].map({True: "High Stakes", False: "Normal"})
    out["is_run_in"] = (out["gameweek"] >= RUN_IN_START).astype(int)
    cols = [
        "season", "game_id", "date", "team", "opponent", "venue", "gameweek",
        "xG", "xGA", "xgd", "scored", "conceded", "gd", "result", "points",
        "cum_pts", "leader_pts", "title_gap", "stakes_intensity",
        "is_high_stakes", "is_high_stakes_retro", "stake_label", "is_rivalry",
        "is_big6_opp", "is_run_in", "is_win", "match_drop_index",
        "xG_z", "xGA_z", "xG_roll5", "xGA_roll5", "pts_roll5", "win_rate_roll5",
    ]
    return out[cols].sort_values(["team", "date"]).reset_index(drop=True)


def build_pre_arteta() -> pd.DataFrame:
    """2017-19 Arsenal under Wenger/Emery, the baseline Arteta inherited
    (NB03 Chart A1). Raw scrape, so points/xgd are derived here."""
    pre = pd.read_csv(PRE_ARTETA_PATH)
    pre["season"] = pre["season"].astype(str)
    pre["xgd"] = pre["xG"] - pre["xGA"]
    pre["points"] = np.where(pre["scored"] > pre["conceded"], 3,
                             np.where(pre["scored"] == pre["conceded"], 1, 0))
    pre["is_win"] = (pre["scored"] > pre["conceded"]).astype(float)
    return pre[["season", "date", "team", "opponent", "venue", "xG", "xGA",
                "xgd", "scored", "conceded", "points", "is_win"]]


def build_era_comparison(df: pd.DataFrame, pre: pd.DataFrame) -> pd.DataFrame:
    """Pre-Arteta vs Arteta era, the numbers behind NB03 Chart A1."""
    ars = df[df["team"] == "Arsenal"]
    rows = []
    for label, frame in [("Pre-Arteta (2017-19)", pre), ("Arteta era (2019-26)", ars)]:
        rows.append({
            "era": label,
            "matches": len(frame),
            "xG": frame["xG"].mean(),
            "xGA": frame["xGA"].mean(),
            "xGD": frame["xG"].mean() - frame["xGA"].mean(),
            "ppg": frame["points"].mean(),
            "win_rate": frame["is_win"].mean(),
        })
    return pd.DataFrame(rows)


# ---------------------------------------------------------------------------
# NB05 Section A: normality
# ---------------------------------------------------------------------------

def build_normality(df: pd.DataFrame) -> pd.DataFrame:
    rows = []
    for col in ["points", "xG", "xGA", "match_drop_index"]:
        stat, p = stats.shapiro(df[col].dropna())
        rows.append({
            "variable": col,
            "shapiro_W": stat,
            "p_value": p,
            "normal_at_05": bool(p > 0.05),
            "n": int(df[col].notna().sum()),
        })
    return pd.DataFrame(rows)


# ---------------------------------------------------------------------------
# NB05 Section B: the xG/xGA family of 8 tests, with corrections
# ---------------------------------------------------------------------------

def _bonferroni(p: np.ndarray) -> np.ndarray:
    return np.minimum(p * len(p), 1.0)


def _benjamini_hochberg(p: np.ndarray) -> np.ndarray:
    n = len(p)
    order = np.argsort(p)
    ranked = p[order] * n / (np.arange(n) + 1)
    # enforce monotonicity from the largest p downwards
    ranked = np.minimum.accumulate(ranked[::-1])[::-1]
    out = np.empty(n)
    out[order] = np.minimum(ranked, 1.0)
    return out


def build_stakes_tests(df: pd.DataFrame) -> pd.DataFrame:
    """Welch's t-test and Mann-Whitney U for xG and xGA, high-stakes-retro vs
    normal, per team. Eight tests, then Bonferroni and Benjamini-Hochberg across
    the whole family (NB05 Section B)."""
    rows = []
    for team in config.TITLE_TEAMS:
        t = df[df["team"] == team]
        hs, nm = t[t["is_high_stakes_retro"]], t[~t["is_high_stakes_retro"]]
        for metric in ["xG", "xGA"]:
            welch = stats.ttest_ind(hs[metric], nm[metric], equal_var=False)
            mwu = stats.mannwhitneyu(hs[metric], nm[metric], alternative="two-sided")
            pooled_sd = np.sqrt((hs[metric].var(ddof=1) + nm[metric].var(ddof=1)) / 2)
            rows.append({
                "team": team, "metric": metric,
                "high_stakes_mean": hs[metric].mean(),
                "normal_mean": nm[metric].mean(),
                "difference": hs[metric].mean() - nm[metric].mean(),
                "cohens_d": (hs[metric].mean() - nm[metric].mean()) / pooled_sd,
                "p_welch": welch.pvalue,
                "p_mannwhitney": mwu.pvalue,
                "n_high_stakes": len(hs), "n_normal": len(nm),
            })
    out = pd.DataFrame(rows)
    p = out["p_welch"].to_numpy()
    out["p_bonferroni"] = _bonferroni(p)
    out["p_benjamini_hochberg"] = _benjamini_hochberg(p)
    out["significant_raw"] = out["p_welch"] < 0.05
    out["significant_corrected"] = out["p_bonferroni"] < 0.05
    return out


# ---------------------------------------------------------------------------
# NB05 Section C: the Bottle Gap, tested two ways
# ---------------------------------------------------------------------------

def build_permutation(df: pd.DataFrame) -> pd.DataFrame:
    """For each team-season, shuffle which matches count as high stakes and see
    how extreme the real PPG gap looks against that null (NB05 Section C)."""
    rng = np.random.default_rng(PERM_SEED)
    rows = []
    for (team, season), grp in df.groupby(["team", "season"]):
        pts = grp["points"].to_numpy(dtype=float)
        flag = grp["is_high_stakes_retro"].to_numpy(dtype=bool)
        k, n = int(flag.sum()), len(pts)
        if k == 0 or k == n:
            continue
        observed = pts[flag].mean() - pts.mean()

        # one shuffled selection of k matches per resample
        idx = np.argsort(rng.random((N_PERM, n)), axis=1)[:, :k]
        null = pts[idx].mean(axis=1) - pts.mean()
        p_two_sided = float((np.abs(null) >= abs(observed)).mean())

        rows.append({
            "team": team, "season": season,
            "n_high_stakes": k, "n_matches": n,
            "high_stakes_ppg": pts[flag].mean(),
            "baseline_ppg": pts.mean(),
            "observed_gap": observed,
            "null_mean": float(null.mean()),
            "null_p5": float(np.percentile(null, 5)),
            "null_p95": float(np.percentile(null, 95)),
            "p_value": p_two_sided,
        })
    out = pd.DataFrame(rows)
    out["p_bonferroni"] = _bonferroni(out["p_value"].to_numpy())
    out["significant_raw"] = out["p_value"] < 0.05
    out["significant_corrected"] = out["p_bonferroni"] < 0.05
    return out


def build_wilcoxon(perm: pd.DataFrame) -> pd.DataFrame:
    """Season-level Wilcoxon signed-rank on the per-season gaps, which avoids
    treating 38 matches in one season as 38 independent observations
    (NB05 Section C)."""
    rows = []
    for team in config.TITLE_TEAMS:
        gaps = perm.loc[perm["team"] == team, "observed_gap"].to_numpy()
        res = stats.wilcoxon(gaps)
        rows.append({"scope": team, "n_seasons": len(gaps), "mean_gap": gaps.mean(),
                     "median_gap": float(np.median(gaps)),
                     "statistic": float(res.statistic), "p_value": float(res.pvalue)})
    all_gaps = perm["observed_gap"].to_numpy()
    res = stats.wilcoxon(all_gaps)
    rows.append({"scope": "Pooled (all 4 teams)", "n_seasons": len(all_gaps),
                 "mean_gap": all_gaps.mean(), "median_gap": float(np.median(all_gaps)),
                 "statistic": float(res.statistic), "p_value": float(res.pvalue)})
    return pd.DataFrame(rows)


def _overlap_once(df: pd.DataFrame, tiebreak: str, rng=None):
    """Average overlap between a team-season's high-stakes matches and its
    best / middle / worst results, under one tie-breaking rule."""
    worst, mid, best, n_seasons = [], [], [], 0
    for _, grp in df.groupby(["team", "season"]):
        g = grp.reset_index(drop=True)
        pts = g["points"].to_numpy()
        k = int(g["is_high_stakes_retro"].sum())
        if k == 0:
            continue
        actual = set(g.index[g["is_high_stakes_retro"]].to_numpy())

        if tiebreak == "chronological":
            order = np.argsort(pts, kind="stable")
        elif tiebreak == "reverse_chronological":
            order = (len(pts) - 1) - np.argsort(pts[::-1], kind="stable")
        else:
            order = np.lexsort((rng.random(len(pts)), pts))

        w_set, b_set = set(order[:k]), set(order[-k:])
        m_set = set(range(len(pts))) - w_set - b_set
        worst.append(len(actual & w_set))
        mid.append(len(actual & m_set))
        best.append(len(actual & b_set))
        n_seasons += 1
    return np.mean(worst), np.mean(mid), np.mean(best), n_seasons


def build_stakes_overlap(df: pd.DataFrame) -> pd.DataFrame:
    """NB05 Section C asked whether high-stakes matches overlap a team's best
    results more than chance, and concluded they do (5.89 against 2.63).

    That conclusion does not hold. `points` only takes three values, so "the
    best 10 results" of a 38-match season is decided almost entirely by how
    ties are broken, and NB05's stable sort breaks them chronologically. High-
    stakes matches fall late in a season (mean gameweek 31.0 against 15.4), so
    a chronological tie-break puts them in the "best" group by construction.
    Reversing the tie-break flips the result to 0.39, and averaging over random
    tie-breaks lands on the chance baseline. This table exports all three so
    the app can show the artifact rather than repeat the claim.
    """
    rng = np.random.default_rng(PERM_SEED)
    rows = []
    schemes = [
        ("chronological", "Ties broken chronologically (NB05's original)"),
        ("reverse_chronological", "Ties broken reverse-chronologically"),
    ]
    for key, label in schemes:
        w, m, b, n = _overlap_once(df, key)
        rows.append({"tiebreak": key, "tiebreak_label": label,
                     "worst": w, "middle": m, "best": b, "n_team_seasons": n})

    draws = np.array([_overlap_once(df, "random", rng)[:3] for _ in range(200)])
    w, m, b = draws.mean(axis=0)
    rows.append({"tiebreak": "random", "tiebreak_label": "Ties broken at random (200 draws)",
                 "worst": w, "middle": m, "best": b, "n_team_seasons": rows[0]["n_team_seasons"]})

    out = pd.DataFrame(rows)
    out["chance_worst"] = 10 * 10 / 38
    out["chance_middle"] = 10 * 18 / 38
    out["chance_best"] = 10 * 10 / 38
    return out


def build_stakes_bias_check(df: pd.DataFrame) -> pd.DataFrame:
    """The tie-free version of the same question. Win rate and points in
    high-stakes matches against normal ones, which needs no sorting and so
    cannot be distorted by ties."""
    rows = []
    for team in list(config.TITLE_TEAMS) + ["All four teams"]:
        t = df if team == "All four teams" else df[df["team"] == team]
        hs, nm = t[t["is_high_stakes_retro"]], t[~t["is_high_stakes_retro"]]
        rows.append({
            "team": team,
            "win_rate_high_stakes": hs["is_win"].mean(),
            "win_rate_normal": nm["is_win"].mean(),
            "win_rate_diff": hs["is_win"].mean() - nm["is_win"].mean(),
            "ppg_high_stakes": hs["points"].mean(),
            "ppg_normal": nm["points"].mean(),
            "ppg_diff": hs["points"].mean() - nm["points"].mean(),
            "mean_gameweek_high_stakes": hs["gameweek"].mean(),
            "mean_gameweek_normal": nm["gameweek"].mean(),
            "n_high_stakes": len(hs), "n_normal": len(nm),
        })
    return pd.DataFrame(rows)


# ---------------------------------------------------------------------------
# NB05 Sections D-F: cross-team, bootstrap, power
# ---------------------------------------------------------------------------

def build_kruskal(perm: pd.DataFrame) -> pd.DataFrame:
    groups = [perm.loc[perm["team"] == t, "observed_gap"].to_numpy() for t in config.TITLE_TEAMS]
    h, p = stats.kruskal(*groups)
    return pd.DataFrame([{
        "test": "Kruskal-Wallis across 4 teams' season-level PPG gaps",
        "H_statistic": h, "p_value": p, "n_groups": len(groups),
        "n_total": sum(len(g) for g in groups),
    }])


def build_bootstrap_ci(df: pd.DataFrame, pressure: pd.DataFrame) -> pd.DataFrame:
    """Bootstrap 95% CI around each team's Pressure Resilience Index
    (NB05 Section E). Resamples that team's contention seasons."""
    rng = np.random.default_rng(BOOT_SEED)
    rows = []
    for team in config.TITLE_TEAMS:
        gaps = pressure.loc[
            (pressure["team"] == team) & (pressure["is_contention_season"]), "ppg_gap"
        ].dropna().to_numpy()
        if len(gaps) == 0:
            continue
        draws = rng.choice(gaps, size=(N_BOOT, len(gaps)), replace=True).mean(axis=1)
        rows.append({
            "team": team,
            "pressure_resilience_index": gaps.mean(),
            "ci_low": float(np.percentile(draws, 2.5)),
            "ci_high": float(np.percentile(draws, 97.5)),
            "n_contention_seasons": len(gaps),
            "includes_zero": bool(np.percentile(draws, 2.5) <= 0 <= np.percentile(draws, 97.5)),
        })
    return pd.DataFrame(rows)


def build_power(df: pd.DataFrame) -> pd.DataFrame:
    """Minimum detectable effect size at the sample sizes actually available,
    against the effect sizes actually observed (NB05 Section F)."""
    from statsmodels.stats.power import TTestIndPower

    n_hs, n_norm = 10, 28
    mdes = TTestIndPower().solve_power(
        effect_size=None, nobs1=n_hs, ratio=n_norm / n_hs, alpha=0.05, power=0.8
    )
    rows = []
    for team in config.TITLE_TEAMS:
        t = df[df["team"] == team]
        hs, nm = t[t["is_high_stakes_retro"]], t[~t["is_high_stakes_retro"]]
        for metric in ["xG", "xGA", "points"]:
            pooled_sd = np.sqrt((hs[metric].var(ddof=1) + nm[metric].var(ddof=1)) / 2)
            d = abs(hs[metric].mean() - nm[metric].mean()) / pooled_sd
            rows.append({
                "team": team, "metric": metric, "observed_cohens_d": d,
                "min_detectable_d": mdes,
                "detectable": bool(d >= mdes),
                "n_high_stakes_per_season": n_hs, "n_normal_per_season": n_norm,
            })
    return pd.DataFrame(rows)


# ---------------------------------------------------------------------------
# NB03/NB04 tables the app shows directly
# ---------------------------------------------------------------------------

def build_killer_matches(df: pd.DataFrame, pressure: pd.DataFrame) -> pd.DataFrame:
    """Run-in (GW29+) high-stakes matches where points were dropped, for all
    four teams. NB03 audits Arsenal's four contention seasons and finds 13;
    is_contention_season lets the app reproduce exactly that subset or widen it."""
    mask = (
        (df["gameweek"] >= RUN_IN_START)
        & df["is_high_stakes_retro"]
        & (df["result"] != "W")
    )
    cols = ["team", "season", "gameweek", "date", "opponent", "venue",
            "scored", "conceded", "result", "points", "xG", "xGA",
            "match_drop_index", "stakes_intensity"]
    out = df.loc[mask, cols].copy()
    out["overperformed"] = out["match_drop_index"] < 0

    contention = pressure.set_index(["team", "season"])["is_contention_season"]
    keys = pd.MultiIndex.from_frame(out[["team", "season"]])
    out["is_contention_season"] = contention.reindex(keys).fillna(False).to_numpy()
    out["is_title_challenge"] = out["season"].isin(TITLE_CHALLENGE_SEASONS)
    return out.sort_values(["team", "season", "gameweek"]).reset_index(drop=True)


def build_opponent_tier(df: pd.DataFrame) -> pd.DataFrame:
    """PPG gap under pressure split by Big 6 vs everyone else (NB04 Chart E1).
    The gap is high-stakes PPG minus NORMAL PPG within that tier, not minus the
    tier's overall average, matching NB04's pivot."""
    rows = []
    for team in config.TITLE_TEAMS:
        t = df[df["team"] == team]
        for tier_name, tier_mask in [("Big 6", t["is_big6_opp"] == 1), ("Rest", t["is_big6_opp"] == 0)]:
            sub = t[tier_mask]
            hs = sub[sub["is_high_stakes_retro"]]
            nm = sub[~sub["is_high_stakes_retro"]]
            if len(hs) == 0 or len(nm) == 0:
                continue
            rows.append({
                "team": team, "opponent_tier": tier_name,
                "normal_ppg": nm["points"].mean(),
                "high_stakes_ppg": hs["points"].mean(),
                "ppg_gap": hs["points"].mean() - nm["points"].mean(),
                "n_matches": len(sub), "n_high_stakes": len(hs),
            })
    return pd.DataFrame(rows)


def build_season_table(df: pd.DataFrame) -> pd.DataFrame:
    """Per team-season headline table: final points, xG/xGA, run-in record.
    Feeds the story pages' summary tables."""
    rows = []
    for (team, season), grp in df.groupby(["team", "season"]):
        run_in = grp[grp["gameweek"] >= RUN_IN_START]
        hs = grp[grp["is_high_stakes_retro"]]
        rows.append({
            "team": team, "season": season,
            "points": int(grp["points"].sum()),
            "wins": int((grp["result"] == "W").sum()),
            "draws": int((grp["result"] == "D").sum()),
            "losses": int((grp["result"] == "L").sum()),
            "goals_for": int(grp["scored"].sum()), "goals_against": int(grp["conceded"].sum()),
            "xG": grp["xG"].mean(), "xGA": grp["xGA"].mean(),
            "xGD_per_match": grp["xgd"].mean(),
            "ppg": grp["points"].mean(),
            "run_in_points": int(run_in["points"].sum()),
            "run_in_available": len(run_in) * 3,
            "run_in_ppg": run_in["points"].mean(),
            "high_stakes_ppg": hs["points"].mean(),
            "mean_stakes_in_high_bucket": hs["stakes_intensity"].mean(),
        })
    return pd.DataFrame(rows).sort_values(["team", "season"])


# ---------------------------------------------------------------------------

def main() -> None:
    from .comparator import build_team_pressure_table

    df = add_drop_index(load_processed())
    df["is_big6_opp"] = df["opponent"].isin(config.BIG6).astype(int)
    pre = build_pre_arteta()
    pressure = build_team_pressure_table()
    pressure["season"] = pressure["season"].astype(str)

    perm = build_permutation(df)

    outputs = {
        "matches.csv": build_matches(df),
        "pre_arteta.csv": pre,
        "era_comparison.csv": build_era_comparison(df, pre),
        "season_table.csv": build_season_table(df),
        "killer_matches.csv": build_killer_matches(df, pressure),
        "opponent_tier.csv": build_opponent_tier(df),
        "normality.csv": build_normality(df),
        "stakes_tests.csv": build_stakes_tests(df),
        "permutation.csv": perm,
        "wilcoxon.csv": build_wilcoxon(perm),
        "stakes_overlap.csv": build_stakes_overlap(df),
        "stakes_bias_check.csv": build_stakes_bias_check(df),
        "kruskal.csv": build_kruskal(perm),
        "bootstrap_ci.csv": build_bootstrap_ci(df, pressure),
        "power.csv": build_power(df),
    }

    config.DATA_APP_DIR.mkdir(parents=True, exist_ok=True)
    for name, frame in outputs.items():
        frame.to_csv(config.DATA_APP_DIR / name, index=False)
        print(f"wrote {name:<22} {frame.shape[0]:>5} rows x {frame.shape[1]:>2} cols")

    _verify(df, outputs)


def _verify(df: pd.DataFrame, out: dict) -> None:
    """Check recomputed values against what the project docs already claim.
    Any MISMATCH here means either the docs or this module is wrong, and it
    has to be resolved before the app ships the number."""
    print("\nverification against documented values")
    print("-" * 64)

    def check(label, got, want, tol=0.005):
        ok = abs(got - want) <= tol
        print(f"  {'OK  ' if ok else 'MISS'}  {label:<44} {got:.3f} (doc {want:.3f})")
        return ok

    tests = out["stakes_tests.csv"].set_index(["team", "metric"])
    check("Arsenal xG p-value", tests.loc[("Arsenal", "xG"), "p_welch"], 0.665)
    check("Arsenal xGA p-value", tests.loc[("Arsenal", "xGA"), "p_welch"], 0.031)
    check("Man Utd xG p-value", tests.loc[("Manchester United", "xG"), "p_welch"], 0.241)
    check("Man Utd xGA p-value", tests.loc[("Manchester United", "xGA"), "p_welch"], 0.032)
    check("Arsenal high-stakes xG", tests.loc[("Arsenal", "xG"), "high_stakes_mean"], 1.84, 0.02)
    check("Arsenal normal xG", tests.loc[("Arsenal", "xG"), "normal_mean"], 1.78, 0.02)
    check("Arsenal high-stakes xGA", tests.loc[("Arsenal", "xGA"), "high_stakes_mean"], 1.32, 0.02)
    check("Arsenal normal xGA", tests.loc[("Arsenal", "xGA"), "normal_mean"], 1.05, 0.02)
    print(f"  ..    none survive Bonferroni: "
          f"{not out['stakes_tests.csv']['significant_corrected'].any()}")

    kw = out["kruskal.csv"].iloc[0]
    check("Kruskal-Wallis H", kw["H_statistic"], 0.449, 0.02)
    check("Kruskal-Wallis p", kw["p_value"], 0.930, 0.02)

    ov = out["stakes_overlap.csv"].set_index("tiebreak")
    check("overlap best, NB05's tie-break", ov.loc["chronological", "best"], 5.89, 0.05)
    check("overlap middle, NB05's tie-break", ov.loc["chronological", "middle"], 2.04, 0.05)
    check("overlap worst, NB05's tie-break", ov.loc["chronological", "worst"], 2.07, 0.05)
    print(f"  ..    same figure, reversed tie-break:            "
          f"{ov.loc['reverse_chronological', 'best']:.2f} best")
    print(f"  ..    same figure, random tie-break:              "
          f"{ov.loc['random', 'best']:.2f} best (chance {ov.loc['random', 'chance_best']:.2f})")
    bias = out["stakes_bias_check.csv"].set_index("team")
    print(f"  ..    tie-free check, win rate difference:        "
          f"{bias.loc['All four teams', 'win_rate_diff']:+.3f}")

    pw = out["power.csv"].iloc[0]
    check("minimum detectable d", pw["min_detectable_d"], 1.06, 0.02)

    wil = out["wilcoxon.csv"].set_index("scope")
    check("Wilcoxon pooled p", wil.loc["Pooled (all 4 teams)", "p_value"], 0.67, 0.03)

    perm = out["permutation.csv"]
    n_sig = int(perm["significant_raw"].sum())
    print(f"  {'OK  ' if n_sig == 1 else 'MISS'}  {'permutation: significant team-seasons':<44} "
          f"{n_sig} (doc 1)")

    km = out["killer_matches.csv"]
    ars_km = km[(km["team"] == "Arsenal") & km["is_title_challenge"]]
    n_neg = int((ars_km["match_drop_index"] < 0).sum())
    print(f"  {'OK  ' if len(ars_km) == 13 else 'MISS'}  {'Arsenal killer matches':<44} "
          f"{len(ars_km)} (doc 13)")
    print(f"  {'OK  ' if n_neg == 2 else 'MISS'}  {'  of which negative Drop Index':<44} "
          f"{n_neg} (doc 2)")

    named = {
        ("2223", "Liverpool"): 1.964,
        ("2425", "Crystal Palace"): 0.734,
        ("2526", "Manchester City"): 0.746,
    }
    for (season, opp), want in named.items():
        row = ars_km[(ars_km["season"] == season) & (ars_km["opponent"] == opp)]
        if len(row):
            check(f"DI {season} vs {opp}", row["match_drop_index"].iloc[0], want, 0.01)
        else:
            print(f"  MISS  DI {season} vs {opp}: not found in killer matches")

    tier = out["opponent_tier.csv"].set_index(["team", "opponent_tier"])
    check("Liverpool Big 6 PPG gap", tier.loc[("Liverpool", "Big 6"), "ppg_gap"], -0.288, 0.01)
    check("Arsenal Big 6 PPG gap", tier.loc[("Arsenal", "Big 6"), "ppg_gap"], 0.020, 0.01)
    check("Man City Big 6 PPG gap", tier.loc[("Manchester City", "Big 6"), "ppg_gap"], 0.080, 0.01)


if __name__ == "__main__":
    main()
