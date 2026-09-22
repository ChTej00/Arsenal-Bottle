"""The Monte Carlo simulation engine.

Mirrors notebooks/07_monte_carlo_simulation.ipynb Sections H and I: build the
remaining-fixture table from a frozen form snapshot, turn a fixture plus a
stakes value into win/draw/loss probabilities, and simulate a season by
drawing one uniform number per fixture per run.

The only addition beyond the notebook is `overrides` in run_simulation, for
the app's what-if control (fix a chosen fixture's result, then resimulate).
It is applied after the random draw for every fixture, so it changes nothing
about how the RNG stream is consumed: with no overrides, results are bit-for-
bit identical to the notebook's engine at a given seed.
"""
import numpy as np
import pandas as pd

from . import config


def last3_form(season_df: pd.DataFrame) -> dict:
    """Each team's mean points over its last 3 played matches. Used for the
    form_mult term in stakes_intensity, held fixed for the rest of the season."""
    played = season_df[season_df["scored"].notna()].sort_values(["team", "gameweek"])
    return played.groupby("team")["points"].apply(lambda x: x.tail(3).mean()).to_dict()


def is_rivalry_pair(team: str, opp: str) -> bool:
    return opp in config.RIVALRIES.get(team, [])


def build_fixtures(remaining_df: pd.DataFrame, snap: pd.DataFrame, recent_form: dict, t_idx: dict) -> pd.DataFrame:
    """Every remaining fixture, described from the home team's side: frozen
    form snapshot for both sides, head-to-head record, and the static half of
    stakes_intensity's ingredients (form_mult, rivalry_bonus). stakes_intensity
    itself is NOT computed here, it is recomputed every simulated gameweek
    from the simulated table (see run_simulation)."""
    home_rows = remaining_df[remaining_df["venue"] == "home"][
        ["game_id", "gameweek", "date", "team", "opponent", "h2h_pts_avg3"]
    ].rename(columns={"team": "home_team", "opponent": "away_team"})
    fx = home_rows.sort_values(["gameweek", "date"]).reset_index(drop=True)
    fx["home_idx"] = fx["home_team"].map(t_idx)
    fx["away_idx"] = fx["away_team"].map(t_idx)

    static_rows = []
    for _, r in fx.iterrows():
        own, opp = snap.loc[r["home_team"]], snap.loc[r["away_team"]]
        rivalry = is_rivalry_pair(r["home_team"], r["away_team"])
        static_rows.append({
            "xG_roll5": own["xG_roll5"], "xGA_roll5": own["xGA_roll5"],
            "pts_roll5": own["pts_roll5"], "win_rate_roll5": own["win_rate_roll5"],
            "is_home": 1, "is_big6_opp": int(r["away_team"] in config.BIG6),
            "opp_xgd_roll5": opp["xgd_roll5"], "parity_gap": abs(own["xgd_roll5"] - opp["xgd_roll5"]),
            "is_non_big6_rivalry": int(rivalry and r["away_team"] not in config.BIG6),
            "form_mult": config.FORM_MULT_BONUS if recent_form[r["home_team"]] < config.FORM_POOR_THRESHOLD else 1.0,
            "rivalry_bonus": config.RIVALRY_BONUS if rivalry else 0.0,
        })
    return pd.concat([fx, pd.DataFrame(static_rows)], axis=1)


def fixture_probs(fx: pd.DataFrame, stakes: np.ndarray, fitted) -> np.ndarray:
    """P(loss), P(draw), P(win) for the home team in each fixture, one row per
    simulated season. stakes has shape (n_runs, n_fixtures); everything else
    about a fixture is fixed."""
    n_runs, n_fx = stakes.shape
    static = fx[config.STATIC_COLS].to_numpy(dtype=float)
    X = np.empty((n_runs, n_fx, len(fitted.features)))
    for k, col in enumerate(fitted.features):
        X[:, :, k] = stakes if col == "stakes_intensity" else static[:, config.STATIC_COLS.index(col)]
    flat = pd.DataFrame(X.reshape(-1, len(fitted.features)), columns=fitted.features)
    return fitted.model.predict_proba(fitted.scaler.transform(flat)).reshape(n_runs, n_fx, 3)


def run_simulation(fx: pd.DataFrame, start_pts: np.ndarray, fitted, n_runs: int = config.N_RUNS,
                    seed: int = config.LIVE_SEED, overrides: dict | None = None) -> np.ndarray:
    """Simulate n_runs seasons from the given starting points. `overrides`,
    if given, is {game_id: outcome} with outcome in {0=loss,1=draw,2=win} for
    the home team, forced across every run for that one fixture. Applied after
    the random draw, so it never changes how the RNG stream is consumed."""
    overrides = overrides or {}
    sim_rng = np.random.default_rng(seed)
    pts = np.tile(start_pts, (n_runs, 1)).astype(float)

    for gw in sorted(fx["gameweek"].unique()):
        gfx = fx[fx["gameweek"] == gw]
        home_i, away_i = gfx["home_idx"].to_numpy(), gfx["away_idx"].to_numpy()

        ranked = np.sort(pts, axis=1)[:, ::-1]
        leader, p4, p5, p6, p7, p17, p18 = [ranked[:, k][:, None] for k in (0, 3, 4, 5, 6, 16, 17)]
        tp = pts[:, home_i]

        title_gap = leader - tp
        cl_gap = np.where(tp >= p4, p4 - p5, p4 - tp)
        eur_gap = np.where(tp >= p6, p6 - p7, p6 - tp)
        releg_gap = np.where(tp >= p17, tp - p18, p17 - tp)

        gwf = 1 / (1 + np.exp(-config.GW_SIGMOID_K * (gw - config.GW_SIGMOID_MID)))
        ptsrem = (38 - gw + 1) * 3
        f = lambda gap: np.clip(1 - gap / ptsrem, 0, None)
        base = np.maximum.reduce([
            gwf * f(title_gap),
            gwf * f(cl_gap) * config.CL_WEIGHT,
            gwf * f(eur_gap) * config.EUR_WEIGHT,
            gwf * f(releg_gap) * config.RELEG_WEIGHT,
        ]) / config.GW38_MAX
        stakes = np.clip(base * gfx["form_mult"].to_numpy() + gfx["rivalry_bonus"].to_numpy(), 0, 1)

        probs = fixture_probs(gfx, stakes, fitted)
        u = sim_rng.random(stakes.shape)
        outcome = np.where(u < probs[:, :, 0], 0, np.where(u < probs[:, :, 0] + probs[:, :, 1], 1, 2))

        if overrides:
            for j, gid in enumerate(gfx["game_id"].to_numpy()):
                if gid in overrides:
                    outcome[:, j] = overrides[gid]

        delta = np.zeros_like(pts)
        for j in range(len(gfx)):
            o = outcome[:, j]
            delta[o == 2, home_i[j]] += 3
            delta[o == 1, home_i[j]] += 1
            delta[o == 1, away_i[j]] += 1
            delta[o == 0, away_i[j]] += 3
        pts = pts + delta
    return pts


def live_snapshot(feat: pd.DataFrame, season: str = config.CURRENT_SEASON) -> dict:
    """Freeze the current season as of 'today': each team's next unplayed
    fixture gives its form snapshot, and every row with no result yet is a
    remaining fixture. Matches NB07 Section G/H. No h2h hiding is needed here,
    unplayed rows are genuinely NaN, not real future results to hide."""
    season_df = feat[feat["season"] == season].copy()
    teams = sorted(season_df["team"].unique())
    t_idx = {t: i for i, t in enumerate(teams)}

    next_fixture = (season_df[season_df["scored"].isna()]
                    .sort_values(["team", "gameweek"]).groupby("team").first().reset_index())
    snap = next_fixture.set_index("team")[config.SNAP_COLS]

    current_row = (season_df[season_df["scored"].notna()]
                   .sort_values(["team", "gameweek"]).groupby("team").last().reset_index())
    current_pts = current_row.set_index("team")["cum_pts"].reindex(teams).fillna(0).values.astype(float)

    recent_form = last3_form(season_df)
    remaining = season_df[season_df["scored"].isna()].copy()
    fixtures = build_fixtures(remaining, snap, recent_form, t_idx)
    return {"teams": teams, "t_idx": t_idx, "current_pts": current_pts, "fixtures": fixtures}


def backtest_snapshot(feat: pd.DataFrame, season: str, cutoff_gw: int) -> dict:
    """Freeze a completed historical season at gameweek `cutoff_gw`. Unlike
    live_snapshot, the rest of that season's results already exist as real
    rows, so they have to be explicitly hidden: the form snapshot is read off
    gameweek cutoff_gw+1 (whose rolling features already only reflect matches
    up to cutoff_gw, since they are shift(1)'d), and head-to-head is
    recomputed with every result after the cutoff date masked out first, so
    nothing from the rest of the season leaks into it. Matches NB07 Section J.
    `feat` is the full build_features() output covering every season up to and
    including `season` (used to recompute the hidden-future h2h, same as the
    notebook's `combined[combined['season'] <= BT_SEASON]`).
    """
    season_df = feat[feat["season"] == season].copy()
    teams = sorted(season_df["team"].unique())
    t_idx = {t: i for i, t in enumerate(teams)}

    as_of = season_df[season_df["gameweek"] == cutoff_gw].set_index("team")["cum_pts"].reindex(teams)
    current_pts = as_of.values.astype(float)

    snap = season_df[season_df["gameweek"] == cutoff_gw + 1].set_index("team")[config.SNAP_COLS]
    recent_form = last3_form(season_df[season_df["gameweek"] <= cutoff_gw])

    cut_date = season_df.loc[season_df["gameweek"] == cutoff_gw, "date"].max()
    h = feat[feat["season"] <= season][["team", "opponent", "date", "points"]].copy()
    h = h.sort_values(["team", "opponent", "date"])
    h.loc[h["date"] > cut_date, "points"] = np.nan
    h["h2h_pts_avg3"] = h.groupby(["team", "opponent"])["points"].transform(
        lambda x: x.shift(1).rolling(3, min_periods=1).mean()
    )

    remaining = season_df[season_df["gameweek"] > cutoff_gw].drop(columns="h2h_pts_avg3").merge(
        h[["team", "opponent", "date", "h2h_pts_avg3"]], on=["team", "opponent", "date"], how="left"
    )
    remaining["h2h_pts_avg3"] = remaining["h2h_pts_avg3"].fillna(1.5)

    fixtures = build_fixtures(remaining, snap, recent_form, t_idx)
    return {"teams": teams, "t_idx": t_idx, "current_pts": current_pts, "fixtures": fixtures}


def summarize(final_pts: np.ndarray, teams: list, seed_tiebreak: int = config.TIEBREAK_SEED) -> pd.DataFrame:
    tie_rng = np.random.default_rng(seed_tiebreak)
    jitter = tie_rng.random(final_pts.shape) * 1e-6
    order = np.argsort(-(final_pts + jitter), axis=1)
    rank = np.argsort(order, axis=1)
    p5, p50, p95 = np.percentile(final_pts, [5, 50, 95], axis=0)
    return pd.DataFrame({
        "title_prob": (rank == 0).mean(axis=0),
        "top4_prob": (rank <= 3).mean(axis=0),
        "releg_prob": (rank >= 17).mean(axis=0),
        "pts_p5": p5, "pts_median": p50, "pts_p95": p95,
    }, index=teams).sort_values("title_prob", ascending=False)
